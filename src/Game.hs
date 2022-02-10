-- | Specifies the game involving trinkets, locations and credits.

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game
    (
    -- trinkets
      combineTrinkets
    , fightTrinkets
    , FightData(..)
    , Action(..)
    , getAction
    , getTrinketAction
    , randomNewTrinketRarity
    , randomExistingTrinketRarity
    , printTrinkets
    , trinketColour
    , nextTrinketId
    , validTrinketName

    -- conversion
    , fromTrinkets
    , fromCredits

    -- economy
    , userOwns
    , giveItems
    , takeItems
    , takeOrComplain
    , ownsOrComplain
    , takeOrPunish
    , punishWallet
    , decrementWallet
    , fightEmbed
    , trinketRewards
    , discoverEmbed
    , fromTrinket
    , fromWords
    , fromWord
    , impersonateUser
    , impersonateUserRandom
    , fromUsers
    , fromUser
    , trinketUpgradeOdds
    ) where

import           Relude                  hiding ( First
                                                , get
                                                , many
                                                , optional
                                                )

import           Game.Data               hiding ( userName )
import           Game.Items
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

import           Data.MultiSet                  ( MultiSet )

import           Control.Lens            hiding ( noneOf )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.ByteString.Base64
import           Data.Default                   ( def )
import           Data.List                      ( maximum )
import qualified Data.MultiSet                 as MS
import qualified Data.Text                     as T
import           Discord.Internal.Types.Prelude
import           Discord.Requests
import           Discord.Types
import           Game.Effects
import           Game.Utils
import           System.Random
import           System.Random.Shuffle
import           Text.Parsec             hiding ( (<|>) )
import           Utils                          ( odds )


impersonateUser :: Either GuildMember Text -> ChannelId -> Text -> DictM ()
impersonateUser whoTo whereTo whatTo = do
    let
        name = case whoTo of
            Left member ->
                fromMaybe (userName . memberUser $ member) $ memberNick member
            Right name' -> name'
    mayAvatar <- case whoTo of
        Left member -> do
            let userID        = (userId . memberUser) member
                mayAvatarHash = userAvatar . memberUser $ member
            maybe
                (pure Nothing)
                (   pure
                .   Just
                .   ("data:image/jpeg;base64," <>)
                .   encodeBase64
                <=< getAvatarData userID
                )
                mayAvatarHash
        Right _ -> pure (Just "")
    maybeHook <- view globalWebhook <$> getGlobal
    hook      <- case maybeHook of
        Just hook -> do
            restCall' . ModifyWebhook hook $ ModifyWebhookOpts
                (Just name)
                mayAvatar
                (Just whereTo)
        Nothing -> do
            hook <- restCall' . CreateWebhook whereTo $ CreateWebhookOpts
                name
                mayAvatar
            void . modifyGlobal $ set globalWebhook (Just $ webhookId hook)
            return hook

    restCall'_
        . ExecuteWebhookWithToken (webhookId hook) (webhookToken hook)
        . ExecuteWebhookWithTokenOpts (Just name)
        $ WebhookContentText whatTo

impersonateUserRandom :: Either GuildMember Text -> ChannelId -> DictM ()
impersonateUserRandom member channel = do
    messages <- restCall' $ GetChannelMessages channel (50, LatestMessages)
    let prompt =
            T.concat (map renderMessage . reverse $ messages)
                <> either (userName . memberUser) id member
                <> "\n"
    output <- getJ1 32 prompt <&> parse parser ""
    case output of
        Left  f -> throwError $ Fuckup (show f)
        Right t -> impersonateUser member channel t
  where
    renderMessage m =
        (userName . messageAuthor) m <> "\n" <> messageText m <> "\n\n"
    parser = fromString <$> many (noneOf "\n")


-- trinkets (high-level)
------------------------

combineTrinkets :: TrinketData -> TrinketData -> DictM (TrinketID, TrinketData)
combineTrinkets t1 t2 = do
    -- Take the highest trinket rarity, sometimes upgrading. This is a lot more common when the trinkets are of the same rarity.
    rng <- newStdGen
    let upgradeOdds =
            trinketUpgradeOdds (t1 ^. trinketRarity) (t2 ^. trinketRarity)
        baseRarity = maximum . fmap (view trinketRarity) $ [t1, t2]
        rarity     = (if odds upgradeOdds rng then succ' else id) baseRarity

    res <- getJ1With (J1Opts 0.9 0.9) 16 prompt
    let mayTrinket =
            rightToMaybe . parseTrinketCombination <=< listToMaybe . lines $ res

    case mayTrinket of
        Nothing   -> combineTrinkets t1 t2
        Just name -> do
            valid <- validTrinketName name
            let absorbed = name `elem` fmap (view trinketName) [t1, t2]

            if valid && not absorbed
                then do
                    let trinket = TrinketData name rarity
                    tId <- nextTrinketId
                    setTrinket tId trinket
                    return (tId, trinket)
                else combineTrinkets t1 t2
  where
    examples =
        [ "In an online message board, items can be combined together to create new items. Here are some examples of various combinations:"
        , "Combine 'a tin can' with '3.2g of gunpowder' to get 'a bomb'."
        , "Combine 'a small bird' with 'a small bird' to get 'a large bird'."
        , "Combine 'a permit to ban one user of your choice' with 'a user' to get 'BANNED'."
        -- , "Combine 'a remote tropical island with coconuts and a treasure chest and rum' with 'a rusty key' to get 'rum'."
        , "Combine 'the ability to control time' with 'a portal to another dimension' to get 'Godhood'."
        , "Combine 'a bag of dicks' with 'your ass' to get 'dicks up your ass'."
        , "Combine 'an ache of discontent' with 'a polaroid peel-off' to get 'a depressing photograph'."
        , "Combine 'a tiny cookie' with 'blood dreams of a dead end' to get 'a cookie with blood'."
        , "Combine 'a baby with no arms or legs' with 'arms' to get 'a baby with no legs'."
        , "Combine 'a poor girl' with 'a heart' to get 'a happy girl'."
        , "Combine 'flour' with 'a fire' to get 'bread'."
        , "Combine 'desperation' with 'night' to get 'hopeless night'."
        , "Combine 'credit' with 'a house' to get 'a mansion'."
        , "Combine 'a statement' with 'creative liberties' to get 'a lie'."
        , "Combine 'commitment' with 'sugar' to get 'diabetes'."
        , "Combine 'cookie-less cookies' with 'an empty glass' to get 'a non-filling diet'."
        , "Combine 'an unknown alien' with 'a flying pig' to get 'an interspecies wedding'."
        , "Combine 'firewall' with 'wall' to get 'firewallwall'."
        , "Combine 'p' with 'rain' to get 'pain'."
        ]
    prompt =
        unlines examples
            <> "\nCombine '"
            <> t1
            ^. trinketName
            <> "' with '"
            <> t2
            ^. trinketName
            <> "' to get '"
    succ' rarity = if rarity == Unspeakable then Unspeakable else succ rarity

discoverEmbed :: Text -> [(TrinketID, TrinketData)] -> DictM CreateEmbed
discoverEmbed source trinkets = do
    displays    <- forM trinkets (uncurry displayTrinket)
    trinketList <- case displays of
        []       -> throwError $ Fuckup "Attempt to display empty trinket"
        [t     ] -> return t
        (t : ts) -> return $ T.intercalate ", " ts <> " and " <> t
    let maxRarity = maximum . fmap (view $ _2 . trinketRarity) $ trinkets
        embedDesc = "You find " <> trinketList <> "."
    return $ mkEmbed source embedDesc [] (Just $ trinketColour maxRarity)

-- Some data representing a fight.
data FightData = FightData
    { fightWinnerIsFirst :: Bool
    , fightDescription   :: Text
    }

fightEmbed
    :: (TrinketID, TrinketData)
    -> (TrinketID, TrinketData)
    -> FightData
    -> DictM CreateEmbed
fightEmbed (t1, attacker) (t2, defender) fightData = do
    attackerDesc <- displayTrinket t1 attacker
    defenderDesc <- displayTrinket t2 defender
    let FightData attackerWins fightDesc = fightData
        winDesc = if attackerWins then "wins" else "loses"
        winnerColour = if attackerWins
            then trinketColour (attacker ^. trinketRarity)
            else trinketColour (defender ^. trinketRarity)
        embedDesc =
            attackerDesc
                <> " fights "
                <> defenderDesc
                <> " and "
                <> winDesc
                <> "! "
                <> fightDesc
                <> "."
    return $ mkEmbed "Trinket fight!" embedDesc [] (Just winnerColour)

fightTrinkets :: TrinketData -> TrinketData -> Maybe Bool -> DictM FightData
fightTrinkets t1 t2 winner = do
    res <- getJ1With (J1Opts 1 0.9) 16 (prompt t1 t2)
    let mayResult =
            rightToMaybe
                .   parse parTrinketCombat ""
                <=< listToMaybe
                .   lines
                $   res
    case mayResult of
        Nothing                  -> fightTrinkets t1 t2 winner
        Just (firstWon, details) -> return $ FightData firstWon details
  where
    examples =
        [ "Item 1: a baby with no limbs. Item 2: a type of vehicle. Winner: vehicle, because: A lot of crying from a flattened baby."
        , "Item 1: a makeshift bomb. Item 2: everything. Winner: bomb, because: Anything and everything can be blown up."
        , "Item 1: a free pass to ban one member. Item 2: a warehouse. Winner: ban, because: The warehouse gets banned."
        , "Item 1: a crocodile with no jaws. Item 2: the ability to travel through time. Winner: time travel, because: A crocodile dies of old age."
        , "Item 1: large turnips. Item 2: a poisonous snake. Winner: snake, because: The turnips get poisoned."
        , "Item 1: KILL. Item 2: a bed. Winner: KILL, because: Bed KILLED."
        , "Item 1: complete and utter silence. Item 2: tasty steak. Winner: steak, because: It's no longer silent."
        , "Item 1: a clean shirt. Item 2: a small cookie. Winner: cookie, because: Cookie crumbs all over the damn shirt."
        , "Item 1: a sheet of paper. Item 2: a knife. Winner: knife, because: The knife slices through the sheet of paper."
        , "Item 1: your mother. Item 2: bedtimes. Winner: your mother, because: your mother sets the bedtimes."
        , "Item 1: dictator. Item 2: the world. Winner: dictator, because: dictator orders the world to lose."
        , "Item 1: an interesting read. Item 2: the apocalypse. Winner: the apocalypse, because: the apocalypse destroys all potential readers."
        , "Item 1: a pumpkin pie. Item 2: dictator. Winner: dictator, because: dictator eats the pie."
        , "Item 1: my expectations. Item 2: a submergible. Winner: my expectations, because: my expectations are lower than the submergible."
        , "Item 1: the internet. Item 2: good ideas. Winner: the internet, because: good ideas get lost in the internet."
        , "Item 1: taxes. Item 2: infinity. Winner: infinity, because: taxes can't put a dent on infinity."
        , "Item 1: pikachu. Item 2: a painter. Winner: pikachu, because: pikachu electrocutes the painter to death."
        ]
    prompt t1' t2' =
        "In an online message board, items can be put to fight against each other. The more violent items often win. Here are some examples:\n"
            <> T.unlines examples
            <> "Item 1: "
            <> (t1' ^. trinketName)
            <> ". Item 2: "
            <> (t2' ^. trinketName)
            <> ". Winner:"
            <> winnerText t1' t2'
    winnerText t1' t2' = case winner of
        Just True  -> " " <> t1' ^. trinketName
        Just False -> " " <> t2' ^. trinketName
        Nothing    -> ""
    parTrinketCombat = do
        winnerWords <-
            some (noneOf ",")
            <&> MS.fromList
            .   filter ((> 3) . T.length)
            .   words
            .   fromString
        let t1Words =
                MS.fromList
                    . filter ((> 3) . T.length)
                    . words
                    . view trinketName
                    $ t1
            t2Words =
                MS.fromList
                    . filter ((> 3) . T.length)
                    . words
                    . view trinketName
                    $ t2
        void $ string ", because: "
        desc      <- manyTill anyChar (char '.')
        firstWins <- if
            | not . MS.null . MS.intersection t1Words $ winnerWords
            -> pure True
            | not . MS.null . MS.intersection t2Words $ winnerWords
            -> pure False
            | otherwise
            -> fail
                $  "incomprehensible result: "
                <> (toString . unwords . MS.elems) winnerWords
        return (firstWins, fromString desc)


-- actions
----------

data Action = Become Text
            | Create Text
            | Nickname Text
            | SelfDestruct
            | Points Int
            | Credits Int
            | AddEffect Text
            | Consume
            deriving Show -- debug

getAction :: Text -> DictM (Text, [Action])
getAction name = do
    output <- shuffleM examples >>= getJ1With (J1Opts 1 0.87) 16 . toPrompt
    either (const $ getAction name) return . parse parser "" $ output
  where
    examples =
        [ "Item: a real, life-sized dinosaur. Action: dies instantly. [become: a dinosaur corpse, lose point: 5]"
        , "Item: a large loaf of bread. Action: becomes rotten. [become: a large, rotten loaf of bread]"
        , "Item: a tuba. Action: makes some music. [create: a tuba solo]"
        , "Item: Lucifer. Action: spreads hellfire. [create: hellfire]"
        , "Item: balloons. Action: pop. [create: helium, lose point: 2]"
        , "Item: your honorable mother. Action: grants you a name. [nickname: Alice, gain point: 2]"
        , "Item: a gateway to another world. Action: takes someone to another world. [create: a strange world, nickname: a dimensional voyager]"
        , "Item: a little frog. Action: needs help. [nickname: froggy]"
        , "Item: ebola. Action: makes someone sick. [nickname: the diseased]"
        , "Item: a bomb. Action: explodes violently, killing hundreds. [self-destruct]"
        , "Item: a Discord server. Action: is torn apart by drama. [lose point: 3, self-destruct]"
        , "Item: three of something. Action: form a magnificent trio. [gain point: 5]"
        , "Item: moderator powers. Action: bans a member. [effect: silenced]"
        , "Item: a creepy girl. Action: improves herself. [gain point: 10, consume]"
        , "Item: a nuclear power plant. Action: catastrophically fails. [lose point: 10,  self-destruct]"
        , "Item: a peon. Action: does nothing (like a stupid peon). [lose point: 1]"
        , "Item: a lot of heroin. Action: starts an addiction. [become: a heroin addiction, lose point: 2]"
        , "Item: an open door. Action: drops a bucket of money-taking juice onto your head. [lose money: 25, effect: silenced, effect: taxed]"
        , "Item: a deceptive salesman. Action: convinces you to give up your money. [lose money: 10]"
        , "Item: an odd contraption. Action: releases a few coins. [gain money: 1, gain point: 1, consume]"
        , "Item: two sides of the same coin. Action: splits in half. [consume, create: heads coin, create: tails coin]"
        , "Item: a muzzle. Action: silences a dog or a human. [effect: silenced]"
        , "Item: a tax collector. Action: knocks on your door. [effect: taxed]"
        , "Item: an extra dick. Action: produces two extra dicks. [create: a dick, create: a dick, gain point: 1]"
        ]
    toPrompt es = makePrompt es <> " Item: " <> name <> ". Action:"

    parser = do
        desc   <- (some (noneOf ".!?") <&> fromString) <* oneOf ".!?"
        effect <-
            (string " [" *> actionParser `sepBy1` string ", " <* string "]")
                <|> return []
        return (desc, effect)

    actionParser = (asum . map try)
        [ string "become: " >> many (noneOf "],") <&> Become . fromString
        , string "create: " >> many (noneOf "],") <&> Create . fromString
        , string "nickname: " >> many (noneOf "],") <&> Nickname . fromString
        , string "effect: " >> do
            name' <- many (noneOf "],") <&> fromString
            if isJust (findEffect name') then pure $ AddEffect name' else empty
        , do
            gain <-
                (string "gain " >> return True)
                    <|> (string "lose " >> return False)
            void $ string "money: "
            sign   <- (string "-" >> return True) <|> return False
            number <- many digit
            let
                credits =
                    (if sign then negate else id)
                        . fromMaybe 0
                        . readMaybe
                        $ number
            pure . Credits . (if gain then id else negate) $ credits
        , do
            gain <-
                (string "gain " >> return True)
                    <|> (string "lose " >> return False)
            void $ string "point: "
            sign   <- (string "-" >> return True) <|> return False
            number <- many digit
            let
                points =
                    (if sign then negate else id)
                        . fromMaybe 0
                        . readMaybe
                        $ number
            pure . Points . (if gain then id else negate) $ points
        , string "self-destruct" $> SelfDestruct
        , string "consume" $> Consume
        ]

getTrinketAction :: TrinketData -> DictM (Text, [Action])
getTrinketAction = getAction . view trinketName


-- trinkets (low-level)
-----------------------

-- The chance for two trinkets to upgrade when combined
trinketUpgradeOdds :: Rarity -> Rarity -> Double
trinketUpgradeOdds r1 r2 = case max r1 r2 of
    Unspeakable -> 0.000
    Forbidden   -> if r1 == r2 then 0.001 else 0.000
    Mythic      -> if r1 == r2 then 0.003 else 0.001
    Legendary   -> if r1 == r2 then 0.009 else 0.004
    Rare        -> if r1 == r2 then 0.150 else 0.025
    Uncommon    -> if r1 == r2 then 0.200 else 0.050
    Common      -> if r1 == r2 then 0.400 else 0.150


randomNewTrinketRarity :: DictM Rarity
randomNewTrinketRarity = do
    outcome :: Double <- randomRIO (0, 1)
    return $ if
        | outcome <= 0.650 -> Common
        | outcome <= 0.850 -> Uncommon
        | outcome <= 0.950 -> Rare
        | outcome <= 0.985 -> Legendary
        | outcome <= 0.996 -> Mythic
        | outcome <= 0.999 -> Forbidden
        | outcome <= 1.000 -> Unspeakable
        | otherwise        -> Common

-- Because higher tiers are supposed to be more rare, it's rarer to get a trinket someone else already owns.
randomExistingTrinketRarity :: DictM Rarity
randomExistingTrinketRarity = do
    outcome :: Double <- randomIO
    return $ if
        | outcome <= 0.750 -> Common
        | outcome <= 0.950 -> Uncommon
        | outcome <= 0.990 -> Rare
        | outcome <= 0.995 -> Legendary
        | outcome <= 1.000 -> Mythic
        | otherwise        -> Common

trinketRewards :: Rarity -> Credit
trinketRewards Common      = 5
trinketRewards Uncommon    = 15
trinketRewards Rare        = 30
trinketRewards Legendary   = 200
trinketRewards Mythic      = 1000
trinketRewards Forbidden   = 20000
trinketRewards Unspeakable = 666666

-- | Canonical trinket colors for embeds.
-- | In order: White, blue, purple, gold.
trinketColour :: Rarity -> ColorInteger
trinketColour Common      = 0xB3C0B7
trinketColour Uncommon    = 0x0F468A
trinketColour Rare        = 0x6B007F
trinketColour Legendary   = 0xFBB90C
trinketColour Mythic      = 0xE460D5
trinketColour Forbidden   = 0xF32D2D
trinketColour Unspeakable = 0x9FFF00

validTrinketName :: Text -> DictM Bool
validTrinketName t = do
    nameIsFree <- lookupTrinketName name <&> isNothing
    return
        $         not ("A " `T.isPrefixOf` name)
        &&        name
        `notElem` (  commonTrinketExamples
                  <> uncommonTrinketExamples
                  <> rareTrinketExamples
                  <> legendaryTrinketExamples
                  )
        &&        nameIsFree
    where name = T.strip t

parseTrinketCombination :: Text -> Either ParseError Text
parseTrinketCombination = parse go ""
    where go = manyTill anyChar (string "'.") <&> fromString

nextTrinketId :: DictM Int
nextTrinketId = countTrinket <&> succ

printTrinkets :: MultiSet TrinketID -> DictM [Text]
printTrinkets trinkets = do
    pairs <-
        forM
                (MS.elems trinkets)
                (\t -> do
                    trinketData <- getTrinket t
                    return (t, trinketData)
                )
            <&> MS.fromList
    displays <- mapConcurrently'
        (\case
            (trinket, Just trinketData) ->
                displayTrinket trinket trinketData <&> Just
            (_, Nothing) -> return Nothing
        )
        (MS.elems pairs)
    (return . catMaybes) displays


-- items
--------

fromUsers :: MultiSet UserId -> Items
fromUsers users = def & itemUsers .~ users

fromUser :: UserId -> Items
fromUser = fromUsers . MS.singleton

fromTrinkets :: MultiSet TrinketID -> Items
fromTrinkets trinkets = def & itemTrinkets .~ trinkets

fromTrinket :: TrinketID -> Items
fromTrinket = fromTrinkets . MS.singleton

fromWords :: MultiSet Text -> Items
fromWords wordSet = def & itemWords .~ wordSet

fromWord :: Text -> Items
fromWord = fromWords . MS.singleton

fromCredits :: Credit -> Items
fromCredits credits = def & itemCredits .~ credits

-- | Take a set of items from a user without any checking.
takeItems :: UserId -> Items -> DictM ()
takeItems user items =
    void $ modifyUser user (over userItems $ flip subtractItems items)

combineItems :: Items -> Items -> Items
combineItems it1 it2 = Items
    { _itemCredits  = it1 ^. itemCredits + it2 ^. itemCredits
    , _itemTrinkets = (it1 ^. itemTrinkets) `MS.union` (it2 ^. itemTrinkets)
    , _itemWords    = (it1 ^. itemWords) `MS.union` (it2 ^. itemWords)
    , _itemUsers    = (it1 ^. itemUsers) `MS.union` (it2 ^. itemUsers)
    }

-- | `subtractItems` a b = a - b where (-) represents item subtraction
subtractItems :: Items -> Items -> Items
subtractItems it1 it2 = Items
    { _itemCredits  = it1 ^. itemCredits - it2 ^. itemCredits
    , _itemTrinkets = (it1 ^. itemTrinkets) MS.\\ (it2 ^. itemTrinkets)
    , _itemWords    = (it1 ^. itemWords) MS.\\ (it2 ^. itemWords)
    , _itemUsers    = (it1 ^. itemUsers) MS.\\ (it2 ^. itemUsers)
    }


-- generic
---------

userOwns :: Items -> Items -> Bool
userOwns ownedItems claimedItems =
    let
        Items { _itemCredits = claimedCredits, _itemTrinkets = claimedTrinkets, _itemWords = claimedWords, _itemUsers = claimedUsers }
            = claimedItems
        ownsCredits =
            claimedCredits <= 0 || (ownedItems ^. itemCredits) >= claimedCredits
        ownsTrinkets =
            claimedTrinkets `MS.isSubsetOf` (ownedItems ^. itemTrinkets)
        ownsUsers = claimedUsers `MS.isSubsetOf` (ownedItems ^. itemUsers)
        ownsWords = claimedWords `MS.isSubsetOf` (ownedItems ^. itemWords)
    in
        ownsCredits && ownsTrinkets && ownsUsers && ownsWords

-- Subtract the set canonical amount from a wallet.
decrementWallet :: UserId -> DictM ()
decrementWallet user =
    void $ modifyUser user (over (userItems . itemCredits) pred)

-- | Punish a wallet with a message. You probably want to use takeOrPunish or takeOrComplain instead.
punishWallet :: UserId -> DictM ()
punishWallet user = do
    decrementWallet user
    throwError
        $ Complaint
              "You don't have the goods you so shamelessly claim ownership to, and now you have even less. Credits, that is."

-- | Add items to a users inventory.
giveItems :: UserId -> Items -> DictM ()
giveItems user gift = do
    userData <- getUser user
    let newItems = combineItems gift . view userItems $ userData
    setUser user $ userData & userItems .~ newItems

-- | Take a set of items, returning False when they weren't owned.
takeOrPunish :: UserId -> Items -> DictM Bool
takeOrPunish user items = if user == dictId
    then return True
    else do
        userData <- getUser user
        let res = userOwns (userData ^. userItems) items
        if res then takeItems user items else punishWallet user
        return res

-- | Take a set of items, throwing a complaint when they weren't owned.
takeOrComplain :: UserId -> Items -> DictM ()
takeOrComplain user items = do
    ownsOrComplain user items
    takeItems user items

-- | Check ownership of a set of items, throwing a complaint when they weren't owned.
ownsOrComplain :: UserId -> Items -> DictM ()
ownsOrComplain user items = unless (user == dictId) $ do
    owned <- flip userOwns items . view userItems <$> getUser user
    unless owned $ punishWallet user


