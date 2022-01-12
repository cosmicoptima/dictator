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
    , TrinketAction(..)
    , getTrinketAction
    , randomNewTrinketRarity
    , randomExistingTrinketRarity
    , printTrinkets
    , trinketColour
    , nextTrinketId
    , validTrinketName

    -- conversion
    , itemsToUser
    , userToItems
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
    ) where

import           Relude                  hiding ( First
                                                , get
                                                , many
                                                )

import           Game.Data
import           Game.Items
import           Game.Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

import           Data.MultiSet                  ( MultiSet )

import           Control.Lens            hiding ( noneOf )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Default                   ( def )
import           Data.List                      ( maximum )
import qualified Data.MultiSet                 as MS
import qualified Data.Text                     as T
import qualified Database.Redis                as DB
import           Discord.Internal.Types.Prelude
import           Discord.Types                  ( CreateEmbed )
import           System.Random
import           System.Random.Shuffle
import           Text.Parsec             hiding ( (<|>) )


-- trinkets (high-level)
------------------------

combineTrinkets
    :: DB.Connection
    -> TrinketData
    -> TrinketData
    -> DictM (TrinketID, TrinketData)
combineTrinkets conn t1 t2 = do
    res <- getJ1With (J1Opts 0.9 0.9) 16 prompt
    let rarity = maximum . fmap (view trinketRarity) $ [t1, t2]
    let mayTrinket =
            rightToMaybe . parseTrinketCombination <=< listToMaybe . lines $ res
    case mayTrinket of
        Nothing   -> combineTrinkets conn t1 t2
        Just name -> do
            valid <- validTrinketName conn name
            let absorbed = name `elem` fmap (view trinketName) [t1, t2]

            if valid && not absorbed
                then do
                    let trinket = TrinketData name rarity
                    tId <- nextTrinketId conn
                    setTrinket conn tId trinket
                    return (tId, trinket)
                else combineTrinkets conn t1 t2
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

data TrinketAction = Become Text | Create Text | Nickname Text | SelfDestruct

getTrinketAction :: TrinketData -> DictM (Text, Maybe TrinketAction)
getTrinketAction t = do
    prompt <- shuffleM examples <&> toPrompt
    getJ1With (J1Opts 1.1 0.93) 16 prompt
        >>= either (const $ getTrinketAction t) return
        .   parse parser ""
  where
    examples =
        [ "Item: a real, life-sized dinosaur. Action: dies instantly. [become: a dinosaur corpse]"
        , "Item: a large loaf of bread. Action: becomes rotten. [become: a large, rotten loaf of bread]"
        , "Item: a tuba. Action: makes some music. [create: a tuba solo]"
        , "Item: a single chicken. Action: lays an egg. [create: a small egg]"
        , "Item: your honorable mother. Action: grants you a name. [nickname: Alice]"
        , "Item: a gateway to another world. Action: takes someone to another world. [nickname: dimensional voyager]"
        , "Item: ebola. Action: makes someone sick. [nickname: diseased]"
        , "Item: a bomb. Action: explodes violently, killing hundreds. [self-destruct]"
        , "Item: a nuclear power plant. Action: catastrophically fails. [self-destruct]"
        ]
    toPrompt es = makePrompt es <> "Item: " <> t ^. trinketName <> ". Action:"

    parser = do
        desc   <- (some (noneOf ".!?") <&> fromString) <* oneOf ".!?"
        effect <-
            Just
            <$> (  string " ["
                *> asum
                       [ string "become: "
                       >>  many (noneOf "]")
                       <&> Become
                       .   fromString
                       , string "create: "
                       >>  many (noneOf "]")
                       <&> Create
                       .   fromString
                       , string "self-destruct" $> SelfDestruct
                       , string "nickname: "
                       >>  many (noneOf "]")
                       <&> Nickname
                       .   fromString
                       ]
                <* string "]"
                )
            <|> return Nothing
        return (desc, effect)


-- trinkets (low-level)
-----------------------

randomNewTrinketRarity :: DictM Rarity
randomNewTrinketRarity = do
    outcome :: Double <- randomIO
    return $ if
        | 0.00 < outcome && outcome <= 0.70 -> Common
        | 0.70 < outcome && outcome <= 0.90 -> Uncommon
        | 0.90 < outcome && outcome <= 0.97 -> Rare
        | 0.97 < outcome && outcome <= 1.00 -> Legendary
        | otherwise                         -> Common

randomExistingTrinketRarity :: DictM Rarity
randomExistingTrinketRarity = do
    outcome :: Double <- randomIO
    return $ if
        | 0.00 < outcome && outcome <= 0.90 -> Common
        | 0.90 < outcome && outcome <= 0.97 -> Uncommon
        | 0.97 < outcome && outcome <= 1.00 -> Rare
        | otherwise                         -> Common

trinketRewards :: Rarity -> Credit
trinketRewards Common    = 5
trinketRewards Uncommon  = 15
trinketRewards Rare      = 30
trinketRewards Legendary = 200

-- | Canonical trinket colors for embeds.
-- | In order: White, blue, purple, gold.
trinketColour :: Rarity -> ColorInteger
trinketColour Common    = 0xB3C0B7
trinketColour Uncommon  = 0x0F468A
trinketColour Rare      = 0x6B007F
trinketColour Legendary = 0xFBB90C

validTrinketName :: DB.Connection -> Text -> DictM Bool
validTrinketName conn t = do
    nameIsFree <- lookupTrinketName conn name <&> isNothing
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

nextTrinketId :: DB.Connection -> DictM Int
nextTrinketId conn = countTrinket conn <&> succ

printTrinkets :: DB.Connection -> MultiSet TrinketID -> DictM [Text]
printTrinkets conn trinkets = do
    pairs <-
        forM
                (MS.elems trinkets)
                (\t -> do
                    trinketData <- getTrinket conn t
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

-- | Project a user into a collection of only their item data.
userToItems :: UserData -> Items
userToItems userData = Items { _itemCredits  = userData ^. userCredits
                             , _itemTrinkets = userData ^. userTrinkets
                             , _itemWords    = userData ^. userWords
                             }

-- | Given a user, update their item data to the following.
itemsToUser :: UserData -> Items -> UserData
itemsToUser userData items =
    userData
        &  userCredits
        .~ (items ^. itemCredits)
        &  userTrinkets
        .~ (items ^. itemTrinkets)
        &  userWords
        .~ (items ^. itemWords)

-- | Take a set of items from a user without any checking.
takeItems :: DB.Connection -> UserId -> Items -> DictM ()
takeItems conn user items = void $ modifyUser conn user removeItems
  where
    removeItems userData =
        userData
            &  userCredits
            -~ (items ^. itemCredits)
            &  userTrinkets
            %~ (MS.\\ (items ^. itemTrinkets))
            &  userWords
            %~ (MS.\\ (items ^. itemWords))

combineItems :: Items -> Items -> Items
combineItems it1 it2 = Items
    { _itemCredits  = it1 ^. itemCredits + it2 ^. itemCredits
    , _itemTrinkets = (it1 ^. itemTrinkets) `MS.union` (it2 ^. itemTrinkets)
    , _itemWords    = (it1 ^. itemWords) `MS.union` (it2 ^. itemWords)
    }


-- generic
---------

userOwns :: UserData -> Items -> Bool
userOwns userData items =
    let
        Items { _itemCredits = claimedCredits, _itemTrinkets = claimedTrinkets, _itemWords = claimedWords }
            = items
        ownsCredits =
            claimedCredits <= 0 || (userData ^. userCredits) >= claimedCredits
        ownsTrinkets =
            claimedTrinkets `MS.isSubsetOf` (userData ^. userTrinkets)
        ownsWords = claimedWords `MS.isSubsetOf` (userData ^. userWords)
    in
        ownsCredits && ownsTrinkets && ownsWords

-- Subtract the set canonical amount from a wallet.
decrementWallet :: DB.Connection -> UserId -> DictM ()
decrementWallet conn user = void $ modifyUser conn user (over userCredits pred)

-- | Punish a wallet with a message. You probably want to use takeOrPunish or takeOrComplain instead.
punishWallet :: DB.Connection -> UserId -> DictM ()
punishWallet conn user = do
    sendMessageToGeneral
        "You don't have the goods you so shamelessly claim ownership to, and now you own even less. Credits, that is."
    decrementWallet conn user

-- | Add items to a users inventory.
giveItems :: DB.Connection -> UserId -> Items -> DictM ()
giveItems conn user gift = do
    userData <- getUser conn user <&> fromMaybe def
    let newItems = combineItems gift . userToItems $ userData
    setUser conn user . itemsToUser userData $ newItems

-- | Take a set of items, returning False when they weren't owned.
takeOrPunish :: DB.Connection -> UserId -> Items -> DictM Bool
takeOrPunish conn user items = if user == dictId
    then return True
    else do
        userData <- getUser conn user <&> fromMaybe def
        let res = userOwns userData items
        if res then takeItems conn user items else punishWallet conn user
        return res

-- | Take a set of items, throwing a complaint when they weren't owned.
takeOrComplain :: DB.Connection -> UserId -> Items -> DictM ()
takeOrComplain conn user items = do
    ownsOrComplain conn user items
    takeItems conn user items

-- | Check ownership of a set of items, throwing a complaint when they weren't owned.
ownsOrComplain :: DB.Connection -> UserId -> Items -> DictM ()
ownsOrComplain conn user items = unless (user == dictId) $ do
    owned <- getUser conn user <&> flip userOwns items . fromMaybe def
    unless owned $ do
        decrementWallet conn user
        throwError
            $ Complaint
                  "You don't have the goods you so shamelessly claim ownership to, and now you have even less. Credits, that is."
