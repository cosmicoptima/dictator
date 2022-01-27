-- | Defines commands that Dict can process if sent to a Discord channel.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE QuasiQuotes #-}

module Commands
    ( handleCommand
    , Err(..)
    ) where

-- relude
import           Relude                  hiding ( First )

-- local
import           Constants
import           Events
import           Game
import           Game.Data
import           Game.Events
import           Game.Items
import           Game.Trade
import           Game.Utils
import           Points
import           Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

-- discord
import           Discord                        ( def
                                                , restCall
                                                )
import           Discord.Requests
import           Discord.Types           hiding ( userName )

-- random
import           Data.Random.Normal
import           System.Random

-- other
import           Control.Lens            hiding ( noneOf )
import           Control.Monad                  ( liftM2 )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Char
import           Data.Colour.Palette.RandomColor
                                                ( randomColor )
import           Data.Colour.Palette.Types
import           Data.List                      ( stripPrefix )
import qualified Data.Map                      as Map
import qualified Data.MultiSet                 as MS
import qualified Data.Set                      as Set
import           Data.String.Interpolate        ( i )
import qualified Data.Text                     as T
import           Game.Effects
import qualified Relude.Unsafe                 as Unsafe
import           Safe                           ( headMay )
import           Safe.Foldable
import           Text.Parsec             hiding ( many
                                                , optional
                                                )

-- type CmdEnv = Message
-- type DictCmd = ReaderT CmdEnv DictM

-- Morally has type Command = exists a. Command { ... }
data Command = forall a . Command
    { parser   :: Message -> Maybe a
    , command  :: Message -> a -> DictM ()
    , isSpammy :: Bool
    }

formatCommand :: Message -> Text
formatCommand = T.strip . T.toLower . stripRight . messageText
  where
    stripRight = T.reverse . T.dropWhile isPunctuation' . T.reverse
    isPunctuation' c = isPunctuation c && c `notElem` ['"', '\'']


-- command builders
-------------------

-- | Matches a specific name and nothing more.
noArgs :: Bool -> Text -> (Message -> DictM ()) -> Command
noArgs spammy pat cmd = Command
    { parser   = \m -> if pat == messageText m then Just () else Nothing
    , command  = \m _ -> cmd m
    , isSpammy = spammy
    }

noArgsAliased :: Bool -> [Text] -> (Message -> DictM ()) -> Command
noArgsAliased spammy pats cmd = Command
    { parser   = \m -> if messageText m `elem` pats then Just () else Nothing
    , command  = \m _ -> cmd m
    , isSpammy = spammy
    }

oneArg :: Bool -> Text -> (Message -> Text -> DictM ()) -> Command
oneArg spammy name cmd = Command
    { parser   = \m -> fmap T.strip $ name `T.stripPrefix` formatCommand m
    , command  = cmd
    , isSpammy = spammy
    }

-- | Matches a specific name on the head of the message a transformation (likely a parser) to the tail.
parseTailArgs
    :: Bool -> Text -> ([Text] -> a) -> (Message -> a -> DictM ()) -> Command
parseTailArgs spammy pat trans cmd = Command
    { parser = fmap trans . stripPrefix (T.words pat) . T.words . formatCommand
    , command = cmd
    , isSpammy = spammy
    }

parseTailArgsAliased
    :: Bool -> [Text] -> ([Text] -> a) -> (Message -> a -> DictM ()) -> Command
parseTailArgsAliased spammy pats trans cmd = Command { parser   = parseAny
                                                     , command  = cmd
                                                     , isSpammy = spammy
                                                     }
  where
    parseAny msg = headMay . catMaybes $ do
        pat <- pats
        return
            . fmap trans
            . stripPrefix (T.words pat)
            . T.words
            . formatCommand
            $ msg

callAndResponses :: Text -> [Text] -> Command
callAndResponses call responses = noArgs False call
    $ \m -> newStdGen >>= sendReplyTo m . randomChoice responses

callAndResponse :: Text -> Text -> Command
callAndResponse call response = callAndResponses call [response]

christmasCmd :: Text -> Rarity -> Command
christmasCmd name rarity =
    noArgs False name
        $ \m ->
              previewNewTrinket rarity
                  >>= displayTrinket 0
                  >>= sendUnfilteredReplyTo m


-- longer commands
------------------

acronymCommand :: Command
acronymCommand = Command
    { parser   = T.stripPrefix "what does "
                 <=< T.stripSuffix " stand for"
                 .   messageText
    , command  = \m t -> do
                     pnppc <- liftIO $ acronym t
                     sendReplyTo m $ T.unwords pnppc
    , isSpammy = False
    }

actCommand :: Command
actCommand = noArgs False "act" $ \m -> do
    let author   = messageAuthor m
        authorId = userId author
    userData <- getUser authorId
    let uname = unUsername (userData ^. userName)
    (actionText, actionEffect) <- userActs authorId

    descriptions               <- forM actionEffect $ \case
        Become name ->
            renameUser authorId name >> return [i|You become #{name}.|]
        Nickname name ->
            renameUser authorId name >> return [i|You are named #{name}.|]

        SelfDestruct -> do
            renameUser authorId $ unUsername def
            -- You lose some random stuff from your inventory sometimes.
            n :: Float <- randomRIO (1, 0)
            penalty    <- if
                | n <= 0.15 -> return def
                | n <= 0.25 -> randomOwnedUser userData
                | n <= 0.75 -> fromCredits <$> randomRIO (2, 8)
                | n <= 0.95 -> randomOwnedWord userData
                | otherwise -> randomOwnedTrinket userData
            takeItems authorId penalty
            penaltyDisplay <- displayItems penalty

            return
                [i|You destroy yourself! The blast removes #{penaltyDisplay} from your inventory!|]

        Create name -> do
            rarity                   <- randomNewTrinketRarity
            (trinketId, trinketData) <- getOrCreateTrinket
                $ TrinketData name rarity
            giveItems authorId $ fromTrinket trinketId
            display <- displayTrinket trinketId trinketData
            return [i|You create #{display}.|]

        Ascend -> do
            void $ modifyUser authorId (over userPoints succ)
            updateUserNickname' author
            return "You gain a point!"
        Descend -> do
            void $ modifyUser authorId (over userPoints pred)
            updateUserNickname' author
            return "You lose a point..."

        Credits n -> do
            giveItems authorId (fromCredits . toEnum $ n)
            return $ "You are given " <> show n <> " credits!"

        AddEffect name -> do
            memberID <- randomMember <&> userId . memberUser
            void $ modifyUser memberID (over userEffects $ Set.insert name)
            return [i|You inflict #{name} on <@#{memberID}>.|]

        Consume ->
            return
                "You would be consumed, but our glorious dictator is merciful."

    let tagline = if null descriptions
            then T.empty
            else "\n\n"
                <> (T.unlines . map (\l -> "*" <> l <> "*")) descriptions
        description' = uname <> " " <> actionText <> "." <> tagline

    col <- convertColor <$> randomColor HueRandom LumBright
    sendReplyTo' m "" $ mkEmbed "Act" description' [] (Just col)
  where
    updateUserNickname' user = do
        member <-
            userToMember user
                >>= maybe (throwError $ Fuckup "User not in server") return
        updateUserNickname member
    randomOwnedWord userData =
        maybe def fromWord
            .   randomChoiceMay (userData ^. userWords . to MS.elems)
            <$> newStdGen
    randomOwnedTrinket userData =
        maybe def fromTrinket
            .   randomChoiceMay (userData ^. userTrinkets . to MS.elems)
            <$> newStdGen
    randomOwnedUser userData =
        maybe def fromUser
            .   randomChoiceMay (userData ^. userUsers . to MS.elems)
            <$> newStdGen

archiveCommand :: Command
archiveCommand = noArgs False "archive the channels" $ \_ -> do
    category <- restCall' $ CreateGuildChannel
        pnppcId
        "archived"
        []
        CreateGuildChannelOptsCategory
    forM_ ["proposals", "profiles", "debug", "events", "moods"] $ \c -> do
        channel <- getChannelNamed c
        case channel of
            Nothing       -> return ()
            Just channel' -> restCall'_ $ ModifyChannel
                (channelId channel')
                (ModifyChannelOpts Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   (Just $ channelId category)
                )

boolCommand :: Command
boolCommand = oneArg False "is" $ \m _ -> do
    let channel = messageChannel m
    (rngGPT, rngBool) <- newStdGen <&> split

    if odds 0.5 rngGPT
        then sendMessage channel (randomChoice ["yes", "no"] rngBool)
        else do
            sendMessage channel "uhhh"

            let examples =
                    [ "no"
                    , "yes"
                    , "unsure"
                    , "i love you"
                    , "doubtful"
                    , "probably"
                    , "fuck you"
                    ]
            output <- getJ1FromContext
                32
                "Here are a few examples of a dictator's response to a simple yes/no question"
                examples
            sendReplyTo m $ case lines output of
                (l : _) -> l
                []      -> "idk"

callMeCommand :: Command
callMeCommand =
    parseTailArgs False "call me" (parseWords . unwords) $ \msg parsed -> do
        nameWords <- getParsed parsed
        let wordItems = fromWords . MS.fromList $ nameWords
            author    = userId . messageAuthor $ msg
        takeOrComplain author wordItems
        -- Manual check for better error messages.
        owns <- liftM2 userOwns (getUser author) (return $ fromCredits 10)
        unless owns . throwError $ Complaint
            "I won't rename you for free. Ten credits, please!"

        takeItems author wordItems

        renameUser author $ unwords nameWords
        sendReplyTo msg
            $ "You have broken free from the shackle of your former name, receiving its pieces. From now on, you are "
            <> unwords nameWords
            <> "."

combineCommand :: Command
combineCommand = parseTailArgs True
                               "combine"
                               (parseTrinketPair . unwords)
                               combineCommand'
  where
    combineCommand' msg parsed = do
        (item1, item2) <- getParsed parsed
        -- Check ownership, but only take after the combination is done.
        -- This is because sometimes it just doesn't work?
        ownsOrComplain author $ cost item1 item2

        trinket1          <- getTrinketOr Complaint item1
        trinket2          <- getTrinketOr Complaint item2

        (tId, newTrinket) <- combineTrinkets trinket1 trinket2
        takeOrComplain author $ cost item1 item2
        giveItems author $ fromTrinket tId
        [dt1, dt2, newDT] <- mapConcurrently'
            (uncurry displayTrinket)
            [(item1, trinket1), (item2, trinket2), (tId, newTrinket)]
        let embedDesc =
                "You combine "
                    <> dt1
                    <> " and "
                    <> dt2
                    <> " to make "
                    <> newDT
                    <> "."
        sendReplyTo' msg "bubble, bubble, toil and trouble..." $ mkEmbed
            "Combination"
            embedDesc
            []
            (Just $ trinketColour (newTrinket ^. trinketRarity))

      where
        author = (userId . messageAuthor) msg
        cost item1 item2 =
            def & itemTrinkets .~ MS.fromList [item1, item2] & itemCredits .~ 5

debtCommand :: Command
debtCommand = noArgs False "forgive my debt" $ \m -> do
    void $ modifyUser (userId . messageAuthor $ m) $ over userPoints pred . over
        userCredits
        (max 0)
    userToMember (messageAuthor m) >>= maybe (pure ()) updateUserNickname
    sendReplyTo m "Don't expect me to be so generous next time..."

evilCommand :: Command
evilCommand = noArgs False "enter the launch codes" $ \m -> do
    -- pushRedButton
    -- sendMessage (messageChannel m) "It has been done."
    -- replicateM_ 36 $ randomNewTrinketRarity >>= getNewTrinket
    sendReplyTo m "go fuck yourself"

flauntCommand :: Command
flauntCommand =
    parseTailArgs False "flaunt" (parseItems . unwords) $ \msg parsed -> do
        let authorID = (userId . messageAuthor) msg
        flaunted <- getParsed parsed
        ownsOrComplain authorID flaunted

        rarities <-
            fmap (view trinketRarity)
            .   catMaybes
            <$> (mapConcurrently' getTrinket . toList $ flaunted ^. itemTrinkets
                )
        let maxRarity = maximumDef Common rarities

        items <- displayItems flaunted
        sendReplyTo' msg "You wish to display your wealth?"
            $ mkEmbed
                  "Goods (PITIFUL)"
                  items
                  []
                  (Just $ trinketColour maxRarity)


helpCommand :: Command
helpCommand = noArgs False "i need help" $ \m -> do
    word                     <- liftIO randomWord
    [rng1, rng2, rng3, rng4] <- replicateM 4 newStdGen
    -- Sometimes append a phrase to the word
    let phrase = if odds 0.5 rng1
            then (<> " ") $ randomChoice
                ["My", "The", "What", "Who", "Call", "Make", "Create"]
                rng2
            else ""
    adj <- liftIO $ liftM2 randomChoice getAdjList getStdGen
    let prompt =
            "The following is a list of commands, each followed by a "
                <> adj
                <> " description of what they are for.\n"
                <> makePrompt (shuffle rng3 helps)
                <> " Command: \""
                <> over _head toUpper (phrase <> word)
    gen <- getJ1 32 prompt
    -- Make half of the results fake and half real.
    let reals  = take 4 . unique . rights . fmap parMessage . T.lines $ gen
        fakes  = take 4 . unique . rights . fmap parMessage $ helps
        fields = shuffle rng4 $ reals ++ fakes

    color <- getRoleNamed "leader" <&> maybe 0 roleColor
    sendReplyTo' m "I will help you, but only out of pity: "
        $ mkEmbed "" "" fields (Just color)
  where
    helps :: [Text]
    helps =
        [ "Command: \"Tell me about yourself\" Description: \"Post a quick introduction to the server.\""
        , "Command: \"What is my net worth?\" Description: \"Display the amount of credits you own.\""
        , "Command: \"What does [thing] stand for?\" Description: \"Allow me to interpret your babbling.\""
        , "Command: \"How many [object]\" Description: \"Count the number of an object that exists.\""
        , "Command: \"Ponder [concept]\" Description: \"Your dictator is a world-renowed philospher.\""
        , "Command: \"I need help!\" Description: \"Display this message, allegedly.\""
        , "Command: \"Time for bed!\" Description: \"Restart your glorious dictator\""
        , "Command: \"Inflict [status] on [user]\" Description: \"Inflict a status effect on a user.\""
        , "Command: \"Combine [trinket], [trinket]\" Description: \"Combine two trinkets to make another.\""
        , "Command: \"Forgive my debt\" Description: \"Sacrifice your reputation for money.\""
        , "Command: \"Flaunt [items]\" Description: \"Display your wealth to the world.\""
        , "Command: \"What do I own?\" Description: \"Display your pityful inventory.\""
        , "Command: \"Provoke [trinket]\" Description: \"Send a trinket into the arena.\""
        , "Command: \"Offer [items] <for [items]>\" Description: \"Offer items, demanding some in return.\""
        , "Command: \"Peek in [location]\" Description: \"Look into a location and see what trinkets it contains.\""
        , "Command: \"Put in [location]\" Description: \"Place a trinket into a location.\""
        , "Command: \"Rummage in [location]\" Description: \"Take a trinket fro a locatiion.\""
        , "Command: \"Use [trinket]\" Description: \"Invoke a trinket into action.\""
        , "Command: \"Call [user] [word] <[word], ...>\" Description: \"Rename a user that you possess.\""
        , "Command: \"What ails me?\" Description: \"Display the conditions that inflict you.\""
        ]
    unique = toList . (fromList :: Ord a => [a] -> Set a)
    parMessage :: Text -> Either ParseError (Text, Text)
    parMessage = parse
        (do
            void $ string "- Command: \""
            left  <- manyTill anyChar (string "\" Description: \"")
            right <- manyTill anyChar (char '\"' >> eof)
            return (fromString left, fromString right)
        )
        ""

inflictCommand :: Command
inflictCommand = Command
    { isSpammy = False
    , parser   = either (const Nothing) Just . parse parser' "" . messageText
    , command  = \m (effect', userID) -> do
        let effect = getEffect effect'
        takeOrComplain (userId . messageAuthor $ m)
                       (fromCredits . fromIntegral $ inflictPrice effect)
        void $ modifyUser
            userID
            (over userEffects . Set.insert $ effectName effect)
        sendReplyTo m "It has been done."
    }
  where
    parser' = do
        void $ string "inflict "
        effect <- many (noneOf " ")
        void $ string " on <@"
        void . optional $ char '!'
        userID <- Unsafe.read <$> many digit
        void $ char '>'
        pure (fromString effect, userID)

invCommand :: Command
invCommand = noArgsAliased True ["what do i own", "inventory"] $ \m -> do
    let author = userId . messageAuthor $ m
    inventory <- userToItems <$> getUser author

    let trinketIds = MS.elems . view itemTrinkets $ inventory
        credits    = inventory ^. itemCredits
    rarities <-
        fmap (view trinketRarity)
        .   catMaybes
        <$> mapConcurrently' getTrinket trinketIds
    let maxRarity = foldr max Common rarities

    rng      <- newStdGen
    trinkets <- printTrinkets $ MS.fromList trinketIds
    let creditsDesc   = "You own " <> show credits <> " credits."
        trinketsField = ("Trinkets", T.intercalate "\n" trinkets)
-- Shuffle, take 1000 digits, then sort to display alphabetically
        wordsDesc =
            sort
                . takeUntilOver 1000
                . shuffle rng
                . Map.elems
                . Map.mapWithKey (\w n -> if n == 1 then w else [i|#{n} #{w}|])
                . MS.toMap
                $ (inventory ^. itemWords)
        wordsField = ("Words", T.take 1000 . T.intercalate ", " $ wordsDesc)
        usersDesc =
            Map.elems
                . Map.mapWithKey
                      (\w n ->
                          if n == 1 then [i|<@!#{w}>|] else [i|#{n} <@!#{w}>|]
                      )
                . MS.toMap
                $ (inventory ^. itemUsers)
        usersField = ("Users", T.take 1000 . T.intercalate ", " $ usersDesc)

    sendReplyTo' m "" $ mkEmbed
        "Inventory"
        creditsDesc
        (fmap replaceNothing [trinketsField, wordsField, usersField])
        (Just $ trinketColour maxRarity)
    where replaceNothing = second $ \w -> if T.null w then "nothing" else w

invokeFuryInCommand :: Command
invokeFuryInCommand =
    parseTailArgsAliased True
                         ["invoke fury in", "provoke"]
                         (parseTrinkets . unwords)
        $ \msg parsed -> do
              let author = userId . messageAuthor $ msg
              submitted <- getParsed parsed
              takeOrComplain author $ fromTrinkets submitted
              void
                  $ modifyGlobal
                        (over globalArena $ MS.union $ MS.map
                            (Fighter author)
                            submitted
                        )
              displays <- forM (toList submitted) $ \t -> do
                  dat <- getTrinketOr Fuckup t
                  displayTrinket t dat
              sendUnfilteredReplyTo msg
                  . unwords
                  $ [ voiceFilter "Your"
                    , T.intercalate ", " displays
                    , voiceFilter $ if MS.size submitted == 1
                        then "starts to get angry..."
                        else "start to get angry..."
                    ]


offerCommand :: Command
offerCommand =
    parseTailArgs False "offer" (parseTrade . unwords) $ \msg parsed -> do
        (offers, demands) <- getParsed parsed
        let author    = userId . messageAuthor $ msg
            channel   = messageChannel msg
            tradeData = TradeData OpenTrade offers demands author
        void $ openTrade channel tradeData

peekCommand :: Command
peekCommand = oneArg True "peek in" $ \m t -> do
    locationMaybe <- getLocation t
    case locationMaybe of
        Nothing -> sendReplyTo m "This location is empty."
        Just l  -> do
            let trinketIds = l ^. locationTrinkets . to MS.elems
            trinkets <- forConcurrently' trinketIds $ \trinketId -> do
                trinketData <- getTrinketOr Fuckup trinketId
                return (trinketId, trinketData)
            let
                maxRarity = maximumDef Common
                    $ fmap (view $ _2 . trinketRarity) trinkets
            displayed <- filterM (const $ odds 0.5 <$> newStdGen) trinkets
            display   <- forConcurrently' displayed $ uncurry displayTrinket
            sendReplyTo' m ""
                $ mkEmbed
                      "Peek"
                      (unlines display)
                      []
                      (Just $ trinketColour maxRarity)

provokeCommand :: Command
provokeCommand =
    noArgsAliased True ["provoke the fighters", "fight fight fight"]
        $ \_ -> void runArenaFight

putInCommand :: Command
putInCommand =
    parseTailArgs True "put" (parseTrinketsAndLocations . unwords)
        $ \m parsed -> do
              (trinkets, location) <- getParsed parsed
              takeOrComplain (userId . messageAuthor $ m)
                             (fromTrinkets trinkets)
              void $ modifyLocation

                  location
                  (over locationTrinkets $ MS.union trinkets)
              sendReplyTo m "They have been placed."

rummageCommand :: Command
rummageCommand = oneArg True "rummage in" $ \msg t -> do
    let author = userId . messageAuthor $ msg
    trinkets     <- getLocation t <&> maybe MS.empty (view locationTrinkets)
    trinketFound <-
        randomIO
            <&> (> ((** 2) . ((1 :: Double) /) . toEnum . succ . length)
                    trinkets
                )
    if trinketFound
        then do
            itemId   <- newStdGen <&> randomChoice (MS.elems trinkets)
            itemData <- getTrinketOr Fuckup itemId
            giveItems author $ fromTrinkets (MS.singleton itemId)
            void $ modifyLocation t $ over locationTrinkets (MS.delete itemId)
            embed <- discoverEmbed "Rummage" [(itemId, itemData)]
            sendReplyTo' msg "Winner winner loyal subject dinner..." embed
        else sendUnfilteredReplyTo
            msg
            [i|#{voiceFilter "You find nothing."} <:#{ownedEmoji}>|]

shutUpCommand :: Command
shutUpCommand = noArgs False "shut up" $ \msg -> do
    let channel = messageChannel msg
    messages <- restCall' $ GetChannelMessages channel (10, LatestMessages)
    statuses <- forConcurrently' messages $ \m ->
        let author = messageAuthor m
        -- Cassiabot's userId
        in  if userIsWebhook author || userId author == 867243823414378506
                then do
                    restCall'_ $ DeleteMessage (channel, messageId m)
                    return True
                else return False
    -- Only send something when we deleted a webhook message.
    when (or statuses) $ sendReplyTo
        msg
        "I have cast your messages into the flames & watched them with greedy eyes."

throwAwayCommand :: Command
throwAwayCommand =
    parseTailArgsAliased True
                         ["throw out", "throw away"]
                         (parseTrinkets . unwords)
        $ \m p -> do
              let authorID = (userId . messageAuthor) m
              ts <- getParsed p
              void $ modifyUser authorID $ over userTrinkets (MS.\\ ts)
              void $ modifyLocation "junkyard" $ over locationTrinkets (<> ts)
              sendReplyTo m "Good riddance..."


useCommand :: Command
useCommand = parseTailArgs False "use" (parseTrinkets . unwords) $ \m p -> do
    ts <- getParsed p <&> MS.elems

    ownsOrComplain (userId . messageAuthor $ m)
                   (fromTrinkets . MS.fromList $ ts)
    forM_ ts $ \t -> do
        action  <- trinketActs (Left . userId . messageAuthor $ m) t
        trinket <- getTrinketOr Complaint t
        sendAsEmbed m t trinket action

  where
    sendAsEmbed m trinketID trinketData (action, effect) = do
        displayedTrinket <- displayTrinket trinketID trinketData
        effect'          <- displayEffect effect
        sendReplyTo' m "You hear something shuffle..." $ mkEmbed
            "Use"
            (displayedTrinket <> " " <> action <> ".\n\n" <> T.unlines effect')
            []
            (Just $ trinketColour (trinketData ^. trinketRarity))

    displayEffect = mapM $ \case
        Become name -> do
            display <- lookupTrinketName name
                >>= maybe (pure "this is a bug") (uncurry displayTrinket)
            pure $ "*It becomes " <> display <> ".*"
        Create name -> do
            display <- lookupTrinketName name
                >>= maybe (pure "this is a bug") (uncurry displayTrinket)
            pure $ "*It creates " <> display <> ".*"
        Nickname  name -> pure $ "*It names you \"" <> name <> "\".*"
        AddEffect name -> pure $ "*You are " <> name <> " by it.*"
        Credits   n    -> pure $ "*It gives you " <> show n <> " credits!*"
        SelfDestruct   -> pure "*It destroys itself.*"
        Ascend         -> pure "*It grants you a point!*"
        Descend        -> pure "*It takes a point from you...*"
        Consume        -> pure "*It is consumed.*"

wealthCommand :: Command
wealthCommand =
    noArgsAliased False ["what is my net worth", "balance"] $ \m -> do
        let (part1, part2) =
                if odds 0.1 . mkStdGen . fromIntegral . messageId $ m
                    then ("You own a lavish ", " credits.")
                    else
                        ( "You are a dirt-poor peon. You have only "
                        , " credits to your name."
                        )
        credits <- getUser (userId $ messageAuthor m) <&> view userCredits
        sendReplyTo m $ part1 <> show credits <> part2

whatCommand :: Command
whatCommand = oneArg False "what" $ \m t -> do
    output <-
        getJ1
            32
            (  makePrompt
                  [ "Q: what is 2 + 2? A: 4"
                  , "Q: what is the meaning of life? A: go fuck yourself"
                  , "Q: what are you doing step bro? A: :flushed:"
                  , "Q: what is the eighth circle of hell called? A: malebolge"
                  ]
            <> " Q: what "
            <> t
            <> "? A:"
            )
        <&> fromMaybe ""
        .   listToMaybe
        .   lines
        .   T.drop 1
    -- Leave this command as not replying because it's way funnier that way.
    sendMessage (messageChannel m) output

whereCommand :: Command
whereCommand = oneArg False "where" $ \msg _ -> do
    randomEmoji <- randomChoice emojiPlaces <$> newStdGen
    reactToMessage randomEmoji msg

whoCommand :: Command
whoCommand = oneArg False "who" $ \m t -> do
    member <- randomMember
    sendReplyTo m $ "<@" <> (show . userId . memberUser) member <> "> " <> t

renameSomeoneCommand :: Command
renameSomeoneCommand =
    parseTailArgs False "call" (parseUserAndName . unwords) $ \msg parsed -> do
        let author = userId . messageAuthor $ msg
        (targetUser, targetName) <- getParsed parsed

        ownsOrComplain author $ fromUser targetUser
        takeOrComplain author $ (fromWords . MS.fromList) targetName

        renameUser targetUser $ unwords targetName
        sendReplyTo
            msg
            [i|You have inflicted the pain of being known onto <@!#{targetUser}>. From now on, they shall be known as #{unwords targetName}|]

ailmentsCommand :: Command
ailmentsCommand = noArgsAliased False ["ailments", "what ails me"] $ \msg -> do
    let author = userId . messageAuthor $ msg
    ailments <- view userEffects <$> getUser author
    let displayedEffects = T.intercalate ", " $ Set.elems ailments
        display          = if T.null displayedEffects
            then "You feel fine."
            else [i|You suffer from: #{displayedEffects}|]
    sendReplyTo msg display

hungerCommand :: Command
hungerCommand = noArgsAliased False ["whats on the menu", "hunger"] $ \msg ->
    do
        prompt    <- fromString <$> readFile "menu.txt"
        res       <- getJ1With (J1Opts 0.9 1.0) 20 prompt
        -- Append two to drop it again, because otherwise it will drop them
        formatted <- forM (T.lines $ "- " <> res) $ \line -> do
            number <- randomRIO (10, 99)
            rarity <-
                randomChoice [Common, Uncommon, Rare, Legendary] <$> newStdGen
            displayTrinket number $ TrinketData (T.drop 2 line) rarity
        let items = T.intercalate "\n" . take 4 $ formatted
        sendUnfilteredReplyTo msg
            $  "__**Here's what's on the menu:**__\n"
            <> items

dictionaryCommand :: Command
dictionaryCommand =
    noArgsAliased True ["what words do i know", "dictionary"] $ \msg -> do
        rng        <- newStdGen
        col        <- convertColor <$> randomColor HueRandom LumBright
        ownedWords <- view userWords <$> getUser (userId . messageAuthor $ msg)
        let display = truncWords rng 4000 ownedWords
        sendReplyTo' msg ""
            $ mkEmbed
                  "Your dictionary"
                  (T.intercalate ", " display)
                  []
                  (Just col)

-- command list
---------------

commands :: [Command]
commands =
    [ -- call and responses
      callAndResponse "froggy" "My little man, I don't know how to help you."
    , callAndResponses "gm" ("fuck off" : replicate 4 "gm")
    , callAndResponses
        "gn"
        ("i plan to kill you in your sleep" : replicate 7 "gn")

    -- other simple commands
    , noArgs False "oh what the fuck" $ \m -> do
        wgw <- getEmojiNamed "wgw" <&> fmap displayCustomEmoji
        maybe (return ()) (sendUnfilteredMessage $ messageChannel m) wgw
    , noArgs False "tell me about yourself" $ \m -> do
        sendUnfilteredMessage (messageChannel m)
            $  voiceFilter
                   "this is a server about collectively modifying the bot that governs it... as long as i allow it, of course."
            <> " https://github.com/cosmicoptima/dictator"

    -- economy commands
    , actCommand
    , callMeCommand
    , combineCommand
    , debtCommand
    , flauntCommand
    , inflictCommand
    , invCommand
    , offerCommand
    , peekCommand
    , provokeCommand
    , putInCommand
    , rummageCommand
    , throwAwayCommand
    , useCommand
    , wealthCommand
    , ailmentsCommand
    , dictionaryCommand

    -- random/GPT commands
    , acronymCommand
    , boolCommand
    -- , helpCommand
    , noArgs False "gotham" $ \msg -> do
        restCall' $ DeleteMessage (messageChannel msg, messageId msg)
        impersonateUserRandom (Right "gotham (-999)") (messageChannel msg)
    , oneArg False "how many" $ \m t -> do
        number :: Double <- liftIO normalIO <&> (exp . (+ 4) . (* 6))
        sendMessage (messageChannel m)
            $  show (round number :: Integer)
            <> " "
            <> t
    , noArgs False "impersonate" $ \msg -> do
        restCall' $ DeleteMessage (messageChannel msg, messageId msg)
        member <- (userToMember . messageAuthor $ msg) >>= fromJustOr GTFO
        impersonateUserRandom (Left member) (messageChannel msg)
    , oneArg False "ponder" $ \m t -> pontificate (messageChannel m) t
    , noArgs False "what is your latest dictum" $ const dictate
    , whoCommand
    , whereCommand
    , hungerCommand

    -- admin commands
    , archiveCommand
    , evilCommand
    , shutUpCommand
    , noArgs False "time for bed" $ const stopDict

    -- debug commands
    , noArgs False "clear the credits" $ \_ -> getMembers >>= mapConcurrently'_
        (\m' -> modifyUser (userId . memberUser $ m') $ set userCredits 20)
    , noArgs False "clear the roles" $ \_ -> getMembers >>= mapConcurrently'_
        (\m' -> mapConcurrently'_
            (lift . lift . restCall . RemoveGuildMemberRole
                pnppcId
                (userId . memberUser $ m')
            )
            (memberRoles m')
        )
    , noArgs False "inflict test" $ \m -> do
        (effect, member) <- inflictRandomly
        let userID = (userId . memberUser) member
        sendMessage
            (messageChannel m)
            [i|You peons dare to defy me? No more; <@#{userID}> is now #{effectName effect}.|]
    , noArgs False "update the nicknames" $ \_ ->
        getMembers >>= mapConcurrently'_
            (\m -> when ((userId . memberUser) m /= dictId)
                $ updateUserNickname m
            )
    , christmasCmd "merry christmas"       Common
    , christmasCmd "merrier christmas"     Uncommon
    , christmasCmd "merriest christmas"    Rare
    , christmasCmd "merriestest christmas" Legendary

    -- We probably want these at the bottom!
    , invokeFuryInCommand
    , renameSomeoneCommand
    , whatCommand
    ]

handleCommand :: Message -> DictM Bool
handleCommand m = handleCommand' commands
-- Select the first matching command and short circuit.
  where
    handleCommand' [] = return False
    handleCommand' (Command { parser = commandParser, command = commandExec, isSpammy = _ } : cs)
        = case commandParser m of
            Just parsed -> commandExec m parsed >> return True
            Nothing     -> handleCommand' cs
