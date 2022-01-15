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
import           System.Random.Shuffle          ( shuffle' )

-- other
import           Control.Lens            hiding ( noneOf )
import           Control.Monad                  ( liftM2 )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Char
import           Data.Foldable                  ( maximum )
import           Data.List                      ( stripPrefix )
import qualified Data.Map                      as Map
import qualified Data.MultiSet                 as MS
import           Data.String.Interpolate        ( i )
import qualified Data.Text                     as T
import           Text.Parsec

-- Morally has type Command = exists a. Command { ... }
-- Existential types in Haskell have a strange syntax!
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

oneArg :: Bool -> Text -> (Message -> Text -> DictM ()) -> Command
oneArg spammy name cmd = Command
    { parser   = \m -> fmap T.strip $ name `T.stripPrefix` formatCommand m
    , command  = cmd
    , isSpammy = spammy
    }

-- | Matches a specific name on the head of the message a transformation (likely a parser) to the tail.
parseTailArgs
    :: Bool -> [Text] -> ([Text] -> a) -> (Message -> a -> DictM ()) -> Command
parseTailArgs spammy pat trans cmd = Command
    { parser   = fmap trans . stripPrefix pat . T.words . formatCommand
    , command  = cmd
    , isSpammy = spammy
    }


callAndResponses :: Text -> [Text] -> Command
callAndResponses call responses = noArgs False call $ \m ->
    newStdGen >>= sendMessage (messageChannel m) . randomChoice responses

callAndResponse :: Text -> Text -> Command
callAndResponse call response = callAndResponses call [response]

christmasCmd :: Text -> Rarity -> Command
christmasCmd name rarity = noArgs False name $ \m ->
    previewNewTrinket rarity >>= displayTrinket 0 >>= sendUnfilteredMessage
        (messageChannel m)


-- longer commands
------------------

acronymCommand :: Command
acronymCommand = Command
    { parser   = T.stripPrefix "what does "
                 <=< T.stripSuffix " stand for"
                 .   messageText
    , command  = \m t -> do
                     pnppc <- liftIO $ acronym t
                     sendMessage (messageChannel m) $ T.unwords pnppc
    , isSpammy = False
    }

actCommand :: Command
actCommand = noArgs False "act" $ \m -> do
    let author   = messageAuthor m
        authorId = userId author
    uname <- getUser authorId <&> unUsername . maybe def (view userName)
    (actionText, actionEffect) <- userActs authorId

    case actionEffect of
        Just (Become   name) -> renameUser authorId name
        Just (Nickname name) -> renameUser authorId name
        Just SelfDestruct    -> renameUser authorId $ unUsername def

        Just (Create name)   -> do
            rarity       <- randomNewTrinketRarity
            (trinket, _) <- getTrinketByName name rarity
            void $ modifyUser authorId (over userTrinkets $ MS.insert trinket)

        Just Ascend -> do
            void $ modifyUser authorId (over userPoints succ)
            updateUserNickname' author
        Just Descend -> do
            void $ modifyUser authorId (over userPoints pred)
            updateUserNickname' author

        _ -> pure ()

    let description = uname <> " " <> actionText <> "." <> case actionEffect of
            Just (Become   name) -> "\n\n*You become " <> name <> ".*"
            Just (Create   name) -> "\n\n*You create " <> name <> ".*"
            Just (Nickname name) -> "\n\n*You are named " <> name <> ".*"
            Just SelfDestruct    -> "\n\n*You destroy yourself.*"
            Just Ascend          -> "\n\n*You gain a point!*"
            Just Descend         -> "\n\n*You lose a point.*"
            _                    -> ""
    void . restCall' $ CreateMessageEmbed (messageChannel m) "" $ mkEmbed
        "Act"
        description
        []
        Nothing
  where
    updateUserNickname' user = do
        member <-
            userToMember user
                >>= maybe (throwError $ Fuckup "User not in server") return
        updateUserNickname member

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

arenaCommand :: Command
arenaCommand = noArgs True "fight fight fight" $ \m -> do
    let authorID = userId . messageAuthor $ m

    rng         <- newStdGen
    myTrinketId <-
        getUserOr Fuckup authorID
        <&> flip randomChoice rng
        .   MS.elems
        .   view userTrinkets
    myTrinketData <- getTrinketOr Fuckup myTrinketId

    arenaStatus'  <- getGlobal <&> view globalAdhocFighter
    case arenaStatus' of
        Nothing -> do
            void . modifyGlobal $ set globalAdhocFighter
                                      (Just $ Fighter authorID myTrinketId)
            displayedTrinket <- displayTrinket myTrinketId myTrinketData
            sendUnfilteredMessage (messageChannel m)
                $  voiceFilter "Your"
                <> " "
                <> displayedTrinket
                <> " "
                <> voiceFilter "prepares to fight..."
        Just (Fighter theirUserId theirTrinketId) -> do
            void . modifyGlobal $ set globalAdhocFighter Nothing
            theirTrinketData          <- getTrinketOr Fuckup theirTrinketId
            FightData theyWon details <- fightTrinkets theirTrinketData
                                                       myTrinketData
                                                       Nothing
            let
                (winnerUserId, loserUserId, winnerTrinketData, loserTrinketId)
                    = if theyWon
                        then
                            ( theirUserId
                            , authorID
                            , theirTrinketData
                            , myTrinketId
                            )
                        else
                            ( authorID
                            , theirUserId
                            , myTrinketData
                            , theirTrinketId
                            )


            void . modifyUser winnerUserId $ over
                userCredits
                (+ winnerTrinketData ^. trinketRarity . to trinketRewards)
            void . modifyUser loserUserId $ over userTrinkets
                                                 (MS.delete loserTrinketId)

            myDisplay    <- displayTrinket myTrinketId myTrinketData
            theirDisplay <- displayTrinket theirTrinketId theirTrinketData
            let winnerDisplay = if theyWon then theirDisplay else myDisplay

            let embedDesc =
                    theirDisplay
                        <> " (<@"
                        <> show theirUserId
                        <> ">) and "
                        <> myDisplay
                        <> " (<@"
                        <> show authorID
                        <> ">) fight...\n\n"
                        <> winnerDisplay
                        <> " (<@"
                        <> show winnerUserId
                        <> ">) wins! "
                        <> details
                        <> "."
            void
                . restCall'
                $ CreateMessageEmbed (messageChannel m) ""
                $ mkEmbed
                      "Arena"
                      embedDesc
                      []
                      (  Just
                      $  winnerTrinketData
                      ^. trinketRarity
                      .  to trinketColour
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
            sendMessage channel $ case lines output of
                (l : _) -> l
                []      -> "idk"

callMeCommand :: Command
callMeCommand =
    parseTailArgs False ["call", "me"] (parseWords . unwords) $ \msg parsed ->
        do
            nameWords <- getParsed parsed
            let wordItems = fromWords . MS.fromList $ nameWords
                author    = userId . messageAuthor $ msg
                channel   = messageChannel msg
            takeOrComplain author wordItems
            -- Manual check for better error messages.
            owns <- liftM2 userOwns
                           (getUserOr Fuckup author)
                           (return $ fromCredits 10)
            unless owns . throwError $ Complaint
                "I won't rename you for free. Ten credits, please!"

            takeItems author wordItems

            renameUser author $ unwords nameWords
            sendMessage channel
                $ "You have broken free from the shackle of your former name, receiving its pieces. From now on, you are "
                <> unwords nameWords
                <> "."

combineCommand :: Command
combineCommand = parseTailArgs True
                               ["combine"]
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
        giveItems author $ (fromTrinkets . MS.fromList) [tId]
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
        void
            . restCall'
            . CreateMessageEmbed
                  channel
                  (voiceFilter "bubble, bubble, toil and trouble...")
            $ mkEmbed "Combination"
                      embedDesc
                      []
                      (Just $ trinketColour (newTrinket ^. trinketRarity))

      where
        author  = (userId . messageAuthor) msg
        channel = messageChannel msg
        cost item1 item2 =
            def & itemTrinkets .~ MS.fromList [item1, item2] & itemCredits .~ 5

evilCommand :: Command
evilCommand = noArgs False "enter the launch codes" $ \m -> do
    pushRedButton
    sendMessage (messageChannel m) "It has been done."
    replicateM_ 36 $ randomNewTrinketRarity >>= getNewTrinket

flauntCommand :: Command
flauntCommand =
    parseTailArgs False ["flaunt"] (parseTrinkets . unwords) $ \msg parsed -> do
        let authorID = (userId . messageAuthor) msg
            channel  = messageChannel msg
        flauntedTrinkets <- getParsed parsed
        userData         <- getUser authorID <&> fromMaybe def
        if userOwns userData $ fromTrinkets flauntedTrinkets
            then do
                rarities <-
                    fmap (view trinketRarity)
                    .   catMaybes
                    <$> (mapConcurrently' getTrinket . toList $ flauntedTrinkets
                        )
                let maxRarity = foldr max Common rarities
                trinkets <- printTrinkets flauntedTrinkets
                let display = T.intercalate "\n" trinkets
                void
                    . restCall'
                    . CreateMessageEmbed
                          channel
                          (voiceFilter "You wish to display your wealth?")
                    $ mkEmbed "Goods (PITIFUL)"
                              display
                              []
                              (Just $ trinketColour maxRarity)
            else do
                sendMessage
                    channel
                    "You don't own the goods you so shamelessly try to flaunt, and now you own even less. Credits, that is."
                void $ modifyUser authorID (over userCredits pred)

helpCommand :: Command
helpCommand = noArgs False "i need help" $ \m -> do
    rng  <- newStdGen
    word <- liftIO randomWord
    adj  <- liftIO $ liftM2 randomChoice getAdjList getStdGen
    let prompt =
            "The following is a list of commands, each followed by a "
                <> adj
                <> " description of what they are for.\n"
                <> makePrompt helps
                <> " Command: \""
                <> over _head toUpper word
    gen <- getJ1 32 prompt
    num <- randomRIO (6, 9)
    let fields =
            take num
                .  shuffle rng
                .  unique
                .  rights
                .  fmap parMessage
                .  T.lines
                $  prompt
                <> gen

    color <- getRoleNamed "leader" <&> maybe 0 roleColor
    void
        . restCall'
        . CreateMessageEmbed
              (messageChannel m)
              (voiceFilter "I will help you, but only out of pity: ")
        $ mkEmbed "" "" fields (Just color)
  where
    helps :: [Text]
    helps =
        [ "Command: \"Tell me about yourself\" Description: \"Introduce myself to you lesser beings.\""
        , "Command: \"What is my net worth?\" Description: \"I'll let you know how much you're worth to me.\""
        , "Command: \"What does [thing] stand for?\" Description: \"Allow me to interpret your babbling.\""
        , "Command: \"How many [object]\" Description: \"I am excellent at mathematics.\""
        , "Command: \"Show the points\" Description: \"I know you lot love to argue amongst yourselves.\""
        , "Command: \"Ponder [concept]\" Description: \"Your dictator is a world-renowed philospher.\""
        , "Command: \"I need help!\" Description: \"Yeah, you do, freak.\""
        , "Command: \"Time for bed!\" Description: \"I lose track of time easily. Let me know when it\"s time to sleep.\""
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

invCommand :: Command
invCommand = noArgs True "what do i own" $ \m -> do
    let author = userId . messageAuthor $ m
    inventory <- maybe def userToItems <$> getUser author
    rng       <- newStdGen

    let trinketIds = MS.elems . view itemTrinkets $ inventory
        credits    = inventory ^. itemCredits
    rarities <-
        fmap (view trinketRarity)
        .   catMaybes
        <$> mapConcurrently' getTrinket trinketIds
    let maxRarity = foldr max Common rarities

    trinkets <- printTrinkets $ MS.fromList trinketIds
    let creditsDesc   = "You own " <> show credits <> " credits."
        trinketsField = ("Trinkets", T.intercalate "\n" trinkets)
        wordsDesc =
            shuffle rng
                . Map.elems
                . Map.mapWithKey (\w n -> if n == 1 then w else [i|#{n} #{w}|])
                . MS.toMap
                $ (inventory ^. itemWords)
        wordsField = ("Words", T.take 1000 . T.intercalate ", " $ wordsDesc)

    restCall'_ . CreateMessageEmbed (messageChannel m) "" $ mkEmbed
        "Inventory"
        creditsDesc
        [trinketsField, wordsField]
        (Just $ trinketColour maxRarity)

invokeFuryInCommand :: Command
invokeFuryInCommand =
    parseTailArgs True ["invoke", "fury", "in"] (parseTrinkets . unwords)
        $ \msg parsed -> do
              let author  = userId . messageAuthor $ msg
                  channel = messageChannel msg
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
              sendUnfilteredMessage channel
                  . unwords
                  $ [ voiceFilter "Your"
                    , T.intercalate ", " displays
                    , voiceFilter $ if MS.size submitted == 1
                        then "starts to get angry..."
                        else "start to get angry..."
                    ]

makeFightCommand :: Command
makeFightCommand =
    parseTailArgs True ["make"] (parse parseFightCommand "" . unwords)
        $ \msg parsed -> do
              let author  = userId . messageAuthor $ msg
                  channel = messageChannel msg
              (t1, t2) <- getParsed parsed
              ownsOrComplain author $ (fromTrinkets . MS.fromList) [t1]
              attacker  <- getTrinketOr Complaint t1
              defender  <- getTrinketOr Complaint t2
              fightData <- fightTrinkets attacker defender Nothing
              embed     <- fightEmbed (t1, attacker) (t2, defender) fightData
              restCall'_ $ CreateMessageEmbed
                  channel
                  (voiceFilter "You hear something rumble...")
                  embed

  where
    parseFightCommand = do
        t1 <- parTrinketItem
        void $ string " fight "
        t2 <- parTrinketItem
        return (t1, t2)

offerCommand :: Command
offerCommand =
    parseTailArgs False ["offer"] (parseTrade . unwords) $ \msg parsed -> do
        (offers, demands) <- getParsed parsed
        let author    = userId . messageAuthor $ msg
            channel   = messageChannel msg
            tradeData = TradeData OpenTrade offers demands author
        void $ openTrade channel tradeData

peekCommand :: Command
peekCommand = oneArg True "peek in" $ \m t -> do
    locationMaybe <- getLocation t
    case locationMaybe of
        Nothing -> sendMessage (messageChannel m) "This location is empty."
        Just l  -> do
            let trinketIds = l ^. locationTrinkets . to MS.elems
            trinkets <- forConcurrently' trinketIds $ \trinketId -> do
                trinketData <- getTrinketOr Fuckup trinketId
                return (trinketId, trinketData)
            let maxRarity = maximum $ fmap (view $ _2 . trinketRarity) trinkets
            displayed <- filterM (const $ odds 0.5 <$> newStdGen) trinkets
            display   <- forConcurrently' displayed $ uncurry displayTrinket
            void
                . restCall'
                . CreateMessageEmbed (messageChannel m) ""
                $ mkEmbed "Peek"
                          (unlines display)
                          []
                          (Just $ trinketColour maxRarity)

provokeCommand :: Command
provokeCommand = noArgs True "provoke the fighters" $ \_ -> void runArenaFight

putInCommand :: Command
putInCommand =
    parseTailArgs True ["put"] (parseTrinketsAndLocations . unwords)
        $ \m parsed -> do
              (trinkets, location) <- getParsed parsed
              takeOrComplain (userId . messageAuthor $ m)
                             (fromTrinkets trinkets)
              void $ modifyLocation

                  location
                  (over locationTrinkets $ MS.union trinkets)
              sendMessage (messageChannel m) "They have been placed."

rummageCommand :: Command
rummageCommand = oneArg True "rummage in" $ \msg t -> do
    let author  = userId . messageAuthor $ msg
        channel = messageChannel msg
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
            restCall'_ $ CreateMessageEmbed
                channel
                (voiceFilter "Winner winner loyal subject dinner...")
                embed
        else void $ sendUnfilteredMessage
            channel
            (voiceFilter "You find nothing." <> " <:" <> ownedEmoji <> ">")

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
    when (or statuses) $ do
        sendMessage
            channel
            "I have cast your messages into the flames & watched them with greedy eyes."

throwAwayCommand :: Command
throwAwayCommand =
    parseTailArgs True ["throw", "away"] (parseTrinkets . unwords) $ \m p ->
        (do
            let authorID = (userId . messageAuthor) m
                channel  = messageChannel m
            ts <- getParsed p
            void $ modifyUser authorID $ over userTrinkets (MS.\\ ts)
            void $ modifyLocation "junkyard" $ over locationTrinkets (<> ts)
            sendMessage channel "Good riddance..."
        )

useCommand :: Command
useCommand = parseTailArgs False ["use"] (parseTrinkets . unwords) $ \m p -> do
    ts <- getParsed p <&> MS.elems

    ownsOrComplain (userId . messageAuthor $ m)
                   (fromTrinkets . MS.fromList $ ts)
    forConcurrently'_ ts $ \t -> do
        action  <- trinketActs (Left . userId . messageAuthor $ m) t
        trinket <- getTrinketOr Complaint t
        sendAsEmbed m t trinket action

  where
    sendAsEmbed m trinketID trinketData (action, effect) = do
        displayedTrinket <- displayTrinket trinketID trinketData
        effect'          <- displayEffect effect
        restCall'_ $ CreateMessageEmbed
            (messageChannel m)
            (voiceFilter "You hear something shuffle...")
            (mkEmbed "Use"
                     (displayedTrinket <> " " <> action <> "." <> effect')
                     []
                     (Just $ trinketColour (trinketData ^. trinketRarity))
            )

    displayEffect = do
        \case
            Nothing            -> pure ""
            Just (Become name) -> do
                display <- getTrinketByName name Common
                    >>= uncurry displayTrinket
                pure $ "\n\n*It becomes " <> display <> ".*"
            Just (Create name) -> do
                display <- getTrinketByName name Common
                    >>= uncurry displayTrinket
                pure $ "\n\n*It creates " <> display <> ".*"
            Just (Nickname name) ->
                pure $ "\n\n*It names you \"" <> name <> "\".*"
            Just (Credits n) ->
                pure $ "\n\n*It gives you " <> show n <> " credits!*"
            Just SelfDestruct -> pure "\n\n*It destroys itself.*"
            Just Ascend       -> pure "\n\n*It grants you a point!*"
            Just Descend      -> pure "\n\n*It takes a point from you...*"

wealthCommand :: Command
wealthCommand = Command
    { parser = \m -> if messageText m `elem` ["balance", "what is my net worth"]
                   then Just ()
                   else Nothing
    , command = \m _ -> do
        let (part1, part2) =
                if odds 0.1 . mkStdGen . fromIntegral . messageId $ m
                    then ("You own a lavish ", " credits.")
                    else
                        ( "You are a dirt-poor peon. You have only "
                        , " credits to your name."
                        )
        credits <- getUser (userId $ messageAuthor m)
            <&> maybe 0 (view userCredits)
        sendMessage (messageChannel m) $ part1 <> show credits <> part2
    , isSpammy = False
    }

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
    sendMessage (messageChannel m) output

whereCommand :: Command
whereCommand = oneArg False "where" $ \msg _ -> do
    randomEmoji <- randomChoice emojiPlaces <$> newStdGen
    reactToMessage randomEmoji msg

whoCommand :: Command
whoCommand = oneArg False "who" $ \m t -> do
    member <- randomMember
    sendMessage (messageChannel m)
        $  "<@"
        <> (show . userId . memberUser) member
        <> "> "
        <> t

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
        maybe (return ()) (sendMessage $ messageChannel m) wgw
    , noArgs False "tell me about yourself" $ \m -> do
        sendUnfilteredMessage (messageChannel m)
            $  voiceFilter
                   "this is a server about collectively modifying the bot that governs it... as long as i allow it, of course."
            <> " https://github.com/cosmicoptima/dictator"

    -- economy commands
    , actCommand
    , arenaCommand
    , callMeCommand
    , combineCommand
    , flauntCommand
    , invCommand
    , invokeFuryInCommand
    , makeFightCommand
    , offerCommand
    , peekCommand
    , provokeCommand
    , putInCommand
    , rummageCommand
    , throwAwayCommand
    , useCommand
    , wealthCommand

    -- random/GPT commands
    , acronymCommand
    , boolCommand
    , helpCommand
    , oneArg False "how many" $ \m t -> do
        number :: Double <- liftIO normalIO <&> (exp . (+ 4) . (* 6))
        sendMessage (messageChannel m)
            $  show (round number :: Integer)
            <> " "
            <> t
    , oneArg False "ponder" $ \m t -> pontificate (messageChannel m) t
    , noArgs False "what is your latest dictum" $ const dictate
    , whoCommand
    , whereCommand

    -- admin commands
    , archiveCommand
    , evilCommand
    , shutUpCommand
    , noArgs False "time for bed" $ const stopDict

    -- debug commands
    , noArgs False "impersonate" $ \msg -> do
        restCall' $ DeleteMessage (messageChannel msg, messageId msg)
        member <- (userToMember . messageAuthor $ msg) >>= fromJustOr GTFO
        impersonateUserRandom member (messageChannel msg)
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
    , noArgs False "update the nicknames" $ \_ ->
        getMembers >>= mapConcurrently'_
            (\m -> when ((userId . memberUser) m /= dictId)
                $ updateUserNickname m
            )
    , christmasCmd "merry christmas"       Common
    , christmasCmd "merrier christmas"     Uncommon
    , christmasCmd "merriest christmas"    Rare
    , christmasCmd "merriestest christmas" Legendary

    -- We probably want this at the bottom!
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
