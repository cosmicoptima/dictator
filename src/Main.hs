{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Main
    ( main
    ) where

import           Relude                  hiding ( First
                                                , get
                                                )

import           Commands
import           Events
import           Game.Data
import           Utils
import           Utils.DictM
import           Utils.Discord

import           Discord                        ( RunDiscordOpts
                                                    ( discordGatewayIntent
                                                    , discordOnEvent
                                                    , discordOnStart
                                                    , discordToken
                                                    )
                                                , def
                                                , runDiscord
                                                )
import           Discord.Requests
import           Discord.Types

import           Constants
import           Control.Lens
import qualified Data.MultiSet                 as MS
import qualified Data.Text                     as T
import           Data.Time.Clock                ( addUTCTime
                                                , getCurrentTime
                                                )
import qualified Database.Redis                as DB
import           Game                           ( fromCredits
                                                , takeItems
                                                )
import           Game.Events
import           Game.Trade
import           Points                         ( updateUserNickname )
import           System.Random
import           UnliftIO
import           UnliftIO.Concurrent            ( forkIO
                                                , threadDelay
                                                )
import           Utils.Language                 ( getJ1
                                                , getJ1FromContext
                                                )


-- forbidden word handling
--------------------------

updateForbiddenWords :: DB.Connection -> DictM ()
updateForbiddenWords conn = do
    fullWordList <- liftIO getWordList

    wordList     <- replicateM 10 (newStdGen <&> randomChoice fullWordList)
    void $ modifyGlobal conn (set globalForbidden wordList)

    general    <- getGeneralChannel <&> channelId
    globalData <- getGlobal conn
    pinId      <- case globalData ^. globalWarning of
        Just pin -> return pin
        Nothing  -> do
            pinId <- restCall' (CreateMessage general "aa") <&> messageId
            void . modifyGlobal conn $ set globalWarning (Just pinId)
            restCall' $ AddPinnedMessage (general, pinId)
            return pinId

    rng <- newStdGen
    let warning = voiceFilter $ if odds 0.5 rng
            then
                "The following words and terms are hereby illegal, forbidden, banned and struck from all records, forever: "
            else
                "I declare that the following so-called words do not exist, have never existed, and will continue to not exist: "

        embed = mkEmbed "Forbidden words:"
                        (T.intercalate ", " wordList)
                        []
                        (Just 0xFF0000)

    restCall'_ $ EditMessage (general, pinId) warning (Just embed)


forbidUser :: DB.Connection -> ChannelId -> Text -> UserId -> DictM ()
forbidUser conn channel badWord user = do

    sendMessage channel bannedWordMessage
    takeItems conn user $ fromCredits 20

    setUserPermsInChannel False channel user 0x800
    -- 15 seconds as microseconds
    threadDelay 15000000
    setUserPermsInChannel True channel user 0x800
  where
    bannedWordMessage =
        "You arrogant little insect! You clearly wish to disrespect my authority by uttering a word so vile as '"
            <> badWord
            <> "', so your credits will be *severely* decremented."

handleForbidden :: DB.Connection -> Message -> DictM ()
handleForbidden conn m = do
    messageForbidden <- messageForbiddenWith content
    case messageForbidden of
        Just word -> do
            forbidUser conn (messageChannel m) word (userId author)
            updateForbiddenWords conn
        Nothing -> return ()
  where
    author  = messageAuthor m
    content = messageText m

    messageForbiddenWith message = do
        forbidden <- getGlobal conn <&> view globalForbidden
        return . find (`elem` forbidden) . tokenizeMessage $ message


-- GPT events
-------------

handlePontificate :: Message -> DictM ()
handlePontificate m =
    when (odds 0.02 . mkStdGen . fromIntegral . messageId $ m)
        $ pontificate channel content
  where
    channel = messageChannel m
    content = messageText m


-- owned!!!
-----------

handleOwned :: Message -> DictM ()
handleOwned m = when ownagePresent $ do
    [rngChoice, rngEmoji, rngHead] <- replicateM 3 newStdGen
    let emoji   = randomChoice [ownedEmoji, ownedEmoji, "skull"] rngEmoji
        channel = messageChannel m

    if
        | odds 0.02 rngHead
        -> sendMessage
            channel
            "Never say 'owned' again or I will rip your head from that stupid tiny neck of yours, asshole."
        | isCeleste
        -> randomChoice
            ( sendMessage channel "shut the fuck up, celeste"
            : replicate 2 (reactToMessage emoji m)
            )
            rngChoice
        | otherwise
        -> reactToMessage emoji m

  where
    isCeleste     = ((== 140541286498304000) . userId . messageAuthor) m
    ownagePresent = (T.isInfixOf "owned" . messageText) m

handleReact :: Message -> DictM ()
handleReact msg = do
    rng <- newStdGen
    when (odds 0.05 rng) $ do
        emojiList <-
            randomChoice
                    [ emojiPositive
                    , emojiNeutral
                    , emojiNegative
                    , emojiEverything
                    ]
                <$> newStdGen
        randomEmoji <- randomChoice emojiList <$> newStdGen
        reactToMessage randomEmoji msg

handleRandomTrade :: DB.Connection -> Message -> DictM ()
handleRandomTrade conn m = randomIO >>= \c -> if c > (0.015 :: Double)
    then pure ()
    else do
        trade <- randomTrade conn dictId
        void $ openTrade conn (messageChannel m) trade


-- messages
-----------------

-- | Handle a message assuming that either is or isn't a command.
handleMessage :: DB.Connection -> Message -> DH ()
handleMessage conn m = unless (userIsBot . messageAuthor $ m) $ do
    logErrorsInChannel (messageChannel m) $ do
        let author = userId . messageAuthor $ m
        commandRun <- handleCommand conn m

        handleRandomTrade conn m
        unless commandRun $ do
            lucky <- oddsIO 0.001
            when lucky . void . modifyUser conn author $ over userPoints (+ 1)

            handleReact m
            handleOwned m
            handlePontificate m
            handleForbidden conn m
-- events
---------

seconds, minutes, hours, days :: Double -> Double
seconds = id
minutes = (* 60)
hours = (* 3600)
days = (* 86400)

data RandomEvent = RandomEvent
    { avgDelay    :: Double
    , randomEvent :: DB.Connection -> DictM ()
    }

data ScheduledEvent = ScheduledEvent
    { absDelay       :: Double
    , scheduledEvent :: DB.Connection -> DictM ()
    }

randomEvents :: [RandomEvent]
randomEvents =
    [ -- gmposting and gnposting
      RandomEvent { avgDelay    = days 1
                  , randomEvent = const $ sendMessageToGeneral "gm"
                  }
    , RandomEvent { avgDelay    = days 1
                  , randomEvent = const $ sendMessageToGeneral "gn"
                  }
    -- trades
    , RandomEvent { avgDelay = minutes 45, randomEvent = dictatorRandomTrade }
    -- declarations and decrees
    , RandomEvent { avgDelay = minutes 90, randomEvent = const dictate }
    -- trigger events in locations
    , RandomEvent { avgDelay = hours 8, randomEvent = dictatorAddToArena }
    , RandomEvent { avgDelay = seconds 5, randomEvent = mayLocationEvent }
    ]
  where
    dictatorRandomTrade conn = do
        trade   <- randomTrade conn dictId
        general <- channelId <$> getGeneralChannel
        void $ openTrade conn general trade

    mayLocationEvent c = do
        locations <- getallLocation c
        forConcurrently'_
            locations
            (\(place, _) ->
                randomIO
                    >>= flip when (randomLocationEvent c place)
                    .   (> (0.99993 :: Double))
            )

scheduledEvents :: [ScheduledEvent]
scheduledEvents =
    [ ScheduledEvent { absDelay       = hours 2
                     , scheduledEvent = updateForbiddenWords
                     }
    , ScheduledEvent { absDelay       = minutes 30
                     , scheduledEvent = void . runArenaFight
                     }
    ]

performRandomEvents :: DB.Connection -> DictM ()
performRandomEvents conn = do
    threadDelay 100000
    lift . void . forkIO $ mapConcurrently_ maybePerformRandomEvent randomEvents
    performRandomEvents conn

  where
    maybePerformRandomEvent (RandomEvent rngDelay event) = do
        rng <- newStdGen
        when (odds (0.1 / rngDelay) rng) . dieOnErrors $ event conn

startScheduledEvents :: DB.Connection -> DictM ()
startScheduledEvents conn = do
    lift $ mapConcurrently_ scheduledEventLoop scheduledEvents
  where
    scheduledEventLoop sched@(ScheduledEvent delay event) = do
        -- Sleep for the required amount of time, noting that this is in nanoseconds.
        threadDelay . secsToUs $ delay
        dieOnErrors $ event conn
        scheduledEventLoop sched
    secsToUs = round . (* 1e6)


-- main
-------

startHandler :: DB.Connection -> DH ()
startHandler conn = do
    dieOnErrors $ sendMessageToGeneral "Rise and shine!"
    mapConcurrently_
        (forkIO . dieOnErrors)
        [ unbanUsersFromGeneral
        , performRandomEvents conn
        , startScheduledEvents conn
        , forgiveDebt
        , threadDelay 5000000 >> updateForbiddenWords conn
        , createChannelIfDoesn'tExist "arena"   False
        , createChannelIfDoesn'tExist "botspam" False
        , createChannelIfDoesn'tExist "log"     True
        , threadDelay 5000000 >> setChannelPositions
        , createRarityEmojisIfDon'tExist
        ]
  where
    forgiveDebt = getMembers >>= lift . mapConcurrently_
        (\m -> dieOnErrors $ modifyUser conn (userId . memberUser $ m) $ over
            userCredits
            (max 0)
        )

    unbanUsersFromGeneral = do
        general <- getGeneralChannel
        getMembers >>= lift . mapConcurrently_
            (\m -> do
                forkIO . dieOnErrors $ setUserPermsInChannel
                    True
                    (channelId general)
                    (userId . memberUser $ m)
                    0x800
            )

    createChannelIfDoesn'tExist name forbidden = getChannelNamed name >>= maybe
        (do
            everyoneID <- getEveryoneRole <&> roleId
            restCall'_ $ CreateGuildChannel
                pnppcId
                name
                ([ Overwrite everyoneID "role" 0 2048 | forbidden ])
                (CreateGuildChannelOptsText Nothing Nothing Nothing Nothing)
        )
        (const $ return ())

    setChannelPositions = do
        [general, arena, botspam, log] <- mapConcurrently'
            (getChannelNamed >=> maybe (die "channel should exist") return)
            ["general", "arena", "botspam", "log"]
        setPosition general 0
        setPosition arena   1
        setPosition botspam 2
        setPosition log     3
      where
        setPosition channel pos = restCall'_ $ ModifyChannel
            (channelId channel)
            (ModifyChannelOpts Nothing
                               (Just pos)
                               Nothing
                               Nothing
                               Nothing
                               Nothing
                               Nothing
                               Nothing
            )

    createRarityEmojisIfDon'tExist = mapConcurrently'_
        createRarityEmojiIfDoesn'tExist
        ["common", "uncommon", "rare", "legendary"]

    createRarityEmojiIfDoesn'tExist name = do
        emojis <- restCall' $ ListGuildEmojis pnppcId
        unless (name `elem` map emojiName emojis) $ do
            image <-
                readFileBS ("assets/" <> toString name <> ".png")
                    <&> parseEmojiImage
            case image of
                Left e ->
                    sendMessageToGeneral
                        $  "couldn't create emoji "
                        <> name
                        <> ": "
                        <> e
                Right i -> restCall'_ $ CreateGuildEmoji pnppcId name i

eventHandler :: DB.Connection -> Event -> DH ()
eventHandler conn = \case
    MessageCreate m   -> handleMessage conn m

    MessageUpdate c m -> logErrors $ do
        message  <- restCall' $ GetChannelMessage (c, m)
        -- Only respond to edited messages that are less than a couple minutes old to reduce spam.
        realTime <- liftIO getCurrentTime
        when (120 `addUTCTime` messageTimestamp message >= realTime)
            $ lift (handleMessage conn message)

    GuildMemberAdd _ m -> logErrors $ do
        void $ modifyUser conn
                          (userId . memberUser $ m)
                          (over userCredits (+ 50))
        updateUserNickname conn m

    GuildMemberUpdate _ _ user _ -> logErrors $ userToMember user >>= \case
        Just member -> updateUserNickname conn member
        Nothing     -> return ()

    MessageReactionAdd react -> logErrorsInChannel channel $ do
        when ((emojiName . reactionEmoji) react `elem` handshakes) $ do
            getTrade conn message >>= \case
                Just trade -> handleTrade conn channel message trade author
                _          -> return ()

        when ((emojiName . reactionEmoji) react `elem` zippers) $ do
            messageObject <- restCall' $ GetChannelMessage (channel, message)
            handleCensor conn channel messageObject author
      where
        -- TODO find out which one of these is real
        handshakes = ["handshake", ":handshake:", "🤝"]
        zippers    = ["zipper_mouth", ":zipper_mouth:", "🤐"]

        message    = reactionMessageId react
        channel    = reactionChannelId react
        author     = reactionUserId react


    _ -> return ()

handleCensor :: DB.Connection -> ChannelId -> Message -> UserId -> DictM ()
handleCensor conn channel message censor = do
    ownedWords <- view userWords <$> getUserOr Fuckup conn censor
    let victim        = userId . messageAuthor $ message
        postWords     = tokenizeMessage . messageText $ message
        censoredWords = MS.fromList postWords `MS.intersection` ownedWords

    unless (MS.null censoredWords) $ do
        replacement <- replaceWords (messageText message)
                                    (MS.elems censoredWords)
        sendUnfilteredMessage channel
            $  voiceFilter "I think you meant the following:"
            <> "\n\n"
            <> replacement

replaceWords :: Text -> [Text] -> DictM Text
replaceWords text replaced = do
    let stripped = T.filter (`notElem` ['[', ']']) text
        template = foldr replaceWord stripped replaced
        tokens   = (+ 10) . length . T.words $ text

    response <-
        getJ1With (J1Opts 0.7 0.7) tokens
        $ "A dictator on an online forum toys with his subjects by replacing their words.\n"
        <> T.unlines (examples template)
    result <- maybe (replaceWords text replaced) return
        $ (listToMaybe . T.lines) response
    return $ changeVoiceUnquot result

  where
    examples template =
        [ "This: I am craving [food] right now Becomes: I am craving [thick cock] right now"
        , "This: i am so [fucking] tired Becomes: i am so [boring, smelly and] tired"
        , "This: [celeste] why would [you] do that Becomes: [dictator] why would [you're great] do that"
        , "This: omg i [love] you Becomes: omg i [wish to murder] you"
        , "This: [huh] Becomes: [i love you]"
        , "This: [this] is so great lmao Becomes: [our glorious dictator] is so great lmao"
        , "This: oh, fuck [off] Becomes: oh, fuck [me]"
        , "This: " <> template <> " Becomes:"
        ]
    replaceWord w = T.replace w ("[" <> w <> "]")

    changeVoiceUnquot message =
        let (prefix, suffix) = splitFirst '[' message
        in  if T.null suffix then prefix else prefix <> changeVoiceQuot suffix
    changeVoiceQuot message =
        let (prefix, suffix) = splitFirst ']' message
        in  if T.null suffix
                then voiceFilter prefix
                else voiceFilter prefix <> changeVoiceUnquot suffix

    splitFirst char message = case T.split (== char) message of
        []                -> ("", "")
        [prefix         ] -> (prefix, "")
        (prefix : suffix) -> (prefix, T.intercalate (T.singleton char) suffix)

main :: IO ()
main = do
    token <- readFile "token.txt"
    conn  <- DB.checkedConnect DB.defaultConnectInfo
    void . runDiscord $ def
        { discordToken         = fromString token
        , discordOnStart       = startHandler conn
        , discordOnEvent       = eventHandler conn
        -- Enable intents so we can see username updates.
        , discordGatewayIntent = def { gatewayIntentMembers = True }
        }
