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
import           Constants
import           Events
import           Game                           ( fromCredits
                                                , takeItems
                                                )
import           Game.Data
import           Game.Events
import           Game.Trade
import           Points                         ( updateUserNickname )
import           Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language          hiding ( tokens )

import           Discord                        ( RunDiscordOpts
                                                    ( discordOnEvent
                                                    , discordOnStart
                                                    , discordToken
                                                    )
                                                , def
                                                , runDiscord
                                                )
import           Discord.Requests
import           Discord.Types

import           Control.Lens
-- import           Data.Bits
import qualified Data.MultiSet                 as MS
import qualified Data.Text                     as T
import           Data.Time.Clock                ( addUTCTime
                                                , getCurrentTime
                                                )
import qualified Database.Redis                as DB
import           System.Random
import           UnliftIO
import           UnliftIO.Concurrent            ( forkIO
                                                , threadDelay
                                                )


-- forbidden word handling
--------------------------

updateForbiddenWords :: DictM ()
updateForbiddenWords = do
    pure ()
    -- fullWordList <- liftIO getWordList

    -- wordList     <- replicateM 10 (newStdGen <&> randomChoice fullWordList)
    -- void $ modifyGlobal (set globalForbidden wordList)

    -- general    <- getGeneralChannel <&> channelId
    -- globalData <- getGlobal
    -- pinId      <- case globalData ^. globalWarning of
    --     Just pin -> return pin
    --     Nothing  -> do
    --         pinId <- restCall' (CreateMessage general "aa") <&> messageId
    --         void . modifyGlobal $ set globalWarning (Just pinId)
    --         restCall' $ AddPinnedMessage (general, pinId)
    --         return pinId

    -- rng <- newStdGen
    -- let warning = voiceFilter $ if odds 0.5 rng
    --         then
    --             "The following words and terms are hereby illegal, forbidden, banned and struck from all records, forever: "
    --         else
    --             "I declare that the following so-called words do not exist, have never existed, and will continue to not exist: "

    --     embed = mkEmbed "Forbidden words:"
    --                     (T.intercalate ", " wordList)
    --                     []
    --                     (Just 0xFF0000)

    -- restCall'_ $ EditMessage (general, pinId) warning (Just embed)


forbidUser :: ChannelId -> Text -> UserId -> DictM ()
forbidUser channel badWord user = do

    sendMessage channel bannedWordMessage
    takeItems user $ fromCredits 20

    setUserPermsInChannel False channel user 0x800
    -- 15 seconds as microseconds
    threadDelay 15000000
    setUserPermsInChannel True channel user 0x800
  where
    bannedWordMessage =
        "You arrogant little insect! You clearly wish to disrespect my authority by uttering a word so vile as '"
            <> badWord
            <> "', so your credits will be *severely* decremented."

handleForbidden :: Message -> DictM ()
handleForbidden m = do
    messageForbidden <- messageForbiddenWith content
    case messageForbidden of
        Just word -> do
            forbidUser (messageChannel m) word (userId author)
            updateForbiddenWords
        Nothing -> return ()
  where
    author  = messageAuthor m
    content = messageText m

    messageForbiddenWith message = do
        forbidden <- getGlobal <&> view globalForbidden
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

handleRandomTrade :: Message -> DictM ()
handleRandomTrade m = randomIO >>= \c -> if c > (0.015 :: Double)
    then pure ()
    else do
        trade <- randomTrade dictId
        void $ openTrade (messageChannel m) trade


-- messages
-----------------

-- | Handle a message assuming that either is or isn't a command.
handleMessage :: Message -> DictM ()
handleMessage m = unless (userIsBot . messageAuthor $ m) $ do
    lift . logErrorsInChannel (messageChannel m) $ do
        let author = userId . messageAuthor $ m
        commandRun <- handleCommand m

        handleRandomTrade m
        unless commandRun $ do
            lucky <- oddsIO 0.001
            when lucky . void . modifyUser author $ over userPoints (+ 1)

            handleReact m
            handleOwned m
            handlePontificate m
            handleForbidden m
-- events
---------

seconds, minutes, hours, days :: Double -> Double
seconds = id
minutes = (* 60)
hours = (* 3600)
days = (* 86400)

data RandomEvent = RandomEvent
    { avgDelay    :: Double
    , randomEvent :: DictM ()
    }

data ScheduledEvent = ScheduledEvent
    { absDelay       :: Double
    , scheduledEvent :: DictM ()
    }

randomEvents :: [RandomEvent]
randomEvents =
    [ -- gmposting and gnposting
      RandomEvent { avgDelay = days 1, randomEvent = sendMessageToGeneral "gm" }
    , RandomEvent { avgDelay = days 1, randomEvent = sendMessageToGeneral "gn" }
    -- trades
    , RandomEvent { avgDelay = minutes 45, randomEvent = dictatorRandomTrade }
    -- declarations and decrees
    , RandomEvent { avgDelay = minutes 90, randomEvent = dictate }
    -- trigger events in locations
    , RandomEvent { avgDelay = hours 8, randomEvent = dictatorAddToArena }
    , RandomEvent { avgDelay = seconds 5, randomEvent = mayLocationEvent }
    ]
  where
    dictatorRandomTrade = do
        trade   <- randomTrade dictId
        general <- channelId <$> getGeneralChannel
        void $ openTrade general trade

    mayLocationEvent = do
        locations <- getallLocation
        forConcurrently'_
            locations
            (\(place, _) ->
                randomIO
                    >>= flip when (randomLocationEvent place)
                    .   (> (0.99993 :: Double))
            )

scheduledEvents :: [ScheduledEvent]
scheduledEvents =
    [ ScheduledEvent { absDelay       = hours 2
                     , scheduledEvent = updateForbiddenWords
                     }
    , ScheduledEvent { absDelay       = minutes 30
                     , scheduledEvent = void runArenaFight
                     }
    ]

performRandomEvents :: DictM ()
performRandomEvents = do
    threadDelay 100000
    mapConcurrently'_ maybePerformRandomEvent randomEvents
    performRandomEvents

  where
    maybePerformRandomEvent :: RandomEvent -> DictM ()
    maybePerformRandomEvent (RandomEvent rngDelay event) = do
        rng <- newStdGen
        when (odds (0.1 / rngDelay) rng) event

startScheduledEvents :: DictM ()
startScheduledEvents = do
    mapConcurrently'_ scheduledEventLoop scheduledEvents
  where
    scheduledEventLoop sched@(ScheduledEvent delay event) = do
        -- Sleep for the required amount of time, noting that this is in nanoseconds.
        threadDelay . secsToUs $ delay
        void event
        scheduledEventLoop sched
    secsToUs = round . (* 1e6)


-- main
-------

startHandler :: DB.Connection -> DH ()
startHandler conn = do
    logErrors' conn $ sendMessageToGeneral "Rise and shine!"
    mapConcurrently_
        (forkIO . logErrors' conn)
        [ unbanUsersFromGeneral
        , performRandomEvents
        , startScheduledEvents
        , forgiveDebt
        , threadDelay 5000000 >> updateForbiddenWords
        , createChannelIfDoesn'tExist "arena"   False
        , createChannelIfDoesn'tExist "botspam" False
        , createChannelIfDoesn'tExist "log"     True
        , threadDelay 5000000 >> setChannelPositions
        , createRarityEmojisIfDon'tExist
        -- , removeNicknamePerms
        , deleteOldPins
        ]
  where
    forgiveDebt = getMembers >>= mapConcurrently'_
        (\m -> modifyUser (userId . memberUser $ m) $ over userCredits (max 0))

    unbanUsersFromGeneral = do
        general <- getGeneralChannel
        getMembers >>= mapConcurrently'_
            (\m -> do
                setUserPermsInChannel True
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

    deleteOldPins = do
        g <- channelId <$> getGeneralChannel
        forM_
                [ 924953651045343242
                , 924953694544478239
                , 929770134984335370
                , 930251914812219514
                , 931779531155582976
                ]
            $ \m -> restCall'_ $ DeleteMessage (m, g)

    -- removeNicknamePerms = do
    --     everyoneRole <- getEveryoneRole
    --     let newPerms = rolePerms everyoneRole .&. (-67108865)
    --     void . restCall' $ ModifyGuildRole
    --         pnppcId
    --         (roleId everyoneRole)
    --         (ModifyGuildRoleOpts Nothing (Just newPerms) Nothing Nothing Nothing
    --         )





eventHandler :: DB.Connection -> Event -> DH ()
eventHandler conn event = logErrors' conn $ case event of
    MessageCreate m   -> handleMessage m

    MessageUpdate c m -> do
        message  <- restCall' $ GetChannelMessage (c, m)
        -- Only respond to edited messages that are less than a couple minutes old to reduce spam.
        realTime <- liftIO getCurrentTime
        when (120 `addUTCTime` messageTimestamp message >= realTime)
            $ handleMessage message

    GuildMemberAdd _ m -> do
        void $ modifyUser (userId . memberUser $ m) (over userCredits (+ 50))
        updateUserNickname m

    MessageReactionAdd react -> do
        when ((emojiName . reactionEmoji) react `elem` handshakes) $ do
            getTrade message >>= \case
                Just trade -> handleTrade channel message trade author
                _          -> return ()

        when ((emojiName . reactionEmoji) react `elem` zippers) $ do
            messageObject <- restCall' $ GetChannelMessage (channel, message)
            handleCensor channel messageObject author
      where
        -- TODO find out which one of these is real
        handshakes = ["handshake", ":handshake:", "🤝"]
        zippers    = ["zipper_mouth", ":zipper_mouth:", "🤐"]

        message    = reactionMessageId react
        channel    = reactionChannelId react
        author     = reactionUserId react


    _ -> return ()

handleCensor :: ChannelId -> Message -> UserId -> DictM ()
handleCensor channel message censor = do
    ownedWords <- view userWords <$> getUserOr Fuckup censor
    let _victim       = userId . messageAuthor $ message
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
        getJ1With (J1Opts 0.85 0.85) tokens
        . T.strip
        $ "A dictator on an online forum toys with his subjects by replacing their words.\n"
        <> T.unlines (examples template)
    result <- maybe (replaceWords text replaced) return
        $ (listToMaybe . T.lines) response
    return $ changeVoiceUnquot result

  where
    examples template =
        [ "This: I am craving [food] right now\nBecomes: I am craving [thick cock] right now"
        , "This: i am so [fucking] tired\nBecomes: i am so [boring, smelly and] tired"
        , "This: [celeste] why would [you] do that\nBecomes: [dictator] why would [you're great] do that"
        , "This: omg i [love] you\nBecomes: omg i [wish to murder] you"
        -- , "This: [huh]\nBecomes: [i love you]"
        , "This: [this] is so great lmao\nBecomes: [our glorious dictator] is so great lmao"
        , "This: oh, fuck [off]\nBecomes: oh, fuck [me]"
        , "This: " <> template <> "\nBecomes:"
        ]
    replaceWord w =
        T.unwords
            . map (\w' -> if w' == w then "[" <> w <> "]" else w')
            . T.words

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
    void . runDiscord $ def { discordToken   = fromString token
                            , discordOnStart = startHandler conn
                            , discordOnEvent = eventHandler conn
        -- Enable intents so we can see username updates.
        -- , discordGatewayIntent = def { gatewayIntentMembers = True }
                            }
