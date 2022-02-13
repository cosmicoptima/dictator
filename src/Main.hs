{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main
    ( main
    ) where

import           Relude                  hiding ( First
                                                , get
                                                )

import           Commands
import           Constants
import           Events
import           Game
import           Game.Data
import           Game.Effects            hiding ( hours
                                                , minutes
                                                , seconds
                                                )
import           Game.Events
import           Game.NPCs
import           Game.Trade
import           Points                         ( updateUserNickname )
import           Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language          hiding ( tokens )

import           Discord.Requests
import           Discord.Types

import           Control.Lens
import qualified Data.MultiSet                 as MS
import qualified Data.Set                      as Set
import           Data.String.Interpolate
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
import           Utils.Twitter

import           Web.Twitter.Conduit

import           Discord
import           Game.Items
import Game.Roles (fixRoles)



-- forbidden word handling
--------------------------

updateEncouragedWords :: DictM ()
updateEncouragedWords = do
    wordList <- replicateM 3 $ liftIO randomWord
    void $ modifyGlobal
        (set globalEncouraged wordList . set globalSubmitted Set.empty)

    deleteOldPins
    general <- getGeneralChannel <&> channelId

    let warning =
            voiceFilter
                "I hereby declare that the following words to be encouraged, novel, inspired and generally of a higher class than others:"

        embed = (mkEmbed "Today's encouraged words:"
                         (T.intercalate ", " wordList)
                         []
                         (Just 0x00FF00)
                )
            { createEmbedFooterText =
                "Submit these great words with `submit \"word\"` for great praise."
            }
    res <- restCall' $ CreateMessageEmbed general warning embed
    restCall'_ $ AddPinnedMessage (general, messageId res)

deleteOldPins :: DictM ()
deleteOldPins = do
    general <- channelId <$> getGeneralChannel
    pins    <- restCall' $ GetPinnedMessages general
    -- Leave up manually pinned posts
    let allowedPins =
            [ 882079724120203284
            , 932742319474606181
            , 940294272421343233
            , 941925453327908904
            ]
    forConcurrently'_ pins $ \m -> when (messageId m `notElem` allowedPins) $ do
        restCall'_ $ DeletePinnedMessage (general, messageId m)

-- GPT events
-------------

handleImpersonate :: Message -> DictM ()
handleImpersonate m =
    when (odds 0.03 . mkStdGen . pred . fromIntegral . messageId $ m)
        $   randomMember
        >>= \member -> if (userId . memberUser) member == dictId
                then randomNPCSpeak
                else impersonateUserRandom member (messageChannel m)
  where
    randomNPCSpeak = do
        npcs <- getallNPC <&> map fst
        npc  <- newStdGen <&> randomChoice npcs
        npcSpeak (messageChannel m) npc

handleNPCSpeak :: Message -> DictM ()
handleNPCSpeak m =
    when (odds 0.03 . mkStdGen . succ . fromIntegral . messageId $ m)
        $ randomNPCSpeakGroup (messageChannel m)

handlePontificate :: Message -> DictM ()
handlePontificate m =
    when (odds 0.04 . mkStdGen . fromIntegral . messageId $ m)
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

handleEffects :: Message -> DictM ()
handleEffects m = do
    let authorID = (userId . messageAuthor) m
    effects <- map getEffect . Set.elems . view userEffects <$> getUser authorID
    forM_ effects $ \eff -> everyMessage eff m

handleRandomInflict :: Message -> DictM ()
handleRandomInflict m = randomIO >>= \c -> when (c < (0.005 :: Double)) $ do
    (effect, member) <- inflictRandomly
    let userID = (userId . memberUser) member
    sendMessage
        (messageChannel m)
        [i|You peons dare to defy me? No more; <@#{userID}> is now #{effectName effect}.|]

handleRandomTrade :: Message -> DictM ()
handleRandomTrade m = randomIO >>= \c -> when (c < (0.015 :: Double)) $ do
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

        handleEffects m
        handleRandomInflict m
        handleRandomTrade m
        unless commandRun $ do
            lucky <- oddsIO 0.001
            when lucky . void . modifyUser author $ over userPoints (+ 1)

            handleReact m
            handleOwned m
            handlePontificate m
            handleImpersonate m
            handleNPCSpeak m
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

data DailyEvent = DailyEvent
    { dailyEvent :: DictM ()
    }

randomEvents :: [RandomEvent]
randomEvents =
    [ -- gmposting and gnposting
      RandomEvent { avgDelay = days 1, randomEvent = sendMessageToGeneral "gm" }
    , RandomEvent { avgDelay = days 1, randomEvent = sendMessageToGeneral "gn" }
    -- trades
    , RandomEvent { avgDelay = minutes 60, randomEvent = dictatorRandomTrade }
    -- declarations and decrees
    , RandomEvent { avgDelay = minutes 100, randomEvent = dictate }
    , RandomEvent { avgDelay = minutes 100, randomEvent = postImage }
    -- trigger events in locations
    , RandomEvent { avgDelay = hours 8, randomEvent = dictatorAddToArena }
    , RandomEvent { avgDelay = seconds 5, randomEvent = mayLocationEvent }
    -- NPC speak
    , RandomEvent
        { avgDelay    = hours 1
        , randomEvent = getGeneralChannel >>= randomNPCSpeakGroup . channelId
        }
    ]
  where
    dictatorRandomTrade = do
        trade   <- randomTrade dictId
        general <- channelId <$> getGeneralChannel
        void $ openTrade general trade

    mayLocationEvent = do
        locations <- getallLocation
        forConcurrently'_ locations $ \(place, _) ->
            randomIO
                >>= flip when (randomLocationEvent place)
                .   (> (0.99993 :: Double))


scheduledEvents :: [ScheduledEvent]
scheduledEvents =
    [ ScheduledEvent { absDelay       = minutes 30
                     , scheduledEvent = void runArenaFight
                     }
    , ScheduledEvent { absDelay = 1, scheduledEvent = runEffects }
    , ScheduledEvent { absDelay       = minutes 5
                     , scheduledEvent = performDailyEvents
                     }
    ]

dailyEvents :: [DailyEvent]
dailyEvents = [DailyEvent { dailyEvent = updateEncouragedWords }]

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

performDailyEvents :: DictM ()
performDailyEvents = do
    realDay <- utctDay <$> liftIO getCurrentTime
    lastDay <- view globalDay <$> getGlobal
    when (realDay /= lastDay) $ do
        modifyGlobal_ $ set globalDay realDay
        mapM_ dailyEvent dailyEvents


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

startHandler :: Env -> DH ()
startHandler env = do
    logErrors' env $ sendMessageToGeneral "Rise and shine!"
    mapConcurrently_
        (forkIO . logErrors' env)
        [ unbanUsersFromGeneral
        , performRandomEvents
        , startScheduledEvents
        , createChannelIfDoesn'tExist "arena"   False
        , createChannelIfDoesn'tExist "botspam" False
        , createChannelIfDoesn'tExist "log"     True
        , threadDelay 5000000 >> setChannelPositions
        , createRarityEmojisIfDon'tExist
        -- , removeNicknamePerms
        , addNewPins
        , fixRoles
        ]
  where
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
        ["common", "uncommon", "rare", "legendary", "mythic"]

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
                Right img -> restCall'_ $ CreateGuildEmoji pnppcId name img

    addNewPins = do
        general <- channelId <$> getGeneralChannel
        restCall' $ AddPinnedMessage (general, 941925453327908904)

    -- removeNicknamePerms = do
    --     everyoneRole <- getEveryoneRole
    --     let newPerms = rolePerms everyoneRole .&. (-67108865)
    --     void . restCall' $ ModifyGuildRole
    --         pnppcId
    --         (roleId everyoneRole)
    --         (ModifyGuildRoleOpts Nothing (Just newPerms) Nothing Nothing Nothing
    --         )


eventHandler :: Env -> Event -> DH ()
eventHandler env event = case event of
    MessageCreate m   -> logErrors' env $ handleMessage m

    MessageUpdate c m -> logErrors' env $ do
        message  <- restCall' $ GetChannelMessage (c, m)
        -- Only respond to edited messages that are less than a couple minutes old to reduce spam.
        realTime <- liftIO getCurrentTime
        when (120 `addUTCTime` messageTimestamp message >= realTime)
            $ handleMessage message

    GuildMemberAdd _ m -> logErrors' env $ do
        general                <- channelId <$> getGeneralChannel
        -- Guild members recieve a package of items to help them start.
        word                   <- liftIO randomWord
        (trinket, trinketData) <- randomTrinket
        let newMember = userId . memberUser $ m
            intro =
                voiceFilter
                    [i|Welcome, <@#{newMember}>. You will recieve the following, conditional on your continued compliance.|]
            newUserItems = Items { _itemTrinkets = MS.singleton trinket
                                 , _itemWords    = MS.singleton word
                                 , _itemUsers    = MS.empty
                                 , _itemCredits  = 40
                                 }
        display <- (\w -> [i|You are gifted #{w}.|])
            <$> displayItems newUserItems
        let embed = mkEmbed
                "Welcome"
                display
                []
                (Just $ trinketData ^. trinketRarity . to trinketColour)
        giveItems newMember newUserItems
        restCall'_ $ CreateMessageEmbed general intro embed
        updateUserNickname m

    MessageReactionAdd react ->
        flip runReaderT env . logErrorsInChannel channel $ do
            when ((emojiName . reactionEmoji) react `elem` handshakes) $ do
                getTrade message >>= \case
                    Just trade -> handleTrade channel message trade author
                    _          -> return ()

            when ((emojiName . reactionEmoji) react `elem` zippers) $ do
                messageObject <- restCall'
                    $ GetChannelMessage (channel, message)
                handleCensor channel messageObject author

            when ((emojiName . reactionEmoji) react `elem` birds) $ do
                -- We want to count the reactions, and we only get one here, so we get the rest.
                msgObj <- restCall' $ GetChannelMessage (channel, message)
                users  <- restCall' $ GetReactions
                    (channel, message)
                    (emojiName . reactionEmoji $ react)
                    (25, LatestReaction)
                global <- getGlobal
                let metThreshhold = length users >= 3
                    correctUser   = isDict . messageAuthor $ msgObj
                    notTweeted =
                        message `Set.notMember` (global ^. globalTweeted)
                when (metThreshhold && correctUser && notTweeted) $ do
                    sendReplyTo msgObj "Send tweet."
                    tweetId <- sendTweet . twitterFilter $ messageText msgObj
                    setGlobal $ global & globalTweeted %~ Set.insert message
                    sendUnfilteredReplyTo
                        msgObj
                        [i|https://twitter.com/nomic_dict/status/#{tweetId}|]

      where
        -- TODO find out which one of these is real
        handshakes    = ["handshake", ":handshake:", "🤝"]
        zippers       = ["zipper_mouth", ":zipper_mouth:", "🤐"]
        birds         = ["bird", ":bird:", "🐦"]

        message       = reactionMessageId react
        channel       = reactionChannelId react
        author        = reactionUserId react

        twitterFilter = T.replace "**" "" . T.replace "__" ""

    _ -> return ()

handleCensor :: ChannelId -> Message -> UserId -> DictM ()
handleCensor channel message censor = do
    ownedWords <- view (userItems . itemWords) <$> getUser censor
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
    creds <- liftIO twitterAuth
    let env = Env { envDb = conn, envTw = creds }
    res <- runDiscord $ def
        { discordToken         = fromString token
        , discordOnStart       = startHandler env
        , discordOnEvent       = eventHandler env
        , discordGatewayIntent = def { gatewayIntentMembers = True }
        }
    print res
        -- Enable intents so we can see user joins.
