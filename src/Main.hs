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
                                                    ( discordOnEvent
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
import           Control.Monad.Except           ( MonadError(throwError) )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( addUTCTime
                                                , getCurrentTime
                                                )
import qualified Database.Redis                as DB
import           Game                           ( fromCredits
                                                , giveItems
                                                , makeOfferEmbed
                                                , openOfferDesc
                                                , ownsOrComplain
                                                , punishWallet
                                                , takeItems
                                                , userOwns
                                                )
import           Game.Events
import           Game.Items                     ( parseItems )
import           Relude.Unsafe                  ( read )
import           System.Random
import           Text.Parsec                    ( ParseError
                                                , digit
                                                , many1
                                                , parse
                                                , string
                                                )
import           Text.Parsec.Text               ( Parser )
import           UnliftIO
import           UnliftIO.Concurrent            ( forkIO
                                                , threadDelay
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
            <> "', so your credit will be severely punished."

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
        return $ find (`elem` forbidden) . tokenizeMessage $ message


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
    (rngChoice, rngEmoji) <- split <$> newStdGen
    let emoji   = randomChoice [ownedEmoji, ownedEmoji, "skull"] rngEmoji
        channel = messageChannel m
    if isCeleste
        then randomChoice
            ( sendMessage channel "shut the fuck up, celeste"
            : replicate 2 (reactToMessage emoji m)
            )
            rngChoice
        else randomChoice
            ( sendMessage
                    channel
                    "Never say 'owned' again or I will rip your head from that stupid tiny neck of yours, asshole."
            : replicate 50 (reactToMessage emoji m)
            )
            rngChoice
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


-- messages
-----------------

-- | Handle a message assuming that either is or isn't a command.
handleMessage :: DB.Connection -> Message -> DH ()
handleMessage conn m = unless (userIsBot . messageAuthor $ m) $ do
    logErrorsInChannel (messageChannel m) $ do
        commandRun <- handleCommand conn m
        unless commandRun $ do
            channel <- getChannelByID . messageChannel $ m
            if channelName channel `elem` ["botspam", "arena"]
                then do
                    restCall'_ $ DeleteMessage (channelId channel, messageId m)
                    sendMessage (channelId channel)
                                "Speak up, I can't hear you."
                else do
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
    -- declarations and decrees
    , RandomEvent { avgDelay = minutes 90, randomEvent = const dictate }
    -- trigger events in locations
    , RandomEvent { avgDelay = hours 8, randomEvent = dictatorAddToArena }
    , RandomEvent { avgDelay = seconds 5, randomEvent = mayLocationEvent }
    ]
  where
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
    , ScheduledEvent { absDelay = minutes 30, scheduledEvent = giveCredits }
    ]
  where
    giveCredits = \c -> getMembers >>= mapConcurrently'_
        (\m -> modifyUser c (userId . memberUser $ m) $ over userCredits succ)

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
        , setOwnNickname
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

    setOwnNickname = restCall' $ ModifyCurrentUserNick pnppcId "dictator"

eventHandler :: DB.Connection -> Event -> DH ()
eventHandler conn = \case
    MessageCreate m   -> handleMessage conn m

    MessageUpdate c m -> logErrors $ do
        message  <- restCall' $ GetChannelMessage (c, m)
        -- Only respond to edited messages that are less than a couple minutes old to reduce spam.
        realTime <- liftIO getCurrentTime
        when (120 `addUTCTime` messageTimestamp message >= realTime)
            $ lift (handleMessage conn message)

    GuildMemberAdd _ m -> do
        logErrors $ removeTeamRoles conn
        logErrors $ modifyUser conn
                               (userId . memberUser $ m)
                               (over userCredits (+ 50))

    MessageReactionAdd react -> logErrorsInChannel channel $ do
        messageData <- restCall' (GetChannelMessage (channel, message))
        embed       <-
            maybe (throwError GTFO) return
            . listToMaybe
            . messageEmbeds
            $ messageData
        let isHandshake = (emojiName . reactionEmoji) react == "🤝"
            isOpenOffer = embedTitle embed == Just openOfferDesc

        when (isHandshake && isOpenOffer) $ do
            let personReacting = reactionUserId react
            personOffering <-
                getParsed
                . parseAuthor
                . fromMaybe ""
                . embedDescription
                $ embed
            demandedItems <- getValue "Demands" embed
            offeredItems  <- getValue "Offers" embed


            if personOffering == personReacting
                then sendMessage
                    channel
                    "Do you believe I can't tell humans apart? You can't accept your own offer. It has been cancelled instead."
                else do
                    ownsOrComplain conn personReacting demandedItems
                    -- Manual error handling and ownership checks because trades are delayed.
                    offersOwned <-
                        flip userOwns offeredItems
                            <$> getUserOr Fuckup conn personOffering
                    if offersOwned
                        then do
                            let mention = "<@" <> show personOffering <> ">"
                            sendMessage channel
                                $ "You don't have the goods you've put up for offer, "
                                <> mention
                                <> ". Your trade has been cancelled and your credits have been decremented."
                            punishWallet conn personOffering
                        else do
                            takeItems conn personOffering offeredItems
                            giveItems conn personReacting offeredItems
                            takeItems conn personReacting demandedItems
                            giveItems conn personOffering demandedItems
                    return ()
            let
                newEmbed = makeOfferEmbed False
                                          personOffering
                                          (offeredItems, demandedItems)
            restCall'_ $ EditMessage (channel, message) "" (Just newEmbed)
      where
        message = reactionMessageId react
        channel = reactionChannelId react

        getValue value =
            getParsed
                . parseItems
                . maybe "nothing" embedFieldValue
                . find ((== value) . embedFieldName)
                . embedFields

        parseAuthor :: Text -> Either ParseError UserId
        parseAuthor = parse parAuthor ""

        parAuthor :: Parser UserId
        parAuthor = do
            void $ string "Offered by <@"
            digits <- many1 digit
            void $ string ">"
            return . read $ digits


    _ -> return ()

main :: IO ()
main = do
    token <- readFile "token.txt"
    conn  <- DB.checkedConnect DB.defaultConnectInfo
    void . runDiscord $ def { discordToken   = fromString token
                            , discordOnStart = startHandler conn
                            , discordOnEvent = eventHandler conn
                            }
