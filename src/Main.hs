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

import           Control.Lens
import           Control.Monad.Except
import qualified Data.MultiSet                 as MS
import qualified Data.Text                     as T
import qualified Database.Redis                as DB
import           Game
import           Game.Events
import           System.Random
import           UnliftIO.Async                 ( mapConcurrently_ )
import           UnliftIO.Concurrent            ( forkIO
                                                , threadDelay
                                                )


-- forbidden word handling
--------------------------

awardTeamMembersCredit :: DB.Connection -> Team -> Double -> DictM ()
awardTeamMembersCredit conn rewardedTeam n = getMembers >>= mapM_
    (\m -> do
        let memberID = (userId . memberUser) m
        memberData <- getUser conn memberID <&> fromMaybe def
        when (Just rewardedTeam == memberData ^. userTeam) . void $ modifyUser
            conn
            memberID
            (over userCredits (+ n))
    )

updateForbiddenWords :: DB.Connection -> DictM ()
updateForbiddenWords conn = do
    fullWordList <- liftIO getWordList
    mapM_
        (\team -> do
            wordList <- replicateM 10 (newStdGen <&> randomChoice fullWordList)
            modifyTeam conn team (set teamForbidden wordList)
        )
        [First, Second]

    general <- getGeneralChannel <&> channelId
    mapM_ (upsertPin general) [First, Second]

  where
    upsertPin channel team = do
        teamData <- getTeam conn team <&> fromMaybe def
        pinId    <- case teamData ^. teamWarning of
            Just pin -> return pin
            Nothing  -> do
                pinId <- restCall' (CreateMessage channel "aa") <&> messageId
                void $ modifyTeam conn team $ set teamWarning (Just pinId)
                restCall' $ AddPinnedMessage (channel, pinId)
                return pinId

        embed <- warningEmbed (teamData ^. teamForbidden) team
        void . restCall' $ EditMessage (channel, pinId)
                                       (warning team)
                                       (Just embed)

    warning t = voiceFilter $ case t of
        First
            -> "The following words and terms are hereby illegal, forbidden, banned and struck from all records, forever: "
        Second
            -> "I declare that the following so-called words do not exist, have never existed, and will continue to not exist: "

    warningEmbed wordList team = do
        role <- getTeamRole conn team
        return $ mkEmbed ("Forbidden words for " <> roleName role <> ":")
                         (T.intercalate ", " wordList)
                         []
                         (Just . roleColor $ role)

forbidUser :: DB.Connection -> ChannelId -> Text -> UserId -> DictM ()
forbidUser conn channel badWord user = do
    Just team <- getUser conn user <&> (view userTeam =<<)

    bannedWordMessage team >>= sendMessage channel
    void $ modifyTeam conn (otherTeam team) (over teamPoints (+ 10))

    setUserPermsInChannel False channel user 0x800
    -- 15 seconds as microseconds
    threadDelay 15000000
    setUserPermsInChannel True channel user 0x800
  where
    getTeamName team = getTeamRole conn team <&> roleName
    bannedWordMessage badTeam = do
        badTeamName  <- getTeamName badTeam
        goodTeamName <- (getTeamName . otherTeam) badTeam
        return
            $  "You arrogant little insect! Team "
            <> badTeamName
            <> " clearly wish to disrespect my authority by uttering a word so vile as '"
            <> badWord
            <> "', so team "
            <> goodTeamName
            <> " will be awarded 10 points."

handleForbidden :: DB.Connection -> Message -> DictM ()
handleForbidden conn m = do
    Just (Just culpritTeam) <- getUser conn (userId author)
        <&> fmap (view userTeam)
    messageForbidden <- messageForbiddenWith content culpritTeam
    case messageForbidden of
        Just word -> do
            forbidUser conn (messageChannel m) word (userId author)
            updateForbiddenWords conn
            awardTeamMembersCredit conn (otherTeam culpritTeam) 10
        Nothing -> return ()
  where
    author  = messageAuthor m
    content = messageText m

    messageForbiddenWith message team = do
        Just forbidden <- getTeam conn team <&> fmap (view teamForbidden)
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
    (rngCeleste, rngEmoji) <- newStdGen <&> split
    let emoji = randomChoice [ownedEmoji, ownedEmoji, "skull"] rngEmoji

    if isCeleste
        then do
            randomChoice
                ( sendMessageToGeneral "shut the fuck up, celeste"
                : replicate 2 (reactToMessage emoji m)
                )
                rngCeleste
        else reactToMessage emoji m
  where
    isCeleste     = ((== 140541286498304000) . userId . messageAuthor) m
    ownagePresent = (T.isInfixOf "owned" . messageText) m


-- messages
-----------------

-- | Handle a message assuming that either is or isn't a command.
handleMessage :: DB.Connection -> Message -> DH ()
handleMessage conn m = unless (userIsBot . messageAuthor $ m) $ do
    commandRun <- runExceptT (handleCommand conn m) >>= \case
        Right run             -> return run
        Left  (Fuckup    err) -> debugPrint err >> return True
        Left  (Complaint err) -> do
            ignoreErrors . sendMessage (messageChannel m) $ err
            return True
        Left (Gibberish err) -> do
            ignoreErrors
                .  sendMessage (messageChannel m)
                $  "What the fuck is this?```"
                <> show err
                <> "```"
            return True
        Left GTFO -> return True

    logErrors $ unless commandRun $ do
        handleOwned m
        handlePontificate m
        handleForbidden conn m


-- events
---------

minutes, hours, days :: Double -> Double
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
    -- add items to locations
    -- FIXME
    , RandomEvent
        { avgDelay    = 10
        , randomEvent = \c ->
            do
                getallLocation c
            >>= mapM_
                    (\(place, _) -> do
                        p :: Double <- randomIO
                        if
                            | p < 0.9999 -> return ()
                            | p < 0.9995 -> do
                                (rng, rng') <- newStdGen <&> split
                                location    <-
                                    getLocation c place
                                        >>= maybe
                                                ( throwError
                                                $ Fuckup
                                                      "This location doesn't exist?"
                                                )
                                                return
                                let inLocation = location ^. locationTrinkets
                                    t1 = randomChoice (MS.elems inLocation) rng
                                    t2 =
                                        randomChoice (MS.elems inLocation) rng'
                                void $ trinketsBreed c place t1 t2
                            | p < 0.9997 -> do
                                rarity <- randomNewTrinketRarity
                                mkNewTrinket c rarity
                                    >>= trinketSpawns c place
                                    .   fst
                            | otherwise -> do
                                rarity <- randomExistingTrinketRarity
                                getRandomTrinket c rarity
                                    >>= trinketSpawns c place
                                    .   fst
                    )
        }
    ]

scheduledEvents :: [ScheduledEvent]
scheduledEvents =
    [ ScheduledEvent { absDelay       = hours 2
                     , scheduledEvent = updateForbiddenWords
                     }
    , ScheduledEvent
        { absDelay       = minutes 10
        , scheduledEvent = \c ->
            getMembers
                >>= mapM_
                        (\m -> modifyUser c (userId . memberUser $ m)
                            $ over userCredits succ
                        )
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
    ignoreErrors . sendMessageToGeneral $ "Rise and shine!"
    mapConcurrently_
        (forkIO . dieOnErrors)
        [ unbanUsersFromGeneral
        , performRandomEvents conn
        , startScheduledEvents conn
        , updateTeamRoles conn
        , forgiveDebt
        , threadDelay 5000000 >> updateForbiddenWords conn
        , createLogIfDoesn'tExist
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

    createLogIfDoesn'tExist = getChannelNamed "log" >>= maybe
        (do
            everyoneID <- getEveryoneRole <&> roleId
            void . restCall' $ CreateGuildChannel
                pnppcId
                "log"
                [Overwrite everyoneID "role" 0 2048]
                (CreateGuildChannelOptsText Nothing Nothing Nothing Nothing)
        )
        (const $ return ())

    createRarityEmojisIfDon'tExist = mapM_
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
                Right i -> void . restCall' $ CreateGuildEmoji pnppcId name i

eventHandler :: DB.Connection -> Event -> DH ()
eventHandler conn = \case
    MessageCreate m    -> handleMessage conn m
    GuildMemberAdd _ _ -> logErrors $ updateTeamRoles conn
    _                  -> return ()

main :: IO ()
main = do
    token <- readFile "token.txt"
    conn  <- DB.checkedConnect DB.defaultConnectInfo
    void . runDiscord $ def { discordToken   = fromString token
                            , discordOnStart = startHandler conn
                            , discordOnEvent = eventHandler conn
                            }
