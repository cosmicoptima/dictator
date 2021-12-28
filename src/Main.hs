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

-- local modules
----------------
import           Commands
import           Datatypes
import           DiscordUtils
import           Economy
import           Events
import           GenText
import           Items
import           Utils

-- discord
----------
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

-- parsing
----------
import           Data.Char
import qualified Data.Text                     as T
import           Text.Parsec             hiding ( token
                                                , try
                                                )

-- random
---------
import           Data.Random.Normal
import           System.Random
import           System.Random.Shuffle          ( shuffle' )

-- all else
-----------
import           Control.Lens
import           Control.Monad                  ( liftM2 )
import           Data.List                      ( intersect )
import           Data.Maybe
import qualified Database.Redis                as DB
import           UnliftIO.Async                 ( mapConcurrently_ )
import           UnliftIO.Concurrent            ( forkIO
                                                , threadDelay
                                                )


-- forbidden word handling
--------------------------

awardTeamMembersCredit :: DB.Connection -> Team -> Double -> DH ()
awardTeamMembersCredit conn rewardedTeam n = getMembers >>= mapConcurrently_
    (\m -> do
        let memberID = (userId . memberUser) m
        Just memberData <- getUser conn memberID
        when (Just rewardedTeam == memberData ^. userTeam) . void $ modifyUser
            conn
            memberID
            (over userCredits (+ n))
    )

updateForbiddenWords :: DB.Connection -> DH ()
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

forbidUser :: DB.Connection -> ChannelId -> Text -> UserId -> DH ()
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

handleForbidden :: DB.Connection -> Message -> DH ()
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

handlePontificate :: Message -> DH ()
handlePontificate m =
    when (odds 0.02 . mkStdGen . fromIntegral . messageId $ m)
        $ pontificate channel content
  where
    channel = messageChannel m
    content = messageText m


-- owned!!!
-----------

ownedEmoji :: Text
ownedEmoji = "owned:899536714773717012"

handleOwned :: Message -> DH ()
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


-- commands
-----------------

-- FIXME
-- | Handle a message assuming it's a command. If it isn't, fire off the handler for regular messages.
handleCommand :: DB.Connection -> Message -> DH ()
handleCommand conn m = do
    if not . userIsBot . messageAuthor $ m
        then case words . stripPuncRight $ content of
            ("what" : theFuck) -> do
                output <-
                    getGPT
                        (  makePrompt
                              [ "Q: what is 2 + 2? A: 4"
                              , "Q: what is the meaning of life? A: go fuck yourself"
                              , "Q: what are you doing step bro? A: :flushed:"
                              , "Q: what is the eighth circle of hell called? A: malebolge"
                              ]
                        <> " Q: what "
                        <> unwords theFuck
                        <> "? A:"
                        )
                    <&> fromMaybe ""
                    .   listToMaybe
                    .   lines
                    .   T.drop 1
                sendMessage channel output

            ("who" : didThis) -> do
                randomN :: Double <- newStdGen <&> fst . random
                randomMember      <- if randomN < 0.75
                    then
                        (do
                            general <- getGeneralChannel
                            restCall'
                                    (GetChannelMessages
                                        (channelId general)
                                        (100, LatestMessages)
                                    )
                                >>= ( (<&> messageAuthor)
                                    . (newStdGen <&>)
                                    . randomChoice
                                    )
                                >>= userToMember
                                <&> fromJust
                        )
                    else getMembers >>= ((newStdGen <&>) . randomChoice)
                sendMessage channel
                    $  "<@"
                    <> (show . userId . memberUser) randomMember
                    <> "> "
                    <> T.unwords didThis

            ("flaunt" : goods) -> do
                case parseTrinkets . unwords $ goods of
                    Left err ->
                        sendMessage channel
                            $  "What the fuck is this? ```"
                            <> show err
                            <> "```"
                    Right flauntedTrinkets -> do
                        trinketIds <- getUser conn authorId
                            <&> maybe [] (view userTrinkets)
                        if flauntedTrinkets
                            == intersect flauntedTrinkets trinketIds
                        then
                            do
                                trinkets <-
                                    mapM (getTrinket conn) flauntedTrinkets
                                        <&> catMaybes
                                let display =
                                        T.intercalate "\n"
                                            .   fmap (\w -> "**" <> w <> "**")
                                            $   uncurry displayTrinket
                                            <$> zip flauntedTrinkets trinkets
                                void
                                    . restCall'
                                    . CreateMessageEmbed
                                          channel
                                          (voiceFilter
                                              "You wish to display your wealth?"
                                          )
                                    $ mkEmbed "Goods (PITIFUL)"
                                              display
                                              []
                                              Nothing
                        else
                            do
                                sendMessage
                                    channel
                                    "You don't own the goods you so shamelessly try to flaunt, and now you own even less. Credits, that is."
                                void $ modifyUser conn
                                                  authorId
                                                  (over userCredits pred)
            _ -> handleMessage conn m
        else pure ()
  where
    stripPuncRight = T.reverse . T.dropWhile isPunctuation . T.reverse

    content        = T.toLower . messageText $ m
    channel        = messageChannel m
    author         = messageAuthor m
    authorId       = userId author


-- other messages
-----------------

-- | Handle a message assuming that it isn't a command.
handleMessage :: DB.Connection -> Message -> DH ()
handleMessage conn m = do
    handleCommand' conn m -- will replace handleCommand soon
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
    , randomEvent :: DB.Connection -> DH ()
    }

data ScheduledEvent = ScheduledEvent
    { absDelay       :: Double
    , scheduledEvent :: DB.Connection -> DH ()
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
    ]

scheduledEvents :: [ScheduledEvent]
scheduledEvents =
    [ ScheduledEvent { absDelay       = hours 2
                     , scheduledEvent = updateForbiddenWords
                     }
    ]

performRandomEvents :: DB.Connection -> DH ()
performRandomEvents conn = do
    threadDelay 100000
    void . forkIO $ mapConcurrently_ maybePerformRandomEvent randomEvents
    performRandomEvents conn

  where
    maybePerformRandomEvent (RandomEvent rngDelay event) = do
        rng <- newStdGen
        when (odds (0.1 / rngDelay) rng) $ event conn

startScheduledEvents :: DB.Connection -> DH ()
startScheduledEvents conn = do
    mapConcurrently_ scheduledEventLoop scheduledEvents
  where
    scheduledEventLoop sched@(ScheduledEvent delay event) = do
        -- Sleep for the required amount of time, noting that this is in nanoseconds.
        threadDelay . secsToUs $ delay
        event conn
        scheduledEventLoop sched
    secsToUs = round . (* 1e6)


-- main
-------

startHandler :: DB.Connection -> DH ()
startHandler conn = do
    sendMessageToGeneral "Rise and shine!"
    mapConcurrently_
        forkIO
        [ unbanUsersFromGeneral
        , performRandomEvents conn
        , startScheduledEvents conn
        , updateTeamRoles conn
        , forgiveDebt
        , threadDelay 5000000 >> updateForbiddenWords conn
        ]
  where
    forgiveDebt = getMembers >>= mapConcurrently_
        (\m -> modifyUser conn (userId . memberUser $ m)
            $ over userCredits (max 0)
        )

    unbanUsersFromGeneral = do
        general <- getGeneralChannel
        getMembers >>= mapConcurrently_
            (\m -> do
                setUserPermsInChannel True
                                      (channelId general)
                                      (userId . memberUser $ m)
                                      0x800
            )

eventHandler :: DB.Connection -> Event -> DH ()
eventHandler conn = \case
    MessageCreate m    -> handleCommand conn m
    GuildMemberAdd _ _ -> updateTeamRoles conn
    _                  -> return ()

main :: IO ()
main = do
    token <- readFile "token.txt"
    conn  <- DB.checkedConnect DB.defaultConnectInfo
    void . runDiscord $ def { discordToken   = fromString token
                            , discordOnStart = startHandler conn
                            , discordOnEvent = eventHandler conn
                            }
