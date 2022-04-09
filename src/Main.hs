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
                                                , head
                                                )

import           Commands
import           Constants
import           Events
import           Game
import           Game.Data
import           Game.NPCs
import           Utils
import           Utils.DictM
import           Utils.Discord

import           Discord.Requests
import           Discord.Types

import           Control.Lens
import qualified Data.Set                      as Set
import           Data.String.Interpolate
import qualified Data.Text                     as T
import           Data.Time.Clock                ( addUTCTime
                                                , getCurrentTime
                                                )
import qualified Database.Redis                as DB
import           System.Process.Typed
import           System.Random
import           UnliftIO                hiding ( handle )
import           UnliftIO.Concurrent            ( forkIO
                                                , killThread
                                                , threadDelay
                                                )
import           Utils.Twitter

import           Web.Twitter.Conduit

import           Discord
import           Network.Wreq.Session           ( newAPISession )

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
    npcs <- listNPC
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
  [rngChoice, rngHead, rngDestroy] <- replicateM 3 newStdGen
  let channel = messageChannel m

  if
    | odds 0.01 rngHead
    -> sendReplyTo
      m
      "Never say 'owned' again or I will rip your head from that stupid tiny neck of yours, asshole."
    | odds 0.10 rngDestroy
    -> do
      sendReplyTo m "Absolutely not. You are destroyed."
    | isCeleste
    -> randomChoice
      ( sendMessage channel "shut the fuck up, celeste"
      : replicate 2 (reactToMessage ownedEmoji m)
      )
      rngChoice
    | otherwise
    -> reactToMessage ownedEmoji m

 where
  isCeleste     = ((== 140541286498304000) . userId . messageAuthor) m
  ownagePresent = (T.isInfixOf "owned" . messageText) m

handleReact :: Message -> DictM ()
handleReact msg = do
  rng <- newStdGen
  when (odds 0.05 rng) $ do
    emojiList <-
      randomChoice [emojiPositive, emojiNeutral, emojiNegative, emojiEverything]
        <$> newStdGen
    randomEmoji <- randomChoice emojiList <$> newStdGen
    reactToMessage randomEmoji msg

-- messages
-----------------

-- | Handle a message assuming that either is or isn't a command.
handleMessage :: Message -> DictM ()
handleMessage m = unless (userIsBot . messageAuthor $ m) $ do
  lift . logErrorsInChannel (messageChannel m) $ do
    commandRun <- handleCommand m

    unless commandRun $ do
      handleReact m
      handleOwned m
      handlePontificate m
      handleImpersonate m
      handleNPCSpeak m
-- events
---------

minutes, hours, days :: Double -> Double
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
  [
    -- gmposting and gnposting
    RandomEvent { avgDelay = days 1, randomEvent = sendMessageToGeneral "gm" }
  , RandomEvent { avgDelay = days 1, randomEvent = sendMessageToGeneral "gn" }
    -- declarations and decrees
  , RandomEvent { avgDelay = minutes 100, randomEvent = dictate }
  , RandomEvent { avgDelay = minutes 120, randomEvent = postImage }
    -- NPC speak
  , RandomEvent
    { avgDelay    = hours 3
    , randomEvent = getGeneralChannel >>= randomNPCSpeakGroup . channelId
    }
  ]


scheduledEvents :: [ScheduledEvent]
scheduledEvents =
  [ScheduledEvent { absDelay = minutes 5, scheduledEvent = performDailyEvents }]

dailyEvents :: [DailyEvent]
dailyEvents = [DailyEvent { dailyEvent = return () }]

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
  mapConcurrently_ (forkIO . logErrors' env)
                   [performRandomEvents, startScheduledEvents]

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
    general <- channelId <$> getGeneralChannel
    let newMember = userId . memberUser $ m
        intro     = [i|Welcome, <@#{newMember}>. You will recieve nothing.|]
    sendMessage general intro

  MessageReactionAdd react -> logErrors' env $ do
    when ((emojiName . reactionEmoji) react `elem` birds) $ do
          -- We want to count the reactions, and we only get one here, so we get the rest.
      msgObj <- restCall' $ GetChannelMessage (channel, message)
      users  <- restCall' $ GetReactions (channel, message)
                                         (emojiName . reactionEmoji $ react)
                                         (25, LatestReaction)
      global <- getGlobal
      let metThreshhold = length users >= 3
          correctUser   = (== dictId) . userId . messageAuthor $ msgObj
          notTweeted    = message `Set.notMember` (global ^. globalTweeted)
      when (metThreshhold && correctUser && notTweeted) $ do
        sendReplyTo msgObj "Send tweet."
        tweetId <- sendTweet . twitterFilter $ messageText msgObj
        setGlobal $ global & globalTweeted %~ Set.insert message
        sendUnfilteredReplyTo
          msgObj
          [i|https://twitter.com/nomic_dict/status/#{tweetId}|]

   where
      -- TODO find out which one of these is real
    birds         = ["bird", ":bird:", "🐦"]

    message       = reactionMessageId react
    channel       = reactionChannelId react

    twitterFilter = T.replace "**" "" . T.replace "__" ""

  _ -> return ()

main :: IO ()
main = do
  handle <- forkIO $ runProcess_ (proc "python3" ["python/memories.py"])

  token  <- readFile "token.txt"
  conn   <- DB.checkedConnect DB.defaultConnectInfo
  creds  <- liftIO twitterAuth
  sesh   <- newAPISession

  let env = Env { envDb = conn, envTw = creds, envSs = sesh }
  res <- runDiscord $ def
    { discordToken         = fromString token
    , discordOnStart       = startHandler env
    , discordOnEvent       = eventHandler env
    , discordGatewayIntent = def { gatewayIntentMembers = True }
    }
  killThread handle
  print res
        -- Enable intents so we can see user joins.
