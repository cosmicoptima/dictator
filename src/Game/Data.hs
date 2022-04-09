-- | Abstracts database access with helper functions and types.

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Game.Data
  (
    -- global
    GlobalData
  , globalActiveTokens
  , globalExhaustedTokens
  , globalWebhook
  , globalTweeted
  , getGlobal
  , setGlobal
  , globalDay
  , modifyGlobal

    -- users
  , UserData(..)
  , userDummy
  , getUser
  , setUser

    -- npcs
  , NPCData(..)
  , npcAdjectives
  , npcInterests
  , npcMemories
  , getNPC
  , getNPC'
  , setNPC
  , npcAvatar
  , modifyNPC
  , deleteNPC
  , listNPC
  -- , getallNPC

    -- red button
  , pushRedButton
  , modifyGlobal_
  , modifyUser) where

import           Relude                  hiding ( First
                                                , get
                                                , many
                                                , words
                                                )

import           Utils.DictM
import           Utils.Discord

import           Control.Lens            hiding ( noneOf
                                                , set
                                                )
import qualified Data.ByteString               as BS
import           Data.Default
import           Data.List               hiding ( words )
import           Data.Time                      ( Day(ModifiedJulianDay) )
import           Database.Redis
import           Discord.Internal.Types.Prelude
import           Text.Parsec             hiding ( Reply )

-- TYPES (definitions and instances)
------------------------------------

data UserData = UserData
  { _userDummy :: ()
  }
  deriving (Eq, Generic, Read, Show)

makeLenses ''UserData

data GlobalData = GlobalData
  { _globalExhaustedTokens :: Set Text
  , _globalActiveTokens    :: Set Text
  , _globalWebhook         :: Maybe WebhookId
  , _globalDay             :: Day
  , _globalTweeted         :: Set MessageId
  }
  deriving (Generic, Read, Show) -- show is for debug, can be removed eventually

makeLenses ''GlobalData

instance Default Day where
  def = ModifiedJulianDay 0

instance Default GlobalData

data NPCData = NPCData
  { _npcAdjectives :: [Text]
  , _npcInterests  :: [Text]
  , _npcMemories   :: Set Text
  , _npcAvatar     :: Maybe ByteString
  }
  deriving (Read, Show, Generic)

instance Default NPCData

makeLenses ''NPCData

-- DATABASE
-----------

runRedis' :: Connection -> Redis (Either Reply a) -> IO a
runRedis' c f = runRedis c f >>= either (die . show) return

getWithType :: Connection -> ByteString -> Text -> Text -> IO (Maybe Text)
getWithType conn type_ key field =
  runRedis'
      conn
      (get . BS.intercalate ":" $ [type_, encodeUtf8 key, encodeUtf8 field])
    <&> fmap decodeUtf8

setWithType :: Connection -> ByteString -> Text -> Text -> Text -> IO ()
setWithType conn type_ key field value = void . runRedis' conn $ set
  (BS.intercalate ":" [type_, encodeUtf8 key, encodeUtf8 field])
  (encodeUtf8 value)

deleteWithType :: Connection -> ByteString -> Text -> Text -> IO ()
deleteWithType conn type_ key field = void . runRedis' conn $ del
  [BS.intercalate ":" [type_, encodeUtf8 key, encodeUtf8 field]]

readWithType
  :: Read b => Text -> (a -> Text) -> Connection -> a -> Text -> MaybeT IO b
readWithType type_ f conn id_ key =
  liftIO (getWithType conn (encodeUtf8 type_) (f id_) key)
    >>= hoistMaybe
    >>= hoistMaybe
    .   readMaybe
    .   toString

-- this is gross, i know. sorry!
showWithType
  :: (Show b)
  => Text
  -> (a -> Text)
  -> Connection
  -> a
  -> Text
  -> Getting b c b
  -> c
  -> IO ()
showWithType type_ f conn id_ key getter data_ =
  setWithType conn (encodeUtf8 type_) (f id_) key (show $ data_ ^. getter)

countWithType :: MonadIO m => Text -> Connection -> m Int
countWithType type_ conn =
  liftIO
    $   runRedis' conn (keys $ encodeUtf8 type_ <> ":*")
    <&> length
    .   nub
    .   rights
    .   map (parse parser "")
 where
  parser = do
    void . string $ toString type_ <> ":"
    many (noneOf ":")

getallWithType
  :: Eq a => Text -> (a -> DictM (Maybe b)) -> (Text -> a) -> DictM [(a, b)]
getallWithType type_ f g = do
  conn        <- asks envDb
  distinctIDs <-
    liftIO
    $   runRedis' conn (keys $ encodeUtf8 type_ <> ":*")
    <&> nub
    .   rights
    .   map (fmap (g . fromString) . parse parser "")
  mapConcurrently' (\x -> f x <&> (x, )) distinctIDs <&> mapMaybe raiseMaybe
 where
  raiseMaybe = \case
    (a, Just b ) -> Just (a, b)
    (_, Nothing) -> Nothing
  parser = do
    void . string $ toString type_ <> ":"
    many (noneOf ":")

listWithType :: Eq a => Text -> (Text -> a) -> DictM [a]
listWithType type_ f = do
  conn <- asks envDb
  liftIO
    $   runRedis' conn (keys $ encodeUtf8 type_ <> ":*")
    <&> nub
    .   rights
    .   map (fmap (f . fromString) . parse parser "")
 where
  parser = do
    void . string $ toString type_ <> ":"
    many (noneOf ":")


readGlobalType :: (Default a, Read a) => Connection -> Text -> IO a
readGlobalType conn field =
  fromMaybe def <$> runMaybeT (readWithType "global" (const "") conn () field)

showGlobalType :: Show a => Connection -> Text -> Getting a b a -> b -> IO ()
showGlobalType = flip (showWithType "global" (const "")) ()

getGlobal :: DictM GlobalData
getGlobal = do
  conn            <- asks envDb
  exhausted       <- liftIO $ readGlobalType conn "exhausted"
  active          <- liftIO $ readGlobalType conn "active"
  webhook         <- liftIO $ readGlobalType conn "webhook"
  tweeted         <- liftIO $ readGlobalType conn "tweeted"
  day             <- liftIO $ readGlobalType conn "day"
  return $ GlobalData { _globalExhaustedTokens = exhausted
                      , _globalWebhook         = webhook
                      , _globalTweeted         = tweeted
                      , _globalActiveTokens    = active
                      , _globalDay             = day
                      }


setGlobal :: GlobalData -> DictM ()
setGlobal globalData = do
  conn <- asks envDb
  liftIO $ showGlobalType conn "exhausted" globalExhaustedTokens globalData
  liftIO $ showGlobalType conn "active" globalActiveTokens globalData
  liftIO $ showGlobalType conn "webhook" globalWebhook globalData
  liftIO $ showGlobalType conn "tweeted" globalTweeted globalData
  liftIO $ showGlobalType conn "day" globalDay globalData

modifyGlobal :: (GlobalData -> GlobalData) -> DictM GlobalData
modifyGlobal f = do
  globalData <- getGlobal <&> f
  setGlobal globalData
  return globalData

modifyGlobal_ :: (GlobalData -> GlobalData) -> DictM ()
modifyGlobal_ = void . modifyGlobal

readUserType :: (Default a, Read a) => Connection -> UserId -> Text -> IO a
readUserType conn userID key =
  fromMaybe def <$> runMaybeT (readWithType "users" show conn userID key)

showUserType
  :: Show a => Connection -> UserId -> Text -> Getting a b a -> b -> IO ()
showUserType = showWithType "users" show

getUser :: UserId -> DictM UserData
getUser userId = do
  conn <- asks envDb
  liftIO $ do
    dummy      <- readUserType conn userId "hgkjashdgkjashg"
    return UserData
      { _userDummy = dummy
      }

setUser :: UserId -> UserData -> DictM ()
setUser userId userData = do
  conn <- asks envDb
  liftIO $ showUserType conn userId "hgkjashdgkjashg" userDummy userData

modifyUser :: UserId -> (UserData -> UserData) -> DictM UserData
modifyUser userId f = do
  userData <- getUser userId <&> f
  setUser userId userData
  return userData

readNPCType :: (Default a, Read a) => Connection -> Text -> Text -> IO a
readNPCType conn userID key =
  fromMaybe def <$> runMaybeT (readWithType "npcs" id conn userID key)

readNPCType'
  :: Read a => Connection -> Text -> Text -> IO (Maybe a)
readNPCType' conn userID key =
  runMaybeT (readWithType "npcs" id conn userID key)

showNPCType
  :: Show a => Connection -> Text -> Text -> Getting a b a -> b -> IO ()
showNPCType = showWithType "npcs" id

getNPC :: Text -> DictM NPCData
getNPC name = do
  conn <- asks envDb
  liftIO $ do
    adjectives <- readNPCType conn name "adjectives"
    interests  <- readNPCType conn name "interests"
    memories   <- readNPCType conn name "memories"
    avatar     <- readNPCType conn name "avatar"
    return NPCData { _npcAdjectives = adjectives
                   , _npcInterests  = interests
                   , _npcMemories   = memories
                   , _npcAvatar     = avatar
                   }

getNPC' :: Text -> DictM (Maybe NPCData)
getNPC' name = do
  conn <- asks envDb
  liftIO . runMaybeT $ do
    adjectives <- MaybeT $ readNPCType' conn name "adjectives"
    interests  <- MaybeT $ readNPCType' conn name "interests"
    memories   <- MaybeT $ readNPCType' conn name "memories"
    avatar     <- MaybeT $ readNPCType' conn name "avatar"
    return NPCData { _npcAdjectives = adjectives
                   , _npcInterests  = interests
                   , _npcMemories   = memories
                   , _npcAvatar     = avatar
                   }

setNPC :: Text -> NPCData -> DictM ()
setNPC name npcData = do
  conn <- asks envDb
  liftIO $ showNPCType conn name "adjectives" npcAdjectives npcData
  liftIO $ showNPCType conn name "interests" npcInterests npcData
  liftIO $ showNPCType conn name "memories" npcMemories npcData
  liftIO $ showNPCType conn name "avatar" npcAvatar npcData

modifyNPC :: Text -> (NPCData -> NPCData) -> DictM NPCData
modifyNPC name f = do
  npcData <- getNPC name <&> f
  setNPC name npcData
  return npcData

deleteNPC :: Text -> DictM ()
deleteNPC name = do
  conn <- asks envDb
  liftIO $ deleteWithType conn "npcs" name "adjectives"
  liftIO $ deleteWithType conn "npcs" name "interests"
  liftIO $ deleteWithType conn "npcs" name "memories"
  liftIO $ deleteWithType conn "npcs" name "avatar"

listNPC :: DictM [Text]
listNPC = listWithType "npcs" id

pushRedButton :: DictM ()
pushRedButton = do
  conn <- asks envDb
  void . liftIO $ runRedis' conn flushdb
