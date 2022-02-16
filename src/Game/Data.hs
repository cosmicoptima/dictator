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

module Game.Data
  (
    -- global
    GlobalData
  , Fighter(..)
  , fighterOwner
  , fighterTrinket
  , globalActiveTokens
  , globalExhaustedTokens
  , globalEncouraged
  , globalEffects
  , globalWebhook
  , globalSubmitted
  , globalTweeted
  , globalArena
  , getGlobal
  , setGlobal
  , globalDay
  , globalRoles
  , modifyGlobal

    -- users
  , UserData(..)
  , Username(..)
  , Effect
  , userName
  , userPoints
  , userEffects
  , userItems
  , getUser
  , setUser
  , modifyUserRaw
  , maxInventorySizeOf

    -- trinkets
  , TrinketData(..)
  , Rarity(..)
  , trinketName
  , trinketRarity
  , displayTrinket
  , getTrinket
  , getTrinketOr
  , setTrinket
  , getallTrinket
  , countTrinket

    -- locations
  , LocationData(..)
  , locationTrinkets
  , getLocation
  , setLocation
  , modifyLocation
  , getallLocation
  , countLocation
  , getLocationOr

    -- trades
  , TradeData(..)
  , TradeStatus(..)
  , tradeStatus
  , tradeOffers
  , tradeDemands
  , tradeAuthor
  , setTrade
  , getTrade

    -- npcs
  , NPCData(..)
  , npcMemories
  , getNPC
  , getNPC'
  , setNPC
  , npcAvatar
  , modifyNPC
  , deleteNPC
  , getallNPC

    -- red button
  , pushRedButton
  , modifyGlobal_
  ) where

import           Prelude                        ( log )
import           Relude                  hiding ( First
                                                , get
                                                , many
                                                , words
                                                )

import           Utils.DictM
import           Utils.Discord

import           Data.MultiSet                  ( MultiSet )
import qualified Data.MultiSet                 as MS

import           Control.Lens            hiding ( noneOf
                                                , set
                                                )
import           Control.Monad.Except
import qualified Data.ByteString               as BS
import           Data.Default
import           Data.List               hiding ( words )
import qualified Data.Map                      as Map
import           Data.String.Interpolate        ( i )
import           Data.Time                      ( Day(ModifiedJulianDay) )
import           Database.Redis
import           Discord.Internal.Types.Prelude
import           Game.Items
import           Relude.Unsafe
import           Text.Parsec             hiding ( Reply )

-- TYPES (definitions and instances)
------------------------------------

data Rarity = Common | Uncommon | Rare | Legendary | Mythic | Forbidden | Unspeakable deriving (Eq, Ord, Generic, Read, Show, Enum)

data TrinketData = TrinketData
  { _trinketName   :: Text
  , _trinketRarity :: Rarity
  }
  deriving (Eq, Ord, Generic, Read, Show)

makeLenses ''TrinketData

displayRarity :: Rarity -> DictM Text
displayRarity rarity = getEmojiNamed name >>= maybe
  (throwError $ Fuckup "rarity emoji doesn't exist")
  (return . displayCustomEmoji)
 where
  name = case rarity of
    Common      -> "common"
    Uncommon    -> "uncommon"
    Rare        -> "rare"
    Legendary   -> "legendary"
    Mythic      -> "mythic"
    Forbidden   -> "forbidden"
    Unspeakable -> "unspeakable"

-- unfortunately this is IO since it has to look up the rarity emojis
displayTrinket :: TrinketID -> TrinketData -> DictM Text
displayTrinket id_ trinket = do
  rarityEmoji <- displayRarity (trinket ^. trinketRarity)
  return
    $  "**#"
    <> show id_
    <> " "
    <> (trinket ^. trinketName)
    <> "** "
    <> rarityEmoji

newtype Username = Username { unUsername :: Text } deriving (Eq, Read, Show)

type Effect = Text
type Achievement = Text

data UserData = UserData
  { _userItems        :: Items
  , _userAchievements :: Set Achievement
  , _userName         :: Username
  , _userPoints       :: Integer
  , _userEffects      :: Set Effect
  }
  deriving (Eq, Generic, Read, Show)

makeLenses ''UserData

instance Default Username where
  def = Username "peon"

instance Default UserData

newtype LocationData = LocationData
    { _locationTrinkets :: MultiSet TrinketID
    } deriving Generic

makeLenses ''LocationData

instance Default LocationData

data Fighter = Fighter
  { _fighterOwner   :: UserId
  , _fighterTrinket :: TrinketID
  }
  deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''Fighter

data GlobalData = GlobalData
  { _globalExhaustedTokens :: Set Text
  , _globalActiveTokens    :: Set Text
  , _globalEncouraged      :: [Text]
  , _globalWebhook         :: Maybe WebhookId
  , _globalEffects         :: Map UserId (Set Effect)
  , _globalArena           :: MultiSet Fighter
  , _globalDay             :: Day
  , _globalSubmitted       :: Set UserId
  , _globalTweeted         :: Set MessageId
  , _globalRoles           :: Set RoleId
  }
  deriving (Generic, Read, Show) -- show is for debug, can be removed eventually

makeLenses ''GlobalData

instance Default Day where
  def = ModifiedJulianDay 0

instance Default GlobalData


data TradeStatus = OpenTrade | ClosedTrade | PendingTrade deriving (Eq, Show, Read, Generic)

data TradeData = TradeData
  { _tradeStatus  :: TradeStatus
  , _tradeOffers  :: Items
  , _tradeDemands :: Items
  , _tradeAuthor  :: UserId
  }
  deriving (Read, Show, Generic)

makeLenses ''TradeData


data NPCData = NPCData
  { _npcMemories :: Set Text
  , _npcAvatar   :: Maybe ByteString
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
  arena           <- liftIO $ readGlobalType conn "arena"
  encouragedWords <- liftIO $ readGlobalType conn "encouraged"
  webhook         <- liftIO $ readGlobalType conn "webhook"
  effects         <- liftIO $ readGlobalType conn "effects"
  tweeted         <- liftIO $ readGlobalType conn "tweeted"
  day             <- liftIO $ readGlobalType conn "day"
  submitted       <- liftIO $ readGlobalType conn "submitted"
  roles           <- liftIO $ readGlobalType conn "roles"
  return $ GlobalData { _globalExhaustedTokens = exhausted
                      , _globalActiveTokens    = active
                      , _globalEncouraged      = encouragedWords
                      , _globalSubmitted       = submitted
                      , _globalWebhook         = webhook
                      , _globalEffects         = effects
                      , _globalTweeted         = tweeted
                      , _globalArena           = arena
                      , _globalDay             = day
                      , _globalRoles           = roles
                      }


setGlobal :: GlobalData -> DictM ()
setGlobal globalData = do
  conn <- asks envDb
  liftIO $ showGlobalType conn "exhausted" globalExhaustedTokens globalData
  liftIO $ showGlobalType conn "active" globalActiveTokens globalData
  liftIO $ showGlobalType conn "encouraged" globalEncouraged globalData
  liftIO $ showGlobalType conn "webhook" globalWebhook globalData
  liftIO $ showGlobalType conn "effects" globalEffects globalData
  liftIO $ showGlobalType conn "submitted" globalSubmitted globalData
  liftIO $ showGlobalType conn "tweeted" globalTweeted globalData
  liftIO $ showGlobalType conn "arena" globalArena globalData
  liftIO $ showGlobalType conn "roles" globalRoles globalData
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
    credits      <- readUserType conn userId "credits"
    achievements <- readUserType conn userId "achievements"
    name         <- readUserType conn userId "name"
    trinkets     <- readUserType conn userId "trinkets"
    points       <- readUserType conn userId "points"
    words        <- readUserType conn userId "words"
    users        <- readUserType conn userId "users"
    roles        <- readUserType conn userId "colors"
    allEffects   <- readGlobalType conn "effects"
    let effects = fromMaybe def $ allEffects Map.!? userId

    return UserData
      { _userAchievements = achievements
      , _userName         = name
      , _userPoints       = points
      , _userEffects      = effects
      , _userItems        = Items { _itemCredits  = credits
                                  , _itemTrinkets = trinkets
                                  , _itemWords    = words
                                  , _itemUsers    = users
                                  , _itemRoles    = roles
                                  }
      }

maxInventorySizeOf :: Integer -> Integer
maxInventorySizeOf =
  min 100 . max 8 . (+ 8) . round @Double . log . fromInteger . abs

setUser :: UserId -> UserData -> DictM ()
setUser userId userData = do
  conn <- asks envDb
  let inventorySize = fromInteger . maxInventorySizeOf $ userData ^. userPoints
  currentUserData <- getUser userId
  if MS.size (userData ^. userItems . itemTrinkets)
       >  inventorySize
       && MS.size (userData ^. userItems . itemTrinkets)
       >  MS.size (currentUserData ^. userItems . itemTrinkets)
    then do
      void $ modifyUserRaw
        userId
        ( over (userItems . itemTrinkets)
        $ MS.fromList
        . take inventorySize
        . MS.elems
        )
      throwError $ Complaint
        [i|You don't *need* more than #{inventorySize} trinkets...|]
    else liftIO $ showUserType conn
                               userId
                               "trinkets"
                               (userItems . itemTrinkets)
                               userData

  allEffects <- liftIO $ readGlobalType conn "effects"
  let updatedEffects = Map.insert userId (userData ^. userEffects) allEffects

  liftIO $ showUserType conn userId "credits" (userItems . itemCredits) userData
  liftIO $ showUserType conn userId "points" userPoints userData
  liftIO $ showUserType conn userId "words" (userItems . itemWords) userData
  liftIO $ showUserType conn userId "users" (userItems . itemUsers) userData
  liftIO $ showUserType conn userId "colors" (userItems . itemRoles) userData
  liftIO $ showUserType conn userId "achievements" userAchievements userData
  liftIO $ showUserType conn userId "name" userName userData
  liftIO $ showGlobalType conn "effects" id updatedEffects

-- you should probably use modifyUser in Game.Effects instead
modifyUserRaw :: UserId -> (UserData -> UserData) -> DictM UserData
modifyUserRaw userId f = do
  userData <- getUser userId <&> f
  setUser userId userData
  return userData

readTrinketType :: Read a => Connection -> TrinketID -> Text -> MaybeT IO a
readTrinketType = readWithType "trinkets" show

showTrinketType
  :: Show a => Connection -> TrinketID -> Text -> Getting a b a -> b -> IO ()
showTrinketType = showWithType "trinkets" show

getTrinket :: TrinketID -> DictM (Maybe TrinketData)
getTrinket id_ = do
  conn <- asks envDb
  liftIO . runMaybeT $ do
    name   <- readTrinketType conn id_ "name"
    rarity <- readTrinketType conn id_ "rarity"

    return TrinketData { _trinketName = name, _trinketRarity = rarity }

getTrinketOr :: (Text -> Err) -> TrinketID -> DictM TrinketData
getTrinketOr f t = getTrinket t >>= \case
  Just trinket -> return trinket
  Nothing ->
    throwError (f $ "Trinket with ID " <> show t <> " isn't even real!")

setTrinket :: TrinketID -> TrinketData -> DictM ()
setTrinket trinketId trinketData = do
  conn <- asks envDb
  liftIO $ do
    showTrinketType conn trinketId "name"   trinketName   trinketData
    showTrinketType conn trinketId "rarity" trinketRarity trinketData

getallTrinket :: DictM [(TrinketID, TrinketData)]
getallTrinket = do
  getallWithType "trinkets" getTrinket (read . toString)

countTrinket :: DictM Int
countTrinket = asks envDb >>= countWithType "trinkets"


readLocationType :: Read a => Connection -> Text -> Text -> MaybeT IO a
readLocationType = readWithType "location" id

showLocationType
  :: Show a => Connection -> Text -> Text -> Getting a b a -> b -> IO ()
showLocationType = showWithType "location" id

getLocation :: Text -> DictM (Maybe LocationData)
getLocation name = do
  conn <- asks envDb
  liftIO . runMaybeT $ readLocationType conn name "trinkets" <&> LocationData

getLocationOr :: (Text -> Err) -> Text -> DictM LocationData
getLocationOr f name = getLocation name >>= \case
  Just location -> return location
  Nothing       -> throwError
    (f $ "Location " <> name <> " can't be retrieved because it doesn't exist")

setLocation :: Text -> LocationData -> DictM ()
setLocation name locationData = do
  conn <- asks envDb
  liftIO $ showLocationType conn name "trinkets" locationTrinkets locationData

modifyLocation :: Text -> (LocationData -> LocationData) -> DictM LocationData
modifyLocation name f = do
  locationData <- getLocation name <&> f . fromMaybe def
  setLocation name locationData
  return locationData

getallLocation :: DictM [(Text, LocationData)]
getallLocation = getallWithType "location" getLocation id

countLocation :: DictM Int
countLocation = asks envDb >>= countWithType "location"


readTradeType :: Read a => Connection -> MessageId -> Text -> MaybeT IO a
readTradeType = readWithType "trades" show

showTradeType
  :: Show a => Connection -> MessageId -> Text -> Getting a b a -> b -> IO ()
showTradeType = showWithType "trades" show

getTrade :: MessageId -> DictM (Maybe TradeData)
getTrade tradeId = do
  conn <- asks envDb
  liftIO . runMaybeT $ do
    status  <- readTradeType conn tradeId "status"
    offers  <- readTradeType conn tradeId "offers"
    demands <- readTradeType conn tradeId "demands"
    author  <- readTradeType conn tradeId "author"

    return TradeData { _tradeStatus  = status
                     , _tradeOffers  = offers
                     , _tradeDemands = demands
                     , _tradeAuthor  = author
                     }

setTrade :: MessageId -> TradeData -> DictM ()
setTrade tradeId tradeData = do
  conn <- asks envDb
  liftIO $ do
    showTradeType conn tradeId "status"  tradeStatus  tradeData
    showTradeType conn tradeId "offers"  tradeOffers  tradeData
    showTradeType conn tradeId "demands" tradeDemands tradeData
    showTradeType conn tradeId "author"  tradeAuthor  tradeData

-- modifyTrade
--     :: Connection -> MessageId -> (TradeData -> TradeData) -> DictM TradeData
-- modifyTrade conn tradeId f = do
--     tradeData <- getTrade conn tradeId <&> f . fromMaybe def
--     setTrade conn tradeId tradeData
--     return tradeData


readNPCType :: (Default a, Read a) => Connection -> Text -> Text -> IO a
readNPCType conn userID key =
  fromMaybe def <$> runMaybeT (readWithType "npcs" id conn userID key)

readNPCType'
  :: (Default a, Read a) => Connection -> Text -> Text -> IO (Maybe a)
readNPCType' conn userID key =
  runMaybeT (readWithType "npcs" id conn userID key)

showNPCType
  :: Show a => Connection -> Text -> Text -> Getting a b a -> b -> IO ()
showNPCType = showWithType "npcs" id

getNPC :: Text -> DictM NPCData
getNPC name = do
  conn <- asks envDb
  liftIO $ do
    memories <- readNPCType conn name "memories"
    avatar   <- readNPCType conn name "avatar"
    return NPCData { _npcMemories = memories, _npcAvatar = avatar }

getNPC' :: Text -> DictM (Maybe NPCData)
getNPC' name = do
  conn <- asks envDb
  liftIO . runMaybeT $ do
    memories <- MaybeT $ readNPCType' conn name "memories"
    avatar   <- MaybeT $ readNPCType' conn name "avatar"
    return NPCData { _npcMemories = memories, _npcAvatar = avatar }

setNPC :: Text -> NPCData -> DictM ()
setNPC name npcData = do
  conn <- asks envDb
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
  liftIO $ deleteWithType conn "npcs" name "memories"
  liftIO $ deleteWithType conn "npcs" name "avatar"

getallNPC :: DictM [(Text, NPCData)]
getallNPC = getallWithType "npcs" getNPC' id

pushRedButton :: DictM ()
pushRedButton = do
  conn <- asks envDb
  void . liftIO $ runRedis' conn flushdb
