-- | Abstracts database access with helper functions and types.

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}

module Game.Data
    (
    -- global
      GlobalData
    , Fighter(..)
    , fighterOwner
    , fighterTrinket
    , globalAdhocFighter
    , globalArena
    , globalForbidden
    , globalWarning
    , getGlobal
    , setGlobal
    , modifyGlobal

    -- users
    , UserData(..)
    , Username(..)
    , userCredits
    , userName
    , userPoints
    , userTrinkets
    , getUser
    , getUserOr
    , setUser
    , modifyUser

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
    , displayItems

    -- red button
    , pushRedButton
    ) where

import           Relude                  hiding ( First
                                                , get
                                                , many
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
import           Data.List
import qualified Data.Text                     as T
import           Database.Redis
import qualified Database.Redis                as DB
import           Discord.Internal.Types.Prelude
import           Game.Items
import           Relude.Unsafe
import           Text.Parsec             hiding ( Reply )


-- TYPES (definitions and instances)
------------------------------------

data Rarity = Common | Uncommon | Rare | Legendary deriving (Eq, Ord, Generic, Read, Show)

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
        Common    -> "common"
        Uncommon  -> "uncommon"
        Rare      -> "rare"
        Legendary -> "legendary"

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

data UserData = UserData
    { _userCredits  :: Credit
    , _userName     :: Username
    , _userTrinkets :: MultiSet TrinketID
    , _userPoints   :: Integer
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
    { _globalAdhocFighter :: Maybe Fighter
    , _globalArena        :: MultiSet Fighter
    , _globalForbidden    :: [Text]
    , _globalWarning      :: Maybe MessageId
    }
    deriving (Generic, Read, Show) -- show is for debug, can be removed eventually

makeLenses ''GlobalData

instance Default GlobalData

data TradeStatus = OpenTrade | ClosedTrade deriving (Eq, Show, Read, Generic)

data TradeData = TradeData
    { _tradeStatus  :: TradeStatus
    , _tradeOffers  :: Items
    , _tradeDemands :: Items
    , _tradeAuthor  :: UserId
    }
    deriving (Read, Show, Generic)

makeLenses ''TradeData


-- DATABASE
-----------

runRedis' :: Connection -> Redis (Either Reply a) -> IO a
runRedis' c f = runRedis c f >>= either (die . show) return

getWithType :: Connection -> ByteString -> Text -> Text -> IO (Maybe Text)
getWithType conn type_ key field =
    runRedis'
            conn
            ( get
            . BS.intercalate ":"
            $ [type_, encodeUtf8 key, encodeUtf8 field]
            )
        <&> fmap decodeUtf8

setWithType :: Connection -> ByteString -> Text -> Text -> Text -> IO ()
setWithType conn type_ key field value = void . runRedis' conn $ set
    (BS.intercalate ":" [type_, encodeUtf8 key, encodeUtf8 field])
    (encodeUtf8 value)

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
    :: Eq a
    => Text
    -> Connection
    -> (a -> DictM (Maybe b))
    -> (Text -> a)
    -> DictM [(a, b)]
getallWithType type_ conn f g = do
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


readGlobalType :: Read a => Connection -> Text -> MaybeT IO a
readGlobalType = flip (readWithType "global" (const "")) ()

showGlobalType :: Show a => Connection -> Text -> Getting a b a -> b -> IO ()
showGlobalType = flip (showWithType "global" (const "")) ()

getGlobal :: Connection -> DictM GlobalData
getGlobal conn = getGlobal' <&> fromMaybe def
  where
    getGlobal' = liftIO . runMaybeT $ do
        adhocStatus    <- readGlobalType conn "adhocFighter"
        arenaStatus    <- readGlobalType conn "arena"
        forbiddenWords <- readGlobalType conn "forbidden"
        warningPin     <- readGlobalType conn "warning"
        return $ GlobalData adhocStatus arenaStatus forbiddenWords warningPin

setGlobal :: Connection -> GlobalData -> DictM ()
setGlobal conn globalData = do
    liftIO $ showGlobalType conn "adhocFighter" globalAdhocFighter globalData
    liftIO $ showGlobalType conn "arena" globalArena globalData
    liftIO $ showGlobalType conn "forbidden" globalForbidden globalData
    liftIO $ showGlobalType conn "warning" globalWarning globalData

modifyGlobal :: Connection -> (GlobalData -> GlobalData) -> DictM GlobalData
modifyGlobal conn f = do
    globalData <- getGlobal conn <&> f
    setGlobal conn globalData
    return globalData


readUserType :: Read a => Connection -> UserId -> Text -> MaybeT IO a
readUserType = readWithType "users" show

showUserType
    :: Show a => Connection -> UserId -> Text -> Getting a b a -> b -> IO ()
showUserType = showWithType "users" show

getUser :: Connection -> UserId -> DictM (Maybe UserData)
getUser conn userId = liftIO . runMaybeT $ do
    credits  <- readUserType conn userId "credits"
    name     <- readUserType conn userId "name"
    trinkets <- readUserType conn userId "trinkets"
    points   <- readUserType conn userId "points"

    return UserData { _userCredits  = credits
                    , _userName     = name
                    , _userTrinkets = trinkets
                    , _userPoints   = points
                    }

getUserOr :: (Text -> Err) -> Connection -> UserId -> DictM UserData
getUserOr f conn u = getUser conn u >>= \case
    Just user -> return user
    Nothing ->
        throwError (f $ "User with ID " <> show u <> " isn't in the database!")

setUser :: Connection -> UserId -> UserData -> DictM ()
setUser conn userId userData = do
    if MS.size (userData ^. userTrinkets) > maxTrinkets
        then do
            void $ modifyUser
                conn
                userId
                (over userTrinkets $ MS.fromList . take maxTrinkets . MS.elems)
            throwError
                (  Complaint
                $  "Nobody *needs* more than "
                <> show maxTrinkets
                <> " trinkets..."
                )
        else liftIO $ showUserType conn userId "trinkets" userTrinkets userData
    liftIO $ showUserType conn userId "name" userName userData
    liftIO $ showUserType conn userId "credits" userCredits userData
    liftIO $ showUserType conn userId "points" userPoints userData
    where maxTrinkets = 10

modifyUser :: Connection -> UserId -> (UserData -> UserData) -> DictM UserData
modifyUser conn userId f = do
    userData <- getUser conn userId <&> f . fromMaybe def
    setUser conn userId userData
    return userData

readTrinketType :: Read a => Connection -> TrinketID -> Text -> MaybeT IO a
readTrinketType = readWithType "trinkets" show

showTrinketType
    :: Show a => Connection -> TrinketID -> Text -> Getting a b a -> b -> IO ()
showTrinketType = showWithType "trinkets" show

getTrinket :: Connection -> TrinketID -> DictM (Maybe TrinketData)
getTrinket conn id_ = liftIO . runMaybeT $ do
    name   <- readTrinketType conn id_ "name"
    rarity <- readTrinketType conn id_ "rarity"

    return TrinketData { _trinketName = name, _trinketRarity = rarity }

getTrinketOr :: (Text -> Err) -> Connection -> TrinketID -> DictM TrinketData
getTrinketOr f conn t = getTrinket conn t >>= \case
    Just trinket -> return trinket
    Nothing ->
        throwError (f $ "Trinket with ID " <> show t <> " isn't even real!")

setTrinket :: Connection -> TrinketID -> TrinketData -> DictM ()
setTrinket conn trinketId trinketData = liftIO $ do
    showTrinketType conn trinketId "name"   trinketName   trinketData
    showTrinketType conn trinketId "rarity" trinketRarity trinketData

getallTrinket :: Connection -> DictM [(TrinketID, TrinketData)]
getallTrinket conn =
    getallWithType "trinkets" conn (getTrinket conn) (read . toString)

countTrinket :: Connection -> DictM Int
countTrinket = countWithType "trinkets"


readLocationType :: Read a => Connection -> Text -> Text -> MaybeT IO a
readLocationType = readWithType "location" id

showLocationType
    :: Show a => Connection -> Text -> Text -> Getting a b a -> b -> IO ()
showLocationType = showWithType "location" id

getLocation :: Connection -> Text -> DictM (Maybe LocationData)
getLocation conn name =
    liftIO . runMaybeT $ readLocationType conn name "trinkets" <&> LocationData

getLocationOr :: (Text -> Err) -> Connection -> Text -> DictM LocationData
getLocationOr f conn name = getLocation conn name >>= \case
    Just location -> return location
    Nothing       -> throwError
        (  f
        $  "Location "
        <> name
        <> " can't be retrieved because it doesn't exist"
        )

setLocation :: Connection -> Text -> LocationData -> DictM ()
setLocation conn name locationData =
    liftIO $ showLocationType conn name "trinkets" locationTrinkets locationData

modifyLocation
    :: Connection
    -> Text
    -> (LocationData -> LocationData)
    -> DictM LocationData
modifyLocation conn name f = do
    locationData <- getLocation conn name <&> f . fromMaybe def
    setLocation conn name locationData
    return locationData

getallLocation :: Connection -> DictM [(Text, LocationData)]
getallLocation conn = getallWithType "location" conn (getLocation conn) id

countLocation :: Connection -> DictM Int
countLocation = countWithType "location"


readTradeType :: Read a => Connection -> MessageId -> Text -> MaybeT IO a
readTradeType = readWithType "trades" show

showTradeType
    :: Show a => Connection -> MessageId -> Text -> Getting a b a -> b -> IO ()
showTradeType = showWithType "trades" show

getTrade :: Connection -> MessageId -> DictM (Maybe TradeData)
getTrade conn tradeId = liftIO . runMaybeT $ do
    status  <- readTradeType conn tradeId "status"
    offers  <- readTradeType conn tradeId "offers"
    demands <- readTradeType conn tradeId "demands"
    author  <- readTradeType conn tradeId "author"

    return TradeData { _tradeStatus  = status
                     , _tradeOffers  = offers
                     , _tradeDemands = demands
                     , _tradeAuthor  = author
                     }

setTrade :: Connection -> MessageId -> TradeData -> DictM ()
setTrade conn tradeId tradeData = do
    liftIO $ showTradeType conn tradeId "status" tradeStatus tradeData
    liftIO $ showTradeType conn tradeId "offers" tradeOffers tradeData
    liftIO $ showTradeType conn tradeId "demands" tradeDemands tradeData
    liftIO $ showTradeType conn tradeId "author" tradeAuthor tradeData

-- modifyTrade
--     :: Connection -> MessageId -> (TradeData -> TradeData) -> DictM TradeData
-- modifyTrade conn tradeId f = do
--     tradeData <- getTrade conn tradeId <&> f . fromMaybe def
--     setTrade conn tradeId tradeData
--     return tradeData

displayItems :: DB.Connection -> Items -> DictM Text
displayItems conn it = do
    trinketsDisplay <- showTrinkets (it ^. itemTrinkets . to MS.elems)
    let display =
            T.intercalate ", "
                . filter (not . T.null)
                $ showCredits (it ^. itemCredits)
                : trinketsDisplay
    return $ if display == "" then "nothing" else display
  where
    showCredits 0 = ""
    showCredits n = show n <> "c"

    showTrinkets = mapM $ \trinketId -> do
        trinketData <- getTrinketOr Complaint conn trinketId
        displayTrinket trinketId trinketData


pushRedButton :: Connection -> DictM ()
pushRedButton conn = void . liftIO $ runRedis' conn flushdb
