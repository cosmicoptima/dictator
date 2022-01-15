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
    , globalWebhook
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
    , userWords
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
import qualified Data.Text                     as T
import           Database.Redis
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
    , _userWords    :: MultiSet Text
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
    , _globalWebhook      :: Maybe WebhookId
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
    :: Eq a => Text -> (a -> DictM (Maybe b)) -> (Text -> a) -> DictM [(a, b)]
getallWithType type_ f g = do
    conn        <- ask
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

getGlobal :: DictM GlobalData
getGlobal = getGlobal' <&> fromMaybe def
  where
    getGlobal' = do
        conn <- ask
        liftIO . runMaybeT $ do
            adhocStatus    <- readGlobalType conn "adhocFighter"
            arenaStatus    <- readGlobalType conn "arena"
            forbiddenWords <- readGlobalType conn "forbidden"
            warningPin     <- readGlobalType conn "warning"
            webhook        <- readGlobalType conn "webhook"
            return $ GlobalData adhocStatus
                                arenaStatus
                                forbiddenWords
                                warningPin
                                webhook

setGlobal :: GlobalData -> DictM ()
setGlobal globalData = do
    conn <- ask
    liftIO $ showGlobalType conn "adhocFighter" globalAdhocFighter globalData
    liftIO $ showGlobalType conn "arena" globalArena globalData
    liftIO $ showGlobalType conn "forbidden" globalForbidden globalData
    liftIO $ showGlobalType conn "warning" globalWarning globalData
    liftIO $ showGlobalType conn "webhook" globalWebhook globalData

modifyGlobal :: (GlobalData -> GlobalData) -> DictM GlobalData
modifyGlobal f = do
    globalData <- getGlobal <&> f
    setGlobal globalData
    return globalData


readUserType :: Read a => Connection -> UserId -> Text -> MaybeT IO a
readUserType = readWithType "users" show

showUserType
    :: Show a => Connection -> UserId -> Text -> Getting a b a -> b -> IO ()
showUserType = showWithType "users" show

getUser :: UserId -> DictM (Maybe UserData)
getUser userId = do
    conn <- ask
    liftIO . runMaybeT $ do
        credits  <- readUserType conn userId "credits"
        name     <- readUserType conn userId "name"
        trinkets <- readUserType conn userId "trinkets"
        points   <- readUserType conn userId "points"
        words    <- readUserType conn userId "words"

        return UserData { _userCredits  = credits
                        , _userName     = name
                        , _userTrinkets = trinkets
                        , _userPoints   = points
                        , _userWords    = words
                        }

getUserOr :: (Text -> Err) -> UserId -> DictM UserData
getUserOr f u = getUser u >>= \case
    Just user -> return user
    Nothing ->
        throwError (f $ "User with ID " <> show u <> " isn't in the database!")

setUser :: UserId -> UserData -> DictM ()
setUser userId userData = do
    conn <- ask
    if MS.size (userData ^. userTrinkets) > maxTrinkets
        then do
            void $ modifyUser
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
    liftIO $ showUserType conn userId "words" userWords userData
    where maxTrinkets = 10

modifyUser :: UserId -> (UserData -> UserData) -> DictM UserData
modifyUser userId f = do
    userData <- getUser userId <&> f . fromMaybe def
    setUser userId userData
    return userData

readTrinketType :: Read a => Connection -> TrinketID -> Text -> MaybeT IO a
readTrinketType = readWithType "trinkets" show

showTrinketType
    :: Show a => Connection -> TrinketID -> Text -> Getting a b a -> b -> IO ()
showTrinketType = showWithType "trinkets" show

getTrinket :: TrinketID -> DictM (Maybe TrinketData)
getTrinket id_ = do
    conn <- ask
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
    conn <- ask
    liftIO $ do
        showTrinketType conn trinketId "name"   trinketName   trinketData
        showTrinketType conn trinketId "rarity" trinketRarity trinketData

getallTrinket :: DictM [(TrinketID, TrinketData)]
getallTrinket = do
    getallWithType "trinkets" getTrinket (read . toString)

countTrinket :: DictM Int
countTrinket = ask >>= countWithType "trinkets"


readLocationType :: Read a => Connection -> Text -> Text -> MaybeT IO a
readLocationType = readWithType "location" id

showLocationType
    :: Show a => Connection -> Text -> Text -> Getting a b a -> b -> IO ()
showLocationType = showWithType "location" id

getLocation :: Text -> DictM (Maybe LocationData)
getLocation name = do
    conn <- ask
    liftIO . runMaybeT $ readLocationType conn name "trinkets" <&> LocationData

getLocationOr :: (Text -> Err) -> Text -> DictM LocationData
getLocationOr f name = getLocation name >>= \case
    Just location -> return location
    Nothing       -> throwError
        (  f
        $  "Location "
        <> name
        <> " can't be retrieved because it doesn't exist"
        )

setLocation :: Text -> LocationData -> DictM ()
setLocation name locationData = do
    conn <- ask
    liftIO $ showLocationType conn name "trinkets" locationTrinkets locationData

modifyLocation :: Text -> (LocationData -> LocationData) -> DictM LocationData
modifyLocation name f = do
    locationData <- getLocation name <&> f . fromMaybe def
    setLocation name locationData
    return locationData

getallLocation :: DictM [(Text, LocationData)]
getallLocation = getallWithType "location" getLocation id

countLocation :: DictM Int
countLocation = ask >>= countWithType "location"


readTradeType :: Read a => Connection -> MessageId -> Text -> MaybeT IO a
readTradeType = readWithType "trades" show

showTradeType
    :: Show a => Connection -> MessageId -> Text -> Getting a b a -> b -> IO ()
showTradeType = showWithType "trades" show

getTrade :: MessageId -> DictM (Maybe TradeData)
getTrade tradeId = do
    conn <- ask
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
    conn <- ask
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

displayItems :: Items -> DictM Text
displayItems it = do
    trinketsDisplay <- showTrinkets (it ^. itemTrinkets . to MS.elems)
    let wordsDisplay = fmap show (it ^. itemWords . to MS.elems)
        display =
            T.intercalate ", "
                .  filter (not . T.null)
                $  showCredits (it ^. itemCredits)
                :  wordsDisplay
                ++ trinketsDisplay
    return $ if display == "" then "nothing" else display
  where
    showCredits 0 = ""
    showCredits n = show n <> "c"

    showTrinkets = mapM $ \trinketId -> do
        trinketData <- getTrinketOr Complaint trinketId
        displayTrinket trinketId trinketData


pushRedButton :: DictM ()
pushRedButton = do
    conn <- ask
    void . liftIO $ runRedis' conn flushdb
