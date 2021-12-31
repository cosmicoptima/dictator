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
    , arenaStatus
    , getGlobal
    , setGlobal
    , modifyGlobal

    -- teams
    , Team(..)
    , TeamData(..)
    , Points
    , otherTeam
    , teamForbidden
    , teamRole
    , teamWarning
    , teamPoints
    , getTeam
    , setTeam
    , modifyTeam

    -- users
    , Credit
    , UserData(..)
    , userTeam
    , userCredits
    , userTrinkets
    , getUser
    , getUserOr
    , setUser
    , modifyUser

    -- trinkets
    , TrinketID
    , TrinketData(..)
    , Rarity(..)
    , trinketName
    , trinketRarity
    , displayTrinket
    , getTrinket
    , getTrinketOr
    , setTrinket

    -- locations
    , LocationData(..)
    , locationTrinkets
    , getLocation
    , setLocation
    , modifyLocation
    , getallLocation
    , countLocation
    , getLocationOr
    ) where

import           Relude                  hiding ( First
                                                , get
                                                , many
                                                )

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
import           Database.Redis
import           Discord.Internal.Types.Prelude
import           Text.Parsec             hiding ( Reply )


-- TYPES (definitions and instances)
------------------------------------

newtype GlobalData = GlobalData { _arenaStatus :: Maybe UserId } deriving Generic

makeLenses ''GlobalData

instance Default GlobalData


data Team = First | Second deriving (Eq, Generic, Read, Show)

otherTeam :: Team -> Team
otherTeam First  = Second
otherTeam Second = First

type Points = Integer

data TeamData = TeamData
    { _teamForbidden :: [Text]
    , _teamRole      :: Maybe RoleId
    , _teamWarning   :: Maybe MessageId
    , _teamPoints    :: Points
    }
    deriving Generic

makeLenses ''TeamData

instance Default TeamData


data Rarity = Common | Uncommon | Rare | Legendary deriving (Eq, Ord, Generic, Read, Show)
type TrinketID = Int

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


type Credit = Double

data UserData = UserData
    { _userTeam     :: Maybe Team
    , _userCredits  :: Credit
    , _userTrinkets :: MultiSet TrinketID
    }
    deriving (Eq, Generic, Read, Show)

makeLenses ''UserData

instance Default UserData


newtype LocationData = LocationData
    { _locationTrinkets :: MultiSet TrinketID
    } deriving Generic

makeLenses ''LocationData

instance Default LocationData


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

countWithType :: MonadIO m => Connection -> Text -> m Int
countWithType conn type_ =
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
    :: MonadIO m => Connection -> Text -> (Text -> m (Maybe a)) -> m [(Text, a)]
getallWithType conn type_ f = do
    distinctIDs <-
        liftIO
        $   runRedis' conn (keys $ encodeUtf8 type_ <> ":*")
        <&> nub
        .   rights
        .   map (fmap fromString . parse parser "")
    mapM (\x -> f x <&> (x, )) distinctIDs <&> mapMaybe raiseMaybe
  where
    raiseMaybe = \case
        (a, Just b ) -> Just (a, b)
        (_, Nothing) -> Nothing
    parser = do
        void . string $ toString type_ <> ":"
        many (noneOf ":")


shiftR3 :: (a -> b -> c -> d) -> c -> a -> b -> d
shiftR3 f x y z = f y z x

readGlobalType :: Read a => Connection -> Text -> MaybeT IO a
readGlobalType = shiftR3 (readWithType "global" (const "")) ""

showGlobalType :: Show a => Connection -> Text -> Getting a b a -> b -> IO ()
showGlobalType = shiftR3 (showWithType "global" (const "")) ""

getGlobal :: Connection -> DictM GlobalData
getGlobal conn = getGlobal' <&> fromMaybe def
  where
    getGlobal' = liftIO . runMaybeT $ do
        arenaStatus' <- readGlobalType conn "arena"
        return $ GlobalData arenaStatus'

setGlobal :: Connection -> GlobalData -> DictM ()
setGlobal conn globalData =
    liftIO $ showGlobalType conn "arena" arenaStatus globalData

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
    team     <- readUserType conn userId "team"
    credits  <- readUserType conn userId "credits"
    trinkets <- readUserType conn userId "trinkets"

    return UserData { _userTeam     = team
                    , _userCredits  = credits
                    , _userTrinkets = trinkets
                    }

getUserOr :: (Text -> Err) -> Connection -> UserId -> DictM UserData
getUserOr f conn u = getUser conn u >>= \case
    Just user -> return user
    Nothing ->
        throwError (f $ "User with ID " <> show u <> " isn't in the database!")

setUser :: Connection -> UserId -> UserData -> DictM ()
setUser conn userId userData = do
    liftIO $ showUserType conn userId "team" userTeam userData
    liftIO $ showUserType conn userId "credits" userCredits userData
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
    where maxTrinkets = 10

modifyUser :: Connection -> UserId -> (UserData -> UserData) -> DictM UserData
modifyUser conn userId f = do
    userData <- getUser conn userId <&> f . fromMaybe def
    setUser conn userId userData
    return userData


toTeamKey :: Team -> Text
toTeamKey = \case
    First  -> "1"
    Second -> "2"

readTeamType :: Read a => Connection -> Team -> Text -> MaybeT IO a
readTeamType = readWithType "teams" toTeamKey

showTeamType
    :: Show a => Connection -> Team -> Text -> Getting a b a -> b -> IO ()
showTeamType = showWithType "teams" toTeamKey

getTeam :: Connection -> Team -> DictM (Maybe TeamData)
getTeam conn team = liftIO . runMaybeT $ do
    points    <- readTeamType conn team "points"
    role      <- readTeamType conn team "role"
    forbidden <- readTeamType conn team "forbidden"
    warning   <- readTeamType conn team "warning"

    return TeamData { _teamPoints    = points
                    , _teamRole      = role
                    , _teamForbidden = forbidden
                    , _teamWarning   = warning
                    }

setTeam :: Connection -> Team -> TeamData -> DictM ()
setTeam conn team teamData = liftIO $ do
    showTeamType conn team "points"    teamPoints    teamData
    showTeamType conn team "role"      teamRole      teamData
    showTeamType conn team "forbidden" teamForbidden teamData
    showTeamType conn team "warning"   teamWarning   teamData

modifyTeam :: Connection -> Team -> (TeamData -> TeamData) -> DictM TeamData
modifyTeam conn team f = do
    teamData <- getTeam conn team <&> f . fromMaybe def
    setTeam conn team teamData
    return teamData


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
getallLocation conn = getallWithType conn "location" (getLocation conn)

countLocation :: Connection -> DictM Int
countLocation = flip countWithType "location"
