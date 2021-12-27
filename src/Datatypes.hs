{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Datatypes
    ( Team(..)
    , Points
    , TeamData(..)
    , Credit
    , UserData(..)
    , Rarity(..)
    , TrinketData(..)
    , TrinketId
    , getUserData
    , getTrinketData
    , getTeamData
    , setUserData
    , setTrinketData
    , setTeamData
    , otherTeam
    ) where

import           Relude                  hiding ( First
                                                , get
                                                )


import qualified Data.ByteString               as BS
import           Data.Default
import           Database.Redis
import           Discord.Internal.Types.Prelude

data Team = First | Second deriving (Eq, Generic, Read, Show)

otherTeam :: Team -> Team
otherTeam First  = Second
otherTeam Second = First

type Points = Integer

data TeamData = TeamData
    { teamForbidden :: [Text]
    , teamRole      :: Maybe RoleId
    , teamWarning   :: Maybe MessageId
    , teamPoints    :: Points
    }
    deriving (Eq, Generic, Read, Show)

instance Default TeamData where
    def = TeamData { teamForbidden = []
                   , teamRole      = Nothing
                   , teamWarning   = Nothing
                   , teamPoints    = 0
                   }

type Credit = Double

data UserData = UserData
    { userTeam    :: Maybe Team
    , userCredits :: Credit
    }
    deriving (Eq, Generic, Read, Show)

instance Default UserData where
    def = UserData { userTeam = Nothing, userCredits = 0 }

data Rarity = Common | Rare | Epic deriving (Eq, Generic, Read, Show)

type TrinketId = Int

data TrinketData = TrinketData
    { trinketName   :: Text
    , trinketRarity :: Rarity
    }
    deriving (Eq, Generic, Read, Show)

getUserData :: Connection -> UserId -> IO (Maybe UserData)
getUserData conn uId = runMaybeT $ do
    team <- liftIO (getWithType conn "users" (show uId) "team") >>= hoistMaybe
    team <- hoistMaybe $ (readMaybe . toString) team
    credits <-
        liftIO (getWithType conn "users" (show uId) "credits") >>= hoistMaybe
    credits <- hoistMaybe $ (readMaybe . toString) credits
    return UserData { userTeam = team, userCredits = credits }

getTeamData :: Connection -> Team -> IO (Maybe TeamData)
getTeamData conn team = runMaybeT $ do
    tWords <-
        liftIO (getWithType conn "teams" (toTeamKey team) "forbidden")
            >>= hoistMaybe
    tWords     <- hoistMaybe $ (readMaybe . toString) tWords
    warningPin <-
        liftIO (getWithType conn "teams" (toTeamKey team) "warning")
            >>= hoistMaybe
    warningPin <- hoistMaybe $ (readMaybe . toString) warningPin
    points     <-
        liftIO (getWithType conn "teams" (toTeamKey team) "points")
            >>= hoistMaybe
    points <- hoistMaybe $ (readMaybe . toString) points
    roleId <-
        liftIO (getWithType conn "teams" (toTeamKey team) "role") >>= hoistMaybe
    roleId <- hoistMaybe $ (readMaybe . toString) roleId
    return TeamData { teamForbidden = tWords
                    , teamWarning   = warningPin
                    , teamPoints    = points
                    , teamRole      = roleId
                    }

getTrinketData :: Connection -> TrinketId -> IO (Maybe TrinketData)
getTrinketData conn tId = runMaybeT $ do
    name <-
        liftIO (getWithType conn "trinkets" (show tId) "name") >>= hoistMaybe
    name   <- hoistMaybe $ (readMaybe . toString) name
    rarity <-
        liftIO (getWithType conn "trinkets" (show tId) "rarity") >>= hoistMaybe
    rarity <- hoistMaybe $ (readMaybe . toString) rarity
    return TrinketData { trinketName = name, trinketRarity = rarity }

setUserData :: Connection -> UserId -> UserData -> IO ()
setUserData conn uId uData = do
    setWithType conn "users" (show uId) "team"    (show $ userTeam uData)
    setWithType conn "users" (show uId) "credits" (show $ userCredits uData)

setTeamData :: Connection -> Team -> TeamData -> IO ()
setTeamData conn team tData = do
    setWithType conn
                "teams"
                (toTeamKey team)
                "forbidden"
                (show $ teamForbidden tData)
    setWithType conn
                "teams"
                (toTeamKey team)
                "warning"
                (show $ teamWarning tData)
    setWithType conn "teams" (toTeamKey team) "points" (show $ teamPoints tData)
    setWithType conn "teams" (toTeamKey team) "role"   (show $ teamRole tData)

setTrinketData :: Connection -> TrinketId -> TrinketData -> IO ()
setTrinketData conn tId tData = do
    setWithType conn "trinkets" (show tId) "name"   (show $ trinketName tData)
    setWithType conn "trinkets" (show tId) "rarity" (show $ trinketRarity tData)


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

-- setnxWithType :: Connection -> ByteString -> Text -> Text -> Text -> IO ()
-- setnxWithType conn type_ key field value = void . runRedis' conn $ setnx
--     (BS.intercalate ":" [type_, encodeUtf8 key, encodeUtf8 field])
--     (encodeUtf8 value)

-- asReadable :: Read a => IO (Maybe Text) -> IO (Maybe a)
-- asReadable = (<&> fmap (read . toString))

-- userGet :: Connection -> Snowflake -> Text -> IO (Maybe Text)
-- userGet conn = getWithType conn "user" . show

-- userSet :: Connection -> Snowflake -> Text -> Text -> IO ()
-- userSet conn = setWithType conn "user" . show

-- userSetnx :: Connection -> Snowflake -> Text -> Text -> IO ()
-- userSetnx conn = setnxWithType conn "user" . show

toTeamKey :: Team -> Text
toTeamKey = \case
    First  -> "1"
    Second -> "2"

-- teamGet :: Connection -> Team -> Text -> IO (Maybe Text)
-- teamGet conn = getWithType conn "teams" . toTeamKey

-- teamSet :: Connection -> Team -> Text -> Text -> IO ()
-- teamSet conn = setWithType conn "teams" . toTeamKey

-- trinketGet :: Connection -> Int -> Text -> IO (Maybe Text)
-- trinketGet conn = getWithType conn "trinket" . show

-- trinketSet :: Connection -> Int -> Text -> Text -> IO ()
-- trinketSet conn = setWithType conn "trinket" . show
