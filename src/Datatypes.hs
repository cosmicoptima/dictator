{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Datatypes
    ( Team(..)
    , Points
    , TeamData(..)
    , Credit
    , UserData(..)
    , Rarity(..)
    , TrinketData(..)
    , TrinketId
    , showTrinket
    , getUserData
    , getTrinketData
    , getTeamData
    , setUserData
    , setTrinketData
    , setTeamData
    , modifyUserData
    , modifyTeamData
    , otherTeam
    , teamForbiddenL
    , teamRoleL
    , teamWarningL
    , teamPointsL
    , userTeamL
    , userCreditsL
    , userTrinketsL
    , trinketNameL
    , trinketRarityL
    ) where

import           Relude                  hiding ( First
                                                , get
                                                )


import           Control.Lens                   ( makeLensesFor )
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

makeLensesFor (fmap (\n -> (n, n <> "L")) ["teamForbidden", "teamRole", "teamWarning", "teamPoints"]) ''TeamData

instance Default TeamData where
    def = TeamData { teamForbidden = []
                   , teamRole      = Nothing
                   , teamWarning   = Nothing
                   , teamPoints    = 0
                   }

type Credit = Double
type TrinketId = Int

data UserData = UserData
    { userTeam     :: Maybe Team
    , userCredits  :: Credit
    , userTrinkets :: [TrinketId]
    }
    deriving (Eq, Generic, Read, Show)

makeLensesFor (fmap (\n -> (n, n <> "L")) ["userTeam", "userCredits", "userTrinkets"]) ''UserData

instance Default UserData where
    def = UserData { userTeam = Nothing, userCredits = 0, userTrinkets = [] }

data Rarity = Common | Rare | Epic deriving (Eq, Generic, Read, Show)

data TrinketData = TrinketData
    { trinketName   :: Text
    , trinketRarity :: Rarity
    }
    deriving (Eq, Generic, Read, Show)

makeLensesFor (fmap (\n -> (n, n <> "L")) ["trinketName", "trinketRarity"]) ''TrinketData

data GuildData = GuildData
    { guildScrapyard :: [TrinketId]
    }

makeLensesFor (fmap (\n -> (n, n <> "L")) ["guildScrapyard"]) ''GuildData

instance Default GuildData where
    def = GuildData { guildScrapyard = [] }


showTrinket :: TrinketId -> TrinketData -> Text
showTrinket tId trinket =
    "#"
        <> show tId
        <> " "
        <> trinketName trinket
        <> " ("
        <> (show . trinketRarity) trinket
        <> ")"

getUserData :: Connection -> UserId -> IO (Maybe UserData)
getUserData conn uId = runMaybeT $ do
    team <- liftIO (getWithType conn "users" (show uId) "team") >>= hoistMaybe
    team <- hoistMaybe $ (readMaybe . toString) team
    credits <-
        liftIO (getWithType conn "users" (show uId) "credits") >>= hoistMaybe
    credits  <- hoistMaybe $ (readMaybe . toString) credits
    trinkets <-
        liftIO (getWithType conn "users" (show uId) "trinkets") >>= hoistMaybe
    trinkets <- hoistMaybe $ (readMaybe . toString) trinkets
    return UserData { userTeam     = team
                    , userCredits  = credits
                    , userTrinkets = trinkets
                    }

setUserData :: Connection -> UserId -> UserData -> IO ()
setUserData conn uId uData = do
    setWithType conn "users" (show uId) "team"     (show $ userTeam uData)
    setWithType conn "users" (show uId) "credits"  (show $ userCredits uData)
    setWithType conn "users" (show uId) "trinkets" (show $ userTrinkets uData)

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

modifyUserData :: Connection -> UserId -> (UserData -> UserData) -> IO UserData
modifyUserData conn uId f = do
    userData <- getUserData conn uId <&> fromMaybe def
    let userData' = f userData
    setUserData conn uId userData'
    return userData'

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

modifyTeamData :: Connection -> Team -> (TeamData -> TeamData) -> IO TeamData
modifyTeamData conn team f = do
    teamData <- getTeamData conn team <&> fromMaybe def
    let teamData' = f teamData
    setTeamData conn team teamData'
    return teamData'

getTrinketData :: Connection -> TrinketId -> IO (Maybe TrinketData)
getTrinketData conn tId = runMaybeT $ do
    name <-
        liftIO (getWithType conn "trinkets" (show tId) "name") >>= hoistMaybe
    name   <- hoistMaybe $ (readMaybe . toString) name
    rarity <-
        liftIO (getWithType conn "trinkets" (show tId) "rarity") >>= hoistMaybe
    rarity <- hoistMaybe $ (readMaybe . toString) rarity
    return TrinketData { trinketName = name, trinketRarity = rarity }

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
