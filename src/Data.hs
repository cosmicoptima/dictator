{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data where

import           Relude                  hiding ( First
                                                , get
                                                )

import           DiscordUtils                   ( DH )

import qualified Data.ByteString               as BS
import           Database.Redis
import           Discord.Types                  ( Snowflake )
import           Relude.Unsafe                  ( read )


runRedis' :: Connection -> Redis (Either Reply a) -> DH a
runRedis' c f = liftIO (runRedis c f) >>= either (die . show) return


getWithType :: Connection -> ByteString -> Text -> Text -> DH (Maybe Text)
getWithType conn type_ key field =
    runRedis'
            conn
            ( get
            . BS.intercalate ":"
            $ [type_, encodeUtf8 key, encodeUtf8 field]
            )
        <&> fmap decodeUtf8

setWithType :: Connection -> ByteString -> Text -> Text -> Text -> DH ()
setWithType conn type_ key field value = void . runRedis' conn $ set
    (BS.intercalate ":" [type_, encodeUtf8 key, encodeUtf8 field])
    (encodeUtf8 value)

setnxWithType :: Connection -> ByteString -> Text -> Text -> Text -> DH ()
setnxWithType conn type_ key field value = void . runRedis' conn $ setnx
    (BS.intercalate ":" [type_, encodeUtf8 key, encodeUtf8 field])
    (encodeUtf8 value)

asReadable :: Read a => DH (Maybe Text) -> DH (Maybe a)
asReadable = (<&> fmap (read . toString))


userGet :: Connection -> Snowflake -> Text -> DH (Maybe Text)
userGet conn = getWithType conn "user" . show

userSet :: Connection -> Snowflake -> Text -> Text -> DH ()
userSet conn = setWithType conn "user" . show

userSetnx :: Connection -> Snowflake -> Text -> Text -> DH ()
userSetnx conn = setnxWithType conn "user" . show


data Team = First | Second | Neutral deriving (Eq, Generic, Read, Show)

toTeamKey :: Team -> Text
toTeamKey = \case
    Neutral -> "0"
    First   -> "1"
    Second  -> "2"

teamGet :: Connection -> Team -> Text -> DH (Maybe Text)
teamGet conn = getWithType conn "teams" . toTeamKey

teamSet :: Connection -> Team -> Text -> Text -> DH ()
teamSet conn = setWithType conn "teams" . toTeamKey
