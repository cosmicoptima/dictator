
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils.Twitter where

import           Relude                  hiding ( First
                                                , get
                                                )



import           Control.Lens.Operators
import           Data.Aeson
import qualified Data.ByteString.Char8         as BS
import qualified Data.Text                     as T
import           Network.Wreq
import           Utils.DictM

-- dictTwitterId = 1485338136741875717

twitterAuth :: IO TwitterAuth
twitterAuth = do
    -- Remove comment lines starting with #
    twitterLines <- filter (not . BS.isPrefixOf "#") . BS.lines <$> readFileBS
        "twitter.txt"
    let [apiKey, apiSecret, userToken, tokenSecret] = take 4 twitterLines
    return $ TwitterAuth { twAuthApiKey      = apiKey
                         , twAuthApiSecret   = apiSecret
                         , twAuthUserToken   = userToken
                         , twAuthTokenSecret = tokenSecret
                         }


sendTweet :: Text -> DictM ()
sendTweet tweet = do
    TwitterAuth apiKey apiSecret userToken tokenSecret <- asks envTw
    let opts =
            defaults & auth ?~ oauth1Auth apiKey apiSecret userToken tokenSecret
        tweet' = String $ T.take 280 tweet
    void . liftIO $ postWith opts
                             "https://api.twitter.com/2/tweets"
                             (object [("text", tweet')])



