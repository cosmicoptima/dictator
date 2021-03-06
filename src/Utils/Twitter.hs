{-# LANGUAGE FlexibleInstances #-}

module Utils.Twitter where

import           Relude                  hiding ( First
                                                , get
                                                )



import           Control.Lens.Operators
import           Control.Lens.Prism
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8         as BS
import qualified Data.Text                     as T
import           Network.Wreq
import           Safe                           ( readMay )
import           Utils.DictM
-- import           Utils.Discord                  ( sendMessageToLogs )

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

sendTweet :: Text -> DictM Int
sendTweet tweet = do
  TwitterAuth apiKey apiSecret userToken tokenSecret <- asks envTw
  let opts =
        defaults & auth ?~ oauth1Auth apiKey apiSecret userToken tokenSecret
      tweet' = String $ T.take 280 tweet
  res <- liftIO $ postWith opts
                           "https://api.twitter.com/2/tweets"
                           (object [("text", tweet')])
  let parsed :: Maybe Value = decode (res ^. responseBody)
      tweetId               = parsed ^? _Just . key "data" . key "id" . _String
  maybe (throwError $ Complaint "The bird is dead.")
        return
        (tweetId >>= readMay . toString)
