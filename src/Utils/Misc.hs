
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

module Utils.Misc where

import           Relude                  hiding ( First
                                                , get
                                                )



<<<<<<< HEAD
=======
import           Control.Lens
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.Random           ( newStdGen )
import qualified Data.ByteString.Char8         as BS
import           Data.Default                   ( Default(def) )
import           Data.List                      ( (!!) )
import qualified Data.Text                     as T
import qualified Database.Redis                as DB
import           Network.HTTP.Client            ( defaultManagerSettings )
import           Network.Wreq            hiding ( manager
                                                , options
                                                )
import           UnliftIO
import           UnliftIO.Concurrent            ( threadDelay )
>>>>>>> 34728bacbf497b71755e379a3fdf282f4e0d7eaa
import           Web.Twitter.Conduit

twitterThings :: IO [ByteString]
twitterThings = BS.lines <$> readFileBS "twitter.txt"

-- API keys and secrets.
twitterTokens :: IO OAuth
twitterTokens = do
    file <- twitterThings
    return $ twitterOAuth { oauthConsumerKey    = file !! 1
                          , oauthConsumerSecret = file !! 2
                          }

-- OAuth user context
-- "Allows an authorized Twitter App to perform a Twitter action on behalf of an account"
-- Basically, user tokens and secrets.
twitterCredentials :: IO Credential
twitterCredentials = do
    file <- twitterThings
    return $ Credential
        [("oauth_token", file !! 3), ("oauth_token_secret", file !! 4)]

makeTwitter :: IO TWInfo
makeTwitter = do
    tokens      <- twitterTokens
    credentials <- twitterCredentials
    let token = def { twOAuth = tokens, twCredential = credentials }
    return $ TWInfo token Nothing

<<<<<<< HEAD
-- sendTweet :: Text -> DictM ()
-- sendTweet tweet = do
--     info    <- asks envTwInfo
--     manager <- asks envTwManager
--     call'_ info manager $ statusesUpdate tweet
--     where call'_ info mgr = void . liftIO . call info mgr

    -- tokens      <- twitterTokens
    -- 
    -- credentials <- twitterCredentials
    -- let token = def { twOAuth = tokens, twCredential = credentials }
    --     info  = TWInfo token Nothing
=======
sendTweet :: Text -> DictM ()
sendTweet tweet = do
    info    <- asks envTwInfo
    manager <- asks envTwManager
    call'_ info manager $ statusesUpdate tweet
    where call'_ info mgr = void . liftIO . call info mgr
>>>>>>> 34728bacbf497b71755e379a3fdf282f4e0d7eaa
