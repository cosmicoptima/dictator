
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

import           Utils
import           Utils.DictM

import           Discord                        ( FromJSON
                                                , restCall
                                                )
import           Discord.Internal.Rest.Prelude  ( Request )
import           Discord.Requests
import           Discord.Types

import           Control.Lens
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.Random           ( newStdGen )
import           Data.Default                   ( Default(def) )
import qualified Data.Text                     as T
import qualified Database.Redis                as DB
import           Network.HTTP.Client            ( defaultManagerSettings )
import           Network.Wreq            hiding ( manager
                                                , options
                                                )
import           UnliftIO
import           UnliftIO.Concurrent            ( threadDelay )
import           Web.Twitter.Conduit

twitterTokens :: IO OAuth
twitterTokens =
    return $ twitterOAuth { oauthConsumerKey = "", oauthConsumerSecret = "" }

twitterCredentials :: IO Credential
twitterCredentials =
    return $ Credential [("oauth_token", ""), ("oauth_token_secret", "")]

makeTwitter :: IO TWInfo
makeTwitter = do undefined

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
