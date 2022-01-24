
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
