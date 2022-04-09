-- | Specifies the game involving trinkets, locations and credits.

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game
  ( sendWebhookMessage
  , impersonateUser
  , impersonateUserRandom
  , impersonateNameRandom
  ) where

import           Relude                  hiding ( First
                                                , get
                                                , many
                                                , optional
                                                )

import           Game.Data
import           Utils.DictM
import           Utils.Discord
import           Utils.Language


import           Control.Lens            hiding ( noneOf )
import           Control.Monad.Except           ( MonadError(throwError) )
import qualified Data.Text                     as T
import           Discord.Internal.Types.Prelude
import           Discord.Requests
import           Discord.Types
import           Text.Parsec             hiding ( (<|>) )


sendWebhookMessage :: ChannelId -> Text -> Text -> Maybe Text -> DictM ()
sendWebhookMessage whereTo whatTo whoTo mayAvatar = do
  maybeHook <- view globalWebhook <$> getGlobal
  hook      <- case maybeHook of
    Just hook -> do
      restCall' . ModifyWebhook hook $ ModifyWebhookOpts (Just whoTo)
                                                         mayAvatar
                                                         (Just whereTo)
    Nothing -> do
      hook <- restCall' . CreateWebhook whereTo $ CreateWebhookOpts whoTo
                                                                    mayAvatar
      void . modifyGlobal $ set globalWebhook (Just $ webhookId hook)
      return hook

  restCall'
    . ExecuteWebhookWithToken (webhookId hook) (webhookToken hook)
    . ExecuteWebhookWithTokenOpts (Just whoTo)
    $ WebhookContentText whatTo


impersonateUser :: GuildMember -> ChannelId -> Text -> DictM ()
impersonateUser member whereTo whatTo = do
  let name = fromMaybe (userName . memberUser $ member) $ memberNick member
      userID = (userId . memberUser) member
      mayAvatarHash = userAvatar . memberUser $ member
  mayAvatar <- maybe (pure Nothing)
                     (pure . Just . encodeAvatarData <=< getAvatarData userID)
                     mayAvatarHash
  sendWebhookMessage whereTo whatTo name mayAvatar

impersonateUserRandom :: GuildMember -> ChannelId -> DictM ()
impersonateUserRandom member channel = do
  messages <- restCall' $ GetChannelMessages channel (50, LatestMessages)
  let prompt =
        T.concat (map renderMessage . reverse $ messages)
          <> (userName . memberUser) member
          <> "\n"
  output <- getJ1 32 prompt <&> parse parser ""
  case output of
    Left  f -> throwError $ Fuckup (show f)
    Right t -> impersonateUser member channel t
 where
  renderMessage m =
    (userName . messageAuthor) m <> "\n" <> messageText m <> "\n\n"
  parser = fromString <$> many (noneOf "\n")

impersonateNameRandom :: ChannelId -> Text -> DictM ()
impersonateNameRandom channel name = do
  messages <- restCall' $ GetChannelMessages channel (50, LatestMessages)
  let prompt =
        T.concat (map renderMessage . reverse $ messages) <> name <> "\n"
  output <- getJ1 32 prompt <&> parse parser ""
  case output of
    Left  f -> throwError $ Fuckup (show f)
    Right t -> sendWebhookMessage channel t name Nothing
 where
  renderMessage m =
    (userName . messageAuthor) m <> "\n" <> messageText m <> "\n\n"
  parser = fromString <$> many (noneOf "\n")
