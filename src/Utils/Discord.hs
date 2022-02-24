-- | discord-haskell but better.

-- (TODO the bits of dict-specific code should be moved imo)

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

module Utils.Discord where

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
import           Data.ByteString.Base64
import           Data.Char                      ( isSpace )
import           Data.Default                   ( Default(def) )
import qualified Data.Text                     as T
import           Network.Wreq            hiding ( options )
import           UnliftIO
import           UnliftIO.Concurrent            ( threadDelay )


-- DictM
--------

logErrors :: DictM a -> ReaderT Env DH ()
logErrors m = runExceptT m >>= \case
  Right _               -> return ()
  Left  (Fuckup    err) -> debugPrint err
  Left  (Complaint err) -> do
    ignoreErrors . sendMessageToGeneral $ err
  Left (Gibberish err) -> do
    ignoreErrors
      .  sendMessageToGeneral
      $  "What the fuck is this?```"
      <> show err
      <> "```"
  Left GTFO -> return ()

logErrors' :: Env -> DictM a -> DH ()
logErrors' conn = flip runReaderT conn . logErrors

logErrorsInChannel :: ChannelId -> DictM a -> ReaderT Env DH ()
logErrorsInChannel channel m = runExceptT m >>= \case
  Right _               -> return ()
  Left  (Fuckup    err) -> debugPrint err
  Left  (Complaint err) -> do
    ignoreErrors . sendMessage channel $ err
  Left (Gibberish err) -> do
    ignoreErrors
      .  sendMessage channel
      $  "What the fuck is this?```"
      <> show err
      <> "```"
  Left GTFO -> return ()

dieOnErrors :: DictM a -> ReaderT Env DH a
dieOnErrors m = runExceptT m >>= \case
  Left  err -> debugPrint err >> (die . show) err
  Right a   -> return a


mapConcurrently'_ :: Traversable t => (a -> DictM b) -> t a -> DictM ()
mapConcurrently'_ f = lift . mapConcurrently_ (logErrors . f)

mapConcurrently' :: Traversable t => (a -> DictM b) -> t a -> DictM (t b)
mapConcurrently' f = lift . mapConcurrently (dieOnErrors . f)

forConcurrently' :: Traversable t => t a -> (a -> DictM b) -> DictM (t b)
forConcurrently' = flip mapConcurrently'

forConcurrently'_ :: Traversable t => t a -> (a -> DictM b) -> DictM ()
forConcurrently'_ = flip mapConcurrently'_

-- all else
-----------

pnppcId :: GuildId
pnppcId = 878376227428245555

dictId :: UserId
dictId = 878385073735467060

ownedEmoji :: Text
ownedEmoji = "owned:899536714773717012"

isDict :: User -> Bool
isDict = (== dictId) . userId


-- | like `restCall` but specialised for the DictM monad. Throws `Fuckup` on error.
restCall' :: (FromJSON a, Request (r a)) => r a -> DictM a
restCall' req = (lift . lift . restCall) req >>= \case
  Left  err -> throwError $ Fuckup (show err)
  Right res -> return res

-- | Like `restCall'` but returns the unit.
restCall'_ :: (FromJSON a, Request (r a)) => r a -> DictM ()
restCall'_ = void . restCall'

getGuild :: DictM Guild
getGuild = restCall' $ GetGuild pnppcId

getMembers :: DictM [GuildMember]
getMembers =
  restCall' $ ListGuildMembers pnppcId $ GuildMembersTiming (Just 1000) Nothing

userToMember :: UserId -> DictM (Maybe GuildMember)
userToMember u = getMembers <&> find ((== u) . userId . memberUser)

userToMemberOr :: (Text -> Err) -> UserId -> DictM GuildMember
userToMemberOr f u =
  userToMember u >>= maybe (throwError $ f "join the server.") return

getChannelByID :: Snowflake -> DictM Channel
getChannelByID = restCall' . GetChannel

getChannelByMessage :: Message -> DictM Channel
getChannelByMessage = getChannelByID . messageChannel

getChannelNamed :: Text -> DictM (Maybe Channel)
getChannelNamed name = do
  channels <- restCall' $ GetGuildChannels pnppcId
  return . find ((== name) . channelName) $ channels

getGeneralChannel :: DictM Channel
getGeneralChannel =
  getChannelNamed "general"
    >>= maybe (throwError $ Fuckup "#general doesn't exist") return

getBotspamChannel :: DictM Channel
getBotspamChannel =
  getChannelNamed "botspam"
    >>= maybe (throwError $ Fuckup "#botspam doesn't exist") return

getLogChannel :: DictM Channel
getLogChannel =
  getChannelNamed "log"
    >>= maybe (throwError $ Fuckup "#log doesn't exist") return

sendUnfilteredMessage :: ChannelId -> Text -> DictM ()
sendUnfilteredMessage channel text = if T.null . stripped $ text
  then do
    msg <- restCall' $ CreateMessage channel (voiceFilter "empty message. >:(")
    threadDelay $ 5 * 1000000
    restCall'_ $ DeleteMessage (channel, messageId msg)
  else restCall'_ $ CreateMessage channel text
  where stripped = T.dropWhile isSpace . T.dropWhileEnd isSpace

sendMessage :: ChannelId -> Text -> DictM ()
sendMessage channel = sendUnfilteredMessage channel . voiceFilter

sendMessageToGeneral :: Text -> DictM ()
sendMessageToGeneral text =
  getGeneralChannel >>= flip sendMessage text . channelId

sendMessageToBotspam :: Text -> DictM ()
sendMessageToBotspam text =
  getBotspamChannel >>= flip sendMessage text . channelId

sendMessageToLogs :: Text -> DictM ()
sendMessageToLogs text = getLogChannel >>= flip sendMessage text . channelId

sendUnfilteredReply :: ChannelId -> MessageId -> Text -> DictM ()
sendUnfilteredReply channel message content =
  restCall'_ . CreateMessageDetailed channel $ def
    { messageDetailedContent   = content
    , messageDetailedReference = Just $ MessageReference (Just message)
                                                         Nothing
                                                         Nothing
                                                         False
    }

sendUnfilteredReplyTo :: Message -> Text -> DictM ()
sendUnfilteredReplyTo m = sendUnfilteredReply (messageChannel m) (messageId m)

sendReply :: ChannelId -> MessageId -> Text -> DictM ()
sendReply channel message content =
  sendUnfilteredReply channel message $ voiceFilter content

sendReplyTo :: Message -> Text -> DictM ()
sendReplyTo m = sendReply (messageChannel m) (messageId m)

sendReply' :: ChannelId -> MessageId -> Text -> CreateEmbed -> DictM ()
sendReply' channel message content embed =
  restCall'_ . CreateMessageDetailed channel $ def
    { messageDetailedContent   = voiceFilter content
    , messageDetailedEmbed     = Just embed
    , messageDetailedReference = Just $ MessageReference (Just message)
                                                         Nothing
                                                         Nothing
                                                         False
    }

sendReplyTo' :: Message -> Text -> CreateEmbed -> DictM ()
sendReplyTo' m = sendReply' (messageChannel m) (messageId m)

mkEmbed :: Text -> Text -> [(Text, Text)] -> Maybe ColorInteger -> CreateEmbed
mkEmbed title desc fields = CreateEmbed ""
                                        ""
                                        Nothing
                                        title
                                        ""
                                        Nothing
                                        desc
                                        (map toField fields)
                                        Nothing
                                        ""
                                        Nothing
 where
  toField (fieldTitle, fieldDesc) = EmbedField fieldTitle fieldDesc Nothing


-- {-# WARNING debugPutStr "please don't flood #general" #-}
debugPutStr :: Text -> ReaderT Env DH ()
debugPutStr t = ignoreErrors $ sendMessageToGeneral
  (fromString . ("```\n" <>) . (<> "\n```") . take 1900 . toString $ t)

-- {-# WARNING debugPrint "please don't flood #general"  #-}
debugPrint :: Show a => a -> ReaderT Env DH ()
debugPrint = debugPutStr . show

-- {-# WARNING debugDie "please don't flood #general"  #-}
debugDie :: Text -> ReaderT Env DH a
debugDie m = debugPutStr m >> die (toString m)

reactToMessage :: Text -> Message -> DictM ()
reactToMessage e m =
  restCall' $ CreateReaction (messageChannel m, messageId m) e

getRoleNamed :: Text -> DictM (Maybe Role)
getRoleNamed name = do
  roles <- restCall' $ GetGuildRoles pnppcId
  return . find ((== name) . roleName) $ roles

getRoleByID :: RoleId -> DictM (Maybe Role)
getRoleByID rId = do
  roles <- restCall' $ GetGuildRoles pnppcId
  return . find ((== rId) . roleId) $ roles

getEveryoneRole :: DictM Role
getEveryoneRole =
    -- Apparently the @ is needed. Why.
  getRoleNamed "@everyone"
    >>= maybe (throwError $ Fuckup "@everyone doesn't exist. wait, what?")
              return

setUserPermsInChannel :: Bool -> ChannelId -> UserId -> Integer -> DictM ()
setUserPermsInChannel allow channel user perms = do
  restCall'
    $ EditChannelPermissions channel (overwriteId permsId) permsOptsAllow
  return ()
 where
  permsId = Overwrite user "member" toAllow toDeny
  permsOptsAllow =
    ChannelPermissionsOpts toAllow toDeny ChannelPermissionsOptsUser

  toAllow = if allow then perms else 0
  toDeny  = if allow then 0 else perms


getEmojiNamed :: Text -> DictM (Maybe Emoji)
getEmojiNamed name = do
  emojis <- restCall' $ ListGuildEmojis pnppcId
  return $ find ((== name) . emojiName) emojis

displayCustomEmoji :: Emoji -> Text
displayCustomEmoji e =
  "<:" <> emojiName e <> ":" <> (show . fromMaybe 0 . emojiId) e <> ">"

randomMember :: DictM GuildMember
randomMember = do
  rng <- newStdGen
  if odds 0.75 rng then weightedRandomMember else trueRandomMember
 where
    -- Select from recent messages
  weightedRandomMember = do
    rng'     <- newStdGen
    general  <- getGeneralChannel
    messages <- restCall'
      $ GetChannelMessages (channelId general) (100, LatestMessages)
    let member = flip randomChoice rng' $ do
          sample <- messageAuthor <$> messages
          guard $ (not . userIsWebhook) sample
          return $ userId sample
    -- Fallback to uniform random
    userToMember member >>= maybe trueRandomMember return

  -- Select uniformly from member list
  trueRandomMember = do
    members <- getMembers
    randomChoice members <$> newStdGen

-- | Poll a message looking for reactions. Used for interactivity.
waitForReaction
  :: [Text] -> UserId -> Message -> (Text -> DictM ()) -> DictM ()
waitForReaction options user msg callback = do
  forConcurrently'_ options $ \opt -> restCall' $ CreateReaction messagePair opt
  -- We have to run in a different thread because we're waiting. We also have to poll a few times with a delay!
  void . lift . async . ignoreErrors $ doAttempts (2 :: Int) (5 :: Int)
 where
  messagePair = (messageChannel msg, messageId msg)

  doAttempts _            0 = return ()
  doAttempts secondsDelay n = do
    threadDelay $ secondsDelay * 1000000
    succeeded <- handleReactions options
    if succeeded then return () else doAttempts secondsDelay (pred n)

  handleReactions []            = return False
  handleReactions (option : xs) = do
    reactors <- restCall' $ GetReactions messagePair option (0, LatestReaction)
    if user `elem` fmap userId reactors
      then callback option >> return True
      else handleReactions xs

getAvatarData :: UserId -> Text -> DictM ByteString
getAvatarData userID hash = do
  response <- liftIO
    (  get
    $  "https://cdn.discordapp.com/avatars/"
    <> show userID
    <> "/"
    <> toString hash
    <> ".png"
    )
  pure . toStrict $ response ^. responseBody

encodeAvatarData :: ByteString -> Text
encodeAvatarData = ("data:image/jpeg;base64," <>) . encodeBase64

fromJustOr :: Err -> Maybe a -> DictM a
fromJustOr err = maybe (throwError err) return

fromLeftOr :: (b -> Err) -> Either a b -> DictM a
fromLeftOr handleErr = either return (throwError . handleErr)

fromRightOr :: (a -> Err) -> Either a b -> DictM b
fromRightOr handleErr = either (throwError . handleErr) return
