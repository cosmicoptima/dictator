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

import           Discord                        ( DiscordHandler
                                                , FromJSON
                                                , restCall
                                                )
import           Discord.Internal.Rest.Prelude  ( Request )
import           Discord.Requests
import           Discord.Types

import qualified Data.Text                     as T
import           Text.Parsec                    ( ParseError )
import           Utils
import Control.Monad.Except (MonadError(throwError))

type DH = DiscordHandler -- `DiscordHandler` is an ugly name!

-- | Global error type
data Err =
    -- | Abort from a command and post a regular message in chat in case of user error.
    Complaint Text
    -- | Abort from a command and post a debug message in case of programmer error.
    | Fuckup Text
    -- | Abort when a command fails to parse.
    | Gibberish ParseError
    -- | Just abort from a command, and silently fail.
    | GTFO
     deriving (Show, Eq)

-- | Global monad transformer stack
type DictM a = ExceptT Err DH a

getParsed :: Either ParseError a -> DictM a
getParsed = hoistEither . first Gibberish

ignoreErrors :: DictM () -> DH ()
ignoreErrors m = void $ runExceptT m

logErrors :: DictM a -> DH ()
logErrors m = runExceptT m >>= \case
    Left  err -> debugPrint err
    Right _   -> return ()

dieOnErrors :: DictM a -> DH a
dieOnErrors m = runExceptT m >>= \case
    Left  err -> debugPrint err >> (die . show) err
    Right a   -> return a

pnppcId :: GuildId
pnppcId = 878376227428245555

dictId :: UserId
dictId = 878385073735467060

ownedEmoji :: Text
ownedEmoji = "owned:899536714773717012"

isDict :: User -> Bool
isDict = (== dictId) . userId


-- | like `restCall`, but simply crashes if there is an error
restCall' :: (FromJSON a, Request (r a)) => r a -> DictM a
restCall' req = lift $ (restCall >=> either (debugDie . show) return) req

getGuild :: DictM Guild
getGuild = restCall' $ GetGuild pnppcId

getMembers :: DictM [GuildMember]
getMembers =
    restCall' $ ListGuildMembers pnppcId $ GuildMembersTiming (Just 100) Nothing

userToMember :: User -> DictM (Maybe GuildMember)
userToMember u = getMembers <&> find ((== u) . memberUser)

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
    getChannelNamed "general" >>= maybe (throwError $ Fuckup "#general doesn't exist") return

getLogChannel :: DictM Channel
getLogChannel =
    getChannelNamed "log" >>= maybe (throwError $ Fuckup "#log doesn't exist") return

sendUnfilteredMessage :: ChannelId -> Text -> DictM ()
sendUnfilteredMessage channel text = if T.null text
    then void . print $ "Sent empty message: " ++ toString text
    else void . restCall' $ CreateMessage channel text

sendMessage :: ChannelId -> Text -> DictM ()
sendMessage channel = sendUnfilteredMessage channel . voiceFilter

sendMessageToGeneral :: Text -> DictM ()
sendMessageToGeneral text =
    getGeneralChannel >>= flip sendMessage text . channelId

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
debugPutStr :: Text -> DH ()
debugPutStr t = ignoreErrors $ sendMessageToGeneral
    (fromString . ("```\n" <>) . (<> "\n```") . take 1900 . toString $ t)

-- {-# WARNING debugPrint "please don't flood #general"  #-}
debugPrint :: Show a => a -> DH ()
debugPrint = debugPutStr . show

-- {-# WARNING debugDie "please don't flood #general"  #-}
debugDie :: Text -> DH a
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
        >>= maybe (throwError $ Fuckup "@everyone doesn't exist. wait, what?") return

setUserPermsInChannel :: Bool -> ChannelId -> UserId -> Integer -> DictM ()
setUserPermsInChannel allow channel user perms = do
    restCall' $ EditChannelPermissions channel
                                       (overwriteId permsId)
                                       permsOptsAllow
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
