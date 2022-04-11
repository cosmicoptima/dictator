-- | Contains the DictM monad transformer stack and associated code.

module Utils.DictM
  ( DH
  , TwitterAuth(..)
  , Env(..)
  , Err(..)
  , DictM
  , getParsed
  , ignoreErrors
  , ignoreErrors'
  ) where

import           Relude

import qualified Database.Redis                as DB
import           Discord                        ( DiscordHandler )
import           Network.Wreq.Session
import           Text.Parsec                    ( ParseError )
import Discord.Internal.Types.Prelude


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

-- | Everything neccesary to post on twitter and nothing more.
data TwitterAuth = TwitterAuth
  { twAuthApiKey      :: ByteString
  , twAuthApiSecret   :: ByteString
  , twAuthUserToken   :: ByteString
  , twAuthTokenSecret :: ByteString
  }

-- | Global environment type
data Env = Env
  { envDb :: DB.Connection
  , envTw :: TwitterAuth
  , envSs :: Session
  }

-- | Global monad transformer stack
type DictM = ExceptT Err (ReaderT Env DH)

getParsed :: Either ParseError a -> DictM a
getParsed = hoistEither . first Gibberish

ignoreErrors' :: Env -> DictM () -> DH ()
ignoreErrors' conn m = runReaderT (void $ runExceptT m) conn

ignoreErrors :: DictM () -> ReaderT Env DH ()
ignoreErrors m = void $ runExceptT m

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