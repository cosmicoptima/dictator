-- | Contains the DictM monad transformer stack and associated code.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.DictM
    ( DH
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
import           Text.Parsec                    ( ParseError )
import           Web.Twitter.Conduit


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

-- | Global environment type
data Env = Env
    { envDb        :: DB.Connection
    , envTwManager :: Manager
    , envTwInfo    :: TWInfo
    }

-- | Global monad transformer stack
type DictM = ExceptT Err (ReaderT Env DH)

getParsed :: Either ParseError a -> DictM a
getParsed = hoistEither . first Gibberish

ignoreErrors' :: Env -> DictM () -> DH ()
ignoreErrors' conn m = runReaderT (void $ runExceptT m) conn

ignoreErrors :: DictM () -> ReaderT Env DH ()
ignoreErrors m = void $ runExceptT m
