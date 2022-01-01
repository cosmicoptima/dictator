-- | Contains the DictM monad transformer stack and associated code.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.DictM
    ( DH
    , Err(..)
    , DictM
    , getParsed
    , ignoreErrors
    ) where

import           Relude

import           Discord                        ( DiscordHandler )
import           Text.Parsec                    ( ParseError )


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
