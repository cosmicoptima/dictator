-- | Defines game events that should (will!) be logged.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Events where

import           Relude

import           Game
import           Game.Data
import           Utils.Discord

import           Control.Lens
import           Control.Monad.Except
import qualified Data.MultiSet                 as MS
import qualified Database.Redis                as DB


-- | An item spawns in a location for no important reason.
trinketSpawns :: DB.Connection -> Text -> TrinketID -> DictM ()
trinketSpawns conn location trinket = void $ modifyLocation
    conn
    location
    (over locationTrinkets $ MS.insert trinket)

-- | A trinket does something.
trinketActs :: DB.Connection -> TrinketID -> DictM Text
trinketActs conn =
    getTrinket conn
        >=> maybe
                (throwError $ Fuckup
                    "This trinket can't act because it doesn't exist."
                )
                getTrinketAction
