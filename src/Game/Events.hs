-- | Defines game events that should (will!) be logged.

{-# LANGUAGE NoImplicitPrelude #-}

module Game.Events where

import           Relude

import           Game.Data
import           Utils.Discord

import           Control.Lens
import qualified Data.MultiSet                 as MS
import qualified Database.Redis                as DB


-- | An item spawns in a location for no important reason.
spawnItem :: DB.Connection -> Text -> TrinketID -> DictM ()
spawnItem conn location trinket = void $ modifyLocation
    conn
    location
    (over locationTrinkets $ MS.insert trinket)
