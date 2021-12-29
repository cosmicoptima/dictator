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
import           Discord.Requests
import           Discord.Types


logEvent :: CreateEmbed -> DictM ()
logEvent e = do
    log <- getLogChannel
    void . restCall' $ CreateMessageEmbed (channelId log) "" e


-- events
---------

-- | An item spawns in a location for no important reason.
trinketSpawns :: DB.Connection -> Text -> TrinketID -> DictM ()
trinketSpawns conn location trinket = do
    trinketData <-
        getTrinket conn trinket
            >>= maybe
                    (throwError $ Fuckup
                        "This item can't spawn because it doesn't exist."
                    )
                    return
    void $ modifyLocation conn
                          location
                          (over locationTrinkets $ MS.insert trinket)
    let logDesc =
            "**"
                <> displayTrinket trinket trinketData
                <> "** spawns in "
                <> location
                <> "."
    logEvent $ mkEmbed "Trinket spawns" logDesc [] Nothing

-- | A trinket does something.
trinketActs :: DB.Connection -> TrinketID -> DictM Text
trinketActs conn t = do
    trinket <-
        getTrinket conn t
            >>= maybe
                    (throwError $ Fuckup
                        "This trinket can't act because it doesn't exist."
                    )
                    return
    action <- getTrinketAction trinket
    let logDesc = "**" <> displayTrinket t trinket <> "** " <> action <> "."
    logEvent $ mkEmbed "Trinket acts" logDesc [] Nothing
    return action
