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

-- | Two trinkets breed, combining without eliminating the originals.
trinketsBreed
    :: DB.Connection
    -> Text
    -> TrinketID
    -> TrinketID
    -> DictM (TrinketID, TrinketData)
trinketsBreed conn place t1 t2 = do
    [td1, td2] <- mapM
        (   getTrinket conn
        >=> maybe
                (throwError $ Fuckup
                    "These two items can't breed; one doesn't exist."
                )
                return
        )
        [t1, t2]
    (newTrinketID, newTrinketData) <- combineTrinkets conn td1 td2
    void $ modifyLocation conn
                          place
                          (over locationTrinkets $ MS.insert newTrinketID)

    [dt1, dt2, newDT] <- mapM
        (uncurry displayTrinket)
        [(t1, td1), (t2, td2), (newTrinketID, newTrinketData)]
    let embedDesc =
            dt1
                <> " and "
                <> dt2
                <> " breed to create "
                <> newDT
                <> " in "
                <> place
                <> ""

    logEvent $ mkEmbed "Trinket sex" embedDesc [] Nothing
    return (newTrinketID, newTrinketData)

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
    displayedTrinket <- displayTrinket trinket trinketData

    let logDesc = displayedTrinket <> " spawns in " <> location <> "."
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
    action           <- getTrinketAction trinket
    displayedTrinket <- displayTrinket t trinket
    let logDesc = displayedTrinket <> " " <> action <> "."
    logEvent $ mkEmbed "Trinket acts" logDesc [] Nothing
    return action
