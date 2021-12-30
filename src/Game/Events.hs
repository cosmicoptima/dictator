-- | Defines game events.

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Events where

import           Relude

import           Game
import           Game.Data
import           Utils
import           Utils.Discord

import           Control.Lens
import           Control.Monad.Except
import qualified Data.MultiSet                 as MS
import qualified Database.Redis                as DB
import           Discord.Requests
import           Discord.Types
import           System.Random


logEvent :: CreateEmbed -> DictM ()
logEvent e = do
    log <- getLogChannel
    void . restCall' $ CreateMessageEmbed (channelId log) "" e


-- event sources
----------------

-- | Something happens in a location.
randomLocationEvent :: DB.Connection -> Text -> DictM ()
randomLocationEvent conn place = do
    p :: Float <- randomRIO (0.0, 1.0)
    if
        | 0.00 <= p && p < 0.60 -> doEvent trinketsBreed
        | 0.20 <= p && p < 0.80 -> doEvent trinketsFight
        | 0.80 <= p && p < 0.90 -> do
            rarity <- randomNewTrinketRarity
            mkNewTrinket conn rarity >>= trinketSpawns conn place . fst
        | otherwise -> do
            rarity <- randomExistingTrinketRarity
            getRandomTrinket conn rarity >>= trinketSpawns conn place . fst
  where
    doEvent event = do
        (rng, rng') <- split <$> newStdGen
        location    <- getLocationOr Fuckup conn place
        let inLocation = location ^. locationTrinkets
            t1         = randomChoice (MS.elems inLocation) rng
            t2         = randomChoice (MS.elems inLocation) rng'
        void $ event conn place t1 t2
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
    [td1         , td2           ] <- mapM (getTrinketOr Fuckup conn) [t1, t2]
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

    let rarity1   = td1 ^. trinketRarity
        rarity2   = td2 ^. trinketRarity
        maxRarity = max rarity1 rarity2
    logEvent $ mkEmbed "Two trinkets having sex!"
                       embedDesc
                       []
                       (Just $ trinketColour maxRarity)
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
    logEvent $ mkEmbed "Trinket spawns"
                       logDesc
                       []
                       (Just $ trinketColour (trinketData ^. trinketRarity))

-- | A trinket does something.
trinketActs :: DB.Connection -> TrinketID -> DictM Text
trinketActs conn t = do
    trinket          <- getTrinketOr Fuckup conn t
    action           <- getTrinketAction trinket
    displayedTrinket <- displayTrinket t trinket
    let logDesc = displayedTrinket <> " " <> action <> "."
    logEvent $ mkEmbed "A trinket acts!"
                       logDesc
                       []
                       (Just $ trinketColour (trinket ^. trinketRarity))
    return action

-- | A trinket fights another and either kills the other or dies itself.
trinketsFight :: DB.Connection -> Text -> TrinketID -> TrinketID -> DictM ()
trinketsFight conn place attacker defender = do
    [attackerData, defenderData] <- forM [attacker, defender]
                                         (getTrinketOr Fuckup conn)
    fightData <- fightTrinkets attackerData defenderData Nothing
    let FightData attackerWins _ = fightData
    embed <- fightEmbed (attacker, attackerData)
                        (defender, defenderData)
                        fightData
    let removed = if attackerWins then defender else attacker
    void $ modifyLocation conn place (over locationTrinkets $ MS.delete removed)
    logEvent embed

