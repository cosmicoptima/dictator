-- | Defines game events.

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Game.Events where

import           Relude

import           Game
import           Game.Data
import           Utils
import           Utils.DictM
import           Utils.Discord

import           Control.Lens            hiding ( noneOf )
import           Control.Monad.Except
import qualified Data.MultiSet                 as MS
import qualified Database.Redis                as DB
import           Discord.Requests
import           Discord.Types
import           System.Random
import           Text.Parsec
import           Utils.Language


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
        | p < 0.40 -> event2T trinketsBreed
        | p < 0.60 -> event1T trinketCreates
        | p < 0.80 -> event2T trinketsFight
        | p < 0.90 -> do
            rarity <- randomNewTrinketRarity
            mkNewTrinket conn rarity >>= trinketSpawns conn place . fst
        | otherwise -> do
            rarity <- randomExistingTrinketRarity
            getRandomTrinket conn rarity >>= trinketSpawns conn place . fst
  where
    event1T event = do
        rng      <- newStdGen
        location <- getLocationOr Fuckup conn place
        let inLocation = location ^. locationTrinkets
            t          = randomChoice (MS.elems inLocation) rng
        void $ event conn place t
    event2T event = do
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
    [td1, td2] <- mapConcurrently' (getTrinketOr Fuckup conn) [t1, t2]
    (newTrinketID, newTrinketData) <- combineTrinkets conn td1 td2
    void $ modifyLocation conn
                          place
                          (over locationTrinkets $ MS.insert newTrinketID)

    [dt1, dt2, newDT] <- mapConcurrently'
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

-- | A trinket creates another trinket.
trinketCreates :: DB.Connection -> Text -> TrinketID -> DictM ()
trinketCreates conn place trinket = do
    trinketData <- getTrinketOr Fuckup conn trinket
    newName     <- getJ1 16 (prompt $ trinketData ^. trinketName)
        <&> parse (some $ noneOf ".") ""
    case newName of
        Left  _    -> trinketCreates conn place trinket
        Right name -> do
            newID <- nextTrinketId conn
            let newData = TrinketData (fromString name)
                                      (trinketData ^. trinketRarity)
            setTrinket conn newID newData
            void $ modifyLocation conn
                                  place
                                  (over locationTrinkets $ MS.insert newID)

            [odt, ndt] <- mapConcurrently'
                (uncurry displayTrinket)
                [(trinket, trinketData), (newID, newData)]
            let embedDesc = odt <> " creates " <> ndt <> " in " <> place <> "."
            logEvent $ mkEmbed
                "New trinket!"
                embedDesc
                []
                (Just . trinketColour . view trinketRarity $ newData)
  where
    prompt name =
        makePrompt
                [ "Item: a hen. Creates: a large egg."
                , "Item: a gun. Creates: gunshots."
                , "Item: a grinning skull. Creates: a sense of horror."
                , "Item: a creator of black holes. Creates: black holes."
                , "Item: a kazoo crocodile. Creates: bangers."
                ]
            <> " Item: "
            <> name
            <> ". Creates:"

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

-- Arena

-- | Do an arena fight and post the results. Return value indicates if a valid fight could actually take place.
runArenaFight :: DB.Connection -> DictM Bool
runArenaFight conn = do
    fighters     <- toList . view globalArena <$> getGlobal conn
    sendMessageToGeneral $ show fighters
    (rng1, rng2) <- split <$> newStdGen
    let attacker = randomChoiceMay fighters rng1
        defender = attacker <&> \a -> randomChoiceMay
-- Do not have users own trinkets fight against each other
            [ f | f <- fighters, f ^. fighterOwner /= a ^. fighterOwner ]
            rng2
    -- This is awful 10am code. Celeste, please tell me there's a nicer way to do this...
    -- btw if you're reading the commits hi. post something nonesense in the chat and i'll pretend to understand it
    -- (if i am awake)
    -- we should totally use this as a private chatroom since nobody else will read it
    -- also also i really need to sleep huh i measn look at all of this
    sendMessageToGeneral $ show (attacker, defender)
    case (attacker, join defender) of
        (Just attacker', Just defender') -> do
            -- Shuffle some data
            [attackerData, defenderData] <-
                forM [attacker', defender']
                $ getTrinketOr Fuckup conn
                . view fighterTrinket
            fightData <- fightTrinkets attackerData defenderData Nothing
            let FightData attackerWins _ = fightData
                (winner, loser)          = if attackerWins
                    then (attacker', defender')
                    else (defender', attacker')
                (winnerData, _loserData) = if attackerWins
                    then (attackerData, defenderData)
                    else (defenderData, attackerData)
            -- Apply results. The winner gets credits and the loser's item is lost forever. :owned:
            giveItems
                conn
                (winner ^. fighterOwner)
                (fromCredits $ trinketRewards (winnerData ^. trinketRarity))
            void $ modifyGlobal conn $ over globalArena (MS.delete loser)

            -- Send output. We also mention the combatants.
            let desc =
                    "<@!"
                        <> show (winner ^. fighterOwner)
                        <> ">, congratulations! <@!"
                        <> show (loser ^. fighterOwner)
                        <> ">, I'm sorry for your loss."
            embed <- fightEmbed (attacker' ^. fighterTrinket, attackerData)
                                (defender' ^. fighterTrinket, defenderData)
                                fightData
            arenaChannel <- getChannelNamed "arena" >>= \case
                Just arena -> return $ channelId arena
                Nothing    -> throwError $ Fuckup "arena channel doesn't exist!"
            void . restCall' $ CreateMessageEmbed arenaChannel desc embed
            return True
        _ -> return False
