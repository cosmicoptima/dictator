-- | Defines game events.

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Game.Events
    ( -- trinkets
      getExistingTrinket
    , getNewTrinket
    , previewNewTrinket
    , randomTrinket
      -- events
    , randomLocationEvent
    , trinketActs
      -- arena
    , runArenaFight
    , dictatorAddToArena
    ) where

import           Relude

import           Game
import           Game.Data
import           Game.Utils
import           Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

import           Control.Lens            hiding ( noneOf )
import           Control.Monad.Except
import qualified Data.MultiSet                 as MS
import qualified Data.Text                     as T
import qualified Database.Redis                as DB
import           Discord.Requests
import           Discord.Types
import           System.Random


logEvent :: CreateEmbed -> DictM ()
logEvent e = do
    log <- getLogChannel
    restCall'_ $ CreateMessageEmbed (channelId log) "" e


-- event sources
----------------

-- | Something happens in a location.
randomLocationEvent :: DB.Connection -> Text -> DictM ()
randomLocationEvent conn place = do
    p :: Double <- randomRIO (0.0, 1.0)
    trinkets    <- getLocationOr Fuckup conn place <&> view locationTrinkets
    let trinketSize = MS.size trinkets

    if
        | p < 0.2 && trinketSize >= 2 -> event2T trinkets trinketsBreed
        | p < 0.7 && trinketSize >= 1 -> do
            rng <- newStdGen
            let t = randomChoice (MS.elems trinkets) rng
            void $ trinketActs conn (Right place) t
        | trinketSize >= 2 -> event2T trinkets trinketsFight
        | otherwise -> return ()
  where
    event2T trinkets event = do
        (rng, rng') <- split <$> newStdGen
        let t1 = randomChoice (MS.elems trinkets) rng
            t2 = randomChoice (MS.elems . MS.delete t1 $ trinkets) rng'
        void $ event conn place t1 t2

dictatorAddToArena :: DB.Connection -> DictM ()
dictatorAddToArena conn = do
    -- Going to arbitrarily say that dictator can't have more than five combatants in the ring.
    fighters <- MS.elems . view globalArena <$> getGlobal conn
    let dictFighters = filter ((== dictId) . view fighterOwner) fighters
    when (length dictFighters < 5) $ do
        (trinketId, trinketData) <- randomTrinket conn
        let fighter = Fighter dictId trinketId
        void . modifyGlobal conn $ over globalArena (MS.insert fighter)

        -- Post in the arena channel informing users that dictator has put someone in to fight.
        trinketDisplay <- displayTrinket trinketId trinketData
        channel        <- getChannelNamed "arena"
            >>= maybe (throwError $ Fuckup "no arena") (return . channelId)
        sendUnfilteredMessage channel
            $  voiceFilter "As punishment for its crimes,"
            <> " "
            <> trinketDisplay
            <> " "
            <> voiceFilter "must fight to the death."


-- locations
------------

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
                <> "."

    let rarity1   = td1 ^. trinketRarity
        rarity2   = td2 ^. trinketRarity
        maxRarity = max rarity1 rarity2
    logEvent $ mkEmbed "Two trinkets having sex!"
                       embedDesc
                       []
                       (Just $ trinketColour maxRarity)
    return (newTrinketID, newTrinketData)

-- | A trinket does something.
trinketActs :: DB.Connection -> Either UserId Text -> TrinketID -> DictM Text
trinketActs conn place t = do
    trinket                    <- getTrinketOr Fuckup conn t
    -- TODO:
    --   destruction
    (actionText, actionEffect) <- getTrinketAction trinket
    case actionEffect of
        Just (Become name) -> do
            rarity         <- randomNewTrinketRarity
            (trinketID, _) <- getTrinketByName conn name rarity
            adjustTrinkets (MS.delete t . MS.insert trinketID)
        Just (Create name) -> do
            rarity         <- randomNewTrinketRarity
            (trinketID, _) <- getTrinketByName conn name rarity
            adjustTrinkets (MS.insert trinketID)
        _ -> pure ()
    displayedTrinket <- displayTrinket t trinket
    let logDesc =
            displayedTrinket
                <> " "
                <> actionText
                <> " in "
                <> displayPlace
                <> "."
                <> displayEffect actionEffect
    logEvent $ mkEmbed "A trinket acts!"
                       logDesc
                       []
                       (Just $ trinketColour (trinket ^. trinketRarity))
    return actionText
  where
    displayEffect = \case
        Just (Become t') -> " (becomes " <> t' <> ")"
        Just (Create t') -> " (creates " <> t' <> ")"
        Just Destroy     -> " (destroys)"
        Nothing          -> ""

    displayPlace = either ((<> ">'s inventory") . ("<@" <>) . show) id place

    adjustTrinkets f = case place of
        Left  u -> void . modifyUser conn u $ over userTrinkets f
        Right l -> void . modifyLocation conn l $ over locationTrinkets f

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
        { createEmbedDescription = createEmbedDescription embed
                                   <> " (location: "
                                   <> place
                                   <> ")"
        }


-- trinkets
-----------

createTrinket :: DB.Connection -> TrinketData -> DictM (TrinketID, TrinketData)
createTrinket conn trinket = do
    tID <- nextTrinketId conn
    setTrinket conn tID trinket
    return (tID, trinket)

getExistingTrinket :: DB.Connection -> Rarity -> DictM (TrinketID, TrinketData)
getExistingTrinket conn rarity = do
    maxTrinketID <- nextTrinketId conn <&> pred
    trinketID    <- randomRIO (1, maxTrinketID)
    getTrinket conn trinketID >>= \case
        Just trinket | (trinket ^. trinketRarity) == rarity ->
            return (trinketID, trinket)
        _ -> getExistingTrinket conn rarity

-- | Not only retrieves a new trinket, but adds it to the database.
getNewTrinket :: DB.Connection -> Rarity -> DictM (TrinketID, TrinketData)
getNewTrinket conn rarity =
    previewNewTrinket conn rarity >>= createTrinket conn

-- | Does not add trinkets to the database. You might want to use getNewTrinket instead!
previewNewTrinket :: DB.Connection -> Rarity -> DictM TrinketData
previewNewTrinket conn rarity = do
    res <- getJ1 20 prompt
    case listToMaybe . rights . fmap parseTrinketName . lines $ res of
        Just name -> do
            valid <- validTrinketName conn name
            if valid
                then return $ TrinketData name rarity
                else previewNewTrinket conn rarity
        Nothing -> previewNewTrinket conn rarity
  where
    promptTrinkets = makePrompt . map (<> ".") $ case rarity of
        Common    -> commonTrinketExamples
        Uncommon  -> uncommonTrinketExamples
        Rare      -> rareTrinketExamples
        Legendary -> legendaryTrinketExamples
    prompt =
        "There exists a dictator of an online chatroom who is eccentric but evil. He often gives out items. Here are some examples of "
            <> show rarity
            <> " items.\n"
            <> promptTrinkets

-- TODO use this for things like combine
getTrinketByName
    :: DB.Connection -> Text -> Rarity -> DictM (TrinketID, TrinketData)
getTrinketByName conn name rarity = lookupTrinketName conn name >>= \case
    Nothing           -> createTrinket conn (TrinketData name rarity)
    Just (id_, data_) -> pure (id_, data_)

randomTrinket :: DB.Connection -> DictM (TrinketID, TrinketData)
randomTrinket conn = do
    rng <- newStdGen
    if odds 0.5 rng
        then do
            rarity <- randomNewTrinketRarity
            getNewTrinket conn rarity
        else do
            rarity <- randomExistingTrinketRarity
            getExistingTrinket conn rarity


-- arena
--------

-- | Do an arena fight and post the results. Return value indicates if a valid fight could actually take place.
runArenaFight :: DB.Connection -> DictM Bool
runArenaFight conn = do
    fighters     <- MS.elems . view globalArena <$> getGlobal conn
    (rng1, rng2) <- split <$> newStdGen
    -- Do not have users own trinkets fight against each other
    let attacker = randomChoiceMay fighters rng1
        defender = attacker <&> \a -> randomChoiceMay
            [ f | f <- fighters, f ^. fighterOwner /= a ^. fighterOwner ]
            rng2
    -- This is awful 10am code. Celeste, please tell me there's a nicer way to do this...
    -- btw if you're reading the commits hi. post something nonesense in the chat and i'll pretend to understand it
    -- (if i am awake)
    -- we should totally use this as a private chatroom since nobody else will read it
    -- also also i really need to sleep huh i measn look at all of this

    -- LMFAO
    -- if only i had been able to post nonsense in the chat at that time
    -- als oyes we can scheme here

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
            let desc = T.unwords
                    [ "<@!" <> show (winner ^. fighterOwner) <> ">"
                    , voiceFilter ", congratulations!"
                    , "<@!" <> show (loser ^. fighterOwner) <> ">"
                    , voiceFilter ", I'm sorry for your loss."
                    ]
            embed <- fightEmbed (attacker' ^. fighterTrinket, attackerData)
                                (defender' ^. fighterTrinket, defenderData)
                                fightData
            arenaChannel <- getChannelNamed "arena" >>= \case
                Just arena -> return $ channelId arena
                Nothing    -> throwError $ Fuckup "arena channel doesn't exist!"
            restCall'_ $ CreateMessageEmbed arenaChannel desc embed
            return True
        _ -> return False
