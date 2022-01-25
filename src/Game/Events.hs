-- | Defines game events.

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Events
    ( -- trinkets
      getExistingTrinket
    , getNewTrinket
    , previewNewTrinket
    , randomTrinket
    , getTrinketByName
      -- events
    , randomLocationEvent
    , trinketActs
    , userActs
      -- arena
    , runArenaFight
    , dictatorAddToArena
    ) where

import           Relude

import           Game
import           Game.Data
import           Game.Effects
import           Game.Items                     ( TrinketID )
import           Game.Utils
import           Points                         ( updateUserNickname )
import           Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

import           Control.Lens            hiding ( noneOf )
import           Control.Monad.Except
import qualified Data.MultiSet                 as MS
import qualified Data.Set                      as Set
import           Data.String.Interpolate
import qualified Data.Text                     as T
import           Discord.Requests
import           Discord.Types           hiding ( userName )
import           Relude.Unsafe
import           System.Random


logEvent :: CreateEmbed -> DictM ()
logEvent e = do
    log <- getLogChannel
    restCall'_ $ CreateMessageEmbed (channelId log) "" e


-- event sources
----------------

-- | Something happens in a location.
randomLocationEvent :: Text -> DictM ()
randomLocationEvent place = do
    p :: Double <- randomRIO (0.0, 1.0)
    trinkets    <- getLocationOr Fuckup place <&> view locationTrinkets
    let trinketSize = MS.size trinkets

    if
        | p < 0.2 && trinketSize >= 2 -> event2T trinkets trinketsBreed
        | p < 0.7 && trinketSize >= 1 -> do
            rng <- newStdGen
            let t = randomChoice (MS.elems trinkets) rng
            void $ trinketActs (Right place) t
        | trinketSize >= 2 -> event2T trinkets trinketsFight
        | otherwise -> return ()
  where
    event2T trinkets event = do
        (rng, rng') <- split <$> newStdGen
        let t1 = randomChoice (MS.elems trinkets) rng
            t2 = randomChoice (MS.elems . MS.delete t1 $ trinkets) rng'
        void $ event place t1 t2

dictatorAddToArena :: DictM ()
dictatorAddToArena = do
    -- Going to arbitrarily say that dictator can't have more than five combatants in the ring.
    fighters <- MS.elems . view globalArena <$> getGlobal
    let dictFighters = filter ((== dictId) . view fighterOwner) fighters
    when (length dictFighters < 5) $ do
        (trinketId, trinketData) <- randomTrinket
        let fighter = Fighter dictId trinketId
        void . modifyGlobal $ over globalArena (MS.insert fighter)

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
    :: Text -> TrinketID -> TrinketID -> DictM (TrinketID, TrinketData)
trinketsBreed place t1 t2 = do
    [td1, td2] <- mapConcurrently' (getTrinketOr Fuckup) [t1, t2]
    (newTrinketID, newTrinketData) <- combineTrinkets td1 td2
    void $ modifyLocation place (over locationTrinkets $ MS.insert newTrinketID)

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
trinketActs :: Either UserId Text -> TrinketID -> DictM (Text, [Action])
trinketActs place t = do
    trinket                    <- getTrinketOr Fuckup t
    (actionText, actionEffect) <- getTrinketAction trinket
    forM_ actionEffect $ \case
        Become name -> do
            (trinketID, _) <- getTrinketByName name $ trinket ^. trinketRarity
            modifyTrinkets (MS.delete t . MS.insert trinketID)

        Create name -> do
            rng <- newStdGen
            let adjustedRarity = (if odds 0.75 rng then pred' else id)
                    (trinket ^. trinketRarity)
            (trinketID, _) <- getTrinketByName name adjustedRarity
            modifyTrinkets (MS.insert trinketID)

        Nickname name -> case place of
            Left  userID -> renameUser userID name
            Right _      -> pure ()

        Credits n -> case place of
            Left  userID -> giveItems userID (fromCredits . toEnum $ n)
            Right _      -> pure ()

        SelfDestruct -> downgradeTrinket t trinket

        Consume      -> downgradeTrinket t trinket

        Ascend       -> case place of
            Left userID -> do
                void $ modifyUser userID (over userPoints succ)
                member <- getMembers <&> fromJust . find
                    ((== userID) . userId . memberUser)
                updateUserNickname member
            Right _ -> pure ()

        Descend -> case place of
            Left userID -> do
                void $ modifyUser userID (over userPoints pred)
                member <- getMembers <&> fromJust . find
                    ((== userID) . userId . memberUser)
                updateUserNickname member
            Right _ -> pure ()

        AddEffect name -> case place of
            Left userID -> do
                sendMessageToGeneral
                    [i|Due to mysterious forces, <@#{userID}> is now #{name}.|]
                void $ modifyUser userID (over userEffects $ Set.insert name)
            Right _ -> do
                member <- randomMember
                let memberID = userId . memberUser $ member
                sendMessageToGeneral
                    [i|Due to mysterious forces, <@#{memberID}> is now #{name}.|]
                void $ modifyUser memberID (over userEffects $ Set.insert name)

    displayedTrinket <- displayTrinket t trinket
    let logDesc =
            displayedTrinket
                <> " "
                <> actionText
                <> " in "
                <> displayPlace
                <> "."
    logEvent $ mkEmbed "A trinket acts!"
                       logDesc
                       []
                       (Just $ trinketColour (trinket ^. trinketRarity))
    return (actionText, actionEffect)
  where
    pred' rarity = if rarity == Common then Common else pred rarity
    displayPlace = either ((<> ">'s inventory") . ("<@" <>) . show) id place

    modifyTrinkets f = case place of
        Left  u -> void . modifyUser u $ over userTrinkets f
        Right l -> void . modifyLocation l $ over locationTrinkets f

    -- Remove a trinket if it's common, otherwise just downgrade it
    downgradeTrinket tId tDat = case tDat ^. trinketRarity of
        Common -> void $ modifyTrinkets (MS.delete tId)
        _      -> do
            (tId', _) <- getTrinketByName (tDat ^. trinketName)
                                          (pred $ tDat ^. trinketRarity)
            modifyTrinkets $ MS.delete tId . MS.insert tId'

-- | A trinket fights another and either kills the other or dies itself.
trinketsFight :: Text -> TrinketID -> TrinketID -> DictM ()
trinketsFight place attacker defender = do
    [attackerData, defenderData] <- forM [attacker, defender]
                                         (getTrinketOr Fuckup)
    fightData <- fightTrinkets attackerData defenderData Nothing
    let FightData attackerWins _ = fightData
    embed <- fightEmbed (attacker, attackerData)
                        (defender, defenderData)
                        fightData
    let removed = if attackerWins then defender else attacker
    void $ modifyLocation place (over locationTrinkets $ MS.delete removed)
    logEvent embed
        { createEmbedDescription = createEmbedDescription embed
                                   <> " (location: "
                                   <> place
                                   <> ")"
        }


-- ???
------

userActs :: UserId -> DictM (Text, [Action])
userActs userID = do
    name <- getUser userID <&> view userName
    getAction (unUsername name)


-- trinkets
-----------

createTrinket :: TrinketData -> DictM (TrinketID, TrinketData)
createTrinket trinket = do
    tID <- nextTrinketId
    setTrinket tID trinket
    return (tID, trinket)

getExistingTrinket :: Rarity -> DictM (TrinketID, TrinketData)
getExistingTrinket rarity = do
    maxTrinketID <- nextTrinketId <&> pred
    trinketID    <- randomRIO (1, maxTrinketID)
    getTrinket trinketID >>= \case
        Just trinket | (trinket ^. trinketRarity) == rarity ->
            return (trinketID, trinket)
        _ -> getExistingTrinket rarity

-- | Not only retrieves a new trinket, but adds it to the database.
getNewTrinket :: Rarity -> DictM (TrinketID, TrinketData)
getNewTrinket rarity = previewNewTrinket rarity >>= createTrinket

-- | Does not add trinkets to the database. You might want to use getNewTrinket instead!
previewNewTrinket :: Rarity -> DictM TrinketData
previewNewTrinket rarity = do
    res <- getJ1 20 prompt
    case listToMaybe . rights . fmap parseTrinketName . lines $ res of
        Just name -> do
            valid <- validTrinketName name
            if valid
                then return $ TrinketData name rarity
                else previewNewTrinket rarity
        Nothing -> previewNewTrinket rarity
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
getTrinketByName :: Text -> Rarity -> DictM (TrinketID, TrinketData)
getTrinketByName name rarity = lookupTrinketName name >>= \case
    Nothing           -> createTrinket (TrinketData name rarity)
    Just (id_, data_) -> pure (id_, data_)

randomTrinket :: DictM (TrinketID, TrinketData)
randomTrinket = do
    rng <- newStdGen
    if odds 0.2 rng
        then do
            rarity <- randomNewTrinketRarity
            getNewTrinket rarity
        else do
            rarity <- randomExistingTrinketRarity
            getExistingTrinket rarity


-- arena
--------

-- | Do an arena fight and post the results. Return value indicates if a valid fight could actually take place.
runArenaFight :: DictM Bool
runArenaFight = do
    fighters     <- MS.elems . view globalArena <$> getGlobal
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
                $ getTrinketOr Fuckup
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
                (winner ^. fighterOwner)
                (fromCredits $ trinketRewards (winnerData ^. trinketRarity))
            void $ modifyGlobal $ over globalArena (MS.delete loser)

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
