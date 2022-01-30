{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Status effects!

module Game.Effects where

import           Relude
import qualified Relude.Unsafe                 as Unsafe

import           Game.Data
import           Utils
import           Utils.DictM
import           Utils.Discord

import           Control.Lens
import           Data.Default
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.String.Interpolate
import           Discord.Requests
import           Discord.Types           hiding ( userName )
import           System.Random


data StatusEffect = StatusEffect
    { effectName   :: Text
    , avgLength    :: Int
    , inflictPrice :: Int
    , everyMessage :: Message -> DictM ()
    , everySecond  :: UserId -> DictM ()
    , onModifyUser :: UserId -> UserData -> UserData -> DictM UserData
    }

seconds, minutes, hours :: Int -> Int
seconds = id
minutes = (* 60)
hours = (* 3600)

instance Default StatusEffect where
    def = StatusEffect { effectName   = "owned"
                       , avgLength    = minutes 15
                       , inflictPrice = 0
                       , everyMessage = const $ pure ()
                       , everySecond  = const $ pure ()
                       , onModifyUser = \_ _ -> pure
                       }

statusEffects :: [StatusEffect]
statusEffects =
    [ def
        { effectName   = "silenced"
        , avgLength    = seconds 40
        , inflictPrice = 20
        , everyMessage = \m ->
            restCall' $ DeleteMessage (messageChannel m, messageId m)
        }
    , def
        { effectName   = "taxed"
        , avgLength    = minutes 20
        , inflictPrice = 100
        , everySecond  = \member ->
            void $ modifyUser member (over userCredits (* 0.9996))
        }
    , def
        { effectName   = "known"
        , avgLength    = minutes 10
        , inflictPrice = 25
        , onModifyUser = \userID in_ out -> do
            if in_ ^. userName == out ^. userName
                then pure out
                else do
                    sendMessageToLogs
                        [i|<@#{userID}> tries to change their username, but they are known.|]
                    pure in_
        }
    , def
        { effectName   = "frozen"
        , avgLength    = minutes 2
        , inflictPrice = 75
        , onModifyUser = \userID inData outData -> do
            if userToItems inData == userToItems outData then do
                sendMessageToLogs
                    [i|<@#{userID} tries to change their inventory, but it is frozen.|]
                pure outData
            else pure outData
        }
    ]

runEffects :: DictM ()
runEffects = do
    ailedMembers <- Map.assocs . view globalEffects <$> getGlobal
    forM_ ailedMembers $ \(userID, effects) -> do
        forM_ (Set.elems effects) $ \effName -> do
            let mayEffect = findEffect effName
            case mayEffect of
                Just eff -> do
                    everySecond eff userID
                    let p = 1 / (fromIntegral . avgLength $ eff :: Double)
                    n <- randomIO
                    when (n < p) $ do
                        void
                            . modifyUser userID
                            . over userEffects
                            . Set.delete
                            $ effName

                        sendMessageToLogs
                            [i|Rejoice, for I am magnanimous! <@#{userID}> is no longer #{effectName eff}.|]

                -- Remove invalid effects
                Nothing ->
                    void
                        . modifyUser userID
                        . over userEffects
                        . Set.delete
                        $ effName

modifyUser :: UserId -> (UserData -> UserData) -> DictM UserData
modifyUser userID f = do
    activeEffects <-
        map getEffect . Set.elems . view userEffects <$> getUser userID
    newData <- getUser userID
        >>= if null activeEffects then pure . f else go activeEffects
    setUser userID newData
    pure newData
  where
    go (eff : effs) data_ = onModifyUser eff userID data_ (f data_) >>= go effs
    go []           data_ = pure data_

getEffect :: Text -> StatusEffect
getEffect = Unsafe.fromJust . findEffect

findEffect :: Text -> Maybe StatusEffect
findEffect name = find ((== name) . effectName) statusEffects

-- TODO some effects should be more common than others...
--      ...if only to not silence too often
inflictRandomly :: DictM (StatusEffect, GuildMember)
inflictRandomly = do
    member <- randomMember
    effect <- newStdGen <&> randomChoice statusEffects
    void $ modifyUser (userId . memberUser $ member)
                      (over userEffects $ Set.insert (effectName effect))
    pure (effect, member)
