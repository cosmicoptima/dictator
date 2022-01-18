{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
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
import           Discord.Types
import           System.Random


data StatusEffect = StatusEffect
    { effectName   :: Text
    , avgLength    :: Int
    , inflictPrice :: Int
    , everyMessage :: Message -> DictM ()
    , everySecond  :: UserId -> DictM ()
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
                       }

statusEffects :: [StatusEffect]
statusEffects =
    [ def
        { effectName   = "silenced"
        , avgLength    = seconds 20
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
    , def { effectName = "known", avgLength = minutes 10, inflictPrice = 25 }
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

                        sendMessageToGeneral
                            [i|Rejoice, for I am magnanimous! <@#{userID}> is no longer #{effectName eff}.|]

                -- Remove invalid effects
                Nothing ->
                    void
                        . modifyUser userID
                        . over userEffects
                        . Set.delete
                        $ effName

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
