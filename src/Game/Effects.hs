{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Status effects!

module Game.Effects where

import           Relude
import qualified Relude.Unsafe                 as Unsafe

import           Game.Data
import           Utils.DictM
import           Utils.Discord

import           Control.Lens
import           Data.Default
import qualified Data.Set                      as Set
import           Discord.Requests
import           Discord.Types
import           System.Random
import Data.String.Interpolate


data StatusEffect = StatusEffect
    { effectName   :: Text
    , avgLength    :: Int
    , everyMessage :: Message -> DictM ()
    , everySecond  :: GuildMember -> DictM ()
    }

instance Default StatusEffect where
    def = StatusEffect { effectName   = "owned"
                       , avgLength    = 900
                       , everyMessage = const $ pure ()
                       , everySecond  = const $ pure ()
                       }

statusEffects :: [StatusEffect]
statusEffects =
    [ def
          { effectName   = "muted"
          , avgLength    = 20
          , everyMessage = \m ->
              restCall' $ DeleteMessage (messageChannel m, messageId m)
          }
    ]


cancelEffects :: DictM ()
cancelEffects = do
    forM_ statusEffects $ \eff -> do
        let p = 1 / (fromIntegral . avgLength $ eff :: Double)
        getMembers >>= mapM_
            (\member -> do
                let userID = (userId . memberUser) member
                n <- randomIO
                when
                    (n < p)
                    (do
                        void
                            . modifyUser userID
                            . over userEffects
                            . Set.delete
                            $ effectName eff
                        sendMessageToGeneral [i|Rejoice, for I am magnanimous! <@#{userID}> is no longer #{effectName eff}.|]
                    )
            )

getEffect :: Text -> StatusEffect
getEffect name = Unsafe.fromJust $ find ((== name) . effectName) statusEffects
