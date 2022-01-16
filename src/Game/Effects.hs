{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Status effects!

module Game.Effects where

import           Relude
import qualified Relude.Unsafe                 as Unsafe

import           Utils.DictM
import           Utils.Discord

import           Data.Default
import           Discord.Requests
import           Discord.Types


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


getEffect :: Text -> StatusEffect
getEffect name = Unsafe.fromJust $ find ((== name) . effectName) statusEffects
