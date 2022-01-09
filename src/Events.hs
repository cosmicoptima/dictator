-- | ??? this file's purpose is unclear

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Events where

import           Relude                  hiding ( First )

-- local modules
----------------
import           Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

--------
-- discord
----------
import           Discord
import           Discord.Types

-- color

-- all else
-----------
import           Control.Monad                  ( liftM2 )
import qualified Database.Redis                as DB
import           System.Random


-- GPT
------

randomAdjective :: DictM Text
randomAdjective = liftIO $ liftM2 randomChoice getAdjList getStdGen

pontificate :: ChannelId -> Text -> DictM ()
pontificate channel what = do
    adj      <- randomAdjective
    response <-
        getJ1 32 $ "Dictator's " <> adj <> " thoughts on " <> what <> ":\n"
    sendMessage channel $ case lines response of
        (_ : line : _) -> line
        (line     : _) -> line
        _              -> response

dictate :: DictM ()
dictate = do
    adj    <- randomAdjective
    output <- getJ1FromContext
        16
        ("A " <> adj <> " forum dictator decrees the following")
        decrees
    case lines output of
        (l : _) | voiceFilter l `notElem` fmap voiceFilter decrees ->
            sendMessageToGeneral l
        _ -> dictate
  where
    decrees =
        [ "i hereby decree that all members are forbidden from using the message board"
        , "i hereby declare my superiority over other posters"
        , "i hereby declare war upon the so-called \"elite\""
        , "i hereby decree my death"
        , "i hereby decree that credits shall be reinstated"
        , "i hereby decree that no members may use lowercase in their postings"
        , "i hereby declare ignorantism the official ideology"
        , "i hereby ban the user gotham"
        , "i hereby declare myself better than you"
        ]


-- other
--------


stopDict :: DB.Connection -> DictM ()
stopDict conn = do
    sendMessageToGeneral "I'm so tired..."
    liftIO $ DB.disconnect conn
    lift stopDiscord
