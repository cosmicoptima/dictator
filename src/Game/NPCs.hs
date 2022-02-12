{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.NPCs
  ( npcSpeak
  ) where

import           Relude                  hiding ( many )

import           Game
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

import           Control.Monad.Except           ( throwError )
import           Data.Aeson
import qualified Data.Text                     as T
import           Discord.Requests
import           Discord.Types
import           System.Exit                    ( ExitCode(..) )
import           System.Process.Typed
import           Text.Parsec


npcSpeak :: ChannelId -> Text -> DictM ()
npcSpeak channel npc = do
  messages <- reverse
    <$> restCall' (GetChannelMessages channel (50, LatestMessages))

  liftIO $ encodeFile "python/input.json" (map messageText messages)
  (exitCode, _, stderr) <- readProcess $ proc "python3" ["python/memories.py"]
  when (exitCode /= ExitSuccess) (throwError . Fuckup . decodeUtf8 $ stderr)
  _notUsedYet <- lift (readFileText "python/output.json")

  let history = T.concat (map renderMessage messages) <> npc <> " says:"

  output <- getJ1 32 history <&> parse parser ""
  case output of
    Left  f -> throwError $ Fuckup (show f)
    Right t -> impersonateUser (Right $ npc <> " (0)") channel t

 where
  renderMessage m =
    (userName . messageAuthor) m <> " says: " <> messageText m <> "\n"

  parser = fromString <$> many (noneOf "\n")
