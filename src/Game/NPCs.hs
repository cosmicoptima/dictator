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
import qualified Data.Text                     as T
import           Discord.Requests
import           Discord.Types
import           System.Process.Typed
import           Text.Parsec


npcSpeak :: ChannelId -> Text -> DictM ()
npcSpeak channel npc = do
  runProcess_ $ proc "python3" ["python/memories.py"]
  lift $ readFile "python/output.json" >>= debugPrint

  messages <- restCall' $ GetChannelMessages channel (50, LatestMessages)
  let history =
        T.concat (map renderMessage . reverse $ messages) <> npc <> "\n"

  output <- getJ1 32 history <&> parse parser ""
  case output of
    Left  f -> throwError $ Fuckup (show f)
    Right t -> impersonateUser (Right $ npc <> " (0)") channel t

 where
  renderMessage m =
    (userName . messageAuthor) m <> "\n" <> messageText m <> "\n\n"

  parser = fromString <$> many (noneOf "\n")
