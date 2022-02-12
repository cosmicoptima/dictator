{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.NPCs
  ( npcSpeak
  ) where

import           Relude                  hiding ( many )

import           Game
import           Game.Data               hiding ( userName )
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

import           Control.Lens            hiding ( noneOf )
import           Control.Monad.Except           ( throwError )
import           Data.Aeson
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           Discord.Requests
import           Discord.Types
import           System.Exit                    ( ExitCode(..) )
import           System.Process.Typed
import           Text.Parsec


data MemoriesInput = MemoriesInput
  { miMessages :: [Text]
  , miMemories :: [Text]
  }
  deriving Generic

instance ToJSON MemoriesInput

newtype MemoriesOutput = MemoriesOutput (Maybe Text)

instance FromJSON MemoriesOutput where
  parseJSON =
    withObject "MemoriesOutput" $ \o -> MemoriesOutput <$> o .:? "memory"


npcSpeak :: ChannelId -> Text -> DictM ()
npcSpeak channel npc = do
  messages <- reverse . filter (not . T.null . messageText) <$> restCall'
    (GetChannelMessages channel (8, LatestMessages))

  memories <- getNPC npc <&> maybe [] (Set.elems . view npcMemories)
  liftIO $ encodeFile "python/input.json"
                      (MemoriesInput (map messageText messages) memories)

  (exitCode, _, stderr_) <- readProcess $ proc "python3" ["python/memories.py"]
  when (exitCode /= ExitSuccess) (throwError . Fuckup . decodeUtf8 $ stderr_)

  memoryJSON <- lift $ readFileBS "python/output.json"
  let memoryEither = eitherDecodeStrict memoryJSON
  memory <- case memoryEither of
    Left  err                     -> throwError . Fuckup . fromString $ err
    Right (MemoriesOutput memory) -> pure memory

  let thought = maybe "" (\m -> npc <> " thinks: " <> m <> "\n") memory
      history =
        T.concat (map renderMessage messages) <> thought <> npc <> " says:"

  output <- getJ1With (J1Opts 1.1 0.9) 32 history <&> parse parser ""
  case output of
    Left  f -> throwError $ Fuckup (show f)
    Right t -> impersonateUser (Right $ npc <> " (0)") channel t

 where
  renderMessage m =
    (userName . messageAuthor) m <> " says: " <> messageText m <> "\n"

  parser = fromString <$> many (noneOf "\n")
