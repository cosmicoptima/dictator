{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

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
import           Data.String.Interpolate        ( i )
import qualified Data.Text                     as T
import           Discord.Requests
import           Discord.Types
import           System.Exit                    ( ExitCode(..) )
import           System.Process.Typed
import           Text.Parsec
import           Utils                          ( randomImage )


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
    (GetChannelMessages channel (50, LatestMessages))

  npcData <- getNPC npc
  avatar  <- case npcData ^. npcAvatar of
    Just av -> return av
    Nothing -> do
      avatar <- encodeAvatarData <$> randomImage
      setNPC npc $ npcData & npcAvatar ?~ avatar
      return avatar

  let memories = npcData ^. npcMemories . to Set.elems
  liftIO $ encodeFile "python/input.json"
                      (MemoriesInput (map messageText messages) memories)

  (exitCode, _, stderr_) <- readProcess $ proc "python3" ["python/memories.py"]
  when (exitCode /= ExitSuccess) (throwError . Fuckup . decodeUtf8 $ stderr_)

  memoryJSON <- lift $ readFileBS "python/output.json"
  let memoryEither = eitherDecodeStrict memoryJSON
  memory <- case memoryEither of
    Left  err                     -> throwError . Fuckup . fromString $ err
    Right (MemoriesOutput memory) -> pure memory

  let
    thought = maybe
      ""
      (\m -> [i|(#{npc} thinks: "#{m}")\n(#{npc} decides to talk about this)\n|]
      )
      memory
    prompt =
      T.concat (map renderMessage messages) <> thought <> npc <> " says:"

  output <- getJ1With (J1Opts 0.95 0.9) 32 prompt <&> parse parser ""
  case output of
    Left  f -> throwError $ Fuckup (show f)
    Right t -> sendWebhookMessage channel t (npc <> " (0)") (Just avatar)

 where
  renderMessage m =
    (userName . messageAuthor) m <> " says: " <> messageText m <> "\n"

  parser = fromString <$> many (noneOf "\n")
