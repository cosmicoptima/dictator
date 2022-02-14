{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Game.NPCs
  ( npcSpeak
  , randomNPCSpeakGroup
  ) where

import           Relude                  hiding ( many )

import           Game
import           Game.Data               hiding ( userName )
import           Utils
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
import           Network.Wreq
import           System.Random
import           Text.Parsec


data MemoriesInput = MemoriesInput
  { miMessages :: [Text]
  , miMemories :: [Text]
  }
  deriving Generic

instance ToJSON MemoriesInput

data MemoriesOutput = MemoriesOutput (Maybe Text) [Text]

instance FromJSON MemoriesOutput where
  parseJSON = withObject "MemoriesOutput"
    $ \o -> MemoriesOutput <$> o .:? "memory" <*> o .: "debug"


npcSpeak :: ChannelId -> Text -> DictM ()
npcSpeak channel npc = do
  messages <- reverse . filter (not . T.null . messageText) <$> restCall'
    (GetChannelMessages channel (50, LatestMessages))

  npcData <- getNPC npc
  avatar  <- encodeAvatarData <$> case npcData ^. npcAvatar of
    Just av -> return av
    Nothing -> do
      avatar <- randomImage
      setNPC npc $ npcData & npcAvatar ?~ avatar
      return avatar

  let memories = npcData ^. npcMemories . to Set.elems

  res <- liftIO $ asJSON =<< postWith
    (defaults & checkResponse .~ Just (\_ _ -> pure ()))
    "http://localhost:5000"
    (toJSON $ MemoriesInput (map messageText messages) memories)
  let MemoriesOutput memory debug = res ^. responseBody

  forM_ debug $ sendMessageToBotspam . ("```\n" <>) . (<> "\n```")

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
    Right t -> do
      when ("i " `T.isPrefixOf` t) . void $ modifyNPC
        npc
        (npcMemories %~ Set.insert t)
      sendWebhookMessage channel t npc (Just avatar)

 where
  renderMessage m =
    (userName . messageAuthor) m <> " says: " <> messageText m <> "\n"

  parser = fromString <$> many (noneOf "\n")


randomNPCSpeakGroup :: ChannelId -> DictM ()
randomNPCSpeakGroup channel = do
  npcs      <- getallNPC <&> map fst
  npc       <- newStdGen <&> randomChoice npcs
  nMessages <- randomRIO (1, 4)
  replicateM_ nMessages $ npcSpeak channel npc
