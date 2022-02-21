{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Game.NPCs
  ( npcSpeak
  , randomNPCSpeakGroup
  , randomNPCConversation
  , createNPC
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
import qualified Network.Wreq.Session          as S
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
  session  <- asks envSs
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

  res <- liftIO $ asJSON =<< S.postWith
    (defaults & checkResponse ?~ (\_ _ -> pure ()))
    session
    "http://localhost:5000"
    (toJSON $ MemoriesInput (map messageText messages) memories)
  let MemoriesOutput memory debug = res ^. responseBody

  forM_ debug $ sendMessageToBotspam . ("```\n" <>) . (<> "\n```")

  -- Pick a random choice from the memories instead of using the embedding, for now.
  -- memory <-
  --   randomChoiceMay (npcData ^. npcMemories . to Set.elems) <$> newStdGen
  let
    thought = maybe
      ""
      (\m -> [i|(#{npc} thinks: "#{m}")\n(#{npc} decides to talk about this)\n|]
      )
      memory
    prompt =
      T.concat (map renderMessage messages) <> thought <> npc <> " says:"

  output <-
    getJ1UntilWith (J1Opts 1 0.9 1 [("<|newline|>", 1)]) ["\n"] prompt
      <&> parse parser ""
  case output of
    Left  f              -> throwError $ Fuckup (show f)
    Right (T.strip -> t) -> do
      n :: Double <- randomIO
      when ("i " `T.isPrefixOf` t || "i'" `T.isPrefixOf` t || n < 0.2)
        . void
        $ modifyNPC npc (over npcMemories $ Set.insert t)
      sendWebhookMessage channel t npc (Just avatar)

 where
  renderMessage m =
    (userName . messageAuthor) m <> " says: " <> messageText m <> "\n"

  parser = fromString <$> many (noneOf "\n")


npcSpeakGroup :: ChannelId -> Text -> DictM ()
npcSpeakGroup channel npc = do
  nMessages <- randomRIO (1, 3)
  replicateM_ nMessages $ npcSpeak channel npc

randomNPCSpeakGroup :: ChannelId -> DictM ()
randomNPCSpeakGroup channel = do
  npcs <- listNPC
  npc  <- newStdGen <&> randomChoice npcs
  npcSpeakGroup channel npc

randomNPCConversation :: Int -> ChannelId -> DictM ()
randomNPCConversation l channel = do
  npcs         <- listNPC
  nNPCs        <- randomRIO (2, 4)
  selectedNPCs <- replicateM nNPCs (newStdGen <&> randomChoice npcs)
  replicateM_ l $ do
    npc <- newStdGen <&> randomChoice selectedNPCs
    npcSpeak channel npc


createNPC :: DictM Text
createNPC = do
  newName <-
    getJ1FromContext 8 "The following is a list of short NPC names" exNames
      <&> parse parser ""
  firstMemory <-
    getJ1FromContext
        16
        "The following is a list of thoughts, beliefs, and memories"
        exMemories
      <&> parse parser ""
  case (newName, firstMemory) of
    (Right (T.strip -> name), Right (T.strip -> memory)) -> do
      avatar <- randomImage
      setNPC name $ NPCData { _npcAvatar   = Just avatar
                            , _npcMemories = Set.fromList [memory]
                            }
      pure name
    _ -> throwError $ Fuckup "Failed to create NPC"
 where
  exNames =
    [ "gotham"
    , "borscht"
    , "deranged clown"
    , "josef stalin"
    , "cat"
    , "PerniciousBob"
    , "john"
    ]
  exMemories =
    [ "mice are not real"
    , "i need to ensure no one knows i am gay"
    , "i am going to kill someone in this chat"
    , "i am a kitty"
    , "i love technology! all glory to our ai overlord"
    , "our glorious dictator deserves far more than a peon such as myself"
    , "i am so cool"
    ]
  parser = fromString <$> many (noneOf "\n")
