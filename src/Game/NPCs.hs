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
    (GetChannelMessages channel (20, LatestMessages))

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

  decides :: Bool <- randomIO
  let
    thought = maybe
      ("" :: Text)
      (\m ->
        [i|(#{npc} thinks: "#{m}")\n(#{npc} decides#{if decides then "" else " not" :: Text} to talk about this)\n|]
      )
      memory
    -- prompt =
    --   T.concat (map renderMessage messages) <> thought <> npc <> " says:"
    prompt =
      [i|#{T.concat (map renderMessage messages)}#{thought}#{npc} says:|]

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


data NPCPersonality = NPCPersonality
  { pname      :: Text
  , padjs      :: [Text]
  , pinterests :: [Text]
  , pmemory    :: Text
  }

createNPC :: DictM Text
createNPC = do
  output <- getJ1 128 prompt
  case parse parser "" output of
    Left  f -> throwError $ Fuckup (show f <> ":\n" <> output)
    Right (NPCPersonality name adjs ints mem) -> do
      avatar <- randomImage
      setNPC name $ NPCData { _npcAvatar     = Just avatar
                            , _npcAdjectives = adjs
                            , _npcInterests  = ints
                            , _npcMemories   = Set.fromList [mem]
                            }
      pure name
 where
  prompt
    = "A small group of previously unknown writers seem to have found their breakout hit: a niche, absurdist online drama of sorts whose storyline plays out on a Discord server. The server (in which only in-character accounts may post) reached 10,000 members on August 18. The plot evolves quickly and characters are ephemeral, but the 22 active characters are currently as follows:\n\n\
    \Username | Personality | Interests | Most recent thought\n\
    \Elmer Foppington | nosy, cheeky, likes nothing more than a cup of tea and a bit of a gossip | antiques, tea parties, murder | \"i am going to kill someone in this chat\"\n\
    \Normal Man | average, dead behind the eyes | cooking, vague non-sequiturs | \"i'm going to make a big pot of stew\"\n\
    \Aberrant | psychopathic, literally raving mad, charismatic | russian avant garde poetry, grotesque body horror, elaborate trolling campaigns | \"JWCTRSDW MSLAWSLT TBMLHG SBQFAAF BSLLWLO IAS YVS AD\"\n\
    \pancake10 | obsequious, weaselly, smooth | '90s cyberculture, hypnotics, space colonization | \"i am doing crystal meth and i think i'm in astral projection\"\n\
    \take earth | tone-deaf, bumbling, manic | transhumanism, alternative medicine, semi-racist right-wing politics | \"i think i'm a cat\"\n"
  parser = do
    name <- many1 (noneOf "|") <&> T.strip . fromString
    _    <- char '|'
    adjs <- many1 (noneOf "|") <&> map T.strip . T.splitOn "," . fromString
    _    <- char '|'
    ints <- many1 (noneOf "|") <&> map T.strip . T.splitOn "," . fromString
    _    <- char '|'
    mem  <- many1 (noneOf "\n") <&> T.strip . fromString
    pure $ NPCPersonality name adjs ints mem
