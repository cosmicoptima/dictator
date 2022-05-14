{-# LANGUAGE DeriveGeneric       #-}

module Game.NPCs
  ( npcSpeak
  , randomNPCSpeakGroup
  , randomNPCConversation
  , createNPC
  ) where

import           Relude                  hiding ( many )

import           Game
import           Game.Data
import           Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

import           Control.Lens            hiding ( noneOf )
import           Control.Monad.Except           ( throwError )
import qualified Data.Set                      as Set
import           Data.String.Interpolate        ( i )
import qualified Data.Text                     as T
import           Discord.Requests
import           Discord.Types
import           System.Random
import           Text.Parsec


npcSpeak :: ChannelId -> Text -> DictM ()
npcSpeak channel npc = do
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
  memory <- fmap (randomChoiceMay memories) newStdGen

  decides :: Bool <- randomIO
  let
    thought = maybe
      ("" :: Text)
      (\m ->
        [i|(#{npc} thinks: "#{m}")\n(#{npc} decides#{if decides then "" else " not" :: Text} to talk about this)\n|]
      )
      memory
    introduction :: Text
      = [i|In this Discord chatlog, pay close attention to the fictional character "#{npc}"'s speech patterns; #{npc} is designed around the following traits:\nPersonality: #{T.intercalate ", " (npcData ^. npcAdjectives)}\nInterests: #{T.intercalate ", " (npcData ^. npcInterests)}\nHere is the chatlog:|]
    -- prompt =
    --   T.concat (map renderMessage messages) <> thought <> npc <> " says:"
    prompt
      = [i|#{introduction}#{T.concat (map renderMessage messages)}#{thought}#{npc} says:|]

  output <-
    getJ1UntilWith (J1Opts 1 0.85 0.3 [("<|newline|>", 0.3)]) ["\n"] prompt
      <&> parse parser ""
  case output of
    Left  f              -> throwError $ Fuckup (show f)
    Right (T.strip -> t) -> do
      n :: Double <- randomIO
      when ("i " `T.isPrefixOf` t || "i'" `T.isPrefixOf` t || n < 0.2)
        . void
        $ modifyNPC npc (over npcMemories $ Set.insert t)
      void $ sendWebhookMessage channel t npc (Just avatar)

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
  { personName      :: Text
  , personAdjs      :: [Text]
  , personInterests :: [Text]
  , personmMemory    :: Text
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
    = "A small group of previously unknown writers seem to have found their breakout hit: a niche, absurdist online drama of sorts whose storyline plays out on a Discord server. The server (in which only the writers' in-character accounts may post) reached 10,000 members on August 18. The plot evolves quickly and characters are ephemeral, but the 22 active characters are currently as follows:\n\n\
    \Username | Personality | Interests | Most worrying thought\n\
    \Elmer Foppington | nosy, cheeky, likes nothing more than a cup of tea and a bit of a gossip | antiques, tea parties, murder | \"i am going to kill someone in this chat\"\n\
    \Normal Man | average, dead behind the eyes | cooking, vague non-sequiturs | \"i haven't cared about my wife in years\"\n\
    \Aberrant | psychopathic, literally raving mad, charismatic | russian avant garde poetry, grotesque body horror, elaborate trolling campaigns | \"my penis is very small and weak\"\n\
    \pancake10 | obsequious, weaselly, smooth | '90s cyberculture, hypnotics, incels | \"mice aren't real\"\n\
    \nick land | tone-deaf, bumbling, manic | transhumanism, semi-racist right-wing politics, memetics | \"i am a kitty meow mew\"\n"
  parser = do
    name <- many1 (noneOf "|") <&> T.strip . fromString
    _    <- char '|'
    adjs <- many1 (noneOf "|") <&> map T.strip . T.splitOn "," . fromString
    _    <- char '|'
    ints <- many1 (noneOf "|") <&> map T.strip . T.splitOn "," . fromString
    _    <- string "| \""
    mem  <- many1 (noneOf "\"\n") <&> T.strip . fromString
    _    <- string "\""
    pure $ NPCPersonality name adjs ints mem
