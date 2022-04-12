{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE FlexibleContexts #-}

module Game.Turing where

import           Control.Lens
import           Control.Lens.Extras
import qualified Data.Map                      as Map
import           Data.String.Interpolate
import qualified Data.Text                     as T
import           Discord.Internal.Types.Prelude
import           Discord.Requests
import           Discord.Types
import           Game.Data
import           Relude
import           Relude.Unsafe                  ( read )
import           Safe
import           Text.Parsec
import           Utils
import           Utils.DictM
import           Utils.Discord

botChannels :: [ChannelId]
botChannels = [878376227428245558]

-- The chance that 
impersonateChance :: Double
impersonateChance = 0.05

data PostKind = User | Bot deriving (Show, Eq, Generic)
data PostInfo = PostInfo
  { _postUser :: UserId
  , _postKind :: PostKind
  }
  deriving (Show, Eq, Generic)

makeLenses ''PostInfo

getPoster :: MessageId -> DictM (Maybe PostInfo)
getPoster = undefined

setPoster :: MessageId -> PostInfo -> DictM ()
setPoster = undefined

-- | Called when a user suspects a post of being bot-derived in a bot channel.
handleCallout :: MessageId -> UserId -> DictM ()
handleCallout message user = whenJustM (getPoster message) $ \info -> do
  let correct = info ^. postKind == Bot
      reward  = if correct then 1 else -1
  modifyGlobal_ . over globalScores $ Map.insertWith (+) user reward

-- | Called every day to manage the results of the turing test game.
handleResults :: DictM ()
handleResults = do
  results <- view globalScores <$> getGlobal
  -- Take the largest positive score as the result.
  let
    scores    = sortWith snd $ Map.toList results
    winner    = headMay $ filter ((> 0) . snd) scores
    runnerUps = unlines . fmap display . take 3 . drop 1 $ scores

    comment   = maybe "You're all horrible"
                      (\(u, _) -> [i|Congratulations, <@#{u}>.|])
                      winner
    winnerField = ("Winner", maybe "Nobody won." display winner)
    runnerField =
      ( "Runner-ups"
      , if T.null runnerUps then "You're all losers." else runnerUps
      )
    example = [i|For example; <@#{dictId}> MY BELOVED DICTATOR.|]
    explainField =
      ( "Reward"
      , unlines
        [ "You can now rename one user, including yourself."
        , "Mention them, and then give them a new name name."
        , example
        ]
      )
    fields = [winnerField, runnerField] ++ [explainField | isJust winner]
    embed = mkEmbed "Results" "" fields Nothing

  channel <- channelId <$> getChannelNamedOr Fuckup "results"
  restCall'_ $ CreateMessageEmbed channel (voiceFilter comment) embed

  
  where display (user, score) = [i|User <@#{user}> with #{score} points.|]

-- | Called when a user posts in the reward channel.
handleReward :: Message -> DictM ()
handleReward message = undefined
--  where
--   parser = do
--     void $ string "<@"
--     user <- many1 digit
--     void $ string "> "
--     name <- many1 anyChar
--     return (read user, name)
