{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE FlexibleContexts #-}

module Game.Turing where

import           Control.Lens            hiding ( noneOf )
import           Control.Monad.Except           ( MonadError(throwError) )
import qualified Data.Map                      as Map
import           Data.String.Interpolate
import qualified Data.Text                     as T
import           Discord.Internal.Types.Prelude
import           Discord.Requests
import           Discord.Types
import           Game
import           Game.Data
import           Relude                  hiding ( many )
import           Relude.Unsafe                  ( read )
import           Safe
import           Text.Parsec
import           Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

botChannels :: [ChannelId]
botChannels = [878376227428245558]

repostMessage :: Message -> DictM ()
repostMessage message = do
  member <- userToMemberOr Fuckup (userId . messageAuthor $ message)
  msgId  <- postAsUser member (messageChannel message) (messageText message)
  setTuring msgId $ PostInfo { _postKind = UserPost
                             , _postUser = (userId . memberUser) member
                             }

impersonateUser :: ChannelId -> UserId -> DictM ()
impersonateUser whereTo whoTo = do
  messages <- restCall' $ GetChannelMessages whereTo (50, LatestMessages)
  member   <- userToMemberOr Fuckup whoTo
  let prompt =
        T.concat (map renderMessage . reverse $ messages)
          <> (userName . memberUser) member
          <> "\n"
  output <- getJ1 32 prompt <&> parse parser ""
  case output of
    Left  f -> throwError $ Fuckup (show f)
    Right t -> do
      msgId <- postAsUser member whereTo t
      setTuring msgId $ PostInfo { _postKind = BotPost
                                 , _postUser = (userId . memberUser) member
                                 }
 where
  renderMessage m =
    (userName . messageAuthor) m <> "\n" <> messageText m <> "\n\n"
  parser = fromString <$> many (noneOf "\n")

-- | Called when a user suspects a post of being bot-derived in a bot channel.
handleCallout :: MessageId -> UserId -> DictM ()
handleCallout message user = whenJustM (getTuring message) $ \info -> do
  let correct = info ^. postKind == BotPost
      reward  = if correct then 1 else -1
  sendMessageToGeneral "yeah ok"
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
    fields = [winnerField, runnerField] ++ [ explainField | isJust winner ]
    embed  = mkEmbed "Results" "" fields Nothing

  modifyGlobal_ $ set globalScores Map.empty
  channel <- channelId <$> getChannelNamedOr Fuckup "results"
  restCall'_ $ CreateMessageEmbed channel (voiceFilter comment) embed
  where display (user, score) = [i|User <@#{user}> with #{score} points.|]

-- | Called when a user posts in the reward channel.
handleReward :: Message -> DictM ()
handleReward message = do
  let author  = userId . messageAuthor $ message
      channel = messageChannel message

  rewardChannel <- getChannelNamedOr Fuckup "results"
  winner <- getGlobal >>= fromJustOr (Fuckup "no winner") . view globalWinner
  when (author /= winner) . restCall'_ $ DeleteMessage
    (channel, messageId message)

  (user, nickname) <- getParsed $ parse parser "" (messageText message)
  sendMessage (channelId rewardChannel) "As you wish."
  setNickname user nickname

  modifyGlobal_ $ set globalWinner Nothing
 where
  parser = do
    void $ string "<@"
    user <- many1 digit
    void $ string "> "
    name <- many1 anyChar
    return (read user, fromString name)
