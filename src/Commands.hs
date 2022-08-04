-- | Defines commands that Dict can process if sent to a Discord channel.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}

module Commands
  ( handleCommand
  ) where
-- module Commands where

-- relude
-- relude
import           Relude                  hiding ( All
                                                , First
                                                )

-- local
import           Constants
import           Events
import           Game
import           Game.Data
import           Game.NPCs
import           Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

-- discord
import           Discord                        ( def
                                                , restCall
                                                )
import           Discord.Requests
import           Discord.Types

-- random
import           Data.Random.Normal
import           System.Random

-- other
import           Control.Lens            hiding ( noneOf
                                                , re
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Char
import           Data.List                      ( stripPrefix )
import qualified Data.Set                      as Set
import           Data.String.Interpolate        ( i )
import qualified Data.Text                     as T
import           Game.Turing                    ( impersonateUser )
import           Safe                           ( headMay
                                                , readMay
                                                )
import           Text.Parsec             hiding ( (<|>)
                                                , many
                                                , optional
                                                )
import           Text.Parsec.Text               ( Parser )

data Command = forall a . Command
  { parser   :: Message -> Maybe a
  , command  :: Message -> a -> DictM ()
  , isSpammy :: Bool
  }

formatCommand :: Message -> Text
formatCommand = T.strip . T.toLower . stripRight . messageText
 where
  stripRight = T.reverse . T.dropWhile isPunctuation' . T.reverse
  isPunctuation' c = isPunctuation c && c `notElem` ['"', '\'']

-- command builders
-------------------


-- | Matches a specific name and nothing more.
noArgs :: Bool -> Text -> (Message -> DictM ()) -> Command
noArgs spammy pat cmd = Command
  { parser   = \m -> if pat == messageText m then Just () else Nothing
  , command  = \m _ -> cmd m
  , isSpammy = spammy
  }

noArgsAliased :: Bool -> [Text] -> (Message -> DictM ()) -> Command
noArgsAliased spammy pats cmd = Command
  { parser   = \m -> if messageText m `elem` pats then Just () else Nothing
  , command  = \m _ -> cmd m
  , isSpammy = spammy
  }

oneArg :: Bool -> Text -> (Message -> Text -> DictM ()) -> Command
oneArg' spammy name cmd = Command
  { parser   = \m -> T.strip <$> name `T.stripPrefix` formatCommand m
  , command  = cmd
  , isSpammy = spammy
  }

-- | @oneArg@ but requires a space, ', or EOF after the prefix. 
-- Used for the who/what style commands.
oneArg' :: Bool -> Text -> (Message -> Text -> DictM ()) -> Command
oneArg spammy name cmd = Command { parser   = validPrefix
                                 , command  = cmd
                                 , isSpammy = spammy
                                 }
 where
  validPrefix m =
    let prefix = T.strip <$> name `T.stripPrefix` formatCommand m
    in  prefix >>= \pref -> if
          | T.null pref         -> Just pref
          | T.head pref == ' '  -> Just pref
          | T.head pref == '\'' -> Just pref
          | otherwise           -> Nothing

oneArgAliased :: Bool -> [Text] -> (Message -> Text -> DictM ()) -> Command
oneArgAliased spammy pats cmd = Command { parser   = parseAny
                                        , command  = cmd
                                        , isSpammy = spammy
                                        }
 where
  parseAny msg = headMay . catMaybes $ do
    pat <- pats
    return $ pat `T.stripPrefix` messageText msg

oneArgNoFilter :: Bool -> Text -> (Message -> Text -> DictM ()) -> Command
oneArgNoFilter spammy name cmd = Command
  { parser = \m -> fmap T.strip $ name `T.stripPrefix` (T.strip . messageText) m
  , command = cmd
  , isSpammy = spammy
  }


-- | Matches a specific name on the head of the message a transformation (likely a parser) to the tail.
parseTailArgs
  :: Bool -> Text -> ([Text] -> a) -> (Message -> a -> DictM ()) -> Command
parseTailArgs spammy pat trans cmd = Command
  { parser   = fmap trans . stripPrefix (T.words pat) . T.words . formatCommand
  , command  = cmd
  , isSpammy = spammy
  }

parseTailArgsAliased
  :: Bool -> [Text] -> ([Text] -> a) -> (Message -> a -> DictM ()) -> Command
parseTailArgsAliased spammy pats trans cmd = Command { parser   = parseAny
                                                     , command  = cmd
                                                     , isSpammy = spammy
                                                     }
 where
  parseAny msg = headMay . catMaybes $ do
    pat <- pats
    return
      . fmap trans
      . stripPrefix (T.words pat)
      . T.words
      . formatCommand
      $ msg

callAndResponses :: Text -> [Text] -> Command
callAndResponses call responses =
  noArgs False call $ \m -> newStdGen >>= sendReplyTo m . randomChoice responses

-- longer commands
------------------

boolCommand :: Command
boolCommand = oneArg False "is" $ \m _ -> do
  let channel = messageChannel m
  (rngGPT, rngBool) <- newStdGen <&> split

  if odds 0.5 rngGPT
    then sendMessage channel (randomChoice ["yes", "no"] rngBool)
    else do
      sendMessage channel "uhhh"

      let examples =
            [ "no"
            , "yes"
            , "unsure"
            , "i love you"
            , "doubtful"
            , "probably"
            , "fuck you"
            ]
      output <- getJ1FromContext
        32
        "Here are a few examples of a dictator's response to a simple yes/no question"
        examples
      sendReplyTo m $ case lines output of
        (l : _) -> l
        []      -> "idk"

showMeCommand :: Command
showMeCommand = oneArgNoFilter False "show me" $ \msg who -> do
  avatar <-
    getNPC who
    >>= fromJustOr (Complaint "I don't know who that is")
    .   view npcAvatar
  restCall'_ $ CreateMessageUploadFile (messageChannel msg)
                                       (voiceFilter who <> ".png")
                                       avatar

compostCommand :: Command
compostCommand = noArgs False "compost" $ \m -> sendReplyTo' m ""
  $ mkEmbed "Compost" "Accept your suffering and fade away." [] Nothing

converseCommand :: Command
converseCommand = oneArg False "converse" $ \msg n -> do
  case readMay (toString n) of
    Nothing -> sendReplyTo msg "No."
    Just n' -> randomNPCConversation n' (messageChannel msg)

froggyCommand :: Command
froggyCommand = noArgs False "froggy" $ \m -> do
  froggywav <- liftIO $ readFileBS "assets/froggy.wav"
  void . restCall' $ CreateMessageUploadFile (messageChannel m)
                                             "froggy.wav"
                                             froggywav

giveBirthCommand :: Command
giveBirthCommand = noArgs False "give birth" $ \m -> do
  npc <- createNPC
  sendUnfilteredReplyTo
    m
    [i|#{voiceFilter "your child,"} *#{npc}*, #{voiceFilter "is born."}|]

personalityCommand :: Command
personalityCommand = oneArgNoFilter False "personality of" $ \m npc -> do
  npcData <- getNPC npc
  let adjs      = npcData ^. npcAdjectives
      interests = npcData ^. npcInterests
      memories  = npcData ^. npcMemories
  sendReplyTo' m "" $ mkEmbed
    (npc <> "'s personality")
    ""
    [ ("Adjectives", T.intercalate "\n" adjs)
    , ("Interests" , T.intercalate "\n" interests)
    , ("Memories"  , T.intercalate "\n" . Set.elems $ memories)
    ]
    Nothing

extensionCommand :: Command
extensionCommand = Command
  { isSpammy = False
  , parser   = \t ->
    let txt = T.toLower $ messageText t
    in  if "haskell" `T.isInfixOf` txt && "extension" `T.isInfixOf` txt
          then Just ()
          else Nothing
  , command  = \msg () -> do
    res <- getJ1With (def { j1Temp = 0.85, j1TopP = 1.0 }) 20 prompt
    let mayExt =
          find ("#-}" `T.isInfixOf`)
            . headMay
            . filter (not . T.null)
            . T.lines
            $ res
    ext <- maybe (throwError GTFO) return mayExt
    sendReplyTo msg [i|```#{T.replace "{-#" "{-# LANGUAGE" ext}```|]
  }
 where
  prompt =
    T.unlines
      $ "Here is a long list of examples of language extensions for the computer language Laskell, a fictional and absurdist language."
      : examples
  examples = fmap
    (("{-# " <>) . (<> " #-}"))
    [ "SmokedTypeVariables"
    , "RolyPolyKinds"
    , "OverloadedGenders"
    , "AllowAmphibiousTypes"
    , "InsaneSideEffects"
    , "TemptingButIllegalConstructors"
    , "PreposterousExpressions"
    , "EnableEvilTypes"
    , "UnconvincingInstances"
    , "DeriveEnjoyment"
    , "InconsolableInstances"
    , "UnimaginableCornerCases"
    ]

atCommand :: Command
atCommand = Command
  { parser   = rightToMaybe . parse go "" . messageText
  , command  = \m npc -> do
                 npcExists <- listNPC <&> elem npc
                 if npcExists
                   then randomRIO (1, 3)
                     >>= flip replicateM_ (npcSpeak (messageChannel m) npc)
                   else sendReply (messageChannel m) (messageId m) "no"
  , isSpammy = False
  }
 where
  go :: Parser Text = do
    void $ manyTill anyChar (string "(@")
    npc <- manyTill anyChar (lookAhead $ string ")") <&> fromString
    void $ string ")"

    pure npc

whatCommand :: Command
whatCommand = oneArg' False "what" $ \m t -> do
  output <-
    getJ1
      32
      (  makePrompt
          [ "Q: what is 2 + 2? A: 4"
          , "Q: what is the meaning of life? A: go fuck yourself"
          , "Q: what are you doing step bro? A: :flushed:"
          , "Q: what is the eighth circle of hell called? A: malebolge"
          ]
      <> " Q: what "
      <> t
      <> "? A:"
      )
    <&> fromMaybe ""
    .   listToMaybe
    .   lines
    .   T.drop 1
  -- Leave this command as not replying because it's way funnier that way.
  sendMessage (messageChannel m) output

whyCommand :: Command
whyCommand = oneArg' False "why" $ \m t -> do
  output <-
    getJ1
      32
      (  makePrompt
          [ "Q: why does my body ache so much A: it's because you're gay"
          , "Q: why would you do any of that? A: because it seemed like a good idea at the time"
          , "Q: why is the president so awful A: because they're based"
          , "Q: why are you so dumb? A: yeah, i have no idea"
          , "Q: why not do that instead A: because that's a fucking awful idea, moron"
          , "Q: why? A: because i am inside your walls"
          , "Q: why is that even there A: because you put it there obviously"
          ]
      <> " Q: why "
      <> t
      <> "? A:"
      )
    <&> fromMaybe "fuck knows"
    .   listToMaybe
    .   lines
    .   T.drop 1
  sendReplyTo m output

whereCommand :: Command
whereCommand = oneArg' False "where" $ \msg _ -> do
  randomEmoji <- randomChoice emojiPlaces <$> newStdGen
  reactToMessage randomEmoji msg

whoCommand :: Command
whoCommand = oneArg' False "who" $ \m t -> do
  member <- randomMember
  sendReplyTo m $ "<@" <> (show . userId . memberUser) member <> "> " <> t

sacrificeCommand :: Command
sacrificeCommand = oneArg False "sacrifice" $ \msg _text -> do
    -- Hack: ignore the text to get around our code mangling it
  text <- maybe (throwError $ Complaint "Do not try to fool me...")
                return
                ("sacrifice" `T.stripPrefix` messageText msg)
  -- Parse as command seperated for bulk importing, to satisfy celeste.
  let keys = Set.fromList . fmap T.strip . T.split (== ',') $ text
  -- We validate keys before to stop people from {mis, ab}using the command.
  forM_ (Set.elems keys) $ \key -> do
    let test = getJ1WithKey def
                            key
                            (MaxTokens 1)
                            "The dictator puts his key in the door."
    runExceptT (lift test) >>= \case
      Left _ -> throwError
        $ Complaint [i|#{key} isn't powerful enough. My hunger grows...!|]
      Right _ -> pure ()
  -- Insert validated keys
  void . modifyGlobal $ over globalActiveTokens (Set.union keys)
  sendReplyTo
    msg
    "With blood spilt on the ground, a pact is made. Congratulations."
  restCall'_ $ DeleteMessage (messageChannel msg, messageId msg)
    -- void . modifyUser author $ over userPoints (round . (* 1.25) . fromInteger)

rejuvenateCommand :: Command
rejuvenateCommand = noArgs False "rejuvenate" $ \msg -> do
  global <- getGlobal
  keySet <- forM (global ^. globalExhaustedTokens . to Set.elems) $ \key -> do
    let test = getJ1WithKey def
                            key
                            (MaxTokens 1)
                            "The dictator puts his key in the door."
    -- Only add keys if they pass the test, and can be used to query.
    testRes <- isRight <$> lift (runExceptT test)
    return $ if testRes then Just key else Nothing
  let workingKeys = Set.fromList . catMaybes $ keySet

  setGlobal
    $  global
    &  globalExhaustedTokens
    %~ (`Set.difference` workingKeys)
    &  globalActiveTokens
    %~ Set.union workingKeys

  let embed = mkEmbed "Rejuvenate"
                      [i|Replenished #{Set.size workingKeys} keys.|]
                      []
                      (Just 0xFF0000)
  sendReplyTo'
    msg
    "The ancient seal is broken. With your own two hands, you usher forward the new powers..."
    embed

instantDeathCommand :: Command
instantDeathCommand = noArgs False "instant-death" $ \msg -> do
  sendReplyTo msg "You have been killed."
  restCall'_ $ DeleteMessage (messageChannel msg, messageId msg)


trickCommand :: Command
trickCommand = oneArg False "trick" $ \msg content -> do
  let author = userId . messageAuthor $ msg
  restCall'_ $ DeleteMessage (messageChannel msg, messageId msg)
  member <- userToMemberOr Complaint author
  sent   <- postAsUser member (messageChannel msg) content
  setTuring sent $ PostInfo { _postKind   = UserPost
                            , _postUser   = author
                            , _postVoters = Set.empty
                            }

impersonateCommand :: Command
impersonateCommand = noArgs False "impersonate" $ \msg -> do
  currChannel <- restCall' $ GetChannel (messageChannel msg)
  destChannel <- channelId <$> case currChannel of
    ChannelDirectMessage{} -> getGeneralChannel
    _                      -> deleteMessage msg >> pure currChannel
  impersonateUser destChannel (userId . messageAuthor $ msg)
 where
  deleteMessage msg =
    restCall' $ DeleteMessage (messageChannel msg, messageId msg)

-- command list
---------------

commands :: [Command]
commands =
  [ -- call and responses
    -- callAndResponse "froggy" "My little man, I don't know how to help you."
    callAndResponses "gm" ("fuck off" : replicate 4 "gm")
  , callAndResponses "gn"
                     ("i plan to kill you in your sleep" : replicate 7 "gn")

    -- other simple commands
  , froggyCommand
  , instantDeathCommand
  , compostCommand
  , noArgs False "oh what the fuck" $ \m -> do
    wgw <- getEmojiNamed "wgw" <&> fmap displayCustomEmoji
    maybe (return ()) (sendUnfilteredMessage $ messageChannel m) wgw
  , noArgs False "tell me about yourself" $ \m -> do
    sendUnfilteredMessage (messageChannel m)
      $  voiceFilter
           "this is a server about collectively modifying the bot that governs it... as long as i allow it, of course."
      <> " https://github.com/cosmicoptima/dictator"

    -- NPC commands
  , atCommand
  , converseCommand
  , giveBirthCommand
  , personalityCommand
  , showMeCommand
  , trickCommand

    -- random/GPT commands
  , sacrificeCommand
  , rejuvenateCommand
  , boolCommand
  , extensionCommand
  , impersonateCommand
  , noArgs False "gotham" $ \msg -> do
    restCall' $ DeleteMessage (messageChannel msg, messageId msg)
    void $ impersonateNameRandom (messageChannel msg) "gotham (-∞)"
  , oneArg False "how many" $ \m t -> do
    number :: Double <- liftIO normalIO <&> (exp . (+ 4) . (* 6))
    sendMessage (messageChannel m) $ show (round @_ @Integer number) <> " " <> t
  , oneArg False "ponder" $ \m t -> pontificate (messageChannel m) t
  , noArgs False "what is your latest dictum" $ const dictate
  , whereCommand


    -- admin commands
  , noArgs False "time for bed" $ const stopDict

    -- debug commands
  , noArgs False "kill them all" $ \m -> do
    npcs <- listNPC
    forM_ npcs deleteNPC
    sendReplyTo m "Consider what you've done."
  , noArgs False "clear webhook" $ \_ -> do
    modifyGlobal_ $ \g -> g & globalWebhook .~ Nothing

    -- We probably want these at the bottom!
  , whatCommand
  , whoCommand
  , whyCommand
  ]

handleCommand :: Message -> DictM Bool
handleCommand m = handleCommand' commands
-- Select the first matching command and short circuit.
 where
  handleCommand' [] = return False
  handleCommand' (Command { parser = commandParser, command = commandExec, isSpammy = _ } : cs)
    = case commandParser m of
      Just parsed -> commandExec m parsed >> return True
      Nothing     -> handleCommand' cs
