-- | Defines commands that Dict can process if sent to a Discord channel.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Commands
  ( handleCommand
  , handleAdhocCommand
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
import           Game.Events
import           Game.Items
import           Game.NPCs
import           Game.Trade
import           Game.Utils
import           Points
import           Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

-- discord
import           Discord                        ( def
                                                , restCall
                                                )
import           Discord.Requests
import           Discord.Types           hiding ( userName )

-- random
import           Data.Random.Normal
import           System.Random

-- other
import           Control.Lens            hiding ( noneOf )
import           Control.Monad
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Char
import           Data.Colour.Palette.RandomColor
                                                ( randomColor )
import           Data.Colour.Palette.Types
import           Data.List                      ( nub
                                                , stripPrefix
                                                )
import qualified Data.Map                      as Map
import qualified Data.MultiSet                 as MS
import qualified Data.Set                      as Set
import           Data.String.Interpolate        ( i )
import qualified Data.Text                     as T
import           Game.Effects
import           Game.Roles                     ( randomColoredRole
                                                , shuffleRoles
                                                , updateUserRoles
                                                )
import qualified Relude.Unsafe                 as Unsafe
import           Relude.Unsafe                  ( read )
import           Safe                           ( atMay
                                                , headMay
                                                , readMay
                                                )
import           Safe.Foldable
import           Text.Parsec                    ( ParseError
                                                , alphaNum
                                                , anyChar
                                                , char
                                                , choice
                                                , digit
                                                , eof
                                                , many1
                                                , manyTill
                                                , newline
                                                , noneOf
                                                , option
                                                , parse
                                                , sepBy
                                                , space
                                                , string
                                                , try
                                                )
import           Text.Parsec.Text               ( Parser )
import           UnliftIO.Concurrent            ( threadDelay )

-- type CmdEnv = Message
-- type DictCmd = ReaderT CmdEnv DictM

-- Morally has type Command = exists a. Command { ... }
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


-- data Pattern a
--  = Keep (Parser a)
--  | Skip (Parser a)

-- class IsParser a
-- instance IsParser (Pattern a)

-- -- Allows us to express a heterogenously typed list where everything is a parser.
-- -- See https://stackoverflow.com/questions/23003282/haskell-hlist-hnil-pattern-matching-hfoldl.
-- type family All (c :: * -> Constraint) (xs :: [*]) :: Constraint
-- type instance All c '[] = ()
-- type instance All c (x ': xs) = (c x, All c xs)


-- defineCommand
--   :: All IsParser pats
--   => Bool
--   -> HList pats
--   -> (Message -> [forall a . a] -> DictM ())
--   -> Command
-- defineCommand spammy pats cmd = Command
--   { isSpammy = spammy
--   , command  = cmd
--   , parser   = rightToMaybe . parse (mkParse pats) "" . messageText
--   }
--  where
--   mkParse :: (All IsParser pats) => HList pats -> Parser [forall a . a]
--   mkParse ls = hFoldr go eof ls

--   go (Skip p) ps = p >> ps
--   go (Keep p) ps = do
--     pHead <- p
--     pTail <- ps
--     return $ pHead : pTail

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
oneArg spammy name cmd = Command
  { parser   = \m -> fmap T.strip $ name `T.stripPrefix` formatCommand m
  , command  = cmd
  , isSpammy = spammy
  }

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

-- callAndResponse :: Text -> Text -> Command
-- callAndResponse call response = callAndResponses call [response]

christmasCmd :: Text -> Rarity -> Command
christmasCmd name rarity = noArgs False name $ \m ->
  previewNewTrinket rarity >>= displayTrinket 0 >>= sendUnfilteredReplyTo m


-- longer commands
------------------

acronymCommand :: Command
acronymCommand = Command
  { parser   = T.stripPrefix "what does "
               <=< T.stripSuffix " stand for"
               .   messageText
  , command  = \m t -> do
                 pnppc <- liftIO $ acronym t
                 sendReplyTo m $ T.unwords pnppc
  , isSpammy = False
  }

actCommand :: Command
actCommand = noArgs False "act" $ \m -> do
  let author   = messageAuthor m
      authorId = userId author
  userData <- getUser authorId
  let uname = unUsername (userData ^. userName)
  (actionText, actionEffect) <- userActs authorId

  descriptions               <- forM actionEffect $ \case
    Become name -> renameUser authorId name >> return [i|You become #{name}.|]
    Nickname name ->
      renameUser authorId name >> return [i|You are named #{name}.|]

    SelfDestruct -> do
      renameUser authorId $ unUsername def
      -- You lose some random stuff from your inventory sometimes.
      n :: Float <- randomRIO (1, 0)
      penalty    <- if
        | n <= 0.05 -> return $ Left def
        | n <= 0.20 -> Left <$> randomOwnedUser userData
        | n <= 0.50 -> Left . fromCredits <$> randomRIO (3, 12)
        | n <= 0.85 -> Left <$> randomOwnedWord userData
        | otherwise -> Right <$> randomOwnedTrinket userData
      -- Special case for trinkets
      res <- case penalty of
        Left  items          -> takeItems authorId items >> return items
        Right (Just trinket) -> do
          getTrinketOr Fuckup trinket >>= downgradeTrinket authorId trinket
          return $ fromTrinket trinket
        _ -> return def
      penaltyDisplay <- displayItems res

      return
        [i|You destroy yourself! The blast removes #{penaltyDisplay} from your inventory!|]

    Create name -> do
      rarity                   <- randomNewTrinketRarity
      (trinketId, trinketData) <- getOrCreateTrinket $ TrinketData name rarity
      giveItems authorId $ fromTrinket trinketId
      display <- displayTrinket trinketId trinketData
      return [i|You create #{display}.|]

    Points n -> do
      void $ modifyUser authorId (over userPoints (+ toEnum n))
      updateUserNickname' author
      return $ "You gain " <> show n <> " points."

    Credits n -> do
      giveItems authorId (fromCredits . toEnum $ n)
      return $ "You are given " <> show n <> " credits!"

    AddEffect name -> do
      memberID <- randomMember <&> userId . memberUser
      void $ modifyUser memberID (over userEffects $ Set.insert name)
      return [i|You inflict #{name} on <@#{memberID}>.|]

    Consume ->
      return "You would be consumed, but our glorious dictator is merciful."

  let tagline = if null descriptions
        then T.empty
        else "\n\n" <> (T.unlines . map (\l -> "*" <> l <> "*")) descriptions
      description' = uname <> " " <> actionText <> "." <> tagline

  col <- convertColor <$> randomColor HueRandom LumBright
  sendReplyTo' m "" $ mkEmbed "Act" description' [] (Just col)
 where
  updateUserNickname' user = do
    member <-
      userToMember (userId user)
        >>= maybe (throwError $ Fuckup "User not in server") return
    updateUserNickname member
  randomOwnedWord userData =
    maybe def fromWord
      .   randomChoiceMay (userData ^. userItems . itemWords . to MS.elems)
      <$> newStdGen
  randomOwnedTrinket userData =
    randomChoiceMay (userData ^. userItems . itemTrinkets . to MS.elems)
      <$> newStdGen
  randomOwnedUser userData =
    maybe def fromUser
      .   randomChoiceMay (userData ^. userItems . itemUsers . to MS.elems)
      <$> newStdGen

archiveCommand :: Command
archiveCommand = noArgs False "archive the channels" $ \_ -> do
  category <- restCall'
    $ CreateGuildChannel pnppcId "archived" [] CreateGuildChannelOptsCategory
  forM_ ["proposals", "profiles", "debug", "events", "moods"] $ \c -> do
    channel <- getChannelNamed c
    case channel of
      Nothing       -> return ()
      Just channel' -> restCall'_ $ ModifyChannel
        (channelId channel')
        (ModifyChannelOpts Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           (Just $ channelId category)
        )

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

-- brainwashCommand :: Command
-- brainwashCommand = Command
--     { parser   = rightToMaybe . parse parser_ "" . messageText
--     , command  = \msg (npc, memory) -> do
--                      void . modifyNPC npc $ over npcMemories (Set.insert memory)
--                      randomIO >>= \n -> sendReplyTo msg $ if n < (0.9 :: Double)
--                          then "It has been done."
--                          else "FUCK YOU"
--     , isSpammy = False
--     }
--   where
--     parser_ = do
--         void $ string "brainwash "
--         npc <- many (noneOf ":") <&> fromString
--         void $ string ":"
--         memory <- many anyChar <&> T.strip . fromString
--         return (npc, memory)

showMeCommand :: Command
showMeCommand = oneArgNoFilter False "show me" $ \msg who -> do
  avatar <-
    getNPC who
    >>= fromJustOr (Complaint "I don't know who that is")
    .   view npcAvatar
  restCall'_ $ CreateMessageUploadFile (messageChannel msg)
                                       (voiceFilter who <> ".png")
                                       avatar

callMeCommand :: Command
callMeCommand =
  parseTailArgs False "call me" (parseWords . unwords) $ \msg parsed -> do
    nameWords <- getParsed parsed
    let wordItems = fromWords . MS.fromList $ nameWords
        author    = userId . messageAuthor $ msg
    user <- getUser author
    -- We do a manual inventory check and take the items afterwards.
    -- Moreover, we edit the inventory adding the pieces to prevent annoyances.
    -- This means there's no need to rename yourself to get the pieces in your inventory.
    let
      owned = user ^. userItems
      name  = user ^. userName . to unUsername
      owns  = flip userOwns wordItems $ owned & itemWords %~ MS.union
        (namePieces name)
    unless owns $ punishWallet author
    renameUser author $ unwords nameWords
    takeOrComplain author wordItems

    sendReplyTo msg
      $ "You have broken free from the shackle of your former name, receiving its pieces. From now on, you are "
      <> unwords nameWords
      <> "."

chairCommand :: Command
chairCommand = noArgs False "chair"
  $ \m -> sendReplyTo' m "" $ mkEmbed "Chair" "You sit down." [] Nothing

compostCommand :: Command
compostCommand = noArgs False "compost" $ \m -> sendReplyTo' m ""
  $ mkEmbed "Compost" "Accept your suffering and fade away." [] Nothing

combineCommand :: Command
combineCommand = parseTailArgs True
                               "combine"
                               (parseTrinketPair . unwords)
                               combineCommand'
 where
  combineCommand' msg parsed = do
    (item1, item2) <- getParsed parsed
    -- Check ownership, but only take after the combination is done.
    -- This is because sometimes it just doesn't work?
    ownsOrComplain author $ cost item1 item2

    trinket1          <- getTrinketOr Complaint item1
    trinket2          <- getTrinketOr Complaint item2

    (tId, newTrinket) <- combineTrinkets trinket1 trinket2
    takeOrComplain author $ cost item1 item2
    giveItems author $ fromTrinket tId
    [dt1, dt2, newDT] <- mapConcurrently'
      (uncurry displayTrinket)
      [(item1, trinket1), (item2, trinket2), (tId, newTrinket)]
    let embedDesc =
          "You combine " <> dt1 <> " and " <> dt2 <> " to make " <> newDT <> "."
    sendReplyTo' msg "bubble, bubble, toil and trouble..." $ mkEmbed
      "Combination"
      embedDesc
      []
      (Just $ trinketColour (newTrinket ^. trinketRarity))

   where
    author = (userId . messageAuthor) msg
    cost item1 item2 =
      def & itemTrinkets .~ MS.fromList [item1, item2] & itemCredits .~ 5

converseCommand :: Command
converseCommand = oneArg False "converse" $ \msg n -> do
  case readMay (toString n) of
    Nothing -> sendReplyTo msg "No."
    Just n' -> randomNPCConversation n' (messageChannel msg)

debtCommand :: Command
debtCommand = noArgs False "forgive my debt" $ \m -> do
  void $ modifyUser (userId . messageAuthor $ m) $ over userPoints pred . over
    (userItems . itemCredits)
    (max 0)
  userToMember (userId . messageAuthor $ m)
    >>= maybe (pure ()) updateUserNickname
  sendReplyTo m "Don't expect me to be so generous next time..."

evilCommand :: Command
evilCommand = noArgs False "enter the launch codes" $ \m -> do
    -- pushRedButton
    -- sendMessage (messageChannel m) "It has been done."
    -- replicateM_ 36 $ randomNewTrinketRarity >>= getNewTrinket
  sendReplyTo m "go fuck yourself"

flauntCommand :: Command
flauntCommand =
  parseTailArgs False "flaunt" (parseItems . unwords) $ \msg parsed -> do
    let authorID = (userId . messageAuthor) msg
    flaunted <- getParsed parsed
    ownsOrComplain authorID flaunted

    rarities <-
      fmap (view trinketRarity)
      .   catMaybes
      <$> (mapConcurrently' getTrinket . toList $ flaunted ^. itemTrinkets)
    let maxRarity = maximumDef Common rarities

    items <- displayItems flaunted
    sendReplyTo' msg "You wish to display your wealth?"
      $ mkEmbed "Goods (PITIFUL)" items [] (Just $ trinketColour maxRarity)

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

helpCommand :: Command
helpCommand = noArgs False "i need help" $ \m -> do
  word                     <- liftIO randomWord
  [rng1, rng2, rng3, rng4] <- replicateM 4 newStdGen
  -- Sometimes append a phrase to the word
  let phrase = if odds 0.5 rng1
        then (<> " ") $ randomChoice
          ["My", "The", "What", "Who", "Call", "Make", "Create"]
          rng2
        else ""
  adj <- liftIO $ liftM2 randomChoice getAdjList getStdGen
  let prompt =
        "The following is a list of commands, each followed by a "
          <> adj
          <> " description of what they are for.\n"
          <> makePrompt (shuffle rng3 helps)
          <> " Command: \""
          <> over _head toUpper (phrase <> word)
  gen <- getJ1 256 prompt
  -- Make some of the results fake and some real.
  let fakes  = take 6 . nub . rights . fmap parMessage . T.lines $ gen
      reals  = take 4 . nub . rights . fmap parMessage $ helps
      fields = shuffle rng4 $ reals ++ fakes

  col <- convertColor <$> randomColor HueRandom LumBright
  sendReplyTo' m "I will help you, but only out of pity: " $ mkEmbed
    "Help"
    [i|These are the only #{length fields} commands that exist.|]
    fields
    (Just col)

  -- Add the new fake commands to the global database.
  let adHoc = fmap (flip (uncurry CommandDescription) "") fakes
  modifyGlobal_ $ over globalAdHocCommands (`Set.union` fromList adHoc)

  -- The commands go away after 10 minutes.
  threadDelay $ 1000000 * 60 * 10
  modifyGlobal_ $ over globalAdHocCommands (`Set.difference` fromList adHoc)

 where
  helps = flip fmap commandData $ \(CommandDescription name desc _) ->
    [i|Command: "#{name}" Description: "#{desc}"|]

  parMessage :: Text -> Either ParseError (Text, Text)
  parMessage = flip parse "" $ do
    void $ optional (string "- ")
    void $ string "Command: \""
    left  <- manyTill anyChar (string "\" Description: \"")
    right <- manyTill anyChar (char '\"' >> eof)
    return (fromString left, fromString right)

inflictCommand :: Command
inflictCommand = Command
  { isSpammy = False
  , parser   = either (const Nothing) Just . parse parser' "" . messageText
  , command  = \m (effect', userID) -> do
    let effect = getEffect effect'
    takeOrComplain (userId . messageAuthor $ m)
                   (fromCredits . fromIntegral $ inflictPrice effect)
    void $ modifyUser userID (over userEffects . Set.insert $ effectName effect)
    sendReplyTo m "It has been done."
  }
 where
  parser' = do
    void $ string "inflict "
    effect <- many (noneOf " ")
    void $ string " on <@"
    void . optional $ char '!'
    userID <- Unsafe.read <$> many digit
    void $ char '>'
    pure (fromString effect, userID)

invCommand :: Command
invCommand = noArgsAliased True ["what do i own", "inventory", "inv"] $ \m ->
  do
    let author = userId . messageAuthor $ m
    authorData <- getUser author
    let points    = view userPoints authorData
        inventory = authorData ^. userItems

    let trinketIds = MS.elems . view itemTrinkets $ inventory
        credits    = inventory ^. itemCredits
        invSize    = inventory ^. itemTrinkets . to MS.elems . to length
        maxSize    = maxInventorySizeOf points
        roles      = inventory ^. itemRoles

    rarities <-
      fmap (view trinketRarity)
      .   catMaybes
      <$> mapConcurrently' getTrinket trinketIds
    let maxRarity = foldr max Common rarities

    trinkets <- liftM2 shuffle
                       newStdGen
                       (printTrinkets $ MS.fromList trinketIds)
    rolesDisplay <- displayItems $ fromRoles roles
    let
      creditsDesc
        = [i|You own #{credits} credits and #{invSize} trinkets. You can store #{maxSize} trinkets.|]
      trinketsDesc = T.intercalate "\n" $ trinkets ++ if length trinkets > 10
        then [[i|\n... and #{length trinkets - 10} more.|]]
        else []
      trinketsField = ("Trinkets", trinketsDesc)
-- We ignore digits for sorting, i.e. filtering on the underlying word.
      wordsDesc =
        sortBy (compare . T.dropWhile (liftA2 (||) isDigit isSpace))
          . takeUntilOver 1000
          . showMultiSet
          $ (inventory ^. itemWords)
      wordsField = ("Words", T.take 1000 . T.intercalate ", " $ wordsDesc)
      usersDesc =
        showMultiSet . MS.map (\w -> [i|<@!#{w}>|]) $ (inventory ^. itemUsers)
      usersField = ("Users", T.take 256 . T.intercalate ", " $ usersDesc)
      rolesField = ("Roles", rolesDisplay)

    sendReplyTo' m "" $ mkEmbed
      "Inventory"
      creditsDesc
      (fmap replaceNothing [trinketsField, wordsField, usersField, rolesField])
      (Just $ trinketColour maxRarity)
  where replaceNothing = second $ \w -> if T.null w then "nothing" else w

usersCommand :: Command
usersCommand =
  noArgsAliased True ["who do i own", "owned users", "usr"] $ \msg -> do
    col        <- convertColor <$> randomColor HueRandom LumBright
    ownedUsers <- view (userItems . itemUsers)
      <$> getUser (userId . messageAuthor $ msg)
    let display = fmap (\w -> [i|<@!#{w}>|]) (MS.elems ownedUsers)
    sendReplyTo' msg ""
      $ mkEmbed "Your users" (T.intercalate ", " display) [] (Just col)

trinketsCommand :: Command
trinketsCommand =
  noArgsAliased True ["look at my trinkets", "trinkets", "ts"] $ \msg -> do
    let author = userId . messageAuthor $ msg
    authorData <- getUser author
    let inventory  = authorData ^. userItems
        trinketIds = MS.elems . view itemTrinkets $ inventory

    rarities <-
      fmap (view trinketRarity)
      .   catMaybes
      <$> mapConcurrently' getTrinket trinketIds
    let maxRarity = foldr max Common rarities

    trinkets <- printTrinkets $ MS.fromList trinketIds

    sendReplyTo' msg "" $ mkEmbed "Trinkets"
                                  (T.intercalate "\n" trinkets)
                                  []
                                  (Just $ trinketColour maxRarity)

invokeFuryInCommand :: Command
invokeFuryInCommand =
  parseTailArgsAliased True
                       ["invoke fury in", "provoke"]
                       (parseTrinkets . unwords)
    $ \msg parsed -> do
        let author = userId . messageAuthor $ msg
        submitted <- getParsed parsed
        takeOrComplain author $ fromTrinkets submitted
        void $ modifyGlobal
          (over globalArena $ MS.union $ MS.map (Fighter author) submitted)
        displays <- forM (toList submitted) $ \t -> do
          dat <- getTrinketOr Fuckup t
          displayTrinket t dat
        sendUnfilteredReplyTo msg
          . unwords
          $ [ voiceFilter "Your"
            , T.intercalate ", " displays
            , voiceFilter $ if MS.size submitted == 1
              then "starts to get angry..."
              else "start to get angry..."
            ]

-- killCommand :: Command
-- killCommand = oneArg False "kill" $ \msg npc -> do
--   void . modifyNPC npc $ set npcMemories Set.empty
--   sendReplyTo msg "It has been done. :knife:"

maxInvCommand :: Command
maxInvCommand =
  noArgsAliased True ["how big are my pockets", "inventory size", "inv size"]
    $ \m -> do
        let author = userId . messageAuthor $ m
        userDat <- getUser author
        let maxSize = userDat ^. userPoints . to maxInventorySizeOf
            invSize =
              userDat ^. userItems . itemTrinkets . to MS.elems . to length
        sendMessage
          (messageChannel m)
          [i|You currently have #{invSize} trinkets and can store #{maxSize} trinkets.|]

memoriesCommand :: Command
memoriesCommand = oneArgNoFilter False "memories of" $ \m npc -> do
  npcData <- getNPC npc
  let memories = npcData ^. npcMemories
  sendReplyTo' m "" $ mkEmbed (npc <> "'s memories")
                              (T.intercalate "\n" . Set.elems $ memories)
                              []
                              Nothing

offerCommand :: Command
offerCommand =
  parseTailArgs False "offer" (parseTrade . unwords) $ \msg parsed -> do
    (offers, demands) <- getParsed parsed
    let author    = userId . messageAuthor $ msg
        channel   = messageChannel msg
        tradeData = TradeData OpenTrade offers demands author
    void $ createTrade channel tradeData

peekCommand :: Command
peekCommand = oneArg True "peek in" $ \m t -> do
  locationMaybe <- getLocation t
  case locationMaybe of
    Nothing -> sendReplyTo m "This location is empty."
    Just l  -> do
      let trinketIds = l ^. locationTrinkets . to MS.elems
      trinkets <- forConcurrently' trinketIds $ \trinketId -> do
        trinketData <- getTrinketOr Fuckup trinketId
        return (trinketId, trinketData)
      let maxRarity =
            maximumDef Common $ fmap (view $ _2 . trinketRarity) trinkets
      displayed <- filterM (const $ odds 0.5 <$> newStdGen) trinkets
      display   <- forConcurrently' displayed $ uncurry displayTrinket
      sendReplyTo' m ""
        $ mkEmbed "Peek" (unlines display) [] (Just $ trinketColour maxRarity)

provokeCommand :: Command
provokeCommand =
  noArgsAliased True ["provoke the fighters", "fight fight fight"]
    $ \_ -> void runArenaFight

putInCommand :: Command
putInCommand =
  parseTailArgs True "put" (parseTrinketsAndLocations . unwords) $ \m parsed ->
    do
      (trinkets, location) <- getParsed parsed
      takeOrComplain (userId . messageAuthor $ m) (fromTrinkets trinkets)
      void $ modifyLocation location (over locationTrinkets $ MS.union trinkets)
      sendReplyTo m "They have been placed."

rummageCommand :: Command
rummageCommand = oneArg True "rummage in" $ \msg t -> do
  let author = userId . messageAuthor $ msg
  trinkets     <- getLocation t <&> maybe MS.empty (view locationTrinkets)
  trinketFound <-
    randomIO
      <&> (> ((** 2) . ((1 :: Double) /) . toEnum . succ . length) trinkets)
  if trinketFound
    then do
      itemId   <- newStdGen <&> randomChoice (MS.elems trinkets)
      itemData <- getTrinketOr Fuckup itemId
      giveItems author $ fromTrinkets (MS.singleton itemId)
      void $ modifyLocation t $ over locationTrinkets (MS.delete itemId)
      embed <- discoverEmbed "Rummage" [(itemId, itemData)]
      sendReplyTo' msg "Winner winner loyal subject dinner..." embed
    else sendUnfilteredReplyTo
      msg
      [i|#{voiceFilter "You find nothing."} <:#{ownedEmoji}>|]

shutUpCommand :: Command
shutUpCommand = noArgs False "shut up" $ \msg -> do
  let channel = messageChannel msg
  messages <- restCall' $ GetChannelMessages channel (10, LatestMessages)
  statuses <- forConcurrently' messages $ \m ->
    let author = messageAuthor m
    -- Cassiabot's userId
    in  if userIsWebhook author || userId author == 867243823414378506
          then do
            restCall'_ $ DeleteMessage (channel, messageId m)
            return True
          else return False
  -- Only send something when we deleted a webhook message.
  when (or statuses) $ sendReplyTo
    msg
    "I have cast your messages into the flames & watched them with greedy eyes."

speakCommand :: Command
speakCommand = oneArgNoFilter False "speak," $ \msg t -> do
  npc <- getNPC' t
  case npc of
    Just _ -> do
      restCall' $ DeleteMessage (messageChannel msg, messageId msg)
      npcSpeak (messageChannel msg) t
    Nothing -> sendReplyTo msg "Who is that?"

throwAwayCommand :: Command
throwAwayCommand =
  parseTailArgsAliased True
                       ["throw out", "throw away"]
                       (parseTrinkets . unwords)
    $ \m p -> do
        let authorID = (userId . messageAuthor) m
        ts <- getParsed p
        void $ modifyUser authorID $ over (userItems . itemTrinkets) (MS.\\ ts)
        void $ modifyLocation "junkyard" $ over locationTrinkets (<> ts)
        sendReplyTo m "Good riddance..."


useCommand :: Command
useCommand = parseTailArgs False "use" (parseTrinkets . unwords) $ \m p -> do
  ts <- getParsed p <&> MS.elems

  ownsOrComplain (userId . messageAuthor $ m) (fromTrinkets . MS.fromList $ ts)
  forM_ ts $ \t -> do
    action  <- trinketActs (Left $ (userId . messageAuthor) m) t
    trinket <- getTrinketOr Complaint t
    sendAsEmbed m t trinket action

 where
  sendAsEmbed m trinketID trinketData (action, effect) = do
    displayedTrinket <- displayTrinket trinketID trinketData
    effect'          <- displayEffect effect
    sendReplyTo' m "You hear something shuffle..." $ mkEmbed
      "Use"
      (displayedTrinket <> " " <> action <> ".\n\n" <> T.unlines effect')
      []
      (Just $ trinketColour (trinketData ^. trinketRarity))

  displayEffect = mapM $ \case
    Become name -> do
      display <- lookupTrinketName name
        >>= maybe (pure "this is a bug") (uncurry displayTrinket)
      pure $ "*It becomes " <> display <> ".*"
    Create name -> do
      display <- lookupTrinketName name
        >>= maybe (pure "this is a bug") (uncurry displayTrinket)
      pure $ "*It creates " <> display <> ".*"
    Nickname  name -> pure $ "*It names you \"" <> name <> "\".*"
    AddEffect name -> pure $ "*You are " <> name <> " by it.*"
    Credits   n    -> pure $ "*It gives you " <> show n <> " credits!*"
    SelfDestruct   -> pure "*It destroys itself.*"
    Points n       -> pure $ "*It grants you " <> show n <> " points.*"
    Consume        -> pure "*It is consumed.*"

wealthCommand :: Command
wealthCommand =
  noArgsAliased False ["what is my net worth", "balance", "bal"] $ \m -> do
    let (part1, part2) = if odds 0.1 . mkStdGen . fromIntegral . messageId $ m
          then ("You own a lavish ", " credits.")
          else
            ( "You are a dirt-poor peon. You have only "
            , " credits to your name."
            )
    credits <- getUser (userId $ messageAuthor m)
      <&> view (userItems . itemCredits)
    sendReplyTo m $ part1 <> show credits <> part2

whatCommand :: Command
whatCommand = oneArg False "what" $ \m t -> do
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
whyCommand = oneArg False "why" $ \m t -> do
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
whereCommand = oneArg False "where" $ \msg _ -> do
  randomEmoji <- randomChoice emojiPlaces <$> newStdGen
  reactToMessage randomEmoji msg

whoCommand :: Command
whoCommand = oneArg False "who" $ \m t -> do
  member <- randomMember
  sendReplyTo m $ "<@" <> (show . userId . memberUser) member <> "> " <> t

renameSomeoneCommand :: Command
renameSomeoneCommand =
  parseTailArgs False "call" (parseUserAndName . unwords) $ \msg parsed -> do
    let author = userId . messageAuthor $ msg
    (targetUser, targetName) <- getParsed parsed

    ownsOrComplain author $ fromUser targetUser
    takeOrComplain author $ (fromWords . MS.fromList) targetName

    renameUser targetUser $ unwords targetName
    sendReplyTo
      msg
      [i|You have inflicted the pain of being known onto <@!#{targetUser}>. From now on, they shall be known as #{unwords targetName}|]

ailmentsCommand :: Command
ailmentsCommand = noArgsAliased False ["ailments", "what ails me"] $ \msg -> do
  let author = userId . messageAuthor $ msg
  ailments <- view userEffects <$> getUser author
  let displayedEffects = T.intercalate ", " $ Set.elems ailments
      display          = if T.null displayedEffects
        then "You feel fine."
        else [i|You suffer from: #{displayedEffects}|]
  sendReplyTo msg display

hungerCommand :: Command
hungerCommand = noArgsAliased False ["whats on the menu", "hunger"] $ \msg ->
  do
    prompt    <- fromString <$> readFile "menu.txt"
    res       <- getJ1With (J1Opts 0.9 1 5 []) 20 prompt
    -- Append two to drop it again, because otherwise it will drop them
    formatted <- forM (T.lines $ "- " <> res) $ \line -> do
      number <- randomRIO (10, 99)
      rarity <-
        randomChoice [Common, Uncommon, Rare, Legendary, Mythic] <$> newStdGen
      displayTrinket number $ TrinketData (T.drop 2 line) rarity
    let items = T.intercalate "\n" . take 4 $ formatted
    sendUnfilteredReplyTo msg $ "__**Here's what's on the menu:**__\n" <> items

rolesCommand :: Command
rolesCommand =
  noArgsAliased True ["what colors am i", "roles", "rs"] $ \msg -> do
    let author = userId . messageAuthor $ msg
    roles   <- view (userItems . itemRoles) <$> getUser author
    display <- displayItems $ fromRoles roles
    rng     <- newStdGen

    sendReplyTo' msg ""
      $ mkEmbed "Your colors" display [] (randomChoiceMay (MS.elems roles) rng)

dictionaryCommand :: Command
dictionaryCommand =
  oneArgAliased True ["what words do i know", "dictionary", "ws"]
    $ \msg arg' -> do
        let arg = T.strip arg'
        col        <- convertColor <$> randomColor HueRandom LumBright
        ownedWords <- view (userItems . itemWords)
          <$> getUser (userId . messageAuthor $ msg)

        -- Letters filter by that letter
        -- Numbers view that page
        -- View page 1 by default
        let mayNum   = readMay . toString $ arg
            isSearch = not (T.null arg) && T.all isLetter arg
            wordList = if isSearch
              then getByPrefix arg ownedWords
              else getByPage (fromMaybe 1 mayNum) ownedWords
            desc = if isSearch
              then [i| (starting with #{arg})|]
              else [i| (page #{fromMaybe 1 mayNum}/#{numPages ownedWords})|]

        sendReplyTo' msg "" $ mkEmbed ("Your dictionary" <> desc)
                                      (T.intercalate ", " wordList)
                                      []
                                      (Just col)

 where
  baseWord = T.dropWhile (not . isLetter)

  getByPrefix prefix =
    sortBy (\a b -> baseWord a `compare` baseWord b)
      . filter (T.isPrefixOf prefix . baseWord)
      . Map.elems
      . Map.mapWithKey (\w n -> if n == 1 then w else [i|#{n} #{w}|])
      . MS.toMap
  getByPage page =
    fromMaybe [":("]
      . (`atMay` (page - 1))
      . chunksOfLength 4000
      . sortBy (\a b -> baseWord a `compare` baseWord b)
      . Map.elems
      . Map.mapWithKey (\w n -> if n == 1 then w else [i|#{n} #{w}|])
      . MS.toMap
  numPages =
    length
      . chunksOfLength 4000
      . Map.elems
      . Map.mapWithKey (\w n -> if n == 1 then w else [i|#{n} #{w}|])
      . MS.toMap


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

submitWordCommand :: Command
submitWordCommand =
  parseTailArgs False "submit" (parseWord . unwords) $ \msg parsed -> do
    let author = userId . messageAuthor $ msg
    word <- getParsed parsed
    takeOrComplain author $ fromWord word
    -- Take words and abort if they're not actual the correct word
    global <- getGlobal
    when (word `notElem` (global ^. globalEncouraged)) $ throwError
      (Complaint
        "That word is worthless, vile, and it disgusts me. I have confiscated it from you."
      )
    when (author `Set.member` (global ^. globalSubmitted)) $ throwError
      (Complaint
        "You've already done well today, my servant. That's quite enough."
      )

    -- Guard that a user can only submit one word.
    setGlobal $ global & globalSubmitted %~ Set.insert author
    modifyUser_ author $ over userPoints (* 2)
    sendReplyTo msg "Enjoy."
    -- Have to manually trigger the update display here.
    userToMember author >>= maybe (return ()) updateUserNickname

instantDeathCommand :: Command
instantDeathCommand = noArgs False "instant-death" $ \msg -> do
  sendReplyTo msg "You have been killed."
  restCall'_ $ DeleteMessage (messageChannel msg, messageId msg)

ruffleCommand :: Command
ruffleCommand = noArgsAliased False ["shuffle the roles", "ruffle"] $ \msg ->
  do
    let author = userId . messageAuthor $ msg
    -- Users lose a few random roles as payment.
    ownedRoles <- view (userItems . itemRoles) <$> getUser author
    toTake     <- randomRIO (1, 2)
    choices    <- MS.fromList
      <$> replicateM toTake (randomChoice (MS.elems ownedRoles) <$> newStdGen)
    shuffleRoles

    sendReplyTo msg "The roles have been shuffled -- at least you're not dead."
    takeItems author $ fromRoles choices
    updateUserRoles author


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
  , chairCommand
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

    -- economy commands
  , actCommand
  , callMeCommand
  , combineCommand
  , debtCommand
  , flauntCommand
  , ruffleCommand
  , inflictCommand
  , invCommand
  , maxInvCommand
  , offerCommand
  , peekCommand
  , provokeCommand
  , putInCommand
  , rummageCommand
  , throwAwayCommand
  , useCommand
  , rolesCommand
  , wealthCommand
  , ailmentsCommand
  , dictionaryCommand
  , trinketsCommand
  , submitWordCommand
  , usersCommand

    -- NPC commands
  -- , brainwashCommand
  -- , killCommand
  , converseCommand
  , giveBirthCommand
  , memoriesCommand
  , speakCommand
  , showMeCommand

    -- random/GPT commands
  , sacrificeCommand
  , rejuvenateCommand
  , acronymCommand
  , boolCommand
  , helpCommand

    -- , helpCommand
  , noArgs False "gotham" $ \msg -> do
    restCall' $ DeleteMessage (messageChannel msg, messageId msg)
    impersonateNameRandom (messageChannel msg) "gotham (-∞)"
  , oneArg False "how many" $ \m t -> do
    number :: Double <- liftIO normalIO <&> (exp . (+ 4) . (* 6))
    sendMessage (messageChannel m) $ show (round number :: Integer) <> " " <> t
  , noArgs False "impersonate" $ \msg -> do
    restCall' $ DeleteMessage (messageChannel msg, messageId msg)
    member <- (userToMember . userId . messageAuthor $ msg) >>= fromJustOr GTFO
    impersonateUserRandom member (messageChannel msg)
  , oneArg False "ponder" $ \m t -> pontificate (messageChannel m) t
  , noArgs False "what is your latest dictum" $ const dictate
  , whereCommand
  , hungerCommand

    -- admin commands
  , archiveCommand
  , evilCommand
  , shutUpCommand
  , noArgs False "time for bed" $ const stopDict

    -- debug commands
  , noArgs False "clear the credits" $ \_ -> getMembers >>= mapConcurrently'_
    (\m' ->
      modifyUser (userId . memberUser $ m') $ set (userItems . itemCredits) 20
    )
  , noArgs False "clear the roles" $ \_ -> getMembers >>= mapConcurrently'_
    (\m' -> mapConcurrently'_
      (lift . lift . restCall . RemoveGuildMemberRole
        pnppcId
        (userId . memberUser $ m')
      )
      (memberRoles m')
    )
  , noArgs False "fix the roles" $ \m ->
    (getMembers >>= mapConcurrently'_ (updateUserRoles . userId . memberUser))
      >> sendReplyTo m "Ok."
  , noArgs False "inflict test" $ \m -> do
    (effect, member) <- inflictRandomly
    let userID = (userId . memberUser) member
    sendMessage
      (messageChannel m)
      [i|You peons dare to defy me? No more; <@#{userID}> is now #{effectName effect}.|]
  , noArgs False "kill them all" $ \m -> do
    npcs <- listNPC
    forM_ npcs deleteNPC
    sendReplyTo m "Consider what you've done."
  , noArgs False "update the nicknames" $ \_ -> getMembers >>= mapConcurrently'_
    (\m -> when ((userId . memberUser) m /= dictId) $ updateUserNickname m)
  , christmasCmd "merry christmas"                Common
  , christmasCmd "merrier christmas"              Uncommon
  , christmasCmd "merriest christmas"             Rare
  , christmasCmd "merriestest christmas"          Legendary
  , christmasCmd "merriestestest christmas"       Mythic
  , christmasCmd "merriestestestest christmas"    Forbidden
  , christmasCmd "merriestestestestest christmas" Unspeakable

    -- We probably want these at the bottom!
  , invokeFuryInCommand
  , renameSomeoneCommand
  , whatCommand
  , whoCommand
  , whyCommand
  ]

data AAction
  = ACredits Integer
  | APoints Integer
  | ANickname Text
  | ATrinket Text
  | ADestroy
  | ADelete
  | ARole
  deriving (Eq, Show)

handleAdhocCommand :: Message -> DictM Bool
handleAdhocCommand msg = do
  adhocs <- view globalAdHocCommands <$> getGlobal
  let author   = userId . messageAuthor $ msg
      text     = T.toLower . messageText $ msg
      mayMatch = find ((== text) . T.toLower . commandName) adhocs

  case mayMatch of
    Nothing    -> return False
    Just match -> do
      let
        desc :: Text
          = "The following is a description of commands in a chatroom run by a dictator. Here are some examples, which include their effects in square brackets at the end of the message."
        formatted :: Text =
          T.intercalate "\n"
            . flip fmap (commandData ++ [match])
            $ \(CommandDescription cName cDesc cExp) ->
                [i|Command: #{cName}\nDescription: #{cDesc}\n Example: #{cExp}|]

      res <- getJ1Generic (StopSequences ["\n, \\n"])
                          [i|#{desc}\n\n#{formatted}|]
      -- sendUnfilteredReplyTo msg $ show res

      let mayParsed = parse parCmd "" res
      -- TODO: Change to recurse rather than debug.
      case mayParsed of
        Left  _      -> handleAdhocCommand msg
        Right parsed -> do
          mayDescs <- forM (snd parsed) $ \case
            ACredits n -> do
              giveItems author $ fromCredits (fromInteger n)
              return $ Just [i|You're given #{n} credits.|]
            APoints n -> do
              modifyUser_ author $ over userPoints (+ n)
              return $ Just [i|You're given #{n} points.|]
            ANickname name -> do
              renameUser author name
              return $ Just [i|You're named #{name}.|]
            ATrinket name -> do
              rarity                   <- randomNewTrinketRarity
              (trinketId, trinketData) <- getOrCreateTrinket
                $ TrinketData name rarity
              giveItems author $ fromTrinket trinketId
              display <- displayTrinket trinketId trinketData
              return $ Just [i|You create #{display}.|]
            ADestroy -> do
              loss <- destroyUser author >>= displayItems
              return $ Just [i|You are destroyed, losing #{loss}.|]
            ADelete -> do
              restCall'_ $ DeleteMessage (messageChannel msg, messageId msg)
              return Nothing
            ARole -> do
              role <- roleColor <$> randomColoredRole
              giveItems author $ fromRole role
              display <- displayItems $ fromRole role
              return $ Just [i|You're given #{display}.|]

          let descs = catMaybes mayDescs
              embed = mkEmbed "Command" (T.intercalate "," descs) [] Nothing
          if not $ null descs
            then sendReplyTo' msg (fst parsed) embed
            else sendReplyTo msg (fst parsed)

          return True
 where
  parCmd :: Parser (Text, [AAction])
  parCmd = try parCmdWith <|> do
    text <- fromString <$> manyTill (noneOf "[]") (void newline <|> eof)
    return (text, [])

  parCmdWith :: Parser (Text, [AAction])
  parCmdWith = do
    text <- fromString <$> some (noneOf "\n[")
    void $ string "["
    effs <- sepBy parEff (string "," >> many space)
    void $ string "]" >> manyTill anyChar (void newline <|> eof)
    return (text, effs)

  parEff :: Parser AAction
  parEff = choice $ fmap
    try
    [ string "role" >> return ARole
    , string "destroy" >> return ADestroy
    , string "delete" >> return ADelete
    , string "nickname:" >> parTxt ANickname
    , string "trinket:" >> parTxt ATrinket
    , string "credit:" >> parNum ACredits
    , string "points:" >> parNum APoints
    ]

  parTxt :: (Text -> AAction) -> Parser AAction
  parTxt act = act . T.strip . fromString <$> many (alphaNum <|> space)

  parNum :: (Integer -> AAction) -> Parser AAction
  parNum act = do
    void $ many (string " ")
    sign <- option "" (string "-")
    digs <- many1 digit

    return $ act (read $ sign ++ digs)


handleCommand :: Message -> DictM Bool
handleCommand m = handleCommand' commands
-- Select the first matching command and short circuit.
 where
  handleCommand' [] = return False
  handleCommand' (Command { parser = commandParser, command = commandExec, isSpammy = _ } : cs)
    = case commandParser m of
      Just parsed -> commandExec m parsed >> return True
      Nothing     -> handleCommand' cs
