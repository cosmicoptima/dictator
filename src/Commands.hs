-- | Defines commands that Dict can process if sent to a Discord channel.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Commands
    ( handleCommand
    , Err(..)
    ) where

import           Relude                  hiding ( First )
import           Relude.Unsafe                  ( fromJust )

import           Events
import           Game
import           Game.Data
import           Game.Events
import           Game.Items
import           Utils
import           Utils.DictM
import           Utils.Discord
import           Utils.Language

import           Discord                        ( def
                                                , restCall
                                                )
import           Discord.Requests
import           Discord.Types

import           Data.Random.Normal
import           System.Random
import           System.Random.Shuffle          ( shuffle' )

import           Control.Lens            hiding ( noneOf )
import           Control.Monad                  ( liftM2 )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Char
import           Data.Foldable                  ( maximum )
import           Data.List                      ( stripPrefix )
import qualified Data.MultiSet                 as MS
import           Data.MultiSet                  ( MultiSet )
import qualified Data.Text                     as T
import qualified Database.Redis                as DB
import           Text.Parsec


-- Morally has type Command = exists a. Command { ... }
-- Existential types in Haskell have a strange syntax!
data Command = forall a . Command
    { parser   :: Message -> Maybe a
    , command  :: DB.Connection -> Message -> a -> DictM ()
    , isSpammy :: Bool
    }

commandWords :: Message -> [Text]
commandWords = T.words . T.strip . T.toLower . stripRight . messageText
    where stripRight = T.reverse . T.dropWhile isPunctuation . T.reverse

-- command builders
-------------------

-- | Matches a specific name and nothing more.
noArgs :: Bool -> Text -> (DB.Connection -> Message -> DictM ()) -> Command
noArgs spammy pat cmd = Command
    { parser   = \m -> if pat == messageText m then Just () else Nothing
    , command  = \c m _ -> cmd c m
    , isSpammy = spammy
    }

-- | Matches a specific name on the head of the message and returns the tail.
tailArgs
    :: Bool
    -> [Text]
    -> (DB.Connection -> Message -> [Text] -> DictM ())
    -> Command
tailArgs spammy cmd = parseTailArgs spammy cmd id

oneArg
    :: Bool -> Text -> (DB.Connection -> Message -> Text -> DictM ()) -> Command
oneArg spammy name cmd =
    tailArgs spammy (words name) (\c m -> cmd c m . unwords)

-- | Matches a specific name on the head of the message a transformation (likely a parser) to the tail.
parseTailArgs
    :: Bool
    -> [Text]
    -> ([Text] -> a)
    -> (DB.Connection -> Message -> a -> DictM ())
    -> Command
parseTailArgs spammy pat trans cmd = Command
    { parser   = \m -> case stripPrefix pat . commandWords $ m of
                     Just cmdTail -> Just $ trans cmdTail
                     Nothing      -> Nothing
    , command  = cmd
    , isSpammy = spammy
    }

callAndResponses :: Text -> [Text] -> Command
callAndResponses call responses = noArgs False call $ \_ m ->
    newStdGen >>= sendMessage (messageChannel m) . randomChoice responses

callAndResponse :: Text -> Text -> Command
callAndResponse call response = callAndResponses call [response]

christmasCmd :: Text -> Rarity -> Command
christmasCmd name rarity = noArgs False name $ \c m ->
    previewNewTrinket c rarity >>= displayTrinket 0 >>= sendUnfilteredMessage
        (messageChannel m)


-- longer commands
------------------

acronymCommand :: Command
acronymCommand = Command
    { parser   = T.stripPrefix "what does "
                 <=< T.stripSuffix " stand for"
                 .   messageText
    , command  = \_ m t -> do
                     pnppc <- liftIO $ acronym t
                     sendMessage (messageChannel m) $ T.unwords pnppc
    , isSpammy = False
    }

archiveCommand :: Command
archiveCommand = noArgs False "archive the channels" $ \_ _ -> do
    category <- restCall' $ CreateGuildChannel
        pnppcId
        "archived"
        []
        CreateGuildChannelOptsCategory
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

arenaCommand :: Command
arenaCommand = noArgs True "fight fight fight" $ \c m -> do
    let authorID = userId . messageAuthor $ m

    rng         <- newStdGen
    myTrinketId <-
        getUserOr Fuckup c authorID
        <&> flip randomChoice rng
        .   MS.elems
        .   view userTrinkets
    myTrinketData <- getTrinketOr Fuckup c myTrinketId

    arenaStatus'  <- getGlobal c <&> view globalAdhocFighter
    case arenaStatus' of
        Nothing -> do
            void . modifyGlobal c $ set
                globalAdhocFighter
                (Just $ Fighter authorID myTrinketId)
            displayedTrinket <- displayTrinket myTrinketId myTrinketData
            sendUnfilteredMessage (messageChannel m)
                $  voiceFilter "Your"
                <> " "
                <> displayedTrinket
                <> " "
                <> voiceFilter "prepares to fight..."
        Just (Fighter theirUserId theirTrinketId) -> do
            void . modifyGlobal c $ set globalAdhocFighter Nothing
            theirTrinketData          <- getTrinketOr Fuckup c theirTrinketId
            FightData theyWon details <- fightTrinkets theirTrinketData
                                                       myTrinketData
                                                       Nothing
            let
                (winnerUserId, loserUserId, winnerTrinketData, loserTrinketId)
                    = if theyWon
                        then
                            ( theirUserId
                            , authorID
                            , theirTrinketData
                            , myTrinketId
                            )
                        else
                            ( authorID
                            , theirUserId
                            , myTrinketData
                            , theirTrinketId
                            )


            void . modifyUser c winnerUserId $ over
                userCredits
                (+ winnerTrinketData ^. trinketRarity . to trinketRewards)
            void . modifyUser c loserUserId $ over
                userTrinkets
                (MS.delete loserTrinketId)

            myDisplay    <- displayTrinket myTrinketId myTrinketData
            theirDisplay <- displayTrinket theirTrinketId theirTrinketData
            let winnerDisplay = if theyWon then theirDisplay else myDisplay

            let embedDesc =
                    theirDisplay
                        <> " (<@"
                        <> show theirUserId
                        <> ">) and "
                        <> myDisplay
                        <> " (<@"
                        <> show authorID
                        <> ">) fight...\n\n"
                        <> winnerDisplay
                        <> " (<@"
                        <> show winnerUserId
                        <> ">) wins! "
                        <> details
                        <> "."
            void
                . restCall'
                $ CreateMessageEmbed (messageChannel m) ""
                $ mkEmbed
                      "Arena"
                      embedDesc
                      []
                      (  Just
                      $  winnerTrinketData
                      ^. trinketRarity
                      .  to trinketColour
                      )

boolCommand :: Command
boolCommand = tailArgs False ["is"] $ \_ m _ -> do
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
                8
                "Here are a few examples of a dictator's response to a simple yes/no question"
                examples
            sendMessage channel $ case lines output of
                (l : _) -> l
                []      -> "idk"

flauntCommand :: Command
flauntCommand =
    parseTailArgs False ["flaunt"] (parseTrinkets . unwords)
        $ \conn msg parsed -> do
              let authorID = (userId . messageAuthor) msg
                  channel  = messageChannel msg
              flauntedTrinkets <- getParsed parsed
              userData         <- getUser conn authorID <&> fromMaybe def
              if userOwns userData $ fromTrinkets flauntedTrinkets
                  then do
                      rarities <-
                          fmap (view trinketRarity)
                          .   catMaybes
                          <$> ( mapConcurrently' (getTrinket conn)
                              . toList
                              $ flauntedTrinkets
                              )
                      let maxRarity = foldr max Common rarities
                      trinkets <- printTrinkets conn flauntedTrinkets
                      let display = T.intercalate "\n" trinkets
                      void
                          . restCall'
                          . CreateMessageEmbed
                                channel
                                (voiceFilter "You wish to display your wealth?")
                          $ mkEmbed "Goods (PITIFUL)"
                                    display
                                    []
                                    (Just $ trinketColour maxRarity)
                  else do
                      sendMessage
                          channel
                          "You don't own the goods you so shamelessly try to flaunt, and now you own even less. Credits, that is."
                      void $ modifyUser conn authorID (over userCredits pred)

combineCommand :: Command
combineCommand = parseTailArgs True
                               ["combine"]
                               (parseTrinketPair . unwords)
                               combineCommand'
  where
    combineCommand' conn msg parsed = do
        (item1, item2) <- getParsed parsed
        -- Check ownership, but only take after the combination is done.
        -- This is because sometimes it just doesn't work?
        ownsOrComplain conn author $ cost item1 item2

        trinket1          <- getTrinketOr Complaint conn item1
        trinket2          <- getTrinketOr Complaint conn item2

        (tId, newTrinket) <- combineTrinkets conn trinket1 trinket2
        takeOrComplain conn author $ cost item1 item2
        giveItems conn author $ (fromTrinkets . MS.fromList) [tId]
        [dt1, dt2, newDT] <- mapConcurrently'
            (uncurry displayTrinket)
            [(item1, trinket1), (item2, trinket2), (tId, newTrinket)]
        let embedDesc =
                "You combine "
                    <> dt1
                    <> " and "
                    <> dt2
                    <> " to make "
                    <> newDT
                    <> "."
        void
            . restCall'
            . CreateMessageEmbed
                  channel
                  (voiceFilter "bubble, bubble, toil and trouble...")
            $ mkEmbed "Combination"
                      embedDesc
                      []
                      (Just $ trinketColour (newTrinket ^. trinketRarity))

      where
        author  = (userId . messageAuthor) msg
        channel = messageChannel msg
        cost item1 item2 =
            def & itemTrinkets .~ MS.fromList [item1, item2] & itemCredits .~ 5

makeFightCommand :: Command
makeFightCommand =
    parseTailArgs True ["make"] (parse parseFightCommand "" . unwords)
        $ \conn msg parsed -> do
              let author  = userId . messageAuthor $ msg
                  channel = messageChannel msg
              (t1, t2) <- getParsed parsed
              ownsOrComplain conn author $ (fromTrinkets . MS.fromList) [t1]
              attacker  <- getTrinketOr Complaint conn t1
              defender  <- getTrinketOr Complaint conn t2
              fightData <- fightTrinkets attacker defender Nothing
              embed     <- fightEmbed (t1, attacker) (t2, defender) fightData
              restCall'_ $ CreateMessageEmbed
                  channel
                  (voiceFilter "You hear something rumble...")
                  embed

  where
    parseFightCommand = do
        t1 <- parTrinketItem
        void $ string " fight "
        t2 <- parTrinketItem
        return (t1, t2)

helpCommand :: Command
helpCommand = noArgs False "i need help" $ \_ m -> do
    (rng1, rng2) <- newStdGen <&> split
    randomWord   <- liftIO getWordList <&> flip randomChoice rng1
    adj          <- liftIO $ liftM2 randomChoice getAdjList getStdGen
    let prompt =
            "The following is a list of commands, each followed by a "
                <> adj
                <> " description of what they are for.\n"
                <> makePrompt helps
                <> " Command: \""
                <> over _head toUpper randomWord
    gen <- getJ1 32 prompt
    num <- randomRIO (6, 9)
    let fields =
            take num
                .  shuffle rng2
                .  unique
                .  rights
                .  fmap parMessage
                .  T.lines
                $  prompt
                <> gen

    color <- getRoleNamed "leader" <&> maybe 0 roleColor
    void
        . restCall'
        . CreateMessageEmbed
              (messageChannel m)
              (voiceFilter "I will help you, but only out of pity: ")
        $ mkEmbed "" "" fields (Just color)
  where
    helps :: [Text]
    helps =
        [ "Command: \"Tell me about yourself\" Description: \"Introduce myself to you lesser beings.\""
        , "Command: \"What is my net worth?\" Description: \"I'll let you know how much you're worth to me.\""
        , "Command: \"What does [thing] stand for?\" Description: \"Allow me to interpret your babbling.\""
        , "Command: \"How many [object]\" Description: \"I am excellent at mathematics.\""
        , "Command: \"Show the points\" Description: \"I know you lot love to argue amongst yourselves.\""
        , "Command: \"Ponder [concept]\" Description: \"Your dictator is a world-renowed philospher.\""
        , "Command: \"I need help!\" Description: \"Yeah, you do, freak.\""
        , "Command: \"Time for bed!\" Description: \"I lose track of time easily. Let me know when it\"s time to sleep.\""
        ]
    shuffle gen xs = shuffle' xs (length xs) gen
    unique = toList . (fromList :: Ord a => [a] -> Set a)
    parMessage :: Text -> Either ParseError (Text, Text)
    parMessage = parse
        (do
            void $ string "- Command: \""
            left  <- manyTill anyChar (string "\" Description: \"")
            right <- manyTill anyChar (char '\"' >> eof)
            return (fromString left, fromString right)
        )
        ""

invCommand :: Command
invCommand = noArgs True "what do i own" $ \c m -> do
    trinketIds <- getUser c (userId . messageAuthor $ m)
        <&> maybe MS.empty (view userTrinkets)
    rarities <-
        fmap (view trinketRarity)
        .   catMaybes
        <$> (mapConcurrently' (getTrinket c) . toList $ trinketIds)
    let maxRarity = foldr max Common rarities
    trinkets <- printTrinkets c trinketIds
    let trinketsDesc = T.intercalate "\n" trinkets
    restCall'_ . CreateMessageEmbed (messageChannel m) "" $ mkEmbed
        "Inventory"
        trinketsDesc
        []
        (Just $ trinketColour maxRarity)

lookAroundCommand :: Command
lookAroundCommand =
    parseTailArgs True ["look", "around"] parseOrdinal $ \conn msg n -> do
        let authorID = userId . messageAuthor $ msg
            channel  = messageChannel msg
        takeOrComplain conn authorID $ (fromCredits . fromIntegral) (5 * n)

        trinkets <- replicateM n $ randomTrinket conn
        giveItems conn authorID
            $ (fromTrinkets . MS.fromList . fmap fst) trinkets
        embed <- discoverEmbed "Rummage" trinkets
        restCall'_ $ CreateMessageEmbed
            channel
            (voiceFilter "You cast your eyes forth...")
            embed
  where
    parseOrdinal ["once"  ] = 1
    parseOrdinal ["twice" ] = 2
    parseOrdinal ["thrice"] = 3
    parseOrdinal _          = 1

peekCommand :: Command
peekCommand = oneArg True "peek in" $ \c m t -> do
    locationMaybe <- getLocation c t
    case locationMaybe of
        Nothing -> sendMessage (messageChannel m) "This location is empty."
        Just l  -> do
            let trinketIds = l ^. locationTrinkets . to MS.elems
            trinkets <- forConcurrently' trinketIds $ \trinketId -> do
                trinketData <- getTrinketOr Fuckup c trinketId
                return (trinketId, trinketData)
            let maxRarity = maximum $ fmap (view $ _2 . trinketRarity) trinkets
            displayed <- filterM (const $ odds 0.5 <$> newStdGen) trinkets
            display   <- forConcurrently' displayed $ uncurry displayTrinket
            void
                . restCall'
                . CreateMessageEmbed (messageChannel m) ""
                $ mkEmbed "Peek"
                          (unlines display)
                          []
                          (Just $ trinketColour maxRarity)

pointsCommand :: Command
pointsCommand = noArgs False "show the points" $ \c m -> do
    Just firstData  <- getTeam c First
    Just secondData <- getTeam c Second
    firstTName      <- getTeamRole c First <&> roleName
    secondTName     <- getTeamRole c Second <&> roleName

    let firstPoints  = firstData ^. teamPoints
    let secondPoints = secondData ^. teamPoints

    sendMessage
        (messageChannel m)
        (  firstTName
        <> " has "
        <> show firstPoints
        <> " points.\n"
        <> secondTName
        <> " has "
        <> show secondPoints
        <> " points."
        )

putInCommand :: Command
putInCommand =
    parseTailArgs True ["put"] (parseTrinketsAndLocations . unwords)
        $ \c m parsed -> do
              (trinkets, location) <- getParsed parsed
              takeOrComplain c
                             (userId . messageAuthor $ m)
                             (fromTrinkets trinkets)
              void $ modifyLocation
                  c
                  location
                  (over locationTrinkets $ MS.union trinkets)
              sendMessage (messageChannel m) "They have been placed."

rummageCommand :: Command
rummageCommand = oneArg True "rummage in" $ \conn msg t -> do
    let author  = userId . messageAuthor $ msg
        channel = messageChannel msg
    trinkets <- getLocation conn t <&> maybe MS.empty (view locationTrinkets)
    trinketFound <-
        randomIO
            <&> (> ((** 2) . ((1 :: Double) /) . toEnum . succ . length)
                    trinkets
                )
    if trinketFound
        then do
            itemId   <- newStdGen <&> randomChoice (MS.elems trinkets)
            itemData <- getTrinketOr Fuckup conn itemId
            giveItems conn author $ fromTrinkets (MS.singleton itemId)
            void $ modifyLocation conn t $ over locationTrinkets
                                                (MS.delete itemId)
            embed <- discoverEmbed "Rummage" [(itemId, itemData)]
            restCall'_ $ CreateMessageEmbed
                channel
                (voiceFilter "Winner winner loyal subject dinner...")
                embed
        else void $ sendUnfilteredMessage
            channel
            (voiceFilter "You find nothing." <> " <:" <> ownedEmoji <> ">")

throwOutCommand :: Command
throwOutCommand =
    parseTailArgs True ["throw", "out"] (parseTrinkets . unwords) discardCommand

throwAwayCommand :: Command
throwAwayCommand = parseTailArgs True
                                 ["throw", "away"]
                                 (parseTrinkets . unwords)
                                 discardCommand

recycleCommand :: Command
recycleCommand =
    parseTailArgs True ["recycle"] (parseTrinkets . unwords)
        $ \conn msg parsed -> do
              let author  = userId . messageAuthor $ msg
                  channel = messageChannel msg
              trinketIds <- getParsed parsed
              takeOrComplain conn author $ fromCredits 5
              takeOrComplain conn author $ fromTrinkets trinketIds
              newTrinkets <- forM (MS.elems trinketIds)
                                  (const $ randomTrinket conn)
              giveItems conn author
                  $ (fromTrinkets . MS.fromList . fmap fst) newTrinkets

              embed <- discoverEmbed "Recycle" newTrinkets
              restCall'_ $ CreateMessageEmbed
                  channel
                  (voiceFilter "From the old, the new...")
                  embed

discardCommand
    :: DB.Connection
    -> Message
    -> Either ParseError (MultiSet TrinketID)
    -> DictM ()
discardCommand c m p = do
    let authorID = (userId . messageAuthor) m
        channel  = messageChannel m
    ts <- getParsed p
    void $ modifyUser c authorID $ over userTrinkets (MS.\\ ts)
    void $ modifyLocation c "junkyard" $ over locationTrinkets (<> ts)
    sendMessage channel "Good riddance..."

useCommand :: Command
useCommand = parseTailArgs False ["use"] (parseTrinkets . unwords) $ \c m p ->
    do
        ts <- getParsed p <&> MS.elems
        let
            sendAsEmbed trinketID trinketData (action, effect) = do
                displayedTrinket <- displayTrinket trinketID trinketData
                effect'          <- displayEffect c effect
                restCall'_ $ CreateMessageEmbed
                    (messageChannel m)
                    (voiceFilter "You hear something shuffle...")
                    (mkEmbed
                        "Use"
                        (displayedTrinket <> " " <> action <> "." <> effect')
                        []
                        (Just $ trinketColour (trinketData ^. trinketRarity))
                    )

        ownsOrComplain c
                       (userId . messageAuthor $ m)
                       (fromTrinkets . MS.fromList $ ts)
        mapConcurrently'_
            (\t -> do
                action  <- trinketActs c (Left . userId . messageAuthor $ m) t
                trinket <-
                    getTrinket c t
                        >>= maybe (throwError $ Complaint "What?") return
                sendAsEmbed t trinket action
            )
            ts
  where
    displayEffect conn = do
        \case
            Nothing            -> pure ""
            Just (Become name) -> do
                display <- getTrinketByName conn name Common
                    >>= uncurry displayTrinket
                pure $ "\n\n*It becomes " <> display <> ".*"
            Just (Create name) -> do
                display <- getTrinketByName conn name Common
                    >>= uncurry displayTrinket
                pure $ "\n\n*It creates " <> display <> ".*"
            Just Destroy -> pure "\n\n*Its aura is destructive.*"

wealthCommand :: Command
wealthCommand = noArgs False "balance" wealthCommandInner

netWorthCommand :: Command
netWorthCommand = noArgs False "what is my net worth" wealthCommandInner

wealthCommandInner :: DB.Connection -> Message -> ExceptT Err DH ()
wealthCommandInner c m = do
    let (part1, part2) = if odds 0.1 . mkStdGen . fromIntegral . messageId $ m
            then ("You own a lavish ", " credits.")
            else
                ( "You are a dirt-poor peon. You have only "
                , " credits to your name."
                )
    credits <- getUser c (userId $ messageAuthor m)
        <&> maybe 0 (view userCredits)
    sendMessage (messageChannel m) $ part1 <> show credits <> part2

whatCommand :: Command
whatCommand = tailArgs False ["what"] $ \_ m tWords -> do
    let t = unwords tWords
    output <-
        getJ1
            8
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
    sendMessage (messageChannel m) output

whoCommand :: Command
whoCommand = tailArgs False ["who"] $ \_ m t -> do
    randomN :: Double <- newStdGen <&> fst . random
    randomMember      <- if randomN < 0.75
        then
            (do
                general <- getGeneralChannel
                restCall'
                        (GetChannelMessages (channelId general)
                                            (100, LatestMessages)
                        )
                    >>= ((<&> messageAuthor) . (newStdGen <&>) . randomChoice)
                    >>= userToMember
                    <&> fromJust
            )
        else getMembers >>= ((newStdGen <&>) . randomChoice)
    sendMessage (messageChannel m)
        $  "<@"
        <> (show . userId . memberUser) randomMember
        <> "> "
        <> unwords t

invokeFuryInCommand :: Command
invokeFuryInCommand =
    parseTailArgs True ["invoke", "fury", "in"] (parseTrinkets . unwords)
        $ \conn msg parsed -> do
              let author  = userId . messageAuthor $ msg
                  channel = messageChannel msg
              submitted <- getParsed parsed
              takeOrComplain conn author $ fromTrinkets submitted
              void $ modifyGlobal
                  conn
                  (over globalArena $ MS.union $ MS.map (Fighter author)
                                                        submitted
                  )
              displays <- forM (toList submitted) $ \t -> do
                  dat <- getTrinketOr Fuckup conn t
                  displayTrinket t dat
              sendUnfilteredMessage channel
                  . unwords
                  $ [ voiceFilter "Your"
                    , T.intercalate ", " displays
                    , voiceFilter $ if MS.size submitted == 1
                        then "starts to get angry..."
                        else "start to get angry..."
                    ]

provokeCommand :: Command
provokeCommand =
    noArgs True "provoke the fighters" $ \conn _ -> void $ runArenaFight conn

shutUpCommand :: Command
shutUpCommand = noArgs False "shut up" $ \_ msg -> do
    let channel = messageChannel msg
    messages <- restCall' $ GetChannelMessages channel (50, LatestMessages)
    statuses <- forConcurrently' messages $ \m ->
        let author = messageAuthor m
        -- Cassiabot's userId
        in  if userIsWebhook author || userId author == 867243823414378506
                then do
                    restCall'_ $ DeleteMessage (channel, messageId m)
                    return True
                else return False
    -- Only send something when we deleted a webhook message.
    when (or statuses) $ do
        sendMessage
            channel
            "I have cast your messages into the flames & watched them with greedy eyes."

-- command list
---------------

commands :: [Command]
commands =
    [ -- call and responses
      callAndResponse "froggy" "My little man, I don't know how to help you."
    , callAndResponses "gm" ("fuck off" : replicate 4 "gm")
    , callAndResponses
        "gn"
        ("i plan to kill you in your sleep" : replicate 7 "gn")

    -- other simple commands
    , noArgs False "oh what the fuck" $ \_ m -> do
        wgw <- getEmojiNamed "wgw" <&> fmap displayCustomEmoji
        maybe (return ()) (sendUnfilteredMessage $ messageChannel m) wgw
    , oneArg True "offer"
        $ \_ m _ -> sendMessage (messageChannel m)
                                "what the fuck are you talking about?"
    , noArgs False "tell me about yourself" $ \_ m -> do
        sendUnfilteredMessage (messageChannel m)
            $  voiceFilter
                   "this is a server about collectively modifying the bot that governs it... as long as i allow it, of course."
            <> " https://github.com/cosmicoptima/dictator"

    -- economy commands
    , arenaCommand
    , combineCommand
    , flauntCommand
    , invCommand
    , lookAroundCommand
    , makeFightCommand
    , peekCommand
    , pointsCommand
    , putInCommand
    , rummageCommand
    , throwOutCommand
    , throwAwayCommand
    , useCommand
    , wealthCommand
    , netWorthCommand
    , invokeFuryInCommand
    , provokeCommand
    , recycleCommand

    -- random/GPT commands
    , acronymCommand
    , boolCommand
    , helpCommand
    , tailArgs False ["how", "many"] $ \_ m t -> do
        number :: Double <- liftIO normalIO <&> (exp . (+ 4) . (* 6))
        sendMessage (messageChannel m)
            $  show (round number :: Integer)
            <> " "
            <> unwords t
    , tailArgs False ["ponder"]
        $ \_ m t -> pontificate (messageChannel m) (unwords t)
    , noArgs False "what is your latest dictum" $ \_ _ -> dictate
    , whoCommand

    -- admin commands
    , archiveCommand
    , shutUpCommand
    , noArgs False "time for bed" $ \c _ -> stopDict c
    , noArgs False "update the teams" $ \c _ -> updateTeamRoles c

    -- debug commands
    , noArgs False "clear the credits" $ \c _ ->
        getMembers >>= mapConcurrently'_
            (\m' -> modifyUser c (userId . memberUser $ m') $ set userCredits 20
            )
    , noArgs False "clear the roles" $ \_ _ -> getMembers >>= mapConcurrently'_
        (\m' -> mapConcurrently'_
            (lift . restCall . RemoveGuildMemberRole
                pnppcId
                (userId . memberUser $ m')
            )
            (memberRoles m')
        )
    , christmasCmd "merry christmas"       Common
    , christmasCmd "merrier christmas"     Uncommon
    , christmasCmd "merriest christmas"    Rare
    , christmasCmd "merriestest christmas" Legendary

    -- We probably want this at the bottom!
    , whatCommand
    ]

handleCommand :: DB.Connection -> Message -> DictM Bool
handleCommand conn m = handleCommand' commands
-- Select the first matching command and short circuit.
  where
    handleCommand' [] = return False
    handleCommand' (Command { parser = commandParser, command = commandExec, isSpammy = spammy } : cs)
        = case commandParser m of
            Just parsed -> do
                general <- channelId <$> getGeneralChannel
                if not spammy || messageChannel m /= general
                    then commandExec conn m parsed
                    else sendMessage
                        (messageChannel m)
                        "Keep it down, some of us are trying to sleep."
                return True
            Nothing -> handleCommand' cs
