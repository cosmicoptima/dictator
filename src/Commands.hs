{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Commands
    ( handleCommand
    , Err(..)
    ) where

import           Relude                  hiding ( First )
import           Relude.Unsafe                  ( fromJust )

import           Datatypes
import           DiscordUtils
import           Economy
import           Events
import           GenText
import           Items
import           Utils

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
import           Data.List                      ( stripPrefix )
import qualified Data.MultiSet                 as MS
import           Data.MultiSet                  ( MultiSet )
import qualified Data.Text                     as T
import qualified Database.Redis                as DB
import           Text.Parsec
import           Text.Parsec.Text


-- Morally has type Command = exists a. Command { ... }
-- Existential types in Haskell have a strange syntax!
data Command = forall a . Command
    { parser  :: Message -> Maybe a
    , command :: DB.Connection -> Message -> a -> DictM ()
    }

commandWords :: Message -> [Text]
commandWords = T.words . T.strip . T.toLower . stripRight . messageText
    where stripRight = T.reverse . T.dropWhile isPunctuation . T.reverse

-- command builders
-------------------

-- | Matches a specific name and nothing more.
noArgs :: Text -> (DB.Connection -> Message -> DictM ()) -> Command
noArgs pat cmd = Command
    { parser  = \m -> if pat == messageText m then Just () else Nothing
    , command = \c m _ -> cmd c m
    }

-- | Matches a specific name on the head of the message and returns the tail.
tailArgs
    :: [Text] -> (DB.Connection -> Message -> [Text] -> DictM ()) -> Command
tailArgs cmd = parseTailArgs cmd id

oneArg :: Text -> (DB.Connection -> Message -> Text -> DictM ()) -> Command
oneArg name cmd = tailArgs (words name) (\c m -> cmd c m . unwords)

-- | Matches a specific name on the head of the message a transformation (likely a parser) to the tail.
parseTailArgs
    :: [Text]
    -> ([Text] -> a)
    -> (DB.Connection -> Message -> a -> DictM ())
    -> Command
parseTailArgs pat trans cmd = Command
    { parser  = \m -> case stripPrefix pat . commandWords $ m of
                    Just cmdTail -> Just $ trans cmdTail
                    Nothing      -> Nothing
    , command = cmd
    }

callAndResponses :: Text -> [Text] -> Command
callAndResponses call responses = noArgs call $ \_ m ->
    newStdGen >>= sendMessage (messageChannel m) . randomChoice responses

callAndResponse :: Text -> Text -> Command
callAndResponse call response = callAndResponses call [response]

christmasCmd :: Text -> Rarity -> Command
christmasCmd name rarity = noArgs name $ \c m ->
    getNewTrinket c rarity >>= sendMessage (messageChannel m) . displayTrinket 0


-- longer commands
------------------

acronymCommand :: Command
acronymCommand = Command
    { parser  = T.stripPrefix "what does "
                <=< T.stripSuffix " stand for"
                .   messageText
    , command = \_ m t -> do
                    pnppc <- liftIO $ acronym t
                    sendMessage (messageChannel m) $ T.unwords pnppc
    }

boolCommand :: Command
boolCommand = tailArgs ["is"] $ \_ m _ -> do
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
    parseTailArgs ["flaunt"] (parseTrinkets . unwords) $ \conn msg parsed -> do
        let authorID = (userId . messageAuthor) msg
            channel  = messageChannel msg
        case parsed of
            Left err ->
                sendMessage channel
                    $  "What the fuck is this? ```"
                    <> show err
                    <> "```"
            Right flauntedTrinkets -> do
                userData <- getUser conn authorID <&> fromMaybe def
                if userOwns userData $ fromTrinkets flauntedTrinkets
                    then do
                        trinkets <-
                            ( mapM (\t -> getTrinket conn t <&> fmap (t, ))
                            . MS.elems
                            )
                                flauntedTrinkets
                            <&> MS.mapMaybe id
                            .   MS.fromList
                        let display =
                                T.intercalate "\n"
                                    . MS.elems
                                    . MS.map (\w -> "**" <> w <> "**")
                                    . MS.map (uncurry displayTrinket)
                                    $ trinkets
                        void

                            . restCall'
                            . CreateMessageEmbed
                                  channel
                                  (voiceFilter
                                      "You wish to display your wealth?"
                                  )
                            $ mkEmbed "Goods (PITIFUL)" display [] Nothing
                    else do
                        sendMessage
                            channel
                            "You don't own the goods you so shamelessly try to flaunt, and now you own even less. Credits, that is."
                        void $ modifyUser conn authorID (over userCredits pred)

combineCommand :: Command
combineCommand = parseTailArgs ["combine"]
                               (parseTrinketPair . unwords)
                               combineCommand'
  where
    combineCommand' conn msg (parsed :: Either ParseError (TrinketID, TrinketID))
        = do
            (item1, item2) <- hoistEither $ first
                (\err ->
                    Complaint $ "What the fuck is this?```" <> show err <> "```"
                )
                parsed
            taken <- takeOrPunish conn author $ cost item1 item2
            when taken $ do
                pair <- liftM2 combine
                               (getTrinket conn item1)
                               (getTrinket conn item2)

                (trinket1, trinket2) <- case pair of
                    Just p -> return p
                    Nothing ->
                        throwError
                            .  Fuckup
                            $  "User owns a trinket "
                            <> show (item1, item2)
                            <> " that isn't in the database!"
                (tId, newTrinket) <- combineTrinkets conn trinket1 trinket2
                void $ modifyUser conn
                                  author
                                  (over userTrinkets (MS.insert tId))
                let embedDesc =
                        "You combine **"
                            <> displayTrinket item1 trinket1
                            <> "** and **"
                            <> displayTrinket item2 trinket2
                            <> "** to make **"
                            <> displayTrinket tId newTrinket
                            <> "**."
                void
                    . restCall'
                    . CreateMessageEmbed
                          channel
                          (voiceFilter "bubble, bubble, toil and trouble...")
                    $ mkEmbed "Combination" embedDesc [] Nothing

      where
        author  = (userId . messageAuthor) msg
        channel = messageChannel msg
        cost item1 item2 =
            def & itemTrinkets .~ MS.fromList [item1, item2] & itemCredits .~ 2
        combine may1 may2 = do
            m1 <- may1
            m2 <- may2
            return (m1, m2)

helpCommand :: Command
helpCommand = noArgs "i need help" $ \_ m -> do
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
invCommand = noArgs "what do i own" $ \c m -> do
    trinketIds <- getUser c (userId . messageAuthor $ m)
        <&> maybe MS.empty (view userTrinkets)
    trinkets <- printTrinkets c trinketIds
    let trinketsDesc =
            T.intercalate "\n" . fmap (\t -> "**" <> t <> "**") $ trinkets
    void . restCall' . CreateMessageEmbed (messageChannel m) "" $ mkEmbed
        "Inventory"
        trinketsDesc
        []
        Nothing

lookAroundCommand :: Command
lookAroundCommand = noArgs "look around" $ \c m -> do
    let authorID = userId . messageAuthor $ m
        channel  = messageChannel m
    userData <- getUser c authorID <&> fromMaybe def
    if
        | userData ^. userCredits < 5 -> sendMessage
            channel
            "You're too poor for that."
        | otherwise -> do
            (rng, rng'   ) <- split <$> newStdGen
            (tId, trinket) <- if odds 0.5 rng
                then mkNewTrinket c (if odds 0.18 rng' then Rare else Common)
                else getRandomTrinket c
            void
                . modifyUser c authorID
                $ ( over userTrinkets (MS.insert tId)
                  . over userCredits  (subtract 5)
                  )

            let embedDesc =
                    "You find **" <> displayTrinket tId trinket <> "**."
                postDesc = "You look around and find..."
            void
                . restCall'
                . CreateMessageEmbed channel (voiceFilter postDesc)
                $ mkEmbed "Rummage" embedDesc [] Nothing

pointsCommand :: Command
pointsCommand = noArgs "show the points" $ \c m -> do
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
putInCommand = Command
    { parser  =
        rightToMaybe
        . parse
              (do
                  void $ string "put "
                  trinkets <-
                      some (noneOf " ")
                      >>= either (const empty) return
                      .   parseTrinkets
                      .   fromString
                  void $ string " in "
                  location <- some anyChar <&> fromString

                  return (trinkets, location) :: Parser
                          (MultiSet TrinketID, Text)
              )
              ""
        . messageText
    , command = \c m (ts, l) -> do
        taken <- takeOrPunish c
                              (userId . messageAuthor $ m)
                              (def & itemTrinkets .~ ts)
        if taken
            then do
                void $ modifyLocation c l (over locationTrinkets $ MS.union ts)
                sendMessage (messageChannel m) "They have been placed."
            else sendMessage
                (messageChannel m)
                "You don't have the goods, and now you have even less etc etc"
    }

rummageCommand :: Command
rummageCommand = oneArg "rummage in" $ \c m t -> do
    trinkets     <- getLocation c t <&> maybe MS.empty (view locationTrinkets)
    trinketFound <-
        randomIO
            <&> (> ((** 2) . ((1 :: Double) /) . toEnum . succ . length)
                    trinkets
                )
    if trinketFound
        then do
            itemID        <- newStdGen <&> randomChoice (MS.elems trinkets)
            Just itemData <- getTrinket c itemID

            void $ modifyUser c (userId . messageAuthor $ m) $ over
                userTrinkets
                (MS.insert itemID)
            void $ modifyLocation c t $ over locationTrinkets (MS.delete itemID)
            void $ restCall' $ CreateMessageEmbed
                (messageChannel m)
                (voiceFilter "Winner winner loyal subject dinner...")
                (mkEmbed
                    "Rummage"
                    ("You find **" <> displayTrinket itemID itemData <> "**.")
                    []
                    Nothing
                )
        else void $ sendUnfilteredMessage
            (messageChannel m)
            (voiceFilter "You find nothing." <> " <:" <> ownedEmoji <> ">")

throwOutCommand :: Command
throwOutCommand = Command
    { parser  = \m -> case stripPrefix ["throw", "out"] . commandWords $ m of
                    Just goods -> Just . parseTrinkets . unwords $ goods
                    Nothing    -> Nothing
    , command = \c m p -> do
        let authorID = (userId . messageAuthor) m
            channel  = messageChannel m
        case p of
            Left e ->
                sendMessage channel
                    $  "What the fuck is this? ```"
                    <> show e
                    <> "```"
            Right ts -> do
                void $ modifyUser c authorID $ over userTrinkets (MS.\\ ts)
                void $ modifyLocation c "junkyard" $ over locationTrinkets
                                                          (<> ts)
                sendMessage channel "Good riddance..."
    }

wealthCommand :: Command
wealthCommand = noArgs "what is my net worth" $ \c m -> do
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
whatCommand = tailArgs ["what"] $ \_ m tWords -> do
    let t = unwords tWords
    output <-
        getGPT
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
whoCommand = tailArgs ["who"] $ \_ m t -> do
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
    , tailArgs ["offer"] $ \_ m _ ->
        sendMessage (messageChannel m) "what the fuck are you talking about?"
    , noArgs "tell me about yourself" $ \_ m -> do
        sendUnfilteredMessage (messageChannel m)
            $  voiceFilter
                   "this is a server about collectively modifying the bot that governs it... as long as i allow it, of course."
            <> " https://github.com/cosmicoptima/dictator"

    -- economy commands
    , flauntCommand
    , invCommand
    , lookAroundCommand
    , pointsCommand
    , putInCommand
    , rummageCommand
    , throwOutCommand
    , wealthCommand
    , combineCommand

    -- random/GPT commands
    , acronymCommand
    , boolCommand
    , helpCommand
    , tailArgs ["how", "many"] $ \_ m t -> do
        number :: Double <- liftIO normalIO <&> (exp . (+ 4) . (* 6))
        sendMessage (messageChannel m)
            $  show (round number :: Integer)
            <> " "
            <> unwords t
    , tailArgs ["ponder"] $ \_ m t -> pontificate (messageChannel m) (unwords t)
    , noArgs "what is your latest dictum" $ \_ _ -> dictate
    , whoCommand

    -- admin commands
    , noArgs "time for bed" $ \c _ -> stopDict c
    , noArgs "update the teams" $ \c _ -> updateTeamRoles c

    -- debug commands
    , noArgs "clear the roles" $ \_ _ -> getMembers >>= lift . mapM_
        (\m' -> mapM_
            (restCall . RemoveGuildMemberRole pnppcId (userId . memberUser $ m')
            )
            (memberRoles m')
        )
    , noArgs "give me money" $ \c m -> do
        if (userId . messageAuthor $ m)
               == 481581288910749698
               && messageChannel m
               == 878376227428245558
            then sendMessageToGeneral "no"
            else do
                newUser <- modifyUser c
                                      (userId . messageAuthor $ m)
                                      (over userCredits (+ 10))
                sendMessage (messageChannel m)
                    $  "You now have "
                    <> show (newUser ^. userCredits)
                    <> " credits. Probably."
    , christmasCmd "merry christmas"    Common
    , christmasCmd "merrier christmas"  Rare
    , christmasCmd "merriest christmas" Epic

    -- We probably want this at the bottom!
    , whatCommand
    ]

handleCommand :: DB.Connection -> Message -> DictM Bool
handleCommand conn m = handleCommand' commands
-- Select the first matching command and short circuit.
  where
    handleCommand' [] = return False
    handleCommand' (Command { parser = commandParser, command = commandExec } : cs)
        = case commandParser m of
            Just parsed -> commandExec conn m parsed >> return True
            Nothing     -> handleCommand' cs
