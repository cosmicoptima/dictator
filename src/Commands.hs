{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commands where

import           Relude                  hiding ( First )

import           Datatypes
import           DiscordUtils
import           Economy
import           Events
import           GenText
import           Utils

import           Discord
import           Discord.Requests
import           Discord.Types

import           Data.Random.Normal
import           System.Random
import           System.Random.Shuffle          ( shuffle' )

import           Control.Lens
import           Control.Monad
import           Data.Char
import qualified Data.Text                     as T
import qualified Database.Redis                as DB
import           Text.Parsec
import           UnliftIO.Async


data Command = Command
    { parser  :: Message -> Maybe Text
    , command :: DB.Connection -> Message -> Text -> DH ()
    }


-- command builders
-------------------

noArgs :: Text -> (DB.Connection -> Message -> DH ()) -> Command
noArgs name cmd = Command
    { parser  = \m -> if messageText m == name then Just mempty else Nothing
    , command = \c m _ -> cmd c m
    }

oneArg :: Text -> (DB.Connection -> Message -> Text -> DH ()) -> Command
oneArg name cmd = Command
    { parser  = fmap T.strip . T.stripPrefix name . messageText
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
boolCommand = oneArg "is" $ \_ m _ -> do
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
        <&> maybe [] (view userTrinkets)
    trinkets <- mapM (getTrinket c) trinketIds <&> catMaybes
    let trinketsDesc =
            T.intercalate "\n"
                .   fmap (\w -> "**" <> w <> "**")
                $   uncurry displayTrinket
                <$> zip trinketIds trinkets
    void . restCall' . CreateMessageEmbed (messageChannel m) "" $ mkEmbed
        "Inventory"
        trinketsDesc
        []
        Nothing

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

rummageCommand :: Command
rummageCommand = oneArg "rummage in" $ \c m t -> do
    let authorID = userId . messageAuthor $ m
        channel  = messageChannel m
    userData <- getUser c authorID <&> fromMaybe def
    if
        | userData ^. userCredits <= 0 -> sendMessage
            channel
            "You're too poor for that."
        | length (userData ^. userTrinkets) >= 8 -> sendMessage
            channel
            "Nobody _needs_ more than 8 trinkets..."
        | otherwise -> do
            rng            <- newStdGen
            (tId, trinket) <- mkNewTrinket
                c
                (if odds 0.18 rng then Rare else Common)
            void
                . modifyUser c authorID
                $ (over userTrinkets (tId :) . over userCredits pred)

            let embedDesc =
                    "You find **" <> displayTrinket tId trinket <> "**."
                postDesc = "You look around in " <> t <> " and find..."
            void
                . restCall'
                . CreateMessageEmbed channel (voiceFilter postDesc)
                $ mkEmbed "Rummage" embedDesc [] Nothing

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
    , oneArg "offer" $ \_ m _ ->
        sendMessage (messageChannel m) "what the fuck are you talking about?"
    , noArgs "tell me about yourself" $ \_ m -> do
        sendUnfilteredMessage (messageChannel m)
            $  voiceFilter
                   "this is a server about collectively modifying the bot that governs it... as long as i allow it, of course."
            <> " https://github.com/cosmicoptima/dictator"

    -- economy commands
    , invCommand
    , pointsCommand
    , rummageCommand
    , wealthCommand

    -- random/GPT commands
    , acronymCommand
    , boolCommand
    , helpCommand
    , oneArg "how many" $ \_ m t -> do
        number :: Double <- liftIO normalIO <&> (exp . (+ 4) . (* 6))
        sendMessage (messageChannel m)
            $  show (round number :: Integer)
            <> " "
            <> t
    , oneArg "ponder" $ const (pontificate . messageChannel)
    , noArgs "what is your latest dictum" $ \_ _ -> dictate

    -- admin commands
    , noArgs "time for bed" $ \c _ -> stopDict c
    , noArgs "update the teams" $ \c _ -> updateTeamRoles c

    -- debug commands
    , noArgs "clear the roles" $ \_ _ -> getMembers >>= mapConcurrently_
        (\m' -> mapConcurrently_
            (restCall . RemoveGuildMemberRole pnppcId (userId . memberUser $ m')
            )
            (memberRoles m')
        )
    , christmasCmd "merry christmas"    Common
    , christmasCmd "merrier christmas"  Rare
    , christmasCmd "merriest christmas" Epic
    ]

-- | the new handleCommand (WIP)
handleCommand' :: DB.Connection -> Message -> DH ()
handleCommand' conn m = forM_ commands
    $ \c -> maybe (return ()) (command c conn m) . flip parser m $ c
