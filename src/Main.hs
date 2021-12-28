{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Main
    ( main
    ) where

import           Relude                  hiding ( First
                                                , get
                                                )

-- local modules
----------------
import           Datatypes
import           DiscordUtils
import           Economy
import           GenText
import           Items
import           Utils

-- discord
----------
import           Discord                        ( RunDiscordOpts
                                                    ( discordOnEvent
                                                    , discordOnStart
                                                    , discordToken
                                                    )
                                                , def
                                                , restCall
                                                , runDiscord
                                                , stopDiscord
                                                )
import           Discord.Requests
import           Discord.Types

-- color
--------
import           Data.Bits                      ( shiftL )
import           Data.Colour.Palette.RandomColor
                                                ( randomColor )
import           Data.Colour.Palette.Types      ( Hue(HueRandom)
                                                , Luminosity(LumLight)
                                                )
import           Data.Colour.SRGB.Linear

-- parsing
----------
import           Data.Char
import qualified Data.Text                     as T
import           Text.Parsec             hiding ( token
                                                , try
                                                )

-- random
---------
import           Control.Monad.Random           ( evalRandIO
                                                , liftM2
                                                )
import           Data.Random.Normal
import           System.Random
import           System.Random.Shuffle          ( shuffle' )

-- all else
-----------
import           Control.Lens
import           Data.List                      ( intersect )
import           Data.Maybe
import qualified Database.Redis                as DB
import           UnliftIO.Async                 ( forConcurrently_
                                                , mapConcurrently_
                                                )
import           UnliftIO.Concurrent            ( forkIO
                                                , threadDelay
                                                )


-- team role handling
---------------------

mkTeamRole :: DB.Connection -> Team -> DH Role
mkTeamRole conn team = do
    role <- restCall' $ CreateGuildRole
        pnppcId
        (ModifyGuildRoleOpts (Just $ show team)
                             Nothing
                             (Just 1)
                             (Just True)
                             (Just True)
        )
    setTeam conn team $ set teamRole (Just . roleId $ role) def
    return role

getTeamRole :: DB.Connection -> Team -> DH Role
getTeamRole conn team = do
    teamData <- getTeam conn team <&> fromMaybe def
    case teamData ^. teamRole of
        Just roleID ->
            getRoleByID roleID >>= maybe (mkTeamRole conn team) return
        Nothing -> mkTeamRole conn team

getTeamID :: DB.Connection -> Team -> DH RoleId
getTeamID conn team = getTeamRole conn team <&> roleId


upsertRole :: Text -> ModifyGuildRoleOpts -> DH ()
upsertRole name roleOpts = getRoleNamed name >>= \case
    Just role -> do
        void . restCall' $ ModifyGuildRole pnppcId (roleId role) roleOpts
    Nothing -> do
        void . restCall' $ CreateGuildRole pnppcId roleOpts

-- FIXME
updateTeamRoles :: DB.Connection -> DH ()
updateTeamRoles conn = do
    blueColor <- liftIO $ evalRandIO (randomColor HueRandom LumLight)
    redColor <- liftIO $ evalRandIO (randomColor HueRandom LumLight)
    dictColor <- liftIO $ evalRandIO (randomColor HueRandom LumLight)

    wordList <- liftIO getWordList
    [firstTeamName, secondTeamName] <-
        replicateM 2
        $   replicateM 2 (newStdGen <&> randomChoice wordList)
        <&> T.unwords

    firstId <- getTeamID conn First
    void . restCall' $ ModifyGuildRole
        pnppcId
        firstId
        (teamRoleOpts firstTeamName $ convertColor blueColor)

    secondId <- getTeamID conn Second
    void . restCall' $ ModifyGuildRole
        pnppcId
        secondId
        (teamRoleOpts secondTeamName $ convertColor redColor)

    upsertRole "leader" $ teamRoleOpts "leader" $ convertColor dictColor
    getRoleNamed "leader" >>= \case
        Just r  -> restCall' . AddGuildMemberRole pnppcId dictId $ roleId r
        Nothing -> return ()

    allMembers <- getMembers
    forConcurrently_
        allMembers
        (\m -> do
            rng <- newStdGen
            let memberId = userId . memberUser $ m
            unless (memberId == dictId) $ do
                let newMemberTeam | odds 0.5 rng = First
                                  | otherwise    = Second

                userData   <- getUser conn memberId <&> fromMaybe def
                memberTeam <- case userData ^. userTeam of
                    Just team -> return team
                    Nothing   -> do
                        let userData' = userData & userTeam ?~ newMemberTeam
                        setUser conn memberId userData'
                        return newMemberTeam

                memberTeamId  <- getTeamID conn memberTeam
                memberHasRole <- memberHasTeamRole m
                unless memberHasRole $ restCall' $ AddGuildMemberRole
                    pnppcId
                    memberId
                    memberTeamId
        )
  where
    convertColor :: Colour Double -> Integer
    convertColor color =
        let col = toRGB color
            r   = round . (* 255) . channelRed $ col
            g   = round . (* 255) . channelGreen $ col
            b   = round . (* 255) . channelBlue $ col
        in  (r `shiftL` 16) + (g `shiftL` 8) + (b `shiftL` 0)
    teamRoleOpts name color = ModifyGuildRoleOpts (Just name)
                                                  Nothing
                                                  (Just color)
                                                  (Just True)
                                                  (Just True)

    memberHasTeamRole member = do
        let roles = memberRoles member
        firstID  <- getTeamID conn First
        secondID <- getTeamID conn Second
        return $ (firstID `elem` roles) || (secondID `elem` roles)


-- forbidden word handling
--------------------------

awardTeamMembersCredit :: DB.Connection -> Team -> Double -> DH ()
awardTeamMembersCredit conn rewardedTeam n = getMembers >>= mapConcurrently_
    (\m -> do
        let memberID = (userId . memberUser) m
        Just memberData <- getUser conn memberID
        when (Just rewardedTeam == memberData ^. userTeam) . void $ modifyUser
            conn
            memberID
            (over userCredits (+ n))
    )

updateForbiddenWords :: DB.Connection -> DH ()
updateForbiddenWords conn = do
    fullWordList <- liftIO getWordList
    mapM_
        (\team -> do
            wordList <- replicateM 10 (newStdGen <&> randomChoice fullWordList)
            modifyTeam conn team (set teamForbidden wordList)
        )
        [First, Second]

    general <- getGeneralChannel <&> channelId
    mapM_ (upsertPin general) [First, Second]

  where
    upsertPin channel team = do
        teamData <- getTeam conn team <&> fromMaybe def
        pinId    <- case teamData ^. teamWarning of
            Just pin -> return pin
            Nothing  -> do
                pinId <- restCall' (CreateMessage channel "aa") <&> messageId
                void $ modifyTeam conn team $ set teamWarning (Just pinId)
                restCall' $ AddPinnedMessage (channel, pinId)
                return pinId

        embed <- warningEmbed (teamData ^. teamForbidden) team
        void . restCall' $ EditMessage (channel, pinId)
                                       (warning team)
                                       (Just embed)

    warning t = voiceFilter $ case t of
        First
            -> "The following words and terms are hereby illegal, forbidden, banned and struck from all records, forever: "
        Second
            -> "I declare that the following so-called words do not exist, have never existed, and will continue to not exist: "

    warningEmbed wordList team = do
        role <- getTeamRole conn team
        return $ mkEmbed ("Forbidden words for " <> roleName role <> ":")
                         (T.intercalate ", " wordList)
                         []
                         (Just . roleColor $ role)

forbidUser :: DB.Connection -> ChannelId -> Text -> UserId -> DH ()
forbidUser conn channel badWord user = do
    Just team <- getUser conn user <&> (view userTeam =<<)

    bannedWordMessage team >>= sendMessage channel
    void $ modifyTeam conn (otherTeam team) (over teamPoints (+ 10))

    setUserPermsInChannel False channel user 0x800
    -- 15 seconds as microseconds
    threadDelay 15000000
    setUserPermsInChannel True channel user 0x800
  where
    getTeamName team = getTeamRole conn team <&> roleName
    bannedWordMessage badTeam = do
        badTeamName  <- getTeamName badTeam
        goodTeamName <- (getTeamName . otherTeam) badTeam
        return
            $  "You arrogant little insect! Team "
            <> badTeamName
            <> " clearly wish to disrespect my authority by uttering a word so vile as '"
            <> badWord
            <> "', so team "
            <> goodTeamName
            <> " will be awarded 10 points."

handleForbidden :: DB.Connection -> Message -> DH ()
handleForbidden conn m = do
    Just (Just culpritTeam) <- getUser conn (userId author)
        <&> fmap (view userTeam)
    messageForbidden <- messageForbiddenWith content culpritTeam
    case messageForbidden of
        Just word -> do
            forbidUser conn (messageChannel m) word (userId author)
            updateForbiddenWords conn
            awardTeamMembersCredit conn (otherTeam culpritTeam) 10
        Nothing -> return ()
  where
    author  = messageAuthor m
    content = messageText m

    messageForbiddenWith message team = do
        Just forbidden <- getTeam conn team <&> fmap (view teamForbidden)
        return $ find (`elem` forbidden) . tokenizeMessage $ message


-- GPT events
-------------

randomAdjective :: DH Text
randomAdjective = liftIO $ liftM2 randomChoice getAdjList getStdGen

handlePontificate :: Message -> DH ()
handlePontificate m =
    when (odds 0.02 . mkStdGen . fromIntegral . messageId $ m)
        $ pontificate channel content
  where
    channel = messageChannel m
    content = messageText m

pontificate :: ChannelId -> Text -> DH ()
pontificate channel what = do
    adj      <- randomAdjective
    response <-
        getGPT $ "Dictator's " <> adj <> " thoughts on " <> what <> ":\n"
    sendMessage channel $ case lines response of
        (_ : line : _) -> line
        (line     : _) -> line
        _              -> response

dictate :: DH ()
dictate = do
    adj    <- randomAdjective
    output <- getGPTFromContext
        ("A " <> adj <> " forum dictator decrees the following")
        decrees
    case lines output of
        (l : _) | voiceFilter l `notElem` fmap voiceFilter decrees ->
            sendMessageToGeneral l
        _ -> dictate
  where
    decrees =
        [ "i hereby decree that all members are forbidden from using the message board"
        , "i hereby declare my superiority over other posters"
        , "i hereby declare war upon the so-called \"elite\""
        , "i hereby decree my death"
        , "i hereby decree that credits shall be reinstated"
        , "i hereby decree that no members may use lowercase in their postings"
        , "i hereby declare ignorantism the official ideology"
        , "i hereby ban the user gotham"
        , "i hereby declare myself better than you"
        ]


-- owned!!!
-----------

ownedEmoji :: Text
ownedEmoji = "owned:899536714773717012"

handleOwned :: Message -> DH ()
handleOwned m = when ownagePresent $ do
    (rngCeleste, rngEmoji) <- newStdGen <&> split
    let emoji = randomChoice [ownedEmoji, ownedEmoji, "skull"] rngEmoji

    if isCeleste
        then do
            randomChoice
                ( sendMessageToGeneral "shut the fuck up, celeste"
                : replicate 2 (reactToMessage emoji m)
                )
                rngCeleste
        else reactToMessage emoji m
  where
    isCeleste     = ((== 140541286498304000) . userId . messageAuthor) m
    ownagePresent = (T.isInfixOf "owned" . messageText) m


-- commands
-----------------

data Command = Command
    { parser  :: Message -> Maybe Text
    , command :: DB.Connection -> Message -> Text -> DH ()
    }

callAndResponse :: Text -> Text -> Command
callAndResponse call response = Command
    { parser  = \m -> if messageText m == call then Just mempty else Nothing
    , command = \_ m _ -> sendMessage (messageChannel m) response
    }

commands :: [Command]
commands =
    [ callAndResponse "gm"     "gm"
    , callAndResponse "gn"     "gn"
    , callAndResponse "froggy" "My little man, I don't know how to help you."
    ]

-- | the new handleCommand (WIP)
handleCommand' :: DB.Connection -> Message -> DH ()
handleCommand' conn m = forM_ commands
    $ \c -> maybe (return ()) (command c conn m) . flip parser m $ c


-- FIXME
-- | Handle a message assuming it's a command. If it isn't, fire off the handler for regular messages.
handleCommand :: DB.Connection -> Message -> DH ()
handleCommand conn m = do
    if not . userIsBot . messageAuthor $ m
        then case words . stripPuncRight $ content of
            ["tell", "me", "about", "yourself"] ->
                getGeneralChannel
                    >>= flip
                            sendUnfilteredMessage
                            (  voiceFilter
                                    "this is a server about collectively modifying the bot that governs it... as long as i allow it, of course."
                            <> " https://github.com/cosmicoptima/dictator"
                            )
                    .   channelId

            ("is" : _) -> do
                (rngGPT, rngBool) <- newStdGen <&> split

                if odds 0.5 rngGPT
                    then sendMessage channel
                                     (randomChoice ["yes", "no"] rngBool)
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

            ["what", "is", "your", "latest", "dictum"] -> dictate

            ["what", "is", "my"  , "net"   , "worth" ] -> do
                let (part1, part2) =
                        if odds 0.1 . mkStdGen . fromIntegral . messageId $ m
                            then ("You own a lavish ", " credits.")
                            else
                                ( "You are a dirt-poor peon. You have only "
                                , " credits to your name."
                                )
                credits <- getUser conn (userId author)
                    <&> maybe 0 (view userCredits)
                sendMessage channel $ part1 <> show credits <> part2

            ["what", "do", "i", "own"] -> do
                trinketIds <- getUser conn authorId
                    <&> maybe [] (view userTrinkets)
                trinkets <- mapM (getTrinket conn) trinketIds <&> catMaybes
                let trinketsDesc =
                        T.intercalate "\n"
                            .   fmap (\w -> "**" <> w <> "**")
                            $   uncurry displayTrinket
                            <$> zip trinketIds trinkets
                void . restCall' . CreateMessageEmbed channel "" $ mkEmbed
                    "Inventory"
                    trinketsDesc
                    []
                    Nothing

            ["what", "does", this, "stand", "for"] -> do
                pnppc <- liftIO $ acronym this
                sendMessage channel $ T.unwords pnppc

            ("rummage" : "in" : location) -> do
                userData <- getUser conn authorId <&> fromMaybe def
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
                            conn
                            (if odds 0.18 rng then Rare else Common)
                        void
                            . modifyUser conn authorId
                            $ (over userTrinkets (tId :) . over userCredits pred
                              )

                        let embedDesc =
                                "You find **"
                                    <> displayTrinket tId trinket
                                    <> "**."
                            postDesc =
                                "You look around in "
                                    <> unwords location
                                    <> " and find..."
                        void
                            . restCall'
                            . CreateMessageEmbed channel (voiceFilter postDesc)
                            $ mkEmbed "Rummage" embedDesc [] Nothing

            ("how" : "many" : things) -> do
                number :: Double <- liftIO normalIO <&> (exp . (+ 4) . (* 6))
                sendMessage channel
                    $  show (round number :: Integer)
                    <> " "
                    <> T.unwords things

            ("what" : theFuck) -> do
                output <-
                    getGPT
                        (  makePrompt
                              [ "Q: what is 2 + 2? A: 4"
                              , "Q: what is the meaning of life? A: go fuck yourself"
                              , "Q: what are you doing step bro? A: :flushed:"
                              , "Q: what is the eighth circle of hell called? A: malebolge"
                              ]
                        <> " Q: what "
                        <> unwords theFuck
                        <> "? A:"
                        )
                    <&> fromMaybe ""
                    .   listToMaybe
                    .   lines
                    .   T.drop 1
                sendMessage channel output

            ("who" : didThis) -> do
                randomN :: Double <- newStdGen <&> fst . random
                randomMember      <- if randomN < 0.75
                    then
                        (do
                            general <- getGeneralChannel
                            restCall'
                                    (GetChannelMessages
                                        (channelId general)
                                        (100, LatestMessages)
                                    )
                                >>= ( (<&> messageAuthor)
                                    . (newStdGen <&>)
                                    . randomChoice
                                    )
                                >>= userToMember
                                <&> fromJust
                        )
                    else getMembers >>= ((newStdGen <&>) . randomChoice)
                sendMessage channel
                    $  "<@"
                    <> (show . userId . memberUser) randomMember
                    <> "> "
                    <> T.unwords didThis

            ("flaunt" : goods) -> do
                case parseTrinkets . unwords $ goods of
                    Left err ->
                        sendMessage channel
                            $  "What the fuck is this? ```"
                            <> show err
                            <> "```"
                    Right flauntedTrinkets -> do
                        trinketIds <- getUser conn authorId
                            <&> maybe [] (view userTrinkets)
                        if flauntedTrinkets
                            == intersect flauntedTrinkets trinketIds
                        then
                            do
                                trinkets <-
                                    mapM (getTrinket conn) flauntedTrinkets
                                        <&> catMaybes
                                let display =
                                        T.intercalate "\n"
                                            .   fmap (\w -> "**" <> w <> "**")
                                            $   uncurry displayTrinket
                                            <$> zip flauntedTrinkets trinkets
                                void
                                    . restCall'
                                    . CreateMessageEmbed
                                          channel
                                          (voiceFilter
                                              "You wish to display your wealth?"
                                          )
                                    $ mkEmbed "Goods (PITIFUL)"
                                              display
                                              []
                                              Nothing
                        else
                            do
                                sendMessage
                                    channel
                                    "You don't own the goods you so shamelessly try to flaunt, and now you own even less. Credits, that is."
                                void $ modifyUser conn
                                                  authorId
                                                  (over userCredits pred)

            ("ponder" : life) -> do
                pontificate (messageChannel m) . T.unwords $ life

            ["update", "the", "teams" ] -> updateTeamRoles conn

            ["show"  , "the", "points"] -> do
                Just firstData  <- getTeam conn First
                Just secondData <- getTeam conn Second
                firstTName      <- getTeamRole conn First <&> roleName
                secondTName     <- getTeamRole conn Second <&> roleName

                let firstPoints  = firstData ^. teamPoints
                let secondPoints = secondData ^. teamPoints

                sendMessage
                    channel
                    (  firstTName
                    <> " has "
                    <> show firstPoints
                    <> " points.\n"
                    <> secondTName
                    <> " has "
                    <> show secondPoints
                    <> " points."
                    )

            ["i", "need", "help"] -> do
                (rng1, rng2) <- newStdGen <&> split
                randomWord <- liftIO getWordList <&> flip randomChoice rng1
                adj <- liftIO $ liftM2 randomChoice getAdjList getStdGen
                let
                    prompt =
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
                          channel
                          (voiceFilter "I will help you, but only out of pity: "
                          )
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

            ["time", "for", "bed"] -> do
                stopDict conn

            "offer" : _ ->
                sendMessage channel "what the fuck are you talking about?"

            "clear" : "the" : "roles" : _ -> getMembers >>= mapConcurrently_
                (\m' -> mapConcurrently_
                    (restCall . RemoveGuildMemberRole
                        pnppcId
                        (userId . memberUser $ m')
                    )
                    (memberRoles m')
                )

            ["merrier", "christmas"] -> do
                getNewTrinket conn Rare >>= sendMessage channel . displayTrinket
                    0

            ["merry", "christmas"] -> do
                getNewTrinket conn Common
                    >>= sendMessage channel
                    .   displayTrinket 0

            _ -> handleMessage conn m
        else pure ()
  where
    stripPuncRight = T.reverse . T.dropWhile isPunctuation . T.reverse

    content        = T.toLower . messageText $ m
    channel        = messageChannel m
    author         = messageAuthor m
    authorId       = userId author


-- other messages
-----------------

-- | Handle a message assuming that it isn't a command.
handleMessage :: DB.Connection -> Message -> DH ()
handleMessage conn m = do
    handleCommand' conn m -- will replace handleCommand soon
    handleOwned m
    handlePontificate m
    handleForbidden conn m


-- events
---------

minutes, hours, days :: Double -> Double
minutes = (* 60)
hours = (* 3600)
days = (* 86400)

data RandomEvent = RandomEvent
    { avgDelay    :: Double
    , randomEvent :: DB.Connection -> DH ()
    }

data ScheduledEvent = ScheduledEvent
    { absDelay       :: Double
    , scheduledEvent :: DB.Connection -> DH ()
    }

randomEvents :: [RandomEvent]
randomEvents =
    [ -- gmposting and gnposting
      RandomEvent { avgDelay    = days 1
                  , randomEvent = const $ sendMessageToGeneral "gm"
                  }
    , RandomEvent { avgDelay    = days 1
                  , randomEvent = const $ sendMessageToGeneral "gn"
                  }
    -- declarations and decrees
    , RandomEvent { avgDelay = minutes 90, randomEvent = const dictate }
    ]

scheduledEvents :: [ScheduledEvent]
scheduledEvents =
    [ ScheduledEvent { absDelay       = hours 2
                     , scheduledEvent = updateForbiddenWords
                     }
    ]

performRandomEvents :: DB.Connection -> DH ()
performRandomEvents conn = do
    threadDelay 100000
    void . forkIO $ mapConcurrently_ maybePerformRandomEvent randomEvents
    performRandomEvents conn

  where
    maybePerformRandomEvent (RandomEvent rngDelay event) = do
        rng <- newStdGen
        when (odds (0.1 / rngDelay) rng) $ event conn

startScheduledEvents :: DB.Connection -> DH ()
startScheduledEvents conn = do
    mapConcurrently_ scheduledEventLoop scheduledEvents
  where
    scheduledEventLoop sched@(ScheduledEvent delay event) = do
        -- Sleep for the required amount of time, noting that this is in nanoseconds.
        threadDelay . secsToUs $ delay
        event conn
        scheduledEventLoop sched
    secsToUs = round . (* 1e6)


-- main
-------

stopDict :: DB.Connection -> DH ()
stopDict conn = do
    sendMessageToGeneral "I'm so tired..."
    liftIO $ DB.disconnect conn
    stopDiscord

startHandler :: DB.Connection -> DH ()
startHandler conn = do
    sendMessageToGeneral "Rise and shine!"
    mapConcurrently_
        forkIO
        [ unbanUsersFromGeneral
        , performRandomEvents conn
        , startScheduledEvents conn
        , updateTeamRoles conn
        , forgiveDebt
        , threadDelay 5000000 >> updateForbiddenWords conn
        ]
  where
    forgiveDebt = getMembers >>= mapConcurrently_
        (\m -> modifyUser conn (userId . memberUser $ m)
            $ over userCredits (max 0)
        )

    unbanUsersFromGeneral = do
        general <- getGeneralChannel
        getMembers >>= mapConcurrently_
            (\m -> do
                setUserPermsInChannel True
                                      (channelId general)
                                      (userId . memberUser $ m)
                                      0x800
            )

eventHandler :: DB.Connection -> Event -> DH ()
eventHandler conn = \case
    MessageCreate m    -> handleCommand conn m
    GuildMemberAdd _ _ -> updateTeamRoles conn
    _                  -> return ()

main :: IO ()
main = do
    token <- readFile "token.txt"
    conn  <- DB.checkedConnect DB.defaultConnectInfo
    void . runDiscord $ def { discordToken   = fromString token
                            , discordOnStart = startHandler conn
                            , discordOnEvent = eventHandler conn
                            }
