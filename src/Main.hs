{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans          #-}

module Main
    ( main
    ) where

import           Relude                  hiding ( First
                                                , get
                                                )

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

import           Datatypes
import           DiscordUtils
import           Economy
import           GenText
import           Utils

import           Control.Lens            hiding ( Context )
import           Control.Monad.Random           ( evalRandIO
                                                , liftM2
                                                )
import           Data.Aeson
import           Data.Bits                      ( shiftL )
import           Data.Char
import           Data.Colour                    ( Colour )
import           Data.Colour.Palette.RandomColor
                                                ( randomColor )
import           Data.Colour.Palette.Types      ( Hue(HueRandom)
                                                , Luminosity(LumLight)
                                                )
import           Data.Colour.SRGB.Linear        ( RGB
                                                    ( channelBlue
                                                    , channelGreen
                                                    , channelRed
                                                    )
                                                , toRGB
                                                )
import           Data.List                      ( intersect )
import           Data.Maybe
import           Data.Random.Normal
import qualified Data.Text                     as T
import qualified Database.Redis                as DB
import           Items                          ( parseTrinkets )
import           System.Random
import           System.Random.Shuffle          ( shuffle' )
import           Text.Parsec             hiding ( token
                                                , try
                                                )
import           UnliftIO.Async                 ( forConcurrently_
                                                , mapConcurrently_
                                                )
import           UnliftIO.Concurrent            ( forkIO
                                                , threadDelay
                                                )

-- dictator!
------------

instance FromJSON Team
instance ToJSON Team

instance FromJSONKey Snowflake
instance ToJSONKey Snowflake


ownedEmoji :: Text
ownedEmoji = "owned:899536714773717012"

createAndReturnRole :: DB.Connection -> Team -> DH Role
createAndReturnRole conn team = do
    role <- restCall' $ CreateGuildRole
        pnppcId
        (ModifyGuildRoleOpts (Just $ show team)
                             Nothing
                             (Just 1)
                             (Just True)
                             (Just True)
        )
    liftIO . setTeamData conn team $ def { teamRole = Just $ roleId role }
    return role

getTeamRole :: DB.Connection -> Team -> DH Role
getTeamRole conn team = do
    teamData <- liftIO $ getTeamData conn team <&> fromMaybe def
    case teamRole teamData of
        Just rId ->
            getRoleById rId >>= maybe (createAndReturnRole conn team) return
        Nothing -> createAndReturnRole conn team

getTeamId :: DB.Connection -> Team -> DH RoleId
getTeamId conn team = getTeamRole conn team <&> roleId

pontificateOn :: ChannelId -> Text -> DH ()
pontificateOn channel what = do
    adj      <- liftIO $ liftM2 randomChoice getAdjList getStdGen
    response <-
        getGPT $ "Dictator's " <> adj <> " thoughts on " <> what <> ":\n"
    sendMessage channel $ case lines response of
        (_ : line : _) -> line
        (line     : _) -> line
        _              -> response

awardTeamMembersCredit :: DB.Connection -> Team -> Double -> DH ()
awardTeamMembersCredit = awardTeamMembersCredit'  where
    awardTeamMembersCredit' conn rewardedTeam n =
        getMembers >>= mapConcurrently_
            (\m -> do
                let memberId = (userId . memberUser) m
                Just memberData <- liftIO $ getUserData conn memberId
                when (Just rewardedTeam == userTeam memberData) $ do
                    let
                        memberData' = memberData
                            { userCredits = userCredits memberData + n
                            }
                    liftIO $ setUserData conn memberId memberData'
            )

dictate :: DH ()
dictate = do
    adj    <- liftIO $ liftM2 randomChoice getAdjList getStdGen
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

            ["gm"] -> unless (userIsBot . messageAuthor $ m) $ do
                rng <- newStdGen
                sendMessage channel
                    $ randomChoice ("fuck off" : replicate 4 "gm") rng

            ["gn"] -> unless (userIsBot . messageAuthor $ m) $ do
                rng <- newStdGen
                sendMessage channel $ randomChoice
                    ("i plan to kill you in your sleep" : replicate 7 "gn")
                    rng

            ["what", "is", "your", "latest", "dictum"] -> dictate

            -- DO NOT RMEOVE
            ["froggy"] -> sendMessage
                channel
                "My little man, I don't know how to help you."

            ["what", "is", "my", "net", "worth"] -> do
                let (part1, part2) =
                        if odds 0.1 . mkStdGen . fromIntegral . messageId $ m
                            then ("You own a lavish ", " credits.")
                            else
                                ( "You are a dirt-poor peon. You have only "
                                , " credits to your name."
                                )
                credits <-
                    liftIO
                    $   getUserData conn (userId author)
                    <&> maybe 0 userCredits
                sendMessage channel $ part1 <> show credits <> part2

            ["what", "do", "i", "own"] -> do
                trinketIds <-
                    liftIO $ getUserData conn authorId <&> maybe [] userTrinkets
                trinkets <-
                    liftIO $ mapM (getTrinketData conn) trinketIds <&> catMaybes
                let trinketsDesc =
                        T.intercalate "\n"
                            .   fmap (\w -> "**" <> w <> "**")
                            $   uncurry showTrinket
                            <$> zip trinketIds trinkets
                void . restCall' . CreateMessageEmbed channel "" $ CreateEmbed
                    ""
                    ""
                    Nothing
                    "Inventory"
                    ""
                    Nothing
                    trinketsDesc
                    []
                    Nothing
                    ""
                    Nothing
                    Nothing

            -- ["incredibly", "merry", "christmas"] -> do
            --     rng <- newStdGen
            --     let rarity = if odds 0.3 rng then Rare else Common
            --     (_, trinket) <- mkNewTrinket conn rarity
            --     sendMessage channel
            --         $  "Merry Christmas! I got the world: "
            --         <> show trinket

            ["what", "does", this, "stand", "for"] -> do
                pnppc <- liftIO $ acronym this
                sendMessage channel $ T.unwords pnppc

            ("rummage" : "around" : "in" : location) -> do
                userData <- liftIO $ getUserData conn authorId <&> fromMaybe def
                if userCredits userData == 0
                    then sendMessage channel
                                     "You're too poor for that, so stop it."
                    else do
                        rng <- newStdGen
                        let rarity = if odds 0.3 rng then Rare else Common
                        (tId, trinket) <- mkNewTrinket conn rarity
                        let userData' = userData
                                { userTrinkets = tId : userTrinkets userData
                                , userCredits  = userCredits userData - 1
                                }
                        liftIO $ setUserData conn authorId userData'


                        let
                            embedDesc =
                                "You find **"
                                    <> showTrinket tId trinket
                                    <> "**."
                        let
                            postDesc =
                                "You look around in "
                                    <> unwords location
                                    <> " and find..."
                        void
                            . restCall'
                            . CreateMessageEmbed channel (voiceFilter postDesc)
                            $ CreateEmbed ""
                                          ""
                                          Nothing
                                          "Rummage"
                                          ""
                                          Nothing
                                          embedDesc
                                          []
                                          Nothing
                                          ""
                                          Nothing
                                          Nothing

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
                        trinketIds <-
                            liftIO
                            $   getUserData conn authorId
                            <&> maybe [] userTrinkets
                        if null $ intersect flauntedTrinkets trinketIds
                            then do
                                trinkets <-
                                    liftIO
                                    $   mapM (getTrinketData conn) trinketIds
                                    <&> catMaybes
                                let display =
                                        T.intercalate "\n"
                                            .   fmap (\w -> "**" <> w <> "**")
                                            $   uncurry showTrinket
                                            <$> zip trinketIds trinkets
                                void
                                    . restCall'
                                    . CreateMessageEmbed
                                          channel
                                          "You wish to display your wealth?"
                                    $ CreateEmbed ""
                                                  ""
                                                  Nothing
                                                  "Goods (PITYFUL)"
                                                  ""
                                                  Nothing
                                                  display
                                                  []
                                                  Nothing
                                                  ""
                                                  Nothing
                                                  Nothing
                            else do
                                sendMessage
                                    channel
                                    "You don't own the goods you so shamelessly try to flaunt, and now you own even less. Credits, that is."
                                void . liftIO $ modifyUserData
                                    conn
                                    authorId
                                    (over userCreditsL pred)




            ("ponder" : life) -> do
                pontificateOn (messageChannel m) . T.unwords $ life

            ["update", "the", "teams" ] -> updateTeamRoles conn

            ["show"  , "the", "points"] -> do
                Just firstData  <- liftIO $ getTeamData conn First
                Just secondData <- liftIO $ getTeamData conn Second
                firstTName      <- getTeamRole conn First <&> roleName
                secondTName     <- getTeamRole conn Second <&> roleName

                let firstPoints  = teamPoints firstData
                let secondPoints = teamPoints secondData

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
                    $ makeEmbed fields color
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

                makeEmbed fields color = CreateEmbed
                    "" -- author's name
                    "" -- author's url
                    Nothing -- author's icon
                    (  "These are the only "
                    <> (show . length) fields
                    <> " commands that exist."
                    ) -- title
                    "" -- url
                    Nothing -- thumbnail
                    "" -- description
                    (fmap makeField fields) -- fields
                    Nothing -- embed image
                    "" -- footer
                    Nothing -- embed icon
                    (Just color) -- colour

                makeField (name, desc) =
                    EmbedField (T.strip name) (T.strip desc) $ Just False


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
                trinket <- getNewTrinket conn Rare
                sendMessage channel $ showTrinket 0 trinket

            ["merry", "christmas"] -> do
                sendMessage channel "..."
                trinket <- getNewTrinket conn Common
                sendMessage channel $ showTrinket 0 trinket

            _ -> handleMessage conn m
        else pure ()
  where
    stripPuncRight = T.reverse . T.dropWhile isPunctuation . T.reverse

    content        = T.toLower . messageText $ m
    channel        = messageChannel m
    author         = messageAuthor m
    authorId       = userId author

-- | Handle a message assuming that it isn't a command.
handleMessage :: DB.Connection -> Message -> DH ()
handleMessage conn m = do
    when (T.isInfixOf "owned" content) $ do
        (rngCeleste, rngEmoji) <- newStdGen <&> split
        let emoji = randomChoice [ownedEmoji, ownedEmoji, "skull"] rngEmoji

        if ((== 140541286498304000) . userId . messageAuthor) m
            then do
                randomChoice
                    [ sendMessageToGeneral "shut the fuck up, celeste"
                    , reactToMessage emoji m
                    ]
                    rngCeleste
            else reactToMessage emoji m

    when (odds 0.02 . mkStdGen . fromIntegral . messageId $ m) $ do
        pontificateOn channel . messageText $ m

    Just (Just culpritTeam) <-
        liftIO $ getUserData conn (userId author) <&> fmap userTeam
    messageForbidden <- liftIO $ messageForbiddenWith content culpritTeam
    case messageForbidden of
        Just word -> do
            timeoutUser word authorId
            updateForbiddenWords conn
            awardTeamMembersCredit conn (otherTeam culpritTeam) 10
        Nothing -> return ()
  where
    content  = T.toLower . messageText $ m
    channel  = messageChannel m
    author   = messageAuthor m
    authorId = userId author

    messageForbiddenWith message team = do
        Just forbidden <- getTeamData conn team <&> fmap teamForbidden
        return $ find (`elem` forbidden) . tokenizeMessage $ message

    bannedWordMessage badWord badTeam goodTeam =
        "You arrogant little insect! Team "
            <> badTeam
            <> " clearly wish to disrespect my authority by uttering a word so vile as '"
            <> badWord
            <> "', so team "
            <> goodTeam
            <> " will be awarded 10 points."

    timeoutUser badWord user = do
        firstTName  <- getTeamRole conn First <&> roleName
        secondTName <- getTeamRole conn Second <&> roleName
        Just team   <- liftIO $ getUserData conn authorId <&> (userTeam =<<)
        case team of
            First -> do
                sendMessageToGeneral
                    $ bannedWordMessage badWord firstTName secondTName
            Second -> do
                sendMessageToGeneral
                    $ bannedWordMessage badWord secondTName firstTName
        void . liftIO $ modifyTeamData conn
                                       (otherTeam team)
                                       (over teamPointsL (+ 10))

        setUserPermsInChannel False (messageChannel m) user 0x800
        -- 15 seconds as microseconds
        threadDelay 15000000
        setUserPermsInChannel True (messageChannel m) user 0x800

seconds, minutes, hours, days :: Double -> Double
seconds = (* 1)
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

createOrModifyGuildRole :: Text -> ModifyGuildRoleOpts -> DH ()
createOrModifyGuildRole name roleOpts = getRoleNamed name >>= \case
    Just role -> do
        void . restCall' $ ModifyGuildRole pnppcId (roleId role) roleOpts
    Nothing -> do
        void . restCall' $ CreateGuildRole pnppcId roleOpts

createOrModifyGuildRoleById :: RoleId -> ModifyGuildRoleOpts -> DH ()
createOrModifyGuildRoleById rId roleOpts = getRoleById rId >>= \case
    Just role -> do
        void . restCall' $ ModifyGuildRole pnppcId (roleId role) roleOpts
    Nothing -> do
        void . restCall' $ CreateGuildRole pnppcId roleOpts

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

    firstId <- getTeamId conn First
    void . restCall' $ ModifyGuildRole
        pnppcId
        firstId
        (teamRoleOpts firstTeamName $ convertColor blueColor)

    secondId <- getTeamId conn Second
    void . restCall' $ ModifyGuildRole
        pnppcId
        secondId
        (teamRoleOpts secondTeamName $ convertColor redColor)

    createOrModifyGuildRole "leader" $ teamRoleOpts "leader" $ convertColor
        dictColor
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

                userData <- liftIO $ getUserData conn memberId <&> fromMaybe def
                memberTeam <- case userTeam userData of
                    Just team -> return team
                    Nothing   -> do
                        let userData' = userData & userTeamL ?~ newMemberTeam
                        liftIO $ setUserData conn memberId userData'
                        return newMemberTeam

                memberTeamId  <- getTeamId conn memberTeam
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
        firstId  <- getTeamId conn First
        secondId <- getTeamId conn Second
        return $ (firstId `elem` roles) || (secondId `elem` roles)


updateForbiddenWords :: DB.Connection -> DH ()
updateForbiddenWords conn = do
    fullWordList <- liftIO getWordList

    forM_
        ([First, Second] :: [Team])
        (\team -> do
            wordList <- replicateM 10 (newStdGen <&> randomChoice fullWordList)
            teamData <- liftIO $ getTeamData conn team <&> fromMaybe def
            let teamData' = teamData { teamForbidden = wordList }
            liftIO $ setTeamData conn team teamData'
        )

    general <- getGeneralChannel <&> channelId
    createOrUpdatePin general First
    createOrUpdatePin general Second
    return ()

  where
    createOrUpdatePin channel team = do
        teamData <- liftIO $ getTeamData conn team <&> fromMaybe def
        pinId    <- case teamWarning teamData of
            Just pin -> return pin
            Nothing  -> do
                pinId <- restCall' (CreateMessage channel "aa") <&> messageId
                liftIO $ setTeamData conn team $ teamData
                    { teamWarning = Just pinId
                    }
                restCall' $ AddPinnedMessage (channel, pinId)
                return pinId

        embed <- warningEmbed (teamForbidden teamData) team
        void . restCall' $ EditMessage (channel, pinId)
                                       (warning team)
                                       (Just embed)

    warning First =
        voiceFilter
            "The following words and terms are hereby illegal, forbidden, banned and struck from all records, forever: "
    warning Second =
        voiceFilter
            "I declare that the following so-called words do not exist, have never existed, and will continue to not exist: "

    warningEmbed wordList team = do
        role <- getTeamRole conn team
        return $ CreateEmbed
            "" -- author's name
            "" -- author's url
            Nothing -- author's icon
            ("Forbidden words for " <> roleName role <> ":") -- title
            "" -- url
            Nothing -- thumbnail
            (T.intercalate ", " wordList) -- description
            []-- fields
            Nothing -- embed image
            "" -- footer
            Nothing -- embed icon
            (Just . roleColor $ role) -- colour


stopDict :: DB.Connection -> DH ()
stopDict conn = do
    sendMessageToGeneral "I'm so tired..."
    liftIO $ DB.disconnect conn
    stopDiscord

startHandler :: DB.Connection -> DH ()
startHandler conn = do
    sendMessageToGeneral "rise and shine!"
    void . forkIO $ unbanUsersFromGeneral
    void . forkIO $ performRandomEvents conn
    void . forkIO $ startScheduledEvents conn
    void . forkIO $ updateTeamRoles conn
    void . forkIO $ do
        -- Wait for 5 seconds to avoid a race condition-ish thing
        threadDelay 5000000
        updateForbiddenWords conn
  where
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
