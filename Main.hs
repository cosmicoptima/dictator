{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main
    ( main
    ) where

import           Prelude                        ( (!!) )
import           Relude                  hiding ( First
                                                , get
                                                )

import           Discord
import           Discord.Internal.Rest.Prelude  ( Request )
import           Discord.Requests
import           Discord.Types

import           Control.Lens            hiding ( Context )
import           Control.Monad.Random           ( evalRandIO )
import           Data.Aeson
import           Data.Bits                      ( shiftL )
import           Data.Colour                    ( Colour )
import           Data.Colour.Palette.RandomColor
                                                ( randomColor )
import           Data.Colour.Palette.Types      ( Hue
                                                    ( HueBlue
                                                    , HueRandom
                                                    , HueRed
                                                    , HueYellow
                                                    )
                                                , Luminosity(LumLight)
                                                )
import           Data.Colour.SRGB.Linear        ( RGB
                                                    ( channelBlue
                                                    , channelGreen
                                                    , channelRed
                                                    )
                                                , toRGB
                                                )
import           Data.Default
import           Data.Maybe                     ( fromJust )
import           Data.Scientific                ( Scientific
                                                , fromFloatDigits
                                                )
import qualified Data.Text                     as T
import           Network.Wreq                   ( get
                                                , post
                                                , responseBody
                                                )
import           System.IO.Error
import           System.Random
import           UnliftIO                       ( mapConcurrently_ )
import           UnliftIO.Concurrent            ( forkIO
                                                , threadDelay
                                                )
import           UnliftIO.Exception


-- utilities
------------

randomChoice :: [a] -> StdGen -> a
randomChoice xs rng = xs !! n where n = fst $ randomR (0, length xs - 1) rng

data MessageFragment = TextBlock Text | CodeBlock Text

fragmentText :: MessageFragment -> Text
fragmentText (TextBlock t) = t
fragmentText (CodeBlock t) = t

-- | Split a message into segments of code blocks and non-code-blocks.
messageSplit :: Text -> [MessageFragment]
messageSplit = filter (not . T.null . fragmentText) . splitMode True
  where
    isTick = (== '`')
    splitMode mode msg = if not $ T.null msg
        then ctor (T.takeWhile (not . isTick) msg) : splitMode
            (not mode)
            (tail' . T.dropWhile (not . isTick) $ msg)
        else []
        where ctor = if mode then TextBlock else CodeBlock
    tail' msg = if T.null msg then T.empty else T.tail msg

-- | Filter a message into dictator's voice, excluding code blocks.
voiceFilter :: Text -> Text
voiceFilter = T.concat . map format . messageSplit . T.strip
  where
    format (TextBlock t) = "**__" <> T.toUpper t <> "__**"
    format (CodeBlock t) = "```" <> t <> "```"

-- | Tokenize a message into individual words.
tokenizeMessage :: Text -> [Text]
tokenizeMessage =
    words . T.filter (not . isPunc) . T.concat . dropCode . messageSplit
  where
    punc :: String
    punc = "!?{}&>\"()|<[@]_+*:^p=;\\#£-/~%,.'"
    isPunc p = elem p punc
    -- Probably something built in to do this kind of work
    dropCode (TextBlock t : ts) = t : dropCode ts
    dropCode (CodeBlock _ : ts) = dropCode ts
    dropCode []                 = []


-- GPT
------

data GPTOpts = GPTOpts
    { temperature :: Scientific
    , topK        :: Int
    , topP        :: Scientific
    }

instance Default GPTOpts where
    def = GPTOpts { temperature = 0.8, topK = 40, topP = 1 }

newtype TextSynthRes = TextSynthRes { fromGPTRes :: Text }
instance FromJSON TextSynthRes where
    parseJSON =
        withObject "TextSynthRes" ((.: "text") >=> return . TextSynthRes)

getGPT :: Text -> DH Text
getGPT = getGPTWith def

getGPTWith :: GPTOpts -> Text -> DH Text
getGPTWith GPTOpts { temperature = t, topK = k, topP = p } prompt = do
    res <- liftIO $ post
        "https://bellard.org/textsynth/api/v1/engines/gptj_6B/completions"
        (object
            [ ("prompt"     , String prompt)
            , ("seed"       , Number 0)
            , ("stream"     , Bool False)
            , ("temperature", Number t)
            , ("top_k"      , Number (intToSci k))
            , ("top_p"      , Number p)
            ]
        )
    either (debugDie . fromString) (return . fromGPTRes)
        . eitherDecode
        . view responseBody
        $ res
    where intToSci = (fromFloatDigits :: Double -> Scientific) . toEnum

makePrompt :: [Text] -> Text
makePrompt = (<> "\n-") . unlines . map (" -" <>)

getGPTFromExamples :: [Text] -> DH Text
getGPTFromExamples = getGPT . makePrompt

-- Discord
----------

pnppcId :: GuildId
pnppcId = 878376227428245555

dictId :: UserId
dictId = 878385073735467060

isDict :: User -> Bool
isDict = (== dictId) . userId

type DH = DiscordHandler -- `DiscordHandler` is an ugly name!

-- | like `restCall`, but simply crashes if there is an error
restCall' :: (FromJSON a, Request (r a)) => r a -> DH a
restCall' = restCall >=> either (debugDie . show) return

getGuild :: DH Guild
getGuild = restCall' $ GetGuild pnppcId

getMembers :: DH [GuildMember]
getMembers =
    restCall' $ ListGuildMembers pnppcId $ GuildMembersTiming (Just 100) Nothing

userToMember :: User -> DH (Maybe GuildMember)
userToMember u = getMembers <&> find ((== u) . memberUser)

getChannelByID :: Snowflake -> DH Channel
getChannelByID = restCall' . GetChannel

getChannelByMessage :: Message -> DH Channel
getChannelByMessage = getChannelByID . messageChannel

getChannelNamed :: Text -> DH (Maybe Channel)
getChannelNamed name = do
    channels <- restCall' $ GetGuildChannels pnppcId
    return . find ((== name) . channelName) $ channels

getGeneralChannel :: DH Channel
getGeneralChannel =
    getChannelNamed "general" >>= maybe (die "#general doesn't exist") return

sendUnfilteredMessage :: ChannelId -> Text -> DH ()
sendUnfilteredMessage channel = void . restCall' . CreateMessage channel

sendMessage :: ChannelId -> Text -> DH ()
sendMessage channel = sendUnfilteredMessage channel . voiceFilter

sendMessageToGeneral :: Text -> DH ()
sendMessageToGeneral text =
    getGeneralChannel >>= flip sendMessage text . channelId

{-# WARNING debugPutStr "please don't flood #general" #-}
debugPutStr :: Text -> DH ()
debugPutStr t = sendMessageToGeneral
    (fromString . ("```\n" <>) . (<> "\n```") . take 1900 . toString $ t)

{-# WARNING debugPrint "please don't flood #general"  #-}
debugPrint :: Show a => a -> DH ()
debugPrint = debugPutStr . show

{-# WARNING debugDie "please don't flood #general"  #-}
debugDie :: Text -> DH a
debugDie m = debugPutStr m >> die (toString m)

reactToMessage :: Text -> Message -> DH ()
reactToMessage e m =
    restCall' $ CreateReaction (messageChannel m, messageId m) e

getRoleNamed :: Text -> DH (Maybe Role)
getRoleNamed name = do
    roles <- restCall' $ GetGuildRoles pnppcId
    return . find ((== name) . roleName) $ roles

getEveryoneRole :: DH Role
getEveryoneRole =
    -- Apparently the @ is needed. Why.
    getRoleNamed "@everyone"
        >>= maybe (die "@everyone doesn't exist. wait, what?") return

setUserPermsInChannel :: Bool -> ChannelId -> UserId -> Integer -> DH ()
setUserPermsInChannel allow channel user perms = do
    restCall' $ EditChannelPermissions channel
                                       (overwriteId permsId)
                                       permsOptsAllow
    return ()
  where
    permsId = Overwrite user "member" toAllow toDeny
    permsOptsAllow =
        ChannelPermissionsOpts toAllow toDeny ChannelPermissionsOptsUser

    toAllow = if allow then perms else 0
    toDeny  = if allow then 0 else perms

-- dictator!
------------

data TeamPoints = TeamPoints
    { _firstPoints  :: Int
    , _secondPoints :: Int
    }
    deriving Generic

newtype UserData = UserData { _credits :: Int } deriving Generic

data Context = Context
    { _forbiddenWords     :: [Text]
    , _forbiddanceMessage :: Maybe MessageId
    , _teamNames          :: Maybe (Text, Text)
    , _teamPoints         :: TeamPoints
    , _userData           :: Maybe (Map Snowflake UserData)
    }
    deriving Generic

instance Default Context where
    def = Context { _forbiddenWords     = []
                  , _forbiddanceMessage = Nothing
                  , _teamNames          = Nothing
                  , _teamPoints         = TeamPoints 0 0
                  , _userData           = Just []
                  }

instance FromJSON TeamPoints
instance ToJSON TeamPoints

instance FromJSONKey Snowflake
instance ToJSONKey Snowflake

instance FromJSON UserData
instance ToJSON UserData

instance FromJSON Context
instance ToJSON Context

makeLenses ''Context
makeLenses ''TeamPoints


ownedEmoji :: Text
ownedEmoji = "owned:899536714773717012"


data Team = First | Second | Neutral

firstTeamRole :: Context -> DH Role
firstTeamRole ctx =
    getRoleNamed (fromMaybe "???" . fmap fst . view teamNames $ ctx)
        >>= maybe (debugDie "first team role doesn't exist") return

secondTeamRole :: Context -> DH Role
secondTeamRole ctx =
    getRoleNamed (fromMaybe "???" . fmap snd . view teamNames $ ctx)
        >>= maybe (debugDie "second team role doesn't exist") return

getTeam :: Context -> GuildMember -> DH Team
getTeam ctx m = do
    let roles = memberRoles m
    firstT  <- firstTeamRole ctx <&> roleId
    secondT <- secondTeamRole ctx <&> roleId
    return if
        | any (== firstT) roles  -> First
        | any (== secondT) roles -> Second
        | otherwise              -> Neutral


getWordList :: DH [Text]
getWordList =
    liftIO
        $   get "https://www.mit.edu/~ecprice/wordlist.10000"
        <&> lines
        .   decodeUtf8
        .   view responseBody


-- | Handle a message assuming it's a command. If it isn't, fire off the handler for regular messages.
handleCommand :: IORef Context -> Message -> DH ()
handleCommand ctxRef m = do
    if not . userIsBot . messageAuthor $ m
        then case (words . messageText) m of
            ["tell", "me", "about", "yourself"] ->
                getGeneralChannel
                    >>= flip
                            sendUnfilteredMessage
                            (  voiceFilter
                                    "this is a server about collectively modifying the bot that governs it... as long as i allow it, of course."
                            <> " https://github.com/cosmicoptima/dictator"
                            )
                    .   channelId

            ["bool"] -> do
                (rngGPT, rngBool) <- newStdGen >>= return . split

                if fst (random rngGPT) > (0.3 :: Double)
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
                        output <- getGPTFromExamples examples
                        sendMessage channel $ case lines output of
                            l : _ -> l
                            []    -> "idk"

            ["gm"] -> unless (userIsBot . messageAuthor $ m) $ do
                rng <- newStdGen
                sendMessage channel
                    $ randomChoice ("fuck off" : replicate 4 "gm") rng

            "gpttest" : p -> do
                output <- (getGPT . unwords) p
                sendMessage channel output

            ["kindly", "undo", "my", "fuckup"] -> do
                ctx     <- readIORef ctxRef
                let (firstTName, secondTName) = fromMaybe ("???", "???") $ ctx ^. teamNames in do
                    fuckup1 <- getRoleNamed firstTName
                    fuckup2 <- getRoleNamed secondTName
                    restCall'
                        . DeleteGuildRole pnppcId
                        . roleId
                        . fromJust
                        $ fuckup1
                    restCall'
                        . DeleteGuildRole pnppcId
                        . roleId
                        . fromJust
                        $ fuckup2

                    modifyIORef
                        ctxRef
                        (set teamNames $ Just ("remaining democrat", "add divide"))
                    modifyIORef
                        ctxRef
                        (set teamNames $ Just ("remaining democrat", "add divide"))

            ["show", "the", "points"] -> do
                ctx <- readIORef ctxRef
                let (firstTName, secondTName) =
                        fromMaybe ("???", "???") $ ctx ^. teamNames
                    points = ctx ^. teamPoints
                    (firstT, secondT) =
                        (points ^. firstPoints, points ^. secondPoints)
                sendMessageToGeneral
                    (  firstTName
                    <> " "
                    <> show firstT
                    <> " - "
                    <> secondTName
                    <> " "
                    <> show secondT
                    )

            ["restart", "yourself"] -> do
                stopDict ctxRef

            _ -> handleMessage ctxRef m
        else pure ()
    where channel = messageChannel m

-- | Handle a message assuming that it isn't a command.
handleMessage :: IORef Context -> Message -> DH ()
handleMessage ctxRef m = do
    if T.isInfixOf "owned" $ messageText m
        then do
            (rngCeleste, rngEmoji) <- newStdGen <&> split
            let emoji = randomChoice [ownedEmoji, "rofl", "skull"] rngEmoji

            if ((== 140541286498304000) . userId . messageAuthor) m
                then do
                    randomChoice
                        [ sendMessageToGeneral "shut the fuck up celeste."
                        , reactToMessage emoji m
                        ]
                        rngCeleste
                else reactToMessage emoji m
        else pure ()

    rng <- getStdGen
    if (fst . random) rng < (0.01 :: Double)
        then do
            response <-
                getGPT $ "Dictator's thoughts on " <> messageText m <> ":\n"
            sendMessage channel $ case lines response of
                (_ : line : _) -> line
                (line     : _) -> line
                _              -> response
        else return ()

    forbiddenWords' <- readIORef ctxRef <&> view forbiddenWords
    if messageForbidden forbiddenWords' $ messageText m
        then (userToMember . messageAuthor) m >>= maybe (return ()) timeoutUser
        else pure ()
  where
    channel = messageChannel m
    messageForbidden wordList =
        isJust . find (`elem` wordList) . tokenizeMessage

    timeoutUser user = do
        ctx <- readIORef ctxRef
        let (firstTName, secondTName) =
                fromMaybe ("???", "???") (ctx ^. teamNames)
        void . forkIO $ getTeam ctx user >>= \case
            First -> do
                sendMessageToGeneral $ firstTName <> "s destroyed"
                modifyIORef ctxRef $ over teamPoints $ over secondPoints succ
            Second -> do
                sendMessageToGeneral $ secondTName <> "s destroyed"
                modifyIORef ctxRef $ over teamPoints $ over firstPoints succ
            Neutral -> return ()
        setUserPermsInChannel False
                              (messageChannel m)
                              (userId . memberUser $ user)
                              0x800
        restCall' $ DeleteMessage (messageChannel m, messageId m)
        -- 10 seconds as microseconds
        threadDelay 10000000
        setUserPermsInChannel True
                              (messageChannel m)
                              (userId . memberUser $ user)
                              0x800


seconds, minutes, hours, days :: Double -> Double
seconds = (* 1)
minutes = (* 60)
hours = (* 3600)
days = (* 86400)

data RandomEvent = RandomEvent
    { avgDelay    :: Double
    , randomEvent :: DH ()
    }

randomEvents :: [RandomEvent]
randomEvents =
    [ -- gmposting and gnposting
      RandomEvent { avgDelay = days 1, randomEvent = sendMessageToGeneral "gm" }
    , RandomEvent { avgDelay = days 1, randomEvent = sendMessageToGeneral "gn" }
    -- declarations and decrees
    , RandomEvent
        { avgDelay    = minutes 90
        , randomEvent = do
            output <- getGPTFromExamples
                [ "i hereby decree that all members are forbidden from using the message board"
                , "i hereby declare war upon the so-called \"elite\""
                , "i hereby decree that credits shall be reinstated"
                , "i hereby decree that no members may use lowercase in their postings"
                , "i hereby declare ignorantism the official ideology"
                , "i hereby ban the user gotham"
                , "i hereby declare myself better than you"
                ]
            case lines output of
                l : _ -> sendMessageToGeneral l
                []    -> return ()
        }
    ]

maybePerformRandomEvent :: RandomEvent -> DH ()
maybePerformRandomEvent r = do
    rng <- newStdGen
    if (fst . random) rng < (0.1 / avgDelay r) then randomEvent r else return ()

performRandomEvents :: DH ()
performRandomEvents = do
    threadDelay 100000
    void . forkIO $ mapConcurrently_ maybePerformRandomEvent randomEvents
    performRandomEvents

createOrModifyGuildRole :: Text -> ModifyGuildRoleOpts -> DH ()
createOrModifyGuildRole name roleOpts = getRoleNamed name >>= \case
    Just role -> do
        void . restCall' $ ModifyGuildRole pnppcId (roleId role) roleOpts
    Nothing -> do
        void . restCall' $ CreateGuildRole pnppcId roleOpts

updateTeamRoles :: IORef Context -> DH ()
updateTeamRoles ctxRef = do
    blueColor <- liftIO $ evalRandIO (randomColor HueBlue LumLight)
    redColor <- liftIO $ evalRandIO (randomColor HueRed LumLight)
    dictColor <- liftIO $ evalRandIO (randomColor HueRandom LumLight)

    wordList <- getWordList
    [firstTeamName, secondTeamName] <-
        replicateM 2
        $   replicateM 2 (newStdGen <&> randomChoice wordList)
        <&> T.unwords

    ctx <- readIORef ctxRef
    modifyIORef ctxRef (set teamNames $ Just (firstTeamName, secondTeamName))

    case ctx ^. teamNames of
        Nothing -> do
            void . restCall' $ CreateGuildRole
                pnppcId
                (teamRoleOpts firstTeamName $ convertColor blueColor)
            void . restCall' $ CreateGuildRole
                pnppcId
                (teamRoleOpts secondTeamName $ convertColor redColor)
        Just (f, s) -> do
            createOrModifyGuildRole f
                $ teamRoleOpts firstTeamName
                $ convertColor blueColor
            createOrModifyGuildRole s
                $ teamRoleOpts secondTeamName
                $ convertColor redColor

    createOrModifyGuildRole "leader" $ teamRoleOpts "leader" $ convertColor
        dictColor
    getRoleNamed "leader" >>= \case
        Just r  -> restCall' . AddGuildMemberRole pnppcId dictId $ roleId r
        Nothing -> pure ()

    newCtx  <- readIORef ctxRef

    firstT  <- firstTeamRole newCtx <&> roleId
    secondT <- secondTeamRole newCtx <&> roleId

    getMembers >>= mapM_
        (\m ->
            let memberId = userId . memberUser $ m
            in
                if memberId /= dictId
                    then do
                        rng     <- newStdGen
                        hasRole <-
                            restCall' (GetGuildMember pnppcId memberId)
                            <&> any (`elem` ([firstT, secondT] :: [Snowflake]))
                            .   memberRoles
                        unless hasRole $ restCall' $ AddGuildMemberRole
                            pnppcId
                            memberId
                            (if (fst . random) rng > (0.5 :: Double)
                                then firstT
                                else secondT
                            )
                    else pure ()
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


updateForbiddenWords :: IORef Context -> DH ()
updateForbiddenWords ctxRef = do
    fullWordList <- getWordList
    wordList     <- replicateM 10 (newStdGen <&> randomChoice fullWordList)

    modifyIORef ctxRef $ set forbiddenWords wordList
    general   <- getGeneralChannel <&> channelId

    forbidPin <- readIORef ctxRef <&> view forbiddanceMessage
    case forbidPin of
        Just pin -> do
            void . restCall' $ EditMessage (general, pin)
                                           (warningText wordList)
                                           Nothing
        Nothing -> do
            pinId <- restCall' . CreateMessage general $ warningText wordList
            restCall' $ AddPinnedMessage (general, messageId pinId)
            modifyIORef ctxRef . set forbiddanceMessage . Just $ messageId pinId

    void . restCall' $ ModifyChannel
        general
        (ModifyChannelOpts Nothing
                           Nothing
                           (Just $ warningText wordList)
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
        )
    where warningText = T.intercalate ", "

stopDict :: IORef Context -> DH ()
stopDict ctxRef = do
    sendMessageToGeneral "restarting"
    ctx <- readIORef ctxRef
    writeFileLBS "data.json" . encode $ ctx
    stopDiscord


startHandler :: IORef Context -> DH ()
startHandler ctxRef = do
    sendMessageToGeneral "hello, world!"
    void . forkIO $ performRandomEvents
    void . forkIO $ updateTeamRoles ctxRef
    void . forkIO $ updateForbiddenWords ctxRef

eventHandler :: IORef Context -> Event -> DH ()
eventHandler ctxRef event = case event of
    MessageCreate m    -> handleCommand ctxRef m
    GuildMemberAdd _ _ -> updateTeamRoles ctxRef
    _                  -> return ()

main :: IO ()
main = do
    token   <- readFile "token.txt"
    ctxJSON <- (readFileLBS "data.json" <&> Just) `catch` \e ->
        if isDoesNotExistError e then return Nothing else throwIO e
    let ctx = maybe def (fromMaybe def . decode) ctxJSON

    ctxRef <- newIORef ctx
    void . runDiscord $ def { discordToken   = fromString token
                            , discordOnStart = startHandler ctxRef
                            , discordOnEvent = eventHandler ctxRef
                            }
