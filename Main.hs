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
import qualified Data.Map                      as Map
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

data MessageFragment
    = TextBlock Text
    | CodeBlock Text
    | Space
    | Newline
    deriving (Eq, Show)

fragmentText :: MessageFragment -> Text
fragmentText (TextBlock t) = t
fragmentText (CodeBlock t) = t
fragmentText Space         = " "
fragmentText Newline       = "\n"

-- | Split a message into segments of code blocks and non-code-blocks.
messageSplit :: Text -> [MessageFragment]
messageSplit = filter (not . isNullText) . splitMode True
  where
    isSpecial = (`elem` ([' ', '\n', '`'] :: [Char]))
    isTick    = (== '`')
    isNullText (TextBlock t) = T.null t
    isNullText (CodeBlock t) = T.null t
    isNullText _             = False

    splitMode mode msg = if not $ T.null msg
        then case T.head msg of
            ' ' | mode  -> Space : (splitMode mode . tail' $ msg)
            '\n' | mode -> Newline : (splitMode mode . tail' $ msg)
            '`'         -> splitMode (not mode) . tail' $ msg
            _ ->
                ctor (T.takeWhile (not . filt) msg)
                    : (splitMode mode . T.dropWhile (not . filt) $ msg)
        else []
      where
        ctor = if mode then TextBlock else CodeBlock
        filt = if mode then isSpecial else isTick
    tail' msg = if T.null msg then T.empty else T.tail msg

-- | Filter a message into dictator's voice, excluding code blocks.
voiceFilter :: Text -> Text
voiceFilter = T.concat . map format . messageSplit
  where
    format (TextBlock t) = "**__" <> T.toUpper t <> "__**"
    format (CodeBlock t) = "```" <> t <> "```"
    format Space         = " "
    format Newline       = "\n"

-- | Tokenize a message into individual words.
tokenizeMessage :: Text -> [Text]
tokenizeMessage =
    words
        . T.filter (not . isPunc)
        . T.concat
        . map fragmentText
        . filter (not . isCode)
        . messageSplit
  where
    punc :: String
    punc = "!?{}&>\"()|<[@]_+*:^p=;\\#£-/~%,.'"
    isPunc p = elem p punc
    -- Probably something built in to do this kind of work
    isCode (CodeBlock _) = True
    isCode _             = False

-- | Randomly choose true/false conveniently given a probability in [0.0, 1.0]
odds :: Double -> StdGen -> Bool
odds chance = (chance >) . fst . random


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

data Team = First | Second | Neutral deriving (Eq, Generic)

instance Default Team where
    def = Neutral

data UserData = UserData
    { _userCredits :: Int
    , _userTeam    :: Team
    }
    deriving Generic

instance Default UserData where
    def = UserData { _userCredits = 0, _userTeam = Neutral }

data TeamData = TeamData
    { _forbiddanceMessage :: Maybe MessageId
    , _forbiddenWords     :: [Text]
    , _teamName           :: Maybe Text
    , _teamPoints         :: Int
    }
    deriving Generic

data TeamPair = Pair
    { _firstTeam  :: TeamData
    , _secondTeam :: TeamData
    }
    deriving Generic

instance Default TeamPair where
    def = Pair def def

instance Default TeamData where
    def = TeamData { _forbiddenWords     = []
                   , _forbiddanceMessage = Nothing
                   , _teamName           = Nothing
                   , _teamPoints         = 0
                   }

data Context = Context
    { _teamData :: TeamPair
    , _userData :: Map Snowflake UserData
    }
    deriving Generic

instance Default Context where
    def = Context { _teamData = Pair def def, _userData = Map.empty }


instance FromJSON Team
instance ToJSON Team

instance FromJSONKey Snowflake
instance ToJSONKey Snowflake

instance FromJSON UserData
instance ToJSON UserData

instance FromJSON TeamPair
instance ToJSON TeamPair

instance FromJSON TeamData
instance ToJSON TeamData

instance FromJSON Context
instance ToJSON Context

makeLenses ''Context
makeLenses ''TeamData
makeLenses ''TeamPair
makeLenses ''UserData

ownedEmoji :: Text
ownedEmoji = "owned:899536714773717012"

firstTeamRole :: Context -> DH Role
firstTeamRole ctx =
    getRoleNamed
            ( fromMaybe "???"
            . view teamName
            . view firstTeam
            . view teamData
            $ ctx
            )
        >>= maybe (debugDie "first team role doesn't exist") return

secondTeamRole :: Context -> DH Role
secondTeamRole ctx =
    getRoleNamed
            ( fromMaybe "???"
            . view teamName
            . view secondTeam
            . view teamData
            $ ctx
            )
        >>= maybe (debugDie "second team role doesn't exist") return

getTeam :: UserId -> Context -> Team
getTeam m = _userTeam . fromMaybe def . Map.lookup m . _userData

getWordList :: DH [Text]
getWordList =
    liftIO
        $   get "https://www.mit.edu/~ecprice/wordlist.10000"
        <&> lines
        .   decodeUtf8
        .   view responseBody


pontificateOn :: ChannelId -> Text -> DH ()
pontificateOn channel what = do
    response <- getGPT $ "Dictator's thoughts on " <> what <> ":\n"
    sendMessage channel $ case lines response of
        (_ : line : _) -> line
        (line     : _) -> line
        _              -> response

userCredit :: Context -> UserId -> Int
userCredit ctx user =
    _userCredits . fromMaybe def . Map.lookup user . _userData $ ctx

awardTeamMembersCredit :: IORef Context -> Team -> Int -> DH ()
awardTeamMembersCredit = awardTeamMembersCredit'  where
    awardTeamMembersCredit' _ Neutral _ = return ()
    awardTeamMembersCredit' ctxRef rewardedTeam n =
        giveCreditWhere ctxRef (give n rewardedTeam)

    give :: Int -> Team -> (UserData -> UserData)
    give n rewardedTeam = \dat -> if _userTeam dat == rewardedTeam
        then dat { _userCredits = _userCredits dat + n }
        else dat

    giveCreditWhere :: IORef Context -> (UserData -> UserData) -> DH ()
    giveCreditWhere ctxRef func = do
        -- Insert any members who aren't inside the map
        getMembers >>= mapM_
            (\m -> modifyIORef ctxRef $ over userData $ Map.insertWith
                (flip const) -- Use old value if it exists
                (userId . memberUser $ m) -- For each member
                def -- Otherwise insert the default
            )
        modifyIORef ctxRef . over userData . Map.map $ func


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

            ("ponder" : life) -> do
                pontificateOn (messageChannel m) . T.unwords $ life

            ("gpttest" : p) -> do
                output <- (getGPT . unwords) p
                sendMessage channel output

            ["update", "the", "teams" ] -> updateTeamRoles ctxRef

            ["show"  , "the", "points"] -> do
                ctx <- readIORef ctxRef
                let firstTName =
                        fromMaybe "???"
                            . view teamName
                            . view firstTeam
                            . view teamData
                            $ ctx
                    secondTName =
                        fromMaybe "???"
                            . view teamName
                            . view secondTeam
                            . view teamData
                            $ ctx
                    firstPoints =
                        view teamPoints . view firstTeam . view teamData $ ctx
                    secondPoints =
                        view teamPoints . view secondTeam . view teamData $ ctx
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

            -- ["even", "the", "points"] -> do
            --     modifyIORef ctxRef . over teamPoints . over firstPoints . const $ 13
            --     modifyIORef ctxRef . over teamPoints . over secondPoints . const $ 13

            ["what", "is", "my", "net", "worth?"] -> do
                ctx <- readIORef ctxRef
                let (part1, part2) =
                        if odds 0.1 . mkStdGen . fromIntegral . messageId $ m
                            then ("You own a lavish ", " credits.")
                            else
                                ( "You are a dirt-poor peon. You have only "
                                , " credits to your name."
                                )
                sendMessage channel
                    $  part1
                    <> (show . userCredit ctx . userId $ author)
                    <> part2

            ["time", "for", "bed"] -> do
                stopDict ctxRef

            _ -> handleMessage ctxRef m
        else pure ()
  where
    channel = messageChannel m
    author  = messageAuthor m

-- | Handle a message assuming that it isn't a command.
handleMessage :: IORef Context -> Message -> DH ()
handleMessage ctxRef m = do
    if T.isInfixOf "owned" . messageText $ m
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
        else return ()

    if odds 0.1 . mkStdGen . fromIntegral . messageId $ m
        then do
            pontificateOn channel . messageText $ m
        else return ()

    ctx <- readIORef ctxRef
    let culpritTeam = getTeam author ctx
    if messageForbiddenIn ctx (messageText m) culpritTeam
        then do
            timeoutUser author
            updateForbiddenWords ctxRef
            awardTeamMembersCredit ctxRef (otherTeam . getTeam author $ ctx) 10
        else return ()
  where
    channel = messageChannel m
    author  = userId . messageAuthor $ m

    otherTeam First   = Second
    otherTeam Second  = First
    otherTeam Neutral = Neutral

    messageForbiddenIn ctx message team =
        let forbidden = case team of
                First   -> ctx ^. (teamData . firstTeam . forbiddenWords)
                Second  -> ctx ^. (teamData . secondTeam . forbiddenWords)
                Neutral -> []
        in  isJust
                . find (`elem` (forbidden :: [Text]))
                . tokenizeMessage
                $ message


    bannedWordMessage badTeam goodTeam =
        "You arrogant little insect! Team "
            <> badTeam
            <> " clearly wish to disrespect my authority, so team "
            <> goodTeam
            <> " will be awarded 10 points."

    timeoutUser user = do
        ctx <- readIORef ctxRef
        let firstTName =
                fromMaybe "???"
                    . view teamName
                    . view firstTeam
                    . view teamData
                    $ ctx
            secondTName =
                fromMaybe "???"
                    . view teamName
                    . view secondTeam
                    . view teamData
                    $ ctx
        case getTeam user ctx of
            First -> do
                sendMessageToGeneral $ bannedWordMessage firstTName secondTName
                modifyIORef ctxRef
                    . over teamData
                    . over secondTeam
                    . over teamPoints
                    $ (+ 10)
            Second -> do
                sendMessageToGeneral $ bannedWordMessage secondTName firstTName
                modifyIORef ctxRef
                    . over teamData
                    . over firstTeam
                    . over teamPoints
                    $ (+ 10)
            Neutral -> return ()

        setUserPermsInChannel False (messageChannel m) user 0x800
        restCall' $ DeleteMessage (messageChannel m, messageId m)
        -- 10 seconds as microseconds
        threadDelay 10000000
        setUserPermsInChannel True (messageChannel m) user 0x800


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
    if odds (0.1 / avgDelay r) rng then randomEvent r else return ()

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
    modifyIORef ctxRef . over teamData . over firstTeam . set teamName $ Just
        firstTeamName
    modifyIORef ctxRef . over teamData . over firstTeam . set teamName $ Just
        firstTeamName

    case ctx ^. (teamData . firstTeam . teamName) of
        Nothing -> do
            void . restCall' $ CreateGuildRole
                pnppcId
                (teamRoleOpts firstTeamName $ convertColor blueColor)

        Just f -> do
            createOrModifyGuildRole f
                $ teamRoleOpts firstTeamName
                $ convertColor blueColor

    case ctx ^. (teamData . secondTeam . teamName) of
        Nothing -> do
            void . restCall' $ CreateGuildRole
                pnppcId
                (teamRoleOpts secondTeamName $ convertColor redColor)
        Just s ->
            do
                    createOrModifyGuildRole s
                $ teamRoleOpts secondTeamName
                $ convertColor redColor


    createOrModifyGuildRole "leader" $ teamRoleOpts "leader" $ convertColor
        dictColor
    getRoleNamed "leader" >>= \case
        Just r  -> restCall' . AddGuildMemberRole pnppcId dictId $ roleId r
        Nothing -> return ()

    allMembers <- getMembers

    -- First insert any users who don't exist
    flip
        mapM_
        allMembers
        (\m ->
            let memberId = userId . memberUser $ m
            in  modifyIORef ctxRef . over userData $ Map.alter
                    (\case
                        Just existing -> Just existing
                        Nothing       -> Just def
                    )
                    memberId
        )

    -- uwu
    modifyIORef ctxRef . over userData $ Map.adjust
        (\dat -> dat { _userTeam = First })
        110161277707399168
    modifyIORef ctxRef . over userData $ Map.adjust
        (\dat -> dat { _userTeam = First })
        299608037101142026
    modifyIORef ctxRef . over userData $ Map.adjust
        (\dat -> dat { _userTeam = Second })
        140541286498304000
    modifyIORef ctxRef . over userData $ Map.adjust
        (\dat -> dat { _userTeam = Second })
        405193965260898315

    -- Second we update the roles in our userData
    flip
        mapM_
        allMembers
        (\m ->
            let memberId = userId . memberUser $ m
            in
                if memberId /= dictId
                    then do
                        rng <- newStdGen
                        case getTeam memberId ctx of
                            Neutral -> do
                                modifyIORef ctxRef . over userData $ Map.adjust
                                    (\dat -> dat
                                        { _userTeam = if odds 0.5 rng
                                                          then First
                                                          else Second
                                        }
                                    )
                                    memberId
                            _ -> return ()
                    else return ()
        )

    -- Then we update them on discord
    ctx2       <- readIORef ctxRef
    firstRole  <- firstTeamRole ctx2
    secondRole <- secondTeamRole ctx2
    flip
        mapM_
        allMembers
        (\m ->
            let memberId = userId . memberUser $ m
            in
                case getTeam memberId ctx2 of
                    Neutral -> return ()
                    First ->
                        restCall'
                            . AddGuildMemberRole pnppcId memberId
                            . roleId
                            $ firstRole
                    Second ->
                        restCall'
                            . AddGuildMemberRole pnppcId memberId
                            . roleId
                            $ secondRole
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
    fullWordList   <- getWordList
    firstWordList  <- replicateM 10 (newStdGen <&> randomChoice fullWordList)
    secondWordList <- replicateM 10 (newStdGen <&> randomChoice fullWordList)

    modifyIORef ctxRef
        . over teamData
        . over firstTeam
        . set forbiddenWords
        $ firstWordList
    modifyIORef ctxRef
        . over teamData
        . over firstTeam
        . set forbiddenWords
        $ secondWordList
    general        <- getGeneralChannel <&> channelId

    firstForbidPin <- readIORef ctxRef
        <&> view (teamData . firstTeam . forbiddanceMessage)
    secondForbidPin <- readIORef ctxRef
        <&> view (teamData . secondTeam . forbiddanceMessage)
    case firstForbidPin of
        Just pin -> do
            void . updateForbidPin general pin $ firstWordList
        Nothing -> do
            pinId <- createForbidPin general firstWordList
            modifyIORef ctxRef
                . over (teamData . firstTeam)
                . set forbiddanceMessage
                . Just
                $ pinId
    case secondForbidPin of
        Just pin -> do
            void . updateForbidPin general pin $ secondWordList
        Nothing -> do
            pinId <- createForbidPin general secondWordList
            modifyIORef ctxRef
                . over (teamData . firstTeam)
                . set forbiddanceMessage
                . Just
                $ pinId
  where
    updateForbidPin general pin wordList = do
        restCall' $ EditMessage (general, pin) (warningText wordList) Nothing
    createForbidPin general wordList = do
        pinMsg <- restCall' . CreateMessage general $ warningText wordList
        restCall' $ AddPinnedMessage (general, messageId pinMsg)
        return . messageId $ pinMsg
    warningText bannedWords =
        voiceFilter
                "The following words and terms are hereby illegal, forbidden, banned and struck from all records, forever: "
            <> T.intercalate ", " bannedWords

stopDict :: IORef Context -> DH ()
stopDict ctxRef = do
    sendMessageToGeneral "I'm so tired..."
    ctx <- readIORef ctxRef
    writeFileLBS "data.json" . encode $ ctx
    stopDiscord


startHandler :: IORef Context -> DH ()
startHandler ctxRef = do
    sendMessageToGeneral "hello, world!"
    void . forkIO $ unbanUsersFromGeneral
    void . forkIO $ performRandomEvents
    void . forkIO $ updateTeamRoles ctxRef
    void . forkIO $ updateForbiddenWords ctxRef
  where
    unbanUsersFromGeneral = do
        general <- getGeneralChannel
        getMembers >>= mapM_
            (\m -> do
                setUserPermsInChannel True
                                      (channelId general)
                                      (userId . memberUser $ m)
                                      0x800
            )

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
