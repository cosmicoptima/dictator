{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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
import           Data.Maybe
import           Data.Random.Normal
import           Data.Scientific                ( Scientific
                                                , fromFloatDigits
                                                )
import qualified Data.Text                     as T
import           GenText
import           Items
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
import           Utils

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

getRoleById :: RoleId -> DH (Maybe Role)
getRoleById rId = do
    roles <- restCall' $ GetGuildRoles pnppcId
    return . find ((== rId) . roleId) $ roles

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

firstTeamId :: RoleId
firstTeamId = 921236611814014977

secondTeamId :: RoleId
secondTeamId = 921236614993297442

firstTeamRole :: DH Role
firstTeamRole =
    getRoleById firstTeamId
        >>= maybe (debugDie "first team role doesn't exist") return

secondTeamRole :: DH Role
secondTeamRole =
    getRoleById secondTeamId
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
                (\_ x -> x) -- Use old value if it exists
                (userId . memberUser $ m) -- For each member
                def -- Otherwise insert the default
            )
        modifyIORef ctxRef . over userData . Map.map $ func


-- | Handle a message assuming it's a command. If it isn't, fire off the handler for regular messages.
handleCommand :: IORef Context -> Message -> DH ()
handleCommand ctxRef m = do
    if not . userIsBot . messageAuthor $ m
        then case words content of
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
                (rngGPT, rngBool) <- newStdGen <&> split

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
                        sendMessage channel $ case words output of
                            l : _ -> l
                            []    -> "idk"

            ["gm"] -> unless (userIsBot . messageAuthor $ m) $ do
                rng <- newStdGen
                sendMessage channel
                    $ randomChoice ("fuck off" : replicate 4 "gm") rng

            ["gn"] -> unless (userIsBot . messageAuthor $ m) $ do
                rng <- newStdGen
                sendMessage channel $ randomChoice
                    ("i plan to kill you in your sleep" : replicate 7 "gn")
                    rng

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

            ("ponder" : life) -> do
                pontificateOn (messageChannel m) . T.unwords $ life

            -- ("gpttest" : p) -> do
            --     output <- (getGPT . unwords) p
            --     sendMessage channel output

            ["update", "the", "teams" ] -> updateTeamRoles ctxRef

            ["show"  , "the", "points"] -> do
                ctx         <- readIORef ctxRef
                firstTName  <- firstTeamRole <&> roleName
                secondTName <- secondTeamRole <&> roleName
                let firstPoints =
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

            -- ["get", "rid", "of", "those", "damn", "roles"] -> do
            --     getMembers >>= mapConcurrently_
            --         (\m' -> mapConcurrently_
            --             (restCall . RemoveGuildMemberRole
            --                 pnppcId
            --                 (userId . memberUser $ m')
            --             )
            --             (memberRoles m')
            --         )

            -- ["even", "the", "points"] -> do
            --     modifyIORef ctxRef . over teamPoints . over firstPoints . const $ 13
            --     modifyIORef ctxRef . over teamPoints . over secondPoints . const $ 13

            ["time", "for", "bed!"] -> do
                stopDict ctxRef

            -- ("offer" : deal) -> case p . unwords $ deal of
            --     Left _ ->
            --         sendMessage channel "what the fuck are you talking about?"
            --     Right (offers, demands) ->
            --         sendMessage channel
            --             $  pprint offers
            --             <> " demanding "
            --             <> pprint demands

            _ -> handleMessage ctxRef m
        else pure ()
  where
    content = T.toLower . messageText $ m
    channel = messageChannel m
    author  = messageAuthor m

-- | Handle a message assuming that it isn't a command.
handleMessage :: IORef Context -> Message -> DH ()
handleMessage ctxRef m = do
    when (T.isInfixOf "owned" content) $ do
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

        when (odds 0.04 . mkStdGen . fromIntegral . messageId $ m) $ do
            pontificateOn channel . messageText $ m

        ctx <- readIORef ctxRef
        let culpritTeam = getTeam author ctx
        when (messageForbiddenIn ctx content culpritTeam) $ do
            timeoutUser author
            updateForbiddenWords ctxRef
            awardTeamMembersCredit ctxRef (otherTeam . getTeam author $ ctx) 10
  where
    content = T.toLower . messageText $ m
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
        ctx         <- readIORef ctxRef
        firstTName  <- firstTeamRole <&> roleName
        secondTName <- secondTeamRole <&> roleName
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
    , randomEvent :: IORef Context -> DH ()
    }

data ScheduledEvent = ScheduledEvent
    { absDelay       :: Double
    , scheduledEvent :: IORef Context -> DH ()
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
    , RandomEvent
        { avgDelay    = minutes 90
        , randomEvent = const $ do
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

scheduledEvents :: [ScheduledEvent]
scheduledEvents =
    [ ScheduledEvent { absDelay       = hours 2
                     , scheduledEvent = updateForbiddenWords
                     }
    ]

performRandomEvents :: IORef Context -> DH ()
performRandomEvents ctx = do
    threadDelay 100000
    void . forkIO $ mapConcurrently_ maybePerformRandomEvent randomEvents
    performRandomEvents ctx

  where
    maybePerformRandomEvent (RandomEvent rngDelay event) = do
        rng <- newStdGen
        when (odds (0.1 / rngDelay) rng) $ event ctx

startScheduledEvents :: IORef Context -> DH ()
startScheduledEvents ctxRef = do
    mapConcurrently_ scheduledEventLoop scheduledEvents
  where
    scheduledEventLoop sched@(ScheduledEvent delay event) = do
        -- Sleep for the required amount of time, noting that this is in nanoseconds.
        threadDelay . secsToUs $ delay
        event ctxRef
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

    -- case ctx ^. (teamData . firstTeam . teamName) of
    --     Nothing -> do
    --         void . restCall' $ CreateGuildRole
    --             pnppcId
    --             (teamRoleOpts firstTeamName $ convertColor blueColor)

    createOrModifyGuildRoleById firstTeamId
        $ teamRoleOpts firstTeamName
        $ convertColor blueColor

    createOrModifyGuildRoleById secondTeamId
        $ teamRoleOpts secondTeamName
        $ convertColor redColor

    -- case ctx ^. (teamData . secondTeam . teamName) of
    --     Nothing -> do
    --         void . restCall' $ CreateGuildRole
    --             pnppcId
    --             (teamRoleOpts secondTeamName $ convertColor redColor)
    --     Just s ->
    --         do



    createOrModifyGuildRole "leader" $ teamRoleOpts "leader" $ convertColor
        dictColor
    getRoleNamed "leader" >>= \case
        Just r  -> restCall' . AddGuildMemberRole pnppcId dictId $ roleId r
        Nothing -> return ()

    allMembers <- getMembers

    -- First insert any users who don't exist
    forM_
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
    ctx <- readIORef ctxRef
    forM_
        allMembers
        (\m ->
            let memberId = userId . memberUser $ m
            in
                unless (memberId == dictId) $ do
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
        )

    -- Then we update them on discord
    ctx2       <- readIORef ctxRef
    firstRole  <- firstTeamRole
    secondRole <- secondTeamRole
    forM_
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
        . over secondTeam
        . set forbiddenWords
        $ secondWordList

    general <- getGeneralChannel <&> channelId
    createOrUpdatePin general First
    createOrUpdatePin general Second
    return ()

  where
    createOrUpdatePin _       Neutral = return ()
    createOrUpdatePin channel team    = do
        ctx <- readIORef ctxRef
        let forbidPin = view (teamData . setter team . forbiddanceMessage) ctx
            wordList  = view (teamData . setter team . forbiddenWords) ctx
        pinId <- case forbidPin of
            Just pin -> return pin
            Nothing  -> do
                pinId <- restCall' (CreateMessage channel "aa") <&> messageId
                modifyIORef ctxRef
                    . over (teamData . setter team)
                    . set forbiddanceMessage
                    $ Just pinId
                restCall' $ AddPinnedMessage (channel, pinId)
                return pinId

        embed <- warningEmbed wordList team
        void . restCall' $ EditMessage (channel, pinId)
                                       (warning team)
                                       (Just embed)

    setter Neutral = error "This should never happen"
    setter First   = firstTeam
    setter Second  = secondTeam

    warning Neutral = error "This can't ever happen"
    warning First =
        voiceFilter
            "The following words and terms are hereby illegal, forbidden, banned and struck from all records, forever: "
    warning Second =
        voiceFilter
            "I declare that the following so-called words do not exist, have never existed, and will continue to not exist: "

    warningEmbed _        Neutral = error "You know the drill"
    warningEmbed wordList team    = do
        role <- if team == First then firstTeamRole else secondTeamRole
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

stopDict :: IORef Context -> DH ()
stopDict ctxRef = do
    sendMessageToGeneral "I'm so tired..."
    ctx <- readIORef ctxRef
    writeFileLBS "data.json" . encode $ ctx
    stopDiscord


startHandler :: IORef Context -> DH ()
startHandler ctxRef = do
    sendMessageToGeneral "rise and shine!"
    void . forkIO $ unbanUsersFromGeneral
    void . forkIO $ performRandomEvents ctxRef
    void . forkIO $ startScheduledEvents ctxRef
    void . forkIO $ updateTeamRoles ctxRef
    void . forkIO $ do
        -- Wait for 5 seconds to avoid a race condition-ish thing
        threadDelay 5000000
        updateForbiddenWords ctxRef
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
