{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Events where

import           Relude                  hiding ( First )

-- local modules
----------------
import           Datatypes
import           DiscordUtils
import           GenText
import           Utils

-- discord
----------
import           Discord
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

-- all else
-----------
import           Control.Lens
import           Control.Monad                  ( liftM2 )
import           Control.Monad.Random           ( evalRandIO )
import qualified Data.Text                     as T
import qualified Database.Redis                as DB
import           System.Random
import           UnliftIO.Async


-- teams (TODO move some of this, probably)
--------

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


-- GPT
------

randomAdjective :: DH Text
randomAdjective = liftIO $ liftM2 randomChoice getAdjList getStdGen

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


-- other
--------

stopDict :: DB.Connection -> DH ()
stopDict conn = do
    sendMessageToGeneral "I'm so tired..."
    liftIO $ DB.disconnect conn
    stopDiscord
