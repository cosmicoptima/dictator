{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Game.Roles where

import           Relude                  hiding ( First
                                                , get
                                                )

import           Utils.DictM
import           Utils.Discord

import           Discord.Types

import           Control.Lens


import           Control.Monad                  ( liftM2 )
import           Control.Monad.Random           ( newStdGen )
import           Data.Aeson                     ( Value
                                                , decode
                                                )
import           Data.Aeson.Lens         hiding ( members )
import           Data.Default                   ( Default(def) )
import qualified Data.MultiSet                 as MS
import qualified Data.Set                      as Set
import           Data.String.Interpolate        ( i )
import           Discord.Internal.Rest.Guild    ( GuildRequest(..)
                                                , ModifyGuildRoleOpts(..)
                                                )
import           Game.Data
import           Game.Effects
import           Game.Items
import           Network.Wreq
import           Numeric.Lens
import           Text.Printf                    ( printf )
import           Utils                          ( randomChoice )

minRoles :: Int
minRoles = 4

avgRoles :: Int
avgRoles = 8

maxRoles :: Int
maxRoles = 12


randomNamedColour :: DictM (Text, ColorInteger)
randomNamedColour = do
    -- Retire the api key if we couldn't get a good result from it.
    -- We have to override wreq to not throw exceptions, then match on the response.
    res <- liftIO $ get "https://colornames.org/random/json/"
    let parsed :: Maybe Value = decode (res ^. responseBody)
        name = parsed ^? _Just . key "name" . _String
        col = parsed ^? _Just . key "hexCode" . _String . to toString . hex

    fromJustOr (Fuckup $ res ^. responseBody . to show) $ do
        name' <- name
        col'  <- col
        return (name', col')

randomColorRole :: DictM RoleId
randomColorRole = liftM2 randomChoice
                         (view (globalRoles . to Set.elems) <$> getGlobal)
                         newStdGen

randomColoredRole :: DictM Role
randomColoredRole = randomColorRole >>= getRoleByID >>= fromJustOr
    (Fuckup "Role had non existent ID!")

createRandomRole :: DictM ()
createRandomRole = do
    (rName, rCol) <- randomNamedColour
    role          <- restCall' $ CreateGuildRole pnppcId $ ModifyGuildRoleOpts
        { modifyGuildRoleOptsName            = Just
            [i|0x#{(printf "%06x" rCol) :: String} "#{rName}"|]
        , modifyGuildRoleOptsPermissions     = Nothing
        , modifyGuildRoleOptsColor           = Just rCol
        , modifyGuildRoleOptsSeparateSidebar = Just False
        , modifyGuildRoleOptsMentionable     = Just False
        }
    modifyGlobal_ $ over globalRoles (Set.insert $ roleId role)

removeRole :: RoleId -> DictM ()
removeRole role = do
    restCall'_ $ DeleteGuildRole pnppcId role
    modifyGlobal_ $ over globalRoles (Set.delete role)
    -- We have to remove the role from people's inventory, or at least try to.
    getRoleByID role >>= \case
        Just roleData -> do
            members <- getMembers
            forM_ members $ \mem -> do
                let user = userId . memberUser $ mem
                    col  = roleColor roleData
                modifyUser_ user
                    $ over (userItems . itemRoles) (MS.deleteAll col)
        Nothing -> return ()

fixRoles :: DictM ()
fixRoles = do
    roles <- view globalRoles <$> getGlobal

    when (Set.size roles < minRoles) $ do
        let toMake = minRoles - Set.size roles
        replicateM_ toMake createRandomRole

    when (Set.size roles > maxRoles) $ do
        let toDel = Set.size roles - maxRoles
        replicateM_ toDel $ randomColorRole >>= removeRole


lookupRole :: ColorInteger -> DictM (Maybe Role)
lookupRole col =
    find ((== col) . roleColor) <$> restCall' (GetGuildRoles pnppcId)

updateUserRoles :: UserId -> DictM ()
updateUserRoles user = when (user /= dictId) $ do
    ownedRoles <- view (userItems . itemRoles) <$> getUser user
    -- First, try to remove all the roles they have, but shouldn't.
    shownRoles <- memberRoles <$> userToMemberOr Complaint user
    forM_ shownRoles $ \shown -> do
        color <- maybe def roleColor <$> getRoleByID shown
        -- Filter out the server booster color, as well as colorless ones.
        let builtinColor = color == 0xf47fff || color == 0x000000
        when (not builtinColor && color `MS.notMember` ownedRoles)
             (restCall'_ $ RemoveGuildMemberRole pnppcId user shown)

    -- Then, add all the roles they have; we don't need to check if they don't have them.
    forM_ (MS.elems ownedRoles) $ lookupRole >=> \case
        Just roleData -> do
            restCall'_ $ AddGuildMemberRole pnppcId user (roleId roleData)
        Nothing -> return ()



-- This code can stay because it was quite nice, but is sadly useless now.
-- lookupRole :: Text -> DictM (Maybe Role)
-- lookupRole name = do
--     validRoles <- view globalRoles <$> getGlobal
--     roles      <- restCall' $ GetGuildRoles pnppcId
--     let matches = filter
--             (liftA2 (&&)
--                     ((`Set.member` validRoles) . roleId)
--                     ((name `T.isPrefixOf`) . roleName)
--             )
--             roles
--     return $ case matches of
--         [role] -> Just role
--         _      -> Nothing
