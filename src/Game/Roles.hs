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

import           Commands
import           Utils.DictM
import           Utils.Discord

import           Discord.Types

import           Control.Lens


import           Data.Aeson                     ( Value
                                                , decode
                                                )
import           Data.Aeson.Lens
import           Data.Default
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           Discord.Internal.Rest.Guild    ( GuildRequest(..)
                                                , ModifyGuildRoleOpts(..)
                                                )
import           Game.Data
import           Network.Wreq
import           Numeric.Lens


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

createRandomRole :: DictM ()
createRandomRole = do
    (rName, rCol) <- randomNamedColour
    role          <- restCall' $ CreateGuildRole pnppcId $ ModifyGuildRoleOpts
        { modifyGuildRoleOptsName            = Just rName
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

lookupRole :: Text -> DictM (Maybe Role)
lookupRole name = do
    validRoles <- view globalRoles <$> getGlobal
    roles      <- restCall' $ GetGuildRoles pnppcId
    let matches = filter
            (liftA2 (&&)
                    ((`Set.member` validRoles) . roleId)
                    ((name `T.isPrefixOf`) . roleName)
            )
            roles
    return $ case matches of
        [role] -> Just role
        _      -> Nothing
