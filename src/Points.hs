{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Points where

import           Relude                  hiding ( First
                                                , get
                                                )

import           Game.Data
import           Utils.DictM
import           Utils.Discord

import           Discord.Requests
import           Discord.Types

import           Control.Lens
import qualified Data.Text                     as T
import qualified Database.Redis                as DB

setNickname :: UserId -> Text -> DictM ()
setNickname user nick =
    restCall' $ ModifyGuildMember pnppcId user $ ModifyGuildMemberOpts
        (Just nick)
        Nothing
        Nothing
        Nothing
        Nothing

updateUserNickname :: DB.Connection -> GuildMember -> DictM ()
updateUserNickname conn member = do
    let user     = memberUser member
        fullName = fromMaybe (userName user) $ memberNick member
        name     = stripPoints fullName
    when (userId user `notElem` [dictId, 891038666703634432]) $ do
        points <- view userPoints <$> getUserOr Fuckup conn (userId user)
        let suffix = " (" <> show points <> ")"
        -- Enforce discord's 32-character limit for usernames by truncating.
        setNickname (userId user) $ T.take (32 - T.length suffix) name <> suffix
  where
    stripPoints = T.dropWhileEnd (`elem` pointChars)
    pointChars  = '(' : ')' : ' ' : ['0' .. '9']
