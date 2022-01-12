{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Utils where

import           Relude

import           Game.Data
import           Utils.DictM

import           Control.Lens
import           Data.Char                      ( isPunctuation )
import qualified Data.MultiSet                 as MS
import qualified Data.Text                     as T
import qualified Database.Redis                as DB
import           Discord.Internal.Types.Prelude
import           Discord.Requests
import           Game.Items                     ( TrinketID )
import           Points                         ( updateUserNickname )
import           Text.Parsec
import           Utils.Discord

-- | Rename a user, giving them the pieces of their old name.
renameUser :: DB.Connection -> UserId -> Text -> DictM ()
renameUser conn userID newName = do
    member <- restCall' $ GetGuildMember pnppcId userID
    void . modifyUser conn userID $ \m ->
        let oldName = m ^. userName . to unUsername
        in  m
                &  userName
                .~ Username newName
                &  userWords
                %~ (MS.union . namePieces $ oldName)
    updateUserNickname conn member

namePieces :: Text -> MS.MultiSet Text
namePieces =
    MS.fromList . T.words . T.toLower . T.filter (not . isPunctuation)

parseTrinketName :: Text -> Either ParseError Text
parseTrinketName =
    parse (fmap fromString $ string "- " *> manyTill anyChar (string ".")) ""

lookupTrinketName
    :: DB.Connection -> Text -> DictM (Maybe (TrinketID, TrinketData))
lookupTrinketName conn name =
    getallTrinket conn <&> find ((== name) . view trinketName . snd)


commonTrinketExamples :: [Text]
commonTrinketExamples =
    [ "3.67oz of rust"
    , "a small bird"
    , "a new mobile phone"
    , "a jar of jam"
    , "three messages"
    , "a ball of purple yawn"
    , "silly little thing"
    , "a lump of lead"
    , "an oily tin can"
    ]

uncommonTrinketExamples :: [Text]
uncommonTrinketExamples =
    [ "a ball of pure malignant evil"
    , "the awfulness of your post"
    , "three message board roles"
    , "nothing"
    , "a glue covered, soaking wet pillow"
    , "a gateway into another world"
    , "a bloody machete"
    , "two smelly socks"
    , "an empty warehouse"
    ]

rareTrinketExamples :: [Text]
rareTrinketExamples =
    [ "a free pass to ban one member"
    , "something really good or at least above average"
    , "the ability to control time"
    , "whatever you want, babe"
    , "the beauty that the world is full to the brim with"
    , "the scummiest and rarest"
    , "at least five other trinkets"
    , "temporary immortality"
    ]

legendaryTrinketExamples :: [Text]
legendaryTrinketExamples =
    [ "a free pass to ban one member"
    , "a bag of dicks"
    , "rough homosexual intercourse"
    , "the entirety of postrat Twitter"
    , "the holy excrement of God Himself"
    , "ownership of the entire forum"
    , "anti-semitism, racism, just general bigotry"
    , "every trinket that exists or will exist"
    , "a hugely oversized penis"
    , "sword of the shitposter (special item)"
    , "control of the official ideology of the message board"
    ]
