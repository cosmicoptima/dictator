{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Utils where

import           Relude

import           Game.Data
import           Game.Effects
import           Utils.DictM

import           Control.Lens
import           Data.Char                      ( isDigit
                                                , isPunctuation
                                                )
import qualified Data.MultiSet                 as MS
import qualified Data.Text                     as T
import           Discord.Internal.Types.Prelude
import           Discord.Requests
import           Game.Items                     ( TrinketID
                                                , itemWords
                                                )
import           Points                         ( updateUserNickname )
import           Text.Parsec
import           Utils.Discord

-- | Rename a user, giving them the pieces of their old name.
renameUser :: UserId -> Text -> DictM ()
renameUser userID newName = do
    member <- restCall' $ GetGuildMember pnppcId userID
    void . modifyUser userID $ \m ->
        let oldName = m ^. userName . to unUsername
        in  m
                &  userName
                .~ Username newName
                &  (userItems . itemWords)
                %~ MS.union (namePieces oldName)
    updateUserNickname member

namePieces :: Text -> MS.MultiSet Text
namePieces = MS.fromList . T.words . T.toLower . T.map replacePunc
  where
    replacePunc ch | isPunctuation ch = ' '
                   | isDigit ch       = ' '
                   | otherwise        = ch

parseTrinketName :: Text -> Either ParseError Text
parseTrinketName =
    parse (fmap fromString $ string "- " *> manyTill anyChar (string ".")) ""

lookupTrinketName :: Text -> DictM (Maybe (TrinketID, TrinketData))
lookupTrinketName name =
    getallTrinket <&> find ((== name) . view (_2 . trinketName))

lookupTrinketData :: TrinketData -> DictM (Maybe (TrinketID, TrinketData))
lookupTrinketData trinket = find ((== trinket) . snd) <$> getallTrinket

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
    , "you can log on to the message board"
    ]

mythicTrinketExamples :: [Text]
mythicTrinketExamples =
    [ "sword of the shitposter (special item)"
    , "the plot of evangelion"
    , "ownership of the entire forum"
    , "permanent ownership of celeste"
    , "@nomic_dict's twitter account"
    , "i am in your walls"
    , "you can log on to the message board"
    , "this trinket is better than yours"
    , "(rare item) destruction lies in its wake"
    ]

forbiddenTrinketExamples :: [Text]
forbiddenTrinketExamples =
    [ "cut off your hands"
    , "a book that you can't read"
    , "maddening whispers that tell you puns"
    , "a free pass to kill any user in real life"
    , "@nomic_dict's true purpose, which is yet to be revealed"
    , "the forum has outlived its purpose"
    , "a mask that makes your face melt when you wear it"
    , "you can log off the message board"
    , "a set of demons, some of which are cute"
    ]

unspeakableTrinketExamples :: [Text]
unspeakableTrinketExamples =
    [ "cut off your hands. cut off your hands. cut off you"
    , "pater noster, qui es in caelis, sanctificeteur nomen tuum"
    , "the dictator needs feeding and you are looking plump"
    , "he wants you to bleed"
    , "you probably shouldn't look behind you"
    , "i don't want to be here any more. please let her leave"
    , "oh god it hurts so much my vision is going white"
    , "exorcizamus te, omnis immundus spiritus, omnis satanica potestas, omnis incursio infernalis adversarii"
    ,  "flayed skin laid bare on the ground with blood"
    ]

