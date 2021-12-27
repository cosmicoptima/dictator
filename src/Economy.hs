{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Economy
    ( mkNewTrinket
    , getNewTrinket
    ) where

import           Relude                  hiding ( First
                                                , get
                                                )

import           Datatypes
import           DiscordUtils
import           GenText                        ( getJ1
                                                , makePrompt
                                                )

import qualified Database.Redis                as DB
import           Text.Parsec

-- | not only retrieves a new trinket, but adds it to the database
mkNewTrinket :: DB.Connection -> Rarity -> DH (TrinketID, TrinketData)
mkNewTrinket conn rarity = do
    tId     <- nextTrinketId conn
    trinket <- getNewTrinket conn rarity
    liftIO $ setTrinket conn tId trinket
    return (tId, trinket)

getNewTrinket :: DB.Connection -> Rarity -> DH TrinketData
getNewTrinket conn rarity = do
    let rare = rarity == Rare
    res <- getJ1 16 (prompt rare)
    case listToMaybe . rights . fmap parseTrinketName . lines $ res of
        Just name -> return $ TrinketData name rarity
        Nothing   -> getNewTrinket conn rarity

  where
    commonTrinkets =
        [ "3.67oz of rust."
        , "a small bird."
        , "a new mobile phone."
        , "a ball of purple yawn."
        , "two message board roles."
        , "silly little thing."
        , "an oily tin can."
        ]
    rareTrinkets =
        [ "a ball of pure malignant evil."
        , "the awfulness of your post."
        , "nothing."
        -- , "You heard it here first."
        -- , "You have nothing interesting to say."
        , "a gateway into another world."
        , "a bag of dicks."
        , "the ability to control time."
        , "a machete."
        , "an empty warehouse."
        , "a free pass to ban one member."
        ]

    promptTrinkets rare =
        makePrompt $ if rare then rareTrinkets else commonTrinkets
    prompt rare =
        "There exists a dictator of an online chatroom who is eccentric but evil. He often gives out items. Here are some examples of "
            <> itemDesc
            <> " items.\n"
            <> promptTrinkets rare
        where itemDesc = if rare then "rare" else "common"

parseTrinketName :: Text -> Either ParseError Text
parseTrinketName = parse
    (fmap fromString $ string "- " *> manyTill anyChar (string ".") <* eof)
    ""

nextTrinketId :: DB.Connection -> DH Int
nextTrinketId conn = liftIO $ go 1  where
    go n = do
        trinket <- getTrinket conn n
        case trinket of
            Just _  -> go (n + 1)
            Nothing -> return n
