{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Economy where

import qualified Prelude                        ( show )
import           Relude                  hiding ( First
                                                , get
                                                )

import           Data
import           DiscordUtils
import           GenText                        ( getJ1 )

import qualified Database.Redis                as DB
import           System.Random
import           Text.Parsec


data Trinket = Trinket
    { trinketId   :: Int
    , trinketName :: Text
    , trinketRare :: Bool
    }
instance Show Trinket where
    show t =
        toString
            $  trinketName t
            <> " (#"
            <> show (trinketId t)
            <> ") ("
            <> (if trinketRare t then "RARE" else "COMMON")
            <> ")"


parRandomTrinket :: Int -> Bool -> Text -> Either ParseError Trinket
parRandomTrinket id_ rare = parse
    (  fmap ((\s -> Trinket id_ s rare) . fromString)
    $  string "Item: "
    *> manyTill anyChar (string ".")
    <* eof
    )
    ""

getRandomTrinket :: DB.Connection -> DH Trinket
getRandomTrinket conn = do
    id_  <- getNextTrinketId conn
    rare <- randomIO <&> (< (0.3 :: Double))
    res  <- getJ1 16 (prompt rare)
    maybe
        (getRandomTrinket conn)
        return
        (listToMaybe . rights . fmap (parRandomTrinket id_ rare) . lines $ res)

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

    promptTrinkets rare = unlines . fmap ("Item: " <>) $ if rare
        then rareTrinkets
        else commonTrinkets
    prompt rare =
        "There exists a dictator of an online chatroom who is eccentric but evil. He often gives out items. Here are some examples of "
            <> itemDesc
            <> " items.\n"
            <> promptTrinkets rare
            <> "\nItem:"
        where itemDesc = if rare then "rare" else "common"

getNextTrinketId :: DB.Connection -> DH Int
getNextTrinketId conn = go 1  where
    go n = do
        exists <- runRedis' conn $ DB.exists ("trinket:" <> show n <> ":name")
        if exists then go (n + 1) else return n
