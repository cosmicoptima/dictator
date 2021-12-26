{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Economy where

import           Relude                  hiding ( First
                                                , get
                                                )

import           DiscordUtils
import           GenText                        ( getJ1 )
import           Prelude                        ( show )
import           System.Random
import           Text.Parsec


data Trinket = Trinket
    { trinketName :: Text
    , trinketRare :: Bool
    }
instance Show Trinket where
    show t =
        toString
            $  trinketName t
            <> " ("
            <> (if trinketRare t then "RARE" else "COMMON")
            <> ")"

parRandomTrinket :: Bool -> Text -> Either ParseError Trinket
parRandomTrinket rare = parse
    (  fmap (flip Trinket rare . fromString)
    $  string "Item: "
    *> manyTill anyChar (string ".")
    <* eof
    )
    ""

getRandomTrinket :: DH Trinket
getRandomTrinket = do
    rare <- randomIO <&> (< (0.3 :: Double))
    res  <- getJ1 16 (prompt rare)
    maybe
        getRandomTrinket
        return
        (listToMaybe . rights . fmap (parRandomTrinket rare) . lines $ res)

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
        , "a bag of dicks... enjoy!"
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
