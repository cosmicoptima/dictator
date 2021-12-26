{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Economy where

import qualified Data.List                     as T
import           DiscordUtils
import           GenText                        ( getJ1 )
import           Relude                  hiding ( First
                                                , get
                                                )
import           Text.Parsec

type Trinket = Text

parRandomTrinket :: Text -> Either ParseError Trinket
parRandomTrinket = parse
    (fmap fromString $ string "Item: " *> manyTill anyChar (string ".") <* eof)
    ""

getRandomTrinket :: DH Text
getRandomTrinket = do
    res <- getJ1 16 prompt
    maybe getRandomTrinket
          return
          (listToMaybe . rights . fmap parRandomTrinket . lines $ res)

  where
    trinkets =
        [ "A ball of pure malignant evil."
        , "The awfulness of your post."
        , "3.67oz of rust."
        , "Absolutely, utterly nothing."
        , "A real life bird."
        , "You heard it here first."
        , "A new mobile phone."
        , "A ball of purple yawn."
        , "Two message board roles."
        , "Silly little thing."
        , "An oily tin can."
        , "You have nothing interesting to say."
        ]
    promptTrinkets = unlines . fmap ("Item: " <>) $ trinkets
    prompt =
        "There exists a dictator of an online chatroom who is eccentric but evil. He often gives out items. Here are some examples.\n"
            <> promptTrinkets
            <> "\nItem:"
