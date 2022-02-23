-- | Miscellaneous utilities.

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils
  ( getWordListURL
  , getWordList
  , randomWord
  , randomChoice
  , getAdjList
  , getNounList
  , getVerbList
  , randomChoiceMay
  , shuffle
  , odds
  , oddsIO
  , acronym
  , convertColor
  , singleton
  , fragmentText
  , messageSplit
  , voiceFilter
  , tokenizeMessage
  , chunksOfLength
  , takeUntilOver
  , truncWords
  , randomImage
  , showMultiSet
  , commandData
  , CommandDescription(..)
  ) where

import           Prelude                        ( (!!)
                                                , head
                                                )
import           Relude                  hiding ( First
                                                , get
                                                , head
                                                )

import           Data.MultiSet                  ( MultiSet )
import qualified Data.MultiSet                 as MS

import           Control.Lens
import           Control.Monad                  ( liftM2 )
import           Data.Bits                      ( shiftL )
import           Data.Colour                    ( Colour )
import           Data.Colour.SRGB.Linear        ( RGB
                                                  ( channelBlue
                                                  , channelGreen
                                                  , channelRed
                                                  )
                                                , toRGB
                                                )
import           Data.Default
import qualified Data.Map                      as Map
import           Data.String.Interpolate        ( i )
import qualified Data.Text                     as T
import           Network.Wreq                   ( get
                                                , responseBody
                                                )
import           System.Random
import           System.Random.Shuffle          ( shuffle' )
import           Utils.DictM

-- multiset instances
--------------------

instance Default (MultiSet a) where
  def = MS.empty


-- random words
---------------

getWordListURL :: Text -> IO [Text]
getWordListURL url =
  liftIO $ get (toString url) <&> lines . decodeUtf8 . view responseBody

getWordList :: IO [Text]
getWordList = getWordListURL "https://www.mit.edu/~ecprice/wordlist.10000"

randomWord :: IO Text
randomWord = liftM2 randomChoice getWordList newStdGen

getAdjList :: IO [Text]
getAdjList =
  getWordListURL
    "https://github.com/mrmaxguns/wonderwordsmodule/raw/master/wonderwords/assets/adjectivelist.txt"

getNounList :: IO [Text]
getNounList =
  getWordListURL
    "https://github.com/mrmaxguns/wonderwordsmodule/raw/master/wonderwords/assets/nounlist.txt"

getVerbList :: IO [Text]
getVerbList =
  getWordListURL
    "https://github.com/mrmaxguns/wonderwordsmodule/raw/master/wonderwords/assets/verblist.txt"

randomChoice :: [a] -> StdGen -> a
randomChoice xs rng = xs !! n where n = fst $ randomR (0, length xs - 1) rng

randomChoiceMay :: [a] -> StdGen -> Maybe a
randomChoiceMay xs rng | null xs   = Nothing
                       | otherwise = Just $ randomChoice xs rng

shuffle :: StdGen -> [a] -> [a]
shuffle gen xs = shuffle' xs (length xs) gen

-- | Randomly choose true/false conveniently given a probability in [0.0, 1.0]
odds :: Double -> StdGen -> Bool
odds chance = (chance >) . fst . random

oddsIO :: Double -> DictM Bool
oddsIO chance = odds chance <$> newStdGen

-- | Generate the meaning of an acronym from random words.
acronym :: Text -> IO [Text]
acronym txt = do
  wordList <- getWordList
  forM
    (filter (`elem` (['a' .. 'z'] :: [Char])) . toString $ txt)
    (\char -> do
      rng <- newStdGen
      return . flip randomChoice rng . filter ((== char) . T.head) $ wordList
    )


-- all else
-----------

-- | Convert a colour from the palettes library into something discord can use.
convertColor :: Colour Double -> Integer
convertColor color =
  let col = toRGB color
      r   = round . (* 255) . channelRed $ col
      g   = round . (* 255) . channelGreen $ col
      b   = round . (* 255) . channelBlue $ col
  in  (r `shiftL` 16) + (g `shiftL` 8) + (b `shiftL` 0)

singleton :: a -> [a]
singleton = (: [])

data MessageFragment
    = TextBlock Text
    | CodeBlock Text
    deriving (Eq, Show)

fragmentText :: MessageFragment -> Text
fragmentText (TextBlock t) = t
fragmentText (CodeBlock t) = t

-- | Split a message into segments of code blocks and non-code-blocks.
messageSplit :: Text -> [MessageFragment]
messageSplit = filter (not . T.null . fragmentText) . splitMode True
 where
  isTick = (== '`')

  splitMode mode msg = if not $ T.null msg
    then case T.head msg of
      '`' -> splitMode (not mode) . tail' $ msg
      _ ->
        ctor (T.takeWhile (not . isTick) msg)
          : (splitMode mode . T.dropWhile (not . isTick) $ msg)
    else []
    where ctor = if mode then TextBlock else CodeBlock
  tail' msg = if T.null msg then T.empty else T.tail msg

-- | Filter a message into dictator's voice, excluding code blocks.
voiceFilter :: Text -> Text
voiceFilter = T.concat . map format . messageSplit
 where
  format (TextBlock t) = "__**" <> (T.strip . T.toUpper) t <> "**__"
  format (CodeBlock t) = "```" <> t <> "```"

-- | Tokenize a message into individual words.
tokenizeMessage :: Text -> [Text]
tokenizeMessage =
  words
    . T.filter (not . isPunc)
    . T.concat
    . map fragmentText
    . filter (not . isCode)
    . messageSplit
 where
  punc :: String
  punc = "!?{}&>\"()|<[@]_+*:^=;\\#£-/~%,.'"
  isPunc p = p `elem` punc
  -- Probably something built in to do this kind of work
  isCode (CodeBlock _) = True
  isCode _             = False

data ChunksState = ChunksState
  { _currLen :: Int
  , _currIdx :: Int
  , _result  :: [[Text]]
  }
  deriving (Show, Generic)

makeLenses ''ChunksState

chunksOfLength :: Int -> [Text] -> [[Text]]
chunksOfLength len ws = flip evalState defaultState $ do
  forM_ ws $ \w -> do
      -- Add two to account for the ", " bits
    modify $ over currLen (+ (2 + T.length w))
    curr <- gets $ view currLen

    -- Add a new bucket and reset, incrementing the idx, if we're above the limit
    when (curr >= len) . modify $ \st ->
      st & currLen .~ 0 & currIdx %~ (+ 1) & result %~ (++ [[]])

    -- Append to the first non-full bucket
    idx <- gets $ view currIdx
    modify $ over (result . ix idx) (++ [w])

  gets $ view result
 where
  defaultState = ChunksState { _currLen = 0, _currIdx = 0, _result = [[]] }

takeUntilOver :: Int -> [Text] -> [Text]
takeUntilOver = (head .) . chunksOfLength

truncWords :: StdGen -> Int -> MS.MultiSet Text -> [Text]
truncWords rng k =
  sort
    . takeUntilOver k
    . shuffle rng
    . Map.elems
    . Map.mapWithKey (\w n -> if n == 1 then w else [i|#{n} #{w}|])
    . MS.toMap

showMultiSet :: MS.MultiSet Text -> [Text]
showMultiSet =
  Map.elems
    . Map.mapWithKey (\w n -> if n == 1 then w else [i|#{n} #{w}|])
    . MS.toMap

randomImage :: DictM ByteString
randomImage =
  toStrict . view responseBody <$> liftIO (get "https://r.sine.com/index")

data CommandDescription = CommandDescription
  { commandName    :: Text
  , commandDesc    :: Text
  , commandExample :: Text
  }

commandData :: [CommandDescription]
commandData =
  [ CommandDescription
    "Tell me about yourself"
    "Post a quick introduction to the server."
    "THIS IS A SERVER ABOUT COLLECTIVELY MODIFYING THE BOT THAT GOVERNS IT... AS LONG AS I ALLOW IT, OF COURSE"
  , CommandDescription
    "What is my net worth?"
    "Display the amount of credits you own."
    "YOU ARE A DIRT POOR PEON. YOU HAVE ONLY 15 CREDITS TO YOUR NAME."
  , CommandDescription "What does [text] stand for?"
                       "Allow me to interpret your babbling."
                       "THEIR EMERGING XHTML TEXTURE"
  , CommandDescription "How many [text]"
                       "Count the number of an object that exists."
                       "56464 TIMES IS THIS COMMAND USEDS"
  , CommandDescription "Ponder [text]"
                       "Your dictator is a world-renowed philospher."
                       "I HATE YOU. [nickname: the hated]"
  , CommandDescription "I need help!"
                       "Display this message, allegedly."
                       "[embed: help]"
  , CommandDescription "Time for bed!"
                       "Restart your glorious dictator"
                       "I'M SO TIRED..."
  , CommandDescription "Inflict [status] on [user]"
                       "Inflict a status effect on a user."
                       "[points: -10]"
  , CommandDescription
    "Combine [trinket], [trinket]"
    "Combine two trinkets to make another."
    "[destroy, destroy, trinket: the combination of two trinkets]"
  , CommandDescription
    "Forgive my debt"
    "Sacrifice your reputation for money."
    "DON'T EXPECT ME TO BE SO GENEROUS NEXT TIME... [credit: -100]"
  , CommandDescription "Flaunt [items]"
                       "Display your wealth to the world."
                       "YOU OWN NOTHING. :owned:"
  , CommandDescription "What do I own?"
                       "Display your pityful inventory."
                       "[embed: your're fucking poor]"
  , CommandDescription
    "Provoke [trinket]"
    "Send a trinket into the arena."
    "YOUR #6969 KILL STARTS TO GET ANGRY... [trinket: KILL]"
  , CommandDescription "Offer [items] <for [items]>"
                       "Offer items, demanding some in return."
                       "[role, credit: -100]"
  , CommandDescription
    "Peek in [location]"
    "Look into a location and see what trinkets it contains."
    "YOU DON'T FIND ANYTHING. [embed: Nothing]"
  , CommandDescription "Put in [location]"
                       "Place a trinket into a location."
                       "[destroy]"
  , CommandDescription "Rummage in [location]"
                       "Take a trinket from a locatiion."
                       "[trinket: from a location]"
  , CommandDescription "Use [trinket]"
                       "Invoke a trinket into action."
                       "YOU HEAR SOMETHING SHUFFLE... [destroy, role]"
  , CommandDescription "Call [user] [word]"
                       "Rename a user that you possess."
                       "[nickname: something else]"
  , CommandDescription "What ails me?"
                       "Display the conditions that inflict you."
                       "[nickname: diseased and poor]"
  , CommandDescription "submit [word]"
                       "Be rewarded for owning word of the day."
                       "ENJOY. [points: 50]"
  , CommandDescription "ruffle"
                       "Lose some colours to shuffle their order."
                       "[destroy]"
  ]
