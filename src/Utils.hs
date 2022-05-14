-- | Miscellaneous utilities.
{-# OPTIONS_GHC -Wno-orphans #-}

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
  , randomImage
  , secondsDelay
  ) where

import           Prelude                        ( (!!) )
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
import qualified Data.Text                     as T
import           Network.Wreq                   ( get
                                                , responseBody
                                                )
import           System.Random
import           System.Random.Shuffle          ( shuffle' )
import           Utils.DictM
import Control.Concurrent (threadDelay)

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
shuffle gen xs | null xs   = []
               | otherwise = shuffle' xs (length xs) gen

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
    (filter (`elem` (['a' .. 'z'] :: String)) . toString $ txt)
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
  in  r `shiftL` 16 + g `shiftL` 8 + b `shiftL` 0

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

randomImage :: DictM ByteString
randomImage =
  toStrict . view responseBody <$> liftIO (get "https://r.sine.com/index")

secondsDelay :: (MonadIO m) => Int -> m ()
secondsDelay = (*1000000) >>> threadDelay >>> liftIO