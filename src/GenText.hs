{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module GenText where

import           Relude                  hiding ( First
                                                , get
                                                )

import           Discord

import           Control.Lens            hiding ( Context )
import           Data.Aeson
import           Data.Default
import           Data.Scientific                ( Scientific
                                                , fromFloatDigits
                                                )
import qualified Data.Text                     as T
import           DiscordUtils
import           Network.Wreq                   ( defaults
                                                , header
                                                , post
                                                , postWith
                                                , responseBody
                                                )
import           System.Random
import           Utils

int2sci :: Int -> Scientific
int2sci = (fromFloatDigits :: Double -> Scientific) . toEnum


data GPTOpts = GPTOpts
    { temperature :: Scientific
    , tokens      :: Int
    , topK        :: Int
    , topP        :: Scientific
    }

instance Default GPTOpts where
    def = GPTOpts { temperature = 0.8, tokens = 32, topK = 999, topP = 1 }

newtype TextSynthRes = TextSynthRes { fromGPTRes :: Text }
instance FromJSON TextSynthRes where
    parseJSON =
        withObject "TextSynthRes" ((.: "text") >=> return . TextSynthRes)

getGPT :: Text -> DictM Text
getGPT = getGPTWith def

getGPTWith :: GPTOpts -> Text -> DictM Text
getGPTWith GPTOpts { temperature = tp, tokens = tk, topK = k, topP = p } prompt
    = do
        apiKey <- readFile "tskey.txt"
        res    <- liftIO $ postWith
            (  defaults
            &  header "Authorization"
            .~ ["Bearer " <> encodeUtf8 apiKey]
            )
            "https://api.textsynth.com/v1/engines/gptj_6B/completions"
            (object
                [ ("prompt"     , String prompt)
                , ("max_tokens" , Number (int2sci tk))
                , ("temperature", Number tp)
                , ("top_k"      , Number (int2sci k))
                , ("top_p"      , Number p)
                ]
            )
        hoistEither
            . bimap (Fuckup . fromString) fromGPTRes
            . eitherDecode
            . view responseBody
            $ res

makePrompt :: [Text] -> Text
makePrompt = (<> "\n-") . unlines . map ("- " <>)

getGPTFromExamples :: [Text] -> DictM Text
getGPTFromExamples = getGPT . makePrompt

getGPTFromContext :: Text -> [Text] -> DictM Text
getGPTFromContext context = getGPT . ((context <> ":\n") <>) . makePrompt


newtype AI21Res = AI21Res { fromJ1Res :: Text }
instance FromJSON AI21Res where
    parseJSON = withObject
        "AI21Res"
        (   (.: "completions")
        >=> (.: "data")
        .   head
        >=> (.: "text")
        >=> (return . AI21Res)
        )

data J1Opts = J1Opts
    { j1Temp :: Scientific
    , j1TopP :: Scientific
    }

getJ1 :: Int -> Text -> DictM Text
getJ1 = getJ1With $ J1Opts { j1Temp = 1, j1TopP = 0.9 }

getJ1With :: J1Opts -> Int -> Text -> DictM Text
getJ1With J1Opts { j1Temp = j1Temp', j1TopP = j1TopP' } tokens prompt = do
    rng    <- newStdGen
    apiKey <-
        readFile "j1key.txt"
        <&> encodeUtf8
        .   flip randomChoice rng
        .   lines
        .   T.strip
        .   fromString
    print apiKey
    res <- liftIO
        (postWith
            (defaults & header "Authorization" .~ ["Bearer " <> apiKey])
            "https://api.ai21.com/studio/v1/j1-jumbo/complete"
            (object
                [ ("prompt"     , String prompt)
                , ("maxTokens"  , Number (int2sci tokens))
                , ("temperature", Number j1Temp')
                , ("topP"       , Number j1TopP')
                ]
            )
        )
    hoistEither
        . bimap (Fuckup . fromString) fromJ1Res
        . eitherDecode
        . view responseBody
        $ res

getJ1FromContext :: Int -> Text -> [Text] -> DictM Text
getJ1FromContext n context = getJ1 n . ((context <> ":\n") <>) . makePrompt
