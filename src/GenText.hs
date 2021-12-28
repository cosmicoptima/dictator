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
import           Control.Monad.Random           ( randomRIO )
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

int2sci :: Int -> Scientific
int2sci = (fromFloatDigits :: Double -> Scientific) . toEnum


data GPTOpts = GPTOpts
    { temperature :: Scientific
    , topK        :: Int
    , topP        :: Scientific
    }

instance Default GPTOpts where
    def = GPTOpts { temperature = 0.8, topK = 999, topP = 1 }

newtype TextSynthRes = TextSynthRes { fromGPTRes :: Text }
instance FromJSON TextSynthRes where
    parseJSON =
        withObject "TextSynthRes" ((.: "text") >=> return . TextSynthRes)

getGPT :: Text -> DictM Text
getGPT = getGPTWith def

getGPTWith :: GPTOpts -> Text -> DictM Text
getGPTWith GPTOpts { temperature = t, topK = k, topP = p } prompt = do
    seed <- randomRIO (0, 2 ^ (16 :: Int)) <&> int2sci
    res  <- liftIO $ post
        "https://bellard.org/textsynth/api/v1/engines/gptj_6B/completions"
        (object
            [ ("prompt"     , String prompt)
            , ("seed"       , Number seed)
            , ("stream"     , Bool False)
            , ("temperature", Number t)
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

getJ1 :: Int -> Text -> DictM Text
getJ1 tokens prompt = do
    apiKey <- readFile "j1key.txt" <&> encodeUtf8 . T.strip . fromString
    print apiKey
    res <- liftIO
        (postWith
            (defaults & header "Authorization" .~ ["Bearer " <> apiKey])
            "https://api.ai21.com/studio/v1/j1-jumbo/complete"
            (object
                [ ("prompt"   , String prompt)
                , ("maxTokens", Number (int2sci tokens))
                , ("topP"     , Number 0.9)
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
