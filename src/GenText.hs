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
import           DiscordUtils
import           Network.Wreq                   ( defaults
                                                , header
                                                , post
                                                , postWith
                                                , responseBody
                                                )
import           Prelude                        ( IOError )
import           UnliftIO


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

getGPT :: Text -> DH Text
getGPT = getGPTWith def

getGPTWith :: GPTOpts -> Text -> DH Text
getGPTWith GPTOpts { temperature = t, topK = k, topP = p } prompt = do
    seed <- randomRIO (0, 2 ^ 16) <&> intToSci
    res  <- liftIO $ post
        "https://bellard.org/textsynth/api/v1/engines/gptj_6B/completions"
        (object
            [ ("prompt"     , String prompt)
            , ("seed"       , Number seed)
            , ("stream"     , Bool False)
            , ("temperature", Number t)
            , ("top_k"      , Number (intToSci k))
            , ("top_p"      , Number p)
            ]
        )
    either (debugDie . fromString) (return . fromGPTRes)
        . eitherDecode
        . view responseBody
        $ res
    where intToSci = (fromFloatDigits :: Double -> Scientific) . toEnum

makePrompt :: [Text] -> Text
makePrompt = (<> "\n-") . unlines . map ("- " <>)

getGPTFromExamples :: [Text] -> DH Text
getGPTFromExamples = getGPT . makePrompt

getGPTFromContext :: Text -> [Text] -> DH Text
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

getJ1 :: Text -> DH Text
getJ1 prompt = do
    debugPutStr "before reading file"
    apiKey <-
        (readFile "j1key.txt" <&> fromString)
            `catch` (\(e :: IOError) -> debugPrint e >> return "")
    debugPutStr "after reading file, before making request"
    res <- liftIO $ postWith
        (defaults & header "Authorization" .~ ["Bearer " <> apiKey])
        "https://api.ai21.com/studio/v1/j1-jumbo/complete"
        (object
            [ ("prompt"     , String prompt)
            , ("numResults" , Number 1)
            , ("maxTokens"  , Number 8)
            , ("topKReturn" , Number 40)
            , ("temperature", Number 1)
            ]
        )
    debugPutStr "after making request, before parsing request"
    either (debugDie . fromString) (return . fromJ1Res)
        . eitherDecode
        . view responseBody
        $ res
