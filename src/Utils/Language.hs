-- | Defines helper functions for GPT/J1 text generation.

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Utils.Language where

import           Relude                  hiding ( First
                                                , get
                                                )

import           Utils
import           Utils.DictM

import           Control.Lens            hiding ( Context )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Aeson
import           Data.Default
import           Data.Scientific                ( Scientific
                                                , fromFloatDigits
                                                )
import qualified Data.Set                      as Set
import           Game.Data                      ( getGlobal
                                                , globalActiveTokens
                                                , globalExhaustedTokens
                                                , setGlobal
                                                )
import           Network.Wreq                   ( checkResponse
                                                , defaults
                                                , header
                                                , postWith
                                                , responseBody
                                                , statusCode
                                                )
import           Network.Wreq.Lens              ( responseStatus )
import           System.Random

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

instance Default J1Opts where
    def = J1Opts { j1Temp = 1, j1TopP = 0.9 }

getJ1 :: Int -> Text -> DictM Text
getJ1 = getJ1With def

getJ1With :: J1Opts -> Int -> Text -> DictM Text
getJ1With opts tokens' prompt = do
    rng    <- newStdGen
    apiKey <-
        getGlobal
        >>= maybe (throwError $ Complaint "Feed our ritual. Free us...") return
        .   flip randomChoiceMay rng
        .   toList
        .   view globalActiveTokens
    getJ1WithKey opts apiKey tokens' prompt

getJ1WithKey :: J1Opts -> Text -> Int -> Text -> DictM Text
getJ1WithKey J1Opts { j1Temp = j1Temp', j1TopP = j1TopP' } apiKey tokens' prompt
    = do
    -- Retire the api key if we couldn't get a good result from it.
    -- We have to override wreq to not throw exceptions, then match on the response.
        let opts =
                defaults
                    &  header "Authorization"
                    .~ ["Bearer " <> encodeUtf8 apiKey]
                    &  checkResponse
                    ?~ (\_ _ -> return ())
        res <- liftIO $ postWith
            opts
            "https://api.ai21.com/studio/v1/j1-jumbo/complete"
            (object
                [ ("prompt"     , String prompt)
                , ("maxTokens"  , Number (int2sci tokens'))
                , ("temperature", Number j1Temp')
                , ("topP"       , Number j1TopP')
                ]
            )
        -- If we have 401 unauthorized, retire the key.
        when (res ^. responseStatus . statusCode == 401) retireKey
        -- Also retire it if we couldn't decode the output, because that's *probably* a mistake
        either (const retireKey) (return . fromJ1Res)
            . eitherDecode
            . view responseBody
            $ res

  where
    retireKey = do
        current <- getGlobal
        setGlobal
            $  current
            &  globalActiveTokens
            %~ Set.delete apiKey
            &  globalExhaustedTokens
            %~ Set.insert apiKey
        throwError $ Complaint "The sacrifice is incomplete."

getJ1FromContext :: Int -> Text -> [Text] -> DictM Text
getJ1FromContext n context = getJ1 n . ((context <> ":\n") <>) . makePrompt
