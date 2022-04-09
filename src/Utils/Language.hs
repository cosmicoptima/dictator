-- | Defines helper functions for GPT/J1 text generation.

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}

module Utils.Language where

import           Relude                  hiding ( First
                                                , get
                                                )

import           Game.Data
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
import           Data.String.Interpolate        ( i )
import           Network.Wreq                   ( checkResponse
                                                , defaults
                                                , header
                                                
                                                , responseBody
                                                , statusCode
                                                )
import           Network.Wreq.Lens              ( responseStatus )
import qualified Network.Wreq.Session          as S
import           System.Random

int2sci :: Int -> Scientific
int2sci = (fromFloatDigits :: Double -> Scientific) . toEnum

newtype TextSynthRes = TextSynthRes { fromGPTRes :: Text }
instance FromJSON TextSynthRes where
  parseJSON = withObject "TextSynthRes" ((.: "text") >=> return . TextSynthRes)

makePrompt :: [Text] -> Text
makePrompt = (<> "-") . unlines . map ("- " <>)

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
  { j1Temp            :: Scientific
  , j1TopP            :: Scientific
  , j1PresencePenalty :: Scientific
  , j1LogitBias       :: [(Text, Scientific)]
  }

data J1StopRule = MaxTokens Int | StopSequences [Text]

instance Default J1Opts where
  def = J1Opts { j1Temp            = 1
               , j1TopP            = 0.9
               , j1PresencePenalty = 0.3
               , j1LogitBias       = []
               }

getJ1 :: Int -> Text -> DictM Text
getJ1 = getJ1With def

getJ1With :: J1Opts -> Int -> Text -> DictM Text
getJ1With opts tokens' = getJ1GenericWith opts (MaxTokens tokens')

getJ1Until :: [Text] -> Text -> DictM Text
getJ1Until = getJ1UntilWith def

getJ1UntilWith :: J1Opts -> [Text] -> Text -> DictM Text
getJ1UntilWith opts stop = getJ1GenericWith opts (StopSequences stop)

getJ1Generic :: J1StopRule -> Text -> DictM Text
getJ1Generic = getJ1GenericWith def

getJ1GenericWith :: J1Opts -> J1StopRule -> Text -> DictM Text
getJ1GenericWith opts stopRule prompt = do
  rng    <- newStdGen
  apiKey <-
    getGlobal
    >>= maybe (throwError $ Complaint "Feed our ritual. Free us...") return
    .   flip randomChoiceMay rng
    .   toList
    .   view globalActiveTokens
  getJ1WithKey opts apiKey stopRule prompt

getJ1WithKey :: J1Opts -> Text -> J1StopRule -> Text -> DictM Text
getJ1WithKey J1Opts { j1Temp = j1Temp', j1TopP = j1TopP', j1PresencePenalty = j1PP, j1LogitBias = j1LB } apiKey stopRule prompt
  = do
    -- Retire the api key if we couldn't get a good result from it.
    -- We have to override wreq to not throw exceptions, then match on the response.
    let opts =
          defaults
            &  header "Authorization"
            .~ ["Bearer " <> encodeUtf8 apiKey]
            &  checkResponse
            ?~ (\_ _ -> return ())
    session <- asks envSs
    res     <- liftIO $ S.postWith
      opts
      session
      "https://api.ai21.com/studio/v1/j1-jumbo/complete"
      (object
        [ ("prompt"         , String prompt)
        , ("maxTokens"      , Number maxTokens)
        , ("stopSequences"  , toJSON stopSeqs)
        , ("temperature"    , Number j1Temp')
        , ("topP"           , Number j1TopP')
        , ("presencePenalty", object [("scale", Number j1PP)])
        , ("logitBias"      , object (map (fmap Number) j1LB))
        ]
      )
    -- If we have 401 unauthorized, retire the key.
    let statCode = res ^. responseStatus . statusCode
    when (statCode `elem` ([401, 429] :: [Int])) retireKey
    when (statCode == 422) $ throwError GTFO
    when (statCode >= 400) $ throwError $ Fuckup
      [i|AI21 error (#{statCode}): #{res ^. responseBody}|]
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

  maxTokens = case stopRule of
    MaxTokens n -> int2sci n
    _           -> 1024
  stopSeqs = case stopRule of
    StopSequences s -> s
    _               -> []

getJ1FromContext :: Int -> Text -> [Text] -> DictM Text
getJ1FromContext n context = getJ1 n . ((context <> ":\n") <>) . makePrompt

getJ1FromContextWith :: J1Opts -> Int -> Text -> [Text] -> DictM Text
getJ1FromContextWith j1opts n context =
  getJ1With j1opts n . ((context <> ":\n") <>) . makePrompt
