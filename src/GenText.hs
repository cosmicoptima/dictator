data GPTOpts = GPTOpts
    { temperature :: Scientific
    , topK        :: Int
    , topP        :: Scientific
    }

instance Default GPTOpts where
    def = GPTOpts { temperature = 0.8, topK = 40, topP = 1 }

newtype TextSynthRes = TextSynthRes { fromGPTRes :: Text }
instance FromJSON TextSynthRes where
    parseJSON =
        withObject "TextSynthRes" ((.: "text") >=> return . TextSynthRes)

getGPT :: Text -> DH Text
getGPT = getGPTWith def

getGPTWith :: GPTOpts -> Text -> DH Text
getGPTWith GPTOpts { temperature = t, topK = k, topP = p } prompt = do
    res <- liftIO $ post
        "https://bellard.org/textsynth/api/v1/engines/gptj_6B/completions"
        (object
            [ ("prompt"     , String prompt)
            , ("seed"       , Number 0)
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
makePrompt = (<> "\n-") . unlines . map (" -" <>)

getGPTFromExamples :: [Text] -> DH Text
getGPTFromExamples = getGPT . makePrompt