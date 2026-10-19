{-# LANGUAGE OverloadedStrings #-}

module EasyLang.Options
  ( Options (..)
  , Input (..)
  , optionsInfo
  ) where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import EasyLang.Config (Config (..))
import EasyLang.LanguagePairs (LanguagePair, parsePairList, parsePairSpecs)
import EasyLang.Reasoning (parseReasoningEffort)
import Options.Applicative
import Text.Read (readMaybe)

data Input = Stdin | File FilePath | Inline [Text]
  deriving (Eq, Show)

data Options = Options
  { targetLanguage :: Text
  , sourceLanguage :: Text
  , model :: Text
  , timeoutSeconds :: Int
  , maxTokens :: Int
  , input :: Input
  , noSpinner :: Bool
  , languagePairs :: [LanguagePair]
  , reasoningEffort :: Text
  } deriving (Eq, Show)

optionsInfo :: Config -> [(String, String)] -> ParserInfo (Either Text Options)
optionsInfo config environment = info
  (options <**> helper <**> infoOption "easy-lang 0.1.0.0" (long "version" <> help "Show version"))
  (fullDesc
    <> progDesc "Translate text with one independent, non-streaming OpenRouter request"
    <> footer "Reads UTF-8 stdin if TEXT and --file are omitted. Required config: ~/.easy-lang/config.toml. Precedence: CLI > environment > config. Explicit --to or EASY_LANG_TO bypasses language pairs. Set OPENROUTER_API_KEY before use.")
  where
    options = resolve
      <$> optional (option nonempty (long "to" <> short 't' <> metavar "LANG"
            <> configHelp "Target language (env: EASY_LANG_TO)" (configTargetLanguage config)))
      <*> optional (option nonempty (long "from" <> metavar "LANG"
            <> configHelp "Source language, or auto (env: EASY_LANG_FROM)" (configSourceLanguage config)))
      <*> optional (option nonempty (long "model" <> short 'm' <> metavar "MODEL"
            <> configHelp "OpenRouter model ID (env: EASY_LANG_MODEL)" (configModel config)))
      <*> optional (option (boundedPositive 3600) (long "timeout" <> metavar "SECONDS"
            <> configHelp "Request timeout, 1-3600 seconds (env: EASY_LANG_TIMEOUT)" (T.pack (show (configTimeoutSeconds config)))))
      <*> optional (option (boundedPositive 262144) (long "max-tokens" <> metavar "N"
            <> configHelp "Output limit, 1-262144 tokens (env: EASY_LANG_MAX_TOKENS)" (T.pack (show (configMaxTokens config)))))
      <*> (File <$> strOption (long "file" <> short 'f' <> metavar "PATH" <> help "Read a UTF-8 file; use - for stdin")
            <|> Inline <$> some (T.pack <$> strArgument (metavar "TEXT"))
            <|> pure Stdin)
      <*> optional
        (flag' True (long "spinner"
          <> configHelp "Enable animation (env: EASY_LANG_SPINNER)" (if configSpinner config then "true" else "false"))
        <|> flag' False (long "no-spinner" <> help "Disable the terminal waiting animation"))
      <*> optional
        (some (option nonempty (long "pair" <> metavar "A:B"
          <> help "Replace language pairs; repeat for multiple pairs (env: EASY_LANG_PAIRS)"))
        <|> flag' [] (long "no-pairs" <> help "Disable automatic bidirectional language pairs"))
      <*> optional (option reasoningEffortReader (long "reasoning-effort" <> metavar "LEVEL"
            <> configHelp "Reasoning level: none/minimal/low/medium/high/xhigh/max (env: EASY_LANG_REASONING_EFFORT)" (configReasoningEffort config)))
    resolve target source selectedModel seconds tokens selectedInput spinner pairSpecs selectedReasoningEffort = do
      cliPairs <- traverse parsePairSpecs pairSpecs
      selectedPairs <- choose cliPairs "EASY_LANG_PAIRS" (parsePairList . T.pack) (configLanguagePairs config)
      let fixedTarget = case target of
            Just _ -> True
            Nothing -> lookup "EASY_LANG_TO" environment /= Nothing
      Options
        <$> choose target "EASY_LANG_TO" parseNonempty (configTargetLanguage config)
        <*> choose source "EASY_LANG_FROM" parseNonempty (configSourceLanguage config)
        <*> choose selectedModel "EASY_LANG_MODEL" parseNonempty (configModel config)
        <*> choose seconds "EASY_LANG_TIMEOUT" (parseBounded 3600) (configTimeoutSeconds config)
        <*> choose tokens "EASY_LANG_MAX_TOKENS" (parseBounded 262144) (configMaxTokens config)
        <*> pure selectedInput
        <*> (not <$> choose spinner "EASY_LANG_SPINNER" parseBool (configSpinner config))
        <*> pure (if fixedTarget then [] else selectedPairs)
        <*> choose selectedReasoningEffort "EASY_LANG_REASONING_EFFORT" (parseReasoningEffort . T.pack) (configReasoningEffort config)
    -- Resolve each field independently, validating only the highest-precedence
    -- supplied value. A valid CLI option can override even an invalid env value.
    choose (Just cli) _ _ _ = Right cli
    choose Nothing name parseValue configured = case lookup name environment of
      Nothing -> Right configured
      Just raw -> first (\message -> T.pack name <> ": " <> message) (parseValue raw)

configHelp :: String -> Text -> Mod f a
configHelp label configured = help (label <> " (config: " <> T.unpack configured <> ")")

nonempty :: ReadM Text
nonempty = eitherReader (first T.unpack . parseNonempty)

reasoningEffortReader :: ReadM Text
reasoningEffortReader = eitherReader (first T.unpack . parseReasoningEffort . T.pack)

parseNonempty :: String -> Either Text Text
parseNonempty raw =
  let trimmed = T.strip (T.pack raw)
  in if T.null trimmed then Left "Value must not be empty" else Right trimmed

boundedPositive :: Int -> ReadM Int
boundedPositive upper = eitherReader (first T.unpack . parseBounded upper)

parseBounded :: Int -> String -> Either Text Int
parseBounded upper raw = case readMaybe raw :: Maybe Integer of
  Just n | n > 0 && n <= toInteger upper -> Right (fromInteger n)
  _ -> Left ("Expected an integer between 1 and " <> T.pack (show upper))

parseBool :: String -> Either Text Bool
parseBool raw = case T.toCaseFold (T.strip (T.pack raw)) of
  "true" -> Right True
  "false" -> Right False
  _ -> Left "Expected true or false"
