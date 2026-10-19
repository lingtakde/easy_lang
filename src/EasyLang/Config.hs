{-# LANGUAGE OverloadedStrings #-}

module EasyLang.Config
  ( Config (..)
  , configPath
  , parseConfig
  , loadConfig
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EasyLang.LanguagePairs (LanguagePair, validatePairs)
import EasyLang.Reasoning (parseReasoningEffort)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import qualified Toml

data Config = Config
  { configSourceLanguage :: Text
  , configTargetLanguage :: Text
  , configModel :: Text
  , configTimeoutSeconds :: Int
  , configMaxTokens :: Int
  , configSpinner :: Bool
  , configLanguagePairs :: [LanguagePair]
  , configReasoningEffort :: Text
  } deriving (Eq, Show)

configPath :: IO FilePath
configPath = (</> ".easy-lang" </> "config.toml") <$> getHomeDirectory

parseConfig :: Text -> Either Text Config
parseConfig contents = do
  table <- case Toml.parse (T.unpack contents) of
    Left _ -> Left "Invalid TOML syntax. Check quotes, escapes, and duplicate keys."
    Right value -> Right value
  let knownKeys = ["source_language", "target_language", "model", "timeout", "max_tokens", "spinner", "language_pairs", "reasoning_effort"]
  if any (`notElem` knownKeys) (Map.keys table)
    then Left "Unknown configuration key. Allowed keys: source_language, target_language, model, timeout, max_tokens, spinner, language_pairs, reasoning_effort."
    else Config
      <$> readString table "source_language"
      <*> readString table "target_language"
      <*> readString table "model"
      <*> readInteger table "timeout" 3600
      <*> readInteger table "max_tokens" 262144
      <*> readBool table "spinner"
      <*> readPairs table
      <*> (readString table "reasoning_effort" >>= parseReasoningEffort)
  where
    required table key = case Map.lookup key table of
      Nothing -> Left ("Missing required configuration key: " <> T.pack key <> ".")
      Just value -> Right value
    readString table key = do
      value <- required table key
      case value of
        Toml.String string | not (T.null (T.strip (T.pack string))) -> Right (T.strip (T.pack string))
        _ -> Left (T.pack key <> " must be a non-empty TOML string.")
    readInteger table key upper = do
      value <- required table key
      case value of
        Toml.Integer n | n > 0 && n <= upper -> Right (fromInteger n)
        _ -> Left (T.pack key <> " must be an integer between 1 and " <> T.pack (show upper) <> ".")
    readBool table key = do
      value <- required table key
      case value of
        Toml.Bool enabled -> Right enabled
        _ -> Left (T.pack key <> " must be a TOML boolean (true or false).")
    readPairs table = do
      value <- required table "language_pairs"
      case value of
        Toml.Array pairs -> traverse readPair pairs >>= validatePairs
        _ -> Left "language_pairs must be an array of two-language arrays, e.g. [[\"zh\", \"en\"]]."
    readPair (Toml.Array [Toml.String firstLanguage, Toml.String secondLanguage]) =
      Right (T.pack firstLanguage, T.pack secondLanguage)
    readPair _ = Left "Each language_pairs entry must contain exactly two language-code strings."

loadConfig :: FilePath -> IO (Either Text Config)
loadConfig path = do
  result <- tryIO $ do
    bytes <- BS.readFile path
    pure $ case TE.decodeUtf8' bytes of
      Left _ -> Left "Configuration must be valid UTF-8."
      Right contents -> parseConfig contents
  pure $ case result of
    Left exception
      | isDoesNotExistError exception -> Left (prefix <> "Missing configuration file. Create it using config.example.toml.")
      | otherwise -> Left (prefix <> "Cannot read the file. Check the path and permissions.")
    Right (Left message) -> Left (prefix <> message)
    Right (Right config) -> Right config
  where
    prefix = "Config " <> T.pack path <> ": "

tryIO :: IO a -> IO (Either IOException a)
tryIO = try
