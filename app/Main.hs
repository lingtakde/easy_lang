{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Char (isAscii, isSpace)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import EasyLang.Client (translate)
import EasyLang.Config (configPath, loadConfig)
import EasyLang.Options
import EasyLang.Progress (withProgress)
import GHC.IO.Encoding (setFileSystemEncoding)
import Options.Applicative (execParser)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hIsTerminalDevice, hSetEncoding, stderr, stdin, stdout, utf8)

main :: IO ()
main = do
  setFileSystemEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  run `catch` handleIOError

run :: IO ()
run = do
  path <- configPath
  config <- loadConfig path >>= either die pure
  environment <- fmap catMaybes $ mapM readOverride
    [ "EASY_LANG_FROM", "EASY_LANG_TO", "EASY_LANG_MODEL"
    , "EASY_LANG_TIMEOUT", "EASY_LANG_MAX_TOKENS", "EASY_LANG_SPINNER"
    , "EASY_LANG_PAIRS", "EASY_LANG_REASONING_EFFORT"
    ]
  options <- execParser (optionsInfo config environment) >>= either die pure
  source <- readInput (input options)
  when (T.null (T.strip source)) $ die "Input is empty. Pass TEXT, --file PATH, or pipe text to stdin."
  rawKey <- lookupEnv "OPENROUTER_API_KEY"
  apiKey <- case T.strip . T.pack <$> rawKey of
    Just key | not (T.null key) && T.all validKeyChar key -> pure key
    _ -> die "Set OPENROUTER_API_KEY first: export OPENROUTER_API_KEY=\"$(omp token openrouter)\""
  result <- withProgress (not (noSpinner options)) (translate apiKey options source)
  case result of
    Left message -> die message
    Right output -> do
      TIO.putStr output
      when (not ("\n" `T.isSuffixOf` output)) (TIO.putStr "\n")
  where
    validKeyChar c = isAscii c && not (isSpace c) && c >= '!' && c <= '~'
    readOverride name = fmap (\value -> (name, value)) <$> lookupEnv name

readInput :: Input -> IO Text
readInput (Inline pieces) = pure (T.intercalate " " pieces)
readInput (File "-") = readStdin
readInput (File path) = BS.readFile path >>= decodeInput
readInput Stdin = readStdin

readStdin :: IO Text
readStdin = do
  terminal <- hIsTerminalDevice stdin
  when terminal $ die "Pass TEXT, --file PATH, or pipe text to stdin. See easy-lang --help."
  BS.getContents >>= decodeInput

decodeInput :: BS.ByteString -> IO Text
decodeInput bytes = case TE.decodeUtf8' bytes of
  Left _ -> die "Input must be valid UTF-8 text."
  Right text -> pure text

-- File and terminal exceptions should never spill request data or credentials.
handleIOError :: IOException -> IO ()
handleIOError _ = die "Could not read input or write output. Check file paths, permissions, and the output pipe."

die :: Text -> IO a
die message = TIO.hPutStrLn stderr ("easy-lang: " <> message) >> exitFailure
