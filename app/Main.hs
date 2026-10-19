{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
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
import GHC.IO.Encoding (initLocaleEncoding, setFileSystemEncoding)
import Options.Applicative (execParser)
import System.Console.Haskeline
  ( Settings (..), getInputLine, haveTerminalUI, noCompletion, runInputT )
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, hIsTerminalDevice, hSetEncoding, stderr, stdin, stdout, utf8)
import System.IO.Error (isEOFError, tryIOError)

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
  case input options of
    Interactive -> runInteractive options
    Stdin -> do
      terminal <- hIsTerminalDevice stdin
      if terminal then runInteractive options else runOnce options readStdin
    Inline pieces -> runOnce options (pure (T.intercalate " " pieces))
    File "-" -> runOnce options readStdin
    File filePath -> runOnce options (BS.readFile filePath >>= decodeInput)
  where
    readOverride name = fmap (\value -> (name, value)) <$> lookupEnv name

runOnce :: Options -> IO Text -> IO ()
runOnce options readSource = do
  source <- readSource
  when (T.null (T.strip source)) $ die "Input is empty. Pass TEXT, --file PATH, or pipe text to stdin."
  apiKey <- readApiKey
  result <- withProgress (not (noSpinner options)) (translate apiKey options source)
  either die writeTranslation result

runInteractive :: Options -> IO ()
runInteractive options = do
  apiKey <- readApiKey
  terminal <- (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice stderr
  when terminal $ TIO.hPutStrLn stderr
    "Interactive mode: enter one line per translation. Press Ctrl+D or Ctrl+C to exit."
  -- Haskeline uses the initial locale encoding, even after hSetEncoding.
  -- Keep byte-based UTF-8 input when that encoding is not UTF-8.
  if terminal && show initLocaleEncoding == "UTF-8"
    then runInputT lineSettings $ do
      terminalUI <- haveTerminalUI
      -- File-style Haskeline prompts use stdout, so retain our own fallback.
      if terminalUI then terminalLoop apiKey else liftIO (loop apiKey terminal)
    else loop apiKey terminal
  where
    terminalLoop apiKey = do
      next <- getInputLine "> "
      case next of
        Nothing -> pure ()
        Just source -> do
          liftIO (translateLine apiKey (T.pack source))
          terminalLoop apiKey
    loop apiKey terminal = do
      when terminal $ TIO.hPutStr stderr "> " >> hFlush stderr
      next <- tryIOError (BS.hGetLine stdin)
      case next of
        Left exception
          | isEOFError exception -> when terminal (TIO.hPutStrLn stderr "")
          | otherwise -> ioError exception
        Right bytes -> do
          case TE.decodeUtf8' bytes of
            Left _ -> reportError "Input must be valid UTF-8 text."
            Right source -> translateLine apiKey source
          loop apiKey terminal
    translateLine apiKey source = unless (T.null (T.strip source)) $ do
      result <- withProgress (not (noSpinner options)) (translate apiKey options source)
      either reportError writeTranslation result

lineSettings :: Settings IO
lineSettings = Settings
  { complete = noCompletion
  , historyFile = Nothing
  , autoAddHistory = False
  }

readApiKey :: IO Text
readApiKey = do
  rawKey <- lookupEnv "OPENROUTER_API_KEY"
  case T.strip . T.pack <$> rawKey of
    Just key | not (T.null key) && T.all validKeyChar key -> pure key
    _ -> die "Set OPENROUTER_API_KEY first: export OPENROUTER_API_KEY=\"$(omp token openrouter)\""
  where
    validKeyChar c = isAscii c && not (isSpace c) && c >= '!' && c <= '~'

writeTranslation :: Text -> IO ()
writeTranslation output = do
  TIO.putStr output
  when (not ("\n" `T.isSuffixOf` output)) (TIO.putStr "\n")
  hFlush stdout

readStdin :: IO Text
readStdin = do
  terminal <- hIsTerminalDevice stdin
  when terminal $ die "Pass TEXT, --file PATH, pipe text to stdin, or use --interactive. See easy-lang --help."
  BS.getContents >>= decodeInput

decodeInput :: BS.ByteString -> IO Text
decodeInput bytes = case TE.decodeUtf8' bytes of
  Left _ -> die "Input must be valid UTF-8 text."
  Right text -> pure text

-- File and terminal exceptions should never spill request data or credentials.
handleIOError :: IOException -> IO ()
handleIOError _ = die "Could not read input or write output. Check file paths, permissions, and the output pipe."

die :: Text -> IO a
die message = reportError message >> exitFailure

reportError :: Text -> IO ()
reportError message = TIO.hPutStrLn stderr ("easy-lang: " <> message)
