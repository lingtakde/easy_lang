module EasyLang.Progress (withProgress) where

import Control.Concurrent
  ( ThreadId, forkFinally, killThread, newEmptyMVar, putMVar, readMVar, threadDelay )
import Control.Concurrent.MVar (MVar)
import Control.Exception (IOException, bracket, catch, mask_)
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric (showFFloat)
import System.Environment (lookupEnv)
import System.IO (hFlush, hIsTerminalDevice, hPutStr, stderr)
import Text.Read (readMaybe)

-- Keep progress on stderr so redirected translations contain only their text.
-- The caller reads input before this action and prints its result afterwards.
withProgress :: Bool -> IO a -> IO a
withProgress allowed action = do
  enabled <- if allowed then terminalSupportsProgress else pure False
  if enabled then bracket startProgress stopProgress (const action) else action

terminalSupportsProgress :: IO Bool
terminalSupportsProgress = check `catch` unavailable
  where
    check = do
      terminal <- hIsTerminalDevice stderr
      term <- lookupEnv "TERM"
      columns <- lookupEnv "COLUMNS"
      -- Leave room for the longest status and avoid the terminal's last column.
      let wideEnough = maybe True (> (20 :: Int)) (columns >>= readMaybe)
      pure (terminal && term /= Just "dumb" && wideEnough)
    unavailable :: IOException -> IO Bool
    unavailable _ = pure False

data Progress = Progress ThreadId (MVar ()) (IORef Bool)

startProgress :: IO Progress
startProgress = do
  started <- getMonotonicTimeNSec
  displayed <- newIORef False
  finished <- newEmptyMVar
  worker <- forkFinally
    (threadDelay 150000 >> animate started displayed 0)
    (const (putMVar finished ()))
  pure (Progress worker finished displayed)

stopProgress :: Progress -> IO ()
stopProgress (Progress worker finished displayed) = do
  killThread worker
  -- Cancellation alone is insufficient: wait until no frame can be written
  -- before clearing the line or letting the caller print a result/error.
  readMVar finished
  visible <- readIORef displayed
  when visible $ ignoreIO (hPutStr stderr "\r\ESC[2K" >> hFlush stderr)

animate :: Word64 -> IORef Bool -> Int -> IO ()
animate started displayed frame = do
  now <- getMonotonicTimeNSec
  let seconds = fromIntegral (now - started) / 1000000000 :: Double
      elapsed = if seconds >= 1000 then "999+" else showFFloat (Just 1) seconds ""
      glyph = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" !! frame
  -- A pending cancellation must not leave an emitted frame marked invisible.
  mask_ $ do
    writeIORef displayed True
    hPutStr stderr ("\r" ++ [glyph] ++ " Translating " ++ elapsed ++ "s\ESC[K")
    hFlush stderr
  threadDelay 80000
  animate started displayed ((frame + 1) `mod` 10)

ignoreIO :: IO () -> IO ()
ignoreIO action = action `catch` ignore
  where
    ignore :: IOException -> IO ()
    ignore _ = pure ()
