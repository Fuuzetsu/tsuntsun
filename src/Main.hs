{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Temp
import Control.Applicative
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS
import Control.Monad
import System.FSNotify
import System.Environment
import System.Exit
import Control.Concurrent
import Data.Text (pack)
import System.Directory
import qualified GHC.IO as I

data Notifier a = Notifier { _in :: MVar a
                           , _out :: MVar a
                           }

condM :: Monad m => (a -> t -> m b) -> m a -> t -> m b
condM c p a = p >>= \p' -> c p' a

whenM :: Monad m => m Bool -> m () -> m ()
whenM = condM when

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM = condM unless

ensureFile :: FilePath -> IO ()
ensureFile f = do
  let d = dirname f
      f' = encodeString f
  createDirectoryIfMissing True (encodeString d)
  unlessM (doesFileExist f') $ writeFile f' ""

main :: IO ()
main = getArgs >>= \case
  [i, t] -> run i (Just t)
  [i] -> run i Nothing
  _ -> putStrLn help >> exitWith (ExitFailure 1)

run :: I.FilePath -> Maybe I.FilePath -> IO a
run i t = do
  whenM (doesDirectoryExist i) $ directoryExit
  n <- Notifier <$> newEmptyMVar <*> newEmptyMVar

  _ <- forkIO $ withSystemTempFile "tsuntsun" $ \o h ->
    watch (fromText . pack $ i) (fromText . pack $ o) n
  putStrLn "Started watching…"
  forever $ do
    i' <- takeMVar (_in n)
    print i'

directoryExit :: IO ()
directoryExit = do
  putStrLn "A filename must be given, got directory instead."
  exitWith $ ExitFailure 1

help :: String
help = "usage: tsuntsun image-watch-filename [tesseract-path]"

watch :: FilePath -> FilePath -> Notifier Event -> IO ()
watch i o Notifier { _in = mi, _out = mo } = withManager $ \mgr -> do
  ensureFile i
  let i' = encodeString i
  doesFileExist i' >>= \case
    True -> return ()
    False -> writeFile i' ""
  watchDir mgr (directory i) isWatchedFile react
  forever $ threadDelay maxBound
  where
    react :: Event -> IO ()
    react = void . tryPutMVar mi

    isWatchedFile :: Event -> Bool
    isWatchedFile (Added f'' _) = f'' == i || f'' == o
    isWatchedFile (Modified f'' _) = f'' == i || f'' == o
    isWatchedFile (Removed f'' _) = f'' == i || f'' == o
