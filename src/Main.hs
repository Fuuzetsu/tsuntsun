{-# LANGUAGE LambdaCase #-}
module Main where

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

data Notifier a = Notifier { _updated :: MVar a }

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
  [i] -> do
    whenM (doesDirectoryExist i) $ directoryExit
    n <- Notifier <$> newEmptyMVar
    _ <- forkIO $ watch (fromText . pack $ i) n
    putStrLn "Started watching…"
    forever $ do
      r <- takeMVar (_updated n)
      print r
  _ -> putStrLn help >> exitWith (ExitFailure 1)

directoryExit :: IO ()
directoryExit = do
  putStrLn "A filename must be given, got directory instead."
  exitWith $ ExitFailure 1

help :: String
help = "usage: tsuntsun image-watch-filename"

watch :: FilePath -> Notifier Event -> IO ()
watch f Notifier { _updated = m } = withManager $ \mgr -> do
  ensureFile f
  let f' = encodeString f
  doesFileExist f' >>= \case
    True -> return ()
    False -> writeFile f' ""
  watchDir mgr (directory f) isWatchedFile react
  forever $ threadDelay maxBound
  where
    react :: Event -> IO ()
    react = void . tryPutMVar m

    isWatchedFile (Added f'' _) = f'' == f
    isWatchedFile (Modified f'' _) = f'' == f
    isWatchedFile (Removed f'' _) = f'' == f
