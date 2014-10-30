{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Map
import           Control.Applicative ((<$>))
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (fromMaybe)
import           Data.Text (pack, Text)
import           Data.Text.IO (readFile)
import           Filesystem.Path.CurrentOS (FilePath, encodeString,
                                            fromText, (<.>), dirname, directory)
import qualified GHC.IO as I (FilePath)
import           Graphics.UI.Gtk
import           NaturalLanguageProcessing.TsunTsun.TH (litFile)
import           Prelude hiding (FilePath, readFile)
import           System.Directory (doesDirectoryExist, createDirectoryIfMissing,
                                   doesFileExist)
import           System.Environment (getArgs)
import           System.Exit (exitWith, ExitCode(..), exitSuccess)
import           System.FSNotify (Event(..), watchDir, withManager)
import           System.IO.Temp (withSystemTempFile)
import           System.Process (spawnProcess, waitForProcess)

data Notifier a = Notifier { _in :: MVar a
                           , _out :: MVar a
                           , _tesseract :: I.FilePath
                           , _outputFile :: I.FilePath
                           , _inputFile :: I.FilePath
                           , _ocrResult :: MVar Text
                           , _img :: MVar Image
                           , _ocrMode :: MVar ImageMode
                           }

data ImageMode = Column | Vertical | Block | Line
               | Word | CircledCharacter | Character
               deriving (Eq, Show, Ord)

modes :: Map ImageMode Int
modes = fromList
  [ (Column          , 4)
  , (Vertical        , 5)
  , (Block           , 6)
  , (Line            , 7)
  , (Word            , 8)
  , (CircledCharacter, 9)
  , (Character       , 10)
  ]


condM :: Monad m => (a -> t -> m b) -> m a -> t -> m b
condM c p a = p >>= \p' -> c p' a

whenM :: Monad m => m Bool -> m () -> m ()
whenM = condM when

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM = condM unless

runWindow :: Notifier Event -> IO ()
runWindow n = do
  _ <- initGUI
  b <- builderNew
  builderAddFromString b gladeXml
  let getObj :: GObjectClass cls => (GObject -> cls) -> Text -> IO cls
      getObj = builderGetObject b

  window <- getObj castToWindow "mainWindow"
  textArea <- getObj castToTextView "textArea"
  image <- getObj castToImage "imageArea"
  aboutDialog <- getObj castToDialog "aboutDialog"

  scrotButton <- getObj castToButton "scrotButton"
  aboutButton <- getObj castToButton "aboutButton"
  quitButton <- getObj castToButton "quitButton"

  verticalRadio <- getObj castToRadioButton "verticalRadio"
  columnRadio <- getObj castToRadioButton "columnRadio"
  blockRadio <- getObj castToRadioButton "blockRadio"
  circledCharacterRadio <- getObj castToRadioButton "circledCharacterRadio"
  characterRadio <- getObj castToRadioButton "characterRadio"

  let rs = zip [ columnRadio, verticalRadio, blockRadio
               , circledCharacterRadio, characterRadio ]
               [ Column, Vertical, Block
               , CircledCharacter, Character ]
      radioConnect (r, m) = onEv toggled (radioToggled n r m) r

  mapM_ radioConnect rs

  toggleButtonSetActive verticalRadio True

  onEv buttonActivated mainQuit quitButton
  onEv buttonActivated (widgetShow aboutDialog) aboutButton
  onEv buttonActivated (void . forkIO . runScrot $ _inputFile n) scrotButton

  buttonSetLabel scrotButton ("scrot -s " ++ _inputFile n)

  mapM_ ($ window) [ onEv keyPressEvent keyPressed
                   , onEv destroyEvent . tryEvent $ liftIO mainQuit
                   , onEv deleteEvent . tryEvent $ liftIO mainQuit
                   ]

  -- Update text
  _ <- forkIO . forever $ takeMVar (_ocrResult n) >>= \t -> postGUIAsync $ do
    bn <- textViewGetBuffer textArea
    textBufferSetText bn t

  -- Update image
  _ <- forkIO . forever $ do
    image' <- takeMVar (_img n) >>= imageGetPixbuf
    postGUIAsync $ imageSetFromPixbuf image image'

  widgetShowAll window
  mainGUI

radioToggled :: Notifier Event -> RadioButton -> ImageMode -> IO ()
radioToggled Notifier { _ocrMode = o } r m =
  whenM (toggleButtonGetActive r) $ takeMVar o >> putMVar o m

onEv :: Signal object s -> s -> object -> IO (ConnectId object)
onEv v r x = x `on` v $ r

gladeXml :: String
gladeXml = [litFile|res/mainwindow.glade|]

keyPressed :: EventM EKey Bool
keyPressed = tryEvent $ do
  "q" <- eventKeyName
  liftIO mainQuit

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

-- | Extension added by tesseract.
ext :: Text
ext = pack "txt"

run :: I.FilePath -> Maybe I.FilePath -> IO ()
run i t = do
  whenM (doesDirectoryExist i) $ directoryExit
  (inm, outm, ocrr, imgm) <- liftM4 (,,,) newEmptyMVar newEmptyMVar
                                          newEmptyMVar newEmptyMVar
  ocrm <- newMVar Vertical

  o <- tmpFilename
  let n = Notifier { _in = inm
                   , _out = outm
                   , _outputFile = o
                   , _inputFile = i
                   , _tesseract = fromMaybe "tesseract" t
                   , _ocrResult = ocrr
                   , _img = imgm
                   , _ocrMode = ocrm
                   }

  -- Watch input/output files
  _ <- forkIO $ watchFile (toFP i) inm
  _ <- forkIO $ watchFile (toFP o <.> ext) outm
  _ <- forkIO $ onInputChange n
  _ <- forkIO $ onOutputChange n

  runWindow n

-- | We cheat a bit here and use withSystemTempFile to give us the
-- name and hope the suffixed version is free.
tmpFilename :: IO I.FilePath
tmpFilename = withSystemTempFile "tsuntsun" (const . return)

eventToFp :: Event -> FilePath
eventToFp (Added    f _) = f
eventToFp (Modified f _) = f
eventToFp (Removed  f _) = f

onInputChange :: Notifier Event -> IO ()
onInputChange n = forever $ do
  i <- eventToFp <$> takeMVar (_in n)
  img <- imageNewFromFile (encodeString i)
  _ <- tryPutMVar (_img n) img
  m <- readMVar (_ocrMode n)
  runTesseract (_tesseract n) (encodeString i) (_outputFile n) m

onOutputChange :: Notifier Event -> IO ()
onOutputChange n = forever $ do
  o <- eventToFp <$> takeMVar (_out n)
  c <- readFile (encodeString o)
  putMVar (_ocrResult n) c

runScrot :: I.FilePath -> IO ()
runScrot p = spawnProcess "scrot" ["-s", p] >>= void . waitForProcess

-- | Runs the tesseract process. Blocks to clean up the process or we
-- end up with zombies.
runTesseract :: I.FilePath -- ^ Path to tesseract binary to use.
             -> I.FilePath -- ^ Path to the image with text to OCR.
             -> I.FilePath -- ^ Base path to output the result to.
                           -- tesseract will append ‘.txt’ suffix to
                           -- this.
             -> ImageMode
             -> IO ()
runTesseract t i o im =
  let p = "-psm" : [show $ modes ! im]
  in spawnProcess t (p ++ ["-l", "jpn", i, o]) >>= void . waitForProcess

toFP :: String -> FilePath
toFP = fromText . pack

-- | Exit function used when given a directory.
directoryExit :: IO ()
directoryExit = do
  putStrLn "A filename must be given, got directory instead."
  exitWith $ ExitFailure 1

-- | Help text.
help :: String
help = "usage: tsuntsun image-watch-filename [tesseract-path]"

-- | Watches the file at specified path and puts into the MVar when
-- its state is updated. Creates the file if it does not yet exist.
watchFile :: FilePath -> MVar Event -> IO ()
watchFile f m = withManager $ \mgr -> do
  ensureFile f
  watchDir mgr (directory f) ((f ==) . eventToFp) (void . tryPutMVar m)
  forever $ threadDelay maxBound
