{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import           Control.Applicative ((<$>))
import           Control.Concurrent
import qualified Control.Concurrent.STM as STM
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Map
import           Data.Maybe (fromMaybe)
import           Data.Text (pack, Text)
import           Data.Text.IO (readFile)
import qualified Filesystem.Path.CurrentOS as FP
import           Graphics.UI.Gtk
import           NaturalLanguageProcessing.TsunTsun.TH (litFile)
import           Prelude hiding (readFile)
import           System.Directory (doesDirectoryExist, createDirectoryIfMissing, doesFileExist)
import           System.Environment (getArgs)
import           System.Exit (exitWith, ExitCode(..))
import           System.FSNotify (Event(..), watchDir, withManager)
import           System.IO.Temp (withSystemTempFile)
import qualified System.Process.Typed as P

data Notifier = Notifier
 { _in :: !(STM.TVar (Maybe Event))
 , _out :: !(STM.TVar (Maybe Event))
 , _tesseract :: FilePath
 , _outputFile :: FilePath
 , _inputFile :: FilePath
 , _ocrResult :: !(STM.TVar (Maybe Text))
 , _img :: !(STM.TVar (Maybe Image))
 , _ocrMode :: !(STM.TVar ImageMode)
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

runWindow :: Notifier -> IO ()
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

  _ <- onEv buttonActivated mainQuit quitButton
  _ <- onEv buttonActivated (widgetShow aboutDialog) aboutButton
  _ <- onEv buttonActivated (void . forkIO . runScrot $ _inputFile n) scrotButton

  buttonSetLabel scrotButton ("scrot -s " ++ _inputFile n)

  mapM_ ($ window) [ onEv keyPressEvent keyPressed
                   , onEv destroyEvent . tryEvent $ liftIO mainQuit
                   , onEv deleteEvent . tryEvent $ liftIO mainQuit
                   ]

  -- Update text
  _ <- forkIO . forever $ do
    t <- takeFull $ _ocrResult n
    postGUIAsync $ do
      bn <- textViewGetBuffer textArea
      textBufferSetText bn t

  -- Update image
  _ <- forkIO . forever $ do
    image' <- takeFull (_img n) >>= imageGetPixbuf
    postGUIAsync $ imageSetFromPixbuf image image'

  widgetShowAll window
  mainGUI

-- | Wait until a 'STM.TVar' contains a value.
takeFull :: STM.TVar (Maybe a) -> IO a
takeFull v = STM.atomically $ STM.readTVar v >>= \case
  Nothing -> STM.retry
  Just a -> STM.writeTVar v Nothing >> pure a

radioToggled :: Notifier -> RadioButton -> ImageMode -> IO ()
radioToggled Notifier { _ocrMode = o } r m = whenM (toggleButtonGetActive r) $ do
  STM.atomically $ STM.writeTVar o m

onEv :: Signal object s -> s -> object -> IO (ConnectId object)
onEv v r x = x `on` v $ r

gladeXml :: String
gladeXml = [litFile|res/mainwindow.glade|]

keyPressed :: EventM EKey Bool
keyPressed = tryEvent $ do
  "q" <- eventKeyName
  liftIO mainQuit

ensureFile :: FP.FilePath -> IO ()
ensureFile f = do
  let d = FP.dirname f
      f' = FP.encodeString f
  createDirectoryIfMissing True (FP.encodeString d)
  unlessM (doesFileExist f') $ writeFile f' ""

main :: IO ()
main = getArgs >>= \case
  [i, t] -> run i (Just t)
  [i] -> run i Nothing
  _ -> putStrLn help >> exitWith (ExitFailure 1)

-- | Extension added by tesseract.
ext :: Text
ext = pack "txt"

run :: FilePath -> Maybe FilePath -> IO ()
run i t = do
  whenM (doesDirectoryExist i) $ directoryExit

  (inm, outm, ocrr, imgm) <- liftM4 (,,,) (STM.newTVarIO Nothing) (STM.newTVarIO Nothing)
                                          (STM.newTVarIO Nothing) (STM.newTVarIO Nothing)
  ocrm <- STM.newTVarIO Vertical

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
  _ <- forkIO $ watchFile (toFP o FP.<.> ext) outm
  _ <- forkIO $ onInputChange n
  _ <- forkIO $ onOutputChange n

  runWindow n

-- | We cheat a bit here and use withSystemTempFile to give us the
-- name and hope the suffixed version is free.
tmpFilename :: IO FilePath
tmpFilename = withSystemTempFile "tsuntsun" (const . return)

eventToFp :: Event -> FilePath
eventToFp (Added    f _) = f
eventToFp (Modified f _) = f
eventToFp (Removed  f _) = f

onInputChange :: Notifier -> IO ()
onInputChange n = forever $ do
  i <- eventToFp <$> takeFull (_in n)
  !img <- imageNewFromFile i
  STM.atomically $ STM.writeTVar (_img n) (Just img)
  m <- STM.atomically $ STM.readTVar (_ocrMode n)
  runTesseract (_tesseract n) i (_outputFile n) m

onOutputChange :: Notifier -> IO ()
onOutputChange n = forever $ do
  o <- eventToFp <$> takeFull (_out n)
  !c <- readFile o
  STM.atomically $ STM.writeTVar (_ocrResult n) (Just c)

runScrot :: FilePath -> IO ()
runScrot p = P.runProcess_ $ P.proc "scrot" ["-s", p]

-- | Runs the tesseract process. Blocks to clean up the process or we
-- end up with zombies.
runTesseract :: FilePath -- ^ Path to tesseract binary to use.
             -> FilePath -- ^ Path to the image with text to OCR.
             -> FilePath -- ^ Base path to output the result to.
                           -- tesseract will append ‘.txt’ suffix to
                           -- this.
             -> ImageMode
             -> IO ()
runTesseract t i o im =
  P.runProcess_ . P.proc t $ "-psm" : show (modes ! im) : ["-l", "jpn", i, o]

toFP :: String -> FP.FilePath
toFP = FP.fromText . pack

-- | Exit function used when given a directory.
directoryExit :: IO ()
directoryExit = do
  putStrLn "A filename must be given, got directory instead."
  exitWith $ ExitFailure 1

-- | Help text.
help :: String
help = "usage: tsuntsun image-watch-filename [tesseract-path]"

-- | Watches the file at specified path and puts into the 'STM.TVar' when
-- its state is updated. Creates the file if it does not yet exist.
watchFile :: FP.FilePath -> STM.TVar (Maybe Event) -> IO ()
watchFile f m = withManager $ \mgr -> do
  ensureFile f
  _ <- watchDir mgr (FP.encodeString $ FP.directory f) ((f ==) . FP.decodeString . eventToFp)
                    (STM.atomically . STM.writeTVar m . Just)
  forever $ threadDelay maxBound
