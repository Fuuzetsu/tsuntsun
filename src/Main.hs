{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           Control.Applicative ((<*>))
import           Control.Concurrent
import qualified Control.Concurrent.STM as STM
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Combinators as CO
import           Data.Function ((&))
import           Data.Monoid ((<>), mempty)
import           Data.Text (Text)
import           Graphics.UI.Gtk
import           NaturalLanguageProcessing.TsunTsun.TH (litFile)
import qualified Options.Applicative as Opt
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.Exit (ExitCode(..))
import           System.FilePath (takeDirectory)
import qualified System.Process.Typed as P

runWindow :: TsunConfig -> IO ()
runWindow TsunConfig { _imagePath = iFile, _tesseractPath = tPath } = do
  _ <- initGUI
  -- Load the UI from glade file.
  let gladeContent = [litFile|res/mainwindow.glade|] :: String
  builder <- builderNew
  builderAddFromString builder gladeContent
  let getObj :: GObjectClass cls => (GObject -> cls) -> Text -> IO cls
      getObj = builderGetObject builder

  -- Register quit callbacks.
  window <- getObj castToWindow "mainWindow"
  _ <- window `on` keyPressEvent $ do
    tryEvent $ eventKeyName >>= \"q" -> liftIO mainQuit
  _ <- window `on` destroyEvent $ tryEvent (liftIO mainQuit)
  _ <- window `on` deleteEvent $ tryEvent (liftIO mainQuit)


  -- Maps radio buttons to integers accepted by tesseract's -psm flag.
  let imageModes :: [(Text, Int)]
      imageModes = [ ("verticalRadio", 5)
                   , ("columnRadio", 4)
                   , ("blockRadio", 6)
                   , ("circledCharacterRadio", 9)
                   , ("characterRadio", 10)
                   ]

  -- Technically we always expect one default choice but just let the
  -- code set it depending on what's active in the UI.
  ocrMode <- STM.newTVarIO Nothing
  forM_ imageModes $ \(r, m) -> getObj castToRadioButton r >>= \obj -> do
    let updateMode = toggleButtonGetActive obj >>= \case
          True -> STM.atomically $ STM.writeTVar ocrMode (Just m)
          False -> pure ()
    updateMode
    obj `on` toggled $ updateMode

  _ <- getObj castToButton "quitButton" >>= \b -> b `on` buttonActivated $ do
    mainQuit

  -- TVar that's fired when scrot has finished its work which should
  -- indicate new contents at iFile. As a bonus, if the file already
  -- exists at program start, we'll pass the STM guard straight away
  -- and load it.
  imageUpdated <- doesFileExist iFile >>= STM.newTVarIO
  _ <- getObj castToButton "scrotButton" >>= \b -> do
    buttonSetLabel b $ "scrot -s " <> iFile
    b `on` buttonActivated $ void . forkIO $ do
      createDirectoryIfMissing True $ takeDirectory iFile
      P.runProcess (P.proc "scrot" ["-s", iFile]) >>= \case
        ExitSuccess -> STM.atomically $ STM.writeTVar imageUpdated True
        _ -> return ()

  -- When a signal comes on imageUpdated after scrot terminates,
  -- display the new image and feed it into tesseract, updating text
  -- area when it's done.
  textBuffer <- getObj castToTextView "textArea" >>= textViewGetBuffer
  runImageUpdate <- postGUIAsync . flip imageSetFromFile iFile
                    <$> getObj castToImage "imageArea"
  void . forkIO . forever $ do
    -- Block until we're notified the image file has been updated.
    STM.atomically $ do
      STM.readTVar imageUpdated >>= STM.check
      STM.writeTVar imageUpdated False

    runImageUpdate
    psmFlags <- STM.readTVarIO ocrMode >>= \case
      Nothing -> pure mempty
      Just m' -> pure ["-psm", show m']
    let procConf = P.proc tPath (psmFlags <>  ["-l", "jpn", "stdin", "stdout"])
                 & P.setStdout P.createSource
                 & P.setStdin P.createSink
    P.withProcess procConf $ \p -> do
      R.runResourceT . C.runConduit $ do
        C.sourceFile iFile C.$$ P.getStdin p
        P.getStdout p
          C..| CO.decodeUtf8
          C..| CO.mapM_ (liftIO . postGUIAsync . textBufferSetText textBuffer)

  widgetShowAll window
  mainGUI

data TsunConfig = TsunConfig
  { _imagePath :: !FilePath
  , _tesseractPath :: !FilePath
  } deriving (Show, Eq, Ord)

main :: IO ()
main = Opt.execParser opts >>= runWindow
  where
    tc = TsunConfig
      <$> Opt.argument Opt.str
          ( Opt.metavar "SCROT_FILE"
         <> Opt.help "File to save to with scrot before it's processed with tesseract." )
      <*> Opt.argument Opt.str
          ( Opt.metavar "TESSERACT_BIN"
         <> Opt.help "Path to the tesseract binary."
         <> Opt.showDefaultWith id
         <> Opt.value "tesseract" )
    opts = Opt.info (tc Opt.<**> Opt.helper)
      ( Opt.fullDesc
     <> Opt.progDesc "Run a frontend to tesseract's Japanese OCR."
     <> Opt.header "Frontend to tesseract's Japanese OCR." )
