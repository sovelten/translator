module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.List.Zipper
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import System.Environment
import System.Exit
import Translator

textBufferGetValue :: TextBufferClass self => self -> IO String
textBufferGetValue buf = do
    start <- textBufferGetStartIter buf
    end <- textBufferGetEndIter buf
    value <- textBufferGetText buf start end True
    return value

statusInfo :: Int -> Int -> Int
statusInfo page cur total = "page " ++ (show page) ++ ": " ++ (show cur) ++ "/" ++ (show total)

main = do
    [file,page] <- getArgs
    --load page sentences
    sentences <- fmap (makeZipper . getSentences) $ openPdfFile file (read page)
    --create zipper
    zipper <- newMVar sentences
    exit <- newEmptyMVar
    initGUI

    builder <- builderNew
    builderAddFromFile builder "janela.glade"

    window <- builderGetObject builder castToWindow "window1"
    window `on` deleteEvent $ liftIO mainQuit >> return False
    origText <- builderGetObject builder castToTextView "textview1"
    buffer1 <- textViewGetBuffer origText
    translText <- builderGetObject builder castToTextView "textview2"
    buffer2 <- textViewGetBuffer translText

    nextButton <- builderGetObject builder castToButton "nextbutton"
    on nextButton buttonActivated $ do
        text2 <- textBufferGetValue buffer2
        modifyMVar_ zipper (return . (updateZipper text2))
        new <- modifyMVar zipper (return . (shiftZipper right))
        textBufferSetText buffer1 $ T.unpack $ originalText new
        textBufferSetText buffer2 $ T.unpack $ translatedText new

    prevButton <- builderGetObject builder castToButton "previousbutton"
    on prevButton buttonActivated $ do
        text2 <- textBufferGetValue buffer2
        modifyMVar_ zipper (return . (updateZipper text2))
        new <- modifyMVar zipper (return . (shiftZipper left))
        textBufferSetText buffer1 $ T.unpack $ originalText new
        textBufferSetText buffer2 $ T.unpack $ translatedText new

    exitButton <- builderGetObject builder castToButton "exitbutton"
    on exitButton buttonActivated
        $ putMVar exit ExitSuccess

    statusBar <- builderGetObject builder castToStatusbar "statusbar1"
    id <- statusbarGetContextId statusBar "Line"
    msgid <- statusbarPush statusBar id (statusInfo (read page) 2 40)

    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    current <- withMVar zipper (return . cursor)
    textBufferSetText buffer1 $ T.unpack $ originalText current
    textBufferSetText buffer2 $ T.unpack $ translatedText current

    widgetShowAll window
    mainGUI
