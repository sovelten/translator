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

statusInfo :: Int -> Int -> Int -> String
statusInfo page cur total = "page " ++ (show page) ++ ": " ++ (show cur) ++ "/" ++ (show total)

updateBar :: StatusbarClass self => self -> ContextId -> (Int, Int, Int) -> IO ()
updateBar bar id (page, cur, total) = do
    msgid <- statusbarPush bar id (statusInfo page cur total)
    return ()

main = do
    [file,page] <- getArgs
    --load page sentences
    sentences <- fmap (makeZipper . getSentences) $ openPdfFile file (read page)
    let total = length $ toList sentences
    --create zipper mvar
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

    statusBar <- builderGetObject builder castToStatusbar "statusbar1"
    id <- statusbarGetContextId statusBar "Line"
    nextButton <- builderGetObject builder castToButton "nextbutton"
    prevButton <- builderGetObject builder castToButton "previousbutton"
    exitButton <- builderGetObject builder castToButton "exitbutton"

    on nextButton buttonActivated $ do
        text2 <- textBufferGetValue buffer2
        modifyMVar_ zipper (return . (updateZipper text2))
        new <- modifyMVar zipper (return . (shiftZipper right))
        updateBar statusBar id ((read page), blockNum new, total)
        textBufferSetText buffer1 $ T.unpack $ originalText new
        textBufferSetText buffer2 $ T.unpack $ translatedText new

    on prevButton buttonActivated $ do
        text2 <- textBufferGetValue buffer2
        modifyMVar_ zipper (return . (updateZipper text2))
        new <- modifyMVar zipper (return . (shiftZipper left))
        updateBar statusBar id ((read page), blockNum new, total)
        textBufferSetText buffer1 $ T.unpack $ originalText new
        textBufferSetText buffer2 $ T.unpack $ translatedText new

    on exitButton buttonActivated
        $ putMVar exit ExitSuccess

    msgid <- statusbarPush statusBar id (statusInfo (read page) 1 total)

    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    current <- withMVar zipper (return . cursor)
    textBufferSetText buffer1 $ T.unpack $ originalText current
    textBufferSetText buffer2 $ T.unpack $ translatedText current

    widgetShowAll window
    mainGUI
