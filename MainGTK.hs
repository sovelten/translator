module Main where

import Control.Monad.IO.Class
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import System.Environment
import Translator

main = do
    [file,page] <- getArgs
    initGUI

    builder <- builderNew
    builderAddFromFile builder "janela.glade"

    window <- builderGetObject builder castToWindow "window1"
    window `on` deleteEvent $ liftIO mainQuit >> return False
    nextButton <- builderGetObject builder castToButton "nextbutton"
    nextButton `on` buttonActivated $ putStrLn "Hello!"
    originalText <- builderGetObject builder castToTextView "textview1"
    buffer1 <- textViewGetBuffer originalText
    sentences <- fmap getSentences $ openPdfFile file (read page)
    textBufferSetText buffer1 $ writeSentence (head sentences)
    translatedText <- builderGetObject builder castToTextView "textview2"
    widgetShowAll window
    mainGUI
