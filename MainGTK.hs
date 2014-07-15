module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.List.Zipper
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import System.Environment
import Translator

--goNext :: Zipper SentenceBlock -> IO Zipper SentenceBlock
--goNext z = do
    --let zipper = right z
    --textBufferSetText buffer1 setcursor zipper

--updateBuffers :: (TextBufferClass self) => self -> SentenceBlock

mvartest :: Zipper SentenceBlock -> IO (Zipper SentenceBlock)
mvartest = return . right

main = do
    [file,page] <- getArgs
    sentences <- fmap (makeZipper . getSentences) $ openPdfFile file (read page)
    zipper <- newMVar sentences
    initGUI

    builder <- builderNew
    builderAddFromFile builder "janela.glade"

    window <- builderGetObject builder castToWindow "window1"
    window `on` deleteEvent $ liftIO mainQuit >> return False
    origText <- builderGetObject builder castToTextView "textview1"
    buffer1 <- textViewGetBuffer origText
    nextButton <- builderGetObject builder castToButton "nextbutton"
    on nextButton buttonActivated $ do
        modifyMVar_ zipper (return . right)
    current <- withMVar zipper (return . cursor)
    putStrLn $ T.unpack $ originalText current
    textBufferSetText buffer1 $ T.unpack $ originalText current
    translatedText <- builderGetObject builder castToTextView "textview2"
    widgetShowAll window
    mainGUI
