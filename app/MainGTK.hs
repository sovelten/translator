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

changeCursor :: (Zipper a -> Zipper a) -> Zipper a -> IO (Zipper a, a)
changeCursor f zip = return (new, cursor new)
    where new = f zip

updateSentenceBlock :: String -> SentenceBlock -> SentenceBlock
updateSentenceBlock text (SentenceBlock n orig _) = SentenceBlock n orig text

textViewGetValue :: TextViewClass self => self -> IO String
textViewGetValue tv = do
    buf <- textViewGetBuffer tv
    start <- textBufferGetStartIter buf
    end <- textBufferGetEndIter buf
    value <- textBufferGetText buf start end True
    return value

main = do
    [file,page] <- getArgs
    --load page sentences
    sentences <- fmap (makeZipper . getSentences) $ openPdfFile file (read page)
    --create zipper
    zipper <- newMVar sentences
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
        block <- modifyMVar zipper (changeCursor right)
        textBufferSetText buffer1 $ T.unpack $ originalText block
        textBufferSetText buffer2 $ T.unpack $ translatedText block

    prevButton <- builderGetObject builder castToButton "previousbutton"
    on prevButton buttonActivated $ do
        text2 <- textBufferGetValue buffer2
        --let currentBlock = updateSentenceBlock 
        block <- modifyMVar insert text zipper
        block <- modifyMVar zipper (changeCursor left)
        textBufferSetText buffer1 $ T.unpack $ originalText block
        textBufferSetText buffer2 $ T.unpack $ translatedText block

    current <- withMVar zipper (return . cursor)
    textBufferSetText buffer1 $ T.unpack $ originalText current

    widgetShowAll window
    mainGUI
