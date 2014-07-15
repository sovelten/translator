{-# LANGUAGE OverloadedStrings #-}
module Translator where

import Data.Either.Combinators (fromRight')
import qualified Data.Text as T
import Pdf.Toolbox.Document
import System.IO

data Sentence = Sentence {sentenceNum :: Int, sentenceText :: T.Text }

data SentenceBlock = SentenceBlock {originalText :: T.Text, translatedText :: T.Text}

data TranslationState = {previous :: [SentenceBlock], current :: SentenceBlock, next :: [SentenceBlock]}

writeSentenceWithNum :: Sentence -> String
writeSentenceWithNum s = number ++ ":" ++ sentence
    where sentence = T.unpack $ sentenceText s
          number = show $ sentenceNum s

writeSentence :: Sentence -> String
writeSentence s = T.unpack $ sentenceText s


toSentence :: (Int,T.Text) -> Sentence
toSentence (n,t) = Sentence n t

getSentences :: T.Text -> [Sentence]
getSentences str = map toSentence $ zip [1..len] list
    where list = map (flip T.snoc '.') $ init $ T.splitOn "." str
          len = length list

getPageText :: MonadIO m => Int -> Pdf m T.Text
getPageText n = do
    pdf <- document
    catalog <- documentCatalog pdf
    rootNode <- catalogPageNode catalog
    --count <- pageNodeNKids rootNode
    --liftIO $ print count
    page <- pageNodePageByNum rootNode n
    pageExtractText page


openPdfFile :: FilePath -> Int -> IO T.Text
openPdfFile file page = fmap fromRight' $ withBinaryFile file ReadMode $ \handle ->
    runPdfWithHandle handle knownFilters $ getPageText page


