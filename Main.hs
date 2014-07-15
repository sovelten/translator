import qualified Data.Text as T
import System.Environment
import Translator

translateOne :: Sentence -> IO Sentence
translateOne s = do
    putStrLn $ writeSentenceWithNum s
    str <- getLine
    return $ Sentence (sentenceNum s) (T.pack str)

main = do
    [file,page] <- getArgs
    content <- openPdfFile file (read page)
    let sentences = getSentences $ content
    translated <- sequence $ map translateOne (take 5 sentences)
    putStrLn $ concatMap (++ " ") $ map writeSentence translated
