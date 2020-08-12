module Main where

import System.Environment
import Web.Browser (openBrowser)
import Markdown

processedMD :: [([Paragraph], String)] -> String
processedMD p = "<html><body>" ++ (concat $ map show paragraphs) ++ "<body/><html/>"
    where paragraphs = fst $ last p

main :: IO ()
main = do
    args <- getArgs
    let fd = head args
    markdown <- readFile fd
    let processed = processedMD $ parseMarkdown markdown
    writeFile "/tmp/haskellMD.html" processed
    openBrowser "/tmp/haskellMD.html" >>= print
