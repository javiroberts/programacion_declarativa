module Markdown where

import Text.ParserCombinators.ReadP
import Data.Char (isPrint,isSpace, isDigit)
import Control.Monad
import InlineParsers
import BlockParsers

data Paragraph = Normal ParText
               | Pre String
               | Heading Int ParText
               | Quote [Paragraph]
               | UList ParText
               | OList ParText

instance Show Paragraph where
    show (Normal content) = "<p>" ++ (concatMap show content) ++ "</p>"
    show (Pre content) = "<code><pre>" ++ content ++ "</pre></code>"
    show (Heading level content) = "<h" ++ (show level) ++ ">" ++ (concatMap show content) ++ "</h" ++ (show level) ++ ">"
    show (Quote contents) = "<blockquote>" ++ (concatMap show contents) ++ "</blockquote>"
    show (UList contents) = "<ul>" ++ (listedList contents) ++ "</ul>"
    show (OList contents) = "<ol>" ++ (listedList contents) ++ "</ol>"

listedList :: ParText -> String
listedList contents = concatMap (wrapLi . show) contents
    where wrapLi s = "<li>" ++ s ++ "</li>"

type ParText = [MText]

data MText = MText String
           | Emph String
           | Strong String
           | Code String
           | Hardbreak

instance Show MText where
    show (MText contents) = contents
    show (Emph contents) = "<em>" ++ contents ++ "</em>"
    show (Strong contents) = "<strong>" ++ contents ++ "</strong>"
    show (Code contents) = "<code>" ++ (encodeStr contents) ++ "</code>"
    show (Hardbreak) = "<br />"

encodeStr :: String -> String
encodeStr (c:cs)
    | c == '&'  = "&amp;" ++ (encodeStr cs)
    | c == '<'  = "&lt;"  ++ (encodeStr cs)
    | c == '>'  = "&gt;"  ++ (encodeStr cs)
    | otherwise = c:(encodeStr cs)
encodeStr [] = []

parseMarkdown :: ReadS [Paragraph]
parseMarkdown = readP_to_S blockParsers

whitespace :: ReadP Char
whitespace = satisfy ws
    where ws c = c `elem` [' ', '\t']

newline :: ReadP String
newline = string "\r\n" <++ string "\n" <++ string "\r"

oneOf :: [String] -> ReadP ()
oneOf xs = do
    rest <- look
    let results = scanEach xs rest
    guard $ True `elem` results
    return ()
    where scanEach strs rest = map (scan rest) strs
          scan (r:rs) (s:ss) | s == r = scan rs ss
          scan _ [] = True
          scan _  _ = False

oneOfConsume :: [String] -> ReadP String
oneOfConsume xs = foldr1 (<++) $ map (string) xs


specials :: [String] -- special characters to be considered
specials = ["*", "_", "`", "\n", ">", "#", "+", "1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "9.", "0."]

markdownText :: [String] -> ReadP String
markdownText [] = do
    consumed <- manyTill get (oneOf specials) <++ munch1 notInSpecials
    guard (consumed /= "")
    return $ consumed
    where notInSpecials c = not $ [c] `elem` specials
markdownText end = do
    consumed <- manyTill get (oneOfConsume end) 
    guard (consumed /= "")
    return $ consumed

parseMText :: [String] -> ReadP MText
parseMText end = do
    consumed <- markdownText end
    guard (consumed /= "")
    return $ MText consumed
