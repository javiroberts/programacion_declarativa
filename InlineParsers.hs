module InlineParsers where

import Markdown

parseInline :: ReadP ParText
parseInline = many1 (hardbreak +++ codeInline +++ inlineAsterisk +++ inlineUnderscore +++ parseMText [])

parseSpansContext :: [String] -> ReadP ParText
parseSpansContext c = many1 ((hardbreak +++ codeInline +++ inlineAsterisk +++ inlineUnderscore) <++ parseMText c)

hardbreak :: ReadP MText
hardbreak = optional (whitespace) >> count 2 whitespace >> satisfy (=='\n') >> return Hardbreak

codeInline :: ReadP MText
codeInline = do
    sep <- string "``" <++ string "`"
    content <- markdownText [sep]
    return $ Code content 

inlineAsterisk :: ReadP MText
inlineAsterisk = strongAsterisk <++ emAsterisk

inlineUnderscore :: ReadP MText
inlineUnderscore = strongUnderscore <++ emUnderscore

emAsterisk :: ReadP MText
emAsterisk = do
    string "*"
    content <- markdownText ["*"]
    return $ Emph content

emUnderscore :: ReadP MText
emUnderscore = do
    string "_"
    content <- markdownText ["_"]
    return $ Emph content

strongAsterisk :: ReadP MText
strongAsterisk = do
    string "**"
    content <- markdownText ["**"]
    return $ Strong content

strongUnderscore :: ReadP MText
strongUnderscore = do
    string "__"
    content <- markdownText ["__"]
    return $ Strong content
