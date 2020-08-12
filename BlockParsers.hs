module BlockParsers where

import Markdown

blockParsers :: ReadP [Paragraph]
blockParsers = blockQuote +++ (many1 (olist +++ ulist +++ preBlock +++ atxHeader +++ setext1Header +++ setext2Header <++ normalParagraph))

normalParagraph :: ReadP Paragraph
normalParagraph = do
    content <- parseInline
    optional (string "\n\n")
    return $ Normal content

indentedLineSpaces :: ReadP String
indentedLineSpaces = do
    string "   "
    content <- manyTill get (char '\n')
    return content

indentedLineTab :: ReadP String
indentedLineTab = do
    char '\t'
    content <- manyTill get (char '\n')
    return content

indentedLine :: ReadP String
indentedLine = indentedLineSpaces +++ indentedLineTab

preBlock :: ReadP Paragraph
preBlock = do
    lines <- many1 indentedLine
    return $ Pre $ concat lines

atxHeader :: ReadP Paragraph
atxHeader = do
    hashes <- munch1 (=='#')
    skipSpaces
    content <- parseInline
    skipSpaces
    optional (munch1 (=='#'))
    optional (char '\n')
    guard $ (length hashes <= 6)
    return $ Heading (length hashes) content

setext1Header :: ReadP Paragraph
setext1Header = do
    content <- parseInline 
    char '\n'
    munch1 (=='=')
    optional (char '\n')
    return $ Heading 1 content

setext2Header :: ReadP Paragraph
setext2Header = do
    content <- parseInline
    char '\n'
    munch1 (=='-')
    optional (char '\n')
    return $ Heading 2 content

blockQuoteLine :: ReadP Paragraph
blockQuoteLine = do
    char '>'
    content <- blockParsers
    return $ Quote content

blockQuote :: ReadP [Paragraph]
blockQuote = many1 blockQuoteLine

ulistElem :: ReadP ParText
ulistElem = do
    char '+'
    skipSpaces
    content <- parseSpansContext ["\n", "+"]
    return $ content

ulist :: ReadP Paragraph
ulist = do
    elems <- many1 ulistElem
    return $ UList $ concat elems 

olistElem :: ReadP ParText
olistElem = do
    munch1 isDigit
    char '.'
    skipSpaces
    content <- parseSpansContext ["\n", "1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "9.","0."]
    return $ content

olist :: ReadP Paragraph
olist = do
    elems <- many1 olistElem
    return $ OList $ concat elems
