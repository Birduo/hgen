import System.Environment
import Text.ParserCombinators.Parsec
import Text.Parsec (endBy, eof)
import HtmlBindings

-- markdown ::= stmt* eof
markdown :: Parser [String]
markdown = do
    -- put list here to make sure that the parser will not stop at the first newline
    try expr `endBy` newline 
    <* eof

-- expr :: list | stmt
expr :: Parser String
expr = try list <|> try stmt

-- stmt ::= comment | header | list | paragraph
stmt :: Parser String
stmt = try comment
    <|> try header
    <|> try text
    <|> return ""

comment :: Parser String
comment = do
    string "// "
    contents <- many text
    return $ commentTag (concat contents)

header :: Parser String
header = do
    hashs <- many1 $ char '#'
    many1 space
    contents <- many text
    return $ hTag (length hashs) (concat contents)

list :: Parser String
list = do
    contents <- many1 $ try ulItem
    return $ ulTag $ concat contents
    <|> do
    contents <- many1 $ try olItem
    return $ olTag $ concat contents

ulItem :: Parser String
ulItem = do
    char '-'
    spaces
    contents <- text
    newline
    return $ "<li>" ++ contents ++ "</li>\n"

olItem :: Parser String
olItem = do
    many1 digit
    char '.'
    spaces
    contents <- text
    newline
    return $ "<li>" ++ contents ++ "</li>\n"


paragraph :: Parser String
paragraph = pTag <$> text

text :: Parser String
text = many1 $ noneOf "\n"

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    let outputFileName = head $ tail args
    file <- readFile fileName
    let result = parse markdown "Error !" file
    case result of
        Left err -> putStrLn $ "Parsing error: " ++ show err
        Right html -> writeFile outputFileName $ concat html
