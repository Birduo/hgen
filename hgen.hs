import System.Environment
import Text.ParserCombinators.Parsec
import Text.Parsec (endBy, eof)
import HtmlBindings

-- markdown ::= stmt* eof
markdown :: Parser [String]
markdown = endBy (try expr) newline <* eof

-- expr :: list | stmt
expr :: Parser String
expr = try list <|> try codeBlock <|> try stmt

-- list ::= ul | ol
list :: Parser String
list = do
    contents <- many1 $ try ulItem
    return $ ulTag $ concat contents
    <|> do
    contents <- many1 $ try olItem
    return $ olTag $ concat contents

-- ulItem ::= '-' text newline
ulItem :: Parser String
ulItem = do
    char '-'
    spaces
    contents <- text
    newline
    return $ liTag contents

-- olItem ::= digit+ '.' text newline
olItem :: Parser String
olItem = do
    many1 digit
    char '.'
    spaces
    contents <- text
    newline
    return $ liTag contents

-- codeBlock ::= "```" (text newline)* "```"
codeBlock :: Parser String
codeBlock = do
    string "```"
    contents <- manyTill anyChar (try $ string "```")
    return $ codeTag contents

-- stmt ::= comment | header | list | paragraph
stmt :: Parser String
stmt = try comment
    <|> try header
    <|> try text
    <|> return "" -- fail case

-- comment ::= "//" text
comment :: Parser String
comment = do
    string "// "
    contents <- many text
    return $ commentTag (concat contents)

-- header ::= '#'+ text
header :: Parser String
header = do
    hashs <- many1 $ char '#'
    many1 space
    contents <- many text
    return $ hTag (length hashs) (concat contents)

-- common text parsing
-- will have italics and bold later
text :: Parser String
text = many1 $ noneOf "\n"

-- convert input string to indent-formatted html
parseMarkdown :: String -> String
parseMarkdown input = case parse markdown "Error !" input of
    Left err -> "Parsing error: " ++ show err
    Right html -> baseHtml $ concat html

-- driving io: read args and output html accordingly
main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    let outputFileName = head $ tail args
    file <- readFile fileName

    let html = parseMarkdown file
    writeFile outputFileName html
