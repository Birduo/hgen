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
    return $ codeBlockTag contents

-- stmt ::= comment | header | list | paragraph
stmt :: Parser String
stmt = try comment
    <|> try header
    <|> try inline
    <|> return "<br>\n" -- if no match, return <br> and consume a newline

-- comment ::= "//" text
comment :: Parser String
comment = do
    string "// "
    contents <- manyTill anyChar (try $ lookAhead newline)
    return $ commentTag contents

-- header ::= '#'+ text
header :: Parser String
header = do
    hashs <- many1 $ char '#'
    many1 space
    contents <- many1 text
    return $ hTag (length hashs) (concat contents)

inline :: Parser String
inline = try bold
    <|> try italic
    <|> try link
    <|> text

bold :: Parser String
bold = do
    string "**"
    contents <- manyTill text (try $ string "**")
    return $ boldTag $ concat contents

italic :: Parser String
italic = do
    string "*"
    contents <- manyTill text (try $ string "*")
    return $ italicTag $ concat contents

link :: Parser String
link = do
    string "["
    contents <- manyTill anyChar (try $ lookAhead $ string "](")
    string "]("
    url <- manyTill anyChar (try $ string ")")
    return $ linkTag contents url

-- common text parsing
text :: Parser String
text = do
    first <- noneOf "*[_\n"
    rest <- manyTill (noneOf "*[_\n") (try $ lookAhead $ oneOf "*[_\n")
    return (first:rest)

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

    let html = parseMarkdown $ file ++ "\n" -- append w/ newline just incase
    writeFile outputFileName html
