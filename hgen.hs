import System.Environment
import Text.ParserCombinators.Parsec
import Text.Parsec (endBy, eof)
import HtmlBindings
import Debug.Trace (trace)

-- markdown ::= stmt* eof
markdown :: Parser [String]
-- markdown = sepEndBy (try expr) newline <* eof
markdown = manyTill expr eof

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
    contents <- inline
    newline
    return $ liTag contents

-- olItem ::= digit+ '.' text newline
olItem :: Parser String
olItem = do
    many1 digit
    char '.'
    spaces
    contents <- inline
    newline
    return $ liTag contents

-- codeBlock ::= "```" (text newline)* "```"
codeBlock :: Parser String
codeBlock = do
    string "```"
    contents <- manyTill anyChar (try $ string "```")
    return $ codeBlockTag contents

-- stmt ::= comment | header | inline | newline
stmt :: Parser String
stmt = try comment 
    <|> try header
    <|> try inline
    <|> do 
        newline
        return "<br>\n" -- if no match, return <br> and consume a newline

-- comment ::= "//" text
comment :: Parser String
comment = do
    string "// "
    contents <- manyTill anyChar newline
    return $ commentTag contents

-- header ::= '#'+ text
header :: Parser String
header = do
    hashs <- many1 $ char '#'
    many1 space
    contents <- many1 text
    newline
    return $ hTag (length hashs) (concat contents)

inline :: Parser String
inline = try bold
    <|> try italic
    <|> try link
    <|> try inlineCode
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

-- inlineCode ::= "`" text "`"
inlineCode :: Parser String
inlineCode = do
    char '`'
    contents <- manyTill anyChar (try $ char '`')
    return $ inlineCodeTag contents

-- common text parsing
text :: Parser String
text = many1 (noneOf "*[_`\n")

-- convert input string to indent-formatted html
parseMarkdown :: String -> String
parseMarkdown input = case parse markdown "Error !" input of
    Left err -> "Parsing error: " ++ show err
    Right html -> concat html

-- driving io: read args and output html accordingly
-- usage: hgen <input.md> <output.html> [header.html]
main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    let outputFileName = args !! 1
    file <- readFile fileName

    let html = parseMarkdown $ file ++ "\n" -- append w/ newline just incase

    -- Check if a third argument is provided
    if length args > 2 then do
        let headerFileName = args !! 2
        header <- readFile headerFileName
        writeFile outputFileName $ baseHtmlFile header html
    else
        writeFile outputFileName $ baseHtml html
