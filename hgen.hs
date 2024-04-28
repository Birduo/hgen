import System.Environment
import Text.ParserCombinators.Parsec
import Text.Parsec (endBy, eof)
import HtmlBindings
import Debug.Trace (trace)
import Data.List (isSuffixOf)

-- markdown ::= stmt* eof
markdown :: String -> Parser [String]
markdown plat = manyTill (expr plat) eof

-- expr :: list | stmt
expr :: String -> Parser String
expr plat = try (list plat) <|> try (codeBlock plat) <|> try (stmt plat)

-- list ::= ul | ol
list :: String -> Parser String
list plat = do
    contents <- many1 $ try (ulItem plat)
    return $ ulTag plat $ concat contents
    <|> do
    contents <- many1 $ try (olItem plat)
    return $ olTag plat $ concat contents

-- ulItem ::= '-' text newline
ulItem :: String -> Parser String
ulItem plat = do
    char '-'
    spaces
    contents <- inline plat
    newline
    return $ liTag plat contents

-- olItem ::= digit+ '.' text newline
olItem :: String -> Parser String
olItem plat = do
    many1 digit
    char '.'
    spaces
    contents <- inline plat
    newline
    return $ liTag plat contents

-- codeBlock ::= "```" (text newline)* "```"
codeBlock :: String -> Parser String
codeBlock plat = do
    string "```"
    language <- try $ manyTill anyChar newline <|> return ""
    contents <- manyTill anyChar (try $ string "```")
    return $ case language of
        "js" -> scriptBlockTag plat contents
        "javascript" -> highlightCodeTag plat "javascript" contents ++ scriptBlockTag plat contents
        "" -> codeBlockTag plat contents
        x -> highlightCodeTag plat x contents

-- stmt ::= comment | header | inline | newline
stmt :: String -> Parser String
stmt plat = try (comment plat) 
    <|> try (header plat)
    <|> try (emptyDiv plat)
    <|> try (inline plat)
    <|> do 
        newline
        return $ breakline plat -- if no match, return and consume a newline

-- comment ::= "//" text
comment :: String -> Parser String
comment plat = do
    string "// "
    contents <- manyTill anyChar newline
    return $ commentTag plat contents

-- header ::= '#'+ text
header :: String -> Parser String
header plat = do
    hashs <- many1 $ char '#'
    many1 space
    contents <- many1 text
    newline
    return $ hTag plat (length hashs) (concat contents)

-- emptyDiv ::= "::" id
emptyDiv :: String -> Parser String
emptyDiv plat = do
    string "::"
    id <- many1 (noneOf " \n")
    return $ emptyDivTag plat id


inline :: String -> Parser String
inline plat = try (bold plat)
    <|> try (italic plat)
    <|> try (img plat)
    <|> try (link plat)
    <|> try (inlineCode plat)
    <|> text

bold :: String -> Parser String
bold plat = do
    string "**"
    contents <- manyTill text (try $ string "**")
    return $ boldTag plat $ concat contents

italic :: String -> Parser String
italic plat = do
    string "*"
    contents <- manyTill text (try $ string "*")
    return $ italicTag plat $ concat contents

img :: String -> Parser String
img plat = do
    string "!["
    alt <- manyTill anyChar (try $ string "](")
    url <- manyTill anyChar (try $ string ")")
    return $ imgTag plat url alt

-- linkFileType checks the file type of a link (css, js, py, html)
fileType :: String -> String
fileType = reverse. takeWhile (/= '.'). reverse

link :: String -> Parser String
link plat = do
    string "["
    contents <- manyTill anyChar (try $ lookAhead $ string "](")
    string "]("
    url <- manyTill anyChar (try $ string ")")
    return $ case fileType url of
        "css" -> cssTag plat url
        "js" -> linkTag plat contents url ++ scriptLinkTag plat url
        _ -> linkTag plat contents url

-- inlineCode ::= "`" text "`"
inlineCode :: String -> Parser String
inlineCode plat = do
    char '`'
    contents <- manyTill anyChar (try $ char '`')
    return $ inlineCodeTag plat contents

-- common text parsing
text :: Parser String
text = many1 (noneOf "*[_`\n")

-- convert input string to indent-formatted html
parseMarkdown :: String -> String -> String
parseMarkdown input plat = case parse (markdown plat) "Error!" input of
    Left err -> "Parsing error: " ++ show err
    Right html -> concat html

-- driving io: read args and output html accordingly
-- usage: hgen <input.md> <output.html> [header.html]
main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    let outputFileName = args!! 1
    file <- readFile fileName

    let plat = "html" -- or any other platform you want to support
    let html = parseMarkdown (file ++ "\n") plat

    -- Check if a third argument is provided
    if length args > 2 then do
        let headerFileName = args!! 2
        header <- readFile headerFileName
        writeFile outputFileName $ baseHtmlFile header html
    else
        writeFile outputFileName $ baseHtml html
