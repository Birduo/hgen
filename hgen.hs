import System.Environment
import Data.Char
import Parsing


-- stmt ::= header | comment | inline | bulleted | numbered | code | text
stmt = header
    <|> comment
    <|> bulletedList
    <|> numberedList
    <|> text

createHeaderTag :: String -> String -> String
createHeaderTag x y = "<h" ++ x ++ ">" ++ y ++ "</h" ++ x ++ ">\n"

-- header ::= "#"* " " text
header = do
    x <- many (char '#')
    char ' '
    createHeaderTag (show (length x)) <$> text

-- comment ::= "//" text 
comment = do
    string "//"
    x <- text
    return ("<!-- " ++ x ++ " -->\n")

-- wrap the text in a <ul> tag
-- bulletedList ::= bulleted*
bulletedList :: Parser String
bulletedList = do
    items <- some bulleted
    return ("<ul>\n" ++ concat items ++ "</ul>")

-- bulleted ::= "*" text | "+" text | "-" text
bulleted = do
    char '*'
    char ' '
    y <- many (sat (/= '\n'))
    return ("<li>" ++ y ++ "</li>\n")
    <|> do
    char '+'
    char ' '
    y <- many (sat (/= '\n'))
    return ("<li>" ++ y ++ "</li>\n")
    <|> do
    char '-'
    char ' '
    y <- many (sat (/= '\n'))
    return ("<li>" ++ y ++ "</li>\n")

-- numberedList ::= numbered*
numberedList :: Parser String
numberedList = do
    items <- some numbered
    return ("<ol>\n" ++ concat items ++ "</ol>\n")

-- numbered ::= [0-9]+ "." text
numbered :: Parser String
numbered = do
    x <- many digit
    char '.'
    y <- text
    return ("<li>" ++ y ++ "</li>\n")

-- todo: add inline formatting for code, bold, and italics
-- text ::= char* "\n"*
text :: Parser String
text = many (sat (/= '\n'))

baseHtml :: String -> String
baseHtml text = "<html>\n<head>\n</head>\n<body>\n" ++ text ++ "\n</body>\n</html>"

parseString :: String -> [String]
parseString str =
    case parse (sepBy stmt (char '\n')) str of
        [] -> error "Parse error: empty input"
        ((parsed, _):_) -> parsed


parseFile :: FilePath -> IO [String]
parseFile filename = do
    str <- readFile filename
    return (parseString str)


-- take an input .md file and return an output .html file
main :: IO ()
main = do
    args <- getArgs
    case args of
        [inFile, outFile] -> do
            parsed <- parseFile inFile
            let html = baseHtml (concat parsed)
            putStrLn html
            writeFile outFile html
        _ -> putStrLn "Usage: hgen <markdown> <html>"
