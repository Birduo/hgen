import System.Environment
import Data.Char
import Parsing


-- stmt ::= header | comment | inline | bulleted | numbered | code | text
stmt = do header
    <|> do comment
    <|> do inline
    <|> do bulleted
    <|> do numbered
    <|> do code
    <|> text

createHeaderTag :: String -> String -> String
createHeaderTag x y = "<h" ++ x ++ ">" ++ y ++ "</h" ++ x ++ ">"

-- header ::= "#"* " " text
header = do
    x <- many (char '#')
    char ' '
    createHeaderTag (show (length x)) <$> text

-- comment ::= "//" text 
comment = do
    string "//"
    x <- text
    return ("<!-- " ++ x ++ " -->")

-- inline ::= "\`" text "\`" 
inline = do
    char '`'
    x <- text
    char '`'
    return x

-- bulleted ::= "*" text | "+" text | "-" text
bulleted = do
    char '*'
    char ' '
    text
    <|> do
    char '+'
    char ' '
    text
    <|> do
    char '-'
    char ' '
    text

-- numbered ::= [0-9]+ "." text
numbered = do
    x <- many digit
    char '.'
    y <- text
    return (x ++ "." ++ y)

-- code ::= "\`\`\`" text "\`\`\`
code = do
    string "```"
    x <- text
    string "```"
    return x

-- text ::= char* "\n"*
text :: Parser String
text = do many (sat (/= '\n'))

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
