import System.Environment
import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)


-- stmt ::= header | comment | text
stmt :: Parser String
stmt = try header
    <|> try comment
    <|> text

createHeaderTag :: String -> String -> String
createHeaderTag x y = "<h" ++ x ++ ">" ++ y ++ "</h" ++ x ++ ">\n"

-- header ::= "#"* " " text
header :: Parser String
header = do
    x <- many (char '#')
    _ <- char ' '
    createHeaderTag (show (length x)) <$> text

-- comment ::= "//" text 
comment :: Parser String
comment = do
    _ <- string "//"
    x <- text
    return ("<!-- " ++ x ++ " -->\n")

bold :: Parser String
bold = do
    _ <- try (string "**") <|> try (string "__")
    content <- many (noneOf "*_")
    _ <- try (string "**") <|> try (string "__")
    return ("<b>" ++ content ++ "</b>")

italic :: Parser String
italic = do
    _ <- try (char '*') <|> try (char '_')
    content <- many (noneOf "*_")
    _ <- try (char '*') <|> try (char '_')
    return ("<i>" ++ content ++ "</i>")

text :: Parser String
text = concat <$> manyTill (try bold <|> fmap return (noneOf "\n")) newline

baseHtml :: String -> String
baseHtml text = "<html>\n<head>\n</head>\n<body>\n" ++ text ++ "\n</body>\n</html>"

parseString :: String -> [String]
parseString str =
    case parse (stmt `sepBy` newline) "" str of
        Left err -> ["Error: " ++ show err]
        Right stmts -> stmts


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
