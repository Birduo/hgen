import System.Environment
import Data.Char (isSpace)

-- using computerphile's video on arithmetic parsing to better understand parsing in Haskell
import Parsing

-- <- operator is like = but it runs the function and applies the result to the var (monad stuff)
-- <|> is a choice operator executing the '|' of the grammar

-- general parsing functions to take a string either from fn or file and parse it

parseString str = parse expr (filter (not . isSpace) str)

parseFile filename = do
    str <- readFile filename
    return (parseString str)

-- arithmetic parsing below !

-- expr ::= term + expr | term - expr | term
expr = do 
    x <- term
    char '+'
    y <- expr
    return (x + y)
    <|> do 
    x <- term
    char '-'
    y <- expr
    return (x - y)
    <|> term

-- term ::= factor * term | factor / term | factor
term = do 
    x <- factor
    char '*'
    y <- term
    return (x * y)
    <|> do
    x <- factor
    char '/'
    y <- term
    return (x / y)
    <|> factor

-- factor ::= (expr) | int
factor = do 
    char '('
    x <- expr
    char ')'
    return x
    <|> num

-- primary parsing below !

-- ident is seen in Parser.hs
-- ident ::= lower many alphanum
-- potential identifier search list: vars = [("a", 3)]

-- num ::= digit+ . digit+
num :: Parser Double
num = do
    x <- some digit
    char '.'
    y <- some digit
    -- Concatenating the two digits and then reading them as a float
    return (read (x ++ "." ++ y) :: Double)
    <|> do
    x <- int
    let out = fromIntegral x :: Double
    return out
