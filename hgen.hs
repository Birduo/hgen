import System.Environment
import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)

-- file ::= (header | comment | text)*
-- header ::= "#" number text
-- comment ::= "//" text
-- text ::= char* bold+ italic+ char*

createHeaderTag :: String -> String -> String
createHeaderTag x y = "<h" ++ x ++ ">" ++ y ++ "</h" ++ x ++ ">\n"

-- comment ::= "//" text 
commentTag :: String -> String
commentTag x = "<!-- " ++ x ++ " -->\n"

boldTag :: String -> String
boldTag x = "<b>" ++ x ++ "</b>"

italicTag :: String -> String
italicTag x = "<i>" ++ x ++ "</i>"

baseHtml :: String -> String
baseHtml text = "<html>\n<head>\n</head>\n<body>\n" ++ text ++ "\n</body>\n</html>"

main :: IO ()
main = putStrLn $ baseHtml $ createHeaderTag "1" "Hello, World!"