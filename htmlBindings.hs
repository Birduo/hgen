module HtmlBindings where

ulTag :: String -> String
ulTag x = "<ul>\n" ++ x ++ "</ul>\n"

olTag :: String -> String
olTag x = "<ol>\n" ++ x ++ "</ol>\n"

hTag :: Int -> String -> String
hTag x y = "<h" ++ show x ++ ">" ++ y ++ "</h" ++ show x ++ ">\n"

pTag :: String -> String
pTag x = "<p>" ++ x ++ "</p>\n"
 
commentTag :: String -> String
commentTag x = "<!-- " ++ x ++ " -->\n"

boldTag :: String -> String
boldTag x = "<b>" ++ x ++ "</b>"

italicTag :: String -> String
italicTag x = "<i>" ++ x ++ "</i>"

baseHtml :: String -> String
baseHtml text = "<!doctype html>\n<html>\n<head>\n</head>\n<body>\n" ++ text ++ "</body>\n</html>"
