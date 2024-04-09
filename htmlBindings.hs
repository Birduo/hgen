module HtmlBindings where

codeBlockTag :: String -> String
codeBlockTag x = "<pre><code>" ++ x ++ "</code></pre>\n"

inlineCodeTag :: String -> String
inlineCodeTag contents = "<code>" ++ contents ++ "</code>"

liTag :: String -> String
liTag x = "<li>" ++ x ++ "</li>\n"

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

linkTag :: String -> String -> String
linkTag x url = "<a href=\"" ++ url ++ "\">" ++ x ++ "</a>"


-- creates boilerplate html
baseHtml :: String -> String
baseHtml text = "<!doctype html>\n<html>\n<head>\n</head>\n<body>\n" ++ text ++ "</body>\n</html>"

-- uses a file for boilerplate html
baseHtmlFile :: String -> String -> String
baseHtmlFile file text = "<!doctype html>\n<html>\n<head>\n" ++ file ++ "</head>\n<body>\n" ++ text ++ "</body>\n</html>"