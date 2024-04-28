module HtmlBindings where

codeBlockTag :: String -> String -> String
codeBlockTag "html" x = "<pre><code class=\"nohighlight\">" ++ x ++ "</code></pre>\n"
codeBlockTag plat x = "<pre><code class=\"nohighlight\">" ++ x ++ "</code></pre>\n"

highlightCodeTag :: String -> String -> String -> String
highlightCodeTag "html" language x = "<pre><code class=\"" ++ "language-" ++ language ++ "\">" ++ x ++ "</code></pre>\n"
highlightCodeTag plat language x = "<pre><code class=\"" ++ "language-" ++ language ++ "\">" ++ x ++ "</code></pre>\n"

scriptBlockTag :: String -> String -> String
scriptBlockTag "html" x = "<script>" ++ x ++ "</script>"
scriptBlockTag plat x = "<script>" ++ x ++ "</script>"

inlineCodeTag :: String -> String -> String
inlineCodeTag "html" contents = "<code>" ++ contents ++ "</code>"
inlineCodeTag plat contents = "<code>" ++ contents ++ "</code>"

scriptLinkTag :: String -> String -> String
scriptLinkTag "html" url = "<script src=\"" ++ url ++ "\"></script>"
scriptLinkTag plat url = "<script src=\"" ++ url ++ "\"></script>"

cssTag :: String -> String -> String
cssTag "html" url = "<link rel=\"stylesheet\" type=\"text/css\" href=\"" ++ url ++ "\">"
cssTag plat url = "<link rel=\"stylesheet\" type=\"text/css\" href=\"" ++ url ++ "\">"

liTag :: String -> String -> String
liTag "html" x = "<li>" ++ x ++ "</li>\n"
liTag plat x = "<li>" ++ x ++ "</li>\n"

-- creates an unordered list in the given language ("html" by default)
ulTag :: String -> String -> String
ulTag "html" x = "<ul>\n" ++ x ++ "</ul>\n"
ulTag plat x = "<ul>\n" ++ x ++ "</ul>\n"

olTag :: String -> String -> String
olTag "html" x = "<ol>\n" ++ x ++ "</ol>\n"
olTag plat x = "<ol>\n" ++ x ++ "</ol>\n"

hTag :: String -> Int -> String -> String
hTag "html" x y = "<h" ++ show x ++ ">" ++ y ++ "</h" ++ show x ++ ">\n"
hTag plat x y = "<h" ++ show x ++ ">" ++ y ++ "</h" ++ show x ++ ">\n"

pTag :: String -> String -> String
pTag "html" x = "<p>" ++ x ++ "</p>\n"
pTag plat x = "<p>" ++ x ++ "</p>\n"
 
commentTag :: String -> String -> String
commentTag "html" x = "<!-- " ++ x ++ " -->\n"
commentTag plat x = "<!-- " ++ x ++ " -->\n"

boldTag :: String -> String -> String
boldTag "html" x = "<b>" ++ x ++ "</b>"
boldTag plat x = "<b>" ++ x ++ "</b>"

italicTag :: String -> String -> String
italicTag "html" x = "<i>" ++ x ++ "</i>"
italicTag plat x = "<i>" ++ x ++ "</i>"

linkTag :: String -> String -> String -> String
linkTag "html" x url = "<a href=\"" ++ url ++ "\">" ++ x ++ "</a>"
linkTag plat x url = "<a href=\"" ++ url ++ "\">" ++ x ++ "</a>"

breakline :: String -> String
breakline "html" = "<br>"
breakline plat = "<br>"

imgTag :: String -> String -> String -> String
imgTag "html" url alt = "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\">"
imgTag plat url alt = "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\">"

emptyDivTag :: String -> String -> String
emptyDivTag "html" id = "<div id=\"" ++ id ++ "\"></div>"
emptyDivTag plat id = "<div id=\"" ++ id ++ "\"></div>"

-- creates boilerplate html
baseHtml :: String -> String
baseHtml text = "<!doctype html>\n<html>\n<head>\n</head>\n<body>\n" ++ text ++ "</body>\n</html>"

-- uses a file for boilerplate html
baseHtmlFile :: String -> String -> String
baseHtmlFile file text = "<!doctype html>\n<html>\n<head>\n" ++ file ++ "</head>\n<body>\n" ++ text ++ "</body>\n</html>"
