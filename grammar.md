// Syntactical grammar for the language.

File ::= [Statement]*
Statement ::= Header | Comment | Inline | Bulleted | Numbered | Code | Text
Header ::= "#" [^#]* Text "\n"
Comment ::= "//" [^/]* Text "\n
Inline ::= "\`" Text "\`"
Bulleted ::= "*" Text "\n" | "+" Text "\n" | "-" Text "\n"
Numbered ::= [0-9]+ "." Text "\n"
Code ::= "\`\`\`" Text "\`\`\`
Text ::= any sequence of characters
