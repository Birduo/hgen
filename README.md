# HGen: Markdown to HTML compiler

Allows most typical .md formatting (minus some edge cases)

Code blocks also work with highlight.js highlighting support

::id creates an empty div with the id "id"

usage: hgen <input.md> <output.html> <header.html>

header.html is optional and prepends the given html to the output file

linking files also loads the file, ex:
`[test](test.js)` would link test.js to the file with a `<script>` tag
