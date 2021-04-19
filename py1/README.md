
## Using Antlr4


## Generate Parser

For example:

```bash
$ cd ./grammars/php
$ antlr4 -package codefmt.phpparser PhpLexer.g4 -Dlanguage=Python3 -o ../../codefmt/phpparser
$ antlr4 -package codefmt.phpparser PhpParser.g4 -Dlanguage=Python3 -o ../../codefmt/phpparser
```
