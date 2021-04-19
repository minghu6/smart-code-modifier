
## Using Antlr4


## Generate Parser

For example:

```bash
$ cd ./codefmt/phpparser/
$ antlr4 -Dlanguage=Python3 PhpLexer.g4 PhpParser.g4
```


## Run examples

```bash
python -m codefmt echo ../examples/PHP/database.php
```
