

from antlr4 import (
    ParseTreeWalker,
    CommonTokenStream,
    InputStream,
    BufferedTokenStream
)
from antlr4.TokenStreamRewriter import TokenStreamRewriter

from codefmt.phpparser.PhpParserListener import PhpParserListener
from codefmt.phpparser.PhpLexer import PhpLexer
from codefmt.phpparser.PhpParser import PhpParser


class PhpRewriter(PhpParserListener):
    def __init__(self, tokens: BufferedTokenStream):
        self.tokens = tokens
        self.rewriter = TokenStreamRewriter(tokens)


class PhpFormatter:
    def __init__(self, src_str: str):
        self.src_str = src_str

    def fmt(self):
        # Lex analyze
        lexer = PhpLexer(InputStream(self.src_str))
        tokens = CommonTokenStream(lexer)

        # synax analyze
        parser = PhpParser(tokens)
        ast = parser.phpBlock()

        # semantics analyze
        walker = ParseTreeWalker()
        rewriter = PhpRewriter(tokens)

        walker.walk(rewriter, ast)

        print(rewriter.rewriter.getDefaultText())
