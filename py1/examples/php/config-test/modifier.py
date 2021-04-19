#!/usr/bin/env python


from pathlib import Path

from overrides import overrides

from codefmt.phpfmt.PhpRewriter import PhpFormatter, PhpRewriter
from codefmt.phpparser.PhpParser import PhpParser


class MyReWriter(PhpRewriter):

    @overrides
    def enterPhpBlock(self, ctx: PhpParser.PhpBlockContext):
        super().enterPhpBlock(ctx)

        if ctx.topStatement():
            for tops in ctx.topStatement():
                if tops.statement():
                    statement = tops.statement()

                    exp = tops.statement().returnStatement().expression()



src = Path('database.php')
src_str = src.open().read()

formatter = PhpFormatter(src_str)
formatter.fmt(ReWriter=MyReWriter)