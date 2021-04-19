"""codefmt
A Code rewriter
Usage:
  codefmt echo <src>
  codefmt fm

Options:
  echo  echo file.
  fm    do nothing.
  <src>  source code path

"""
from pathlib import Path

from docopt import docopt
from schema import Schema, And, Or, Use, SchemaError

from codefmt.constants import *
from codefmt.errors import *


def handle_src_path(src: Path) -> (Codec, str):
    print(src)

    codec = None
    if src.suffix == '.php':
        codec = Codec.PHP

    if src.is_file():
        with open(src) as f:
            src_str = f.read()
    else:
        raise NotImplementedError()

    if codec is None:
        raise UnrecognizedCodecError()

    return (codec, src_str)


def cli():
    arguments = docopt(__doc__)

    rule_src = And(
        Use(Path, error='<src> should be valid path pattern'),
        Path.exists,
        error='path `{}` should exist'
    )
    rule_keyword = Or(False, True)

    schema = Schema({
        'echo': rule_keyword,
        'fm': rule_keyword,
        '<src>': Or(
            None,
            rule_src
        ),
    })

    try:
        arguments = schema.validate(arguments)
    except SchemaError as e:
        exit(e)

    if arguments['echo']:
        codec, src_str = handle_src_path(arguments['<src>'])

        if codec == Codec.PHP:
            from codefmt.phpfmt.PhpRewriter import PhpFormatter

            formatter = PhpFormatter(src_str)
            formatter.fmt()

        else:
            raise UnsupportedCodecError(codec)



    elif arguments['fm']:
        print('fmt')


if __name__ == '__main__':
    cli()
