

class UnrecognizedCodecError(RuntimeError):
    pass


class UnsupportedCodecError(RuntimeError):
    def __init__(self, codec):
        self.codec = codec

    def __str__(self):
        return f'Unsupported {self.codec}'
