from string import (digits,
                    ascii_letters,
                    ascii_lowercase)

from typing import *

from minghu6.algs.operator import first
from minghu6.text.seq_enh import underscore

identifier_dict = '_' + digits + ascii_letters
identifier_start_dict = '_' + ascii_letters
blank_dict = ''.join([' ', '\t', '\n'])
delimiter_dict = ''.join([';', '[', ']', '(', ')', '{', '}', ','])
operator_dict = ''.join(['+',
                         '-',
                         '*',
                         '/',
                         '%',
                         '=',
                         '&',
                         '|',
                         '^',
                         '~',
                         '<',
                         '>',
                         ':',
                         '?',
                         '@',
                         '.'])
number_dict = digits + '.'


def is_var_start(c):
    return c == '$'


def is_identifier(c):
    return c in identifier_dict


def is_identifier_start(c):
    return c in identifier_start_dict


def is_number(c):
    return c in number_dict


def is_blank(c):
    return c in blank_dict


def is_asterisk(c):
    return c == '*'


def is_slash(c):
    return c == '/'


def is_backslash(c):
    return c == '\\'


def is_delimiter(c):
    return c in delimiter_dict


def is_operator(c):
    return c in operator_dict


def is_operator_but_leftbracket(c):
    return c not in '<>' and c in operator_dict


def is_singlequote(c):
    return c == "'"


def is_doublequote(c):
    return c == '"'


def is_anything_but_newline(c):
    return c != '\n'


def is_newline(c):
    return c == '\n'


def is_anything_but_slash_asterisk(c):
    return not c in '*' in '/'


def is_anything_but_slash(c):
    return c != '/'


def is_anything_but_backslash_singlequote(c):
    return c not in '\\\''


def is_anything_but_backslash_doublequote(c):
    return c not in '\\"'


def is_left_bracket(c):
    return c == '<'


def is_question(c):
    return c == '?'


def is_anything_but_leftbracket_question(c):
    return c not in '<?' and c in operator_dict


def is_right_bracket(c):
    return c == '>'


def is_anything(c):
    return True


# Import InputType
for k, v in list(globals().items()):
    if k.startswith('is_') and hasattr(v, '__call__'):
        name = f'{k[3:]}'.upper()
        globals().update({name: name})


lex_state_list = [
    'identifier',
    'number',
    'var',
    'blank',
    'comment_start',
    'singlequote_string',
    'doublequote_string',
    'tag_start',
    'doubleslash_comment',
    'asterisk_comment',
    'asterisk_comment_end_start',
    'singlequote_string_prevent_eval',
    'doublequote_string_prevent_eval',
    'delimiter',
    'operator'
]


# Import LexState
for item in lex_state_list:
    name = item.upper()+'_STATE'
    globals().update({name: name})


lex_dfa_map = {
    BLANK_STATE: {
        IDENTIFIER_START:                 IDENTIFIER_STATE,
        NUMBER:                           NUMBER_STATE,
        VAR_START:                        VAR_STATE,
        OPERATOR:                         OPERATOR_STATE,
        SLASH:                            COMMENT_START_STATE,
        SINGLEQUOTE:                      SINGLEQUOTE_STRING_STATE,
        DOUBLEQUOTE:                      DOUBLEQUOTE_STRING_STATE,
        DELIMITER:                        BLANK_STATE,
        BLANK:                            BLANK_STATE
    },
    IDENTIFIER_STATE: {
        IDENTIFIER: IDENTIFIER_STATE,
        BLANK:      BLANK_STATE,
        SLASH:      COMMENT_START_STATE,
        DELIMITER:  BLANK_STATE,
        OPERATOR:   OPERATOR_STATE
    },
    NUMBER_STATE: {
        NUMBER:    NUMBER_STATE,
        OPERATOR:  OPERATOR_STATE,
        BLANK:     BLANK_STATE,
        DELIMITER: BLANK_STATE
    },
    VAR_STATE: {
        VAR_START:        VAR_STATE,
        IDENTIFIER_START: IDENTIFIER_STATE,
        BLANK:            BLANK_STATE,
        DELIMITER:        BLANK_STATE
    },
    OPERATOR_STATE: {
        OPERATOR:         OPERATOR_STATE,  # 操作符最多两位字符
        NUMBER:           NUMBER_STATE,
        VAR_START:        VAR_STATE,
        IDENTIFIER_START: IDENTIFIER_STATE,
        BLANK:            BLANK_STATE,
        DELIMITER:        BLANK_STATE
    },
    COMMENT_START_STATE: {
        SLASH:                  DOUBLESLASH_COMMENT_STATE,
        ASTERISK:               ASTERISK_COMMENT_STATE,
        ANYTHING_BUT_SLASH_ASTERISK: BLANK_STATE,
    },
    DOUBLESLASH_COMMENT_STATE: {
        ANYTHING_BUT_NEWLINE: DOUBLESLASH_COMMENT_STATE,
        NEWLINE:         BLANK_STATE
    },
    ASTERISK_COMMENT_STATE: {
        ASTERISK: ASTERISK_COMMENT_END_START_STATE
    },
    ASTERISK_COMMENT_END_START_STATE: {
        SLASH:         BLANK_STATE,
        ANYTHING_BUT_SLASH: ASTERISK_COMMENT_STATE
    },
    SINGLEQUOTE_STRING_STATE: {
        ANYTHING_BUT_BACKSLASH_SINGLEQUOTE: SINGLEQUOTE_STRING_STATE,
        BACKSLASH:                     SINGLEQUOTE_STRING_PREVENT_EVAL_STATE,
        SINGLEQUOTE:                   BLANK_STATE
    },
    SINGLEQUOTE_STRING_PREVENT_EVAL_STATE: {
        ANYTHING: SINGLEQUOTE_STRING_STATE
    },
    DOUBLEQUOTE_STRING_STATE: {
        ANYTHING_BUT_BACKSLASH_DOUBLEQUOTE: DOUBLEQUOTE_STRING_STATE,
        BACKSLASH:                     DOUBLEQUOTE_STRING_PREVENT_EVAL_STATE,
        DOUBLEQUOTE:                   BLANK_STATE
    },
    DOUBLEQUOTE_STRING_PREVENT_EVAL_STATE: {
        ANYTHING: DOUBLEQUOTE_STRING_STATE
    }
}


def gen_each_state_reg(state_output: Mapping[object, object]) -> Callable[[object], object]:
    print(state_output)

    def state_reg(c: str) -> str:
        g = globals()

        for k in state_output:
            if g[f'is_{k.lower()}'](c):
                return k

    return state_reg


regularization_map = {}
for k, v in lex_dfa_map.items():
    regularization_map[k] = gen_each_state_reg(v)


def next_state(state: str, c: str) -> str:
    return lex_dfa_map[state][regularization_map[state](c)]


class UnrecognizedCharError(BaseException):
    def __init__(self, lineno, colno, state, c):
        self.lineno = lineno
        self.colno = colno
        self.state = state
        self.c = c

    def __str__(self):
        return f'Unrecognized char {self.c} occurred when state: [{self.state}] in (Ln:{self.lineno},Col:{self.colno})'


def token_finished(state_stack, c):
    if state_stack[-2] in (DOUBLEQUOTE_STRING_STATE, SINGLEQUOTE_STRING_STATE) \
        and state_stack[-1] == BLANK_STATE:

        return False

    if len(state_stack) > 2 and state_stack[-2] == BLANK_STATE \
        and state_stack[-3] in (DOUBLEQUOTE_STRING_STATE, SINGLEQUOTE_STRING_STATE):
        return True

    if state_stack[-2] == DELIMITER_STATE:
        return True

    return state_stack[-1] != state_stack[-2]


def run(src):
    state_stack = [BLANK_STATE]
    token = ''
    lineno = 1
    colno = 0

    token_list = []
    for c in src:
        if c == '\n':
            lineno += 1
            colno = 0
        colno += 1

        failed = False
        try:
            state_stack.append(next_state(state_stack[-1], c))
        except KeyError:
            failed = True

        if failed:
            raise UnrecognizedCharError(lineno, colno, state, c)

        if token_finished(state_stack, c):
            token_list.append(token)
            token = ''
        token += c

    return token_list


if __name__ == '__main__':
    from pprint import pprint

    with open('./draft/phpmyadmin/examples/openid.php') as f:
        src = f.read()

    src = """<?php
declare(strict_types=1);

if (false === @include_once 'OpenID/RelyingParty.php') {
    exit;
}

/* Change this to true if using phpMyAdmin over https */
$secure_cookie = false;

/**
 * Map of authenticated users to MySQL user/password pairs.
 */
$AUTH_MAP = [
    'https://launchpad.net/~username' => [
        'user' => 'root',
        'password' => '',
    ],
];
    """
    token_list = run(src)
    pprint(token_list)
    #print(next_state('blank', 'x'))
