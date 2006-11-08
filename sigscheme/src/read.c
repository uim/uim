/*===========================================================================
 *  Filename : read.c
 *  About    : S-Expression reader
 *
 *  Copyright (C) 2000-2005 Shiro Kawai <shiro AT acm.org>
 *  Copyright (C) 2005-2006 Kazuki Ohta <mover AT hct.zaq.ne.jp>
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the name of authors nor the names of its contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
 *  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 *  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 *  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
 *  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 *  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 *  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 *  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 *  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
===========================================================================*/

/*
 * ChangeLog
 *
 * 2005-06-18 kzk      Copied from read.c of Gauche 0.8.5 and modified for
 *                     SigScheme.
 * 2005-11-01
 *    ...
 * 2006-02-03 YamaKen  Add SRFI-75 support, introduce safe and low-consumptive
 *                     stack management, table-based char classification, and
 *                     overall rewrite.
 *
 */

/* TODO: replace with character class sequence expression-based tokenizer */

/*
 * R5RS: 7.1.1 Lexical structure
 *
 * <token> --> <identifier> | <boolean> | <number> | <character> | <string>
 *      | ( | ) | #( | ' | ` | , | ,@ | .
 * <delimiter> --> <whitespace> | ( | ) | " | ;
 * <whitespace> --> <space or newline>
 * <comment> --> ;  <all subsequent characters up to a
 *                  line break>
 * <atmosphere> --> <whitespace> | <comment>
 * <intertoken space> --> <atmosphere>*
 *
 * <identifier> --> <initial> <subsequent>* | <peculiar identifier>
 * <initial> --> <letter> | <special initial>
 * <letter> --> a | b | c | ... | z
 *
 * <special initial> --> ! | $ | % | & | * | / | : | < | = | > | ? | ^ | _ | ~
 * <subsequent> --> <initial> | <digit> | <special subsequent>
 * <digit> --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
 * <special subsequent> --> + | - | . | @
 * <peculiar identifier> --> + | - | ...
 * <syntactic keyword> --> <expression keyword>
 *      | else | => | define
 *      | unquote | unquote-splicing
 * <expression keyword> --> quote | lambda | if
 *      | set! | begin | cond | and | or | case
 *      | let | let* | letrec | do | delay
 *      | quasiquote
 *
 * `<variable> => <'any <identifier> that isn't
 *                 also a <syntactic keyword>>
 *
 * <boolean> --> #t | #f
 * <character> --> #\ <any character>
 *      | #\ <character name>
 * <character name> --> space | newline
 *
 * <string> --> " <string element>* "
 * <string element> --> <any character other than " or \>
 *      | \" | \\
 *
 * <number> --> <num 2>| <num 8>
 *      | <num 10>| <num 16>
 *
 *
 * <num R> --> <prefix R> <complex R>
 * <complex R> --> <real R> | <real R> @ <real R>
 *     | <real R> + <ureal R> i | <real R> - <ureal R> i
 *     | <real R> + i | <real R> - i
 *     | + <ureal R> i | - <ureal R> i | + i | - i
 * <real R> --> <sign> <ureal R>
 * <ureal R> --> <uinteger R>
 *     | <uinteger R> / <uinteger R>
 *     | <decimal R>
 * <decimal 10> --> <uinteger 10> <suffix>
 *     | . <digit 10>+ #* <suffix>
 *     | <digit 10>+ . <digit 10>* #* <suffix>
 *     | <digit 10>+ #+ . #* <suffix>
 * <uinteger R> --> <digit R>+ #*
 * <prefix R> --> <radix R> <exactness>
 *     | <exactness> <radix R>
 *
 * <suffix> --> <empty>
 *     | <exponent marker> <sign> <digit 10>+
 * <exponent marker> --> e | s | f | d | l
 * <sign> --> <empty>  | + |  -
 * <exactness> --> <empty> | #i | #e
 * <radix 2> --> #b
 * <radix 8> --> #o
 * <radix 10> --> <empty> | #d
 * <radix 16> --> #x
 * <digit 2> --> 0 | 1
 * <digit 8> --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
 * <digit 10> --> <digit>
 * <digit 16> --> <digit 10> | a | b | c | d | e | f
 */

#include <config.h>

#include <limits.h>
#include <stdlib.h>
#include <string.h>
#if (HAVE_STRCASECMP && HAVE_STRINGS_H)
#include <strings.h>
#endif

#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Definitions
=======================================*/
#define OK 0
#define TOKEN_BUF_EXCEEDED (-1)

#define MB_MAX_SIZE (SCM_MB_MAX_LEN + sizeof(""))

/* can accept "backspace" of R5RS and "U0010FFFF" of SRFI-75 */
#define CHAR_LITERAL_LEN_MAX (sizeof("backspace") - sizeof(""))

/* #b-010101... */
#define INT_LITERAL_LEN_MAX (sizeof("-0") + SCM_INT_BITS - sizeof(""))

#define DISCARD_LOOKAHEAD(port) (scm_port_get_char(port))

/* accepts EOF */
#define ICHAR_ASCII_CLASS(c)                                                 \
    (ICHAR_ASCIIP(c) ? scm_char_class_table[c] : SCM_CH_INVALID)
#define ICHAR_CLASS(c)                                                       \
    ((127 < (c)) ? SCM_CH_NONASCII                                           \
                 : (((c) < 0) ? SCM_CH_INVALID : scm_char_class_table[c]))

/*=======================================
  File Local Type Definitions
=======================================*/
enum LexerState {
    LEX_ST_NORMAL,
    LEX_ST_COMMENT
};

enum ScmCharClass {
    /* ASCII */
    SCM_CH_INVALID            = 0,
    SCM_CH_CONTROL            = 1 << 0, /* iscntrl(3) + backslash */
    SCM_CH_WHITESPACE         = 1 << 1, /* [ \t\n\r\v\f] */
    SCM_CH_DIGIT              = 1 << 2, /* [0-9] */
    SCM_CH_HEX_LETTER         = 1 << 3, /* [a-fA-F] */
    SCM_CH_NONHEX_LETTER      = 1 << 4, /* [g-zG-Z] */
    SCM_CH_SPECIAL_INITIAL    = 1 << 5, /* [!$%&*\/:<=>?^_~] */
    SCM_CH_SPECIAL_SUBSEQUENT = 1 << 6, /* [-+\.@] */
    /* currently '.' is not included in SCM_CH_TOKEN_INITIAL */
    SCM_CH_TOKEN_INITIAL      = 1 << 7, /* [()#'`,\"\|\{\}\[\]] */

    SCM_CH_LETTER     = SCM_CH_HEX_LETTER | SCM_CH_NONHEX_LETTER,
    SCM_CH_HEX_DIGIT  = SCM_CH_DIGIT | SCM_CH_HEX_LETTER,
    SCM_CH_INITIAL    = SCM_CH_LETTER | SCM_CH_SPECIAL_INITIAL,
    SCM_CH_SUBSEQUENT = SCM_CH_INITIAL | SCM_CH_DIGIT,
    SCM_CH_PECULIAR_IDENTIFIER_CAND = SCM_CH_SPECIAL_SUBSEQUENT,
    SCM_CH_DELIMITER
        = SCM_CH_CONTROL | SCM_CH_WHITESPACE | SCM_CH_TOKEN_INITIAL,

    /* beyond ASCII */
    SCM_CH_ASCII              = 0 << 8,
    SCM_CH_8BIT               = 1 << 8,
    SCM_CH_MULTIBYTE          = 1 << 9,

    SCM_CH_NONASCII           = SCM_CH_8BIT | SCM_CH_MULTIBYTE
};

/*=======================================
  Variable Definitions
=======================================*/
static const unsigned char scm_char_class_table[] = {
    SCM_CH_CONTROL,            /*   0  nul       */
    SCM_CH_CONTROL,            /*   1  x01       */
    SCM_CH_CONTROL,            /*   2  x02       */
    SCM_CH_CONTROL,            /*   3  x03       */
    SCM_CH_CONTROL,            /*   4  x04       */
    SCM_CH_CONTROL,            /*   5  x05       */
    SCM_CH_CONTROL,            /*   6  x06       */
    SCM_CH_CONTROL,            /*   7  alarm     */
    SCM_CH_CONTROL,            /*   8  backspace */
    SCM_CH_CONTROL | SCM_CH_WHITESPACE, /*   9  tab       */
    SCM_CH_CONTROL | SCM_CH_WHITESPACE, /*  10  newline   */
    SCM_CH_CONTROL | SCM_CH_WHITESPACE, /*  11  vtab      */
    SCM_CH_CONTROL | SCM_CH_WHITESPACE, /*  12  page      */
    SCM_CH_CONTROL | SCM_CH_WHITESPACE, /*  13  return    */
    SCM_CH_CONTROL,            /*  14  x0e       */
    SCM_CH_CONTROL,            /*  15  x0f       */
    SCM_CH_CONTROL,            /*  16  x10       */
    SCM_CH_CONTROL,            /*  17  x11       */
    SCM_CH_CONTROL,            /*  18  x12       */
    SCM_CH_CONTROL,            /*  19  x13       */
    SCM_CH_CONTROL,            /*  20  x14       */
    SCM_CH_CONTROL,            /*  21  x15       */
    SCM_CH_CONTROL,            /*  22  x16       */
    SCM_CH_CONTROL,            /*  23  x17       */
    SCM_CH_CONTROL,            /*  24  x18       */
    SCM_CH_CONTROL,            /*  25  x19       */
    SCM_CH_CONTROL,            /*  26  x1a       */
    SCM_CH_CONTROL,            /*  27  esc       */
    SCM_CH_CONTROL,            /*  28  x1c       */
    SCM_CH_CONTROL,            /*  29  x1d       */
    SCM_CH_CONTROL,            /*  30  x1e       */
    SCM_CH_CONTROL,            /*  31  x1f       */
    SCM_CH_WHITESPACE,         /*  32  space     */
    SCM_CH_SPECIAL_INITIAL,    /*  33  !         */
    SCM_CH_TOKEN_INITIAL,      /*  34  "         */
    SCM_CH_TOKEN_INITIAL,      /*  35  #         */
    SCM_CH_SPECIAL_INITIAL,    /*  36  $         */
    SCM_CH_SPECIAL_INITIAL,    /*  37  %         */
    SCM_CH_SPECIAL_INITIAL,    /*  38  &         */
    SCM_CH_TOKEN_INITIAL,      /*  39  '         */
    SCM_CH_TOKEN_INITIAL,      /*  40  (         */
    SCM_CH_TOKEN_INITIAL,      /*  41  )         */
    SCM_CH_SPECIAL_INITIAL,    /*  42  *         */
    SCM_CH_SPECIAL_SUBSEQUENT, /*  43  +         */
    SCM_CH_TOKEN_INITIAL,      /*  44  ,         */
    SCM_CH_SPECIAL_SUBSEQUENT, /*  45  -         */
    SCM_CH_SPECIAL_SUBSEQUENT /* | SCM_CH_TOKEN_INITIAL */, /*  46  .        */
    SCM_CH_SPECIAL_INITIAL,    /*  47  /         */
    SCM_CH_DIGIT,              /*  48  0         */
    SCM_CH_DIGIT,              /*  49  1         */
    SCM_CH_DIGIT,              /*  50  2         */
    SCM_CH_DIGIT,              /*  51  3         */
    SCM_CH_DIGIT,              /*  52  4         */
    SCM_CH_DIGIT,              /*  53  5         */
    SCM_CH_DIGIT,              /*  54  6         */
    SCM_CH_DIGIT,              /*  55  7         */
    SCM_CH_DIGIT,              /*  56  8         */
    SCM_CH_DIGIT,              /*  57  9         */
    SCM_CH_SPECIAL_INITIAL,    /*  58  :         */
    SCM_CH_TOKEN_INITIAL,      /*  59  ;         */
    SCM_CH_SPECIAL_INITIAL,    /*  60  <         */
    SCM_CH_SPECIAL_INITIAL,    /*  61  =         */
    SCM_CH_SPECIAL_INITIAL,    /*  62  >         */
    SCM_CH_SPECIAL_INITIAL,    /*  63  ?         */
    SCM_CH_SPECIAL_SUBSEQUENT, /*  64  @         */
    SCM_CH_HEX_LETTER,         /*  65  A         */
    SCM_CH_HEX_LETTER,         /*  66  B         */
    SCM_CH_HEX_LETTER,         /*  67  C         */
    SCM_CH_HEX_LETTER,         /*  68  D         */
    SCM_CH_HEX_LETTER,         /*  69  E         */
    SCM_CH_HEX_LETTER,         /*  70  F         */
    SCM_CH_NONHEX_LETTER,      /*  71  G         */
    SCM_CH_NONHEX_LETTER,      /*  72  H         */
    SCM_CH_NONHEX_LETTER,      /*  73  I         */
    SCM_CH_NONHEX_LETTER,      /*  74  J         */
    SCM_CH_NONHEX_LETTER,      /*  75  K         */
    SCM_CH_NONHEX_LETTER,      /*  76  L         */
    SCM_CH_NONHEX_LETTER,      /*  77  M         */
    SCM_CH_NONHEX_LETTER,      /*  78  N         */
    SCM_CH_NONHEX_LETTER,      /*  79  O         */
    SCM_CH_NONHEX_LETTER,      /*  80  P         */
    SCM_CH_NONHEX_LETTER,      /*  81  Q         */
    SCM_CH_NONHEX_LETTER,      /*  82  R         */
    SCM_CH_NONHEX_LETTER,      /*  83  S         */
    SCM_CH_NONHEX_LETTER,      /*  84  T         */
    SCM_CH_NONHEX_LETTER,      /*  85  U         */
    SCM_CH_NONHEX_LETTER,      /*  86  V         */
    SCM_CH_NONHEX_LETTER,      /*  87  W         */
    SCM_CH_NONHEX_LETTER,      /*  88  X         */
    SCM_CH_NONHEX_LETTER,      /*  89  Y         */
    SCM_CH_NONHEX_LETTER,      /*  90  Z         */
    SCM_CH_TOKEN_INITIAL,      /*  91  [         */
    SCM_CH_CONTROL,            /*  92  \\        */
    SCM_CH_TOKEN_INITIAL,      /*  93  ]         */
    SCM_CH_SPECIAL_INITIAL,    /*  94  ^         */
    SCM_CH_SPECIAL_INITIAL,    /*  95  _         */
    SCM_CH_TOKEN_INITIAL,      /*  96  `         */
    SCM_CH_HEX_LETTER,         /*  97  a         */
    SCM_CH_HEX_LETTER,         /*  98  b         */
    SCM_CH_HEX_LETTER,         /*  99  c         */
    SCM_CH_HEX_LETTER,         /* 100  d         */
    SCM_CH_HEX_LETTER,         /* 101  e         */
    SCM_CH_HEX_LETTER,         /* 102  f         */
    SCM_CH_NONHEX_LETTER,      /* 103  g         */
    SCM_CH_NONHEX_LETTER,      /* 104  h         */
    SCM_CH_NONHEX_LETTER,      /* 105  i         */
    SCM_CH_NONHEX_LETTER,      /* 106  j         */
    SCM_CH_NONHEX_LETTER,      /* 107  k         */
    SCM_CH_NONHEX_LETTER,      /* 108  l         */
    SCM_CH_NONHEX_LETTER,      /* 109  m         */
    SCM_CH_NONHEX_LETTER,      /* 110  n         */
    SCM_CH_NONHEX_LETTER,      /* 111  o         */
    SCM_CH_NONHEX_LETTER,      /* 112  p         */
    SCM_CH_NONHEX_LETTER,      /* 113  q         */
    SCM_CH_NONHEX_LETTER,      /* 114  r         */
    SCM_CH_NONHEX_LETTER,      /* 115  s         */
    SCM_CH_NONHEX_LETTER,      /* 116  t         */
    SCM_CH_NONHEX_LETTER,      /* 117  u         */
    SCM_CH_NONHEX_LETTER,      /* 118  v         */
    SCM_CH_NONHEX_LETTER,      /* 119  w         */
    SCM_CH_NONHEX_LETTER,      /* 120  x         */
    SCM_CH_NONHEX_LETTER,      /* 121  y         */
    SCM_CH_NONHEX_LETTER,      /* 122  z         */
    SCM_CH_TOKEN_INITIAL,      /* 123  {         */
    SCM_CH_TOKEN_INITIAL,      /* 124  |         */
    SCM_CH_TOKEN_INITIAL,      /* 125  }         */
    SCM_CH_SPECIAL_INITIAL,    /* 126  ~         */
    SCM_CH_CONTROL,            /* 127  delete    */
};

/*=======================================
  File Local Function Declarations
=======================================*/
static scm_ichar_t skip_comment_and_space(ScmObj port);
static void   read_sequence(ScmObj port, char *buf, int len);
static size_t read_token(ScmObj port, int *err,
                         char *buf, size_t buf_size, enum ScmCharClass delim);

static ScmObj read_sexpression(ScmObj port);
static ScmObj read_list(ScmObj port, scm_ichar_t closeParen);
#if SCM_USE_SRFI75
static scm_ichar_t parse_unicode_sequence(const char *seq, int len);
static scm_ichar_t read_unicode_sequence(ScmObj port, char prefix);
#endif /* SCM_USE_SRFI75 */
#if SCM_USE_CHAR
static ScmObj read_char(ScmObj port);
#endif /* SCM_USE_CHAR */
#if SCM_USE_STRING
static ScmObj read_string(ScmObj port);
#endif /* SCM_USE_STRING */
static ScmObj read_symbol(ScmObj port);
static ScmObj read_number_or_symbol(ScmObj port);
#if SCM_USE_NUMBER
static ScmObj parse_number(ScmObj port,
                           char *buf, size_t buf_size, char prefix);
static ScmObj read_number(ScmObj port, char prefix);
#endif /* SCM_USE_NUMBER */
static ScmObj read_quote(ScmObj port, ScmObj quoter);

/*=======================================
  Function Definitions
=======================================*/
/*===========================================================================
  S-Expression Parser
===========================================================================*/
SCM_EXPORT ScmObj
scm_read(ScmObj port)
{
    ScmObj sexp;
    DECLARE_INTERNAL_FUNCTION("scm_read");

    sexp = read_sexpression(port);
#if SCM_DEBUG
    if ((scm_debug_categories() & SCM_DBG_READ) && !EOFP(sexp)) {
        scm_write(scm_err, sexp);
        scm_port_newline(scm_err);
    }
#endif

    return sexp;
}

SCM_EXPORT ScmObj
scm_read_char(ScmObj port)
{
    DECLARE_INTERNAL_FUNCTION("scm_read_char");

    ENSURE_PORT(port);

    return read_char(port);
}


static scm_ichar_t
skip_comment_and_space(ScmObj port)
{
    scm_ichar_t c;
    int state;

    for (state = LEX_ST_NORMAL;;) {
        c = scm_port_peek_char(port);
        switch (state) {
        case LEX_ST_NORMAL:
            if (c == ';')
                state = LEX_ST_COMMENT;
            else if (!ICHAR_WHITESPACEP(c) || c == EOF)
                return c;  /* peeked */
            break;

        case LEX_ST_COMMENT:
            if (c == '\n' || c == '\r')
                state = LEX_ST_NORMAL;
            else if (c == EOF)
                return c;  /* peeked */
            break;
        }
        scm_port_get_char(port);  /* skip the char */
    }
}

static void
read_sequence(ScmObj port, char *buf, int len)
{
    scm_ichar_t c;
    char *p;
    DECLARE_INTERNAL_FUNCTION("read");

    for (p = buf; p < &buf[len]; p++) {
        c = scm_port_get_char(port);
        if (c == EOF)
            ERR("unexpected EOF");
        if (!ICHAR_ASCIIP(c))
            ERR("unexpected non-ASCII char");
        *p = c;
    }
    buf[len] = '\0';
}

static size_t
read_token(ScmObj port, int *err,
           char *buf, size_t buf_size, enum ScmCharClass delim)
{
    ScmCharCodec *codec;
    enum ScmCharClass ch_class;
    scm_ichar_t c;
    size_t len;
    char *p;
    DECLARE_INTERNAL_FUNCTION("read");

    for (p = buf;;) {
        c = scm_port_peek_char(port);
        ch_class = ICHAR_CLASS(c);
        CDBG((SCM_DBG_PARSER, "c = ~C", c));

        if (p == buf) {
            if (c == EOF)
                ERR("unexpected EOF at a token");
        } else {
            if (ch_class & delim || c == EOF) {
                *err = OK;
                break;
            }
        }

        if (ch_class & SCM_CH_NONASCII) {
#if SCM_USE_SRFI75
            if (&buf[buf_size] <= p + SCM_MB_MAX_LEN) {
                *err = TOKEN_BUF_EXCEEDED;
                break;
            }
            codec = scm_port_codec(port);
            if (SCM_CHARCODEC_CCS(codec) != SCM_CCS_UCS4)
                ERR("non-ASCII char in token on a non-Unicode port: 0x~MX",
                    (scm_int_t)c);
            /* canonicalize internal Unicode encoding */
            p = SCM_CHARCODEC_INT2STR(scm_identifier_codec, p, c,
                                      SCM_MB_STATELESS);
#else
            ERR("non-ASCII char in token: 0x~X", (int)c);
#endif
        } else {
            if (p == &buf[buf_size - sizeof("")]) {
                *err = TOKEN_BUF_EXCEEDED;
                break;
            }
            *p++ = c;
        }
        DISCARD_LOOKAHEAD(port);
    }

    *p = '\0';
    len = p - buf;
    return len;
}

static ScmObj
read_sexpression(ScmObj port)
{
    ScmObj ret;
    enum ScmCharClass ch_class;
    scm_ichar_t c;
    DECLARE_INTERNAL_FUNCTION("read");

    CDBG((SCM_DBG_PARSER, "read_sexpression"));

    for (;;) {
        c = skip_comment_and_space(port);

        CDBG((SCM_DBG_PARSER, "read_sexpression c = ~C", c));

        ch_class = ICHAR_CLASS(c);
        if (ch_class & (SCM_CH_INITIAL | SCM_CH_NONASCII))
            return read_symbol(port);

        if (ch_class & (SCM_CH_DIGIT | SCM_CH_PECULIAR_IDENTIFIER_CAND))
            return read_number_or_symbol(port);

        /* case labels are ordered by appearance rate and penalty cost */
        DISCARD_LOOKAHEAD(port);
        switch (c) {
        case '(':
            return read_list(port, ')');

#if SCM_USE_STRING
        case '\"':
            return read_string(port);
#endif

        case '\'':
            return read_quote(port, SYM_QUOTE);

        case '#':
            c = scm_port_get_char(port);
            switch (c) {
            case 't':
                return SCM_TRUE;
            case 'f':
                return SCM_FALSE;
#if SCM_USE_VECTOR
            case '(':
                ret = scm_p_list2vector(read_list(port, ')'));
#if SCM_CONST_VECTOR_LITERAL
                SCM_VECTOR_SET_IMMUTABLE(ret);
#endif
                return ret;
#endif /* SCM_USE_VECTOR */
#if SCM_USE_CHAR
            case '\\':
                return read_char(port);
#endif
#if SCM_USE_NUMBER
            /* TODO: support exactness prefixes 'i' and 'e' */
            case 'b': case 'o': case 'd': case 'x':
                return read_number(port, c);
#endif
            case EOF:
                ERR("EOF in #");
            default:
                ERR("Unsupported # notation: ~C", c);
            }
            break;

        case '`':
            return read_quote(port, SYM_QUASIQUOTE);

        case ',':
            c = scm_port_peek_char(port);
            switch (c) {
            case EOF:
                ERR("EOF in unquote");
                /* NOTREACHED */

            case '@':
                DISCARD_LOOKAHEAD(port);
                return read_quote(port, SYM_UNQUOTE_SPLICING);

            default:
                return read_quote(port, SYM_UNQUOTE);
            }

        case ')':
            ERR("unexpected ')'");
            /* NOTREACHED */

        case EOF:
            return SCM_EOF;

        default:
            SCM_ASSERT(scm_false);
        }
    }
}

static ScmObj
read_list(ScmObj port, scm_ichar_t closeParen)
{
    ScmObj lst, elm, cdr;
    ScmQueue q;
#if SCM_DEBUG
    ScmBaseCharPort *basecport;
    size_t start_line, cur_line;
#endif
    scm_ichar_t c;
    int err;
    char dot_buf[sizeof("...")];
    DECLARE_INTERNAL_FUNCTION("read");

#if SCM_DEBUG
    CDBG((SCM_DBG_PARSER, "read_list"));
    basecport = SCM_PORT_TRY_DYNAMIC_CAST(ScmBaseCharPort,
                                          SCM_PORT_IMPL(port));
    start_line = (basecport) ? ScmBaseCharPort_line_number(basecport) : 0;
#endif

    for (lst = SCM_NULL, SCM_QUEUE_POINT_TO(q, lst);
         ;
#if SCM_CONST_LIST_LITERAL
         SCM_QUEUE_CONST_ADD(q, elm)
#else
         SCM_QUEUE_ADD(q, elm)
#endif
         )
    {
        c = skip_comment_and_space(port);

        CDBG((SCM_DBG_PARSER, "read_list c = [~C]", c));

        if (c == EOF) {
#if SCM_DEBUG
            if (basecport && start_line) {
                cur_line = ScmBaseCharPort_line_number(basecport);
                ERR("EOF inside list at line ~ZU (started from line ~ZU)",
                    cur_line, start_line);
            } else
#endif
                ERR("EOF inside list");
        } else if (c == closeParen) {
            DISCARD_LOOKAHEAD(port);
            return lst;
        } else if (c == '.') {
            /* Since expressions that beginning with a dot are limited to '.',
             * '...' and numbers in R5RS (See "7.1.1 Lexical structure"), fixed
             * size buffer can safely buffer them.
             */
            read_token(port, &err, dot_buf, sizeof(dot_buf), SCM_CH_DELIMITER);

            if (dot_buf[1] == '\0') {
#if !SCM_STRICT_R5RS
                /* Although implicit delimiter around the dot is allowd by
                 * R5RS, some other implementation doesn't parse so
                 * (e.g. '("foo"."bar") is parsed as 3 element list which 2nd
                 * elem is dot as symbol). To avoid introducing such
                 * incompatibility problem into codes of SigScheme users,
                 * require explicit whitespace around the dot.
                 */
                c = scm_port_peek_char(port);
                if (!ICHAR_WHITESPACEP(c))
                    ERR("implicit dot delimitation is disabled to avoid compatibility problem");
#endif
                if (NULLP(lst))
                    ERR(".(dot) at the start of the list");

                cdr = read_sexpression(port);
                c = skip_comment_and_space(port);
                DISCARD_LOOKAHEAD(port);
                if (c != ')')
                    ERR("bad dot syntax");

                SCM_QUEUE_SLOPPY_APPEND(q, cdr);
                return lst;
            } else if (strcmp(dot_buf, "...") == 0) {
                elm = scm_intern(dot_buf);
            } else {
                ERR("bad dot syntax");
            }
        } else {
            elm = read_sexpression(port);
        }
    }
}

#if SCM_USE_SRFI75
static scm_ichar_t
parse_unicode_sequence(const char *seq, int len)
{
    scm_ichar_t c;
    char *end;
    DECLARE_INTERNAL_FUNCTION("read");

    /* reject ordinary char literal and invalid signed hexadecimal */
    if (len < 3 || !ICHAR_HEXA_NUMERICP(seq[1]))
        return -1;

    c = strtol(&seq[1], &end, 16);
    if (*end)
        return -1;

    switch (seq[0]) {
    case 'x':
        /* #\x<x><x> : <x> = a hexadecimal digit (ignore case) */
        if (len != 3)
            ERR("invalid hexadecimal character sequence. conform \\x<x><x>");
        break;

    case 'u':
        /* #\u<x><x><x><x> : Unicode char of BMP */
        if (len != 5)
            ERR("invalid Unicode sequence. conform \\u<x><x><x><x>");
        break;

    case 'U':
        /* #\U<x><x><x><x><x><x><x><x> : Unicode char of BMP or SMP */
        if (len != 9)
            ERR("invalid Unicode sequence. conform \\U<x><x><x><x><x><x><x><x>");
        break;

    default:
        return -1;
    }

    if ((0xd800 <= c && c <= 0xdfff) || 0x10ffff < c)
        ERR("invalid Unicode value: 0x~MX", (scm_int_t)c);

    return c;
}

static scm_ichar_t
read_unicode_sequence(ScmObj port, char prefix)
{
    int len;
    char seq[sizeof("U0010ffff")];

    switch (prefix) {
    case 'x': len = 2; break;
    case 'u': len = 4; break;
    case 'U': len = 8; break;
    default:
        SCM_ASSERT(scm_false);
    }
    seq[0] = prefix;
    read_sequence(port, &seq[1], len);
    return parse_unicode_sequence(seq, len + sizeof(prefix));
}
#endif /* SCM_USE_SRFI75 */

#if SCM_USE_CHAR
static ScmObj
read_char(ScmObj port)
{
    const ScmSpecialCharInfo *info;
    ScmCharCodec *codec;
    size_t len;
    scm_ichar_t c, next;
#if SCM_USE_SRFI75
    scm_ichar_t unicode;
#endif
    int err;
    char buf[CHAR_LITERAL_LEN_MAX + sizeof("")];
    DECLARE_INTERNAL_FUNCTION("read");

    /* plain char (multibyte-ready) */
    c = scm_port_get_char(port);
    next = scm_port_peek_char(port);
    if (ICHAR_ASCII_CLASS(next) & SCM_CH_DELIMITER || next == EOF)
        return MAKE_CHAR(c);
#if SCM_USE_SRFI75
    else if (!ICHAR_ASCIIP(c))
        ERR("invalid character literal");
#endif

    buf[0] = c;
    len = read_token(port, &err, &buf[1], sizeof(buf) - 1, SCM_CH_DELIMITER);
    if (err == TOKEN_BUF_EXCEEDED)
        ERR("invalid character literal");

    CDBG((SCM_DBG_PARSER, "read_char: ch = ~S", buf));

#if SCM_USE_SRFI75
    unicode = parse_unicode_sequence(buf, len + 1);
    if (0 <= unicode) {
        codec = scm_port_codec(port);
        if (c != 'x' && SCM_CHARCODEC_CCS(codec) != SCM_CCS_UCS4)
            ERR_OBJ("Unicode char sequence on non-Unicode port", port);
        return MAKE_CHAR(unicode);
    }
#endif
    /* named chars */
    for (info = scm_special_char_table; info->esc_seq; info++) {
        /*
         * R5RS: 6.3.4 Characters
         * Case is significant in #\<character>, but not in #\<character name>.
         */
        if (strcasecmp(buf, info->lex_rep) == 0)
            return MAKE_CHAR(info->code);
    }
    ERR("invalid character literal: #\\~S", buf);
}
#endif /* SCM_USE_CHAR */

#if SCM_USE_STRING
static ScmObj
read_string(ScmObj port)
{
    ScmObj obj;
    const ScmSpecialCharInfo *info;
    ScmCharCodec *codec;
    scm_int_t len;
    scm_ichar_t c;
    char *p;
    size_t offset;
    ScmLBuf(char) lbuf;
    char init_buf[SCM_INITIAL_STRING_BUF_SIZE];
    DECLARE_INTERNAL_FUNCTION("read");

    CDBG((SCM_DBG_PARSER, "read_string"));

    LBUF_INIT(lbuf, init_buf, sizeof(init_buf));
    codec = scm_port_codec(port);

    for (offset = 0, p = LBUF_BUF(lbuf), len = 0;
         ;
         offset = p - LBUF_BUF(lbuf), len++)
    {
        c = scm_port_get_char(port);

        CDBG((SCM_DBG_PARSER, "read_string c = ~C", c));

        switch (c) {
        case EOF:
            LBUF_EXTEND(lbuf, SCM_LBUF_F_STRING, offset + 1);
            *p = '\0';
            ERR("EOF in string: \"~S<eof>", LBUF_BUF(lbuf));
            break;

        case '\"':
            LBUF_EXTEND(lbuf, SCM_LBUF_F_STRING, offset + 1);
            *p = '\0';
            obj = MAKE_IMMUTABLE_STRING_COPYING(LBUF_BUF(lbuf), len);
            LBUF_FREE(lbuf);
            return obj;

        case '\\':
            c = scm_port_get_char(port);
#if SCM_USE_SRFI75
            if (strchr("xuU", c)) {
                if (c != 'x' && SCM_CHARCODEC_CCS(codec) != SCM_CCS_UCS4)
                    ERR_OBJ("Unicode char sequence on non-Unicode port", port);
                c = read_unicode_sequence(port, c);
                LBUF_EXTEND(lbuf, SCM_LBUF_F_STRING, offset + MB_MAX_SIZE);
                p = &LBUF_BUF(lbuf)[offset];
                p = SCM_CHARCODEC_INT2STR(codec, p, c, SCM_MB_STATELESS);
                if (!p)
                    ERR("invalid Unicode sequence in string: 0x~MX",
                        (scm_int_t)c);
                goto found;
            } else
#endif
            {
                /* escape sequences */
                for (info = scm_special_char_table; info->esc_seq; info++) {
                    if (strlen(info->esc_seq) == 2 && c == info->esc_seq[1]) {
                        LBUF_EXTEND(lbuf, SCM_LBUF_F_STRING, offset + 1);
                        p = &LBUF_BUF(lbuf)[offset];
                        *p++ = info->code;
                        goto found;
                    }
                }
            }
            ERR("invalid escape sequence in string: \\~C", c);
        found:
            break;

        default:
            LBUF_EXTEND(lbuf, SCM_LBUF_F_STRING, offset + MB_MAX_SIZE);
            p = &LBUF_BUF(lbuf)[offset];
            /* FIXME: support stateful encoding */
            p = SCM_CHARCODEC_INT2STR(codec, p, c, SCM_MB_STATELESS);
            if (!p)
                ERR("invalid char in string: 0x~MX", (scm_int_t)c);
            break;
        }
#if !SCM_USE_NULL_CAPABLE_STRING
        if (c == '\0')
            ERR(SCM_ERRMSG_NULL_IN_STRING);
#endif
    }
    LBUF_END(lbuf)[-1] = '\0';
    ERR("too long string: \"~S\"", LBUF_BUF(lbuf));
    /* NOTREACHED */
}
#endif /* SCM_USE_STRING */

static ScmObj
read_symbol(ScmObj port)
{
    ScmObj sym;
    size_t offset, tail_len;
    int err;
    ScmLBuf(char) lbuf;
    char init_buf[SCM_INITIAL_SYMBOL_BUF_SIZE];

    CDBG((SCM_DBG_PARSER, "read_symbol"));

    LBUF_INIT(lbuf, init_buf, sizeof(init_buf));

    for (offset = 0;;) {
        tail_len = read_token(port, &err,
                              &LBUF_BUF(lbuf)[offset],
                              LBUF_SIZE(lbuf) - offset,
                              SCM_CH_DELIMITER);
        if (err != TOKEN_BUF_EXCEEDED)
            break;
        offset += tail_len;
        LBUF_EXTEND(lbuf, SCM_LBUF_F_SYMBOL, LBUF_SIZE(lbuf) + MB_MAX_SIZE);
    }

    sym = scm_intern(LBUF_BUF(lbuf));
    LBUF_FREE(lbuf);

    return sym;
}

static ScmObj
read_number_or_symbol(ScmObj port)
{
    scm_ichar_t c;
    int err;
    size_t len;
    char buf[INT_LITERAL_LEN_MAX + sizeof("")];
    DECLARE_INTERNAL_FUNCTION("read");

    CDBG((SCM_DBG_PARSER, "read"));

    c = scm_port_peek_char(port);
    SCM_ASSERT(ICHAR_ASCII_CLASS(c)
               & (SCM_CH_DIGIT | SCM_CH_PECULIAR_IDENTIFIER_CAND));

#if SCM_USE_NUMBER
    if (ICHAR_NUMERICP(c))
        return read_number(port, 'd');

    if (c == '+' || c == '-') {
        len = read_token(port, &err, buf, sizeof(buf), SCM_CH_DELIMITER);
        if (err == TOKEN_BUF_EXCEEDED)
            ERR("invalid number literal");


        if (!buf[1]                           /* '+' or '-' */
#if !SCM_STRICT_R5RS
            /* FIXME: Obsolete with SRFI-75 style '|-sym| */
            || (c == '-' && ICHAR_ALPHABETICP(buf[1]))  /* '-sym' */
#endif
            )
        {
            return scm_intern(buf);
        }

        return parse_number(port, buf, sizeof(buf), 'd');
    }
#endif /* SCM_USE_NUMBER */

    if (c == '.') {
        read_token(port, &err, buf, sizeof(buf), SCM_CH_DELIMITER);
        if (strcmp(buf, "...") == 0)
            return scm_intern(buf);
        /* TODO: support numeric expressions when the numeric tower is
           implemented */
        ERR("invalid identifier: ~S", buf);
    }

    if (c == '@')
        ERR("invalid identifier starting with @");

    return read_symbol(port);
}

#if SCM_USE_NUMBER
/* reads 'b123' part of #b123 */
static ScmObj
parse_number(ScmObj port, char *buf, size_t buf_size, char prefix)
{
    scm_int_t number;
    int radix;
    scm_bool err;
    DECLARE_INTERNAL_FUNCTION("read");

    switch (prefix) {
    case 'b': radix = 2;  break;
    case 'o': radix = 8;  break;
    case 'd': radix = 10; break;
    case 'x': radix = 16; break;
    default:
        goto err;
    }

    number = scm_string2number(buf, radix, &err);
    if (!err)
        return MAKE_INT(number);

 err:
    ERR("ill-formatted number: #~C~S", (scm_ichar_t)prefix, buf);
}

static ScmObj
read_number(ScmObj port, char prefix)
{
    int err;
    size_t len;
    char buf[INT_LITERAL_LEN_MAX + sizeof("")];
    DECLARE_INTERNAL_FUNCTION("read");

    len = read_token(port, &err, buf, sizeof(buf), SCM_CH_DELIMITER);
    if (err == TOKEN_BUF_EXCEEDED)
        ERR("invalid number literal");

    return parse_number(port, buf, sizeof(buf), prefix);
}
#endif /* SCM_USE_NUMBER */

static ScmObj
read_quote(ScmObj port, ScmObj quoter)
{
    return SCM_LIST_2(quoter, read_sexpression(port));
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.2 Input
===========================================================================*/
SCM_EXPORT ScmObj
scm_p_read(ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("read", procedure_variadic_0);

    port = scm_prepare_port(args, scm_in);
    return scm_read(port);
}
