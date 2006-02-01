/*===========================================================================
 *  FileName : read.c
 *  About    : S-Expression reader
 *
 *  Copyright (C) 2000-2001 Shiro Kawai <shiro AT acm.org>
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

#include "config.h"

/*=======================================
  System Include
=======================================*/
#include <limits.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/
enum LexerState {
    LEX_ST_NORMAL,
    LEX_ST_COMMENT
};

/*=======================================
  File Local Macro Declarations
=======================================*/
#define OK 0
#define TOKEN_BUF_EXCEEDED (-1)

#define MB_MAX_SIZE (SCM_MB_MAX_LEN + sizeof(""))

/* can accept "backspace" of R5RS and "U0010FFFF" of SRFI-75 */
#define CHAR_LITERAL_LEN_MAX (sizeof("backspace") - sizeof(""))

/* #b-010101... */
#define INT_LITERAL_LEN_MAX (sizeof((char)'-') + SCM_INT_BITS)

#define WHITESPACE_CHARS " \t\n\r\v\f"
#define DELIMITER_CHARS  "()\";" WHITESPACE_CHARS

#define DISCARD_LOOKAHEAD(port) (scm_port_get_char(port))

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static scm_ichar_t skip_comment_and_space(ScmObj port);
static void   read_sequence(ScmObj port, char *buf, int len);
static size_t read_token(ScmObj port, int *err,
                         char *buf, size_t buf_size, const char *delim);

static ScmObj read_sexpression(ScmObj port);
static ScmObj read_list(ScmObj port, scm_ichar_t closeParen);
#if SCM_USE_SRFI75
static scm_ichar_t parse_unicode_sequence(const char *seq, int len);
static scm_ichar_t read_unicode_sequence(ScmObj port, char prefix);
#endif
static ScmObj read_char(ScmObj port);
static ScmObj read_string(ScmObj port);
static ScmObj read_symbol(ScmObj port);
static ScmObj read_number_or_symbol(ScmObj port);
static ScmObj parse_number(ScmObj port,
                           char *buf, size_t buf_size, char prefix);
static ScmObj read_number(ScmObj port, char prefix);
static ScmObj read_quote(ScmObj port, ScmObj quoter);

/*=======================================
  Function Implementations
=======================================*/
/*===========================================================================
  S-Expression Parser
===========================================================================*/
ScmObj
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

ScmObj
scm_read_char(ScmObj port)
{
    DECLARE_INTERNAL_FUNCTION("scm_read_char");

    ENSURE_PORT(port);

    return read_char(port);
}


static int
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
            else if (!isascii(c) || !isspace(c) || c == EOF)
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
        if (!isascii(c))
            ERR("unexpected non-ASCII char");
        *p = c;
    }
    buf[len] = '\0';
}

static size_t
read_token(ScmObj port,
           int *err, char *buf, size_t buf_size, const char *delim)
{
    ScmCharCodec *codec;
    scm_ichar_t c;
    size_t len;
    char *p;
    DECLARE_INTERNAL_FUNCTION("read");

    for (p = buf;;) {
        c = scm_port_peek_char(port);
        CDBG((SCM_DBG_PARSER, "c = %c", (int)c));

        if (p == buf) {
            if (c == EOF)
                ERR("unexpected EOF at a token");
        } else {
            if (strchr(delim, c) || c == EOF) {
                *err = OK;
                break;
            }
        }

        if (isascii(c)) {
            if (p == &buf[buf_size - sizeof("")]) {
                *err = TOKEN_BUF_EXCEEDED;
                break;
            }
            *p++ = c;
        } else {
#if SCM_USE_SRFI75
            if (&buf[buf_size] <= p + SCM_MB_MAX_LEN) {
                *err = TOKEN_BUF_EXCEEDED;
                break;
            }
            codec = scm_port_codec(port);
            if (SCM_CHARCODEC_CCS(codec) != SCM_CCS_UCS4)
                ERR("non-ASCII char in token on a non-Unicode port: 0x%x",
                    (int)c);
            /* canonicalize internal Unicode encoding */
            p = SCM_CHARCODEC_INT2STR(scm_identifier_codec, p, c,
                                      SCM_MB_STATELESS);
#else
            ERR("non-ASCII char in token: 0x%x", (int)c);
#endif
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
    scm_ichar_t c;
    DECLARE_INTERNAL_FUNCTION("read");

    CDBG((SCM_DBG_PARSER, "read_sexpression"));

    for (;;) {
        c = skip_comment_and_space(port);

        CDBG((SCM_DBG_PARSER, "read_sexpression c = %c", (int)c));

        /* case labels are ordered by appearance rate and penalty cost */
        switch (c) {
        case '(':
            DISCARD_LOOKAHEAD(port);
            return read_list(port, ')');

        case '\"':
            DISCARD_LOOKAHEAD(port);
            return read_string(port);

        case '\'':
            DISCARD_LOOKAHEAD(port);
            return read_quote(port, SYM_QUOTE);

        case '#':
            DISCARD_LOOKAHEAD(port);
            c = scm_port_get_char(port);
            switch (c) {
            case 't':
                return SCM_TRUE;
            case 'f':
                return SCM_FALSE;
            case '(':
                ret = scm_p_list2vector(read_list(port, ')'));
#if SCM_CONST_VECTOR_LITERAL
                SCM_VECTOR_SET_IMMUTABLE(ret);
#endif
                return ret;
            case '\\':
                return read_char(port);
            case 'b': case 'o': case 'd': case 'x':
                return read_number(port, c);
            case EOF:
                ERR("EOF in #");
            default:
                ERR("Unsupported # notation: %c", (int)c);
            }
            break;

        case '`':
            DISCARD_LOOKAHEAD(port);
            return read_quote(port, SYM_QUASIQUOTE);

        case ',':
            DISCARD_LOOKAHEAD(port);
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

        case '.': case '+': case '-':
        case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9': case '0':
        case '@':
            return read_number_or_symbol(port);

        case ')':
            ERR("invalid close parenthesis");
            /* NOTREACHED */

        case EOF:
            return SCM_EOF;

        default:
            return read_symbol(port);
        }
    }
}

static ScmObj
read_list(ScmObj port, scm_ichar_t closeParen)
{
    ScmObj lst, elm, cdr;
    ScmQueue q;
    ScmBaseCharPort *basecport;
    scm_ichar_t c;
    int err, start_line, cur_line;
    char dot_buf[sizeof("...")];
    DECLARE_INTERNAL_FUNCTION("read");

    CDBG((SCM_DBG_PARSER, "read_list"));
    basecport = SCM_PORT_TRY_DYNAMIC_CAST(ScmBaseCharPort,
                                          SCM_PORT_IMPL(port));
    start_line = (basecport) ? ScmBaseCharPort_line_number(basecport) : -1;

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

        CDBG((SCM_DBG_PARSER, "read_list c = [%c]", (int)c));

        if (c == EOF) {
            if (basecport) {
                cur_line = ScmBaseCharPort_line_number(basecport);
                ERR("EOF inside list at line %d (starting from line %d)",
                    cur_line, start_line);
            } else {
                ERR("EOF inside list");
            }
        } else if (c == closeParen) {
            DISCARD_LOOKAHEAD(port);
            return lst;
        } else if (c == '.') {
            /* Since expressions that beginning with a dot are limited to '.',
             * '...' and numbers in R5RS (See "7.1.1 Lexical structure"), fixed
             * size buffer can safely buffer them.
             */
            read_token(port, &err, dot_buf, sizeof(dot_buf), DELIMITER_CHARS);

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
                if (!strchr(WHITESPACE_CHARS, c))
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
    if (len < 3 || !isxdigit(seq[1]))
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
        ERR("invalid Unicode value");

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
    if (strchr(DELIMITER_CHARS, next) || next == EOF)
        return MAKE_CHAR(c);
#if SCM_USE_SRFI75
    else if (!isascii(c))
        ERR("invalid character literal");
#endif

    buf[0] = c;
    len = read_token(port, &err, &buf[1], sizeof(buf) - 1, DELIMITER_CHARS);
    if (err == TOKEN_BUF_EXCEEDED)
        ERR("invalid character literal");

    CDBG((SCM_DBG_PARSER, "read_char: ch = %s", buf));

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
        /* FIXME: make strcasecmp(3) portable */
        if (strcasecmp(buf, info->lex_rep) == 0)
            return MAKE_CHAR(info->code);
    }
    ERR("invalid character literal: #\\%s", buf);
}

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

        CDBG((SCM_DBG_PARSER, "read_string c = %c", (int)c));

        switch (c) {
        case EOF:
            LBUF_EXTEND(lbuf, SCM_LBUF_F_STRING, offset + 1);
            *p = '\0';
            ERR("EOF in string: \"%s<eof>", LBUF_BUF(lbuf));
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
                    ERR("invalid Unicode sequence in string: 0x%x", (int)c);
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
            ERR("invalid escape sequence in string: \\%c", (int)c);
        found:
            break;

        default:
            LBUF_EXTEND(lbuf, SCM_LBUF_F_STRING, offset + MB_MAX_SIZE);
            p = &LBUF_BUF(lbuf)[offset];
            /* FIXME: support stateful encoding */
            p = SCM_CHARCODEC_INT2STR(codec, p, c, SCM_MB_STATELESS);
            if (!p)
                ERR("invalid char in string: 0x%x", (int)c);
            break;
        }
#if !SCM_USE_NULL_CAPABLE_STRING
        if (c == '\0')
            ERR(SCM_ERRMSG_NULL_IN_STRING);
#endif
    }
    LBUF_END(lbuf)[-1] = '\0';
    ERR("too long string: \"%s\"", LBUF_BUF(lbuf));
    /* NOTREACHED */
}

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
                              DELIMITER_CHARS);
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

    if (isascii(c)) {
        if (isdigit(c))
            return read_number(port, 'd');

        if (c == '+' || c == '-') {
            len = read_token(port, &err, buf, sizeof(buf), DELIMITER_CHARS);
            if (err == TOKEN_BUF_EXCEEDED)
                ERR("invalid number literal");

            
            if (!buf[1]                           /* '+' or '-' */
#if !SCM_STRICT_R5RS
                || (c == '-' && isalpha(buf[1]))  /* '-sym' */
#endif
                )
            {
                return scm_intern(buf);
            }

            return parse_number(port, buf, sizeof(buf), 'd');
        }

        if (c == '.') {
            read_token(port, &err, buf, sizeof(buf), DELIMITER_CHARS);
            if (strcmp(buf, "...") == 0)
                return scm_intern(buf);
            /* TODO: support numeric expressions when the numeric tower is
               implemented */
            ERR("invalid identifier: %s", buf);
        }

        if (c == '@')
            ERR("invalid identifier: %s", buf);
    }

    return read_symbol(port);
}

/* reads 'b123' part of #b123 */
static ScmObj
parse_number(ScmObj port, char *buf, size_t buf_size, char prefix)
{
    scm_int_t number;
    int radix;
    char *end;
    DECLARE_INTERNAL_FUNCTION("read");

    switch (prefix) {
    case 'b': radix = 2;  break;
    case 'o': radix = 8;  break;
    case 'd': radix = 10; break;
    case 'x': radix = 16; break;
    default:
        goto err;
    }

    number = strtol(buf, &end, radix);
    if (*end)
        goto err;

    return MAKE_INT(number);

 err:
    ERR("ill-formatted number: #%c%s", (int)prefix, buf);
}

static ScmObj
read_number(ScmObj port, char prefix)
{
    int err;
    size_t len;
    char buf[INT_LITERAL_LEN_MAX + sizeof("")];
    DECLARE_INTERNAL_FUNCTION("read");

    len = read_token(port, &err, buf, sizeof(buf), DELIMITER_CHARS);
    if (err == TOKEN_BUF_EXCEEDED)
        ERR("invalid number literal");

    return parse_number(port, buf, sizeof(buf), prefix);
}

static ScmObj
read_quote(ScmObj port, ScmObj quoter)
{
    return SCM_LIST_2(quoter, read_sexpression(port));
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.2 Input
===========================================================================*/
ScmObj
scm_p_read(ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("read", procedure_variadic_0);

    port = scm_prepare_port(args, scm_in);
    return scm_read(port);
}
