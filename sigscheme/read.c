/*===========================================================================
 *  FileName : read.c
 *  About    : S-Expression reader
 *
 *  Copyright (C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *  Copyright (C) 2005      by Kazuki Ohta (mover@hct.zaq.ne.jp)
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
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 *  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
===========================================================================*/
/*
 * FIXME: Large fixed-size buffer on stack without limit checking
 *
 * Fix some functions contained in this file since:
 *
 * - danger
 * - some embedded platform cannot allocate such large stack (approx. 20KB)
 * - inefficient from the viewpoint of memory locality (cache, page, power
 *   consumption etc)
 *
 * Search "FIXME" on this file to locate the codes. I wonder other Scheme
 * implementations may have sophisticated code. Please consider porting them to
 * save development cost, since this part is not the primary value of
 * SigScheme.  -- YamaKen 2005-09-05
 */

/*
 * FIXME: Parse properly as defined in "7.1.1 Lexical structure" of R5RS, and
 * use the popular words for parser as used in R5RS, such as 'token'.
 */

/*=======================================
  System Include
=======================================*/
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
/* can accept "backspace" of R5RS and "U0010FFFF" of SRFI-75 */
#define CHAR_LITERAL_LEN_MAX (sizeof("backspace") - sizeof((char)'\0'))

#define INITIAL_STRING_BUF_SIZE 1024

#define WHITESPACE_CHARS " \t\n\r\v\f"
#define DELIMITER_CHARS  "()\"\';" WHITESPACE_CHARS

/* Compatible with isspace(3). Use this to prevent incorrect space handlings */
#define CASE_ISSPACE                                                         \
    case ' ': case '\t': case '\n': case '\r': case '\v': case '\f'

/* FIXME: discard at first of each reader instead of caller */
#define DISCARD_LOOKAHEAD(port) (SCM_PORT_GET_CHAR(port))

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static int    skip_comment_and_space(ScmObj port);
static void   read_sequence(ScmObj port, char *buf, int len);
static char*  read_word(ScmObj port);
static size_t read_token(ScmObj port,
                         char *buf, size_t buf_size, const char *delim);
#if SCM_USE_SRFI75
static int    parse_unicode_sequence(const char *seq, int len);
static int    read_unicode_sequence(ScmObj port, char prefix);
#endif

static ScmObj read_sexpression(ScmObj port);
static ScmObj read_list(ScmObj port, int closeParen);
static ScmObj read_char(ScmObj port);
static ScmObj read_string(ScmObj port);
static ScmObj read_symbol(ScmObj port);
static ScmObj parse_number(ScmObj port);
static ScmObj read_number_or_symbol(ScmObj port);
static ScmObj read_quote(ScmObj port, ScmObj quoter);

/*=======================================
  Function Implementations
=======================================*/
/*===========================================================================
  S-Expression Parser
===========================================================================*/
ScmObj SigScm_Read(ScmObj port)
{
    ScmObj sexp = SCM_FALSE;
    DECLARE_INTERNAL_FUNCTION("SigScm_Read");

    ASSERT_PORTP(port);

    sexp = read_sexpression(port);
#if SCM_DEBUG
    if ((SigScm_DebugCategories() & SCM_DBG_READ) && !EOFP(sexp)) {
        SigScm_WriteToPort(scm_current_error_port, sexp);
        SigScm_ErrorNewline();
    }
#endif

    return sexp;
}

ScmObj SigScm_Read_Char(ScmObj port)
{
    DECLARE_INTERNAL_FUNCTION("SigScm_Read_Char");

    ASSERT_PORTP(port);

    return read_char(port);
}


static int skip_comment_and_space(ScmObj port)
{
    int c, state;

    for (state = LEX_ST_NORMAL;;) {
        c = SCM_PORT_PEEK_CHAR(port);
        switch (state) {
        case LEX_ST_NORMAL:
            if (c == ';')
                state = LEX_ST_COMMENT;
            else if ((isascii(c) && !isspace(c)) || c == EOF)
                return c;  /* peeked */
            break;

        case LEX_ST_COMMENT:
            if (c == '\n' || c == '\r')
                state = LEX_ST_NORMAL;
            else if (c == EOF)
                return c;  /* peeked */
            break;
        }
        SCM_PORT_GET_CHAR(port);  /* skip the char */
    }
}

static void read_sequence(ScmObj port, char *buf, int len)
{
    int c;
    char *p;

    for (p = buf; p < &buf[len]; p++) {
        c = SCM_PORT_GET_CHAR(port);
        if (c == EOF)
            ERR("unexpected EOF");
        if (!isascii(c))
            ERR("unexpected non-ASCII char");
        *p = c;
    }
    buf[len] = '\0';
}

static ScmObj read_sexpression(ScmObj port)
{
    int c  = 0;
    int c1 = 0;

    CDBG((SCM_DBG_PARSER, "read_sexpression"));

    while (1) {
        c = skip_comment_and_space(port);

        CDBG((SCM_DBG_PARSER, "read_sexpression c = %c", c));

        switch (c) {
        case '(':
            DISCARD_LOOKAHEAD(port);
            return read_list(port, ')');
        case '\"':
            DISCARD_LOOKAHEAD(port);
            return read_string(port);
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
        case '+': case '-':
            SCM_PORT_UNGETC(port, c);
            return read_number_or_symbol(port);
        case '\'':
            DISCARD_LOOKAHEAD(port);
            return read_quote(port, SYM_QUOTE);
        case '`':
            DISCARD_LOOKAHEAD(port);
            return read_quote(port, SYM_QUASIQUOTE);
        case ',':
            DISCARD_LOOKAHEAD(port);
            c1 = SCM_PORT_PEEK_CHAR(port);
            if (c1 == EOF) {
                SigScm_Error("EOF in unquote");
            } else if (c1 == '@') {
                DISCARD_LOOKAHEAD(port);
                return read_quote(port, SYM_UNQUOTE_SPLICING);
            } else {
                SCM_PORT_UNGETC(port, c1);
                return read_quote(port, SYM_UNQUOTE);
            }
            break;
        case '#':
            DISCARD_LOOKAHEAD(port);
            c1 = SCM_PORT_PEEK_CHAR(port);
            switch (c1) {
            case 't': case 'T':
                DISCARD_LOOKAHEAD(port);
                return SCM_TRUE;
            case 'f': case 'F':
                DISCARD_LOOKAHEAD(port);
                return SCM_FALSE;
            case '(':
                DISCARD_LOOKAHEAD(port);
                return ScmOp_list2vector(read_list(port, ')'));
            case '\\':
                DISCARD_LOOKAHEAD(port);
                return read_char(port);
            case 'b': case 'o': case 'd': case 'x':
                SCM_PORT_UNGETC(port, c1);
                return parse_number(port);
            case EOF:
                SigScm_Error("end in #");
            default:
                SigScm_Error("Unsupported # : %c", c1);
            }
            break;
        /* Error sequence */
        case ')':
            SigScm_Error("invalid close parenthesis");
            break;
        case EOF:
            return SCM_EOF;
        default:
            SCM_PORT_UNGETC(port, c);
            return read_symbol(port);
        }
    }
}

static ScmObj read_list(ScmObj port, int closeParen)
{
    ScmObj list_head = SCM_NULL;
    ScmObj list_tail = SCM_NULL;
    ScmObj item   = SCM_NULL;
    ScmObj cdr    = SCM_NULL;
    ScmBaseCharPort *basecport;
    int    start_line, cur_line;
    int    c      = 0;
    int    c2     = 0;
    char  *token  = NULL;

    CDBG((SCM_DBG_PARSER, "read_list"));
    basecport = SCM_PORT_TRY_DYNAMIC_CAST(ScmBaseCharPort,
                                          SCM_PORT_IMPL(port));
    if (basecport)
        start_line = ScmBaseCharPort_line_number(basecport);

    while (1) {
        c = skip_comment_and_space(port);

        CDBG((SCM_DBG_PARSER, "read_list c = [%c]", c));

        if (c == EOF) {
            if (basecport) {
                cur_line = ScmBaseCharPort_line_number(basecport);
                ERR("EOF inside list at line %d. (starting from line %d)",
                    cur_line, start_line);
            } else {
                SigScm_Error("EOF inside list.");
            }
        } else if (c == closeParen) {
            DISCARD_LOOKAHEAD(port);
            return list_head;
        } else if (c == '.') {
            DISCARD_LOOKAHEAD(port);
            c2 = SCM_PORT_PEEK_CHAR(port);
            CDBG((SCM_DBG_PARSER, "read_list process_dot c2 = [%c]", c2));
            if ((isascii(c2) && isspace(c2))
                || c2 == '(' || c2 == '"' || c2 == ';') {
                DISCARD_LOOKAHEAD(port);
                cdr = read_sexpression(port);
                if (NULLP(list_tail))
                    SigScm_Error(".(dot) at the start of the list.");

                c = skip_comment_and_space(port);
                DISCARD_LOOKAHEAD(port);
                if (c != ')')
                    SigScm_Error("bad dot syntax");

                SET_CDR(list_tail, cdr);
                return list_head;
            }

            /*
             * This dirty hack here picks up the current token as a
             * symbol beginning with the dot (that's how Guile and
             * Gauche behave).
             */
            SCM_PORT_UNGETC(port, c2);
            token = read_word(port);
            token = (char*)realloc(token, strlen(token) + 1 + 1);
            memmove(token + 1, token, strlen(token)+1);
            token[0] = '.';
            item = Scm_Intern(token);
            free(token);
        } else {
            SCM_PORT_UNGETC(port, c);
            item = read_sexpression(port);
        }

        /* Append item to the list_tail. */
        if (NULLP(list_tail)) {
            /* create new list */
            list_head = CONS(item, SCM_NULL);
            list_tail = list_head;
        } else {
            /* update list_tail */
            SET_CDR(list_tail, CONS(item, SCM_NULL));
            list_tail = CDR(list_tail);
        }
    }
}

#if SCM_USE_SRFI75
static int parse_unicode_sequence(const char *seq, int len)
{
    int c;
    char *first_nondigit;

    /* reject ordinary char literal and invalid signed hexadecimal */
    if (len < 3 || !isxdigit(seq[1]))
        return -1;

    switch (seq[0]) {
    case 'x':
        /* #\x<x><x> : <x> = a hexadecimal digit (ignore case) */
        if (len != 3)
            ERR("invalid hexadecimal character sequence. conform \\x<x><x>");
        break;

    case 'u':
        /* #\u<x><x><x><x> : Unicode char of BMP */
        if (len != 5 || (0xd800 <= c && c <= 0xdfff))
            ERR("invalid Unicode sequence. conform \\u<x><x><x><x>");
        break;

    case 'U':
        /* #\U<x><x><x><x><x><x><x><x> : Unicode char of BMP or SMP */
        if (len != 8 || (0xd800 <= c && c <= 0xdfff) || 0x10ffff < c)
            ERR("invalid Unicode sequence. conform \\U<x><x><x><x><x><x><x><x>");
        break;

    default:
        return -1;
    }
    c = strtol(&seq[1], &first_nondigit, 16);
    return (*first_nondigit) ? -1 : c;
}

static int read_unicode_sequence(ScmObj port, char prefix)
{
    int len;
    char seq[sizeof("U0010ffff")];

    switch (prefix) {
    case 'x': len = 2; break;
    case 'u': len = 4; break;
    case 'U': len = 8; break;
    default:
        /* FIXME: add fatal error handling */
        break;
    }
    seq[0] = prefix;
    read_sequence(port, &seq[1], len);
    return parse_unicode_sequence(seq, len + sizeof(prefix));
}
#endif /* SCM_USE_SRFI75 */

static ScmObj read_char(ScmObj port)
{
    int c, next;
#if SCM_USE_SRFI75
    int unicode;
#endif
    const ScmSpecialCharInfo *info;
    size_t len;
    char buf[CHAR_LITERAL_LEN_MAX + sizeof((char)'\0')];

    /* plain char (multibyte-ready) */
    c = SCM_PORT_GET_CHAR(port);
    next = SCM_PORT_PEEK_CHAR(port);
    if (strchr(DELIMITER_CHARS, next) || next == EOF)
        return Scm_NewChar(c);
#if SCM_USE_SRFI75
    else if (!isascii(c))
        ERR("invalid character literal");
#endif

    buf[0] = c;
    len = read_token(port, &buf[1], sizeof(buf) - 1, DELIMITER_CHARS);
    CDBG((SCM_DBG_PARSER, "read_char : ch = %s", buf));

#if SCM_USE_SRFI75
    unicode = parse_unicode_sequence(buf, len + 1);
    if (0 <= unicode)
        return Scm_NewChar(unicode);
#endif
    /* named chars */
    for (info = Scm_special_char_table; info->esc_seq; info++) {
        if (strcmp(buf, info->lex_rep) == 0)
            return Scm_NewChar(info->code);
    }
    ERR("invalid character literal: #\%s", buf);
}

/* FIXME: extend buffer on demand */
static ScmObj read_string(ScmObj port)
{
    ScmObj obj;
    const ScmSpecialCharInfo *info;
    int c;
    size_t bufsize;
    char *p, *buf;
    char autobuf[INITIAL_STRING_BUF_SIZE];

    CDBG((SCM_DBG_PARSER, "read_string"));

    buf = autobuf;
    bufsize = sizeof(autobuf);
    for (p = buf; p < &buf[bufsize];) {
        c = SCM_PORT_GET_CHAR(port);

        CDBG((SCM_DBG_PARSER, "read_string c = %c", c));

        switch (c) {
        case EOF:
            *p = '\0';
            ERR("EOF in string: \"%s<eof>", buf);
            break;

        case '\"':
            *p = '\0';
            obj = Scm_NewImmutableStringCopying(buf);
            if (buf != autobuf)
                free(buf);
            return obj;

        case '\\':
            c = SCM_PORT_GET_CHAR(port);
#if SCM_USE_SRFI75
            if (strchr("xuU", c)) {
                c = read_unicode_sequence(port, c);
                /* FIXME: check Unicode capability of Scm_current_char_codec */
                p = SCM_CHARCODEC_INT2STR(Scm_current_char_codec,
                                          p, c, SCM_MB_STATELESS);
                if (!p)
                    ERR("invalid Unicode sequence in string: 0x%x", c);
                goto found;
            } else
#endif
            {
                /* escape sequences */
                for (info = Scm_special_char_table; info->esc_seq; info++) {
                    if (strlen(info->esc_seq) == 2 && c == info->esc_seq[1]) {
                        *p++ = info->code;
                        goto found;
                    }
                }
            }
            ERR("invalid escape sequence in string: \\%c", c);
        found:
            break;

        default:
            /* FIXME: support stateful encoding */
            p = SCM_CHARCODEC_INT2STR(Scm_current_char_codec,
                                      p, c, SCM_MB_STATELESS);
            if (!p)
                ERR("invalid char in string: 0x%x", c);
            break;
        }
    }
    buf[bufsize - 1] = '\0';
    ERR("too long string: \"%s\"", buf);
    /* NOTREACHED */
}

static ScmObj read_symbol(ScmObj port)
{
    char  *sym_name = read_word(port);
    ScmObj sym = Scm_Intern(sym_name);
    free(sym_name);

    CDBG((SCM_DBG_PARSER, "read_symbol"));

    return sym;
}

/*
 * FIXME: Parse properly as defined in "7.1.1 Lexical structure" of R5RS. For
 * example, 1+ is not a valid identifier and should be rejected to prevent
 * introducing unintended R5RS-incompatibility.
 */
static ScmObj read_number_or_symbol(ScmObj port)
{
    int number = 0;
    int str_len = 0;
    char *str = NULL;
    char *first_nondigit = NULL;
    ScmObj ret = SCM_NULL;

    CDBG((SCM_DBG_PARSER, "read_number_or_symbol"));

    /* read char sequence */
    str = read_word(port);
    str_len = strlen(str);

    /* see if it's a decimal integer */
    number = (int)strtol(str, &first_nondigit, 10);

    /* set return obj */
    ret = (*first_nondigit) ? Scm_Intern(str) : Scm_NewInt(number);

    /* free */
    free(str);

    return ret;
}


static char *read_word(ScmObj port)
{
    char  stringbuf[1024];  /* FIXME! */
    int   stringlen = 0;
    int   c = 0;
    char *dst = NULL;

    while (1) {
        c = SCM_PORT_PEEK_CHAR(port);

        CDBG((SCM_DBG_PARSER, "c = %c", c));

        switch (c) {
        case EOF: /* don't became an error for handling c-eval, like Scm_eval_c_string("some-symbol"); */
        case '(': case ')': case '\"': case '\'': case ';':
        CASE_ISSPACE:
            SCM_PORT_UNGETC(port, c);
            stringbuf[stringlen] = '\0';
            dst = strdup(stringbuf);
            return dst;

        default:
            DISCARD_LOOKAHEAD(port);
            stringbuf[stringlen++] = (char)c;
            break;
        }
    }
}

static size_t read_token(ScmObj port,
                         char *buf, size_t buf_size, const char *delim)
{
    int c;
    size_t len;
    char *p;

    for (p = buf; p < &buf[buf_size];) {
        c = SCM_PORT_PEEK_CHAR(port);
        CDBG((SCM_DBG_PARSER, "c = %c", c));

        if (p == buf) {
            if (c == EOF)
                ERR("unexpected EOF at a token");
        } else {
            if (strchr(delim, c) || c == EOF) {
                *p = '\0';
                len = p - buf;
                return len;
            }
        }

        DISCARD_LOOKAHEAD(port);
        if (isascii(c)) {
            *p++ = c;
        } else {
#if SCM_USE_SRFI75
            /* FIXME: check Unicode capability of Scm_current_char_codec */
            p = SCM_CHARCODEC_INT2STR(Scm_current_char_codec,
                                      p, c, SCM_MB_STATELESS);
#else
            ERR("non-ASCII char in token: 0x%x", c);
#endif
        }
    }
    ERR("token buffer exceeded");
}

static ScmObj read_quote(ScmObj port, ScmObj quoter)
{
    return SCM_LIST_2(quoter, read_sexpression(port));
}

/* str should be what appeared right after '#' (eg. #b123) */
static ScmObj parse_number(ScmObj port)
{
    int radix  = 0;
    int number = 0;
    char *first_nondigit = NULL;
    char *numstr = read_word(port);

    switch (numstr[0]) {
    case 'b': radix = 2;  break;
    case 'o': radix = 8;  break;
    case 'd': radix = 10; break;
    case 'x': radix = 16; break;
    default:
        SigScm_Error("ill-formatted number: #%s", numstr);
    }

    /* get num */
    number = (int)strtol(numstr+1, &first_nondigit, radix);
    if (*first_nondigit)
        SigScm_Error("ill-formatted number: #%s", numstr);

    /* free str */
    free(numstr);

    return Scm_NewInt(number);
}
