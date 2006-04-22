/*===========================================================================
 *  Filename : format.c
 *  About    : Format strings
 *
 *  Copyright (C) 2006 YamaKen <yamaken AT bp.iij4u.or.jp>
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

/* The help strings (MSG_SRFI48_DIRECTIVE_HELP and MSG_SSCM_DIRECTIVE_HELP) are
 * derived from the reference implementation of SRFI-48. Here is the copyright
 * for the strings. No other part is covered by this copyright.
 *   -- 2006-03-18 YamaKen */
/*
 * Copyright (C) Kenneth A Dickey (2003). All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include "config.h"

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"
#if SCM_USE_MULTIBYTE_CHAR
#include "encoding.h"
#endif

/*=======================================
  File Local Macro Definitions
=======================================*/
#define PRETTY_PRINT_PROCEDURE_NAME "pretty-print"

#define MSG_SRFI48_DIRECTIVE_HELP                                             \
"(format [<port>] <format-string> [<arg>...])\n"                              \
"  - <port> is #t, #f or an output-port\n"                                    \
"  - any escape sequence is case insensitive\n"                               \
"\n"                                                                          \
"SEQ   MNEMONIC        DESCRIPTION\n"                                         \
"~H    [Help]          output this text\n"                                    \
"~A    [Any]           (display arg) for humans\n"                            \
"~S    [Slashified]    (write arg) for parsers\n"                             \
"~W    [WriteCircular] like ~s but outputs with write/ss\n"                   \
"~~    [Tilde]         output a tilde\n"                                      \
"~T    [Tab]           output a tab character\n"                              \
"~%    [Newline]       output a newline character\n"                          \
"~&    [Freshline]     output a newline if the previous output was not a newline\n" \
"~D    [Decimal]       the arg is a number which is output in decimal radix\n" \
"~X    [heXadecimal]   the arg is a number which is output in hexdecimal radix\n" \
"~O    [Octal]         the arg is a number which is output in octal radix\n"  \
"~B    [Binary]        the arg is a number which is output in binary radix\n" \
"~F\n"                                                                        \
"~wF   [Fixed]         the arg is a string or number which has width w and\n" \
"~w,dF                 d digits after the decimal\n"                          \
"~C    [Character]     charater arg is output by write-char\n"                \
"~_    [Space]         a single space character is output\n"                  \
"~Y    [Yuppify]       the list arg is pretty-printed to the output\n"        \
"~?    [Indirection]   recursive format: next 2 args are format-string and list\n" \
"                      of arguments\n"                                        \
"~K    [Indirection]   same as ~?\n"

#if SCM_USE_SSCM_FORMAT_EXTENSION
#define MSG_SSCM_DIRECTIVE_HELP                                              \
"(format+ [<port>] <format-string> [<arg>...])\n"                            \
"  - <port> is #t, #f or an output-port\n"                                   \
"  - any escape sequence is case insensitive\n"                              \
"\n"                                                                         \
"  The format+ procedure is a SigScheme-specific superset of SRFI-48.\n"     \
"  Following directives accept optional width w and d digits after the decimal,\n" \
"  and w accepts leading zero as zero-digit-padding specifier. All other rules\n" \
"  are same as SRFI-48. See also the help message for SRFI-48.\n"            \
"\n"                                                                         \
"SEQ        MNEMONIC       DESCRIPTION\n"                                    \
"~[w[,d]]D  [Decimal]      the arg is a number output in decimal radix\n"    \
"~[w[,d]]X  [heXadecimal]  the arg is a number output in hexdecimal radix\n" \
"~[w[,d]]O  [Octal]        the arg is a number output in octal radix\n"      \
"~[w[,d]]B  [Binary]       the arg is a number output in binary radix\n"     \
"~[w[,d]]F  [Fixed]        the arg is a string or number\n"
#endif /* SCM_USE_SSCM_FORMAT_EXTENSION */

#define NEWLINE_CHAR                                                         \
    (SCM_NEWLINE_STR[sizeof(SCM_NEWLINE_STR) - sizeof("") - 1])

/*=======================================
  File Local Type Definitions
=======================================*/
/* To allow non-ASCII string such as UCS2, format string is abstracted. */
#if SCM_USE_MULTIBYTE_CHAR
typedef ScmMultibyteString format_string_t;

#define FORMAT_STR_INIT(mbs_fmt, str)                                        \
    SCM_MBS_INIT2((mbs_fmt), (str), strlen(str))

#define FORMAT_STR_POS(mbs_fmt)   (SCM_MBS_GET_STR(mbs_fmt))

#define FORMAT_STR_ENDP(mbs_fmt)  (!SCM_MBS_GET_SIZE(mbs_fmt))

#define FORMAT_STR_READ(mbs_fmt)                                             \
    (SCM_CHARCODEC_READ_CHAR(scm_current_char_codec, (mbs_fmt)))

#define FORMAT_STR_PEEK(mbs_fmt)                                             \
    (format_str_peek((mbs_fmt), SCM_MANGLE(name)))

#else /* SCM_USE_MULTIBYTE_CHAR */

typedef const char *format_string_t;

#define FORMAT_STR_INIT(fmt, str) ((fmt) = (str))
#define FORMAT_STR_POS(fmt)       (fmt)
#define FORMAT_STR_ENDP(fmt)      (!*(fmt))
#define FORMAT_STR_READ(fmt)      (*(fmt)++)
#define FORMAT_STR_PEEK(fmt)      (*(fmt))
#endif /* SCM_USE_MULTIBYTE_CHAR */

#define FORMAT_STR_SKIP_CHAR(fmt) (FORMAT_STR_READ(fmt), 0)

enum format_arg_type {
    ARG_VA_LIST,
    ARG_SCM_LIST
};

struct format_args {
    enum format_arg_type type;
    union {
        va_list va;
        ScmObj scm;
    } lst;
};

#define POP_FORMAT_ARG(args)                                                 \
    (((args)->type == ARG_VA_LIST) ? va_arg((args)->lst.va, ScmObj)          \
                                   : MUST_POP_ARG((args)->lst.scm))

/*=======================================
  Variable Definitions
=======================================*/
SCM_GLOBAL_VARS_BEGIN(static_format);
#define static
static ScmObj l_sym_pretty_print;
#undef static
SCM_GLOBAL_VARS_END(static_format);
#define l_sym_pretty_print   SCM_GLOBAL_VAR(static_format, l_sym_pretty_print)
SCM_DEFINE_STATIC_VARS(static_format);

/*=======================================
  File Local Function Declarations
=======================================*/
#if SCM_USE_MULTIBYTE_CHAR
static scm_ichar_t format_str_peek(ScmMultibyteString mbs_fmt,
                                   const char *caller);
#endif
static signed char read_width(format_string_t *fmt);
static ScmValueFormat read_number_prefix(enum ScmFormatCapability fcap,
                                         format_string_t *fmt);
static void format_int(ScmObj port,
                       ScmValueFormat vfmt, uintmax_t n, int radix);
#if SCM_USE_RAW_C_FORMAT
static scm_ichar_t format_raw_c_directive(ScmObj port,
                                          format_string_t *fmt, va_list *args);
#endif
#if SCM_USE_SRFI28
static scm_ichar_t format_directive(ScmObj port, scm_ichar_t last_ch,
                                    enum ScmFormatCapability fcap,
                                    format_string_t *fmt,
                                    struct format_args *args);
#endif
static ScmObj format_internal(ScmObj port, enum ScmFormatCapability fcap,
                              const char *fmt, struct format_args *args);

/*=======================================
  Function Definitions
=======================================*/
SCM_EXPORT void
scm_init_format(void)
{
    SCM_GLOBAL_VARS_INIT(static_format);

    scm_gc_protect_with_init(&l_sym_pretty_print,
                             scm_intern(PRETTY_PRINT_PROCEDURE_NAME));
}

#if SCM_USE_MULTIBYTE_CHAR
static scm_ichar_t
format_str_peek(ScmMultibyteString mbs_fmt, const char *caller)
{
    return (FORMAT_STR_ENDP(mbs_fmt)) ? '\0' :
        scm_charcodec_read_char(scm_current_char_codec, &mbs_fmt, caller);
}
#endif /* SCM_USE_MULTIBYTE_CHAR */

SCM_EXPORT void
scm_pretty_print(ScmObj port, ScmObj obj)
{
    ScmObj proc_pretty_print;
    DECLARE_INTERNAL_FUNCTION("scm_pretty_print");

    /* FIXME: search pretty-print in current env */
    proc_pretty_print = SCM_SYMBOL_VCELL(l_sym_pretty_print);

    if (!EQ(proc_pretty_print, SCM_UNBOUND)) {
        ENSURE_PROCEDURE(proc_pretty_print);
        scm_call(proc_pretty_print, LIST_1(obj));
    } else {
        scm_write(port, obj);
    }
}

static signed char
read_width(format_string_t *fmt)
{
    scm_ichar_t c;
    scm_int_t ret;
    scm_bool err;
    char *bufp;
    char buf[sizeof("0127")];
    DECLARE_INTERNAL_FUNCTION("format");

    for (bufp = buf;
         (c = FORMAT_STR_PEEK(*fmt), ICHAR_NUMERICP(c))
             && bufp < &buf[sizeof(buf) - 1];
         FORMAT_STR_SKIP_CHAR(*fmt))
    {
        *bufp++ = c;
    }
    *bufp = '\0';
    ret = scm_string2number(buf, 10, &err);
    if (err)  /* empty case */
        ret = -1;

    if (ret > 127)
        ERR("too much column width: ~D", (int)ret);

    return ret;
}

static ScmValueFormat
read_number_prefix(enum ScmFormatCapability fcap, format_string_t *fmt)
{
    scm_ichar_t c;
    ScmValueFormat vfmt;
    DECLARE_INTERNAL_FUNCTION("format");

    SCM_VALUE_FORMAT_INIT(vfmt);
    c = FORMAT_STR_PEEK(*fmt);

    if (c == '0' && (fcap & SCM_FMT_LEADING_ZEROS)) {
        FORMAT_STR_SKIP_CHAR(*fmt);
        vfmt.pad = '0';
    }
    vfmt.width = read_width(fmt);
    c = FORMAT_STR_PEEK(*fmt);

    if (c == ',') {
        FORMAT_STR_SKIP_CHAR(*fmt);
        vfmt.frac_width = read_width(fmt);
    }

    return vfmt;
}

static void
format_int(ScmObj port, ScmValueFormat vfmt, uintmax_t n, int radix)
{
    char *str;

    str = scm_int2string(vfmt, n, radix);
    scm_port_puts(port, str);
    free(str);
}

#if SCM_USE_RAW_C_FORMAT
/* returns '\0' if no valid directive handled */
/* ([CP]|(0?[0-9]+(,0?[0-9]+)?)?(S|[MWQLGJTZ]?[UDXOB])) */
static scm_ichar_t
format_raw_c_directive(ScmObj port, format_string_t *fmt, va_list *args)
{
    format_string_t orig_fmt;
    const void *orig_pos;
    const char *str;
    scm_int_t cstr_len, str_len, i;
    scm_ichar_t c;
    intmax_t n;
    uintmax_t un;
    int radix;
    scm_bool modifiedp;
    ScmValueFormat vfmt;
    DECLARE_INTERNAL_FUNCTION("internal format");

    orig_fmt = *fmt;
    orig_pos = FORMAT_STR_POS(*fmt);

    c = FORMAT_STR_PEEK(*fmt);
    switch (c) {
    case 'C': /* Character */
        FORMAT_STR_SKIP_CHAR(*fmt);
        c = va_arg(*args, scm_ichar_t);
        scm_port_put_char(port, c);
        return (c == '\0') ? ' ' : c;

    case 'P': /* Pointer */
        FORMAT_STR_SKIP_CHAR(*fmt);
        scm_port_puts(port, "0x");
        SCM_VALUE_FORMAT_INIT4(vfmt, sizeof(void *) * CHAR_BIT / 4,
                               -1, '0', scm_false);
        format_int(port, vfmt, (uintptr_t)va_arg(*args, void *), 16);
        return c;

    default:
        break;
    }

    vfmt = read_number_prefix(SCM_FMT_RAW_C | SCM_FMT_SSCM_ADDENDUM, fmt);
    c = FORMAT_STR_PEEK(*fmt);
    if (c == 'S') { /* String */
        FORMAT_STR_SKIP_CHAR(*fmt);
        str = va_arg(*args, const char *);
        cstr_len = strlen(str);
#if SCM_USE_MULTIBYTE_CHAR
        str_len = scm_mb_bare_c_strlen(scm_current_char_codec, str);
#else
        str_len = cstr_len;
#endif
        for (i = str_len; i < vfmt.width; i++)
            scm_port_put_char(port, ' ');  /* ignore leading zero */
        scm_port_puts(port, str);
        return (*str) ? str[cstr_len - 1] : c;
    }

    /* size modifiers (ordered by size) */
    modifiedp = scm_true;
    switch (c) {
    case 'W': /* int32_t */
        un = va_arg(*args, uint32_t);
        n = (int32_t)un;
        break;

    case 'M': /* scm_int_t */
        un = va_arg(*args, scm_uint_t);
        n = (scm_int_t)un;
        break;

    case 'L': /* long */
        un = va_arg(*args, unsigned long);
        n = (long)un;
        break;

    case 'Q': /* int64_t */
        un = va_arg(*args, uint64_t);
        n = (int64_t)un;
        break;

    case 'J': /* intmax_t */
        un = va_arg(*args, uintmax_t);
        n = (intmax_t)un;
        break;

    case 'T': /* ptrdiff_t */
        un = (uintmax_t)va_arg(*args, ptrdiff_t);
        n = (ptrdiff_t)un;
        break;

    case 'Z': /* size_t */
        un = va_arg(*args, size_t);
        /* portable ssize_t replacement */
#if (SIZEOF_SIZE_T == SIZEOF_INTPTR_T)
        n = (intptr_t)un;
#elif (SIZEOF_SIZE_T == SIZEOF_INT32_T)
        n = (int32_t)un;
#elif (SIZEOF_SIZE_T == SIZEOF_INT64_T)
        n = (int64_t)un;
#elif (SIZEOF_SIZE_T == SIZEOF_INTMAX_T)
        n = (intmax_t)un;
#else
#error "This platform is not supported"
#endif
        break;

    default:
        modifiedp = scm_false;
        un = n = 0;  /* dummy to suppress warning */
        break;
    }
    if (modifiedp) {
        FORMAT_STR_SKIP_CHAR(*fmt);
        c = FORMAT_STR_PEEK(*fmt);
    }

    /* integer format specifiers */
    switch (c) {
    case 'U': /* Unsigned decimal */
        vfmt.signedp = scm_false;
        /* FALLTHROUGH */
    case 'D': /* Decimal */
        radix = 10;
        break;

    case 'X': /* unsigned heXadecimal */
        radix = 16;
        vfmt.signedp = scm_false;
        break;

    case 'O': /* unsigned Octal */
        radix = 8;
        vfmt.signedp = scm_false;
        break;

    case 'B': /* unsigned Binary */
        radix = 2;
        vfmt.signedp = scm_false;
        break;

    default:
        /* no internal directives found */
        if (FORMAT_STR_POS(*fmt) != orig_pos)
            *fmt = orig_fmt;
        return '\0';
    }
    FORMAT_STR_SKIP_CHAR(*fmt);
    if (!modifiedp) {
        un = va_arg(*args, unsigned int);
        n = (int)un;
    }
    format_int(port, vfmt, (vfmt.signedp) ? (uintmax_t)n : un, radix);

    return c;
}
#endif /* SCM_USE_RAW_C_FORMAT */

#if SCM_USE_SRFI28
static scm_ichar_t
format_directive(ScmObj port, scm_ichar_t last_ch,
                 enum ScmFormatCapability fcap,
                 format_string_t *fmt, struct format_args *args)
{
    const void *orig_pos;
    char directive;
    scm_bool eolp;
#if SCM_USE_SRFI48
    ScmObj obj, indirect_fmt, indirect_args;
    scm_bool prefixedp;
    int radix;
    scm_int_t i;
    ScmValueFormat vfmt;
#endif
    DECLARE_INTERNAL_FUNCTION("format");

#if SCM_USE_SRFI48
    orig_pos = FORMAT_STR_POS(*fmt);
    vfmt = read_number_prefix(fcap, fmt);
    prefixedp = (FORMAT_STR_POS(*fmt) != orig_pos);
#endif /* SCM_USE_SRFI48 */
    directive = ICHAR_DOWNCASE(FORMAT_STR_PEEK(*fmt));
    eolp = scm_false;

#if SCM_USE_SRFI48
    if (fcap & SCM_FMT_SRFI48_ADDENDUM) {
        radix = -1;
        switch (directive) {
        case 'f': /* Fixed */
            obj = POP_FORMAT_ARG(args);
            if (STRINGP(obj)) {
                for (i = SCM_STRING_LEN(obj); i < vfmt.width; i++)
                    scm_port_put_char(port, ' ');  /* ignore leading zero */
                scm_display(port, obj);
            } else {
                if (!INTP(obj))
                    ERR_OBJ("integer or string required but got", obj);
                format_int(port, vfmt, SCM_INT_VALUE(obj), 10);
            }
            goto fin;

        case 'd': /* Decimal */
            radix = 10;
            break;

        case 'x': /* heXadecimal */
            radix = 16;
            break;

        case 'o': /* Octal */
            radix = 8;
            break;

        case 'b': /* Binary */
            radix = 2;
            break;

        default:
            break;
        }
        if (radix > 0 && (!prefixedp || (fcap & SCM_FMT_PREFIXED_RADIX))) {
            obj = POP_FORMAT_ARG(args);
            ENSURE_INT(obj);
            format_int(port, vfmt, SCM_INT_VALUE(obj), radix);
            goto fin;
        }
    }
#endif /* SCM_USE_SRFI48 */

    if (prefixedp)
        ERR("invalid prefix for directive ~~~C", (scm_ichar_t)directive);

    if (fcap & SCM_FMT_SRFI28) {
        switch (directive) {
        case 'a': /* Any */
            scm_display(port, POP_FORMAT_ARG(args));
            goto fin;

        case 's': /* Slashified */
            scm_write(port, POP_FORMAT_ARG(args));
            goto fin;

        case '%': /* Newline */
            scm_port_newline(port);
            eolp = scm_true;
            goto fin;

        case '~': /* Tilde */
            scm_port_put_char(port, '~');
            goto fin;

        default:
            break;
        }
    }

#if SCM_USE_SRFI48
    if (fcap & SCM_FMT_SRFI48_ADDENDUM) {
        switch (directive) {
        case 'w': /* WriteCircular */
            scm_write_ss(port, POP_FORMAT_ARG(args));
            goto fin;

        case 'y': /* Yuppify */
            scm_pretty_print(port, POP_FORMAT_ARG(args));
            goto fin;

        case 'k': /* Indirection (backward compatability) */
        case '?': /* Indirection */
            indirect_fmt = POP_FORMAT_ARG(args);
            ENSURE_STRING(indirect_fmt);
            indirect_args = POP_FORMAT_ARG(args);
            ENSURE_LIST(indirect_args);
            scm_lformat(port,
                        fcap & ~SCM_FMT_RAW_C,
                        SCM_STRING_STR(indirect_fmt), indirect_args);
            goto fin;

        case 'c': /* Character */
            obj = POP_FORMAT_ARG(args);
            ENSURE_CHAR(obj);
            scm_port_put_char(port, SCM_CHAR_VALUE(obj));
            goto fin;

        case 't': /* Tab */
            scm_port_put_char(port, '\t');
            goto fin;

        case '_': /* Space */
            scm_port_put_char(port, ' ');
            goto fin;

        case '&': /* Freshline */
            if (last_ch != NEWLINE_CHAR)
                scm_port_newline(port);
            eolp = scm_true;
            goto fin;

        case 'h': /* Help */
#if SCM_USE_SSCM_FORMAT_EXTENSION
            if (fcap & SCM_FMT_SSCM_ADDENDUM)
                scm_port_puts(port, MSG_SSCM_DIRECTIVE_HELP);
            else
#endif
                scm_port_puts(port, MSG_SRFI48_DIRECTIVE_HELP);
            goto fin;

        default:
            break;
        }
    }
#endif /* SCM_USE_SRFI48 */

    /* Although SRFI-48 does not specified about unknown directives, the
     * reference implementation treats it as error. */
    ERR("invalid escape sequence: ~~~C", (scm_ichar_t)directive);

 fin:
    FORMAT_STR_SKIP_CHAR(*fmt);
    return (eolp) ? NEWLINE_CHAR : directive;
}
#endif /* SCM_USE_SRFI28 */

static ScmObj
format_internal(ScmObj port, enum ScmFormatCapability fcap,
                const char *fmt, struct format_args *args)
{
    scm_ichar_t c, last_c, handled;
    format_string_t cur;
    scm_bool implicit_portp;
    DECLARE_INTERNAL_FUNCTION("format");

    if (FALSEP(port)) {
        port = scm_p_srfi6_open_output_string();
        implicit_portp = scm_true;
    } else if (EQ(port, SCM_TRUE)) {
        port = scm_out;
        implicit_portp = scm_false;
    } else {
        if (!PORTP(port))
            ERR_OBJ("port or boolean required but got", port);
        implicit_portp = scm_false;
    }

    last_c = '\0';
    FORMAT_STR_INIT(cur, fmt); 
    while (!FORMAT_STR_ENDP(cur)) {
        c = FORMAT_STR_READ(cur);
        if (c == '~') {
#if SCM_USE_RAW_C_FORMAT
            if (fcap & SCM_FMT_RAW_C) {
                SCM_ASSERT(args->type == ARG_VA_LIST);
                handled = format_raw_c_directive(port, &cur, &args->lst.va);
                if (handled) {
                    last_c = handled;
                    continue;
                }
            }
            /* FALLTHROUGH */
#endif /* SCM_USE_RAW_C_FORMAT */
#if SCM_USE_SRFI28
            if (fcap & (SCM_FMT_SRFI28 | SCM_FMT_SRFI48 | SCM_FMT_SSCM)) {
                SCM_ASSERT(args->type == ARG_VA_LIST
                           || args->type == ARG_SCM_LIST);
                last_c = format_directive(port, last_c, fcap, &cur, args);
                continue;
            }
#endif /* SCM_USE_SRFI28 */
            SCM_ASSERT(scm_false);
        } else {
            scm_port_put_char(port, c);
            last_c = c;
        }
    }

    if (args->type == ARG_SCM_LIST)
        ENSURE_NO_MORE_ARG(args->lst.scm);
    return (implicit_portp) ? scm_p_srfi6_get_output_string(port) : SCM_UNDEF;
}

SCM_EXPORT ScmObj
scm_lformat(ScmObj port,
            enum ScmFormatCapability fcap, const char *fmt, ScmObj scm_args)
{
    struct format_args args;

    args.type = ARG_SCM_LIST;
    args.lst.scm = scm_args;
    return format_internal(port, fcap, fmt, &args);
}

SCM_EXPORT ScmObj
scm_vformat(ScmObj port,
            enum ScmFormatCapability fcap, const char *fmt, va_list c_args)
{
    struct format_args args;

    args.type = ARG_VA_LIST;
    args.lst.va = c_args;
    return format_internal(port, fcap, fmt, &args);
}

SCM_EXPORT ScmObj
scm_format(ScmObj port, enum ScmFormatCapability fcap, const char *fmt, ...)
{
    va_list args;
    ScmObj ret;

    va_start(args, fmt);
    ret = scm_vformat(port, fcap, fmt, args);
    va_end(args);

    return ret;
}
