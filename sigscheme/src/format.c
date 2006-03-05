/*===========================================================================
 *  FileName : format.c
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

#include "config.h"

/*=======================================
  System Include
=======================================*/
#include <stddef.h>
#include <stdarg.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"
#if SCM_USE_MULTIBYTE_CHAR
#include "encoding.h"
#endif

/*=======================================
  File Local Macro Definitions
=======================================*/
#define PRETTY_PRINT_PROCEDURE_NAME "pretty-print"

/* FIXME */
#define MSG_SRFI48_DIRECTIVE_HELP \
    "\n" \
    "\n" \
    "\n"

#if SCM_USE_SSCM_FORMAT_EXTENSION
/* FIXME */
#define MSG_SSCM_DIRECTIVE_HELP \
    "\n" \
    "\n" \
    "\n"
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
  Variable Declarations
=======================================*/
static scm_bool initialized;
static ScmObj sym_pretty_print;

/*=======================================
  File Local Function Declarations
=======================================*/
#if SCM_USE_MULTIBYTE_CHAR
static scm_ichar_t format_str_peek(ScmMultibyteString mbs_fmt,
                                   const char *caller);
#endif
static signed char read_number(format_string_t *fmt);
static ScmValueFormat read_number_prefix(enum ScmFormatCapability fcap,
                                         format_string_t *fmt);
static void format_int(ScmObj port,
                       ScmValueFormat vfmt, uintmax_t n, int radix);
#if SCM_USE_RAW_C_FORMAT
static scm_bool format_raw_c_directive(ScmObj port,
                                       format_string_t *fmt, va_list *args);
#endif
#if SCM_USE_SRFI28
static scm_bool format_directive(ScmObj port, scm_ichar_t prev_ch,
                                 enum ScmFormatCapability fcap,
                                 format_string_t *fmt,
                                 struct format_args *args);
#endif
static ScmObj format_internal(ScmObj port, enum ScmFormatCapability fcap,
                              const char *fmt, struct format_args *args);

/*=======================================
  Function Implementations
=======================================*/
void
scm_init_format(void)
{
    if (!initialized) {
        scm_gc_protect_with_init(&sym_pretty_print,
                                 scm_intern(PRETTY_PRINT_PROCEDURE_NAME));
        initialized = scm_true;
    }
}

#if SCM_USE_MULTIBYTE_CHAR
static scm_ichar_t
format_str_peek(ScmMultibyteString mbs_fmt, const char *caller)
{
    return scm_charcodec_read_char(scm_current_char_codec, &mbs_fmt, caller);
}
#endif /* SCM_USE_MULTIBYTE_CHAR */

void
scm_pretty_print(ScmObj port, ScmObj obj)
{
    ScmObj proc_pretty_print;

    proc_pretty_print = scm_symbol_value(sym_pretty_print,
                                         SCM_INTERACTION_ENV);
    if (!EQ(proc_pretty_print, SCM_UNBOUND))
        scm_call(proc_pretty_print, LIST_1(obj));
    else
        scm_write(port, obj);
}

static signed char
read_number(format_string_t *fmt)
{
    scm_ichar_t c;
    scm_int_t ret;
    scm_bool err;
    char *bufp;
    char buf[sizeof("99")];
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
    vfmt.width = read_number(fmt);
    c = FORMAT_STR_PEEK(*fmt);

    if (c == ',') {
        FORMAT_STR_SKIP_CHAR(*fmt);
        vfmt.frac_width = read_number(fmt);
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
/* ([CP]|(0?[0-9]+(,0?[0-9]+)?)?(S|([MWQLGJTZ]?[UDXOB]))) */
static scm_bool
format_raw_c_directive(ScmObj port, format_string_t *fmt, va_list *args)
{
    const void *orig_pos;
    scm_ichar_t c;
    uintmax_t n;  /* FIXME: sign extension */
    int radix;
    scm_bool modifiedp;
    ScmValueFormat vfmt;
    DECLARE_INTERNAL_FUNCTION("internal format");

    orig_pos = FORMAT_STR_POS(*fmt);

    c = FORMAT_STR_PEEK(*fmt);
    switch (c) {
    case 'C': /* Character */
        FORMAT_STR_SKIP_CHAR(*fmt);
        scm_port_put_char(port, va_arg(*args, scm_ichar_t));
        return scm_true;

    case 'P': /* Pointer */
        FORMAT_STR_SKIP_CHAR(*fmt);
        scm_port_puts(port, "0x");
        SCM_VALUE_FORMAT_INIT4(vfmt, sizeof(void *) * CHAR_BIT / 4,
                               -1, '0', scm_false);
        format_int(port, vfmt, (uintptr_t)va_arg(*args, void *), 16);
        return scm_true;

    default:
        break;
    }

    vfmt = read_number_prefix(SCM_FMT_RAW_C | SCM_FMT_SSCM_ADDENDUM, fmt);
    c = FORMAT_STR_PEEK(*fmt);
    if (c == 'S') { /* String */
        FORMAT_STR_SKIP_CHAR(*fmt);
        /* FIXME: reflect vfmt.width */
        scm_port_puts(port, va_arg(*args, const char *));
        return scm_true;
    }

    /* size modifiers (ordered by size) */
    modifiedp = scm_true;
    switch (c) {
    case 'W': /* int32_t */
        n = va_arg(*args, uint32_t);
        break;

    case 'M': /* scm_int_t */
        n = va_arg(*args, scm_uint_t);
        break;

    case 'L': /* long */
        n = va_arg(*args, unsigned long);
        break;

    case 'Q': /* int64_t */
        n = va_arg(*args, uint64_t);
        break;

    case 'J': /* intmax_t */
        n = va_arg(*args, uintmax_t);
        break;

    case 'T': /* ptrdiff_t */
        n = (uintmax_t)va_arg(*args, ptrdiff_t);
        break;

    case 'Z': /* size_t */
        n = va_arg(*args, size_t);
        break;

    default:
        modifiedp = scm_false;
        n = 0;  /* dummy to suppress warning */
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
        SCM_ASSERT(FORMAT_STR_POS(*fmt) == orig_pos);
        return scm_false;
    }
    FORMAT_STR_SKIP_CHAR(*fmt);
    if (!modifiedp)
        n = va_arg(*args, unsigned int);
    format_int(port, vfmt, n, radix);

    return scm_true;
}
#endif /* SCM_USE_RAW_C_FORMAT */

#if SCM_USE_SRFI28
static scm_bool
format_directive(ScmObj port, scm_ichar_t prev_ch,
                 enum ScmFormatCapability fcap,
                 format_string_t *fmt, struct format_args *args)
{
    const void *orig_pos;
    char directive;
#if SCM_USE_SRFI48
    ScmObj obj, indirect_fmt, indirect_args;
    scm_bool prefixedp;
    int radix;
    ScmValueFormat vfmt;
#endif
    DECLARE_INTERNAL_FUNCTION("format");

#if SCM_USE_SRFI48
    orig_pos = FORMAT_STR_POS(*fmt);
    vfmt = read_number_prefix(fcap, fmt);
    prefixedp = (FORMAT_STR_POS(*fmt) != orig_pos);
#endif /* SCM_USE_SRFI48 */
    directive = ICHAR_DOWNCASE(FORMAT_STR_PEEK(*fmt));

    if (fcap & SCM_FMT_SRFI28) {
        if (prefixedp)
            goto err_invalid_prefix;

        switch (directive) {
        case 'a': /* Any */
            scm_display(port, POP_FORMAT_ARG(args));
            goto fin;

        case 's': /* Slashified */
            scm_write(port, POP_FORMAT_ARG(args));
            goto fin;

        case '%': /* Newline */
            scm_port_newline(port);
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
        radix = -1;
        switch (directive) {
        case 'f': /* Fixed */
            obj = POP_FORMAT_ARG(args);
            if (STRINGP(obj)) {
                /* FIXME: reflect vfmt.width */
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

        if (prefixedp)
            goto err_invalid_prefix;

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
                        fcap, SCM_STRING_STR(indirect_fmt), indirect_args);
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
            if (prev_ch != NEWLINE_CHAR)
                scm_port_newline(port);
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
    ERR("invalid escape sequence: ~%c", directive);

 err_invalid_prefix:
    ERR("invalid prefix for directive ~%c", directive);

 fin:
    FORMAT_STR_SKIP_CHAR(*fmt);
    return scm_true;
}
#endif /* SCM_USE_SRFI28 */

static ScmObj
format_internal(ScmObj port, enum ScmFormatCapability fcap,
                const char *fmt, struct format_args *args)
{
    scm_ichar_t c, prev_c;
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

    prev_c = '\0';
    FORMAT_STR_INIT(cur, fmt);
    for (; !FORMAT_STR_ENDP(cur); prev_c = c) {
        c = FORMAT_STR_READ(cur);
        if (c == '~') {
#if SCM_USE_RAW_C_FORMAT
            if (fcap & SCM_FMT_RAW_C) {
                SCM_ASSERT(args->type == ARG_VA_LIST);
                if (format_raw_c_directive(port, &cur, &args->lst.va))
                    continue;
            }
#endif /* SCM_USE_RAW_C_FORMAT */
#if SCM_USE_SRFI28
            if (fcap & (SCM_FMT_SRFI28 | SCM_FMT_SRFI48 | SCM_FMT_SSCM)) {
                SCM_ASSERT(args->type == ARG_VA_LIST
                           || args->type == ARG_SCM_LIST);
                if (format_directive(port, prev_c, fcap, &cur, args))
                    continue;
            }
#endif /* SCM_USE_SRFI28 */
            SCM_ASSERT(scm_false);
        } else {
            scm_port_put_char(port, c);
        }
    }

    if (args->type == ARG_SCM_LIST)
        ENSURE_NO_MORE_ARG(args->lst.scm);
    return (implicit_portp) ? scm_p_srfi6_get_output_string(port) : SCM_UNDEF;
}

ScmObj
scm_lformat(ScmObj port,
            enum ScmFormatCapability fcap, const char *fmt, ScmObj scm_args)
{
    struct format_args args;

    args.type = ARG_SCM_LIST;
    args.lst.scm = scm_args;
    return format_internal(port, fcap, fmt, &args);
}

ScmObj
scm_vformat(ScmObj port,
            enum ScmFormatCapability fcap, const char *fmt, va_list c_args)
{
    struct format_args args;

    args.type = ARG_VA_LIST;
    args.lst.va = c_args;
    return format_internal(port, fcap, fmt, &args);
}

ScmObj
scm_format(ScmObj port, enum ScmFormatCapability fcap, const char *fmt, ...)
{
    va_list args;
    ScmObj ret;

    va_start(args, fmt);
    ret = scm_vformat(port, fcap, fmt, args);
    va_end(args);

    return ret;
}
