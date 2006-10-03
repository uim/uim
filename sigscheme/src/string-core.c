/*===========================================================================
 *  Filename : string-core.c
 *  About    : Core procedures of R5RS strings
 *
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

#include <config.h>

#include <string.h>
#include <stdlib.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Definitions
=======================================*/

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Definitions
=======================================*/
/*===========================================================================
  R5RS : 6.3 Other data types : 6.3.5 Strings
===========================================================================*/
SCM_EXPORT ScmObj
scm_p_stringp(ScmObj obj)
{
    DECLARE_FUNCTION("string?", procedure_fixed_1);

    return MAKE_BOOL(STRINGP(obj));
}

SCM_EXPORT ScmObj
scm_p_string_length(ScmObj str)
{
    scm_int_t len;
    DECLARE_FUNCTION("string-length", procedure_fixed_1);

    ENSURE_STRING(str);

#if SCM_USE_MULTIBYTE_CHAR
    len = scm_mb_bare_c_strlen(scm_current_char_codec, SCM_STRING_STR(str));
#else
    len = SCM_STRING_LEN(str);
#endif

    return MAKE_INT(len);
}

SCM_EXPORT ScmObj
scm_p_stringequalp(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string=?", procedure_fixed_2);

    ENSURE_STRING(str1);
    ENSURE_STRING(str2);

    return MAKE_BOOL(STRING_EQUALP(str1, str2));
}

/* FIXME: support stateful encoding */
SCM_EXPORT ScmObj
scm_p_string_append(ScmObj args)
{
    ScmObj rest, str;
    size_t byte_len;
    scm_int_t mb_len;
    char  *new_str, *dst;
    const char *src;
    DECLARE_FUNCTION("string-append", procedure_variadic_0);

    if (NULLP(args))
        return MAKE_STRING_COPYING("", 0);

    /* count total size of the new string */
    byte_len = mb_len = 0;
    rest = args;
    FOR_EACH (str, rest) {
        ENSURE_STRING(str);
        mb_len   += SCM_STRING_LEN(str);
#if SCM_USE_MULTIBYTE_CHAR
        byte_len += strlen(SCM_STRING_STR(str));
#else
        byte_len = mb_len;
#endif
    }

    new_str = scm_malloc(byte_len + sizeof(""));

    /* copy all strings into new_str */
    dst = new_str;
    FOR_EACH (str, args) {
        for (src = SCM_STRING_STR(str); *src;)
            *dst++ = *src++;
    }
    *dst = '\0';

#if SCM_USE_NULL_CAPABLE_STRING
    /* each string is chopped at first null and the result is incorrect */
    return MAKE_STRING(new_str, STRLEN_UNKNOWN);
#else
    return MAKE_STRING(new_str, mb_len);
#endif
}

SCM_EXPORT ScmObj
scm_p_string_copy(ScmObj str)
{
    DECLARE_FUNCTION("string-copy", procedure_fixed_1);

    ENSURE_STRING(str);

#if SCM_USE_NULL_CAPABLE_STRING
    /* result is truncated at first null and incorrect */
    return MAKE_STRING_COPYING(SCM_STRING_STR(str), STRLEN_UNKNOWN);
#else
    return MAKE_STRING_COPYING(SCM_STRING_STR(str), SCM_STRING_LEN(str));
#endif
}
