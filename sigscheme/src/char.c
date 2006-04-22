/*===========================================================================
 *  Filename : char.c
 *  About    : R5RS characters
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

#include "config.h"

#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  File Local Macro Definitions
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
  R5RS : 6.3 Other data types : 6.3.4 Characters
===========================================================================*/
SCM_EXPORT ScmObj
scm_p_charp(ScmObj obj)
{
    DECLARE_FUNCTION("char?", procedure_fixed_1);

    return MAKE_BOOL(CHARP(obj));
}

SCM_EXPORT ScmObj
scm_p_char_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char=?", procedure_fixed_2);

    ENSURE_CHAR(ch1);
    ENSURE_CHAR(ch2);

#if SCM_HAS_IMMEDIATE_CHAR_ONLY
    return MAKE_BOOL(EQ(ch1, ch2));
#else
    return MAKE_BOOL(SCM_CHAR_VALUE(ch1) == SCM_CHAR_VALUE(ch2));
#endif
}

#define CHAR_CMP_BODY(op, ch1, ch2)                                          \
    do {                                                                     \
        ENSURE_CHAR(ch1);                                                    \
        ENSURE_CHAR(ch2);                                                    \
                                                                             \
        return MAKE_BOOL(SCM_CHAR_VALUE(ch1) op SCM_CHAR_VALUE(ch2));        \
    } while (/* CONSTCOND */ 0)

SCM_EXPORT ScmObj
scm_p_char_lessp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char<?", procedure_fixed_2);

    CHAR_CMP_BODY(<, ch1, ch2);
}

SCM_EXPORT ScmObj
scm_p_char_greaterp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char>?", procedure_fixed_2);

    CHAR_CMP_BODY(>, ch1, ch2);
}

SCM_EXPORT ScmObj
scm_p_char_less_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char<=?", procedure_fixed_2);

    CHAR_CMP_BODY(<=, ch1, ch2);
}

SCM_EXPORT ScmObj
scm_p_char_greater_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char>=?", procedure_fixed_2);

    CHAR_CMP_BODY(>=, ch1, ch2);
}

#define CHAR_CI_CMP_BODY(op, ch1, ch2)                                       \
    do {                                                                     \
        scm_ichar_t val1, val2;                                              \
                                                                             \
        ENSURE_CHAR(ch1);                                                    \
        ENSURE_CHAR(ch2);                                                    \
                                                                             \
        val1 = ICHAR_FOLDCASE(SCM_CHAR_VALUE(ch1));                          \
        val2 = ICHAR_FOLDCASE(SCM_CHAR_VALUE(ch2));                          \
                                                                             \
        return MAKE_BOOL(val1 op val2);                                      \
    } while (/* CONSTCOND */ 0)

SCM_EXPORT ScmObj
scm_p_char_ci_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci=?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(==, ch1, ch2);
}

SCM_EXPORT ScmObj
scm_p_char_ci_lessp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci<?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(<, ch1, ch2);
}

SCM_EXPORT ScmObj
scm_p_char_ci_greaterp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci>?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(>, ch1, ch2);
}

SCM_EXPORT ScmObj
scm_p_char_ci_less_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci<=?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(<=, ch1, ch2);
}

SCM_EXPORT ScmObj
scm_p_char_ci_greater_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci>=?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(>=, ch1, ch2);
}

#undef CHAR_CMP_BODY
#undef CHAR_CI_CMP_BODY

SCM_EXPORT ScmObj
scm_p_char_alphabeticp(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-alphabetic?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(ICHAR_ALPHABETICP(val));
}

SCM_EXPORT ScmObj
scm_p_char_numericp(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-numeric?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(ICHAR_NUMERICP(val));
}

SCM_EXPORT ScmObj
scm_p_char_whitespacep(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-whitespace?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(ICHAR_WHITESPACEP(val));
}

SCM_EXPORT ScmObj
scm_p_char_upper_casep(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-upper-case?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(ICHAR_UPPER_CASEP(val));
}

SCM_EXPORT ScmObj
scm_p_char_lower_casep(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-lower-case?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(ICHAR_LOWER_CASEP(val));
}

SCM_EXPORT ScmObj
scm_p_char2integer(ScmObj ch)
{
    DECLARE_FUNCTION("char->integer", procedure_fixed_1);

    ENSURE_CHAR(ch);

    return MAKE_INT(SCM_CHAR_VALUE(ch));
}

SCM_EXPORT ScmObj
scm_p_integer2char(ScmObj n)
{
    scm_int_t val;
    DECLARE_FUNCTION("integer->char", procedure_fixed_1);

    ENSURE_INT(n);

    val = SCM_INT_VALUE(n);
#if SCM_USE_MULTIBYTE_CHAR
    if (!SCM_CHARCODEC_CHAR_LEN(scm_current_char_codec, val))
#else
    if (!ICHAR_ASCIIP(val))
#endif
        ERR("invalid char value: #x~MX", SCM_INT_VALUE(n));

    return MAKE_CHAR(val);
}

SCM_EXPORT ScmObj
scm_p_char_upcase(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-upcase", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);
    SCM_CHAR_SET_VALUE(ch, ICHAR_UPCASE(val));

    return ch;
}

SCM_EXPORT ScmObj
scm_p_char_downcase(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-downcase", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);
    SCM_CHAR_SET_VALUE(ch, ICHAR_DOWNCASE(val));

    return ch;
}
