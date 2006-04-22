/*===========================================================================
 *  Filename : module-srfi60.c
 *  About    : SRFI-60 Integers as Bits
 *
 *  Copyright (C) 2005-2006 YamaKen
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
  File Local Macro Definitions
=======================================*/
#define BITWISE_OPERATION_BODY(op, left, right)                              \
    do {                                                                     \
        scm_int_t result;                                                    \
                                                                             \
        result = 0;                                                          \
        switch (*state) {                                                    \
        case SCM_REDUCE_0:                                                   \
            break;                                                           \
        case SCM_REDUCE_1:                                                   \
            ENSURE_INT(right);                                               \
            return right;                                                    \
        case SCM_REDUCE_PARTWAY:                                             \
        case SCM_REDUCE_LAST:                                                \
            ENSURE_INT(left);                                                \
            ENSURE_INT(right);                                               \
            result = (SCM_INT_VALUE(left) op SCM_INT_VALUE(right));          \
            break;                                                           \
        default:                                                             \
            SCM_ASSERT(scm_false);                                           \
        }                                                                    \
        return MAKE_INT(result);                                             \
    } while (/* CONSTCOND */ 0)

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/
#include "functable-srfi60.c"

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Definitions
=======================================*/
SCM_EXPORT void
scm_initialize_srfi60(void)
{
    scm_register_funcs(scm_srfi60_func_info_table);

    scm_define_alias("bitwise-and",   "logand");
    scm_define_alias("bitwise-ior",   "logior");
    scm_define_alias("bitwise-xor",   "logxor");
    scm_define_alias("bitwise-not",   "lognot");
    scm_define_alias("bitwise-merge", "bitwise-if");
    scm_define_alias("any-bits-set?", "logtest");
}

/* Bitwise Operations */
SCM_EXPORT ScmObj
scm_p_srfi60_logand(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("logand", reduction_operator);

    BITWISE_OPERATION_BODY(&, left, right);
}

SCM_EXPORT ScmObj
scm_p_srfi60_logior(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("logior", reduction_operator);

    BITWISE_OPERATION_BODY(|, left, right);
}

SCM_EXPORT ScmObj
scm_p_srfi60_logxor(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("logxor", reduction_operator);

    BITWISE_OPERATION_BODY(^, left, right);
}

SCM_EXPORT ScmObj
scm_p_srfi60_lognot(ScmObj n)
{
    DECLARE_FUNCTION("lognot", procedure_fixed_1);

    ENSURE_INT(n);

    return MAKE_INT(~SCM_INT_VALUE(n));
}

SCM_EXPORT ScmObj
scm_p_srfi60_bitwise_if(ScmObj mask, ScmObj n0, ScmObj n1)
{
    scm_int_t result, c_mask;
    DECLARE_FUNCTION("bitwise-if", procedure_fixed_3);

    ENSURE_INT(mask);
    ENSURE_INT(n0);
    ENSURE_INT(n1);

    c_mask = SCM_INT_VALUE(mask);
    result = (c_mask & SCM_INT_VALUE(n0)) | (~c_mask & SCM_INT_VALUE(n1));

    return MAKE_INT(result);
}

SCM_EXPORT ScmObj
scm_p_srfi60_logtest(ScmObj j, ScmObj k)
{
    DECLARE_FUNCTION("logtest", procedure_fixed_2);

    ENSURE_INT(j);
    ENSURE_INT(k);

    return MAKE_BOOL(SCM_INT_VALUE(j) & SCM_INT_VALUE(k));
}
