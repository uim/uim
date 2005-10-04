/*===========================================================================
 *  FileName : operations-srfi60.c
 *  About    : SRFI-60 integers as bits
 *
 *  Copyright (C) 2005      by YamaKen
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
 *  =========================================================================*/
/*=======================================
  System Include
=======================================*/

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/
#define BITWISE_OPERATION_BODY(op, opstr)                                    \
    do {                                                                     \
        int result = 0;                                                      \
        switch (*state) {                                                    \
        case SCM_REDUCE_0:                                                   \
            break;                                                           \
        case SCM_REDUCE_1:                                                   \
            ASSERT_INTP(left);                                               \
            return right;                                                    \
        case SCM_REDUCE_PARTWAY:                                             \
        case SCM_REDUCE_LAST:                                                \
            /* left is already ensured as int by previous loop */            \
            ASSERT_INTP(right);                                              \
            result = (SCM_INT_VALUE(left) op SCM_INT_VALUE(right));          \
            break;                                                           \
        default:                                                             \
            SigScm_Error(opstr " : (internal error) unrecognized state specifier: %d", *state); \
        }                                                                    \
        return Scm_NewInt(result);                                           \
    } while (/* CONSTCOND */ 0)

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Implementations
=======================================*/
void SigScm_Initialize_SRFI60(void)
{
    /*=======================================================================
      SRFI-60 Procedures
    =======================================================================*/
    Scm_RegisterReductionOperator("logand"   , ScmOp_SRFI60_logand);
    Scm_RegisterReductionOperator("logior"   , ScmOp_SRFI60_logior);
    Scm_RegisterReductionOperator("logxor"   , ScmOp_SRFI60_logxor);
    Scm_RegisterProcedureFixed1("lognot"     , ScmOp_SRFI60_lognot);
    Scm_RegisterProcedureFixed3("bitwise-if" , ScmOp_SRFI60_bitwise_if);
    Scm_RegisterProcedureFixed2("logtest"    , ScmOp_SRFI60_logtest);
    Scm_DefineAlias("bitwise-and"            , "logand");
    Scm_DefineAlias("bitwise-ior"            , "logior");
    Scm_DefineAlias("bitwise-xor"            , "logxor");
    Scm_DefineAlias("bitwise-not"            , "lognot");
    Scm_DefineAlias("bitwise-merge"          , "bitwise-if");
    Scm_DefineAlias("any-bits-set?"          , "logtest");
}

/*=============================================================================
  SRFI-60 : Integers as Bits
=============================================================================*/

/* Bitwise Operations */
ScmObj ScmOp_SRFI60_logand(ScmObj left, ScmObj right,
                           enum ScmReductionState *state)
{
    DECLARE_FUNCTION("logand", ReductionOperator);
    BITWISE_OPERATION_BODY(&, "logand");
}

ScmObj ScmOp_SRFI60_logior(ScmObj left, ScmObj right,
                           enum ScmReductionState *state)
{
    DECLARE_FUNCTION("logior", ReductionOperator);
    BITWISE_OPERATION_BODY(|, "logior");
}

ScmObj ScmOp_SRFI60_logxor(ScmObj left, ScmObj right,
                           enum ScmReductionState *state)
{
    DECLARE_FUNCTION("logexor", ReductionOperator);
    BITWISE_OPERATION_BODY(^, "logxor");
}

ScmObj ScmOp_SRFI60_lognot(ScmObj n)
{
    DECLARE_FUNCTION("lognot", ProcedureFixed1);

    ASSERT_INTP(n);

    return Scm_NewInt(~SCM_INT_VALUE(n));
}

ScmObj ScmOp_SRFI60_bitwise_if(ScmObj mask, ScmObj n0, ScmObj n1)
{
    int result, c_mask;
    DECLARE_FUNCTION("bitwise-if", ProcedureFixed3);

    ASSERT_INTP(mask);
    ASSERT_INTP(n0);
    ASSERT_INTP(n1);

    c_mask = SCM_INT_VALUE(mask);
    result = (c_mask & SCM_INT_VALUE(n0)) | (~c_mask & SCM_INT_VALUE(n1));

    return Scm_NewInt(result);
}

ScmObj ScmOp_SRFI60_logtest(ScmObj j, ScmObj k)
{
    DECLARE_FUNCTION("logtest", ProcedureFixed2);

    ASSERT_INTP(j);
    ASSERT_INTP(k);

    return (SCM_INT_VALUE(j) & SCM_INT_VALUE(k)) ? SCM_TRUE : SCM_FALSE;
}
