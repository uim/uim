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

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Implementations
=======================================*/

/*=============================================================================
  SRFI-60 : Integers as Bits
=============================================================================*/

/* Bitwise Operations */

/* FIXME: Rewrite as a SCM_REDUCTION_OPERATOR function */
ScmObj ScmOp_SRFI60_logand(ScmObj args, ScmObj env)
{
    SCM_REDUCE((accum & elm), 0, args, env,
               int, INTP, SCM_INT_VALUE, Scm_NewInt,
               "logand : integer required but got ");
}

/* FIXME: Rewrite as a SCM_REDUCTION_OPERATOR function */
ScmObj ScmOp_SRFI60_logior(ScmObj args, ScmObj env)
{
    SCM_REDUCE((accum | elm), 0, args, env,
               int, INTP, SCM_INT_VALUE, Scm_NewInt,
               "logior : integer required but got ");
}

/* FIXME: Rewrite as a SCM_REDUCTION_OPERATOR function */
ScmObj ScmOp_SRFI60_logxor(ScmObj args, ScmObj env)
{
    SCM_REDUCE((accum ^ elm), 0, args, env,
               int, INTP, SCM_INT_VALUE, Scm_NewInt,
               "logxor : integer required but got ");
}

ScmObj ScmOp_SRFI60_lognot(ScmObj n)
{
    if (!INTP(n))
        SigScm_ErrorObj("lognot : integer required but got ", n);

    return Scm_NewInt(~SCM_INT_VALUE(n));
}

ScmObj ScmOp_SRFI60_bitwise_if(ScmObj mask, ScmObj n0, ScmObj n1)
{
    int result, c_mask;

    if (!INTP(mask))
        SigScm_ErrorObj("bitwise-if : integer required but got ", mask);
    if (!INTP(n0))
        SigScm_ErrorObj("bitwise-if : integer required but got ", n0);
    if (!INTP(n1))
        SigScm_ErrorObj("bitwise-if : integer required but got ", n1);

    c_mask = SCM_INT_VALUE(mask);
    result = (c_mask & SCM_INT_VALUE(n0)) | (~c_mask & SCM_INT_VALUE(n1));

    return Scm_NewInt(result);
}

ScmObj ScmOp_SRFI60_logtest(ScmObj j, ScmObj k)
{
    if (!INTP(j))
        SigScm_ErrorObj("logtest : integer required but got ", j);
    if (!INTP(k))
        SigScm_ErrorObj("logtest : integer required but got ", k);

    return (SCM_INT_VALUE(j) & SCM_INT_VALUE(k)) ? SCM_TRUE : SCM_FALSE;
}
