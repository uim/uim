/*===========================================================================
 *  FileName : operations-siod.c
 *  About    : SIOD compatible procedures
 *
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
/*=======================================
  System Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  Local Include
=======================================*/

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
static long sscm_verbose_level = 0;

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Implementations
=======================================*/
/*=======================================
  SIOD compatible procedures

  TODO : remove these functions!
=======================================*/
ScmObj ScmOp_symbol_boundp(ScmObj obj)
{
    if (SYMBOLP(obj)
        && !EQ(SCM_SYMBOL_VCELL(obj), SCM_UNBOUND))
    {
        return SCM_TRUE;
    }

    return SCM_FALSE;
}

ScmObj ScmOp_symbol_value(ScmObj var)
{
    if (!SYMBOLP(var))
        SigScm_ErrorObj("symbol-value : require symbol but got ", var);

    return symbol_value(var, SCM_NULL);
}

ScmObj ScmOp_set_symbol_value(ScmObj var, ScmObj val)
{
    /* sanity check */
    if (!SYMBOLP(var))
        SigScm_ErrorObj("set-symbol-value! : require symbol but got ", var);

    return SCM_SYMBOL_SET_VCELL(var, val);
}

ScmObj ScmOp_bit_and(ScmObj obj1, ScmObj obj2)
{
    if (!INTP(obj1))
        SigScm_ErrorObj("bit-and : number required but got ", obj1);
    if (!INTP(obj2))
        SigScm_ErrorObj("bit-and : number required but got ", obj2);

    return Scm_NewInt(SCM_INT_VALUE(obj1) & SCM_INT_VALUE(obj2));
}

ScmObj ScmOp_bit_or(ScmObj obj1, ScmObj obj2)
{
    if (!INTP(obj1))
        SigScm_ErrorObj("bit-or : number required but got ", obj1);
    if (!INTP(obj2))
        SigScm_ErrorObj("bit-or : number required but got ", obj2);

    return Scm_NewInt(SCM_INT_VALUE(obj1) | SCM_INT_VALUE(obj2));
}

ScmObj ScmOp_bit_xor(ScmObj obj1, ScmObj obj2)
{
    if (!INTP(obj1))
        SigScm_ErrorObj("bit-xor : number required but got ", obj1);
    if (!INTP(obj2))
        SigScm_ErrorObj("bit-xor : number required but got ", obj2);

    return Scm_NewInt(SCM_INT_VALUE(obj1) ^ SCM_INT_VALUE(obj2));
}

ScmObj ScmOp_bit_not(ScmObj obj)
{
    if (!INTP(obj))
        SigScm_ErrorObj("bit-not : number required but got ", obj);

    return Scm_NewInt(~SCM_INT_VALUE(obj));
}

ScmObj ScmOp_the_environment(ScmObj arg, ScmObj env)
{
    return env;
}

ScmObj ScmOp_closure_code(ScmObj closure)
{
    if (!CLOSUREP(closure))
        SigScm_ErrorObj("%%closure-code : closure required but got ", closure);

    return SCM_CLOSURE_EXP(closure);
}

ScmObj ScmOp_verbose(ScmObj args, ScmObj env)
{
    if (!NULLP(args)) {
        if (!INTP(CAR(args)))
            SigScm_ErrorObj("verbose : integer required but got ", args);

        sscm_verbose_level = SCM_INT_VALUE(CAR(args));
    }

    return Scm_NewInt(sscm_verbose_level);
}

long SigScm_get_verbose_level(void)
{
    return sscm_verbose_level;
}

void SigScm_set_verbose_level(long level)
{
    sscm_verbose_level = level;
}
