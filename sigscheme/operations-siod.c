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
/*
 * SIOD's verbose-level compatible debug message printing control:
 * Search 'siod_verbose_level' in slib.c to know further detail.
 *
 * Extra control:
 *   v0: suppress all printing even if normal 'write' or 'display'
 *   v1: print each result of repl
 *   v2: print the "> " prompt
 */
#define SCM_DBG_SIOD_V0 SCM_DBG_NONE
#define SCM_DBG_SIOD_V1 (SCM_DBG_ERRMSG | SCM_DBG_BACKTRACE)
#define SCM_DBG_SIOD_V2 SCM_DBG_SIOD_V1
#define SCM_DBG_SIOD_V3 (SCM_DBG_SIOD_V2 | SCM_DBG_FILE)
#define SCM_DBG_SIOD_V4 (SCM_DBG_SIOD_V3 | SCM_DBG_GC)
#define SCM_DBG_SIOD_V5 (SCM_DBG_SIOD_V4 | SCM_DBG_READ)

/*=======================================
  Variable Declarations
=======================================*/
static const int sscm_debug_mask_tbl[] = {
    SCM_DBG_SIOD_V0,
    SCM_DBG_SIOD_V1,
    SCM_DBG_SIOD_V2,
    SCM_DBG_SIOD_V3,
    SCM_DBG_SIOD_V4,
    SCM_DBG_SIOD_V5
};
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
void SigScm_Initialize_SIOD(void)
{
    ScmExp_use(Scm_Intern("srfi-60"), SCM_INTERACTION_ENV);
    Scm_DefineAlias("bit-and"               , "logand");
    Scm_DefineAlias("bit-or"                , "logior");
    Scm_DefineAlias("bit-xor"               , "logxor");
    Scm_DefineAlias("bit-not"               , "lognot");

    Scm_RegisterProcedureVariadic1("symbol-bound?"     , ScmOp_symbol_boundp);
    Scm_RegisterProcedureFixed1("symbol-value"         , ScmOp_symbol_value);
    Scm_RegisterProcedureFixed2("set-symbol-value!"    , ScmOp_set_symbol_value);
#if SCM_COMPAT_SIOD_BUGS
    Scm_RegisterProcedureFixed2("="                    , ScmOp_siod_eql);
#endif
    Scm_RegisterProcedureFixedTailRec0("the-environment" , ScmOp_the_environment);
    Scm_RegisterProcedureFixed1("%%closure-code"       , ScmOp_closure_code);
    Scm_RegisterProcedureVariadic0("verbose" , ScmOp_verbose);

    SigScm_SetVerboseLevel(2);
}

/*
 * TODO:
 * - generalize to SCM_USE_NONSTD_FEATURES
 * - describe compatibility with de facto standard of other Scheme
 *   implementations (accept env as optional arg, etc)
 */
ScmObj ScmOp_symbol_boundp(ScmObj sym, ScmObj rest)
{
    ScmObj env = SCM_INVALID;
    DECLARE_FUNCTION("symbol-bound?", ProcedureVariadic1);

    ASSERT_SYMBOLP(sym);

#if SCM_COMPAT_SIOD_BUGS
    /* SIOD compatible implementation */
    return (SCM_SYMBOL_BOUNDP(sym)) ? SCM_TRUE : SCM_FALSE;
#else
    env = POP_ARG(rest);
    if (VALIDP(env))
        ASSERT_ENVP(env);
    else
        env = SCM_INTERACTION_ENV;

    return (!NULLP(Scm_LookupEnvironment(sym, env))
            || SCM_SYMBOL_BOUNDP(sym)) ? SCM_TRUE : SCM_FALSE;
#endif
}

/*
 * TODO:
 * - replace with a portable proc such as (eval 'sym (interaction-environment))
 * - make the portable proc interface similar to a de facto standard of other
 *   Scheme implementations if existing
 */
ScmObj ScmOp_symbol_value(ScmObj var)
{
    if (!SYMBOLP(var))
        SigScm_ErrorObj("symbol-value : symbol required but got ", var);

    return Scm_SymbolValue(var, SCM_NULL);
}

/*
 * TODO:
 * - replace with a portable proc such as (eval '(set! sym val)
 *                                               (interaction-environment))
 * - make the portable proc interface similar to a de facto standard of other
 *   Scheme implementations if existing
 */
ScmObj ScmOp_set_symbol_value(ScmObj var, ScmObj val)
{
    /* sanity check */
    if (!SYMBOLP(var))
        SigScm_ErrorObj("set-symbol-value! : symbol required but got ", var);

    return SCM_SYMBOL_SET_VCELL(var, val);
}

ScmObj ScmOp_siod_eql(ScmObj obj1, ScmObj obj2)
{
    if (EQ(obj1, obj2))
        return SCM_TRUE;
    else if (!INTP(obj1) || !INTP(obj2))
        return SCM_FALSE;
    else if (SCM_INT_VALUE(obj1) == SCM_INT_VALUE(obj2))
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_the_environment(ScmEvalState *eval_state)
{
    eval_state->ret_type = SCM_RETTYPE_AS_IS;

    return eval_state->env;
}

ScmObj ScmOp_closure_code(ScmObj closure)
{
    ScmObj exp, body;

    if (!CLOSUREP(closure))
        SigScm_ErrorObj("%%closure-code : closure required but got ", closure);

    exp = SCM_CLOSURE_EXP(closure);

    if (NULLP(CDDR(exp)))
	body = CADR(exp);
    else
	body = CONS(Scm_Intern("begin"), CDR(exp));
    
    return CONS(CAR(exp), body);
}

ScmObj ScmOp_verbose(ScmObj args)
{
    if (!NULLP(args)) {
        if (!INTP(CAR(args)))
            SigScm_ErrorObj("verbose : integer required but got ", args);

        SigScm_SetVerboseLevel(SCM_INT_VALUE(CAR(args)));
    }

    return Scm_NewInt(sscm_verbose_level);
}

long SigScm_GetVerboseLevel(void)
{
    return sscm_verbose_level;
}

void SigScm_SetVerboseLevel(long level)
{
    if (level < 0)
        SigScm_Error("SigScm_SetVerboseLevel : negative value has been given");

    sscm_verbose_level = level;

    if (level > 5)
        level = 5;
    SigScm_SetDebugCategories(sscm_debug_mask_tbl[level]);

    if (level >= 2)
        SigScm_SetDebugCategories(SigScm_DebugCategories()
                                  | SigScm_PredefinedDebugCategories());

    if (level == 0) {
        scm_current_error_port = NULL;
        scm_current_output_port = NULL;
    } else {
        if (!scm_current_error_port)
            scm_current_error_port = scm_std_error_port;
        if (!scm_current_output_port)
            scm_current_output_port = scm_std_output_port;
    }
}
