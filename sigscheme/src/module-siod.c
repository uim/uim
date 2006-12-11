/*===========================================================================
 *  Filename : module-siod.c
 *  About    : SIOD compatible procedures
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

#include <stddef.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"
#include "scmport-null.h"

/*=======================================
  File Local Macro Definitions
=======================================*/
/*
 * SIOD's verbose-level compatible debug message printing control:
 * Search 'siod_verbose_level' in slib.c to know further detail.
 *
 * Don't change the verbose level 2 for SCM_DBG_BACKTRACE. This is used to
 * suppress backtrace when run by the testing framework of uim.
 *   -- YamaKen 2005-11-05
 *
 * Extra control:
 *   v0: suppress all printing even if normal 'write' or 'display'
 *   v1: print each result of repl
 *   v2: print the "> " prompt
 */
#define SCM_DBG_SIOD_V0 SCM_DBG_NONE
#define SCM_DBG_SIOD_V1 SCM_DBG_ERRMSG
#define SCM_DBG_SIOD_V2 (SCM_DBG_SIOD_V1 | SCM_DBG_BACKTRACE)
#define SCM_DBG_SIOD_V3 (SCM_DBG_SIOD_V2 | SCM_DBG_FILE)
#define SCM_DBG_SIOD_V4 (SCM_DBG_SIOD_V3 | SCM_DBG_GC)
#define SCM_DBG_SIOD_V5 (SCM_DBG_SIOD_V4 | SCM_DBG_READ)

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/
#include "functable-siod.c"

static const int sscm_debug_mask_tbl[] = {
    SCM_DBG_SIOD_V0,
    SCM_DBG_SIOD_V1,
    SCM_DBG_SIOD_V2,
    SCM_DBG_SIOD_V3,
    SCM_DBG_SIOD_V4,
    SCM_DBG_SIOD_V5
};

SCM_GLOBAL_VARS_BEGIN(static_siod);
#define static
static long l_sscm_verbose_level;

static ScmObj l_null_port;
static ScmObj l_saved_output_port;
static ScmObj l_saved_error_port;
#undef static
SCM_GLOBAL_VARS_END(static_siod);
#define l_sscm_verbose_level SCM_GLOBAL_VAR(static_siod, l_sscm_verbose_level)
#define l_null_port          SCM_GLOBAL_VAR(static_siod, l_null_port)
#define l_saved_output_port  SCM_GLOBAL_VAR(static_siod, l_saved_output_port)
#define l_saved_error_port   SCM_GLOBAL_VAR(static_siod, l_saved_error_port)
SCM_DEFINE_STATIC_VARS(static_siod);

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Definitions
=======================================*/
SCM_EXPORT void
scm_initialize_siod(void)
{
    ScmCharPort *cport;

    SCM_GLOBAL_VARS_INIT(static_siod);

    scm_register_funcs(scm_functable_siod);

    scm_use("sscm-ext");
    scm_define_alias("the-environment", "%%current-environment");

    scm_use("srfi-60");
    scm_define_alias("bit-and", "logand");
    scm_define_alias("bit-or",  "logior");
    scm_define_alias("bit-xor", "logxor");
    scm_define_alias("bit-not", "lognot");

    scm_gc_protect_with_init(&l_null_port,         SCM_FALSE);
    scm_gc_protect_with_init(&l_saved_output_port, SCM_FALSE);
    scm_gc_protect_with_init(&l_saved_error_port,  SCM_FALSE);

    scm_nullport_init();
    cport = scm_make_char_port(ScmNullPort_new());
    l_null_port = MAKE_PORT(cport, SCM_PORTFLAG_INPUT | SCM_PORTFLAG_OUTPUT);

    /* To allow re-initialization of the interpreter, this variables must be
     * initialized by assignment. Initialized .data section does not work for
     * such situation.  -- YamaKen 2006-03-31 */
    l_sscm_verbose_level = -1;
    scm_set_verbose_level(2);
}

/*
 * TODO:
 * - replace with a portable proc such as (eval 'sym (interaction-environment))
 * - make the portable proc interface similar to a de facto standard of other
 *   Scheme implementations if existing
 */
SCM_EXPORT ScmObj
scm_p_symbol_value(ScmObj var)
{
    DECLARE_FUNCTION("symbol-value", procedure_fixed_1);

    ENSURE_SYMBOL(var);

    return scm_symbol_value(var, SCM_INTERACTION_ENV);
}

/*
 * TODO:
 * - replace with a portable proc such as (eval '(set! sym val)
 *                                               (interaction-environment))
 * - make the portable proc interface similar to a de facto standard of other
 *   Scheme implementations if existing
 */
SCM_EXPORT ScmObj
scm_p_set_symbol_valuex(ScmObj var, ScmObj val)
{
    DECLARE_FUNCTION("set-symbol-value!", procedure_fixed_2);

    ENSURE_SYMBOL(var);

    SCM_SYMBOL_SET_VCELL(var, val);

    return val;
}

SCM_EXPORT ScmObj
scm_p_siod_equal(ScmObj obj1, ScmObj obj2)
{
    DECLARE_FUNCTION("=", procedure_fixed_2);

    if (EQ(obj1, obj2))
        return SCM_TRUE;

    if (!INTP(obj1) || !INTP(obj2))
        return SCM_FALSE;
    return MAKE_BOOL(SCM_INT_VALUE(obj1) == SCM_INT_VALUE(obj2));
}

/* TODO: remove this once uim has been revised */
SCM_EXPORT ScmObj
scm_p_closure_code(ScmObj closure)
{
    ScmObj exp, body;
    DECLARE_FUNCTION("%%closure-code", procedure_fixed_1);

    ENSURE_CLOSURE(closure);

    exp = SCM_CLOSURE_EXP(closure);
    /* make SIOD-compatible 'begin' -prefixed body */
    body = CONS(scm_intern("begin"), CDR(exp));

    return CONS(CAR(exp), body);
}

SCM_EXPORT ScmObj
scm_p_verbose(ScmObj args)
{
    ScmObj level;
    DECLARE_FUNCTION("verbose", procedure_variadic_0);

    if (!NULLP(args)) {
        level = POP(args);
        ASSERT_NO_MORE_ARG(args);
        ENSURE_INT(level);

        scm_set_verbose_level(SCM_INT_VALUE(level));
    }

    return MAKE_INT(l_sscm_verbose_level);
}

SCM_EXPORT ScmObj
scm_p_eof_val(void)
{
    DECLARE_FUNCTION("eof-val", procedure_fixed_0);

    return SCM_EOF;
}

SCM_EXPORT ScmObj
scm_s_undefine(ScmObj var, ScmObj env)
{
    ScmRef val;
    DECLARE_FUNCTION("undefine", syntax_fixed_1);

    ENSURE_SYMBOL(var);

    val = scm_lookup_environment(var, env);
    if (val != SCM_INVALID_REF)
        SET(val, SCM_UNBOUND);
    else
        SCM_SYMBOL_SET_VCELL(var, SCM_UNBOUND);

    return SCM_FALSE;
}

SCM_EXPORT long
scm_get_verbose_level(void)
{
    return l_sscm_verbose_level;
}

SCM_EXPORT void
scm_set_verbose_level(long level)
{
    DECLARE_INTERNAL_FUNCTION("scm_set_verbose_level");

    if (level < 0)
        ERR("positive value required but got: ~LD", level);

    if (l_sscm_verbose_level == level)
        return;

    l_sscm_verbose_level = level;

    if (level > 5)
        level = 5;
    scm_set_debug_categories(sscm_debug_mask_tbl[level]);

    if (level >= 2)
        scm_set_debug_categories(scm_debug_categories()
                                 | scm_predefined_debug_categories());

    if (level == 0) {
        if (!EQ(scm_err, l_null_port))
            l_saved_error_port = scm_err;
        if (!EQ(scm_out, l_null_port))
            l_saved_output_port = scm_out;

        scm_err = l_null_port;
        scm_out = l_null_port;
    } else {
        if (EQ(scm_err, l_null_port))
            scm_err = l_saved_error_port;
        if (EQ(scm_out, l_null_port))
            scm_out = l_saved_output_port;
    }
}
