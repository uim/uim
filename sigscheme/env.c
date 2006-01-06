/*===========================================================================
 *  FileName : env.c
 *  About    : A Scheme environemnt implementation
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

/*
 *   Environment is a list formed as below.
 *
 *     Frame = (cons (var1 var2 var3 ...)
 *                   (val1 val2 val3 ...))
 *     Env   = (Frame1 Frame2 Frame3 ...)
 *
 *   Other 2 forms are also used to handle dotted args.
 *
 *     Frame = (cons (var1 var2 var3 . rest1)
 *                   (val1 val2 val3 var4 var5 ...))
 *
 *     Frame = (cons rest2
 *                   (val1 val2 val3 var4 var5 ...))
 *
 *   In this case, rest1 is bound to (var4 var5 ...) and rest2 is bound to
 *   (val1 val2 val3 var4 var5 ...).
 *
 *   The environment object should not be manipulated manually, to allow
 *   replacing with another implementation. Use the function interfaces.
 *
 *   To ensure valid use of the environment objects is environment
 *   constructor's responsibility. i.e. Any lookup operations assume that the
 *   environment object is valid. To keep the assumption true, any environemnt
 *   object modification and injection from user code must be
 *   validated. Although the validation for the injection may cost high,
 *   ordinary code only use (interaction-environment) and other R5RS
 *   environment specifiers. Since these 'trusted' specifiers can cheaply be
 *   identified, the validation cost is also. The validation can reject any
 *   hand-maid invalid environment objects.
 */

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
#define TRUSTED_ENVP(env) (EQ(env, SCM_INTERACTION_ENV)                      \
                           || EQ(env, SCM_R5RS_ENV)                          \
                           || EQ(env, SCM_NULL_ENV))

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmRef lookup_frame(ScmObj var, ScmObj frame);
static scm_bool valid_framep(ScmObj frame);

/*=======================================
  Function Implementations
=======================================*/
/**
 * Construct new frame on an env
 *
 * @a vars and @a vals must surely be a list.
 *
 * @param vars Symbol list as variable names of new frame. It accepts dotted
 *             list to handle function arguments directly.
 * @param vals Arbitrary Scheme object list as values of new frame.
 *
 * @see scm_eval()
 */
ScmObj
scm_extend_environment(ScmObj vars, ScmObj vals, ScmObj env)
{
    ScmObj frame;
    DECLARE_INTERNAL_FUNCTION("scm_extend_environment");

    SCM_ASSERT(scm_valid_environment_extensionp(vars, vals));
    SCM_ASSERT(VALID_ENVP(env));

    frame = CONS(vars, vals);
    return CONS(frame, env);
}

/** Add a binding to newest frame of an env */
ScmObj
scm_add_environment(ScmObj var, ScmObj val, ScmObj env)
{
    ScmObj newest_frame;
    ScmObj new_vars, new_vals;
    DECLARE_INTERNAL_FUNCTION("scm_add_environment");

    SCM_ASSERT(SYMBOLP(var));
    SCM_ASSERT(VALID_ENVP(env));

    /* add (var, val) pair to the newest frame in env */
    if (NULLP(env)) {
        newest_frame = CONS(LIST_1(var), LIST_1(val));
        env = LIST_1(newest_frame);
    } else if (CONSP(env)) {
        newest_frame = CAR(env);
        new_vars = CONS(var, CAR(newest_frame));
        new_vals = CONS(val, CDR(newest_frame));
        newest_frame = CONS(new_vars, new_vals);

        SET_CAR(env, newest_frame);
    } else {
        SCM_ASSERT(scm_false);
    }
    return env;
}

/**
 * Lookup a variable of an env
 *
 * @return Reference to the variable. SCM_INVALID_REF if not found.
 */
ScmRef
scm_lookup_environment(ScmObj var, ScmObj env)
{
    ScmObj frame;
    ScmRef ref;
    DECLARE_INTERNAL_FUNCTION("scm_lookup_environment");

    SCM_ASSERT(SYMBOLP(var));
    SCM_ASSERT(VALID_ENVP(env));

    /* lookup in frames */
    for (; !NULLP(env); env = CDR(env)) {
        frame = CAR(env);
        ref = lookup_frame(var, frame);
        if (ref != SCM_INVALID_REF)
            return ref;
    }
    SCM_ASSERT(NULLP(env));

    return SCM_INVALID_REF;
}

/** Lookup a variable of a frame */
static ScmRef
lookup_frame(ScmObj var, ScmObj frame)
{
    ScmObj vars;
    ScmRef vals;
    DECLARE_INTERNAL_FUNCTION("lookup_frame");

    SCM_ASSERT(SYMBOLP(var));
    SCM_ASSERT(valid_framep(frame));

    for (vars = CAR(frame), vals = REF_CDR(frame);
         CONSP(vars);
         vars = CDR(vars), vals = REF_CDR(DEREF(vals)))
    {
        if (EQ(var, CAR(vars)))
            return REF_CAR(DEREF(vals));
    }
    /* dotted list */
    if (EQ(var, vars))
        return vals;

    return SCM_INVALID_REF;
}

/*
 * Validators
 */
scm_bool
scm_valid_environmentp(ScmObj env)
{
    ScmObj frame, rest;
    DECLARE_INTERNAL_FUNCTION("scm_valid_environmentp");

    if (TRUSTED_ENVP(env))
        return scm_true;

    /*
     * The env is extended and untrusted. Since this case rarely occurs in
     * ordinary codes, the expensive validation cost is acceptable.
     */

    if (!PROPER_LISTP(env))
        return scm_false;
    for (rest = env; !NULLP(rest); rest = CDR(rest)) {
        frame = CAR(rest);
        if (!valid_framep(frame))
            return scm_false;
    }

    return scm_true;
}

static scm_bool
valid_framep(ScmObj frame)
{
    ScmObj vars, vals;
    DECLARE_INTERNAL_FUNCTION("valid_framep");

    if (CONSP(frame)) {
        vars = CAR(frame);
        vals = CDR(frame);
        if (scm_valid_environment_extensionp(vars, vals))
            return scm_true;
    }
    return scm_false;
}

scm_bool
scm_valid_environment_extensionp(ScmObj formals, ScmObj actuals)
{
    int formals_len, actuals_len;

    formals_len = scm_validate_formals(formals);
    actuals_len = scm_validate_actuals(actuals);
    return scm_valid_environment_extension_lengthp(formals_len, actuals_len);
}

/* formals_len must be validated by scm_validate_formals() prior to here */
scm_bool
scm_valid_environment_extension_lengthp(int formals_len, int actuals_len)
{
    if (SCM_LISTLEN_ERRORP(formals_len) || !SCM_LISTLEN_PROPERP(actuals_len))
        return scm_false;
    if (SCM_LISTLEN_DOTTEDP(formals_len)) {
        formals_len = SCM_LISTLEN_BEFORE_DOT(formals_len);
        return (formals_len <= actuals_len);
    }
    return (formals_len == actuals_len);
}

int
scm_validate_formals(ScmObj formals)
{
    ScmObj var;
    int len;
    DECLARE_INTERNAL_FUNCTION("scm_validate_formals");

    /* This loop goes infinite if the formals is circular. SigSchme expects
     * that user codes are sane here. */
    for (len = 0; var = POP_ARG(formals), VALIDP(var); len++) {
        if (!SYMBOLP(var))
            return SCM_LISTLEN_ENCODE_ERROR(len);
    }
    if (NULLP(formals))
        return len;
    /* dotted list allowed */
    if (SYMBOLP(formals))
        return SCM_LISTLEN_ENCODE_DOTTED(len + 1);
    return SCM_LISTLEN_ENCODE_ERROR(len);
}

int
scm_validate_actuals(ScmObj actuals)
{
    int len;

    len = scm_length(actuals);
    if (SCM_LISTLEN_DOTTEDP(len))
        len = SCM_LISTLEN_ENCODE_ERROR(len);
    return len;
}
