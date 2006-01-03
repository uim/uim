/*===========================================================================
 *  FileName : env.c
 *  About    : A Scheme Environemnt Implementation
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
 *   The environment object should not be manipulated manually, to allow
 *   replacing with another implementation. Use the three function interface.
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

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmRef lookup_frame(ScmObj var, ScmObj frame);

/*=======================================
  Function Implementations
=======================================*/
/**
 * Construct new frame on an env
 *
 * @a vars and @a vals must surely be a list.
 *
 * @param vars Symbol list as variable names of new frame. It accepts dot list
 *             to handle function arguments directly.
 * @param vals Arbitrary Scheme object list as values of new frame. Side
 *             effect: destructively modifyies the vals when vars is a dot
 *             list.
 * @see scm_eval()
 */
ScmObj
scm_extend_environment(ScmObj vars, ScmObj vals, ScmObj env)
{
    ScmObj frame, rest_vars, rest_vals;
    DECLARE_INTERNAL_FUNCTION("scm_extend_environment");

#if SCM_STRICT_ARGCHECK
    if (!LISTP(env))
        ERR("broken environment");

    for (rest_vars = vars, rest_vals = vals;
         CONSP(rest_vars) && !NULLP(rest_vals);
         rest_vars = CDR(rest_vars), rest_vals = CDR(rest_vals))
    {
        if (!SYMBOLP(CAR(rest_vars)))
            break;
    }
    if (!(NULLP(rest_vars) || SYMBOLP(rest_vars)))
        ERR_OBJ("broken environment extension", rest_vars);
#endif /* SCM_STRICT_ARGCHECK */

    /* create new frame */
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

    /* sanity check */
    if (!SYMBOLP(var))
        ERR_OBJ("broken environment handling", var);

    /* add (var, val) pair to the newest frame in env */
    if (NULLP(env)) {
        newest_frame = CONS(LIST_1(var), LIST_1(val));
        env = LIST_1(newest_frame);
    } else if (CONSP(env)) {
        newest_frame = CAR(env);
        new_vars = CONS(var, CAR(newest_frame));
        new_vals = CONS(val, CDR(newest_frame));

        SET_CAR(env, CONS(new_vars, new_vals));
    } else {
        ERR_OBJ("broken environent", env);
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

    /* lookup in frames */
    for (; CONSP(env); env = CDR(env)) {
        frame = CAR(env);
        ref   = lookup_frame(var, frame);
        if (ref != SCM_INVALID_REF)
            return ref;
    }

#if SCM_STRICT_ARGCHECK
    if (!NULLP(env))
        ERR_OBJ("broken environent", env);
#endif

    return SCM_INVALID_REF;
}

/** Lookup a variable of a frame */
static ScmRef
lookup_frame(ScmObj var, ScmObj frame)
{
    ScmObj vars;
    ScmRef vals;
    DECLARE_INTERNAL_FUNCTION("lookup_frame");

#if SCM_STRICT_ARGCHECK
    ENSURE_SYMBOL(var);
    ENSURE_CONS(frame);
#endif

    for (vars = CAR(frame), vals = REF_CDR(frame);
         CONSP(vars);
         vars = CDR(vars), vals = REF_CDR(DEREF(vals)))
    {
#if SCM_STRICT_ARGCHECK
        /*
         * This is required to reject hand-maid broken frame:
         *   (eval '(+ x y) '((x . 4)
         *                    (y . 6)))
         *
         * It can be removed once the typed environment object is implemented.
         */
        ENSURE_CONS(DEREF(vals));
#endif
        if (EQ(var, CAR(vars)))
            return REF_CAR(DEREF(vals));
    }
    if (EQ(vars, var))
        return vals;

    return SCM_INVALID_REF;
}

/* 'var' must be a symbol as precondition */
ScmObj
scm_symbol_value(ScmObj var, ScmObj env)
{
    ScmRef ref;
    ScmObj val;
    DECLARE_INTERNAL_FUNCTION("scm_symbol_value");

    /* first, lookup the environment */
    ref = scm_lookup_environment(var, env);
    if (ref != SCM_INVALID_REF) {
        /* variable is found in environment, so returns its value */
        return DEREF(ref);
    }

    /* finally, look at the VCELL */
    val = SCM_SYMBOL_VCELL(var);
    if (EQ(val, SCM_UNBOUND))
        ERR_OBJ("unbound variable", var);

    return val;
}
