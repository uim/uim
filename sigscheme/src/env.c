/*===========================================================================
 *  Filename : env.c
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

#include "config.h"

#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  File Local Macro Definitions
=======================================*/
#define TRUSTED_ENVP(env) (EQ(env, SCM_INTERACTION_ENV)                      \
                           || EQ(env, SCM_R5RS_ENV)                          \
                           || EQ(env, SCM_NULL_ENV))

/*=======================================
  Variable Definitions
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static scm_bool valid_framep(ScmObj frame);

/*=======================================
  Function Definitions
=======================================*/
SCM_EXPORT scm_bool
scm_toplevel_environmentp(ScmObj env)
{
    return NULLP(env);
}

#if SCM_USE_HYGIENIC_MACRO

/* ScmPackedEnv is scm_int_t. */

SCM_EXPORT ScmPackedEnv
scm_pack_env(ScmObj env)
{
    scm_int_t depth;
    DECLARE_INTERNAL_FUNCTION("scm_env_depth");

    depth = scm_length(env);
    SCM_ASSERT(SCM_LISTLEN_PROPERP(depth));
    return depth;
}

/* Not used. */
SCM_EXPORT ScmObj
scm_unpack_env(ScmPackedEnv packed, ScmObj context)
{
    scm_int_t depth;

    depth = scm_length(context);
    while (depth-- > packed)
        context = CDR(context);
    return context;
}


static ScmRef
lookup_n_frames(ScmObj id, scm_int_t n, ScmObj env)
{
    ScmRef ref;

    while (n--) {
        SCM_ASSERT(ENVP(env));
        ref = scm_lookup_frame(id, CAR(env));
        if (ref != SCM_INVALID_REF)
            return ref;
        env = CDR(env);
    }
    return SCM_INVALID_REF;
}


/**
 * Resolves X in scm_unpack_env(XPENV, ENV), Y in ENV and tests
 * whether they are bound to the same location.  The parameters x and
 * y are thus UNINTERCHANGEABLE.
 *
 * The pattern matcher must compare scm_wrap_identifier(x, xpenv) with
 * y, but for performance we'd like to do that without actually
 * allocating the wrapper.  In the absence of syntax-case or
 * comparable mechanisms allowing for unhygienic transforms, the
 * binding frame of X and Y are always both contained in ENV, so we
 * might as well require that ENV be the environment in which one of
 * the operands (namely, Y) is to be looked up.
 *
 * But this is definitely an ugly interface, and also inconvenient
 * because the function needs a different signature when unhygienic
 * transforms are enabled.  So, FIXME: is there a better way?
 *
 * Moving this to macro.c can be an option, but keep in mind some
 * aspects are inherently tightly coupled with the lookup functions.
 */
SCM_EXPORT scm_bool
scm_identifierequalp(ScmObj x, ScmPackedEnv xpenv,
                     ScmObj y, ScmPackedEnv penv, ScmObj env)
{
    ScmRef yloc;

    SCM_ASSERT(xpenv <= penv);
    SCM_ASSERT(SCM_PENV_EQ(scm_pack_env(env), penv));

    while (penv-- > xpenv) {
        if (scm_lookup_frame(y, CAR(env)) != SCM_INVALID_REF)
            return scm_false;
        env = CDR(env);
    }
    if (EQ(x, y))
        return scm_true;
    yloc = scm_lookup_environment(y, env);
    if (yloc != SCM_INVALID_REF)
        return (scm_lookup_environment(x, env) == yloc);
    if (scm_lookup_environment(x, env) != SCM_INVALID_REF)
        return scm_false;
    return EQ(SCM_UNWRAP_KEYWORD(x), SCM_UNWRAP_KEYWORD(y));
}

/**
 * Returns an identifier that is bound to the same location as ID
 * within ENV (whose packed representation is DEPTH), but is not eq?
 * with ID.
 */
SCM_EXPORT ScmObj
scm_wrap_identifier(ScmObj id, ScmPackedEnv depth, ScmObj env)
{
    scm_int_t id_depth;

    SCM_ASSERT(IDENTIFIERP(id));
    SCM_ASSERT(depth == scm_pack_env(env));

    if (FARSYMBOLP(id)) {
        /* Try to reduce lookup overhead. */
        id_depth = SCM_FARSYMBOL_ENV(id);
        SCM_ASSERT(id_depth <= depth);
        if (lookup_n_frames(id, depth - id_depth, env) == SCM_INVALID_REF) {
            /* ID hasn't been bound since it was captured. */
            return MAKE_FARSYMBOL(SCM_FARSYMBOL_SYM(id), id_depth);
        }
    }
    return MAKE_FARSYMBOL(id, depth);
}
#endif /* SCM_USE_HYGIENIC_MACRO */

/**
 * Construct new frame on an env
 *
 * @a formals and @a actuals must be valid.
 *
 * @param formals Symbol list as variable names of new frame. It accepts dotted
 *                list to handle function arguments directly.
 * @param actuals Arbitrary Scheme object list as values of new frame.
 *
 * @see scm_eval()
 */
SCM_EXPORT ScmObj
scm_extend_environment(ScmObj formals, ScmObj actuals, ScmObj env)
{
    ScmObj frame;
    DECLARE_INTERNAL_FUNCTION("scm_extend_environment");

    SCM_ASSERT(scm_valid_environment_extensionp(formals, actuals));
    SCM_ASSERT(VALID_ENVP(env));

    frame = CONS(formals, actuals);
    return CONS(frame, env);
}

/**
 * Replace entire content of newest frame of an env
 *
 * The environment must be replaced with returned one in caller side even if
 * this implementation returns identical to the one passed. This rule is
 * required to be compatible with future alternative implementations.
 */
SCM_EXPORT ScmObj
scm_replace_environment(ScmObj formals, ScmObj actuals, ScmObj env)
{
    ScmObj frame;
    DECLARE_INTERNAL_FUNCTION("scm_replace_environment");

    SCM_ASSERT(scm_valid_environment_extensionp(formals, actuals));
    SCM_ASSERT(VALID_ENVP(env));
    SCM_ASSERT(CONSP(env));

    frame = CAR(env);
    SET_CAR(frame, formals);
    SET_CDR(frame, actuals);

    return env;
}

/**
 * Replace all actuals of newest frame of an env
 *
 * The environment must be replaced with returned one in caller side even if
 * this implementation returns identical to the one passed. This rule is
 * required to be compatible with future alternative implementations.
 */
SCM_EXPORT ScmObj
scm_update_environment(ScmObj actuals, ScmObj env)
{
    ScmObj frame;
    DECLARE_INTERNAL_FUNCTION("scm_update_environment");

    SCM_ASSERT(VALID_ENVP(env));
    SCM_ASSERT(CONSP(env));

    frame = CAR(env);
    SCM_ASSERT(scm_valid_environment_extensionp(CAR(frame), actuals));
    SET_CDR(frame, actuals);

    return env;
}

/** Add a binding to newest frame of an env */
SCM_EXPORT ScmObj
scm_add_environment(ScmObj var, ScmObj val, ScmObj env)
{
    ScmObj frame, formals, actuals;
    DECLARE_INTERNAL_FUNCTION("scm_add_environment");

    SCM_ASSERT(IDENTIFIERP(var));
    SCM_ASSERT(VALID_ENVP(env));

    /* add (var, val) pair to most recent frame of the env */
    if (NULLP(env)) {
        frame = CONS(LIST_1(var), LIST_1(val));
        env = LIST_1(frame);
    } else if (CONSP(env)) {
        frame = CAR(env);
        formals = CONS(var, CAR(frame));
        actuals = CONS(val, CDR(frame));
        SET_CAR(frame, formals);
        SET_CDR(frame, actuals);
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
SCM_EXPORT ScmRef
scm_lookup_environment(ScmObj var, ScmObj env)
{
    ScmObj frame;
    ScmRef ref;
#if SCM_USE_HYGIENIC_MACRO
    scm_int_t depth, id_depth;
    ScmObj env_save;
#endif /* SCM_USE_HYGIENIC_MACRO */
    DECLARE_INTERNAL_FUNCTION("scm_lookup_environment");

    SCM_ASSERT(IDENTIFIERP(var));
    SCM_ASSERT(VALID_ENVP(env));

    /* lookup in frames */
#if SCM_USE_HYGIENIC_MACRO
    env_save = env;
    depth = 0;
#endif
    for (; !NULLP(env); env = CDR(env)) {
        frame = CAR(env);
        ref = scm_lookup_frame(var, frame);
        if (ref != SCM_INVALID_REF)
            return ref;
#if SCM_USE_HYGIENIC_MACRO
        ++depth;
#endif
    }
    SCM_ASSERT(NULLP(env));

#if SCM_USE_HYGIENIC_MACRO
    if (FARSYMBOLP(var)) {
        scm_int_t i;
        id_depth = SCM_FARSYMBOL_ENV(var);
        if (id_depth > depth)
            scm_macro_bad_scope(var);
        for (i = depth - id_depth; i--; )
            env_save = CDR(env_save);
        ref = lookup_n_frames(SCM_FARSYMBOL_SYM(var),
                              id_depth, env_save);
        SCM_ASSERT(ref != SCM_INVALID_REF || SYMBOLP(SCM_FARSYMBOL_SYM(var)));
        return ref;
    }
#endif

    return SCM_INVALID_REF;
}

/** Lookup a variable in a frame */
SCM_EXPORT ScmRef
scm_lookup_frame(ScmObj var, ScmObj frame)
{
    ScmObj formals;
    ScmRef actuals;
    DECLARE_INTERNAL_FUNCTION("scm_lookup_frame");

    SCM_ASSERT(IDENTIFIERP(var));
    SCM_ASSERT(valid_framep(frame));

    for (formals = CAR(frame), actuals = REF_CDR(frame);
         CONSP(formals);
         formals = CDR(formals), actuals = REF_CDR(DEREF(actuals)))
    {
        if (EQ(var, CAR(formals)))
            return REF_CAR(DEREF(actuals));
    }
    /* dotted list */
    if (EQ(var, formals))
        return actuals;

    return SCM_INVALID_REF;
}

ScmObj
scm_symbol_value(ScmObj var, ScmObj env)
{
    ScmRef ref;
    ScmObj val;
    DECLARE_INTERNAL_FUNCTION("scm_symbol_value");

    SCM_ASSERT(IDENTIFIERP(var));

    /* first, lookup the environment */
    ref = scm_lookup_environment(var, env);
    if (ref != SCM_INVALID_REF) {
        /* variable is found in environment, so returns its value */
        return DEREF(ref);
    }

#if SCM_USE_HYGIENIC_MACRO
    if (FARSYMBOLP(var))
        var = SCM_FARSYMBOL_SYM(var);
    SCM_ASSERT(SYMBOLP(var));
#endif

    /* finally, look at the VCELL */
    val = SCM_SYMBOL_VCELL(var);
    if (EQ(val, SCM_UNBOUND))
        ERR_OBJ("unbound variable", var);

    return val;
}

/*
 * Validators
 */
SCM_EXPORT scm_bool
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
    ScmObj formals, actuals;
    DECLARE_INTERNAL_FUNCTION("valid_framep");

    if (CONSP(frame)) {
        formals = CAR(frame);
        actuals = CDR(frame);
        if (scm_valid_environment_extensionp(formals, actuals))
            return scm_true;
    }
    return scm_false;
}

SCM_EXPORT scm_bool
scm_valid_environment_extensionp(ScmObj formals, ScmObj actuals)
{
    scm_int_t formals_len, actuals_len;

    formals_len = scm_validate_formals(formals);
    actuals_len = scm_validate_actuals(actuals);
    return scm_valid_environment_extension_lengthp(formals_len, actuals_len);
}

/* formals_len must be validated by scm_validate_formals() prior to here */
SCM_EXPORT scm_bool
scm_valid_environment_extension_lengthp(scm_int_t formals_len,
                                        scm_int_t actuals_len)
{
    if (SCM_LISTLEN_ERRORP(formals_len) || !SCM_LISTLEN_PROPERP(actuals_len))
        return scm_false;
    if (SCM_LISTLEN_DOTTEDP(formals_len)) {
        formals_len = SCM_LISTLEN_BEFORE_DOT(formals_len);
        return (formals_len <= actuals_len);
    }
    return (formals_len == actuals_len);
}

SCM_EXPORT scm_int_t
scm_validate_formals(ScmObj formals)
{
#if SCM_STRICT_ARGCHECK
    scm_int_t len;
    DECLARE_INTERNAL_FUNCTION("scm_validate_formals");

    /* This loop goes infinite if the formals is circular. SigSchme expects
     * that user codes are sane here. */
    for (len = 0; CONSP(formals); formals = CDR(formals), len++) {
        if (!IDENTIFIERP(CAR(formals)))
            return SCM_LISTLEN_ENCODE_ERROR(len);
    }
    if (NULLP(formals))
        return len;
    /* dotted list allowed */
    if (IDENTIFIERP(formals))
        return SCM_LISTLEN_ENCODE_DOTTED(len + 1);
    return SCM_LISTLEN_ENCODE_ERROR(len);
#else
    /* Crashless loose validation:
     * Regard any non-list object as symbol. Since the lookup operation search
     * for a variable by EQ, this is safe although loosely allows
     * R5RS-incompatible code. */
    return scm_finite_length(formals);
#endif
}

SCM_EXPORT scm_int_t
scm_validate_actuals(ScmObj actuals)
{
    scm_int_t len;

#if SCM_STRICT_ARGCHECK
    len = scm_length(actuals);
#else
    /* Crashless loose validation:
     * This loop goes infinite if the formals is circular. SigSchme expects
     * that user codes are sane here. */
    len = scm_finite_length(actuals);
#endif
    if (SCM_LISTLEN_DOTTEDP(len))
        len = SCM_LISTLEN_ENCODE_ERROR(len);
    return len;
}
