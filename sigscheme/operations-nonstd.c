/*===========================================================================
 *  FileName : operations-nonstd.c
 *  About    : SigScheme specific non standard operations
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

/*=======================================
  System Include
=======================================*/

/*=======================================
  Local Include
=======================================*/

/*=======================================
  File Local Macro Definitions
=======================================*/

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
/* io.c */
extern const char *scm_lib_path;

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj create_loaded_str(ScmObj filename);

/*=======================================
  Function Implementations
=======================================*/
/*
 * TODO:
 * - describe compatibility with de facto standard of other Scheme
 *   implementations (accept env as optional arg, etc)
 *
 * NOTE: Gauche 0.8.6 has deprecated symbol-bound? and is going to replace the
 * procedure with global-variable-bound?.
 */
/* The implementation is fully compatible with SIOD */
ScmObj ScmOp_symbol_boundp(ScmObj sym, ScmObj rest)
{
    ScmObj env = SCM_INVALID;
    DECLARE_FUNCTION("symbol-bound?", ProcedureVariadic1);

    ASSERT_SYMBOLP(sym);

    env = POP_ARG(rest);
    if (VALIDP(env))
        ASSERT_ENVP(env);
    else
        env = SCM_INTERACTION_ENV;

    return (!NULLP(Scm_LookupEnvironment(sym, env))
            || SCM_SYMBOL_BOUNDP(sym)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_sscm_backtrace(void)
{
    DECLARE_FUNCTION("%%backtrace", ProcedureFixed0);

    SigScm_ShowBacktrace(Scm_TraceStack());

    return SCM_UNDEF;
}

/* SIOD compatible */
ScmObj ScmOp_load_path(void)
{
    DECLARE_FUNCTION("load-path", ProcedureFixed0);
    return Scm_NewStringCopying(scm_lib_path);
}

/* FIXME: add ScmObj SigScm_require(const char *c_filename) */
ScmObj ScmOp_require(ScmObj filename)
{
    ScmObj loaded_str = SCM_FALSE;
#if SCM_COMPAT_SIOD
    ScmObj retsym     = SCM_FALSE;
#endif
    DECLARE_FUNCTION("require", ProcedureFixed1);

    ASSERT_STRINGP(filename);

    loaded_str = create_loaded_str(filename);
    if (FALSEP(ScmOp_providedp(loaded_str))) {
        ScmOp_load(filename);
        ScmOp_provide(loaded_str);
    }

#if SCM_COMPAT_SIOD
    retsym = Scm_Intern(SCM_STRING_STR(loaded_str));
    SCM_SYMBOL_SET_VCELL(retsym, SCM_TRUE);

    return retsym;
#else
    return SCM_TRUE;
#endif
}

static ScmObj create_loaded_str(ScmObj filename)
{
    char  *loaded_str = NULL;
    int    size = 0;

    /* generate loaded_str, contents is filename-loaded* */
    size = (strlen(SCM_STRING_STR(filename)) + strlen("*-loaded*") + 1);
    loaded_str = (char*)malloc(sizeof(char) * size);
    snprintf(loaded_str, size, "*%s-loaded*", SCM_STRING_STR(filename));

    return Scm_NewString(loaded_str);
}

/*
 * TODO: replace original specification with a SRFI standard or other de facto
 * standard
 */
ScmObj ScmOp_provide(ScmObj feature)
{
    DECLARE_FUNCTION("provide", ProcedureFixed1);

    ASSERT_STRINGP(feature);

    /* record to SigScm_features */
    SCM_SYMBOL_SET_VCELL(SigScm_features,
                         CONS(feature, SCM_SYMBOL_VCELL(SigScm_features)));

    return SCM_TRUE;
}

/*
 * TODO: replace original specification with a SRFI standard or other de facto
 * standard
 */
ScmObj ScmOp_providedp(ScmObj feature)
{
    ScmObj provided = SCM_FALSE;
    DECLARE_FUNCTION("provided?", ProcedureFixed1);

    ASSERT_STRINGP(feature);

    provided = ScmOp_member(feature, SCM_SYMBOL_VCELL(SigScm_features));

    return (NFALSEP(provided)) ? SCM_TRUE : SCM_FALSE;
}

/*
 * TODO: describe compatibility with de facto standard of other Scheme
 * implementations
 */
ScmObj ScmOp_file_existsp(ScmObj filepath)
{
    FILE *f;
    DECLARE_FUNCTION("file-exists?", ProcedureFixed1);

    ASSERT_STRINGP(filepath);

    f = fopen(SCM_STRING_STR(filepath), "r");
    if (!f)
        return SCM_FALSE;
    fclose(f);

    return SCM_TRUE;
}

/* TODO: remove to ensure security */
ScmObj ScmOp_delete_file(ScmObj filepath)
{
    DECLARE_FUNCTION("delete-file", ProcedureFixed1);

    ASSERT_STRINGP(filepath);

    if (remove(SCM_STRING_STR(filepath)) == -1)
        ERR_OBJ("delete failed. file = ", filepath);

    return SCM_TRUE;
}
