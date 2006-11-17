/*===========================================================================
 *  Filename : sigscheme.c
 *  About    : Client interfaces
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"
#include "encoding.h"
#if SCM_USE_EVAL_C_STRING
#include "scmport-config.h"
#include "scmport.h"
#include "scmport-str.h"
#endif

/*=======================================
  File Local Macro Definitions
=======================================*/

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/
#include "functable-sscm-core.c"
#include "functable-r5rs-procedure.c"

SCM_GLOBAL_VARS_BEGIN(static_sigscheme);
#define static
static scm_bool l_scm_initialized;

#if SCM_COMPAT_SIOD
static ScmObj l_scm_return_value_cache;
#endif /* SCM_COMPAT_SIOD */
#undef static
SCM_GLOBAL_VARS_END(static_sigscheme);
#define l_scm_initialized SCM_GLOBAL_VAR(static_sigscheme, l_scm_initialized)
#define l_scm_return_value_cache                                             \
    SCM_GLOBAL_VAR(static_sigscheme, l_scm_return_value_cache)
SCM_DEFINE_STATIC_VARS(static_sigscheme);

static const char *const builtin_features[] = {
    "sigscheme",
#if SCM_USE_INTERNAL_DEFINITIONS
    "internal-definitions",
#endif
#if SCM_STRICT_TOPLEVEL_DEFINITIONS
    "strict-toplevel-definitions",
#endif
#if SCM_STRICT_R5RS
    "strict-r5rs",
#endif
#if SCM_STRICT_ARGCHECK
    "strict-argcheck",
#endif
#if SCM_STRICT_NULL_FORM
    "strict-null-form",
#endif
#if SCM_STRICT_VECTOR_FORM
    "strict-vector-form",
#endif
#if SCM_STRICT_ENCODING_CHECK
    "strict-encoding-check",
#endif
#if (SCM_CONST_LIST_LITERAL && SCM_HAS_IMMUTABLE_CONS)
    "const-list-literal",
#endif
#if (SCM_CONST_VECTOR_LITERAL && SCM_HAS_IMMUTABLE_VECTOR)
    "const-vector-literal",
#endif
#if SCM_USE_DEEP_CADRS
    "deep-cadrs",
#endif
#if SCM_COMPAT_SIOD
    "compat-siod",
#endif
#if SCM_COMPAT_SIOD_BUGS
    "siod-bugs",
#endif
#if SCM_USE_NULL_CAPABLE_STRING
    "null-capable-string",
#endif
#if SCM_HAS_IMMEDIATE_CHAR_ONLY
    "immediate-char-only",
#endif
#if SCM_HAS_IMMEDIATE_NUMBER_ONLY
    "immediate-number-only",
#endif
#if SCM_USE_MULTIBYTE_CHAR
    "multibyte-char",
#endif
#if SCM_USE_UTF8
    "utf-8",
#endif
    NULL
};

/*=======================================
  File Local Function Declarations
=======================================*/
static void scm_initialize_internal(const ScmStorageConf *storage_conf);
#if SCM_USE_EVAL_C_STRING
static ScmObj scm_eval_c_string_internal(const char *exp);
#endif

/*=======================================
  Function Definitions
=======================================*/
/**
 * Initialize the interpreter
 *
 * @param storage_conf Storage configuration parameters. NULL instructs
 *                     default.
 */
SCM_EXPORT void
scm_initialize(const ScmStorageConf *storage_conf)
{
    SCM_AGGREGATED_GLOBAL_VARS_INIT();

    SCM_GC_PROTECTED_CALL_VOID(scm_initialize_internal, (storage_conf));
}

static void
scm_initialize_internal(const ScmStorageConf *storage_conf)
{
    const char *const *feature;

    /*=======================================================================
      Core
    =======================================================================*/
    SCM_GLOBAL_VARS_INIT(procedure);
    SCM_GLOBAL_VARS_INIT(static_sigscheme);

    scm_encoding_init();
    scm_init_storage(storage_conf);

    scm_init_error();
    scm_set_debug_categories(SCM_DBG_ERRMSG | SCM_DBG_BACKTRACE
                             | scm_predefined_debug_categories());

#if SCM_USE_PORT
    scm_init_port();
#endif
#if SCM_USE_WRITER
    scm_init_writer();
#endif
#if SCM_USE_FORMAT
    /* FIXME: duplicate call with scm_initialize_srfi{28,48}() */
    scm_init_format();
#endif
#if SCM_USE_LOAD
    scm_init_load();
#endif
    scm_init_module();

    /* fallback to unibyte */
    scm_identifier_codec = scm_mb_find_codec("UTF-8");

    /*=======================================================================
      Register Built-in Functions
    =======================================================================*/
    /* pseudo procedure to deliver multiple values to an arbitrary procedure
     * (assigns an invalid continuation as unique ID) */
    scm_gc_protect_with_init((ScmObj *)&scm_values_applier,
                             MAKE_CONTINUATION());

    /* SigScheme-specific core syntaxes and procedures */
    scm_register_funcs(scm_sscm_core_func_info_table);

    /* R5RS Syntaxes */
    scm_init_syntax();
#if SCM_USE_HYGIENIC_MACRO
    scm_init_macro();
#endif

    /* R5RS Procedures */
    scm_register_funcs(scm_r5rs_procedure_func_info_table);

#if SCM_USE_SSCM_EXTENSIONS
    scm_use("sscm-ext");
#endif
#if SCM_USE_EVAL_C_STRING
    scm_use("srfi-6");
#endif

    /*=======================================================================
      Fixing up
    =======================================================================*/
    /* to evaluate SigScheme-dependent scheme codes conditionally */
    for (feature = &builtin_features[0]; *feature; feature++)
        scm_provide(CONST_STRING(*feature));

    /* Since SCM_SAL_PTR_BITS may use sizeof() instead of autoconf SIZEOF
     * macro, #if is not safe here. */
    if (SCM_PTR_BITS == 64)
        scm_provide(CONST_STRING("64bit-addr"));

#if SCM_NESTED_CONTINUATION_ONLY
    scm_provide(CONST_STRING("nested-continuation-only"));
#endif

    l_scm_initialized = scm_true;
}

SCM_EXPORT void
scm_finalize()
{
    scm_fin_storage();
    l_scm_initialized = scm_false;

    SCM_GLOBAL_VARS_FIN(procedure);
    SCM_GLOBAL_VARS_FIN(static_sigscheme);
    SCM_AGGREGATED_GLOBAL_VARS_FIN();
}

#if SCM_USE_EVAL_C_STRING
SCM_EXPORT ScmObj
scm_eval_c_string(const char *exp)
{
    ScmObj ret;

    SCM_GC_PROTECTED_CALL(ret, ScmObj, scm_eval_c_string_internal, (exp));

    return ret;
}

static ScmObj
scm_eval_c_string_internal(const char *exp)
{
    ScmObj str_port, ret;
    ScmBytePort *bport;
    ScmCharPort *cport;

    bport = ScmInputStrPort_new_const(exp, NULL);
    cport = scm_make_char_port(bport);
    str_port = MAKE_PORT(cport, SCM_PORTFLAG_INPUT);

    ret = scm_read(str_port);
    ret = EVAL(ret, SCM_INTERACTION_ENV);

#if SCM_COMPAT_SIOD
    l_scm_return_value_cache = ret;
#endif

    return ret;
}
#endif /* SCM_USE_EVAL_C_STRING */

#if SCM_COMPAT_SIOD
SCM_EXPORT ScmObj
scm_return_value(void)
{
    return l_scm_return_value_cache;
}
#endif

/* TODO: parse properly */
/* don't access ScmObj if (!l_scm_initialized) */
SCM_EXPORT char **
scm_interpret_argv(char **argv)
{
    char **argp, **rest;
    const char *encoding;
    ScmCharCodec *specified_codec;
    ScmObj err_obj;
    DECLARE_INTERNAL_FUNCTION("scm_interpret_argv");

    encoding = NULL;
    argp = (strcmp(argv[0], "/usr/bin/env") == 0) ? &argv[2] : &argv[1];

    for (; *argp; argp++) {
        /* script name appeared */
        if (*argp[0] != '-')
            break;

        /* character encoding */
        if (strcmp(*argp, "-C") == 0) {
            encoding = *++argp;
            if (!*argp) {
                if (l_scm_initialized) {
                    ERR("no encoding name specified");
                } else {
                    fputs(SCM_ERR_HEADER "no encoding name specified\n",
                          stderr);
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    rest = argp;

    if (encoding) {
#if SCM_USE_MULTIBYTE_CHAR
        specified_codec = scm_mb_find_codec(encoding);
        if (!specified_codec) {
            if (l_scm_initialized) {
                err_obj = CONST_STRING(encoding);
                scm_free_argv(argv);
                ERR_OBJ("unsupported encoding", err_obj);
            } else {
                fprintf(stderr, SCM_ERR_HEADER "unsupported encoding: %s\n",
                        encoding);
                exit(EXIT_FAILURE);
            }
        }
        scm_current_char_codec = specified_codec;
#else
        fprintf(stderr, SCM_ERR_HEADER "encoding switching is not supported on this build\n");
#endif
    }

    return rest;
}

SCM_EXPORT void
scm_free_argv(char **argv)
{
    char **argp;

    for (argp = &argv[0]; *argp; argp++) {
        free(*argp);
    }
    free(argv);
}
