/*===========================================================================
 *  FileName : sigscheme.c
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

#include "config.h"

/*=======================================
  System Include
=======================================*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"
#include "encoding.h"
#if SCM_USE_EVAL_C_STRING
#include "baseport.h"
#include "strport.h"
#endif

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
#include "functable-sscm-core.c"
#include "functable-r5rs-procedure.c"

static scm_bool scm_initialized;

#if SCM_COMPAT_SIOD
static ScmObj scm_return_value_cache    = NULL;
#endif

/*=======================================
  File Local Function Declarations
=======================================*/
static void scm_initialize_internal(const ScmStorageConf *storage_conf);
#if SCM_USE_EVAL_C_STRING
static ScmObj scm_eval_c_string_internal(const char *exp);
#endif

/*=======================================
  Function Implementations
=======================================*/
/**
 * Initialize the interpreter
 *
 * @param storage_conf Storage configuration parameters. NULL instructs
 *                     default.
 */
void
scm_initialize(const ScmStorageConf *storage_conf)
{
#if SCM_GCC4_READY_GC
    SCM_GC_PROTECTED_CALL_VOID(scm_initialize_internal, (storage_conf));
#else
    ScmObj stack_start;

    scm_gc_protect_stack(&stack_start);
    scm_initialize_internal();
    scm_gc_unprotect_stack(&stack_start);
#endif
}

static void
scm_initialize_internal(const ScmStorageConf *storage_conf)
{
    /*=======================================================================
      Core
    =======================================================================*/
    scm_set_debug_categories(SCM_DBG_ERRMSG | SCM_DBG_BACKTRACE
                              | scm_predefined_debug_categories());
    scm_init_storage(storage_conf);
    scm_init_error();
    scm_init_port();
    scm_init_module();

    /* fallback to unibyte */
    scm_identifier_codec = scm_mb_find_codec("UTF-8");

    /*=======================================================================
      Register Built-in Functions
    =======================================================================*/
    /* SigScheme-specific core syntaxes and procedures */
    scm_register_funcs(scm_sscm_core_func_info_table);

    /* R5RS Syntaxes */
    scm_init_syntax();

    /* R5RS Procedures */
    scm_register_funcs(scm_r5rs_procedure_func_info_table);

#if SCM_USE_NONSTD_FEATURES
    scm_use("sscm");
#endif

    /*=======================================================================
      Fixing up
    =======================================================================*/
    /* to evaluate SigScheme-dependent codes conditionally */
    scm_provide(CONST_STRING("sigscheme"));
#if SCM_STRICT_R5RS
    scm_provide(CONST_STRING("strict-r5rs"));
#endif
#if SCM_STRICT_ARGCHECK
    scm_provide(CONST_STRING("strict-argcheck"));
#endif
#if (SCM_CONST_LIST_LITERAL && SCM_HAS_IMMUTABLE_CONS)
    scm_provide(CONST_STRING("const-list-literal"));
#endif
#if (SCM_CONST_VECTOR_LITERAL && SCM_HAS_IMMUTABLE_VECTOR)
    scm_provide(CONST_STRING("const-vector-literal"));
#endif
#if SCM_COMPAT_SIOD_BUGS
    scm_provide(CONST_STRING("siod-bugs"));
#endif
#if SCM_USE_NULL_CAPABLE_STRING
    scm_provide(CONST_STRING("null-capable-string"));
#endif
#if SCM_HAS_IMMEDIATE_CHAR_ONLY
    scm_provide(CONST_STRING("immediate-char-only"));
#endif
#if SCM_HAS_IMMEDIATE_NUMBER_ONLY
    scm_provide(CONST_STRING("immediate-number-only"));
#endif
    /* Since SCM_SAL_PTR_BITS may use sizeof() instead of autoconf SIZEOF
     * macro, #if is not safe here. */
    if (SCM_PTR_BITS == 64)
        scm_provide(CONST_STRING("64bit-addr"));

    scm_initialized = scm_true;
}

void
scm_finalize()
{
    scm_finalize_storage();
    scm_initialized = scm_false;
}

#if SCM_USE_EVAL_C_STRING
ScmObj
scm_eval_c_string(const char *exp)
{
#if !SCM_GCC4_READY_GC
    ScmObj stack_start;
#endif
    ScmObj ret;

#if SCM_GCC4_READY_GC
    SCM_GC_PROTECTED_CALL(ret, ScmObj, scm_eval_c_string_internal, (exp));
#else
    /* start protecting stack */
    scm_gc_protect_stack(&stack_start);

    ret = scm_eval_c_string_internal(exp);

    /* now no need to protect stack */
    scm_gc_unprotect_stack(&stack_start);
#endif

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
    scm_return_value_cache = ret;
#endif

    return ret;
}
#endif /* SCM_USE_EVAL_C_STRING */

#if SCM_COMPAT_SIOD
ScmObj
scm_return_value(void)
{
    return scm_return_value_cache;
}
#endif

/* TODO: parse properly */
/* don't access ScmObj if (!scm_initialized) */
char **
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
                if (scm_initialized) {
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
            if (scm_initialized) {
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

void
scm_free_argv(char **argv)
{
    char **argp;

    for (argp = &argv[0]; *argp; argp++) {
        free(*argp);
    }
    free(argv);
}
