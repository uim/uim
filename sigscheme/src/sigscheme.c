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
#if SCM_USE_MULTIBYTE_CHAR
#include "encoding.h"
#else
#include "encoding-dummy.h"
#endif
#if SCM_USE_EVAL_C_STRING
#include "scmport-config.h"
#include "scmport.h"
#include "scmport-str.h"
#endif

/*=======================================
  File Local Macro Definitions
=======================================*/
#define ERRMSG_UNSUPPORTED_ENCODING "unsupported encoding"
#define ERRMSG_CODEC_SW_NOT_SUPPORTED                                        \
    "character encoding switching is not supported on this build"

#if !SCM_USE_CONTINUATION
#define scm_p_call_with_current_continuation NULL
#define scm_p_dynamic_wind                   NULL
#endif

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/
#include "functable-sscm-core.c"
#include "functable-r5rs-core.c"
#if SCM_USE_READER
#include "functable-r5rs-read.c"
#endif
#if SCM_USE_QUASIQUOTE
#include "functable-r5rs-qquote.c"
#endif
#if SCM_USE_PROMISE
#include "functable-r5rs-promise.c"
#endif
#if SCM_USE_NUMBER
#include "functable-r5rs-number.c"
#endif
#if (SCM_USE_NUMBER_IO && SCM_USE_STRING)
#include "functable-r5rs-number-io.c"
#endif
#if SCM_USE_CHAR
#include "functable-r5rs-char.c"
#endif
#if SCM_USE_STRING
#include "functable-r5rs-string.c"
#endif
#if SCM_USE_STRING_PROCEDURE
#include "functable-r5rs-string-procedure.c"
#endif
#if SCM_USE_VECTOR
#include "functable-r5rs-vector.c"
#endif
#if SCM_USE_DEEP_CADRS
#include "functable-r5rs-deep-cadrs.c"
#endif

SCM_GLOBAL_VARS_BEGIN(static_sigscheme);
#define static
static scm_bool l_scm_initialized;
#undef static
SCM_GLOBAL_VARS_END(static_sigscheme);
#define l_scm_initialized SCM_GLOBAL_VAR(static_sigscheme, l_scm_initialized)
SCM_DEFINE_STATIC_VARS(static_sigscheme);

static const char *const builtin_features[] = {
    "sigscheme",
#if SCM_USE_INTERNAL_DEFINITIONS
    "internal-definitions",
#endif
#if SCM_STRICT_TOPLEVEL_DEFINITIONS
    "strict-toplevel-definitions",
#endif
#if SCM_NESTED_CONTINUATION_ONLY
    "nested-continuation-only",
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
static void scm_initialize_internal(void);
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

    scm_encoding_init();
    scm_init_storage(storage_conf);

    SCM_GC_PROTECTED_CALL_VOID(scm_initialize_internal, ());

    l_scm_initialized = scm_true;
}

static void
scm_initialize_internal(void)
{
    const char *const *feature;

    /* size constraints */
    /* FIXME: check at compile-time */
    if (!((SCM_SAL_PTR_BITS <= SIZEOF_VOID_P * CHAR_BIT)
          && (SCM_SAL_CHAR_BITS <= SIZEOF_SCM_ICHAR_T * CHAR_BIT)
          && (SCM_SAL_INT_BITS <= SIZEOF_SCM_INT_T * CHAR_BIT)
          && (SCM_SAL_STRLEN_BITS <= SCM_SAL_INT_BITS)
          && (SCM_SAL_VECLEN_BITS <= SCM_SAL_INT_BITS)))
        scm_fatal_error("bit width constraints of the storage implementation are broken");

    if (!((SCM_SAL_CHAR_MAX <= SCM_ICHAR_T_MAX)
          && (SCM_INT_T_MIN <= SCM_SAL_INT_MIN
              && SCM_SAL_INT_MAX <= SCM_INT_T_MAX)
          && (SCM_SAL_STRLEN_MAX <= SCM_SAL_INT_MAX)
          && (SCM_SAL_VECLEN_MAX <= SCM_SAL_INT_MAX)))
        scm_fatal_error("size constraints of the storage implementation are broken");

    /*=======================================================================
      Core
    =======================================================================*/
    SCM_GLOBAL_VARS_INIT(procedure);
    SCM_GLOBAL_VARS_INIT(static_sigscheme);

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
#if SCM_USE_READER
    scm_register_funcs(scm_functable_r5rs_read);
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
    scm_gc_protect_with_init(&scm_values_applier, MAKE_CONTINUATION());

    /* SigScheme-specific core syntaxes and procedures */
    scm_register_funcs(scm_functable_sscm_core);

    /* R5RS Syntaxes */
    scm_init_syntax();
#if SCM_USE_QUASIQUOTE
    scm_register_funcs(scm_functable_r5rs_qquote);
#endif
#if SCM_USE_HYGIENIC_MACRO
    scm_init_macro();
#endif
#if SCM_USE_PROMISE
    scm_register_funcs(scm_functable_r5rs_promise);
#endif

    /* R5RS Procedures */
    scm_register_funcs(scm_functable_r5rs_core);
#if !SCM_USE_CONTINUATION
    SCM_SYMBOL_SET_VCELL(scm_intern("call-with-current-continuation"), SCM_UNBOUND);
    SCM_SYMBOL_SET_VCELL(scm_intern("call-with-values"), SCM_UNBOUND);
#endif
#if SCM_USE_NUMBER
    scm_register_funcs(scm_functable_r5rs_number);
#endif
#if (SCM_USE_NUMBER_IO && SCM_USE_STRING)
    scm_register_funcs(scm_functable_r5rs_number_io);
#endif
#if SCM_USE_CHAR
    scm_register_funcs(scm_functable_r5rs_char);
#endif
#if SCM_USE_STRING
    scm_register_funcs(scm_functable_r5rs_string);
#endif
#if SCM_USE_STRING_PROCEDURE
    scm_register_funcs(scm_functable_r5rs_string_procedure);
#endif
#if SCM_USE_VECTOR
    scm_register_funcs(scm_functable_r5rs_vector);
#endif
#if SCM_USE_DEEP_CADRS
    scm_register_funcs(scm_functable_r5rs_deep_cadrs);
#endif

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
}

SCM_EXPORT void
scm_finalize()
{
#if SCM_USE_LOAD
    scm_fin_load();
#endif
    scm_fin_module();
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

    return ret;
}
#endif /* SCM_USE_EVAL_C_STRING */

/* TODO: parse properly */
/* don't access ScmObj if (!l_scm_initialized) */
SCM_EXPORT char **
scm_interpret_argv(char **argv)
{
    char **argp, **rest;
    const char *encoding;
#if SCM_USE_MULTIBYTE_CHAR
    ScmCharCodec *specified_codec;
    ScmObj err_obj;
#endif
    DECLARE_INTERNAL_FUNCTION("scm_interpret_argv");

    encoding = NULL;
    argp = &argv[0];
    if (strcmp(argv[0], "/usr/bin/env") == 0)
        argp++;
    if (*argp)
        argp++;  /* skip executable name */

    /* parse options */
    for (; *argp; argp++) {
        if ((*argp)[0] != '-')
            break;  /* script name appeared */

        /* character encoding */
        if (strcmp(*argp, "-C") == 0) {
            encoding = *++argp;
            if (!encoding) {
                if (l_scm_initialized) {
                    scm_free_argv(argv);
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

    /* apply options */
    if (encoding) {
#if SCM_USE_MULTIBYTE_CHAR
        specified_codec = scm_mb_find_codec(encoding);
        if (!specified_codec) {
            if (l_scm_initialized) {
                err_obj = CONST_STRING(encoding);
                scm_free_argv(argv);
                ERR_OBJ(ERRMSG_UNSUPPORTED_ENCODING, err_obj);
            } else {
                fprintf(stderr,
                        SCM_ERR_HEADER ERRMSG_UNSUPPORTED_ENCODING ": %s\n",
                        encoding);
                exit(EXIT_FAILURE);
            }
        }
        scm_current_char_codec = specified_codec;
#else
        if (l_scm_initialized) {
            scm_free_argv(argv);
            PLAIN_ERR(ERRMSG_CODEC_SW_NOT_SUPPORTED);
        } else {
            fprintf(stderr, SCM_ERR_HEADER ERRMSG_CODEC_SW_NOT_SUPPORTED "\n");
            exit(EXIT_FAILURE);
        }
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
