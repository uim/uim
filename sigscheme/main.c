/*===========================================================================
 *  FileName : main.c
 *  About    : main function
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
/*=======================================
  System Include
=======================================*/
#include <stdlib.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Declarations
=======================================*/
#define PROMPT_STR "sscm> "

#if SCM_COMPAT_SIOD
#define FEATURE_ID_SIOD "siod"
#endif

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
#if SCM_COMPAT_SIOD
static ScmObj feature_id_siod;
#endif

/*=======================================
  File Local Function Declarations
=======================================*/
static void repl(void);
static void repl_loop(void);
static int show_promptp(void);

/*=======================================
  Function Implementations
=======================================*/
static void
repl(void)
{
#if !SCM_GCC4_READY_GC
    ScmObj stack_start = NULL;
#endif

#if !SCM_GCC4_READY_GC
    scm_gc_protect_stack(&stack_start);
#endif

    repl_loop();

#if !SCM_GCC4_READY_GC
    scm_gc_unprotect_stack(&stack_start);
#endif
}

static void
repl_loop(void)
{
    ScmObj sexp, result;
#if SCM_USE_SRFI34
    ScmObj sym_guard, cond_catch, proc_read, err;

    proc_read = scm_symbol_value(scm_intern("read"), SCM_INTERACTION_ENV);
    err = CONS(SCM_UNDEF, SCM_UNDEF); /* unique ID */

    /* prepare the constant part of the form to get the loop fast */
    sym_guard = scm_intern("guard");
    cond_catch = LIST_2(scm_intern("err"),
                        LIST_3(SYM_ELSE,
                               LIST_2(scm_intern("%%inspect-error"),
                                      scm_intern("err")),
                               LIST_2(SCM_SYM_QUOTE, err)));
#endif /* SCM_USE_SRFI34 */

    for (;;) {
        if (show_promptp())
            scm_port_puts(scm_out, PROMPT_STR);

#if SCM_USE_SRFI34
        /* error-proof read */
        sexp = EVAL(LIST_3(sym_guard, cond_catch,
                           LIST_2(proc_read, scm_in)),
                    SCM_INTERACTION_ENV);
        if (EOFP(sexp))
            break;
        if (EQ(sexp, err))
            continue;

        /*
         * Error-proof evaluation
         *
         * (guard (err
         *         (else
         *          (%%inspect-error err)
         *          #<err>))
         *   exp)
         *
         * To allow redefinition of 'guard' and '%%inspect-err', surely access
         * them via symbol instead of prepared syntax or procedure object.
         */
        result = EVAL(LIST_3(sym_guard, cond_catch, sexp),
                      SCM_INTERACTION_ENV);
#else /* SCM_USE_SRFI34 */
        sexp = scm_read(scm_in)
        if (EOFP(sexp))
            break;

        result = EVAL(sexp, SCM_INTERACTION_ENV);
#endif /* SCM_USE_SRFI34 */

        if (!EQ(result, err)) {
            SCM_WRITESS_TO_PORT(scm_out, result);
            scm_port_newline(scm_out);
        }
    }
}

static int
show_promptp(void)
{
#if SCM_COMPAT_SIOD
    return (FALSEP(scm_p_providedp(feature_id_siod))
            || scm_get_verbose_level() >= 2);
#else
    return TRUE;
#endif
}

int
main(int argc, char **argv)
{
    const char *filename;
    char **rest_argv;

    /* must be done before scm_initialize() */
    rest_argv = scm_interpret_argv(argv);
    filename = rest_argv[0];

    scm_initialize();

#if SCM_USE_SRFI34
    scm_use("srfi-34");
#endif

#if SCM_COMPAT_SIOD
    scm_gc_protect_with_init(&feature_id_siod,
                             MAKE_IMMUTABLE_STRING_COPYING(FEATURE_ID_SIOD));
#endif

    if (filename) {
        scm_load(filename);
    } else {
#if SCM_GCC4_READY_GC
        SCM_GC_PROTECTED_CALL_VOID(repl, ());
#else
        repl();
#endif
        /* ERR("usage: sscm <filename>"); */
    }

    scm_finalize();
    return EXIT_SUCCESS;
}
