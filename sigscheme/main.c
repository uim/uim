/*===========================================================================
 *  FileName : main.c
 *  About    : main function
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
static void repl(void)
{
#if !SCM_GCC4_READY_GC
    ScmObj stack_start = NULL;
#endif

#if !SCM_GCC4_READY_GC
    SigScm_GC_ProtectStack(&stack_start);
#endif

    repl_loop();

#if !SCM_GCC4_READY_GC
    SigScm_GC_UnprotectStack(&stack_start);
#endif
}

static void repl_loop(void)
{
    ScmObj sexp, result;
#if SCM_USE_SRFI34
    ScmObj sym_guard, cond_catch, proc_read, err;

    proc_read = Scm_SymbolValue(Scm_Intern("read"), SCM_INTERACTION_ENV);
    err = CONS(SCM_UNDEF, SCM_UNDEF); /* unique ID */

    /* prepare the constant part of the form to get the loop fast */
    sym_guard = Scm_Intern("guard");
    cond_catch = LIST_2(Scm_Intern("err"),
                        LIST_3(SYM_ELSE,
                               LIST_2(Scm_Intern("%%inspect-error"),
                                      Scm_Intern("err")),
                               LIST_2(SCM_SYM_QUOTE, err)));
#endif /* SCM_USE_SRFI34 */

    for (;;) {
        if (show_promptp())
            SCM_PORT_PUTS(scm_current_output_port, PROMPT_STR);

#if SCM_USE_SRFI34
        /* error-proof read */
        sexp = EVAL(LIST_3(sym_guard, cond_catch,
                           LIST_2(proc_read, scm_current_input_port)),
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
        sexp = SigScm_Read(scm_current_input_port)
        if (EOFP(sexp))
            break;

        result = EVAL(sexp, SCM_INTERACTION_ENV);
#endif /* SCM_USE_SRFI34 */

        if (!EQ(result, err)) {
            SCM_WRITESS_TO_PORT(scm_current_output_port, result);
            SigScm_PortNewline(scm_current_output_port);
        }
    }
}

static int show_promptp(void)
{
#if SCM_COMPAT_SIOD
    return (FALSEP(ScmOp_providedp(feature_id_siod))
            || SigScm_GetVerboseLevel() >= 2);
#else
    return TRUE;
#endif
}

int main(int argc, char **argv)
{
    const char *filename;
    char **rest_argv;

    /* must be done before SigScm_Initialize() */
    rest_argv = Scm_InterpretArgv(argv);
    filename = rest_argv[0];

    SigScm_Initialize();

#if SCM_USE_SRFI34
    Scm_Use("srfi-34");
#endif

#if SCM_COMPAT_SIOD
    SigScm_GC_Protect(&feature_id_siod);
    feature_id_siod = Scm_NewImmutableStringCopying(FEATURE_ID_SIOD);
#endif

    if (filename) {
        SigScm_load(filename);
    } else {
#if SCM_GCC4_READY_GC
        SCM_GC_PROTECTED_CALL_VOID(repl, ());
#else
        repl();
#endif
        /* ERR("usage: sscm <filename>"); */
    }

    SigScm_Finalize();
    return EXIT_SUCCESS;
}
