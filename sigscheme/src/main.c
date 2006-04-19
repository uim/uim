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

#if BREW_MAJ_VER  /* FIXME: inappropriate detection method */
#include "sigscheme-combined.c"
#endif

#include <stdlib.h>

#include <unistd.h>
#include <sys/param.h>

#if BREW_MAJ_VER  /* FIXME: inappropriate detection method */
#include "AEEAppGen.h"
#include "AEEStdLib.h"
#endif

#include "sigscheme.h"
#include "sigschemeinternal.h"
#include "scmport-config.h"
#include "scmport.h"

/*=======================================
  File Local Macro Definitions
=======================================*/
#define PROMPT_STR "sscm> "

#if SCM_COMPAT_SIOD
#define FEATURE_ID_SIOD "siod"
#endif

/*=======================================
  File Local Type Definitions
=======================================*/
struct g_sscm {
#if SCM_COMPAT_SIOD
    ScmObj feature_id_siod;
#endif
    char lib_path[MAXPATHLEN + sizeof("")];
};

#if BREW_MAJ_VER  /* FIXME: inappropriate detection method */
/* experimental, broken and existing for technical example */

#define SCM_BREW_USER_APPLET_T CSSCMApplet
typedef struct _CSSCMApplet CSSCMApplet;
struct _CSSCMApplet {
    AEEApplet a;
    struct scm_g_aggregated m_scm_g_aggregated_instance;

    struct g_sscm m_sscm;
};

#define sscm (((CSSCMApplet *)GETAPPINSTANCE())->m_sscm)
#endif /* BREW_MAJ_VER */

/*=======================================
  Variable Declarations
=======================================*/
/* Don't use any global variable other than the 'sscm' */
#if !BREW_MAJ_VER  /* FIXME: inappropriate detection method */
static struct g_sscm sscm;
#endif /* !BREW_MAJ_VER */

/*=======================================
  File Local Function Declarations
=======================================*/
static void repl(void);
static void repl_loop(void);
static scm_bool show_promptp(void);

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
    ScmEvalState eval_state;
    ScmBaseCharPort *cport;
    ScmBytePort *bport;
    ScmObj sexp, result;
#if SCM_USE_SRFI34
    ScmObj sym_guard, cond_catch, proc_read, proc_eval, err;

    proc_read = scm_symbol_value(scm_intern("read"), SCM_INTERACTION_ENV);
    proc_eval = scm_symbol_value(scm_intern("eval"), SCM_INTERACTION_ENV);
    err = CONS(SCM_UNDEF, SCM_UNDEF); /* unique ID */

    /* prepare the constant part of the form to get the loop fast */
    sym_guard = scm_intern("guard");
    cond_catch = LIST_2(scm_intern("err"),
                        LIST_3(scm_intern("else"),
                               LIST_2(scm_intern("%%inspect-error"),
                                      scm_intern("err")),
                               LIST_2(SCM_SYM_QUOTE, err)));
#endif /* SCM_USE_SRFI34 */

    for (;;) {
        if (show_promptp())
            scm_port_puts(scm_out, PROMPT_STR);

#if SCM_USE_SRFI34
        /* error-proof read */
        SCM_EVAL_STATE_INIT1(eval_state, SCM_INTERACTION_ENV);
        sexp = scm_s_srfi34_guard(cond_catch,
                                  LIST_1(LIST_2(proc_read, scm_in)),
                                  &eval_state);
        sexp = SCM_FINISH_TAILREC_CALL(sexp, &eval_state);
        if (EOFP(sexp))
            break;

        /* parse error */
        if (EQ(sexp, err)) {
            cport = SCM_CHARPORT_DYNAMIC_CAST(ScmBaseCharPort,
                                              SCM_PORT_IMPL(scm_in));
            if (cport) {
                bport = cport->bport;
                /* discard all available input */
                while (SCM_BYTEPORT_BYTE_READYP(bport))
                    SCM_BYTEPORT_GET_BYTE(bport);
                continue;
            }
            PLAIN_ERR("unrecoverable parse error");
        }

        /*
         * Error-proof evaluation
         *
         * (guard (err
         *         (else
         *          (%%inspect-error err)
         *          #<err>))
         *   (eval (quote sexp) (interaction-environment)))
         *
         * To allow redefinition of 'guard' and '%%inspect-err', surely access
         * them via symbol instead of prepared syntax or procedure object.
         */
        SCM_EVAL_STATE_INIT1(eval_state, SCM_INTERACTION_ENV);
        result = scm_s_srfi34_guard(cond_catch,
                                    LIST_1(LIST_3(proc_eval,
                                                  LIST_2(SYM_QUOTE, sexp),
                                                  SCM_INTERACTION_ENV)),
                                    &eval_state);
        result = SCM_FINISH_TAILREC_CALL(result, &eval_state);

        if (!EQ(result, err)) {
            SCM_WRITE_SS(scm_out, result);
            scm_port_newline(scm_out);
        }
#else /* SCM_USE_SRFI34 */
        sexp = scm_read(scm_in);
        if (EOFP(sexp))
            break;

        result = EVAL(sexp, SCM_INTERACTION_ENV);
        SCM_WRITE_SS(scm_out, result);
        scm_port_newline(scm_out);
#endif /* SCM_USE_SRFI34 */
    }
}

static scm_bool
show_promptp(void)
{
#if SCM_COMPAT_SIOD
    return (FALSEP(scm_p_providedp(sscm.feature_id_siod))
            || scm_get_verbose_level() >= 2);
#else
    return scm_true;
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

    scm_initialize(NULL);

    /* Explicitly allow current directory-relative path. The sscm command is
     * supposed to neither setuid'ed nor setgid'ed. So the privilege escalation
     * problem for C plugins shall not occur. -- YamaKen 2006-03-25 */
    /*
     * FIXME:
     * - add multiple path capability to libsscm
     * - add library path specifying way for users
     * - support non-UNIX platforms
     */
    if (!getcwd(sscm.lib_path, MAXPATHLEN))
        return EXIT_FAILURE;
    scm_set_lib_path(sscm.lib_path);

#if SCM_USE_SRFI34
    scm_use("srfi-34");
#endif

#if SCM_COMPAT_SIOD
    scm_gc_protect_with_init(&sscm.feature_id_siod,
                             CONST_STRING(FEATURE_ID_SIOD));
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
