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
#include <stdio.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Declarations
=======================================*/
#if SCM_COMPAT_SIOD
#define SIOD_STRING "siod"
#endif

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static void repl(void);
static void repl_loop(void);
static int  is_repl_prompt(void);
static int  is_repl_show_result(void);

/*=======================================
  Function Implementations
=======================================*/
static void repl(void)
{
#if !SCM_GCC4_READY_GC
    ScmObj stack_start = NULL;
#endif

#if !SCM_GCC4_READY_GC
    /* start protecting stack */
    SigScm_GC_ProtectStack(&stack_start);
#endif

    /* goto repl loop */
    repl_loop();

#if !SCM_GCC4_READY_GC
    /* now no need to protect stack */
    SigScm_GC_UnprotectStack(&stack_start);
#endif
}

static void repl_loop(void)
{
    ScmObj s_exp  = SCM_NULL;
    ScmObj result = SCM_NULL;
    int is_prompt      = is_repl_prompt();
    int is_show_result = is_repl_show_result();

    if (is_prompt)
        printf("sscm> ");

    while (s_exp = SigScm_Read(scm_current_input_port), !EOFP(s_exp)) {
        result = EVAL(s_exp, SCM_INTERACTION_ENV);

        if (is_show_result)
        {
#if SCM_USE_SRFI38
            SigScm_WriteToPortWithSharedStructure(scm_current_output_port, result);
#else
            SigScm_WriteToPort(scm_current_output_port, result);
#endif
            printf("\n");
        }

        if (is_prompt)
            printf("sscm> ");
    }
}

static int is_repl_prompt(void)
{
    int ret = FALSE;
#if SCM_COMPAT_SIOD
    if (FALSEP(ScmOp_providedp(Scm_NewStringCopying(SIOD_STRING)))
        || SigScm_GetVerboseLevel() >= 2)
        ret = TRUE;
#else
    ret = TRUE;
#endif
    return ret;
}

static int is_repl_show_result(void)
{
    int ret = FALSE;
#if SCM_COMPAT_SIOD
    if (FALSEP(ScmOp_providedp(Scm_NewStringCopying(SIOD_STRING)))
        || SigScm_GetVerboseLevel() >= 1)
        ret = TRUE;
#else
    ret = TRUE;
#endif
    return ret;
}

int main(int argc, char **argv)
{
    char *filename = argv[1];

    SigScm_Initialize();

    if (argc < 2) {
#if SCM_GCC4_READY_GC
        SCM_GC_PROTECTED_CALL_VOID(repl, ());
#else
        repl();
#endif
        /*        SigScm_Error("usage : sscm <filename>"); */
    } else {
        SigScm_load(filename);
    }

    SigScm_Finalize();
    return 0;
}
