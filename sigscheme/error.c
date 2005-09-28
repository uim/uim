/*===========================================================================
 *  FileName : error.c
 *  About    : handling errors
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
#include <stdarg.h>

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
#define SCM_ERR_HEADER "Error: "
#define SCM_BACKTRACE_HEADER "**** BACKTRACE ****\n"

/*=======================================
  Variable Declarations
=======================================*/
ScmObj scm_current_error_port  = NULL;

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Implementations
=======================================*/
int SigScm_Die(const char *msg, const char *filename, int line) {
    if (SigScm_DebugCategories() & SCM_DBG_ERRMSG) {
        SigScm_ShowErrorHeader();

        fprintf(SCM_PORTINFO_FILE(scm_current_error_port),
                "SigScheme Died : %s (file : %s, line : %d)\n",
                msg, filename, line);
    }

    if (SigScm_DebugCategories() & SCM_DBG_BACKTRACE)
        SigScm_ShowBacktrace();

    /* TODO: doesn't exit here */
    exit(-1);

    return -1;
}

void SigScm_Error(const char *msg, ...)
{
    va_list va;

    if (SigScm_DebugCategories() & SCM_DBG_ERRMSG) {
        SigScm_ShowErrorHeader();

        va_start(va, msg);
        vfprintf(SCM_PORTINFO_FILE(scm_current_error_port), msg, va);
        va_end(va);

        fprintf(SCM_PORTINFO_FILE(scm_current_error_port), "\n");
    }

    if (SigScm_DebugCategories() & SCM_DBG_BACKTRACE)
        SigScm_ShowBacktrace();

    /* TODO: doesn't exit here */
    exit(-1);
}

void SigScm_ErrorObj(const char *msg, ScmObj obj)
{
    if (SigScm_DebugCategories() & SCM_DBG_ERRMSG) {
        SigScm_ShowErrorHeader();

        /* print msg */
        fprintf(SCM_PORTINFO_FILE(scm_current_error_port), "%s", msg);

        /* print obj */
        SigScm_WriteToPort(scm_current_error_port, obj);
        fprintf(SCM_PORTINFO_FILE(scm_current_error_port), "\n");
    }
   
    if (SigScm_DebugCategories() & SCM_DBG_BACKTRACE)
        SigScm_ShowBacktrace();
 
    /* TODO: doesn't exit here*/
    exit(-1);
}

void SigScm_ShowBacktrace(void)
{
#if SCM_DEBUG
    struct trace_frame *f;

    /* show header */
    fprintf(SCM_PORTINFO_FILE(scm_current_error_port), SCM_BACKTRACE_HEADER);

    /* show each frame's obj */
    for (f = scm_trace_root; f; f = f->prev) {
        SigScm_WriteToPort(scm_current_error_port, f->obj);
        
        fprintf(SCM_PORTINFO_FILE(scm_current_error_port), "\n");
    }
#endif
}

void SigScm_ShowErrorHeader(void)
{
    fprintf(SCM_PORTINFO_FILE(scm_current_error_port), SCM_ERR_HEADER);
}
