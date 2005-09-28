/*===========================================================================
 *  FileName : operations-srfi23.c
 *  About    : srfi23 Error reporting mechanism
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
 *  =========================================================================*/
/*=======================================
  System Include
=======================================*/

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

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Implementations
=======================================*/
/*=============================================================================
  SRFI23 : Error reporting mechanism
=============================================================================*/
ScmObj ScmOp_SRFI23_error(ScmObj args, ScmObj env)
{
    if (NULLP(args))
        SigScm_Error("error : at least 1 argument required");

    if (!STRINGP(CAR(args)))
        SigScm_ErrorObj("error : first argument should be string but got ",
                        CAR(args));
    
    /* prepend header */
    SigScm_ShowErrorHeader();

    /* show message */
    fprintf(SCM_PORTINFO_FILE(scm_current_error_port),
            "%s", SCM_STRING_STR(CAR(args)));

    /* show each obj */
    for (args = CDR(args); !NULLP(args); args = CDR(args)) {
        fprintf(SCM_PORTINFO_FILE(scm_current_error_port), " ");

#if SCM_USE_SRFI38
        SigScm_WriteToPortWithSharedStructure(scm_current_error_port,
                                              CAR(args));
#else
        SigScm_WriteToPort(scm_current_error_port, CAR(args));
#endif
    }

    /* new line */
    fprintf(SCM_PORTINFO_FILE(scm_current_error_port), "\n");

    /* show backtrace */
    SigScm_ShowBacktrace();

    /* TODO: doesn't exit here? */
    exit(-1);

    return SCM_UNDEF;
}
