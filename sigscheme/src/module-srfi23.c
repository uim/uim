/*===========================================================================
 *  Filename : module-srfi23.c
 *  About    : SRFI-23 Error reporting mechanism
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

#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Definitions
=======================================*/

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/
#include "functable-srfi23.c"

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Definitions
=======================================*/
SCM_EXPORT void
scm_initialize_srfi23(void)
{
    scm_register_funcs(scm_srfi23_func_info_table);
}

/*===========================================================================
  SRFI23 : Error reporting mechanism
===========================================================================*/
/*
 * This code implements the '4.' of following Specification defined in SRFI-34.
 *
 * 1. Display <reason> and <arg1>... on the screen and terminate the Scheme
 *    program. (This might be suitable for a Scheme system implemented as a
 *    batch compiler.)
 * 2. Display <reason> and <arg1>... on the screen and go back to the
 *    read-evaluate-print loop. (This might be suitable for an interactive
 *    implementation).
 * 4. Package <reason> and <arg1>... up into an error object and pass this
 *    error object to an exception handler. The default exception handler then
 *    might do something as described in points 1 to 3.
 */
SCM_EXPORT ScmObj
scm_p_srfi23_error(ScmObj reason, ScmObj args)
{
    ScmObj err_obj;
    DECLARE_FUNCTION("error", procedure_variadic_1);

#if 0
    /*
     * Although SRFI-23 specified that "The argument <reason> should be a
     * string", we should not force it. Displayable is sufficient.
     */
    ENSURE_STRING(reason);
#endif

    err_obj = scm_make_error_obj(reason, args);
    scm_raise_error(err_obj);
    /* NOTREACHED */
    return SCM_UNDEF;
}
