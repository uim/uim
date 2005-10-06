/*===========================================================================
 *  FileName : operations-srfi6.c
 *  About    : Basic String Ports
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
void SigScm_Initialize_SRFI6(void)
{
    /*=======================================================================
      SRFI-6 Procedures
    =======================================================================*/
    Scm_RegisterProcedureFixed1("open-input-string", ScmOp_SRFI6_open_input_string);
    Scm_RegisterProcedureFixed0("open-output-string", ScmOp_SRFI6_open_output_string);
    Scm_RegisterProcedureFixed1("get-output-string", ScmOp_SRFI6_get_output_string);
}

ScmObj ScmOp_SRFI6_open_input_string(ScmObj str)
{
    DECLARE_FUNCTION("open-input-string", ProcedureFixed1);

    ASSERT_STRINGP(str);

    return Scm_NewStringPort(SCM_STRING_STR(str), PORT_INPUT);
}

ScmObj ScmOp_SRFI6_open_output_string(void)
{
    DECLARE_FUNCTION("open-output-string", ProcedureFixed0);

    return Scm_NewStringPort(NULL, PORT_OUTPUT);
}

ScmObj ScmOp_SRFI6_get_output_string(ScmObj port)
{
    DECLARE_FUNCTION("get-output-string", ProcedureFixed1);

    ASSERT_PORTP(port);

    return Scm_NewStringCopying(SCM_PORT_STR(port));
}
