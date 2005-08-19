/*===========================================================================
 *  FileName : operations-srfi8.c
 *  About    : srfi8 receive syntax
 *
 *  Copyright (C) 2005      by Jun Inoue
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
  SRFI8 : Receive
=============================================================================*/
ScmObj ScmOp_SRFI_8_receive(ScmObj args, ScmObj *envp, int *tail_flag)
{
    /*
     * (receive <formals> <expression> <body>)
     */
    ScmObj env     = *envp;
    ScmObj formals = SCM_NIL;
    ScmObj expr    = SCM_NIL;
    ScmObj body    = SCM_NIL;
    ScmObj actuals = SCM_NIL;
    ScmObj closure = SCM_NIL;

    /* sanity check */
    if (CHECK_3_ARGS(args))
	SigScm_ErrorObj("receive: bad argument list: ", args);

    /* set tail_flag */
    (*tail_flag) = 1;

    formals = SCM_CAR(args);
    expr = SCM_CADR(args);
    body = SCM_CDDR(args);

    /* TODO: Check: do we have to extend the environment first?  The SRFI-8
     * document contradicts itself on this part. */
    actuals = ScmOp_eval(expr, env);

    if (SCM_VALUEPACKETP(actuals))
	actuals = SCM_VALUEPACKET_VALUES(actuals);
    else
	actuals = Scm_NewCons(actuals, SCM_NIL);

    closure = Scm_NewClosure(Scm_NewCons(formals, body), env);

    /* set new env */
    (*envp) = env;

    return Scm_NewCons(closure, actuals);
}
