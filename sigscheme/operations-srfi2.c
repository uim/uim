/*===========================================================================
 *  FileName : operations-srfi2.c
 *  About    : AND-LET*: an AND with local bindings, a guarded LET* special form
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
/* FIXME: Simplify Scm_RegisterSyntaxFixedTailRec2 */
ScmObj ScmOp_SRFI2_and_let_star(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env      = eval_state->env;
    ScmObj bindings = SCM_FALSE;
    ScmObj body     = SCM_FALSE;
    ScmObj var      = SCM_FALSE;
    ScmObj val      = SCM_FALSE;
    ScmObj binding  = SCM_FALSE;

    /* sanity check */
    if CHECK_2_ARGS(args)
        SigScm_Error("and-let* : syntax error\n");

    /* get bindings and body */
    bindings = CAR(args);
    body     = CDR(args);

    /*========================================================================
      (and-let* <claws> <body>)

      <claws> ::= '() | (cons <claw> <claws>)
      <claw>  ::=  (<variable> <expression>) | (<expression>)
                   | <bound-variable>
    ========================================================================*/
    if (CONSP(bindings)) {
        for (; !NULLP(bindings); bindings = CDR(bindings)) {
            binding = CAR(bindings);

            /* FIXME: Support (<exp>) and <bound-variable> style claw */
            if (!CONSP(binding) || NULLP(CDR(binding)))
                SigScm_ErrorObj("and-let* : invalid binding form : ", binding);

            SCM_SHIFT_RAW_2(var, val, binding); /* FIXME: error check */
            val = EVAL(val, env);
            if (FALSEP(val))
                return val;

            env = extend_environment(LIST_1(var), LIST_1(val), env);
        }
    } else if (NULLP(bindings)) {
        env = extend_environment(SCM_NULL,
                                 SCM_NULL,
                                 env);
    } else {
        SigScm_ErrorObj("and-let* : invalid argument ", args);
    }

    eval_state->env = env;

    return ScmExp_begin(body, eval_state);
}
