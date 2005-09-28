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
ScmObj handle_claw(ScmObj claw, ScmObj *envp);

/*=======================================
  Function Implementations
=======================================*/
ScmObj ScmOp_SRFI2_and_let_star(ScmObj claws, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj env   = eval_state->env;
    ScmObj val   = SCM_FALSE;

    /*========================================================================
      (and-let* <claws> <body>)

      <claws> ::= '() | (cons <claw> <claws>)
    ========================================================================*/
    if (!NULLP(claws)) {
        for (; !NULLP(claws); claws = CDR(claws)) {
            val = handle_claw(CAR(claws), &env);
            if (FALSEP(val))
                return SCM_FALSE;
        }
    } else {
        env = extend_environment(SCM_NULL, SCM_NULL, env);
    }

    eval_state->env = env;

    return ScmExp_begin(body, eval_state);
}

ScmObj handle_claw(ScmObj claw, ScmObj *envp)
{
    ScmObj env = *envp;
    ScmObj var = SCM_FALSE;
    ScmObj val = SCM_FALSE;
    ScmObj exp = SCM_FALSE;
    ScmObj ret = SCM_FALSE;

    /*========================================================================
      <claw>  ::=  (<variable> <expression>) | (<expression>)
                   | <bound-variable>
    ========================================================================*/
    if (CONSP(claw)) {
        if (SYMBOLP(CAR(claw))) {
            /* (<variable> <expression>) */
            if (!NULLP(SCM_SHIFT_RAW_2(var, val, claw)))
                SigScm_ErrorObj("and-let* : superflous arguments: ", claw);
            val = EVAL(val, env);
            env = extend_environment(LIST_1(var), LIST_1(val), env);
            ret = val;
        } else if (NULLP(CDR(claw))) {
            /* (<expression>) */
            exp = CAR(claw);
            ret = EVAL(exp, env);
        } 
    } else if (SYMBOLP(claw) || NFALSEP(claw) || FALSEP(claw)) {
        /* <bound-variable> */
        ret = EVAL(claw, env);
    } else {
        SigScm_ErrorObj("and-let* : invalid claw form : ", claw);
    }

    /* set new env */
    *envp = env;

    return ret;
}
