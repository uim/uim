/*===========================================================================
 *  FileName : operations-srfi8.c
 *  About    : SRFI-8 receive: Binding to multiple values
 *
 *  Copyright (C) 2005-2006 Jun Inoue
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
void
scm_initialize_srfi8(void)
{
    REGISTER_FUNC_TABLE(srfi8_func_info_table);
}

ScmObj
scm_s_srfi8_receive(ScmObj formals, ScmObj expr, ScmObj body,
                    ScmEvalState *eval_state)
{
    int formals_len, actuals_len;
    ScmObj env, actuals;
    DECLARE_FUNCTION("receive", syntax_variadic_tailrec_2);

    env = eval_state->env;

    /*
     * (receive <formals> <expression> <body>)
     */

    formals_len = scm_validate_formals(formals);
    if (SCM_LISTLEN_ERRORP(formals_len))
        ERR_OBJ("bad formals", formals);

    /* FIXME: do we have to extend the environment first?  The SRFI-8
     * document contradicts itself on this part. */
    /*
     * In my recognition, the description in SRFI-8 "The environment in which
     * the receive-expression is evaluated is extended by binding <variable1>,
     * ..." does not mean that the environment is extended for the evaluation
     * of the receive-expression. Probably it only specifies which environment
     * will be extended after the evaluation. So current implementation is
     * correct, I think.  -- YamaKen 2006-01-05
     */
    actuals = EVAL(expr, env);

    if (SCM_VALUEPACKETP(actuals)) {
        actuals = SCM_VALUEPACKET_VALUES(actuals);
        actuals_len = scm_finite_length(actuals);
    } else {
        actuals = LIST_1(actuals);
        actuals_len = 1;
    }

    if (!scm_valid_environment_extension_lengthp(formals_len, actuals_len))
        ERR_OBJ("unmatched number of args for multiple values", actuals);
    eval_state->env = env = scm_extend_environment(formals, actuals, env);

    return scm_s_body(body, eval_state);
}
