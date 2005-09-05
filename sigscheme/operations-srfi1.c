/*===========================================================================
 *  FileName : operations-srfi1.c
 *  About    : srfi1 procedures
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
static ScmObj list_gettailcons(ScmObj head)
{
    if (NULLP(head))
        return SCM_NULL;
    if (NULLP(CDR(head)))
        return head;

    for (; !NULLP(head); head = CDR(head)) {
        if (NULLP(CDR(head)))
            return head;
    }

    SigScm_Error("list_gettailcons : cannot get tailcons?\n");
    return SCM_NULL;
}

/*=======================================
  Function Implementations
=======================================*/
/*==============================================================================
  SRFI1 : The procedures : Constructors
==============================================================================*/
ScmObj ScmOp_SRFI1_xcons(ScmObj a, ScmObj b)
{
    return CONS(b, a);
}

ScmObj ScmOp_SRFI1_cons_star(ScmObj obj, ScmObj env)
{
    ScmObj tail_cons = SCM_NULL;
    ScmObj prev_tail = obj;

    if (NULLP(CDR(obj)))
        return CAR(obj);

    for (tail_cons = CDR(obj); !NULLP(tail_cons); tail_cons = CDR(tail_cons)) {
        /* check tail cons cell */
        if (NULLP(CDR(tail_cons))) {
            SET_CDR(prev_tail, CAR(tail_cons));
        }

        prev_tail = tail_cons;
    }

    return obj;
}

ScmObj ScmOp_SRFI1_make_list(ScmObj args, ScmObj env)
{
    ScmObj fill  = SCM_NULL;
    ScmObj head  = SCM_NULL;
    int n = 0;
    int i = 0;

    /* sanity check */
    if CHECK_1_ARG(args)
        SigScm_Error("make-llist : require at least 1 arg\n");
    if (FALSEP(ScmOp_numberp(CAR(args))))
        SigScm_ErrorObj("make-list : number required but got ", CAR(args));

    /* get n */
    n = SCM_INT_VALUE(CAR(args));

    /* get filler if available */
    if (!NULLP(CDR(args)))
        fill = CADR(args);

    /* then create list */
    for (i = n; 0 < i; i--) {
        if (!NULLP(fill))
            head = CONS(fill, head);
        else
            head = CONS(Scm_NewInt(i), head);
    }

    return head;
}

ScmObj ScmOp_SRFI1_list_tabulate(ScmObj args, ScmObj env)
{
    ScmObj scm_n = CAR(args);
    ScmObj proc  = SCM_NULL;
    ScmObj head  = SCM_NULL;
    ScmObj num   = SCM_NULL;
    int n = 0;
    int i = 0;

    /* sanity check */
    if (FALSEP(ScmOp_numberp(scm_n)))
        SigScm_ErrorObj("list-tabulate : number required but got ", scm_n);

    /* get n */
    n = SCM_INT_VALUE(scm_n);

    /* get init_proc if available */
    if (!NULLP(CDR(args)))
        proc = CADR(args);

    /* then create list */
    for (i = n; 0 < i; i--) {
        num = Scm_NewInt(i - 1);

        if (!NULLP(proc)) {
            /* evaluate (proc num) */
            num = EVAL(CONS(proc,
                            CONS(num, SCM_NULL)),
                       env);
        }

        head = CONS(num, head);
    }

    return head;
}

ScmObj ScmOp_SRFI1_list_copy(ScmObj list)
{
    ScmObj head = SCM_NULL;
    ScmObj tail = SCM_NULL;
    ScmObj obj  = SCM_NULL;

    if (FALSEP(ScmOp_listp(list)))
        SigScm_ErrorObj("list-copy : list required but got ", list);

    for (; !NULLP(list); list = CDR(list)) {
        obj = CAR(list);

        /* further copy */
        if (CONSP(obj))
            obj = ScmOp_SRFI1_list_copy(obj);

        /* then create new cons */
        obj = CONS(obj, SCM_NULL);
        if (!NULLP(tail)) {
            SET_CDR(tail, obj);
            tail = obj;
        } else {
            head = obj;
            tail = head;
        }
    }

    return head;
}

ScmObj ScmOp_SRFI1_circular_list(ScmObj list, ScmObj env)
{
    ScmObj tailcons = SCM_NULL;

    if (FALSEP(ScmOp_listp(list)))
        SigScm_ErrorObj("circular-list : list required but got ", list);

    tailcons = list_gettailcons(list);
    SET_CDR(tailcons, list);

    return list;
}

ScmObj ScmOp_SRFI1_iota(ScmObj args, ScmObj env)
{
    ScmObj scm_count = SCM_NULL;
    ScmObj scm_start = SCM_NULL;
    ScmObj scm_step  = SCM_NULL;
    ScmObj head      = SCM_NULL;
    int count = 0;
    int start = 0;
    int step  = 0;
    int i = 0;

    /* sanity check */
    if CHECK_1_ARG(args)
        SigScm_Error("iota : required at least 1 arg\n");

    /* get params */
    scm_count = CAR(args);

    if (!NULLP(CDR(args)))
        scm_start = CADR(args);

    if (!NULLP(scm_start) && !NULLP(CDDR(args)))
        scm_step = CAR(CDDR(args));

    /* param type check */
    if (FALSEP(ScmOp_numberp(scm_count)))
        SigScm_ErrorObj("iota : number required but got ", scm_count);

    if (!NULLP(scm_start) && FALSEP(ScmOp_numberp(scm_start)))
        SigScm_ErrorObj("iota : number required but got ", scm_start);

    if (!NULLP(scm_step)  && FALSEP(ScmOp_numberp(scm_step)))
        SigScm_ErrorObj("iota : number required but got ", scm_step);

    /* now create list */
    count = SCM_INT_VALUE(scm_count);
    start = NULLP(scm_start) ? 0 : SCM_INT_VALUE(scm_start);
    step  = NULLP(scm_step)  ? 1 : SCM_INT_VALUE(scm_step);
    for (i = count - 1; 0 <= i; i--) {
        head = CONS(Scm_NewInt(start + i*step), head);
    }

    return head;
}
