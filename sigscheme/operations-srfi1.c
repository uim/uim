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
    if (SCM_NULLP(head))
	return SCM_NIL;
    if (SCM_NULLP(SCM_CDR(head)))
	return head;

    for (; !SCM_NULLP(head); head = SCM_CDR(head)) {
	if (SCM_NULLP(SCM_CDR(head)))
	    return head;
    }

    SigScm_Error("list_gettailcons : cannot get tailcons?\n");
    return SCM_NIL;
}

/*=======================================
  Function Implementations
=======================================*/
/*==============================================================================
  SRFI1 : The procedures : Constructors
==============================================================================*/
ScmObj ScmOp_SRFI_1_xcons(ScmObj a, ScmObj b)
{
    return Scm_NewCons(b, a);
}

ScmObj ScmOp_SRFI_1_cons_star(ScmObj obj, ScmObj env)
{
    ScmObj tail_cons = SCM_NIL;
    ScmObj prev_tail = obj;

    if (SCM_NULLP(SCM_CDR(obj)))
	return SCM_CAR(obj);

    for (tail_cons = SCM_CDR(obj); !SCM_NULLP(tail_cons); tail_cons = SCM_CDR(tail_cons)) {
	/* check tail cons cell */
	if (SCM_NULLP(SCM_CDR(tail_cons))) {
	    SCM_SETCDR(prev_tail, SCM_CAR(tail_cons));
	}

	prev_tail = tail_cons;
    }

    return obj;
}

ScmObj ScmOp_SRFI_1_make_list(ScmObj args, ScmObj env)
{
    ScmObj fill  = SCM_NIL;
    ScmObj head  = SCM_NIL;
    int n = 0;
    int i = 0;

    /* sanity check */
    if CHECK_1_ARG(args)
	SigScm_Error("make-llist : require at least 1 arg\n");
    if (EQ(ScmOp_numberp(SCM_CAR(args)), SCM_FALSE))
	SigScm_ErrorObj("make-list : number required but got ", SCM_CAR(args));

    /* get n */
    n = SCM_INT_VALUE(SCM_CAR(args));

    /* get filler if available */
    if (!SCM_NULLP(SCM_CDR(args)))
	fill = SCM_CAR(SCM_CDR(args));

    /* then create list */
    for (i = n; 0 < i; i--) {
	if (!SCM_NULLP(fill))
	    head = Scm_NewCons(fill, head);
	else
	    head = Scm_NewCons(Scm_NewInt(i), head);
    }

    return head;
}

ScmObj ScmOp_SRFI_1_list_tabulate(ScmObj args, ScmObj env)
{
    ScmObj scm_n = SCM_CAR(args);
    ScmObj proc  = SCM_NIL;
    ScmObj head  = SCM_NIL;
    ScmObj num   = SCM_NIL;
    int n = 0;
    int i = 0;

    /* sanity check */
    if (EQ(ScmOp_numberp(scm_n), SCM_FALSE))
	SigScm_ErrorObj("list-tabulate : number required but got ", scm_n);

    /* get n */
    n = SCM_INT_VALUE(scm_n);

    /* get init_proc if available */
    if (!SCM_NULLP(SCM_CDR(args)))
	proc = SCM_CAR(SCM_CDR(args));

    /* then create list */
    for (i = n; 0 < i; i--) {
	num = Scm_NewInt(i - 1);

	if (!SCM_NULLP(proc)) {
	    /* evaluate (proc num) */
	    num = ScmOp_eval(Scm_NewCons(proc,
					 Scm_NewCons(num, SCM_NIL)),
			     env);
	}

	head = Scm_NewCons(num, head);
    }

    return head;
}

ScmObj ScmOp_SRFI_1_list_copy(ScmObj list)
{
    ScmObj head = SCM_NIL;
    ScmObj tail = SCM_NIL;
    ScmObj obj  = SCM_NIL;

    if (EQ(ScmOp_listp(list), SCM_FALSE))
	SigScm_ErrorObj("list-copy : list required but got ", list);

    for (; !SCM_NULLP(list); list = SCM_CDR(list)) {
	obj = SCM_CAR(list);

	/* further copy */
	if (SCM_CONSP(obj))
	    obj = ScmOp_SRFI_1_list_copy(obj);

	/* then create new cons */
	obj = Scm_NewCons(obj, SCM_NIL);
	if (!SCM_NULLP(tail)) {
	    SCM_SETCDR(tail, obj);
	    tail = obj;
	} else {
	    head = obj;
	    tail = head;
	}
    }

    return head;
}

ScmObj ScmOp_SRFI_1_circular_list(ScmObj list, ScmObj env)
{
    ScmObj tailcons = SCM_NIL;

    if (EQ(ScmOp_listp(list), SCM_FALSE))
	SigScm_ErrorObj("circular-list : list required but got ", list);

    tailcons = list_gettailcons(list);
    SCM_SETCDR(tailcons, list);

    return list;
}

ScmObj ScmOp_SRFI_1_iota(ScmObj args, ScmObj env)
{
    ScmObj scm_count = SCM_NIL;
    ScmObj scm_start = SCM_NIL;
    ScmObj scm_step  = SCM_NIL;
    ScmObj head      = SCM_NIL;
    int count = 0;
    int start = 0;
    int step  = 0;
    int i = 0;

    /* sanity check */
    if CHECK_1_ARG(args)
	SigScm_Error("iota : required at least 1 arg\n");

    /* get params */
    scm_count = SCM_CAR(args);

    if (!SCM_NULLP(SCM_CDR(args)))
	scm_start = SCM_CAR(SCM_CDR(args));

    if (!SCM_NULLP(scm_start) && !SCM_NULLP(SCM_CDR(SCM_CDR(args))))
	scm_step = SCM_CAR(SCM_CDR(SCM_CDR(args)));

    /* param type check */
    if (EQ(ScmOp_numberp(scm_count), SCM_FALSE))
	SigScm_ErrorObj("iota : number required but got ", scm_count);

    if (!SCM_NULLP(scm_start) && EQ(ScmOp_numberp(scm_start), SCM_FALSE))
	SigScm_ErrorObj("iota : number required but got ", scm_start);

    if (!SCM_NULLP(scm_step)  && EQ(ScmOp_numberp(scm_step), SCM_FALSE))
	SigScm_ErrorObj("iota : number required but got ", scm_step);

    /* now create list */
    count = SCM_INT_VALUE(scm_count);
    start = SCM_NULLP(scm_start) ? 0 : SCM_INT_VALUE(scm_start);
    step  = SCM_NULLP(scm_step)  ? 1 : SCM_INT_VALUE(scm_step);
    for (i = count - 1; 0 <= i; i--) {
	head = Scm_NewCons(Scm_NewInt(start + i*step), head);
    }

    return head;
}
