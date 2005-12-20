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
static ScmObj compare_list(ScmObj eqproc, ScmObj lst1, ScmObj lst2);

/*=======================================
  Function Implementations
=======================================*/
void
scm_initialize_srfi1(void)
{
    /*=======================================================================
      SRFI-1 Procedures
    =======================================================================*/
    REGISTER_FUNC_TABLE(srfi1_func_info_table);
}

/*==============================================================================
  SRFI1 : The procedures : Constructors
==============================================================================*/
ScmObj
scm_p_srfi1_xcons(ScmObj a, ScmObj b)
{
    DECLARE_FUNCTION("xcons", procedure_fixed_2);
    return CONS(b, a);
}

ScmObj
scm_p_srfi1_consstar(ScmObj args)
{
    ScmObj tail_cons = SCM_FALSE;
    ScmObj prev_last = args;
    DECLARE_FUNCTION("cons*", procedure_variadic_0);

    if (NULLP(CDR(args)))
        return CAR(args);

    for (tail_cons = CDR(args); !NULLP(tail_cons); tail_cons = CDR(tail_cons)) {
        /* check tail cons cell */
        if (NULLP(CDR(tail_cons))) {
            SET_CDR(prev_last, CAR(tail_cons));
        }

        prev_last = tail_cons;
    }

    return args;
}

ScmObj
scm_p_srfi1_make_list(ScmObj length, ScmObj args)
{
    ScmObj filler = SCM_FALSE;
    ScmObj head   = SCM_FALSE;
    int len = 0;
    int i   = 0;
    DECLARE_FUNCTION("make-list", procedure_variadic_1);

    ASSERT_INTP(length);

    len = SCM_INT_VALUE(length);

    /* get filler if available */
    if (!NULLP(args))
        filler = CAR(args);
    else
        filler = SCM_FALSE;

    /* then create list */
    for (i = len; 0 < i; i--) {
        head = CONS(filler, head);
    }

    return head;
}

ScmObj
scm_p_srfi1_list_tabulate(ScmObj scm_n, ScmObj args)
{
    ScmObj proc  = SCM_FALSE;
    ScmObj head  = SCM_NULL;
    ScmObj num   = SCM_FALSE;
    int n = 0;
    int i = 0;
    DECLARE_FUNCTION("list-tabulate", procedure_variadic_1);

    ASSERT_INTP(scm_n);

    /* get n */
    n = SCM_INT_VALUE(scm_n);

    /* get init_proc if available */
    if (!NULLP(args))
        proc = CAR(args);

    /* then create list */
    for (i = n; 0 < i; i--) {
        num = scm_make_int(i - 1);

        if (!NULLP(proc))
            num = scm_call(proc, LIST_1(num));

        head = CONS(num, head);
    }

    return head;
}

ScmObj
scm_p_srfi1_list_copy(ScmObj lst)
{
    ScmObj head = SCM_FALSE;
    ScmObj tail = SCM_FALSE;
    ScmObj obj  = SCM_FALSE;
    DECLARE_FUNCTION("list-copy", procedure_fixed_1);

    if (FALSEP(scm_p_listp(lst)))
        ERR_OBJ("list required but got", lst);

    for (; !NULLP(lst); lst = CDR(lst)) {
        obj = CAR(lst);

        /* further copy */
        if (CONSP(obj))
            obj = scm_p_srfi1_list_copy(obj);

        /* then create new cons */
        obj = CONS(obj, SCM_NULL);
        if (!FALSEP(tail)) {
            SET_CDR(tail, obj);
            tail = obj;
        } else {
            head = obj;
            tail = head;
        }
    }

    return head;
}

ScmObj
scm_p_srfi1_circular_list(ScmObj args)
{
    DECLARE_FUNCTION("circular-list", procedure_variadic_0);

    if (FALSEP(scm_p_listp(args)))
        ERR_OBJ("list required but got", args);

    SET_CDR(scm_p_srfi1_last_pair(args), args);
    return args;
}

ScmObj
scm_p_srfi1_iota(ScmObj scm_count, ScmObj args)
{
    ScmObj scm_start = SCM_FALSE;
    ScmObj scm_step  = SCM_FALSE;
    ScmObj head      = SCM_NULL;
    int count = 0;
    int start = 0;
    int step  = 0;
    int i = 0;
    DECLARE_FUNCTION("iota", procedure_variadic_1);

    /* get params */
    if (!NULLP(args))
        scm_start = CAR(args);

    if (!NULLP(scm_start) && !NULLP(CDR(args)))
        scm_step = CAR(CDR(args));

    /* param type check */
    ASSERT_INTP(scm_count);
    if (!NULLP(scm_start))
        ASSERT_INTP(scm_start);
    if (!NULLP(scm_step))
        ASSERT_INTP(scm_step);

    /* now create list */
    count = SCM_INT_VALUE(scm_count);
    start = NULLP(scm_start) ? 0 : SCM_INT_VALUE(scm_start);
    step  = NULLP(scm_step)  ? 1 : SCM_INT_VALUE(scm_step);
    for (i = count - 1; 0 <= i; i--) {
        head = CONS(scm_make_int(start + i*step), head);
    }

    return head;
}

/*==============================================================================
  SRFI1 : The procedures : Predicates
==============================================================================*/
ScmObj
scm_p_srfi1_proper_listp(ScmObj lst)
{
    DECLARE_FUNCTION("proper-list?", procedure_fixed_1);
    return scm_p_listp(lst);
}

ScmObj
scm_p_srfi1_circular_listp(ScmObj obj)
{
    ScmObj slow = obj;
    int len = 0;
    DECLARE_FUNCTION("circular-list?", procedure_fixed_1);

    for (;;) {
        if (NULLP(obj)) break;
        if (!CONSP(obj)) return SCM_FALSE;
        if (len != 0 && obj == slow) return SCM_TRUE; /* circular */

        obj = CDR(obj);
        len++;
        if (NULLP(obj)) break;
        if (!CONSP(obj)) return SCM_FALSE;
        if (obj == slow) return SCM_TRUE; /* circular */

        obj = CDR(obj);
        slow = CDR(slow);
        len++;
    }

    return SCM_FALSE;
}

ScmObj
scm_p_srfi1_dotted_listp(ScmObj obj)
{
    ScmObj slow = obj;
    int len = 0;
    DECLARE_FUNCTION("dotted-list?", procedure_fixed_1);

    for (;;) {
        if (NULLP(obj)) break;
        if (!CONSP(obj)) return SCM_TRUE;
        if (len != 0 && obj == slow) return SCM_FALSE; /* circular */

        obj = CDR(obj);
        len++;
        if (NULLP(obj)) break;
        if (!CONSP(obj)) return SCM_TRUE;
        if (obj == slow) return SCM_FALSE; /* circular */

        obj = CDR(obj);
        slow = CDR(slow);
        len++;
    }

    return SCM_FALSE;
}

ScmObj
scm_p_srfi1_not_pairp(ScmObj pair)
{
    DECLARE_FUNCTION("not-pair?", procedure_fixed_1);
    return CONSP(pair) ? SCM_FALSE : SCM_TRUE;
}

ScmObj
scm_p_srfi1_null_listp(ScmObj lst)
{
    DECLARE_FUNCTION("null-list?", procedure_fixed_1);
    /* TODO : check circular list */
    return MAKE_BOOL(NULLP(lst));
}

ScmObj
scm_p_srfi1_listequal(ScmObj eqproc, ScmObj args)
{
    ScmObj first_lst = SCM_FALSE;
    DECLARE_FUNCTION("list=", procedure_variadic_1);

    if (NULLP(args))
        return SCM_TRUE;

    first_lst = CAR(args);
    args = CDR(args);

    if (NULLP(args))
        return SCM_TRUE;

    for (; !NULLP(args); args = CDR(args)) {
        if (FALSEP(compare_list(eqproc, first_lst, CAR(args))))
            return SCM_FALSE;
    }

    return SCM_TRUE;
}

static ScmObj
compare_list(ScmObj eqproc, ScmObj lst1, ScmObj lst2)
{
#define CHECK_LIST_EQUALITY_WITH_EQPROC(eqproc, obj1, obj2)             \
    (scm_call(eqproc,                                                   \
              LIST_2(obj1, obj2)))

    ScmObj ret_cmp = SCM_FALSE;

    for (; !NULLP(lst1); lst1 = CDR(lst1), lst2 = CDR(lst2)) {
        /* check contents */
        ret_cmp = CHECK_LIST_EQUALITY_WITH_EQPROC(eqproc, CAR(lst1), CAR(lst2));
        if (FALSEP(ret_cmp))
            return SCM_FALSE;

        /* check next cdr's type */
        if (SCM_TYPE(CDR(lst1)) != SCM_TYPE(CDR(lst2)))
            return SCM_FALSE;

        /* check dot pair */
        if (!CONSP(CDR(lst1))) {
            return CHECK_LIST_EQUALITY_WITH_EQPROC(eqproc, CDR(lst1), CDR(lst2));
        }
    }
    return SCM_TRUE;
}

ScmObj
scm_p_srfi1_first(ScmObj lst)
{
    DECLARE_FUNCTION("first", procedure_fixed_1);
    return scm_p_car(lst);
}

ScmObj
scm_p_srfi1_second(ScmObj lst)
{
    DECLARE_FUNCTION("second", procedure_fixed_1);
    return scm_p_cadr(lst);
}

ScmObj
scm_p_srfi1_third(ScmObj lst)
{
    DECLARE_FUNCTION("third", procedure_fixed_1);
    return scm_p_caddr(lst);
}

ScmObj
scm_p_srfi1_fourth(ScmObj lst)
{
    DECLARE_FUNCTION("fourth", procedure_fixed_1);
    return scm_p_cadddr(lst);
}

ScmObj
scm_p_srfi1_fifth(ScmObj lst)
{
    DECLARE_FUNCTION("fifth", procedure_fixed_1);
    return scm_p_car(scm_p_cddddr(lst));
}

ScmObj
scm_p_srfi1_sixth(ScmObj lst)
{
    DECLARE_FUNCTION("sixth", procedure_fixed_1);
    return scm_p_cadr(scm_p_cddddr(lst));
}

ScmObj
scm_p_srfi1_seventh(ScmObj lst)
{
    DECLARE_FUNCTION("seventh", procedure_fixed_1);
    return scm_p_caddr(scm_p_cddddr(lst));
}

ScmObj
scm_p_srfi1_eighth(ScmObj lst)
{
    DECLARE_FUNCTION("eighth", procedure_fixed_1);
    return scm_p_cadddr(scm_p_cddddr(lst));
}

ScmObj
scm_p_srfi1_ninth(ScmObj lst)
{
    DECLARE_FUNCTION("ninth", procedure_fixed_1);
    return scm_p_car(scm_p_cddddr(scm_p_cddddr(lst)));
}

ScmObj
scm_p_srfi1_tenth(ScmObj lst)
{
    DECLARE_FUNCTION("tenth", procedure_fixed_1);
    return scm_p_cadr(scm_p_cddddr(scm_p_cddddr(lst)));
}

ScmObj
scm_p_srfi1_carpluscdr(ScmObj lst)
{
    DECLARE_FUNCTION("car+cdr", procedure_fixed_1);
    return scm_p_values(LIST_2(CAR(lst), CDR(lst)));
}

ScmObj
scm_p_srfi1_take(ScmObj lst, ScmObj scm_idx)
{
    ScmObj tmp      = lst;
    ScmObj ret      = SCM_FALSE;
    ScmObj ret_tail = SCM_FALSE;
    int idx = 0;
    int i;
    DECLARE_FUNCTION("take", procedure_fixed_2);

    ASSERT_INTP(scm_idx);

    idx = SCM_INT_VALUE(scm_idx);
    for (i = 0; i < idx; i++) {
        if (SCM_NULLP(tmp))
            ERR_OBJ("illegal index is specified for", lst);

        if (i != 0) {
            SET_CDR(ret_tail,  CONS(CAR(tmp), SCM_NULL));
            ret_tail = CDR(ret_tail);
        } else {
            ret = CONS(CAR(tmp), SCM_NULL);
            ret_tail = ret;
        }

        tmp = CDR(tmp);
    }

    return ret;
}

ScmObj
scm_p_srfi1_drop(ScmObj lst, ScmObj scm_idx)
{
    ScmObj ret = lst;
    int idx = 0;
    int i;
    DECLARE_FUNCTION("drop", procedure_fixed_2);

    ASSERT_INTP(scm_idx);

    idx = SCM_INT_VALUE(scm_idx);
    for (i = 0; i < idx; i++) {
        if (!CONSP(ret))
            ERR_OBJ("illegal index is specified for", lst);

        ret = CDR(ret);
    }

    return ret;
}

ScmObj
scm_p_srfi1_take_right(ScmObj lst, ScmObj scm_elem)
{
    ScmObj tmp = lst;
    int len = 0;
    DECLARE_FUNCTION("take-right", procedure_fixed_2);

    ASSERT_INTP(scm_elem);

    for (; CONSP(tmp); tmp = CDR(tmp))
        len++;

    len -= SCM_INT_VALUE(scm_elem);

    return scm_p_srfi1_drop(lst, scm_make_int(len));
}

ScmObj
scm_p_srfi1_drop_right(ScmObj lst, ScmObj scm_elem)
{
    ScmObj tmp = lst;
    int len = 0;
    DECLARE_FUNCTION("drop-right", procedure_fixed_2);

    ASSERT_INTP(scm_elem);

    for (; CONSP(tmp); tmp = CDR(tmp))
        len++;

    len -= SCM_INT_VALUE(scm_elem);

    return scm_p_srfi1_take(lst, scm_make_int(len));
}

ScmObj
scm_p_srfi1_taked(ScmObj lst, ScmObj scm_idx)
{
    ScmObj tmp = lst;
    int idx = 0;
    int i;
    DECLARE_FUNCTION("take!", procedure_fixed_2);

    ASSERT_INTP(scm_idx);

    idx = SCM_INT_VALUE(scm_idx);

    for (i = 0; i < idx - 1; i++) {
        tmp = CDR(tmp);
    }

    SET_CDR(tmp, SCM_NULL);

    return lst;
}

ScmObj
scm_p_srfi1_drop_rightd(ScmObj lst, ScmObj scm_idx)
{
    ScmObj tmp = lst;
    int len = 0;
    int i;
    DECLARE_FUNCTION("drop-right!", procedure_fixed_2);

    ASSERT_INTP(scm_idx);

    for (; CONSP(tmp); tmp = CDR(tmp))
        len++;

    len -= SCM_INT_VALUE(scm_idx);

    tmp = lst;
    for (i = 0; i < len - 1; i++) {
        tmp = CDR(tmp);
    }

    SET_CDR(tmp, SCM_NULL);

    return lst;
}

ScmObj
scm_p_srfi1_split_at(ScmObj lst, ScmObj idx)
{
    DECLARE_FUNCTION("split-at", procedure_fixed_2);

    return scm_p_values(LIST_2(scm_p_srfi1_take(lst, idx),
                               scm_p_srfi1_drop(lst, idx)));
}

ScmObj
scm_p_srfi1_split_atd(ScmObj lst, ScmObj idx)
{
    ScmObj drop = scm_p_srfi1_drop(lst, idx);
    DECLARE_FUNCTION("split-at!", procedure_fixed_2);

    return scm_p_values(LIST_2(scm_p_srfi1_taked(lst, idx),
                               drop));
}

ScmObj
scm_p_srfi1_last(ScmObj lst)
{
    DECLARE_FUNCTION("last", procedure_fixed_1);

    /* sanity check */
    if (NULLP(lst))
        ERR_OBJ("non-empty, proper list is required but got", lst);

    return CAR(scm_p_srfi1_last_pair(lst));
}

ScmObj
scm_p_srfi1_last_pair(ScmObj lst)
{
    DECLARE_FUNCTION("last-pair", procedure_fixed_1);

    /* sanity check */
    if (NULLP(lst))
        ERR_OBJ("non-empty, proper list is required but got", lst);

    for (; CONSP(CDR(lst)); lst = CDR(lst))
        ;

    return lst;
}

/*==============================================================================
  SRFI1 : The procedures : Miscellaneous
==============================================================================*/
ScmObj
scm_p_srfi1_lengthplus(ScmObj lst)
{
    DECLARE_FUNCTION("length+", procedure_fixed_1);

    /* FIXME!: remove expensive circular_listp */
    if (NFALSEP(scm_p_srfi1_circular_listp(lst)))
        return SCM_FALSE;

    return scm_p_length(lst);
}

ScmObj
scm_p_srfi1_concatenate(ScmObj args)
{
    ScmObj lsts_of_lst = CAR(args);
    DECLARE_FUNCTION("concatenate", procedure_variadic_0);

#if SCM_STRICT_ARGCHECK
    if (!NULLP(CDR(args)))
        ERR_OBJ("superfluous arguments", args);
#endif

    return scm_p_append(lsts_of_lst);
}
