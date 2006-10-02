/*===========================================================================
 *  Filename : list.c
 *  About    : R5SR pairs and lists
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

/* scm_length() is covered by following license */
/*
 * list.c - List related functions
 *
 *   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
 * 
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 * 
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: list.c,v 1.45 2005/04/12 01:42:27 shirok Exp $
 */

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

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Definitions
=======================================*/
/*===========================================================================
  R5RS : 6.3 Other data types : 6.3.2 Pairs and lists
===========================================================================*/
SCM_EXPORT ScmObj
scm_p_pairp(ScmObj obj)
{
    DECLARE_FUNCTION("pair?", procedure_fixed_1);

    return MAKE_BOOL(CONSP(obj));
}

SCM_EXPORT ScmObj
scm_p_cons(ScmObj car, ScmObj cdr)
{
    DECLARE_FUNCTION("cons", procedure_fixed_2);

    return CONS(car, cdr);
}

SCM_EXPORT ScmObj
scm_p_car(ScmObj obj)
{
    DECLARE_FUNCTION("car", procedure_fixed_1);
#if SCM_COMPAT_SIOD_BUGS
    if (NULLP(obj))
        return SCM_NULL;
#endif

    ENSURE_CONS(obj);

    return CAR(obj);
}

SCM_EXPORT ScmObj
scm_p_cdr(ScmObj obj)
{
    DECLARE_FUNCTION("cdr", procedure_fixed_1);
#if SCM_COMPAT_SIOD_BUGS
    if (NULLP(obj))
        return SCM_NULL;
#endif

    ENSURE_CONS(obj);

    return CDR(obj);
}

SCM_EXPORT ScmObj
scm_p_set_carx(ScmObj pair, ScmObj car)
{
    DECLARE_FUNCTION("set-car!", procedure_fixed_2);

    ENSURE_CONS(pair);
    ENSURE_MUTABLE_CONS(pair);

    SET_CAR(pair, car);

#if SCM_COMPAT_SIOD
    return car;
#else
    return SCM_UNDEF;
#endif
}

SCM_EXPORT ScmObj
scm_p_set_cdrx(ScmObj pair, ScmObj cdr)
{
    DECLARE_FUNCTION("set-cdr!", procedure_fixed_2);

    ENSURE_CONS(pair);
    ENSURE_MUTABLE_CONS(pair);

    SET_CDR(pair, cdr);

#if SCM_COMPAT_SIOD
    return cdr;
#else
    return SCM_UNDEF;
#endif
}

SCM_EXPORT ScmObj
scm_p_caar(ScmObj lst)
{
    DECLARE_FUNCTION("caar", procedure_fixed_1);

    return scm_p_car(scm_p_car(lst));
}

SCM_EXPORT ScmObj
scm_p_cadr(ScmObj lst)
{
    DECLARE_FUNCTION("cadr", procedure_fixed_1);

    return scm_p_car(scm_p_cdr(lst));
}

SCM_EXPORT ScmObj
scm_p_cdar(ScmObj lst)
{
    DECLARE_FUNCTION("cdar", procedure_fixed_1);

    return scm_p_cdr(scm_p_car(lst));
}

SCM_EXPORT ScmObj
scm_p_cddr(ScmObj lst)
{
    DECLARE_FUNCTION("cddr", procedure_fixed_1);

    return scm_p_cdr(scm_p_cdr(lst));
}

SCM_EXPORT ScmObj
scm_p_caddr(ScmObj lst)
{
    DECLARE_FUNCTION("caddr", procedure_fixed_1);

    return scm_p_car(scm_p_cdr(scm_p_cdr(lst)));
}

SCM_EXPORT ScmObj
scm_p_cdddr(ScmObj lst)
{
    DECLARE_FUNCTION("cdddr", procedure_fixed_1);

    return scm_p_cdr(scm_p_cdr(scm_p_cdr(lst)));
}

SCM_EXPORT ScmObj
scm_p_nullp(ScmObj obj)
{
    DECLARE_FUNCTION("null?", procedure_fixed_1);

    return MAKE_BOOL(NULLP(obj));
}

SCM_EXPORT ScmObj
scm_p_listp(ScmObj obj)
{
    DECLARE_FUNCTION("list?", procedure_fixed_1);

    /* fast path */
    if (NULLP(obj))
        return SCM_TRUE;
    if (!CONSP(obj))
        return SCM_FALSE;

    return MAKE_BOOL(PROPER_LISTP(obj));
}

SCM_EXPORT ScmObj
scm_p_list(ScmObj args)
{
    DECLARE_FUNCTION("list", procedure_variadic_0);

    return args;
}

/* scm_length() for non-circular list */
SCM_EXPORT scm_int_t
scm_finite_length(ScmObj lst)
{
    scm_int_t len;

    for (len = 0; CONSP(lst); lst = CDR(lst))
        len++;

    if (NULLP(lst))
        return len;
    else
        return SCM_LISTLEN_ENCODE_DOTTED(len);
}

/*
 * ChangeLog for scm_length()
 *
 * 2005-08-12 kzk      Copied from Scm_Length() of Gauche 0.8.5.
 * 2006-01-05 YamaKen  Return dotted list length and circular indication.
 * 2006-10-02 YamaKen  Change dotted list length definition to SRFI-1's.
 *
 */
/* Returns -1 as one length improper list for non-list obj. */
SCM_EXPORT scm_int_t
scm_length(ScmObj lst)
{
    ScmObj slow;
    scm_int_t len;

    for (len = 0, slow = lst;;) {
        if (NULLP(lst)) break;
        if (!CONSP(lst))
            return SCM_LISTLEN_ENCODE_DOTTED(len);
        if (len != 0 && lst == slow)
            return SCM_LISTLEN_ENCODE_CIRCULAR(len);

        lst = CDR(lst);
        len++;
        if (NULLP(lst)) break;
        if (!CONSP(lst))
            return SCM_LISTLEN_ENCODE_DOTTED(len);
        if (lst == slow)
            return SCM_LISTLEN_ENCODE_CIRCULAR(len);

        lst = CDR(lst);
        slow = CDR(slow);
        len++;
    }

    return len;
}

SCM_EXPORT ScmObj
scm_p_length(ScmObj obj)
{
    scm_int_t len;
    DECLARE_FUNCTION("length", procedure_fixed_1);

    len = scm_length(obj);
    if (!SCM_LISTLEN_PROPERP(len)) {
        if (SCM_LISTLEN_CIRCULARP(len) && !SCM_WRITE_SS_ENABLEDP())
            ERR("proper list required but got a circular list");
        ERR_OBJ("proper list required but got", obj);
    }

    return MAKE_INT(len);
}

SCM_EXPORT ScmObj
scm_p_append(ScmObj args)
{
    ScmQueue q;
    ScmObj lst, elm, ret;
    DECLARE_FUNCTION("append", procedure_variadic_0);

    if (NULLP(args))
        return SCM_NULL;

    ret = SCM_NULL;
    SCM_QUEUE_POINT_TO(q, ret);
    /* duplicate and merge all but the last argument */
    FOR_EACH_BUTLAST (lst, args) {
        FOR_EACH (elm, lst)
            SCM_QUEUE_ADD(q, elm);
        CHECK_PROPER_LIST_TERMINATION(lst, args);
    }
    /* append the last argument */
    SCM_QUEUE_SLOPPY_APPEND(q, lst);

    return ret;
}

SCM_EXPORT ScmObj
scm_p_reverse(ScmObj lst)
{
    ScmObj ret, elm, rest;
    DECLARE_FUNCTION("reverse", procedure_fixed_1);

    ret = SCM_NULL;
    rest = lst;
    FOR_EACH (elm, rest)
        ret = CONS(elm, ret);
    CHECK_PROPER_LIST_TERMINATION(rest, lst);

    return ret;
}

SCM_EXPORT ScmObj
scm_list_tail(ScmObj lst, scm_int_t k)
{
    while (k--) {
        if (!CONSP(lst))
            return SCM_INVALID;
        lst = CDR(lst);
    }

    return lst;
}

SCM_EXPORT ScmObj
scm_p_list_tail(ScmObj lst, ScmObj k)
{
    ScmObj ret;
    DECLARE_FUNCTION("list-tail", procedure_fixed_2);

    ENSURE_INT(k);

    ret = scm_list_tail(lst, SCM_INT_VALUE(k));
    if (!VALIDP(ret))
        ERR_OBJ("out of range", k);

    return ret;
}

SCM_EXPORT ScmObj
scm_p_list_ref(ScmObj lst, ScmObj k)
{
    ScmObj tail;
    DECLARE_FUNCTION("list-ref", procedure_fixed_2);

    ENSURE_INT(k);

    tail = scm_list_tail(lst, SCM_INT_VALUE(k));
    if (!VALIDP(tail) || !CONSP(tail))
        ERR_OBJ("out of range", k);

    return CAR(tail);
}

#define MEMBER_BODY(obj, lst, cmp)                                           \
    do {                                                                     \
        for (; CONSP(lst); lst = CDR(lst))                                   \
            if (cmp(obj, CAR(lst)))                                          \
                return lst;                                                  \
        CHECK_PROPER_LIST_TERMINATION(lst, lst);                             \
        return SCM_FALSE;                                                    \
    } while (/* CONSTCOND */ 0)

SCM_EXPORT ScmObj
scm_p_memq(ScmObj obj, ScmObj lst)
{
    DECLARE_FUNCTION("memq", procedure_fixed_2);

    MEMBER_BODY(obj, lst, EQ);
}

SCM_EXPORT ScmObj
scm_p_memv(ScmObj obj, ScmObj lst)
{
    DECLARE_FUNCTION("memv", procedure_fixed_2);

#if (SCM_HAS_IMMEDIATE_NUMBER_ONLY && SCM_HAS_IMMEDIATE_CHAR_ONLY)
    MEMBER_BODY(obj, lst, EQ);
#else
    MEMBER_BODY(obj, lst, EQVP);
#endif
}

SCM_EXPORT ScmObj
scm_p_member(ScmObj obj, ScmObj lst)
{
    DECLARE_FUNCTION("member", procedure_fixed_2);

    MEMBER_BODY(obj, lst, EQUALP);
}

#undef MEMBER_BODY

#define ASSOC_BODY(obj, alist, cmp)                                          \
    do {                                                                     \
        ScmObj pair, key;                                                    \
                                                                             \
        FOR_EACH (pair, alist) {                                             \
            ENSURE_CONS(pair);                                               \
            key = CAR(pair);                                                 \
            if (cmp(key, obj))                                               \
                return pair;                                                 \
        }                                                                    \
        CHECK_PROPER_LIST_TERMINATION(alist, alist);                         \
        return SCM_FALSE;                                                    \
    } while (/* CONSTCOND */ 0)

SCM_EXPORT ScmObj
scm_p_assq(ScmObj obj, ScmObj alist)
{
    DECLARE_FUNCTION("assq", procedure_fixed_2);

    ASSOC_BODY(obj, alist, EQ);
}

SCM_EXPORT ScmObj
scm_p_assv(ScmObj obj, ScmObj alist)
{
    DECLARE_FUNCTION("assv", procedure_fixed_2);

#if (SCM_HAS_IMMEDIATE_NUMBER_ONLY && SCM_HAS_IMMEDIATE_CHAR_ONLY)
    ASSOC_BODY(obj, alist, EQ);
#else
    ASSOC_BODY(obj, alist, EQVP);
#endif
}

SCM_EXPORT ScmObj
scm_p_assoc(ScmObj obj, ScmObj alist)
{
    DECLARE_FUNCTION("assoc", procedure_fixed_2);

    ASSOC_BODY(obj, alist, EQUALP);
}

#undef ASSOC_BODY
