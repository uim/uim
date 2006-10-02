/*===========================================================================
 *  Filename : test-length.c
 *  About    : test for list length -related functions
 *
 *  Copyright (C) 2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
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

#include <assert.h>

#include "sscm-test.h"
#include "sigschemeinternal.h"


static ScmObj
circular_list(scm_int_t len)
{
    ScmObj lst, tail;
    scm_int_t i;

    assert(0 < len);

    lst = tail = CONS(SCM_TRUE, SCM_NULL);
    for (i = 1; i < len; i++) {
        lst = CONS(SCM_TRUE, lst);
    }
    SET_CDR(tail, lst);

    return lst;
}

TST_CASE("circular_list")
{
    ScmObj cl;

    cl = circular_list(1);
    TST_TN_EQ_OBJ(cl, CDR(cl));

    cl = circular_list(2);
    TST_TN_EQ_OBJ(cl, CDDR(cl));

    cl = circular_list(3);
    TST_TN_EQ_OBJ(cl, CDR(CDDR(cl)));
}

TST_CASE("SCM_LISTLEN_PROPERP")
{
    TST_TN_FALSE(SCM_LISTLEN_PROPERP(SCM_INT_T_MIN));
    TST_TN_FALSE(SCM_LISTLEN_PROPERP(SCM_INT_T_MIN + 1));
    TST_TN_FALSE(SCM_LISTLEN_PROPERP(-3));
    TST_TN_FALSE(SCM_LISTLEN_PROPERP(-2));
    TST_TN_FALSE(SCM_LISTLEN_PROPERP(-1));
    TST_TN_TRUE (SCM_LISTLEN_PROPERP(0));
    TST_TN_TRUE (SCM_LISTLEN_PROPERP(1));
    TST_TN_TRUE (SCM_LISTLEN_PROPERP(2));
    TST_TN_TRUE (SCM_LISTLEN_PROPERP(3));
    TST_TN_TRUE (SCM_LISTLEN_PROPERP(SCM_INT_T_MAX - 1));
    TST_TN_TRUE (SCM_LISTLEN_PROPERP(SCM_INT_T_MAX));
}

TST_CASE("SCM_LISTLEN_DOTTEDP")
{
    TST_TN_FALSE(SCM_LISTLEN_DOTTEDP(SCM_INT_T_MIN));
    TST_TN_TRUE (SCM_LISTLEN_DOTTEDP(SCM_INT_T_MIN + 1));
    TST_TN_TRUE (SCM_LISTLEN_DOTTEDP(-3));
    TST_TN_TRUE (SCM_LISTLEN_DOTTEDP(-2));
    TST_TN_TRUE (SCM_LISTLEN_DOTTEDP(-1));
    TST_TN_FALSE(SCM_LISTLEN_DOTTEDP(0));
    TST_TN_FALSE(SCM_LISTLEN_DOTTEDP(1));
    TST_TN_FALSE(SCM_LISTLEN_DOTTEDP(2));
    TST_TN_FALSE(SCM_LISTLEN_DOTTEDP(3));
    TST_TN_FALSE(SCM_LISTLEN_DOTTEDP(SCM_INT_T_MAX - 1));
    TST_TN_FALSE(SCM_LISTLEN_DOTTEDP(SCM_INT_T_MAX));
}

TST_CASE("SCM_LISTLEN_CIRCULARP")
{
    TST_TN_TRUE (SCM_LISTLEN_CIRCULARP(SCM_INT_T_MIN));
    TST_TN_FALSE(SCM_LISTLEN_CIRCULARP(SCM_INT_T_MIN + 1));
    TST_TN_FALSE(SCM_LISTLEN_CIRCULARP(-3));
    TST_TN_FALSE(SCM_LISTLEN_CIRCULARP(-2));
    TST_TN_FALSE(SCM_LISTLEN_CIRCULARP(-1));
    TST_TN_FALSE(SCM_LISTLEN_CIRCULARP(0));
    TST_TN_FALSE(SCM_LISTLEN_CIRCULARP(1));
    TST_TN_FALSE(SCM_LISTLEN_CIRCULARP(2));
    TST_TN_FALSE(SCM_LISTLEN_CIRCULARP(3));
    TST_TN_FALSE(SCM_LISTLEN_CIRCULARP(SCM_INT_T_MAX - 1));
    TST_TN_FALSE(SCM_LISTLEN_CIRCULARP(SCM_INT_T_MAX));
}

TST_CASE("SCM_LISTLEN_ERRORP")
{
    TST_TN_TRUE (SCM_LISTLEN_ERRORP(SCM_INT_T_MIN));
    TST_TN_FALSE(SCM_LISTLEN_ERRORP(SCM_INT_T_MIN + 1));
    TST_TN_FALSE(SCM_LISTLEN_ERRORP(-3));
    TST_TN_FALSE(SCM_LISTLEN_ERRORP(-2));
    TST_TN_FALSE(SCM_LISTLEN_ERRORP(-1));
    TST_TN_FALSE(SCM_LISTLEN_ERRORP(0));
    TST_TN_FALSE(SCM_LISTLEN_ERRORP(1));
    TST_TN_FALSE(SCM_LISTLEN_ERRORP(2));
    TST_TN_FALSE(SCM_LISTLEN_ERRORP(3));
    TST_TN_FALSE(SCM_LISTLEN_ERRORP(SCM_INT_T_MAX - 1));
    TST_TN_FALSE(SCM_LISTLEN_ERRORP(SCM_INT_T_MAX));
}

TST_CASE("SCM_LISTLEN_DOTTED")
{
    TST_TN_EQ_INT(-(SCM_INT_T_MIN + 1),
                  SCM_LISTLEN_DOTTED(SCM_INT_T_MIN + 1));
    TST_TN_EQ_INT(3, SCM_LISTLEN_DOTTED(-3));
    TST_TN_EQ_INT(2, SCM_LISTLEN_DOTTED(-2));
    TST_TN_EQ_INT(1, SCM_LISTLEN_DOTTED(-1));
    /* passing values out of range results unspecified value */
}

TST_CASE("SCM_LISTLEN_BEFORE_DOT")
{
    TST_TN_EQ_INT(-(SCM_INT_T_MIN + 2),
                  SCM_LISTLEN_BEFORE_DOT(SCM_INT_T_MIN + 1));
    TST_TN_EQ_INT(2, SCM_LISTLEN_BEFORE_DOT(-3));
    TST_TN_EQ_INT(1, SCM_LISTLEN_BEFORE_DOT(-2));
    TST_TN_EQ_INT(0, SCM_LISTLEN_BEFORE_DOT(-1));
    /* passing values out of range results unspecified value */
}

TST_CASE("SCM_PROPER_LISTP")
{
    ScmObj e, n;

    e = SCM_TRUE;
    n = SCM_MAKE_INT(1);

    /* proper lists */
    TST_TN_TRUE (SCM_PROPER_LISTP(SCM_NULL));
    TST_TN_TRUE (SCM_PROPER_LISTP(CONS(e, SCM_NULL)));
    TST_TN_TRUE (SCM_PROPER_LISTP(CONS(e, CONS(e, SCM_NULL))));
    TST_TN_TRUE (SCM_PROPER_LISTP(CONS(e, CONS(e, CONS(e, SCM_NULL)))));
    /* improper lists */
    TST_TN_FALSE(SCM_PROPER_LISTP(SCM_TRUE));
    TST_TN_FALSE(SCM_PROPER_LISTP(CONS(e, SCM_TRUE)));
    TST_TN_FALSE(SCM_PROPER_LISTP(CONS(e, CONS(e, SCM_TRUE))));
    TST_TN_FALSE(SCM_PROPER_LISTP(CONS(e, CONS(e, CONS(e, SCM_TRUE)))));
    TST_TN_FALSE(SCM_PROPER_LISTP(n));
    TST_TN_FALSE(SCM_PROPER_LISTP(CONS(e, n)));
    TST_TN_FALSE(SCM_PROPER_LISTP(CONS(e, CONS(e, n))));
    TST_TN_FALSE(SCM_PROPER_LISTP(CONS(e, CONS(e, CONS(e, n)))));
    /* circular lists */
    TST_TN_FALSE(SCM_PROPER_LISTP(circular_list(1)));
    TST_TN_FALSE(SCM_PROPER_LISTP(circular_list(2)));
    TST_TN_FALSE(SCM_PROPER_LISTP(circular_list(3)));
}

TST_CASE("SCM_DOTTED_LISTP")
{
    ScmObj e, n;

    e = SCM_TRUE;
    n = SCM_MAKE_INT(1);

    /* proper lists */
    TST_TN_FALSE(SCM_DOTTED_LISTP(SCM_NULL));
    TST_TN_FALSE(SCM_DOTTED_LISTP(CONS(e, SCM_NULL)));
    TST_TN_FALSE(SCM_DOTTED_LISTP(CONS(e, CONS(e, SCM_NULL))));
    TST_TN_FALSE(SCM_DOTTED_LISTP(CONS(e, CONS(e, CONS(e, SCM_NULL)))));
    /* improper lists */
    TST_TN_FALSE(SCM_DOTTED_LISTP(SCM_TRUE));
    TST_TN_TRUE (SCM_DOTTED_LISTP(CONS(e, SCM_TRUE)));
    TST_TN_TRUE (SCM_DOTTED_LISTP(CONS(e, CONS(e, SCM_TRUE))));
    TST_TN_TRUE (SCM_DOTTED_LISTP(CONS(e, CONS(e, CONS(e, SCM_TRUE)))));
    TST_TN_FALSE(SCM_DOTTED_LISTP(n));
    TST_TN_TRUE (SCM_DOTTED_LISTP(CONS(e, n)));
    TST_TN_TRUE (SCM_DOTTED_LISTP(CONS(e, CONS(e, n))));
    TST_TN_TRUE (SCM_DOTTED_LISTP(CONS(e, CONS(e, CONS(e, n)))));
    /* circular lists */
    TST_TN_FALSE(SCM_DOTTED_LISTP(circular_list(1)));
    TST_TN_FALSE(SCM_DOTTED_LISTP(circular_list(2)));
    TST_TN_FALSE(SCM_DOTTED_LISTP(circular_list(3)));
}

TST_CASE("SCM_CIRCULAR_LISTP")
{
    ScmObj e, n;

    e = SCM_TRUE;
    n = SCM_MAKE_INT(1);

    /* proper lists */
    TST_TN_FALSE(SCM_CIRCULAR_LISTP(SCM_NULL));
    TST_TN_FALSE(SCM_CIRCULAR_LISTP(CONS(e, SCM_NULL)));
    TST_TN_FALSE(SCM_CIRCULAR_LISTP(CONS(e, CONS(e, SCM_NULL))));
    TST_TN_FALSE(SCM_CIRCULAR_LISTP(CONS(e, CONS(e, CONS(e, SCM_NULL)))));
    /* improper lists */
    TST_TN_FALSE(SCM_CIRCULAR_LISTP(SCM_TRUE));
    TST_TN_FALSE(SCM_CIRCULAR_LISTP(CONS(e, SCM_TRUE)));
    TST_TN_FALSE(SCM_CIRCULAR_LISTP(CONS(e, CONS(e, SCM_TRUE))));
    TST_TN_FALSE(SCM_CIRCULAR_LISTP(CONS(e, CONS(e, CONS(e, SCM_TRUE)))));
    TST_TN_FALSE(SCM_CIRCULAR_LISTP(n));
    TST_TN_FALSE(SCM_CIRCULAR_LISTP(CONS(e, n)));
    TST_TN_FALSE(SCM_CIRCULAR_LISTP(CONS(e, CONS(e, n))));
    TST_TN_FALSE(SCM_CIRCULAR_LISTP(CONS(e, CONS(e, CONS(e, n)))));
    /* circular lists */
    TST_TN_TRUE (SCM_CIRCULAR_LISTP(circular_list(1)));
    TST_TN_TRUE (SCM_CIRCULAR_LISTP(circular_list(2)));
    TST_TN_TRUE (SCM_CIRCULAR_LISTP(circular_list(3)));
}

TST_CASE("scm_finite_length")
{
    ScmObj e, n;

    e = SCM_TRUE;
    n = SCM_MAKE_INT(1);

    /* proper lists */
    TST_TN_EQ_INT(0, scm_finite_length(SCM_NULL));
    TST_TN_EQ_INT(1, scm_finite_length(CONS(e, SCM_NULL)));
    TST_TN_EQ_INT(2, scm_finite_length(CONS(e, CONS(e, SCM_NULL))));
    TST_TN_EQ_INT(3, scm_finite_length(CONS(e, CONS(e, CONS(e, SCM_NULL)))));
    /* improper lists */
    TST_TN_EQ_INT(-1, scm_finite_length(SCM_TRUE));
    TST_TN_EQ_INT(-2, scm_finite_length(CONS(e, SCM_TRUE)));
    TST_TN_EQ_INT(-3, scm_finite_length(CONS(e, CONS(e, SCM_TRUE))));
    TST_TN_EQ_INT(-4, scm_finite_length(CONS(e, CONS(e, CONS(e, SCM_TRUE)))));
    TST_TN_EQ_INT(-1, scm_finite_length(n));
    TST_TN_EQ_INT(-2, scm_finite_length(CONS(e, n)));
    TST_TN_EQ_INT(-3, scm_finite_length(CONS(e, CONS(e, n))));
    TST_TN_EQ_INT(-4, scm_finite_length(CONS(e, CONS(e, CONS(e, n)))));
    /* scm_finite_length() cannot accept circular list */
}

TST_CASE("scm_length")
{
    ScmObj e, n;

    e = SCM_TRUE;
    n = SCM_MAKE_INT(1);

    /* proper lists */
    TST_TN_EQ_INT(0, scm_length(SCM_NULL));
    TST_TN_EQ_INT(1, scm_length(CONS(e, SCM_NULL)));
    TST_TN_EQ_INT(2, scm_length(CONS(e, CONS(e, SCM_NULL))));
    TST_TN_EQ_INT(3, scm_length(CONS(e, CONS(e, CONS(e, SCM_NULL)))));
    /* improper lists */
    TST_TN_EQ_INT(-1, scm_length(SCM_TRUE));
    TST_TN_EQ_INT(-2, scm_length(CONS(e, SCM_TRUE)));
    TST_TN_EQ_INT(-3, scm_length(CONS(e, CONS(e, SCM_TRUE))));
    TST_TN_EQ_INT(-4, scm_length(CONS(e, CONS(e, CONS(e, SCM_TRUE)))));
    TST_TN_EQ_INT(-1, scm_length(n));
    TST_TN_EQ_INT(-2, scm_length(CONS(e, n)));
    TST_TN_EQ_INT(-3, scm_length(CONS(e, CONS(e, n))));
    TST_TN_EQ_INT(-4, scm_length(CONS(e, CONS(e, CONS(e, n)))));
    /* circular lists */
    TST_TN_EQ_INT(SCM_INT_T_MIN, scm_length(circular_list(1)));
    TST_TN_EQ_INT(SCM_INT_T_MIN, scm_length(circular_list(2)));
    TST_TN_EQ_INT(SCM_INT_T_MIN, scm_length(circular_list(3)));
}
