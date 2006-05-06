/*===========================================================================
 *  Filename : test_global.c
 *  About    : unit test for global object handlings
 *
 *  Copyright (C) 2006 YamaKen <yamaken AT bp.iij4u.or.jp>
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

#define SCM_WRITABLE_STATICLESS_PLATFORM 0
#define SCM_COMBINED_SOURCE 0

#include "cutter-sscm.h"

#include <global.h>
#include <sigscheme/sigscheme.h>
#include "sigschemeinternal.h"

SCM_GLOBAL_VARS_BEGIN(exported);
ScmObj obj_a, obj_b;
ScmObj obj_c;
SCM_GLOBAL_VARS_END(exported);
#define obj_a SCM_GLOBAL_VAR(exported, obj_a)
#define obj_b SCM_GLOBAL_VAR(exported, obj_b)
#define obj_c SCM_GLOBAL_VAR(exported, obj_c)
SCM_DECLARE_EXPORTED_VARS(exported);
SCM_DEFINE_EXPORTED_VARS(exported);

SCM_GLOBAL_VARS_BEGIN(static);
ScmObj obj_d, obj_e;
ScmObj obj_f;
SCM_GLOBAL_VARS_END(static);
#define obj_d SCM_GLOBAL_VAR(static, obj_d)
#define obj_e SCM_GLOBAL_VAR(static, obj_e)
#define obj_f SCM_GLOBAL_VAR(static, obj_f)
SCM_DEFINE_STATIC_VARS(static);

UT_DEF2(test_1, "exported vars")
{
    SCM_GLOBAL_VARS_INIT(exported);
    obj_a = MAKE_INT(1);
    obj_b = MAKE_INT(2);
    obj_c = MAKE_INT(3);
    UT_ASSERT_EQUAL_INT(1, SCM_INT_VALUE(obj_a));
    UT_ASSERT_EQUAL_INT(2, SCM_INT_VALUE(obj_b));
    UT_ASSERT_EQUAL_INT(3, SCM_INT_VALUE(obj_c));
}

UT_DEF2(test_2, "static vars")
{
    SCM_GLOBAL_VARS_INIT(static);
    obj_d = MAKE_INT(4);
    obj_e = MAKE_INT(5);
    obj_f = MAKE_INT(6);
    UT_ASSERT_EQUAL_INT(4, SCM_INT_VALUE(obj_d));
    UT_ASSERT_EQUAL_INT(5, SCM_INT_VALUE(obj_e));
    UT_ASSERT_EQUAL_INT(6, SCM_INT_VALUE(obj_f));
}

UT_REGISTER_BEGIN("global")
UT_REGISTER(test_1, "exported vars")
UT_REGISTER(test_2, "static vars")
UT_REGISTER_END
