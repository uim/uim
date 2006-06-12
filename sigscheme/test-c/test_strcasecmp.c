/*===========================================================================
 *  Filename : test_strcasecmp.c
 *  About    : unit test for scm_strcasecmp()
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

#include "cutter-sscm.h"

#include <sigscheme/sigscheme.h>
#include "sigschemeinternal.h"

int scm_strcasecmp(const char *s1, const char *s2);

#include "strcasecmp.c"

UT_DEF2(test_1, "==")
{
    UT_ASSERT_TRUE(0 == scm_strcasecmp("",  ""));
    UT_ASSERT_TRUE(0 == scm_strcasecmp("a", "a"));
    UT_ASSERT_TRUE(0 == scm_strcasecmp("a", "A"));
    UT_ASSERT_TRUE(0 == scm_strcasecmp("A", "a"));
    UT_ASSERT_TRUE(0 == scm_strcasecmp("A", "A"));

    UT_ASSERT_TRUE (0 == scm_strcasecmp("aa",   "aa"));
    UT_ASSERT_TRUE (0 == scm_strcasecmp("aa",   "AA"));
    UT_ASSERT_TRUE (0 == scm_strcasecmp("aa",   "aA"));
    UT_ASSERT_TRUE (0 == scm_strcasecmp("aa",   "Aa"));
    UT_ASSERT_TRUE (0 == scm_strcasecmp("AA",   "aa"));
    UT_ASSERT_TRUE (0 == scm_strcasecmp("aA",   "aa"));
    UT_ASSERT_TRUE (0 == scm_strcasecmp("Aa",   "aa"));
    UT_ASSERT_TRUE (0 == scm_strcasecmp("abc",  "abc"));
    UT_ASSERT_TRUE (0 == scm_strcasecmp("abc",  "ABC"));
    UT_ASSERT_TRUE (0 == scm_strcasecmp("abc",  "ABc"));
    UT_ASSERT_FALSE(0 == scm_strcasecmp("abc",  "abcd"));
    UT_ASSERT_TRUE (0 == scm_strcasecmp("ABC",  "abc"));
    UT_ASSERT_TRUE (0 == scm_strcasecmp("ABc",  "abc"));
    UT_ASSERT_FALSE(0 == scm_strcasecmp("abcd", "abc"));
}

UT_REGISTER_BEGIN("strcasecmp")
UT_REGISTER(test_1, "==")
UT_REGISTER_END
