/*===========================================================================
 *  FileName : test_format.c
 *  About    : unit test for format functions
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

#include <stddef.h>
#include <stdarg.h>
#include <string.h>

#include "cutter-sscm.h"

#include "my-stdint.h"
#include "sigscheme.h"
#include "sigschemeinternal.h"

#define OK 0
#define STR SCM_STRING_STR

static ScmObj
format(const char *fmt, ...)
{
    va_list args;
    ScmObj ret;

    va_start(args, fmt);
    ret = scm_vformat(SCM_FALSE, SCM_FMT_INTERNAL, fmt, args);
    va_end(args);

    return ret;
}

UT_DEF2(test_1, "~D")
{
    UT_ASSERT_EQUAL_STRING("23", STR(format("~D", 23)));
    UT_ASSERT_EQUAL_STRING("0",  STR(format("~D", INT_MAX)));
}

UT_DEF2(test_2, "~X")
{
    UT_ASSERT_EQUAL_STRING("40", STR(format("~X", 64)));
}

UT_REGISTER_BEGIN("format")
UT_REGISTER(test_1, "~D")
UT_REGISTER(test_2, "~X")
UT_REGISTER_END
