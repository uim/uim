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

#define I32 (SIZEOF_INT    == SIZEOF_INT32_T)
#define L32 (SIZEOF_LONG   == SIZEOF_INT32_T)
#define P32 (SIZEOF_VOID_P == SIZEOF_INT32_T)

#define I64 (SIZEOF_INT    == SIZEOF_INT64_T)
#define L64 (SIZEOF_LONG   == SIZEOF_INT64_T)
#define P64 (SIZEOF_VOID_P == SIZEOF_INT64_T)

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

UT_DEF2(test_1, "no directives")
{
    UT_ASSERT_EQUAL_STRING("",   STR(format("")));
    UT_ASSERT_EQUAL_STRING("\"", STR(format("\"")));
    UT_ASSERT_EQUAL_STRING("\\", STR(format("\\")));
    UT_ASSERT_EQUAL_STRING("a",  STR(format("a")));
    UT_ASSERT_EQUAL_STRING("aBc",     STR(format("aBc")));
    UT_ASSERT_EQUAL_STRING("あ",      STR(format("あ")));
    UT_ASSERT_EQUAL_STRING("あい",    STR(format("あい")));
    UT_ASSERT_EQUAL_STRING("aあBいc", STR(format("aあBいc")));
}

UT_DEF2(test_2, "~C")
{
    UT_ASSERT_EQUAL_STRING("",   STR(format("~C", (scm_ichar_t)'\0')));
    UT_ASSERT_EQUAL_STRING("a",  STR(format("~C", (scm_ichar_t)'a')));
    UT_ASSERT_EQUAL_STRING("\"", STR(format("~C", (scm_ichar_t)'\"')));
    UT_ASSERT_EQUAL_STRING("\\", STR(format("~C", (scm_ichar_t)'\\')));
    UT_ASSERT_EQUAL_STRING("あ", STR(format("~C", (scm_ichar_t)0x3042)));
}

UT_DEF2(test_3, "~S")
{
    UT_ASSERT_EQUAL_STRING("",          STR(format("~S",   "")));
    UT_ASSERT_EQUAL_STRING("",          STR(format("~0S",  "")));
    UT_ASSERT_EQUAL_STRING(" ",         STR(format("~1S",  "")));
    UT_ASSERT_EQUAL_STRING("",          STR(format("~00S", "")));
    UT_ASSERT_EQUAL_STRING(" ",         STR(format("~01S", "")));
    UT_ASSERT_EQUAL_STRING("\"",        STR(format("~S",   "\"")));
    UT_ASSERT_EQUAL_STRING("\\",        STR(format("~S",   "\\")));
    UT_ASSERT_EQUAL_STRING("a",         STR(format("~S",   "a")));
    UT_ASSERT_EQUAL_STRING("aBc",       STR(format("~S",   "aBc")));
    UT_ASSERT_EQUAL_STRING("あ",        STR(format("~S",   "あ")));
    UT_ASSERT_EQUAL_STRING("あい",      STR(format("~S",   "あい")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~S",   "aあBいc")));
    UT_ASSERT_EQUAL_STRING("aBc",       STR(format("~0S",  "aBc")));
    UT_ASSERT_EQUAL_STRING("aBc",       STR(format("~1S",  "aBc")));
    UT_ASSERT_EQUAL_STRING("aBc",       STR(format("~2S",  "aBc")));
    UT_ASSERT_EQUAL_STRING("aBc",       STR(format("~3S",  "aBc")));
    UT_ASSERT_EQUAL_STRING(" aBc",      STR(format("~4S",  "aBc")));
    UT_ASSERT_EQUAL_STRING("  aBc",     STR(format("~5S",  "aBc")));
    UT_ASSERT_EQUAL_STRING("aBc",       STR(format("~00S", "aBc")));
    UT_ASSERT_EQUAL_STRING("aBc",       STR(format("~01S", "aBc")));
    UT_ASSERT_EQUAL_STRING("aBc",       STR(format("~02S", "aBc")));
    UT_ASSERT_EQUAL_STRING("aBc",       STR(format("~03S", "aBc")));
    UT_ASSERT_EQUAL_STRING(" aBc",      STR(format("~04S", "aBc")));
    UT_ASSERT_EQUAL_STRING("  aBc",     STR(format("~05S", "aBc")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~0S",  "aあBいc")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~1S",  "aあBいc")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~2S",  "aあBいc")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~3S",  "aあBいc")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~4S",  "aあBいc")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~5S",  "aあBいc")));
    UT_ASSERT_EQUAL_STRING(" aあBいc",  STR(format("~6S",  "aあBいc")));
    UT_ASSERT_EQUAL_STRING("  aあBいc", STR(format("~7S",  "aあBいc")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~00S", "aあBいc")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~01S", "aあBいc")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~02S", "aあBいc")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~03S", "aあBいc")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~04S", "aあBいc")));
    UT_ASSERT_EQUAL_STRING("aあBいc",   STR(format("~05S", "aあBいc")));
    UT_ASSERT_EQUAL_STRING(" aあBいc",  STR(format("~06S", "aあBいc")));
    UT_ASSERT_EQUAL_STRING("  aあBいc", STR(format("~07S", "aあBいc")));
}

UT_DEF2(test_4, "~P")
{
#if P32
    UT_ASSERT_EQUAL_STRING("0x00000000",
			   STR(format("~P", (void *)NULL)));
    UT_ASSERT_EQUAL_STRING("0xffffffff",
			   STR(format("~P", (void *)~0UL)));
    UT_ASSERT_EQUAL_STRING("0x00012abc",
			   STR(format("~P", (void *)0x12ABC)));
#elif P64
    UT_ASSERT_EQUAL_STRING("0x0000000000000000",
			   STR(format("~P", (void *)NULL)));
    UT_ASSERT_EQUAL_STRING("0xffffffffffffffff",
			   STR(format("~P", (void *)~0UL)));
    UT_ASSERT_EQUAL_STRING("0x0000000000012abc",
			   STR(format("~P", (void *)0x12ABC)));
#endif
}

UT_DEF2(test_5, "~D")
{
    UT_ASSERT_EQUAL_STRING("-100", STR(format("~D", -100)));
    UT_ASSERT_EQUAL_STRING("-10",  STR(format("~D", -10)));
    UT_ASSERT_EQUAL_STRING("-1",   STR(format("~D", -1)));
    UT_ASSERT_EQUAL_STRING("0",    STR(format("~D", 0)));
    UT_ASSERT_EQUAL_STRING("1",    STR(format("~D", 1)));
    UT_ASSERT_EQUAL_STRING("10",   STR(format("~D", 10)));
    UT_ASSERT_EQUAL_STRING("100",  STR(format("~D", 100)));

    UT_ASSERT_EQUAL_STRING("-100", STR(format("~0D", -100)));
    UT_ASSERT_EQUAL_STRING("-10",  STR(format("~0D", -10)));
    UT_ASSERT_EQUAL_STRING("-1",   STR(format("~0D", -1)));
    UT_ASSERT_EQUAL_STRING("0",    STR(format("~0D", 0)));
    UT_ASSERT_EQUAL_STRING("1",    STR(format("~0D", 1)));
    UT_ASSERT_EQUAL_STRING("10",   STR(format("~0D", 10)));
    UT_ASSERT_EQUAL_STRING("100",  STR(format("~0D", 100)));

    UT_ASSERT_EQUAL_STRING("-100", STR(format("~03D", -100)));
    UT_ASSERT_EQUAL_STRING("-10",  STR(format("~03D", -10)));
    UT_ASSERT_EQUAL_STRING("-01",  STR(format("~03D", -1)));
    UT_ASSERT_EQUAL_STRING("000",  STR(format("~03D", 0)));
    UT_ASSERT_EQUAL_STRING("001",  STR(format("~03D", 1)));
    UT_ASSERT_EQUAL_STRING("010",  STR(format("~03D", 10)));
    UT_ASSERT_EQUAL_STRING("100",  STR(format("~03D", 100)));

    UT_ASSERT_EQUAL_STRING("-100", STR(format("~3D", -100)));
    UT_ASSERT_EQUAL_STRING("-10",  STR(format("~3D", -10)));
    UT_ASSERT_EQUAL_STRING(" -1",  STR(format("~3D", -1)));
    UT_ASSERT_EQUAL_STRING("  0",  STR(format("~3D", 0)));
    UT_ASSERT_EQUAL_STRING("  1",  STR(format("~3D", 1)));
    UT_ASSERT_EQUAL_STRING(" 10",  STR(format("~3D", 10)));
    UT_ASSERT_EQUAL_STRING("100",  STR(format("~3D", 100)));

    UT_ASSERT_EQUAL_STRING("                                                                                                                            123",
			   STR(format("~127D", 123)));
    UT_ASSERT_EQUAL_STRING("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
			   STR(format("~0127D", 123)));
}

UT_DEF2(test_6, "~U")
{
    UT_ASSERT_EQUAL_STRING("0",    STR(format("~U", 0)));
    UT_ASSERT_EQUAL_STRING("1",    STR(format("~U", 1)));
    UT_ASSERT_EQUAL_STRING("10",   STR(format("~U", 10)));
    UT_ASSERT_EQUAL_STRING("100",  STR(format("~U", 100)));

    UT_ASSERT_EQUAL_STRING("0",    STR(format("~0U", 0)));
    UT_ASSERT_EQUAL_STRING("1",    STR(format("~0U", 1)));
    UT_ASSERT_EQUAL_STRING("10",   STR(format("~0U", 10)));
    UT_ASSERT_EQUAL_STRING("100",  STR(format("~0U", 100)));

    UT_ASSERT_EQUAL_STRING("000",  STR(format("~03U", 0)));
    UT_ASSERT_EQUAL_STRING("001",  STR(format("~03U", 1)));
    UT_ASSERT_EQUAL_STRING("010",  STR(format("~03U", 10)));
    UT_ASSERT_EQUAL_STRING("100",  STR(format("~03U", 100)));

    UT_ASSERT_EQUAL_STRING("  0",  STR(format("~3U", 0)));
    UT_ASSERT_EQUAL_STRING("  1",  STR(format("~3U", 1)));
    UT_ASSERT_EQUAL_STRING(" 10",  STR(format("~3U", 10)));
    UT_ASSERT_EQUAL_STRING("100",  STR(format("~3U", 100)));

#if I32
    UT_ASSERT_EQUAL_STRING("4294967196", STR(format("~U", -100)));
    UT_ASSERT_EQUAL_STRING("4294967286", STR(format("~U", -10)));
    UT_ASSERT_EQUAL_STRING("4294967295", STR(format("~U", -1)));

    UT_ASSERT_EQUAL_STRING("4294967196", STR(format("~0U", -100)));
    UT_ASSERT_EQUAL_STRING("4294967286", STR(format("~0U", -10)));
    UT_ASSERT_EQUAL_STRING("4294967295", STR(format("~0U", -1)));

    UT_ASSERT_EQUAL_STRING("4294967196", STR(format("~03U", -100)));
    UT_ASSERT_EQUAL_STRING("4294967286", STR(format("~03U", -10)));
    UT_ASSERT_EQUAL_STRING("4294967295", STR(format("~03U", -1)));

    UT_ASSERT_EQUAL_STRING("4294967196", STR(format("~3U", -100)));
    UT_ASSERT_EQUAL_STRING("4294967286", STR(format("~3U", -10)));
    UT_ASSERT_EQUAL_STRING("4294967295", STR(format("~3U", -1)));
#elif I64
    UT_ASSERT_EQUAL_STRING("18446744073709551516", STR(format("~U", -100)));
    UT_ASSERT_EQUAL_STRING("18446744073709551606", STR(format("~U", -10)));
    UT_ASSERT_EQUAL_STRING("18446744073709551615", STR(format("~U", -1)));

    UT_ASSERT_EQUAL_STRING("18446744073709551516", STR(format("~0U", -100)));
    UT_ASSERT_EQUAL_STRING("18446744073709551606", STR(format("~0U", -10)));
    UT_ASSERT_EQUAL_STRING("18446744073709551615", STR(format("~0U", -1)));

    UT_ASSERT_EQUAL_STRING("18446744073709551516", STR(format("~03U", -100)));
    UT_ASSERT_EQUAL_STRING("18446744073709551606", STR(format("~03U", -10)));
    UT_ASSERT_EQUAL_STRING("18446744073709551615", STR(format("~03U", -1)));

    UT_ASSERT_EQUAL_STRING("18446744073709551516", STR(format("~3U", -100)));
    UT_ASSERT_EQUAL_STRING("18446744073709551606", STR(format("~3U", -10)));
    UT_ASSERT_EQUAL_STRING("18446744073709551615", STR(format("~3U", -1)));
#endif

    UT_ASSERT_EQUAL_STRING("                                                                                                                            123",
			   STR(format("~127U", 123)));
    UT_ASSERT_EQUAL_STRING("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
			   STR(format("~0127U", 123)));
}

UT_DEF2(test_7, "~X")
{
    UT_ASSERT_EQUAL_STRING("0",    STR(format("~X", 0)));
    UT_ASSERT_EQUAL_STRING("1",    STR(format("~X", 1)));
    UT_ASSERT_EQUAL_STRING("a",    STR(format("~X", 10)));
    UT_ASSERT_EQUAL_STRING("64",   STR(format("~X", 100)));

    UT_ASSERT_EQUAL_STRING("0",    STR(format("~0X", 0)));
    UT_ASSERT_EQUAL_STRING("1",    STR(format("~0X", 1)));
    UT_ASSERT_EQUAL_STRING("a",    STR(format("~0X", 10)));
    UT_ASSERT_EQUAL_STRING("64",   STR(format("~0X", 100)));

    UT_ASSERT_EQUAL_STRING("000",  STR(format("~03X", 0)));
    UT_ASSERT_EQUAL_STRING("001",  STR(format("~03X", 1)));
    UT_ASSERT_EQUAL_STRING("00a",  STR(format("~03X", 10)));
    UT_ASSERT_EQUAL_STRING("064",  STR(format("~03X", 100)));

    UT_ASSERT_EQUAL_STRING("  0",  STR(format("~3X", 0)));
    UT_ASSERT_EQUAL_STRING("  1",  STR(format("~3X", 1)));
    UT_ASSERT_EQUAL_STRING("  a",  STR(format("~3X", 10)));
    UT_ASSERT_EQUAL_STRING(" 64",  STR(format("~3X", 100)));

#if I32
    UT_ASSERT_EQUAL_STRING("ffffff9c", STR(format("~X", -100)));
    UT_ASSERT_EQUAL_STRING("fffffff6", STR(format("~X", -10)));
    UT_ASSERT_EQUAL_STRING("ffffffff", STR(format("~X", -1)));

    UT_ASSERT_EQUAL_STRING("ffffff9c", STR(format("~0X", -100)));
    UT_ASSERT_EQUAL_STRING("fffffff6", STR(format("~0X", -10)));
    UT_ASSERT_EQUAL_STRING("ffffffff", STR(format("~0X", -1)));

    UT_ASSERT_EQUAL_STRING("ffffff9c", STR(format("~03X", -100)));
    UT_ASSERT_EQUAL_STRING("fffffff6", STR(format("~03X", -10)));
    UT_ASSERT_EQUAL_STRING("ffffffff", STR(format("~03X", -1)));

    UT_ASSERT_EQUAL_STRING("ffffff9c", STR(format("~3X", -100)));
    UT_ASSERT_EQUAL_STRING("fffffff6", STR(format("~3X", -10)));
    UT_ASSERT_EQUAL_STRING("ffffffff", STR(format("~3X", -1)));
#elif I64
    UT_ASSERT_EQUAL_STRING("ffffffffffffff9c", STR(format("~X", -100)));
    UT_ASSERT_EQUAL_STRING("fffffffffffffff6", STR(format("~X", -10)));
    UT_ASSERT_EQUAL_STRING("ffffffffffffffff", STR(format("~X", -1)));

    UT_ASSERT_EQUAL_STRING("ffffffffffffff9c", STR(format("~0X", -100)));
    UT_ASSERT_EQUAL_STRING("fffffffffffffff6", STR(format("~0X", -10)));
    UT_ASSERT_EQUAL_STRING("ffffffffffffffff", STR(format("~0X", -1)));

    UT_ASSERT_EQUAL_STRING("ffffffffffffff9c", STR(format("~03X", -100)));
    UT_ASSERT_EQUAL_STRING("fffffffffffffff6", STR(format("~03X", -10)));
    UT_ASSERT_EQUAL_STRING("ffffffffffffffff", STR(format("~03X", -1)));

    UT_ASSERT_EQUAL_STRING("ffffffffffffff9c", STR(format("~3X", -100)));
    UT_ASSERT_EQUAL_STRING("fffffffffffffff6", STR(format("~3X", -10)));
    UT_ASSERT_EQUAL_STRING("ffffffffffffffff", STR(format("~3X", -1)));
#endif

    UT_ASSERT_EQUAL_STRING("                                                                                                                            1ac",
			   STR(format("~127X", 0x1ac)));
    UT_ASSERT_EQUAL_STRING("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001ac",
			   STR(format("~0127X", 0x1ac)));
}

UT_DEF2(test_8, "~O")
{
    UT_ASSERT_EQUAL_STRING("0",    STR(format("~O", 0)));
    UT_ASSERT_EQUAL_STRING("1",    STR(format("~O", 1)));
    UT_ASSERT_EQUAL_STRING("12",   STR(format("~O", 10)));
    UT_ASSERT_EQUAL_STRING("144",  STR(format("~O", 100)));

    UT_ASSERT_EQUAL_STRING("0",    STR(format("~0O", 0)));
    UT_ASSERT_EQUAL_STRING("1",    STR(format("~0O", 1)));
    UT_ASSERT_EQUAL_STRING("12",   STR(format("~0O", 10)));
    UT_ASSERT_EQUAL_STRING("144",  STR(format("~0O", 100)));

    UT_ASSERT_EQUAL_STRING("000",  STR(format("~03O", 0)));
    UT_ASSERT_EQUAL_STRING("001",  STR(format("~03O", 1)));
    UT_ASSERT_EQUAL_STRING("012",  STR(format("~03O", 10)));
    UT_ASSERT_EQUAL_STRING("144",  STR(format("~03O", 100)));

    UT_ASSERT_EQUAL_STRING("  0",  STR(format("~3O", 0)));
    UT_ASSERT_EQUAL_STRING("  1",  STR(format("~3O", 1)));
    UT_ASSERT_EQUAL_STRING(" 12",  STR(format("~3O", 10)));
    UT_ASSERT_EQUAL_STRING("144",  STR(format("~3O", 100)));

#if I32
    UT_ASSERT_EQUAL_STRING("37777777634", STR(format("~O", -100)));
    UT_ASSERT_EQUAL_STRING("37777777766", STR(format("~O", -10)));
    UT_ASSERT_EQUAL_STRING("37777777777", STR(format("~O", -1)));

    UT_ASSERT_EQUAL_STRING("37777777634", STR(format("~0O", -100)));
    UT_ASSERT_EQUAL_STRING("37777777766", STR(format("~0O", -10)));
    UT_ASSERT_EQUAL_STRING("37777777777", STR(format("~0O", -1)));

    UT_ASSERT_EQUAL_STRING("37777777634", STR(format("~03O", -100)));
    UT_ASSERT_EQUAL_STRING("37777777766", STR(format("~03O", -10)));
    UT_ASSERT_EQUAL_STRING("37777777777", STR(format("~03O", -1)));

    UT_ASSERT_EQUAL_STRING("37777777634", STR(format("~3O", -100)));
    UT_ASSERT_EQUAL_STRING("37777777766", STR(format("~3O", -10)));
    UT_ASSERT_EQUAL_STRING("37777777777", STR(format("~3O", -1)));
#elif I64
    UT_ASSERT_EQUAL_STRING("1777777777777777777634", STR(format("~O", -100)));
    UT_ASSERT_EQUAL_STRING("1777777777777777777766", STR(format("~O", -10)));
    UT_ASSERT_EQUAL_STRING("1777777777777777777777", STR(format("~O", -1)));

    UT_ASSERT_EQUAL_STRING("1777777777777777777634", STR(format("~0O", -100)));
    UT_ASSERT_EQUAL_STRING("1777777777777777777766", STR(format("~0O", -10)));
    UT_ASSERT_EQUAL_STRING("1777777777777777777777", STR(format("~0O", -1)));

    UT_ASSERT_EQUAL_STRING("1777777777777777777634", STR(format("~03O", -100)));
    UT_ASSERT_EQUAL_STRING("1777777777777777777766", STR(format("~03O", -10)));
    UT_ASSERT_EQUAL_STRING("1777777777777777777777", STR(format("~03O", -1)));

    UT_ASSERT_EQUAL_STRING("1777777777777777777634", STR(format("~3O", -100)));
    UT_ASSERT_EQUAL_STRING("1777777777777777777766", STR(format("~3O", -10)));
    UT_ASSERT_EQUAL_STRING("1777777777777777777777", STR(format("~3O", -1)));
#endif

    UT_ASSERT_EQUAL_STRING("                                                                                                                            123",
			   STR(format("~127O", 0123)));
    UT_ASSERT_EQUAL_STRING("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
			   STR(format("~0127O", 0123)));
}

UT_DEF2(test_9, "~B")
{
    UT_ASSERT_EQUAL_STRING("0",       STR(format("~B", 0)));
    UT_ASSERT_EQUAL_STRING("1",       STR(format("~B", 1)));
    UT_ASSERT_EQUAL_STRING("1010",    STR(format("~B", 10)));
    UT_ASSERT_EQUAL_STRING("1100100", STR(format("~B", 100)));

    UT_ASSERT_EQUAL_STRING("0",       STR(format("~0B", 0)));
    UT_ASSERT_EQUAL_STRING("1",       STR(format("~0B", 1)));
    UT_ASSERT_EQUAL_STRING("1010",    STR(format("~0B", 10)));
    UT_ASSERT_EQUAL_STRING("1100100", STR(format("~0B", 100)));

    UT_ASSERT_EQUAL_STRING("00000",   STR(format("~05B", 0)));
    UT_ASSERT_EQUAL_STRING("00001",   STR(format("~05B", 1)));
    UT_ASSERT_EQUAL_STRING("01010",   STR(format("~05B", 10)));
    UT_ASSERT_EQUAL_STRING("1100100", STR(format("~05B", 100)));

    UT_ASSERT_EQUAL_STRING("    0",   STR(format("~5B", 0)));
    UT_ASSERT_EQUAL_STRING("    1",   STR(format("~5B", 1)));
    UT_ASSERT_EQUAL_STRING(" 1010",   STR(format("~5B", 10)));
    UT_ASSERT_EQUAL_STRING("1100100", STR(format("~5B", 100)));

#if I32
    UT_ASSERT_EQUAL_STRING("11111111111111111111111110011100",
			   STR(format("~B", -100)));
    UT_ASSERT_EQUAL_STRING("11111111111111111111111111110110",
			   STR(format("~B", -10)));
    UT_ASSERT_EQUAL_STRING("11111111111111111111111111111111",
			   STR(format("~B", -1)));

    UT_ASSERT_EQUAL_STRING("11111111111111111111111110011100",
			   STR(format("~0B", -100)));
    UT_ASSERT_EQUAL_STRING("11111111111111111111111111110110",
			   STR(format("~0B", -10)));
    UT_ASSERT_EQUAL_STRING("11111111111111111111111111111111",
			   STR(format("~0B", -1)));

    UT_ASSERT_EQUAL_STRING("11111111111111111111111110011100",
			   STR(format("~05B", -100)));
    UT_ASSERT_EQUAL_STRING("11111111111111111111111111110110",
			   STR(format("~05B", -10)));
    UT_ASSERT_EQUAL_STRING("11111111111111111111111111111111",
			   STR(format("~05B", -1)));

    UT_ASSERT_EQUAL_STRING("11111111111111111111111110011100",
			   STR(format("~5B", -100)));
    UT_ASSERT_EQUAL_STRING("11111111111111111111111111110110",
			   STR(format("~5B", -10)));
    UT_ASSERT_EQUAL_STRING("11111111111111111111111111111111",
			   STR(format("~5B", -1)));
#elif I64
    UT_ASSERT_EQUAL_STRING("1111111111111111111111111111111111111111111111111111111110011100",
			   STR(format("~B", -100)));
    UT_ASSERT_EQUAL_STRING("1111111111111111111111111111111111111111111111111111111111110110",
			   STR(format("~B", -10)));
    UT_ASSERT_EQUAL_STRING("1111111111111111111111111111111111111111111111111111111111111111",
			   STR(format("~B", -1)));

    UT_ASSERT_EQUAL_STRING("1111111111111111111111111111111111111111111111111111111110011100",
			   STR(format("~0B", -100)));
    UT_ASSERT_EQUAL_STRING("1111111111111111111111111111111111111111111111111111111111110110",
			   STR(format("~0B", -10)));
    UT_ASSERT_EQUAL_STRING("1111111111111111111111111111111111111111111111111111111111111111",
			   STR(format("~0B", -1)));

    UT_ASSERT_EQUAL_STRING("1111111111111111111111111111111111111111111111111111111110011100",
			   STR(format("~05B", -100)));
    UT_ASSERT_EQUAL_STRING("1111111111111111111111111111111111111111111111111111111111110110",
			   STR(format("~05B", -10)));
    UT_ASSERT_EQUAL_STRING("1111111111111111111111111111111111111111111111111111111111111111",
			   STR(format("~05B", -1)));

    UT_ASSERT_EQUAL_STRING("1111111111111111111111111111111111111111111111111111111110011100",
			   STR(format("~5B", -100)));
    UT_ASSERT_EQUAL_STRING("1111111111111111111111111111111111111111111111111111111111110110",
			   STR(format("~5B", -10)));
    UT_ASSERT_EQUAL_STRING("1111111111111111111111111111111111111111111111111111111111111111",
			   STR(format("~5B", -1)));
#endif

    UT_ASSERT_EQUAL_STRING("                                                                                                                            101",
			   STR(format("~127B", 0x5)));
    UT_ASSERT_EQUAL_STRING("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101",
			   STR(format("~0127B", 0x5)));
}

UT_REGISTER_BEGIN("format")
UT_REGISTER(test_1, "no directives")
UT_REGISTER(test_2, "~C")
UT_REGISTER(test_3, "~S")
UT_REGISTER(test_4, "~P")
UT_REGISTER(test_5, "~D")
UT_REGISTER(test_6, "~U")
UT_REGISTER(test_7, "~X")
UT_REGISTER(test_8, "~O")
UT_REGISTER(test_9, "~B")
UT_REGISTER_END
