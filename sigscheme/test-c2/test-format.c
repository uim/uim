/*===========================================================================
 *  Filename : test-format.c
 *  About    : unit test for format functions
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

/* All tests in this file are passed against r3179 (new repository) */

#include <stddef.h>
#include <stdarg.h>
#include <string.h>

#define TST_HAVE_MAIN 1
#include "sscm-test.h"

#include <sigscheme/sigscheme-stdint.h>
#include <sigscheme/sigscheme.h>
#include "sigschemeinternal.h"

#define MSG_SSCM_DIRECTIVE_HELP                                              \
"(format+ [<port>] <format-string> [<arg>...])\n"                            \
"  - <port> is #t, #f or an output-port\n"                                   \
"  - any escape sequence is case insensitive\n"                              \
"\n"                                                                         \
"  The format+ procedure is a SigScheme-specific superset of SRFI-48.\n"     \
"  Following directives accept optional width w and d digits after the decimal,\n" \
"  and w accepts leading zero as zero-digit-padding specifier. All other rules\n" \
"  are same as SRFI-48. See also the help message for SRFI-48.\n"            \
"\n"                                                                         \
"SEQ        MNEMONIC       DESCRIPTION\n"                                    \
"~[w[,d]]D  [Decimal]      the arg is a number output in decimal radix\n"    \
"~[w[,d]]X  [heXadecimal]  the arg is a number output in hexdecimal radix\n" \
"~[w[,d]]O  [Octal]        the arg is a number output in octal radix\n"      \
"~[w[,d]]B  [Binary]       the arg is a number output in binary radix\n"     \
"~[w[,d]]F  [Fixed]        the arg is a string or number\n"

#define I32 (SIZEOF_INT    == SIZEOF_INT32_T)
#define L32 (SIZEOF_LONG   == SIZEOF_INT32_T)
#define P32 (SIZEOF_VOID_P == SIZEOF_INT32_T)

#define I64 (SIZEOF_INT    == SIZEOF_INT64_T)
#define L64 (SIZEOF_LONG   == SIZEOF_INT64_T)
#define P64 (SIZEOF_VOID_P == SIZEOF_INT64_T)

#define IMAX32 (SIZEOF_INTMAX_T == SIZEOF_INT32_T)
#define IMAX64 (SIZEOF_INTMAX_T == SIZEOF_INT64_T)

#define PDIFF32 (SIZEOF_PTRDIFF_T == SIZEOF_INT32_T)
#define PDIFF64 (SIZEOF_PTRDIFF_T == SIZEOF_INT64_T)

#define STR SCM_STRING_STR

static ScmObj lst, clst;

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

int
main(int argc, char **argv)
{
    tst_suite_info suite = TST_DEFAULT_SUITE_SETUP;

    scm_initialize(NULL);

    scm_gc_protect_with_init(&lst,
                             LIST_5(SCM_TRUE,
                                    MAKE_INT(123),
                                    MAKE_CHAR('a'),
                                    CONST_STRING("aBc"),
                                    LIST_1(MAKE_INT(0))));

    scm_gc_protect_with_init(&clst, LIST_2(MAKE_INT(0), MAKE_INT(1)));
    SET_CDR(CDR(clst), clst);

    tst_main(&suite);

    scm_finalize();

    TST_DEFAULT_SUITE_CLEANUP(suite);
    return !!suite.stats.fail;
}

TST_CASE("format no directives")
{
    TST_TN_EQ_STR("",   STR(format("")));
    TST_TN_EQ_STR("\"", STR(format("\"")));
    TST_TN_EQ_STR("\\", STR(format("\\")));
    TST_TN_EQ_STR("a",  STR(format("a")));
    TST_TN_EQ_STR("aBc",     STR(format("aBc")));
    TST_TN_EQ_STR("あ",      STR(format("あ")));
    TST_TN_EQ_STR("あい",    STR(format("あい")));
    TST_TN_EQ_STR("aあBいc", STR(format("aあBいc")));
}

TST_CASE("format ~C")
{
    TST_TN_EQ_STR("",   STR(format("~C", (scm_ichar_t)'\0')));
    TST_TN_EQ_STR("a",  STR(format("~C", (scm_ichar_t)'a')));
    TST_TN_EQ_STR("\"", STR(format("~C", (scm_ichar_t)'\"')));
    TST_TN_EQ_STR("\\", STR(format("~C", (scm_ichar_t)'\\')));
    TST_TN_EQ_STR("あ", STR(format("~C", (scm_ichar_t)0x3042)));
}

TST_CASE("format ~S")
{
    TST_TN_EQ_STR("",          STR(format("~S",   "")));
    TST_TN_EQ_STR("",          STR(format("~0S",  "")));
    TST_TN_EQ_STR(" ",         STR(format("~1S",  "")));
    TST_TN_EQ_STR("",          STR(format("~00S", "")));
    TST_TN_EQ_STR(" ",         STR(format("~01S", "")));
    TST_TN_EQ_STR("\"",        STR(format("~S",   "\"")));
    TST_TN_EQ_STR("\\",        STR(format("~S",   "\\")));
    TST_TN_EQ_STR("a",         STR(format("~S",   "a")));
    TST_TN_EQ_STR("aBc",       STR(format("~S",   "aBc")));
    TST_TN_EQ_STR("あ",        STR(format("~S",   "あ")));
    TST_TN_EQ_STR("あい",      STR(format("~S",   "あい")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~S",   "aあBいc")));
    TST_TN_EQ_STR("aBc",       STR(format("~0S",  "aBc")));
    TST_TN_EQ_STR("aBc",       STR(format("~1S",  "aBc")));
    TST_TN_EQ_STR("aBc",       STR(format("~2S",  "aBc")));
    TST_TN_EQ_STR("aBc",       STR(format("~3S",  "aBc")));
    TST_TN_EQ_STR(" aBc",      STR(format("~4S",  "aBc")));
    TST_TN_EQ_STR("  aBc",     STR(format("~5S",  "aBc")));
    TST_TN_EQ_STR("aBc",       STR(format("~00S", "aBc")));
    TST_TN_EQ_STR("aBc",       STR(format("~01S", "aBc")));
    TST_TN_EQ_STR("aBc",       STR(format("~02S", "aBc")));
    TST_TN_EQ_STR("aBc",       STR(format("~03S", "aBc")));
    TST_TN_EQ_STR(" aBc",      STR(format("~04S", "aBc")));
    TST_TN_EQ_STR("  aBc",     STR(format("~05S", "aBc")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~0S",  "aあBいc")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~1S",  "aあBいc")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~2S",  "aあBいc")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~3S",  "aあBいc")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~4S",  "aあBいc")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~5S",  "aあBいc")));
    TST_TN_EQ_STR(" aあBいc",  STR(format("~6S",  "aあBいc")));
    TST_TN_EQ_STR("  aあBいc", STR(format("~7S",  "aあBいc")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~00S", "aあBいc")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~01S", "aあBいc")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~02S", "aあBいc")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~03S", "aあBいc")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~04S", "aあBいc")));
    TST_TN_EQ_STR("aあBいc",   STR(format("~05S", "aあBいc")));
    TST_TN_EQ_STR(" aあBいc",  STR(format("~06S", "aあBいc")));
    TST_TN_EQ_STR("  aあBいc", STR(format("~07S", "aあBいc")));
}

TST_CASE("format ~P")
{
#if P32
    TST_TN_EQ_STR("0x00000000", STR(format("~P", (void *)NULL)));
    TST_TN_EQ_STR("0xffffffff", STR(format("~P", (void *)~0UL)));
    TST_TN_EQ_STR("0x00012abc", STR(format("~P", (void *)0x12ABC)));
#elif P64
    TST_TN_EQ_STR("0x0000000000000000", STR(format("~P", (void *)NULL)));
    TST_TN_EQ_STR("0xffffffffffffffff", STR(format("~P", (void *)~0UL)));
    TST_TN_EQ_STR("0x0000000000012abc", STR(format("~P", (void *)0x12ABC)));
#endif
}

TST_CASE("format ~D")
{
    TST_TN_EQ_STR("-100", STR(format("~D",   -100)));
    TST_TN_EQ_STR("-10",  STR(format("~D",   -10)));
    TST_TN_EQ_STR("-1",   STR(format("~D",   -1)));
    TST_TN_EQ_STR("0",    STR(format("~D",   0)));
    TST_TN_EQ_STR("1",    STR(format("~D",   1)));
    TST_TN_EQ_STR("10",   STR(format("~D",   10)));
    TST_TN_EQ_STR("100",  STR(format("~D",   100)));

    TST_TN_EQ_STR("-100", STR(format("~0D",  -100)));
    TST_TN_EQ_STR("-10",  STR(format("~0D",  -10)));
    TST_TN_EQ_STR("-1",   STR(format("~0D",  -1)));
    TST_TN_EQ_STR("0",    STR(format("~0D",  0)));
    TST_TN_EQ_STR("1",    STR(format("~0D",  1)));
    TST_TN_EQ_STR("10",   STR(format("~0D",  10)));
    TST_TN_EQ_STR("100",  STR(format("~0D",  100)));

    TST_TN_EQ_STR("-100", STR(format("~03D", -100)));
    TST_TN_EQ_STR("-10",  STR(format("~03D", -10)));
    TST_TN_EQ_STR("-01",  STR(format("~03D", -1)));
    TST_TN_EQ_STR("000",  STR(format("~03D", 0)));
    TST_TN_EQ_STR("001",  STR(format("~03D", 1)));
    TST_TN_EQ_STR("010",  STR(format("~03D", 10)));
    TST_TN_EQ_STR("100",  STR(format("~03D", 100)));

    TST_TN_EQ_STR("-100", STR(format("~3D",  -100)));
    TST_TN_EQ_STR("-10",  STR(format("~3D",  -10)));
    TST_TN_EQ_STR(" -1",  STR(format("~3D",  -1)));
    TST_TN_EQ_STR("  0",  STR(format("~3D",  0)));
    TST_TN_EQ_STR("  1",  STR(format("~3D",  1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3D",  10)));
    TST_TN_EQ_STR("100",  STR(format("~3D",  100)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127D", 123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127D", 123)));
}

TST_CASE("format ~U")
{
    TST_TN_EQ_STR("0",    STR(format("~U",   0)));
    TST_TN_EQ_STR("1",    STR(format("~U",   1)));
    TST_TN_EQ_STR("10",   STR(format("~U",   10)));
    TST_TN_EQ_STR("100",  STR(format("~U",   100)));

    TST_TN_EQ_STR("0",    STR(format("~0U",  0)));
    TST_TN_EQ_STR("1",    STR(format("~0U",  1)));
    TST_TN_EQ_STR("10",   STR(format("~0U",  10)));
    TST_TN_EQ_STR("100",  STR(format("~0U",  100)));

    TST_TN_EQ_STR("000",  STR(format("~03U", 0)));
    TST_TN_EQ_STR("001",  STR(format("~03U", 1)));
    TST_TN_EQ_STR("010",  STR(format("~03U", 10)));
    TST_TN_EQ_STR("100",  STR(format("~03U", 100)));

    TST_TN_EQ_STR("  0",  STR(format("~3U",  0)));
    TST_TN_EQ_STR("  1",  STR(format("~3U",  1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3U",  10)));
    TST_TN_EQ_STR("100",  STR(format("~3U",  100)));

#if I32
    TST_TN_EQ_STR("4294967196", STR(format("~U",   -100)));
    TST_TN_EQ_STR("4294967286", STR(format("~U",   -10)));
    TST_TN_EQ_STR("4294967295", STR(format("~U",   -1)));

    TST_TN_EQ_STR("4294967196", STR(format("~0U",  -100)));
    TST_TN_EQ_STR("4294967286", STR(format("~0U",  -10)));
    TST_TN_EQ_STR("4294967295", STR(format("~0U",  -1)));

    TST_TN_EQ_STR("4294967196", STR(format("~03U", -100)));
    TST_TN_EQ_STR("4294967286", STR(format("~03U", -10)));
    TST_TN_EQ_STR("4294967295", STR(format("~03U", -1)));

    TST_TN_EQ_STR("4294967196", STR(format("~3U",  -100)));
    TST_TN_EQ_STR("4294967286", STR(format("~3U",  -10)));
    TST_TN_EQ_STR("4294967295", STR(format("~3U",  -1)));
#elif I64
    TST_TN_EQ_STR("18446744073709551516", STR(format("~U",   -100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~U",   -10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~U",   -1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~0U",  -100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~0U",  -10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~0U",  -1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~03U", -100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~03U", -10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~03U", -1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~3U",  -100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~3U",  -10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~3U",  -1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127U", 123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127U", 123)));
}

TST_CASE("format ~X")
{
    TST_TN_EQ_STR("0",    STR(format("~X",   0)));
    TST_TN_EQ_STR("1",    STR(format("~X",   1)));
    TST_TN_EQ_STR("a",    STR(format("~X",   10)));
    TST_TN_EQ_STR("64",   STR(format("~X",   100)));

    TST_TN_EQ_STR("0",    STR(format("~0X",  0)));
    TST_TN_EQ_STR("1",    STR(format("~0X",  1)));
    TST_TN_EQ_STR("a",    STR(format("~0X",  10)));
    TST_TN_EQ_STR("64",   STR(format("~0X",  100)));

    TST_TN_EQ_STR("000",  STR(format("~03X", 0)));
    TST_TN_EQ_STR("001",  STR(format("~03X", 1)));
    TST_TN_EQ_STR("00a",  STR(format("~03X", 10)));
    TST_TN_EQ_STR("064",  STR(format("~03X", 100)));

    TST_TN_EQ_STR("  0",  STR(format("~3X",  0)));
    TST_TN_EQ_STR("  1",  STR(format("~3X",  1)));
    TST_TN_EQ_STR("  a",  STR(format("~3X",  10)));
    TST_TN_EQ_STR(" 64",  STR(format("~3X",  100)));

#if I32
    TST_TN_EQ_STR("ffffff9c", STR(format("~X",   -100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~X",   -10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~X",   -1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~0X",  -100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~0X",  -10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~0X",  -1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~03X", -100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~03X", -10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~03X", -1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~3X",  -100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~3X",  -10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~3X",  -1)));
#elif I64
    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~X",   -100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~X",   -10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~X",   -1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~0X",  -100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~0X",  -10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~0X",  -1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~03X", -100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~03X", -10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~03X", -1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~3X",  -100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~3X",  -10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~3X",  -1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            1ac",
                  STR(format("~127X", 0x1ac)));
    TST_TN_EQ_STR("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001ac",
                  STR(format("~0127X", 0x1ac)));
}

TST_CASE("format ~O")
{
    TST_TN_EQ_STR("0",    STR(format("~O",   0)));
    TST_TN_EQ_STR("1",    STR(format("~O",   1)));
    TST_TN_EQ_STR("12",   STR(format("~O",   10)));
    TST_TN_EQ_STR("144",  STR(format("~O",   100)));

    TST_TN_EQ_STR("0",    STR(format("~0O",  0)));
    TST_TN_EQ_STR("1",    STR(format("~0O",  1)));
    TST_TN_EQ_STR("12",   STR(format("~0O",  10)));
    TST_TN_EQ_STR("144",  STR(format("~0O",  100)));

    TST_TN_EQ_STR("000",  STR(format("~03O", 0)));
    TST_TN_EQ_STR("001",  STR(format("~03O", 1)));
    TST_TN_EQ_STR("012",  STR(format("~03O", 10)));
    TST_TN_EQ_STR("144",  STR(format("~03O", 100)));

    TST_TN_EQ_STR("  0",  STR(format("~3O",  0)));
    TST_TN_EQ_STR("  1",  STR(format("~3O",  1)));
    TST_TN_EQ_STR(" 12",  STR(format("~3O",  10)));
    TST_TN_EQ_STR("144",  STR(format("~3O",  100)));

#if I32
    TST_TN_EQ_STR("37777777634", STR(format("~O",   -100)));
    TST_TN_EQ_STR("37777777766", STR(format("~O",   -10)));
    TST_TN_EQ_STR("37777777777", STR(format("~O",   -1)));

    TST_TN_EQ_STR("37777777634", STR(format("~0O",  -100)));
    TST_TN_EQ_STR("37777777766", STR(format("~0O",  -10)));
    TST_TN_EQ_STR("37777777777", STR(format("~0O",  -1)));

    TST_TN_EQ_STR("37777777634", STR(format("~03O", -100)));
    TST_TN_EQ_STR("37777777766", STR(format("~03O", -10)));
    TST_TN_EQ_STR("37777777777", STR(format("~03O", -1)));

    TST_TN_EQ_STR("37777777634", STR(format("~3O",  -100)));
    TST_TN_EQ_STR("37777777766", STR(format("~3O",  -10)));
    TST_TN_EQ_STR("37777777777", STR(format("~3O",  -1)));
#elif I64
    TST_TN_EQ_STR("1777777777777777777634", STR(format("~O",  -100)));
    TST_TN_EQ_STR("1777777777777777777766", STR(format("~O",  -10)));
    TST_TN_EQ_STR("1777777777777777777777", STR(format("~O",  -1)));

    TST_TN_EQ_STR("1777777777777777777634", STR(format("~0O", -100)));
    TST_TN_EQ_STR("1777777777777777777766", STR(format("~0O", -10)));
    TST_TN_EQ_STR("1777777777777777777777", STR(format("~0O", -1)));

    TST_TN_EQ_STR("1777777777777777777634", STR(format("~03O", -100)));
    TST_TN_EQ_STR("1777777777777777777766", STR(format("~03O", -10)));
    TST_TN_EQ_STR("1777777777777777777777", STR(format("~03O", -1)));

    TST_TN_EQ_STR("1777777777777777777634", STR(format("~3O", -100)));
    TST_TN_EQ_STR("1777777777777777777766", STR(format("~3O", -10)));
    TST_TN_EQ_STR("1777777777777777777777", STR(format("~3O", -1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127O", 0123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127O", 0123)));
}

TST_CASE("format ~B")
{
    TST_TN_EQ_STR("0",       STR(format("~B",   0)));
    TST_TN_EQ_STR("1",       STR(format("~B",   1)));
    TST_TN_EQ_STR("1010",    STR(format("~B",   10)));
    TST_TN_EQ_STR("1100100", STR(format("~B",   100)));

    TST_TN_EQ_STR("0",       STR(format("~0B",  0)));
    TST_TN_EQ_STR("1",       STR(format("~0B",  1)));
    TST_TN_EQ_STR("1010",    STR(format("~0B",  10)));
    TST_TN_EQ_STR("1100100", STR(format("~0B",  100)));

    TST_TN_EQ_STR("00000",   STR(format("~05B", 0)));
    TST_TN_EQ_STR("00001",   STR(format("~05B", 1)));
    TST_TN_EQ_STR("01010",   STR(format("~05B", 10)));
    TST_TN_EQ_STR("1100100", STR(format("~05B", 100)));

    TST_TN_EQ_STR("    0",   STR(format("~5B",  0)));
    TST_TN_EQ_STR("    1",   STR(format("~5B",  1)));
    TST_TN_EQ_STR(" 1010",   STR(format("~5B",  10)));
    TST_TN_EQ_STR("1100100", STR(format("~5B",  100)));

#if I32
    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~B", -100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~B", -10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~B", -1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~0B", -100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~0B", -10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~0B", -1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~05B", -100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~05B", -10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~05B", -1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~5B", -100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~5B", -10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~5B", -1)));
#elif I64
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~B", -100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~B", -10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~B", -1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~0B", -100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~0B", -10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~0B", -1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~05B", -100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~05B", -10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~05B", -1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~5B", -100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~5B", -10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~5B", -1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            101",
                  STR(format("~127B", 0x5)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101",
                  STR(format("~0127B", 0x5)));
}

TST_CASE("format ~WD")
{
    TST_TN_EQ_STR("-100", STR(format("~WD",   (int32_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~WD",   (int32_t)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~WD",   (int32_t)-1)));
    TST_TN_EQ_STR("0",    STR(format("~WD",   (int32_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~WD",   (int32_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~WD",   (int32_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~WD",   (int32_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~0WD",  (int32_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~0WD",  (int32_t)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~0WD",  (int32_t)-1)));
    TST_TN_EQ_STR("0",    STR(format("~0WD",  (int32_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0WD",  (int32_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~0WD",  (int32_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~0WD",  (int32_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~03WD", (int32_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~03WD", (int32_t)-10)));
    TST_TN_EQ_STR("-01",  STR(format("~03WD", (int32_t)-1)));
    TST_TN_EQ_STR("000",  STR(format("~03WD", (int32_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03WD", (int32_t)1)));
    TST_TN_EQ_STR("010",  STR(format("~03WD", (int32_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~03WD", (int32_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~3WD",  (int32_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~3WD",  (int32_t)-10)));
    TST_TN_EQ_STR(" -1",  STR(format("~3WD",  (int32_t)-1)));
    TST_TN_EQ_STR("  0",  STR(format("~3WD",  (int32_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3WD",  (int32_t)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3WD",  (int32_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~3WD",  (int32_t)100)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127WD", (int32_t)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127WD", (int32_t)123)));
}

TST_CASE("format ~WU")
{
    TST_TN_EQ_STR("0",    STR(format("~WU",   (int32_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~WU",   (int32_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~WU",   (int32_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~WU",   (int32_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0WU",  (int32_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0WU",  (int32_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~0WU",  (int32_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~0WU",  (int32_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03WU", (int32_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03WU", (int32_t)1)));
    TST_TN_EQ_STR("010",  STR(format("~03WU", (int32_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~03WU", (int32_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3WU",  (int32_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3WU",  (int32_t)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3WU",  (int32_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~3WU",  (int32_t)100)));

    TST_TN_EQ_STR("4294967196", STR(format("~WU",   (int32_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~WU",   (int32_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~WU",   (int32_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~0WU",  (int32_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~0WU",  (int32_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~0WU",  (int32_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~03WU", (int32_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~03WU", (int32_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~03WU", (int32_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~3WU",  (int32_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~3WU",  (int32_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~3WU",  (int32_t)-1)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127WU", (int32_t)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127WU", (int32_t)123)));
}

TST_CASE("format ~WX")
{
    TST_TN_EQ_STR("0",    STR(format("~WX",   (int32_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~WX",   (int32_t)1)));
    TST_TN_EQ_STR("a",    STR(format("~WX",   (int32_t)10)));
    TST_TN_EQ_STR("64",   STR(format("~WX",   (int32_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0WX",  (int32_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0WX",  (int32_t)1)));
    TST_TN_EQ_STR("a",    STR(format("~0WX",  (int32_t)10)));
    TST_TN_EQ_STR("64",   STR(format("~0WX",  (int32_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03WX", (int32_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03WX", (int32_t)1)));
    TST_TN_EQ_STR("00a",  STR(format("~03WX", (int32_t)10)));
    TST_TN_EQ_STR("064",  STR(format("~03WX", (int32_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3WX",  (int32_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3WX",  (int32_t)1)));
    TST_TN_EQ_STR("  a",  STR(format("~3WX",  (int32_t)10)));
    TST_TN_EQ_STR(" 64",  STR(format("~3WX",  (int32_t)100)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~WX",   (int32_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~WX",   (int32_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~WX",   (int32_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~0WX",  (int32_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~0WX",  (int32_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~0WX",  (int32_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~03WX", (int32_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~03WX", (int32_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~03WX", (int32_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~3WX",  (int32_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~3WX",  (int32_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~3WX",  (int32_t)-1)));

    TST_TN_EQ_STR("                                                                                                                            1ac",
                  STR(format("~127WX", (int32_t)0x1ac)));
    TST_TN_EQ_STR("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001ac",
                  STR(format("~0127WX", (int32_t)0x1ac)));
}

TST_CASE("format ~WO")
{
    TST_TN_EQ_STR("0",    STR(format("~WO",   (int32_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~WO",   (int32_t)1)));
    TST_TN_EQ_STR("12",   STR(format("~WO",   (int32_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~WO",   (int32_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0WO",  (int32_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0WO",  (int32_t)1)));
    TST_TN_EQ_STR("12",   STR(format("~0WO",  (int32_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~0WO",  (int32_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03WO", (int32_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03WO", (int32_t)1)));
    TST_TN_EQ_STR("012",  STR(format("~03WO", (int32_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~03WO", (int32_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3WO",  (int32_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3WO",  (int32_t)1)));
    TST_TN_EQ_STR(" 12",  STR(format("~3WO",  (int32_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~3WO",  (int32_t)100)));

    TST_TN_EQ_STR("37777777634", STR(format("~WO",   (int32_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~WO",   (int32_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~WO",   (int32_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~0WO",  (int32_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~0WO",  (int32_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~0WO",  (int32_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~03WO", (int32_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~03WO", (int32_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~03WO", (int32_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~3WO",  (int32_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~3WO",  (int32_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~3WO",  (int32_t)-1)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127WO", (int32_t)0123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127WO", (int32_t)0123)));
}

TST_CASE("format ~WB")
{
    TST_TN_EQ_STR("0",       STR(format("~WB",   (int32_t)0)));
    TST_TN_EQ_STR("1",       STR(format("~WB",   (int32_t)1)));
    TST_TN_EQ_STR("1010",    STR(format("~WB",   (int32_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~WB",   (int32_t)100)));

    TST_TN_EQ_STR("0",       STR(format("~0WB",  (int32_t)0)));
    TST_TN_EQ_STR("1",       STR(format("~0WB",  (int32_t)1)));
    TST_TN_EQ_STR("1010",    STR(format("~0WB",  (int32_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~0WB",  (int32_t)100)));

    TST_TN_EQ_STR("00000",   STR(format("~05WB", (int32_t)0)));
    TST_TN_EQ_STR("00001",   STR(format("~05WB", (int32_t)1)));
    TST_TN_EQ_STR("01010",   STR(format("~05WB", (int32_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~05WB", (int32_t)100)));

    TST_TN_EQ_STR("    0",   STR(format("~5WB",  (int32_t)0)));
    TST_TN_EQ_STR("    1",   STR(format("~5WB",  (int32_t)1)));
    TST_TN_EQ_STR(" 1010",   STR(format("~5WB",  (int32_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~5WB",  (int32_t)100)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~WB", (int32_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~WB", (int32_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~WB", (int32_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~0WB", (int32_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~0WB", (int32_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~0WB", (int32_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~05WB", (int32_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~05WB", (int32_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~05WB", (int32_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~5WB", (int32_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~5WB", (int32_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~5WB", (int32_t)-1)));

    TST_TN_EQ_STR("                                                                                                                            101",
                  STR(format("~127WB", (int32_t)0x5)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101",
                  STR(format("~0127WB", (int32_t)0x5)));
}

TST_CASE("format ~MD")
{
    TST_TN_EQ_STR("-100", STR(format("~MD",   (scm_int_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~MD",   (scm_int_t)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~MD",   (scm_int_t)-1)));
    TST_TN_EQ_STR("0",    STR(format("~MD",   (scm_int_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~MD",   (scm_int_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~MD",   (scm_int_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~MD",   (scm_int_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~0MD",  (scm_int_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~0MD",  (scm_int_t)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~0MD",  (scm_int_t)-1)));
    TST_TN_EQ_STR("0",    STR(format("~0MD",  (scm_int_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0MD",  (scm_int_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~0MD",  (scm_int_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~0MD",  (scm_int_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~03MD", (scm_int_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~03MD", (scm_int_t)-10)));
    TST_TN_EQ_STR("-01",  STR(format("~03MD", (scm_int_t)-1)));
    TST_TN_EQ_STR("000",  STR(format("~03MD", (scm_int_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03MD", (scm_int_t)1)));
    TST_TN_EQ_STR("010",  STR(format("~03MD", (scm_int_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~03MD", (scm_int_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~3MD",  (scm_int_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~3MD",  (scm_int_t)-10)));
    TST_TN_EQ_STR(" -1",  STR(format("~3MD",  (scm_int_t)-1)));
    TST_TN_EQ_STR("  0",  STR(format("~3MD",  (scm_int_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3MD",  (scm_int_t)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3MD",  (scm_int_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~3MD",  (scm_int_t)100)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127MD", (scm_int_t)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127MD", (scm_int_t)123)));
}

TST_CASE("format ~MU")
{
    TST_TN_EQ_STR("0",    STR(format("~MU",   (scm_int_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~MU",   (scm_int_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~MU",   (scm_int_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~MU",   (scm_int_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0MU",  (scm_int_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0MU",  (scm_int_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~0MU",  (scm_int_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~0MU",  (scm_int_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03MU", (scm_int_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03MU", (scm_int_t)1)));
    TST_TN_EQ_STR("010",  STR(format("~03MU", (scm_int_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~03MU", (scm_int_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3MU",  (scm_int_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3MU",  (scm_int_t)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3MU",  (scm_int_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~3MU",  (scm_int_t)100)));

    TST_TN_EQ_STR("4294967196", STR(format("~MU",   (scm_int_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~MU",   (scm_int_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~MU",   (scm_int_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~0MU",  (scm_int_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~0MU",  (scm_int_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~0MU",  (scm_int_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~03MU", (scm_int_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~03MU", (scm_int_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~03MU", (scm_int_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~3MU",  (scm_int_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~3MU",  (scm_int_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~3MU",  (scm_int_t)-1)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127MU", (scm_int_t)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127MU", (scm_int_t)123)));
}

TST_CASE("format ~MX")
{
    TST_TN_EQ_STR("0",    STR(format("~MX",   (scm_int_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~MX",   (scm_int_t)1)));
    TST_TN_EQ_STR("a",    STR(format("~MX",   (scm_int_t)10)));
    TST_TN_EQ_STR("64",   STR(format("~MX",   (scm_int_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0MX",  (scm_int_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0MX",  (scm_int_t)1)));
    TST_TN_EQ_STR("a",    STR(format("~0MX",  (scm_int_t)10)));
    TST_TN_EQ_STR("64",   STR(format("~0MX",  (scm_int_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03MX", (scm_int_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03MX", (scm_int_t)1)));
    TST_TN_EQ_STR("00a",  STR(format("~03MX", (scm_int_t)10)));
    TST_TN_EQ_STR("064",  STR(format("~03MX", (scm_int_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3MX",  (scm_int_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3MX",  (scm_int_t)1)));
    TST_TN_EQ_STR("  a",  STR(format("~3MX",  (scm_int_t)10)));
    TST_TN_EQ_STR(" 64",  STR(format("~3MX",  (scm_int_t)100)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~MX",   (scm_int_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~MX",   (scm_int_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~MX",   (scm_int_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~0MX",  (scm_int_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~0MX",  (scm_int_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~0MX",  (scm_int_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~03MX", (scm_int_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~03MX", (scm_int_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~03MX", (scm_int_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~3MX",  (scm_int_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~3MX",  (scm_int_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~3MX",  (scm_int_t)-1)));

    TST_TN_EQ_STR("                                                                                                                            1ac",
                  STR(format("~127MX", (scm_int_t)0x1ac)));
    TST_TN_EQ_STR("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001ac",
                  STR(format("~0127MX", (scm_int_t)0x1ac)));
}

TST_CASE("format ~MO")
{
    TST_TN_EQ_STR("0",    STR(format("~MO",   (scm_int_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~MO",   (scm_int_t)1)));
    TST_TN_EQ_STR("12",   STR(format("~MO",   (scm_int_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~MO",   (scm_int_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0MO",  (scm_int_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0MO",  (scm_int_t)1)));
    TST_TN_EQ_STR("12",   STR(format("~0MO",  (scm_int_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~0MO",  (scm_int_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03MO", (scm_int_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03MO", (scm_int_t)1)));
    TST_TN_EQ_STR("012",  STR(format("~03MO", (scm_int_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~03MO", (scm_int_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3MO",  (scm_int_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3MO",  (scm_int_t)1)));
    TST_TN_EQ_STR(" 12",  STR(format("~3MO",  (scm_int_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~3MO",  (scm_int_t)100)));

    TST_TN_EQ_STR("37777777634", STR(format("~MO",   (scm_int_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~MO",   (scm_int_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~MO",   (scm_int_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~0MO",  (scm_int_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~0MO",  (scm_int_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~0MO",  (scm_int_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~03MO", (scm_int_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~03MO", (scm_int_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~03MO", (scm_int_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~3MO",  (scm_int_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~3MO",  (scm_int_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~3MO",  (scm_int_t)-1)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127MO", (scm_int_t)0123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127MO", (scm_int_t)0123)));
}

TST_CASE("format ~MB")
{
    TST_TN_EQ_STR("0",       STR(format("~MB",   (scm_int_t)0)));
    TST_TN_EQ_STR("1",       STR(format("~MB",   (scm_int_t)1)));
    TST_TN_EQ_STR("1010",    STR(format("~MB",   (scm_int_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~MB",   (scm_int_t)100)));

    TST_TN_EQ_STR("0",       STR(format("~0MB",  (scm_int_t)0)));
    TST_TN_EQ_STR("1",       STR(format("~0MB",  (scm_int_t)1)));
    TST_TN_EQ_STR("1010",    STR(format("~0MB",  (scm_int_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~0MB",  (scm_int_t)100)));

    TST_TN_EQ_STR("00000",   STR(format("~05MB", (scm_int_t)0)));
    TST_TN_EQ_STR("00001",   STR(format("~05MB", (scm_int_t)1)));
    TST_TN_EQ_STR("01010",   STR(format("~05MB", (scm_int_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~05MB", (scm_int_t)100)));

    TST_TN_EQ_STR("    0",   STR(format("~5MB",  (scm_int_t)0)));
    TST_TN_EQ_STR("    1",   STR(format("~5MB",  (scm_int_t)1)));
    TST_TN_EQ_STR(" 1010",   STR(format("~5MB",  (scm_int_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~5MB",  (scm_int_t)100)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~MB", (scm_int_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~MB", (scm_int_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~MB", (scm_int_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~0MB", (scm_int_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~0MB", (scm_int_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~0MB", (scm_int_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~05MB", (scm_int_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~05MB", (scm_int_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~05MB", (scm_int_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~5MB", (scm_int_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~5MB", (scm_int_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~5MB", (scm_int_t)-1)));

    TST_TN_EQ_STR("                                                                                                                            101",
                  STR(format("~127MB", (scm_int_t)0x5)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101",
                  STR(format("~0127MB", (scm_int_t)0x5)));
}

TST_CASE("format ~QD")
{
    TST_TN_EQ_STR("-100", STR(format("~QD",   (int64_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~QD",   (int64_t)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~QD",   (int64_t)-1)));
    TST_TN_EQ_STR("0",    STR(format("~QD",   (int64_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~QD",   (int64_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~QD",   (int64_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~QD",   (int64_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~0QD",  (int64_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~0QD",  (int64_t)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~0QD",  (int64_t)-1)));
    TST_TN_EQ_STR("0",    STR(format("~0QD",  (int64_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0QD",  (int64_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~0QD",  (int64_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~0QD",  (int64_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~03QD", (int64_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~03QD", (int64_t)-10)));
    TST_TN_EQ_STR("-01",  STR(format("~03QD", (int64_t)-1)));
    TST_TN_EQ_STR("000",  STR(format("~03QD", (int64_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03QD", (int64_t)1)));
    TST_TN_EQ_STR("010",  STR(format("~03QD", (int64_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~03QD", (int64_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~3QD",  (int64_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~3QD",  (int64_t)-10)));
    TST_TN_EQ_STR(" -1",  STR(format("~3QD",  (int64_t)-1)));
    TST_TN_EQ_STR("  0",  STR(format("~3QD",  (int64_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3QD",  (int64_t)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3QD",  (int64_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~3QD",  (int64_t)100)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127QD", (int64_t)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127QD", (int64_t)123)));
}

TST_CASE("format ~QU")
{
    TST_TN_EQ_STR("0",    STR(format("~QU",   (int64_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~QU",   (int64_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~QU",   (int64_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~QU",   (int64_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0QU",  (int64_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0QU",  (int64_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~0QU",  (int64_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~0QU",  (int64_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03QU", (int64_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03QU", (int64_t)1)));
    TST_TN_EQ_STR("010",  STR(format("~03QU", (int64_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~03QU", (int64_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3QU",  (int64_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3QU",  (int64_t)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3QU",  (int64_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~3QU",  (int64_t)100)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~QU",   (int64_t)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~QU",   (int64_t)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~QU",   (int64_t)-1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~0QU",  (int64_t)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~0QU",  (int64_t)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~0QU",  (int64_t)-1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~03QU", (int64_t)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~03QU", (int64_t)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~03QU", (int64_t)-1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~3QU",  (int64_t)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~3QU",  (int64_t)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~3QU",  (int64_t)-1)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127QU", (int64_t)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127QU", (int64_t)123)));
}

TST_CASE("format ~QX")
{
    TST_TN_EQ_STR("0",    STR(format("~QX",   (int64_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~QX",   (int64_t)1)));
    TST_TN_EQ_STR("a",    STR(format("~QX",   (int64_t)10)));
    TST_TN_EQ_STR("64",   STR(format("~QX",   (int64_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0QX",  (int64_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0QX",  (int64_t)1)));
    TST_TN_EQ_STR("a",    STR(format("~0QX",  (int64_t)10)));
    TST_TN_EQ_STR("64",   STR(format("~0QX",  (int64_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03QX", (int64_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03QX", (int64_t)1)));
    TST_TN_EQ_STR("00a",  STR(format("~03QX", (int64_t)10)));
    TST_TN_EQ_STR("064",  STR(format("~03QX", (int64_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3QX",  (int64_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3QX",  (int64_t)1)));
    TST_TN_EQ_STR("  a",  STR(format("~3QX",  (int64_t)10)));
    TST_TN_EQ_STR(" 64",  STR(format("~3QX",  (int64_t)100)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~QX",   (int64_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~QX",   (int64_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~QX",   (int64_t)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~0QX",  (int64_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~0QX",  (int64_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~0QX",  (int64_t)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~03QX", (int64_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~03QX", (int64_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~03QX", (int64_t)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~3QX",  (int64_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~3QX",  (int64_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~3QX",  (int64_t)-1)));

    TST_TN_EQ_STR("                                                                                                                            1ac",
                           STR(format("~127QX", (int64_t)0x1ac)));
    TST_TN_EQ_STR("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001ac",
                           STR(format("~0127QX", (int64_t)0x1ac)));
}

TST_CASE("format ~QO")
{
    TST_TN_EQ_STR("0",    STR(format("~QO",   (int64_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~QO",   (int64_t)1)));
    TST_TN_EQ_STR("12",   STR(format("~QO",   (int64_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~QO",   (int64_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0QO",  (int64_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0QO",  (int64_t)1)));
    TST_TN_EQ_STR("12",   STR(format("~0QO",  (int64_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~0QO",  (int64_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03QO", (int64_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03QO", (int64_t)1)));
    TST_TN_EQ_STR("012",  STR(format("~03QO", (int64_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~03QO", (int64_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3QO",  (int64_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3QO",  (int64_t)1)));
    TST_TN_EQ_STR(" 12",  STR(format("~3QO",  (int64_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~3QO",  (int64_t)100)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~QO",   (int64_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~QO",   (int64_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~QO",   (int64_t)-1)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~0QO",  (int64_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~0QO",  (int64_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~0QO",  (int64_t)-1)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~03QO", (int64_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~03QO", (int64_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~03QO", (int64_t)-1)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~3QO",  (int64_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~3QO",  (int64_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~3QO",  (int64_t)-1)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127QO", (int64_t)0123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127QO", (int64_t)0123)));
}

TST_CASE("format ~QB")
{
    TST_TN_EQ_STR("0",       STR(format("~QB",   (int64_t)0)));
    TST_TN_EQ_STR("1",       STR(format("~QB",   (int64_t)1)));
    TST_TN_EQ_STR("1010",    STR(format("~QB",   (int64_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~QB",   (int64_t)100)));

    TST_TN_EQ_STR("0",       STR(format("~0QB",  (int64_t)0)));
    TST_TN_EQ_STR("1",       STR(format("~0QB",  (int64_t)1)));
    TST_TN_EQ_STR("1010",    STR(format("~0QB",  (int64_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~0QB",  (int64_t)100)));

    TST_TN_EQ_STR("00000",   STR(format("~05QB", (int64_t)0)));
    TST_TN_EQ_STR("00001",   STR(format("~05QB", (int64_t)1)));
    TST_TN_EQ_STR("01010",   STR(format("~05QB", (int64_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~05QB", (int64_t)100)));

    TST_TN_EQ_STR("    0",   STR(format("~5QB",  (int64_t)0)));
    TST_TN_EQ_STR("    1",   STR(format("~5QB",  (int64_t)1)));
    TST_TN_EQ_STR(" 1010",   STR(format("~5QB",  (int64_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~5QB",  (int64_t)100)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~QB", (int64_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~QB", (int64_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~QB", (int64_t)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~0QB", (int64_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~0QB", (int64_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~0QB", (int64_t)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~05QB", (int64_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~05QB", (int64_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~05QB", (int64_t)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~5QB", (int64_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~5QB", (int64_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~5QB", (int64_t)-1)));

    TST_TN_EQ_STR("                                                                                                                            101",
                  STR(format("~127QB", (int64_t)0x5)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101",
                  STR(format("~0127QB", (int64_t)0x5)));
}

TST_CASE("format ~LD")
{
    TST_TN_EQ_STR("-100", STR(format("~LD",   (long)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~LD",   (long)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~LD",   (long)-1)));
    TST_TN_EQ_STR("0",    STR(format("~LD",   (long)0)));
    TST_TN_EQ_STR("1",    STR(format("~LD",   (long)1)));
    TST_TN_EQ_STR("10",   STR(format("~LD",   (long)10)));
    TST_TN_EQ_STR("100",  STR(format("~LD",   (long)100)));

    TST_TN_EQ_STR("-100", STR(format("~0LD",  (long)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~0LD",  (long)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~0LD",  (long)-1)));
    TST_TN_EQ_STR("0",    STR(format("~0LD",  (long)0)));
    TST_TN_EQ_STR("1",    STR(format("~0LD",  (long)1)));
    TST_TN_EQ_STR("10",   STR(format("~0LD",  (long)10)));
    TST_TN_EQ_STR("100",  STR(format("~0LD",  (long)100)));

    TST_TN_EQ_STR("-100", STR(format("~03LD", (long)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~03LD", (long)-10)));
    TST_TN_EQ_STR("-01",  STR(format("~03LD", (long)-1)));
    TST_TN_EQ_STR("000",  STR(format("~03LD", (long)0)));
    TST_TN_EQ_STR("001",  STR(format("~03LD", (long)1)));
    TST_TN_EQ_STR("010",  STR(format("~03LD", (long)10)));
    TST_TN_EQ_STR("100",  STR(format("~03LD", (long)100)));

    TST_TN_EQ_STR("-100", STR(format("~3LD",  (long)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~3LD",  (long)-10)));
    TST_TN_EQ_STR(" -1",  STR(format("~3LD",  (long)-1)));
    TST_TN_EQ_STR("  0",  STR(format("~3LD",  (long)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3LD",  (long)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3LD",  (long)10)));
    TST_TN_EQ_STR("100",  STR(format("~3LD",  (long)100)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127LD", (long)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127LD", (long)123)));
}

TST_CASE("format ~LU")
{
    TST_TN_EQ_STR("0",    STR(format("~LU",   (long)0)));
    TST_TN_EQ_STR("1",    STR(format("~LU",   (long)1)));
    TST_TN_EQ_STR("10",   STR(format("~LU",   (long)10)));
    TST_TN_EQ_STR("100",  STR(format("~LU",   (long)100)));

    TST_TN_EQ_STR("0",    STR(format("~0LU",  (long)0)));
    TST_TN_EQ_STR("1",    STR(format("~0LU",  (long)1)));
    TST_TN_EQ_STR("10",   STR(format("~0LU",  (long)10)));
    TST_TN_EQ_STR("100",  STR(format("~0LU",  (long)100)));

    TST_TN_EQ_STR("000",  STR(format("~03LU", (long)0)));
    TST_TN_EQ_STR("001",  STR(format("~03LU", (long)1)));
    TST_TN_EQ_STR("010",  STR(format("~03LU", (long)10)));
    TST_TN_EQ_STR("100",  STR(format("~03LU", (long)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3LU",  (long)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3LU",  (long)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3LU",  (long)10)));
    TST_TN_EQ_STR("100",  STR(format("~3LU",  (long)100)));

#if L32
    TST_TN_EQ_STR("4294967196", STR(format("~LU",   (long)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~LU",   (long)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~LU",   (long)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~0LU",  (long)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~0LU",  (long)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~0LU",  (long)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~03LU", (long)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~03LU", (long)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~03LU", (long)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~3LU",  (long)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~3LU",  (long)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~3LU",  (long)-1)));
#elif L64
    TST_TN_EQ_STR("18446744073709551516", STR(format("~LU",   (long)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~LU",   (long)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~LU",   (long)-1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~0LU",  (long)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~0LU",  (long)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~0LU",  (long)-1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~03LU", (long)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~03LU", (long)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~03LU", (long)-1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~3LU",  (long)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~3LU",  (long)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~3LU",  (long)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127LU", (long)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127LU", (long)123)));
}

TST_CASE("format ~LX")
{
    TST_TN_EQ_STR("0",    STR(format("~LX",   (long)0)));
    TST_TN_EQ_STR("1",    STR(format("~LX",   (long)1)));
    TST_TN_EQ_STR("a",    STR(format("~LX",   (long)10)));
    TST_TN_EQ_STR("64",   STR(format("~LX",   (long)100)));

    TST_TN_EQ_STR("0",    STR(format("~0LX",  (long)0)));
    TST_TN_EQ_STR("1",    STR(format("~0LX",  (long)1)));
    TST_TN_EQ_STR("a",    STR(format("~0LX",  (long)10)));
    TST_TN_EQ_STR("64",   STR(format("~0LX",  (long)100)));

    TST_TN_EQ_STR("000",  STR(format("~03LX", (long)0)));
    TST_TN_EQ_STR("001",  STR(format("~03LX", (long)1)));
    TST_TN_EQ_STR("00a",  STR(format("~03LX", (long)10)));
    TST_TN_EQ_STR("064",  STR(format("~03LX", (long)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3LX",  (long)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3LX",  (long)1)));
    TST_TN_EQ_STR("  a",  STR(format("~3LX",  (long)10)));
    TST_TN_EQ_STR(" 64",  STR(format("~3LX",  (long)100)));

#if L32
    TST_TN_EQ_STR("ffffff9c", STR(format("~LX",   (long)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~LX",   (long)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~LX",   (long)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~0LX",  (long)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~0LX",  (long)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~0LX",  (long)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~03LX", (long)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~03LX", (long)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~03LX", (long)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~3LX",  (long)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~3LX",  (long)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~3LX",  (long)-1)));
#elif L64
    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~LX",   (long)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~LX",   (long)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~LX",   (long)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~0LX",  (long)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~0LX",  (long)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~0LX",  (long)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~03LX", (long)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~03LX", (long)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~03LX", (long)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~3LX",  (long)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~3LX",  (long)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~3LX",  (long)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            1ac",
                  STR(format("~127LX", (long)0x1ac)));
    TST_TN_EQ_STR("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001ac",
                  STR(format("~0127LX", (long)0x1ac)));
}

TST_CASE("format ~LO")
{
    TST_TN_EQ_STR("0",    STR(format("~LO",   (long)0)));
    TST_TN_EQ_STR("1",    STR(format("~LO",   (long)1)));
    TST_TN_EQ_STR("12",   STR(format("~LO",   (long)10)));
    TST_TN_EQ_STR("144",  STR(format("~LO",   (long)100)));

    TST_TN_EQ_STR("0",    STR(format("~0LO",  (long)0)));
    TST_TN_EQ_STR("1",    STR(format("~0LO",  (long)1)));
    TST_TN_EQ_STR("12",   STR(format("~0LO",  (long)10)));
    TST_TN_EQ_STR("144",  STR(format("~0LO",  (long)100)));

    TST_TN_EQ_STR("000",  STR(format("~03LO", (long)0)));
    TST_TN_EQ_STR("001",  STR(format("~03LO", (long)1)));
    TST_TN_EQ_STR("012",  STR(format("~03LO", (long)10)));
    TST_TN_EQ_STR("144",  STR(format("~03LO", (long)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3LO",  (long)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3LO",  (long)1)));
    TST_TN_EQ_STR(" 12",  STR(format("~3LO",  (long)10)));
    TST_TN_EQ_STR("144",  STR(format("~3LO",  (long)100)));

#if L32
    TST_TN_EQ_STR("37777777634", STR(format("~LO",   (long)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~LO",   (long)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~LO",   (long)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~0LO",  (long)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~0LO",  (long)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~0LO",  (long)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~03LO", (long)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~03LO", (long)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~03LO", (long)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~3LO",  (long)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~3LO",  (long)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~3LO",  (long)-1)));
#elif L64
    TST_TN_EQ_STR("1777777777777777777634", STR(format("~LO",   (long)-100)));
    TST_TN_EQ_STR("1777777777777777777766", STR(format("~LO",   (long)-10)));
    TST_TN_EQ_STR("1777777777777777777777", STR(format("~LO",   (long)-1)));

    TST_TN_EQ_STR("1777777777777777777634", STR(format("~0LO",  (long)-100)));
    TST_TN_EQ_STR("1777777777777777777766", STR(format("~0LO",  (long)-10)));
    TST_TN_EQ_STR("1777777777777777777777", STR(format("~0LO",  (long)-1)));

    TST_TN_EQ_STR("1777777777777777777634", STR(format("~03LO", (long)-100)));
    TST_TN_EQ_STR("1777777777777777777766", STR(format("~03LO", (long)-10)));
    TST_TN_EQ_STR("1777777777777777777777", STR(format("~03LO", (long)-1)));

    TST_TN_EQ_STR("1777777777777777777634", STR(format("~3LO",  (long)-100)));
    TST_TN_EQ_STR("1777777777777777777766", STR(format("~3LO",  (long)-10)));
    TST_TN_EQ_STR("1777777777777777777777", STR(format("~3LO",  (long)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127LO", (long)0123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127LO", (long)0123)));
}

TST_CASE("format ~LB")
{
    TST_TN_EQ_STR("0",       STR(format("~LB",   (long)0)));
    TST_TN_EQ_STR("1",       STR(format("~LB",   (long)1)));
    TST_TN_EQ_STR("1010",    STR(format("~LB",   (long)10)));
    TST_TN_EQ_STR("1100100", STR(format("~LB",   (long)100)));

    TST_TN_EQ_STR("0",       STR(format("~0LB",  (long)0)));
    TST_TN_EQ_STR("1",       STR(format("~0LB",  (long)1)));
    TST_TN_EQ_STR("1010",    STR(format("~0LB",  (long)10)));
    TST_TN_EQ_STR("1100100", STR(format("~0LB",  (long)100)));

    TST_TN_EQ_STR("00000",   STR(format("~05LB", (long)0)));
    TST_TN_EQ_STR("00001",   STR(format("~05LB", (long)1)));
    TST_TN_EQ_STR("01010",   STR(format("~05LB", (long)10)));
    TST_TN_EQ_STR("1100100", STR(format("~05LB", (long)100)));

    TST_TN_EQ_STR("    0",   STR(format("~5LB",  (long)0)));
    TST_TN_EQ_STR("    1",   STR(format("~5LB",  (long)1)));
    TST_TN_EQ_STR(" 1010",   STR(format("~5LB",  (long)10)));
    TST_TN_EQ_STR("1100100", STR(format("~5LB",  (long)100)));

#if L32
    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~LB", (long)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~LB", (long)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~LB", (long)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~0LB", (long)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~0LB", (long)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~0LB", (long)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~05LB", (long)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~05LB", (long)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~05LB", (long)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~5LB", (long)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~5LB", (long)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~5LB", (long)-1)));
#elif L64
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~LB", (long)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~LB", (long)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~LB", (long)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~0LB", (long)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~0LB", (long)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~0LB", (long)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~05LB", (long)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~05LB", (long)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~05LB", (long)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~5LB", (long)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~5LB", (long)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~5LB", (long)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            101",
                  STR(format("~127LB", (long)0x5)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101",
                  STR(format("~0127LB", (long)0x5)));
}

TST_CASE("format ~JD")
{
    TST_TN_EQ_STR("-100", STR(format("~JD",   (intmax_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~JD",   (intmax_t)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~JD",   (intmax_t)-1)));
    TST_TN_EQ_STR("0",    STR(format("~JD",   (intmax_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~JD",   (intmax_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~JD",   (intmax_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~JD",   (intmax_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~0JD",  (intmax_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~0JD",  (intmax_t)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~0JD",  (intmax_t)-1)));
    TST_TN_EQ_STR("0",    STR(format("~0JD",  (intmax_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0JD",  (intmax_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~0JD",  (intmax_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~0JD",  (intmax_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~03JD", (intmax_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~03JD", (intmax_t)-10)));
    TST_TN_EQ_STR("-01",  STR(format("~03JD", (intmax_t)-1)));
    TST_TN_EQ_STR("000",  STR(format("~03JD", (intmax_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03JD", (intmax_t)1)));
    TST_TN_EQ_STR("010",  STR(format("~03JD", (intmax_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~03JD", (intmax_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~3JD",  (intmax_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~3JD",  (intmax_t)-10)));
    TST_TN_EQ_STR(" -1",  STR(format("~3JD",  (intmax_t)-1)));
    TST_TN_EQ_STR("  0",  STR(format("~3JD",  (intmax_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3JD",  (intmax_t)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3JD",  (intmax_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~3JD",  (intmax_t)100)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127JD", (intmax_t)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127JD", (intmax_t)123)));
}

TST_CASE("format ~JU")
{
    TST_TN_EQ_STR("0",    STR(format("~JU",   (intmax_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~JU",   (intmax_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~JU",   (intmax_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~JU",   (intmax_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0JU",  (intmax_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0JU",  (intmax_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~0JU",  (intmax_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~0JU",  (intmax_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03JU", (intmax_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03JU", (intmax_t)1)));
    TST_TN_EQ_STR("010",  STR(format("~03JU", (intmax_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~03JU", (intmax_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3JU",  (intmax_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3JU",  (intmax_t)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3JU",  (intmax_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~3JU",  (intmax_t)100)));

#if IMAX32
    TST_TN_EQ_STR("4294967196", STR(format("~JU",   (intmax_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~JU",   (intmax_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~JU",   (intmax_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~0JU",  (intmax_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~0JU",  (intmax_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~0JU",  (intmax_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~03JU", (intmax_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~03JU", (intmax_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~03JU", (intmax_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~3JU",  (intmax_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~3JU",  (intmax_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~3JU",  (intmax_t)-1)));
#elif IMAX64
    TST_TN_EQ_STR("18446744073709551516", STR(format("~JU",  (intmax_t)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~JU",  (intmax_t)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~JU",  (intmax_t)-1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~0JU", (intmax_t)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~0JU", (intmax_t)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~0JU", (intmax_t)-1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~03JU",(intmax_t)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~03JU",(intmax_t)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~03JU",(intmax_t)-1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~3JU", (intmax_t)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~3JU", (intmax_t)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~3JU", (intmax_t)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127JU", (intmax_t)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127JU", (intmax_t)123)));
}

TST_CASE("format ~JX")
{
    TST_TN_EQ_STR("0",    STR(format("~JX",   (intmax_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~JX",   (intmax_t)1)));
    TST_TN_EQ_STR("a",    STR(format("~JX",   (intmax_t)10)));
    TST_TN_EQ_STR("64",   STR(format("~JX",   (intmax_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0JX",  (intmax_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0JX",  (intmax_t)1)));
    TST_TN_EQ_STR("a",    STR(format("~0JX",  (intmax_t)10)));
    TST_TN_EQ_STR("64",   STR(format("~0JX",  (intmax_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03JX", (intmax_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03JX", (intmax_t)1)));
    TST_TN_EQ_STR("00a",  STR(format("~03JX", (intmax_t)10)));
    TST_TN_EQ_STR("064",  STR(format("~03JX", (intmax_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3JX",  (intmax_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3JX",  (intmax_t)1)));
    TST_TN_EQ_STR("  a",  STR(format("~3JX",  (intmax_t)10)));
    TST_TN_EQ_STR(" 64",  STR(format("~3JX",  (intmax_t)100)));

#if IMAX32
    TST_TN_EQ_STR("ffffff9c", STR(format("~JX",   (intmax_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~JX",   (intmax_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~JX",   (intmax_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~0JX",  (intmax_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~0JX",  (intmax_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~0JX",  (intmax_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~03JX", (intmax_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~03JX", (intmax_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~03JX", (intmax_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~3JX",  (intmax_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~3JX",  (intmax_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~3JX",  (intmax_t)-1)));
#elif IMAX64
    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~JX",   (intmax_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~JX",   (intmax_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~JX",   (intmax_t)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~0JX",  (intmax_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~0JX",  (intmax_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~0JX",  (intmax_t)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~03JX", (intmax_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~03JX", (intmax_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~03JX", (intmax_t)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~3JX",  (intmax_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~3JX",  (intmax_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~3JX",  (intmax_t)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            1ac",
                  STR(format("~127JX", (intmax_t)0x1ac)));
    TST_TN_EQ_STR("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001ac",
                  STR(format("~0127JX", (intmax_t)0x1ac)));
}

TST_CASE("format ~JO")
{
    TST_TN_EQ_STR("0",    STR(format("~JO",   (intmax_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~JO",   (intmax_t)1)));
    TST_TN_EQ_STR("12",   STR(format("~JO",   (intmax_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~JO",   (intmax_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0JO",  (intmax_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0JO",  (intmax_t)1)));
    TST_TN_EQ_STR("12",   STR(format("~0JO",  (intmax_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~0JO",  (intmax_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03JO", (intmax_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03JO", (intmax_t)1)));
    TST_TN_EQ_STR("012",  STR(format("~03JO", (intmax_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~03JO", (intmax_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3JO",  (intmax_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3JO",  (intmax_t)1)));
    TST_TN_EQ_STR(" 12",  STR(format("~3JO",  (intmax_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~3JO",  (intmax_t)100)));

#if IMAX32
    TST_TN_EQ_STR("37777777634", STR(format("~JO",   (intmax_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~JO",   (intmax_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~JO",   (intmax_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~0JO",  (intmax_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~0JO",  (intmax_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~0JO",  (intmax_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~03JO", (intmax_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~03JO", (intmax_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~03JO", (intmax_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~3JO",  (intmax_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~3JO",  (intmax_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~3JO",  (intmax_t)-1)));
#elif IMAX64
    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~JO",   (intmax_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~JO",   (intmax_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~JO",   (intmax_t)-1)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~0JO",  (intmax_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~0JO",  (intmax_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~0JO",  (intmax_t)-1)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~03JO", (intmax_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~03JO", (intmax_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~03JO", (intmax_t)-1)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~3JO",  (intmax_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~3JO",  (intmax_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~3JO",  (intmax_t)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127JO", (intmax_t)0123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127JO", (intmax_t)0123)));
}

TST_CASE("format ~JB")
{
    TST_TN_EQ_STR("0",       STR(format("~JB",   (intmax_t)0)));
    TST_TN_EQ_STR("1",       STR(format("~JB",   (intmax_t)1)));
    TST_TN_EQ_STR("1010",    STR(format("~JB",   (intmax_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~JB",   (intmax_t)100)));

    TST_TN_EQ_STR("0",       STR(format("~0JB",  (intmax_t)0)));
    TST_TN_EQ_STR("1",       STR(format("~0JB",  (intmax_t)1)));
    TST_TN_EQ_STR("1010",    STR(format("~0JB",  (intmax_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~0JB",  (intmax_t)100)));

    TST_TN_EQ_STR("00000",   STR(format("~05JB", (intmax_t)0)));
    TST_TN_EQ_STR("00001",   STR(format("~05JB", (intmax_t)1)));
    TST_TN_EQ_STR("01010",   STR(format("~05JB", (intmax_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~05JB", (intmax_t)100)));

    TST_TN_EQ_STR("    0",   STR(format("~5JB",  (intmax_t)0)));
    TST_TN_EQ_STR("    1",   STR(format("~5JB",  (intmax_t)1)));
    TST_TN_EQ_STR(" 1010",   STR(format("~5JB",  (intmax_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~5JB",  (intmax_t)100)));

#if IMAX32
    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~JB", (intmax_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~JB", (intmax_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~JB", (intmax_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~0JB", (intmax_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~0JB", (intmax_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~0JB", (intmax_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~05JB", (intmax_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~05JB", (intmax_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~05JB", (intmax_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~5JB", (intmax_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~5JB", (intmax_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~5JB", (intmax_t)-1)));
#elif IMAX64
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~JB", (intmax_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~JB", (intmax_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~JB", (intmax_t)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~0JB", (intmax_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~0JB", (intmax_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~0JB", (intmax_t)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~05JB", (intmax_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~05JB", (intmax_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~05JB", (intmax_t)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~5JB", (intmax_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~5JB", (intmax_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~5JB", (intmax_t)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            101",
                  STR(format("~127JB", (intmax_t)0x5)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101",
                  STR(format("~0127JB", (intmax_t)0x5)));
}

TST_CASE("format ~TD")
{
    TST_TN_EQ_STR("-100", STR(format("~TD",   (ptrdiff_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~TD",   (ptrdiff_t)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~TD",   (ptrdiff_t)-1)));
    TST_TN_EQ_STR("0",    STR(format("~TD",   (ptrdiff_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~TD",   (ptrdiff_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~TD",   (ptrdiff_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~TD",   (ptrdiff_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~0TD",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~0TD",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~0TD",  (ptrdiff_t)-1)));
    TST_TN_EQ_STR("0",    STR(format("~0TD",  (ptrdiff_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0TD",  (ptrdiff_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~0TD",  (ptrdiff_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~0TD",  (ptrdiff_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~03TD", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~03TD", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("-01",  STR(format("~03TD", (ptrdiff_t)-1)));
    TST_TN_EQ_STR("000",  STR(format("~03TD", (ptrdiff_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03TD", (ptrdiff_t)1)));
    TST_TN_EQ_STR("010",  STR(format("~03TD", (ptrdiff_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~03TD", (ptrdiff_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~3TD",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~3TD",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR(" -1",  STR(format("~3TD",  (ptrdiff_t)-1)));
    TST_TN_EQ_STR("  0",  STR(format("~3TD",  (ptrdiff_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3TD",  (ptrdiff_t)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3TD",  (ptrdiff_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~3TD",  (ptrdiff_t)100)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127TD", (ptrdiff_t)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127TD", (ptrdiff_t)123)));
}

TST_CASE("format ~TU")
{
    TST_TN_EQ_STR("0",    STR(format("~TU",   (ptrdiff_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~TU",   (ptrdiff_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~TU",   (ptrdiff_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~TU",   (ptrdiff_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0TU",  (ptrdiff_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0TU",  (ptrdiff_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~0TU",  (ptrdiff_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~0TU",  (ptrdiff_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03TU", (ptrdiff_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03TU", (ptrdiff_t)1)));
    TST_TN_EQ_STR("010",  STR(format("~03TU", (ptrdiff_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~03TU", (ptrdiff_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3TU",  (ptrdiff_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3TU",  (ptrdiff_t)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3TU",  (ptrdiff_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~3TU",  (ptrdiff_t)100)));

#if PDIFF32
    TST_TN_EQ_STR("4294967196", STR(format("~TU",   (ptrdiff_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~TU",   (ptrdiff_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~TU",   (ptrdiff_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~0TU",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~0TU",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~0TU",  (ptrdiff_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~03TU", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~03TU", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~03TU", (ptrdiff_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~3TU",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~3TU",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~3TU",  (ptrdiff_t)-1)));
#elif PDIFF64
    TST_TN_EQ_STR("18446744073709551516",
                  STR(format("~TU",   (ptrdiff_t)-100)));
    TST_TN_EQ_STR("18446744073709551606",
                  STR(format("~TU",   (ptrdiff_t)-10)));
    TST_TN_EQ_STR("18446744073709551615",
                  STR(format("~TU",   (ptrdiff_t)-1)));

    TST_TN_EQ_STR("18446744073709551516",
                  STR(format("~0TU",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("18446744073709551606",
                  STR(format("~0TU",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("18446744073709551615",
                  STR(format("~0TU",  (ptrdiff_t)-1)));

    TST_TN_EQ_STR("18446744073709551516",
                  STR(format("~03TU", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("18446744073709551606",
                  STR(format("~03TU", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("18446744073709551615",
                  STR(format("~03TU", (ptrdiff_t)-1)));

    TST_TN_EQ_STR("18446744073709551516",
                  STR(format("~3TU",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("18446744073709551606",
                  STR(format("~3TU",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("18446744073709551615",
                  STR(format("~3TU",  (ptrdiff_t)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127TU", (ptrdiff_t)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127TU", (ptrdiff_t)123)));
}

TST_CASE("format ~TX")
{
    TST_TN_EQ_STR("0",    STR(format("~TX",   (ptrdiff_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~TX",   (ptrdiff_t)1)));
    TST_TN_EQ_STR("a",    STR(format("~TX",   (ptrdiff_t)10)));
    TST_TN_EQ_STR("64",   STR(format("~TX",   (ptrdiff_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0TX",  (ptrdiff_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0TX",  (ptrdiff_t)1)));
    TST_TN_EQ_STR("a",    STR(format("~0TX",  (ptrdiff_t)10)));
    TST_TN_EQ_STR("64",   STR(format("~0TX",  (ptrdiff_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03TX", (ptrdiff_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03TX", (ptrdiff_t)1)));
    TST_TN_EQ_STR("00a",  STR(format("~03TX", (ptrdiff_t)10)));
    TST_TN_EQ_STR("064",  STR(format("~03TX", (ptrdiff_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3TX",  (ptrdiff_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3TX",  (ptrdiff_t)1)));
    TST_TN_EQ_STR("  a",  STR(format("~3TX",  (ptrdiff_t)10)));
    TST_TN_EQ_STR(" 64",  STR(format("~3TX",  (ptrdiff_t)100)));

#if PDIFF32
    TST_TN_EQ_STR("ffffff9c", STR(format("~TX",   (ptrdiff_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~TX",   (ptrdiff_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~TX",   (ptrdiff_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~0TX",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~0TX",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~0TX",  (ptrdiff_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~03TX", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~03TX", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~03TX", (ptrdiff_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~3TX",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~3TX",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~3TX",  (ptrdiff_t)-1)));
#elif PDIFF64
    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~TX",   (ptrdiff_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~TX",   (ptrdiff_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~TX",   (ptrdiff_t)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~0TX",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~0TX",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~0TX",  (ptrdiff_t)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~03TX", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~03TX", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~03TX", (ptrdiff_t)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~3TX",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~3TX",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~3TX",  (ptrdiff_t)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            1ac",
                  STR(format("~127TX", (ptrdiff_t)0x1ac)));
    TST_TN_EQ_STR("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001ac",
                  STR(format("~0127TX", (ptrdiff_t)0x1ac)));
}

TST_CASE("format ~TO")
{
    TST_TN_EQ_STR("0",    STR(format("~TO",   (ptrdiff_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~TO",   (ptrdiff_t)1)));
    TST_TN_EQ_STR("12",   STR(format("~TO",   (ptrdiff_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~TO",   (ptrdiff_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0TO",  (ptrdiff_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0TO",  (ptrdiff_t)1)));
    TST_TN_EQ_STR("12",   STR(format("~0TO",  (ptrdiff_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~0TO",  (ptrdiff_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03TO", (ptrdiff_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03TO", (ptrdiff_t)1)));
    TST_TN_EQ_STR("012",  STR(format("~03TO", (ptrdiff_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~03TO", (ptrdiff_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3TO",  (ptrdiff_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3TO",  (ptrdiff_t)1)));
    TST_TN_EQ_STR(" 12",  STR(format("~3TO",  (ptrdiff_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~3TO",  (ptrdiff_t)100)));

#if PDIFF32
    TST_TN_EQ_STR("37777777634", STR(format("~TO",   (ptrdiff_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~TO",   (ptrdiff_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~TO",   (ptrdiff_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~0TO",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~0TO",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~0TO",  (ptrdiff_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~03TO", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~03TO", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~03TO", (ptrdiff_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~3TO",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~3TO",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~3TO",  (ptrdiff_t)-1)));
#elif PDIFF64
    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~TO",   (ptrdiff_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~TO",   (ptrdiff_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~TO",   (ptrdiff_t)-1)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~0TO",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~0TO",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~0TO",  (ptrdiff_t)-1)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~03TO", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~03TO", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~03TO", (ptrdiff_t)-1)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~3TO",  (ptrdiff_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~3TO",  (ptrdiff_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~3TO",  (ptrdiff_t)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127TO", (ptrdiff_t)0123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127TO", (ptrdiff_t)0123)));
}

TST_CASE("format ~TB")
{
    TST_TN_EQ_STR("0",       STR(format("~TB",   (ptrdiff_t)0)));
    TST_TN_EQ_STR("1",       STR(format("~TB",   (ptrdiff_t)1)));
    TST_TN_EQ_STR("1010",    STR(format("~TB",   (ptrdiff_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~TB",   (ptrdiff_t)100)));

    TST_TN_EQ_STR("0",       STR(format("~0TB",  (ptrdiff_t)0)));
    TST_TN_EQ_STR("1",       STR(format("~0TB",  (ptrdiff_t)1)));
    TST_TN_EQ_STR("1010",    STR(format("~0TB",  (ptrdiff_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~0TB",  (ptrdiff_t)100)));

    TST_TN_EQ_STR("00000",   STR(format("~05TB", (ptrdiff_t)0)));
    TST_TN_EQ_STR("00001",   STR(format("~05TB", (ptrdiff_t)1)));
    TST_TN_EQ_STR("01010",   STR(format("~05TB", (ptrdiff_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~05TB", (ptrdiff_t)100)));

    TST_TN_EQ_STR("    0",   STR(format("~5TB",  (ptrdiff_t)0)));
    TST_TN_EQ_STR("    1",   STR(format("~5TB",  (ptrdiff_t)1)));
    TST_TN_EQ_STR(" 1010",   STR(format("~5TB",  (ptrdiff_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~5TB",  (ptrdiff_t)100)));

#if PDIFF32
    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~TB", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~TB", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~TB", (ptrdiff_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~0TB", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~0TB", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~0TB", (ptrdiff_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~05TB", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~05TB", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~05TB", (ptrdiff_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~5TB", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~5TB", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~5TB", (ptrdiff_t)-1)));
#elif PDIFF64
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~TB", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~TB", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~TB", (ptrdiff_t)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~0TB", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~0TB", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~0TB", (ptrdiff_t)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~05TB", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~05TB", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~05TB", (ptrdiff_t)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~5TB", (ptrdiff_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~5TB", (ptrdiff_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~5TB", (ptrdiff_t)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            101",
                  STR(format("~127TB", (ptrdiff_t)0x5)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101",
                  STR(format("~0127TB", (ptrdiff_t)0x5)));
}

TST_CASE("format ~ZD")
{
    TST_TN_EQ_STR("-100", STR(format("~ZD",   (size_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~ZD",   (size_t)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~ZD",   (size_t)-1)));
    TST_TN_EQ_STR("0",    STR(format("~ZD",   (size_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~ZD",   (size_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~ZD",   (size_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~ZD",   (size_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~0ZD",  (size_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~0ZD",  (size_t)-10)));
    TST_TN_EQ_STR("-1",   STR(format("~0ZD",  (size_t)-1)));
    TST_TN_EQ_STR("0",    STR(format("~0ZD",  (size_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0ZD",  (size_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~0ZD",  (size_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~0ZD",  (size_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~03ZD", (size_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~03ZD", (size_t)-10)));
    TST_TN_EQ_STR("-01",  STR(format("~03ZD", (size_t)-1)));
    TST_TN_EQ_STR("000",  STR(format("~03ZD", (size_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03ZD", (size_t)1)));
    TST_TN_EQ_STR("010",  STR(format("~03ZD", (size_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~03ZD", (size_t)100)));

    TST_TN_EQ_STR("-100", STR(format("~3ZD",  (size_t)-100)));
    TST_TN_EQ_STR("-10",  STR(format("~3ZD",  (size_t)-10)));
    TST_TN_EQ_STR(" -1",  STR(format("~3ZD",  (size_t)-1)));
    TST_TN_EQ_STR("  0",  STR(format("~3ZD",  (size_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3ZD",  (size_t)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3ZD",  (size_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~3ZD",  (size_t)100)));

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127ZD", (size_t)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127ZD", (size_t)123)));
}

TST_CASE("format ~ZU")
{
    TST_TN_EQ_STR("0",    STR(format("~ZU",   (size_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~ZU",   (size_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~ZU",   (size_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~ZU",   (size_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0ZU",  (size_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0ZU",  (size_t)1)));
    TST_TN_EQ_STR("10",   STR(format("~0ZU",  (size_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~0ZU",  (size_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03ZU", (size_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03ZU", (size_t)1)));
    TST_TN_EQ_STR("010",  STR(format("~03ZU", (size_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~03ZU", (size_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3ZU",  (size_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3ZU",  (size_t)1)));
    TST_TN_EQ_STR(" 10",  STR(format("~3ZU",  (size_t)10)));
    TST_TN_EQ_STR("100",  STR(format("~3ZU",  (size_t)100)));

#if P32
    TST_TN_EQ_STR("4294967196", STR(format("~ZU",   (size_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~ZU",   (size_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~ZU",   (size_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~0ZU",  (size_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~0ZU",  (size_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~0ZU",  (size_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~03ZU", (size_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~03ZU", (size_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~03ZU", (size_t)-1)));

    TST_TN_EQ_STR("4294967196", STR(format("~3ZU",  (size_t)-100)));
    TST_TN_EQ_STR("4294967286", STR(format("~3ZU",  (size_t)-10)));
    TST_TN_EQ_STR("4294967295", STR(format("~3ZU",  (size_t)-1)));
#elif P64
    TST_TN_EQ_STR("18446744073709551516", STR(format("~ZU",   (size_t)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~ZU",   (size_t)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~ZU",   (size_t)-1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~0ZU",  (size_t)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~0ZU",  (size_t)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~0ZU",  (size_t)-1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~03ZU", (size_t)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~03ZU", (size_t)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~03ZU", (size_t)-1)));

    TST_TN_EQ_STR("18446744073709551516", STR(format("~3ZU",  (size_t)-100)));
    TST_TN_EQ_STR("18446744073709551606", STR(format("~3ZU",  (size_t)-10)));
    TST_TN_EQ_STR("18446744073709551615", STR(format("~3ZU",  (size_t)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127ZU", (size_t)123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127ZU", (size_t)123)));
}

TST_CASE("format ~ZX")
{
    TST_TN_EQ_STR("0",    STR(format("~ZX",   (size_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~ZX",   (size_t)1)));
    TST_TN_EQ_STR("a",    STR(format("~ZX",   (size_t)10)));
    TST_TN_EQ_STR("64",   STR(format("~ZX",   (size_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0ZX",  (size_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0ZX",  (size_t)1)));
    TST_TN_EQ_STR("a",    STR(format("~0ZX",  (size_t)10)));
    TST_TN_EQ_STR("64",   STR(format("~0ZX",  (size_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03ZX", (size_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03ZX", (size_t)1)));
    TST_TN_EQ_STR("00a",  STR(format("~03ZX", (size_t)10)));
    TST_TN_EQ_STR("064",  STR(format("~03ZX", (size_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3ZX",  (size_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3ZX",  (size_t)1)));
    TST_TN_EQ_STR("  a",  STR(format("~3ZX",  (size_t)10)));
    TST_TN_EQ_STR(" 64",  STR(format("~3ZX",  (size_t)100)));

#if P32
    TST_TN_EQ_STR("ffffffff", STR(format("~ZX",   ~(size_t)0)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~ZX",   (size_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~ZX",   (size_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~ZX",   (size_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~0ZX",  (size_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~0ZX",  (size_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~0ZX",  (size_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~03ZX", (size_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~03ZX", (size_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~03ZX", (size_t)-1)));

    TST_TN_EQ_STR("ffffff9c", STR(format("~3ZX",  (size_t)-100)));
    TST_TN_EQ_STR("fffffff6", STR(format("~3ZX",  (size_t)-10)));
    TST_TN_EQ_STR("ffffffff", STR(format("~3ZX",  (size_t)-1)));
#elif P64
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~ZX",   ~(size_t)0)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~ZX",   (size_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~ZX",   (size_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~ZX",   (size_t)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~0ZX",  (size_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~0ZX",  (size_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~0ZX",  (size_t)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~03ZX", (size_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~03ZX", (size_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~03ZX", (size_t)-1)));

    TST_TN_EQ_STR("ffffffffffffff9c", STR(format("~3ZX",  (size_t)-100)));
    TST_TN_EQ_STR("fffffffffffffff6", STR(format("~3ZX",  (size_t)-10)));
    TST_TN_EQ_STR("ffffffffffffffff", STR(format("~3ZX",  (size_t)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            1ac",
                  STR(format("~127ZX", (size_t)0x1ac)));
    TST_TN_EQ_STR("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001ac",
                  STR(format("~0127ZX", (size_t)0x1ac)));
}

TST_CASE("format ~ZO")
{
    TST_TN_EQ_STR("0",    STR(format("~ZO",   (size_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~ZO",   (size_t)1)));
    TST_TN_EQ_STR("12",   STR(format("~ZO",   (size_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~ZO",   (size_t)100)));

    TST_TN_EQ_STR("0",    STR(format("~0ZO",  (size_t)0)));
    TST_TN_EQ_STR("1",    STR(format("~0ZO",  (size_t)1)));
    TST_TN_EQ_STR("12",   STR(format("~0ZO",  (size_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~0ZO",  (size_t)100)));

    TST_TN_EQ_STR("000",  STR(format("~03ZO", (size_t)0)));
    TST_TN_EQ_STR("001",  STR(format("~03ZO", (size_t)1)));
    TST_TN_EQ_STR("012",  STR(format("~03ZO", (size_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~03ZO", (size_t)100)));

    TST_TN_EQ_STR("  0",  STR(format("~3ZO",  (size_t)0)));
    TST_TN_EQ_STR("  1",  STR(format("~3ZO",  (size_t)1)));
    TST_TN_EQ_STR(" 12",  STR(format("~3ZO",  (size_t)10)));
    TST_TN_EQ_STR("144",  STR(format("~3ZO",  (size_t)100)));

#if P32
    TST_TN_EQ_STR("37777777634", STR(format("~ZO",   (size_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~ZO",   (size_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~ZO",   (size_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~0ZO",  (size_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~0ZO",  (size_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~0ZO",  (size_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~03ZO", (size_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~03ZO", (size_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~03ZO", (size_t)-1)));

    TST_TN_EQ_STR("37777777634", STR(format("~3ZO",  (size_t)-100)));
    TST_TN_EQ_STR("37777777766", STR(format("~3ZO",  (size_t)-10)));
    TST_TN_EQ_STR("37777777777", STR(format("~3ZO",  (size_t)-1)));
#elif P64
    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~ZO",   (size_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~ZO",   (size_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~ZO",   (size_t)-1)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~0ZO",  (size_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~0ZO",  (size_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~0ZO",  (size_t)-1)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~03ZO", (size_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~03ZO", (size_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~03ZO", (size_t)-1)));

    TST_TN_EQ_STR("1777777777777777777634",
                  STR(format("~3ZO",  (size_t)-100)));
    TST_TN_EQ_STR("1777777777777777777766",
                  STR(format("~3ZO",  (size_t)-10)));
    TST_TN_EQ_STR("1777777777777777777777",
                  STR(format("~3ZO",  (size_t)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            123",
                  STR(format("~127ZO", (size_t)0123)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123",
                  STR(format("~0127ZO", (size_t)0123)));
}

TST_CASE("format ~ZB")
{
    TST_TN_EQ_STR("0",       STR(format("~ZB",   (size_t)0)));
    TST_TN_EQ_STR("1",       STR(format("~ZB",   (size_t)1)));
    TST_TN_EQ_STR("1010",    STR(format("~ZB",   (size_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~ZB",   (size_t)100)));

    TST_TN_EQ_STR("0",       STR(format("~0ZB",  (size_t)0)));
    TST_TN_EQ_STR("1",       STR(format("~0ZB",  (size_t)1)));
    TST_TN_EQ_STR("1010",    STR(format("~0ZB",  (size_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~0ZB",  (size_t)100)));

    TST_TN_EQ_STR("00000",   STR(format("~05ZB", (size_t)0)));
    TST_TN_EQ_STR("00001",   STR(format("~05ZB", (size_t)1)));
    TST_TN_EQ_STR("01010",   STR(format("~05ZB", (size_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~05ZB", (size_t)100)));

    TST_TN_EQ_STR("    0",   STR(format("~5ZB",  (size_t)0)));
    TST_TN_EQ_STR("    1",   STR(format("~5ZB",  (size_t)1)));
    TST_TN_EQ_STR(" 1010",   STR(format("~5ZB",  (size_t)10)));
    TST_TN_EQ_STR("1100100", STR(format("~5ZB",  (size_t)100)));

#if P32
    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~ZB", (size_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~ZB", (size_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~ZB", (size_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~0ZB", (size_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~0ZB", (size_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~0ZB", (size_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~05ZB", (size_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~05ZB", (size_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~05ZB", (size_t)-1)));

    TST_TN_EQ_STR("11111111111111111111111110011100",
                  STR(format("~5ZB", (size_t)-100)));
    TST_TN_EQ_STR("11111111111111111111111111110110",
                  STR(format("~5ZB", (size_t)-10)));
    TST_TN_EQ_STR("11111111111111111111111111111111",
                  STR(format("~5ZB", (size_t)-1)));
#elif P64
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~ZB", (size_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~ZB", (size_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~ZB", (size_t)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~0ZB", (size_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~0ZB", (size_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~0ZB", (size_t)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~05ZB", (size_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~05ZB", (size_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~05ZB", (size_t)-1)));

    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111110011100",
                  STR(format("~5ZB", (size_t)-100)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111110110",
                  STR(format("~5ZB", (size_t)-10)));
    TST_TN_EQ_STR("1111111111111111111111111111111111111111111111111111111111111111",
                  STR(format("~5ZB", (size_t)-1)));
#endif

    TST_TN_EQ_STR("                                                                                                                            101",
                  STR(format("~127ZB", (size_t)0x5)));
    TST_TN_EQ_STR("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101",
                  STR(format("~0127ZB", (size_t)0x5)));
}

TST_CASE("format ~c")
{
    TST_TN_EQ_STR("",   STR(format("~c", MAKE_CHAR('\0'))));
    TST_TN_EQ_STR("a",  STR(format("~c", MAKE_CHAR('a'))));
    TST_TN_EQ_STR("\"", STR(format("~c", MAKE_CHAR('\"'))));
    TST_TN_EQ_STR("\\", STR(format("~c", MAKE_CHAR('\\'))));
    TST_TN_EQ_STR("あ", STR(format("~c", MAKE_CHAR(0x3042))));
}

TST_CASE("format ~d")
{
    TST_TN_EQ_STR("-100", STR(format("~d",   MAKE_INT(-100))));
    TST_TN_EQ_STR("-10",  STR(format("~d",   MAKE_INT(-10))));
    TST_TN_EQ_STR("-1",   STR(format("~d",   MAKE_INT(-1))));
    TST_TN_EQ_STR("0",    STR(format("~d",   MAKE_INT(0))));
    TST_TN_EQ_STR("1",    STR(format("~d",   MAKE_INT(1))));
    TST_TN_EQ_STR("10",   STR(format("~d",   MAKE_INT(10))));
    TST_TN_EQ_STR("100",  STR(format("~d",   MAKE_INT(100))));

    TST_TN_EQ_STR("-100", STR(format("~0d",  MAKE_INT(-100))));
    TST_TN_EQ_STR("-10",  STR(format("~0d",  MAKE_INT(-10))));
    TST_TN_EQ_STR("-1",   STR(format("~0d",  MAKE_INT(-1))));
    TST_TN_EQ_STR("0",    STR(format("~0d",  MAKE_INT(0))));
    TST_TN_EQ_STR("1",    STR(format("~0d",  MAKE_INT(1))));
    TST_TN_EQ_STR("10",   STR(format("~0d",  MAKE_INT(10))));
    TST_TN_EQ_STR("100",  STR(format("~0d",  MAKE_INT(100))));

    TST_TN_EQ_STR("-100", STR(format("~03d", MAKE_INT(-100))));
    TST_TN_EQ_STR("-10",  STR(format("~03d", MAKE_INT(-10))));
    TST_TN_EQ_STR("-01",  STR(format("~03d", MAKE_INT(-1))));
    TST_TN_EQ_STR("000",  STR(format("~03d", MAKE_INT(0))));
    TST_TN_EQ_STR("001",  STR(format("~03d", MAKE_INT(1))));
    TST_TN_EQ_STR("010",  STR(format("~03d", MAKE_INT(10))));
    TST_TN_EQ_STR("100",  STR(format("~03d", MAKE_INT(100))));

    TST_TN_EQ_STR("-100", STR(format("~3d",  MAKE_INT(-100))));
    TST_TN_EQ_STR("-10",  STR(format("~3d",  MAKE_INT(-10))));
    TST_TN_EQ_STR(" -1",  STR(format("~3d",  MAKE_INT(-1))));
    TST_TN_EQ_STR("  0",  STR(format("~3d",  MAKE_INT(0))));
    TST_TN_EQ_STR("  1",  STR(format("~3d",  MAKE_INT(1))));
    TST_TN_EQ_STR(" 10",  STR(format("~3d",  MAKE_INT(10))));
    TST_TN_EQ_STR("100",  STR(format("~3d",  MAKE_INT(100))));
}

TST_CASE("format ~x")
{
    TST_TN_EQ_STR("-64",  STR(format("~x",   MAKE_INT(-100))));
    TST_TN_EQ_STR("-a",   STR(format("~x",   MAKE_INT(-10))));
    TST_TN_EQ_STR("-1",   STR(format("~x",   MAKE_INT(-1))));
    TST_TN_EQ_STR("0",    STR(format("~x",   MAKE_INT(0))));
    TST_TN_EQ_STR("1",    STR(format("~x",   MAKE_INT(1))));
    TST_TN_EQ_STR("a",    STR(format("~x",   MAKE_INT(10))));
    TST_TN_EQ_STR("64",   STR(format("~x",   MAKE_INT(100))));

    TST_TN_EQ_STR("-64",  STR(format("~0x",  MAKE_INT(-100))));
    TST_TN_EQ_STR("-a",   STR(format("~0x",  MAKE_INT(-10))));
    TST_TN_EQ_STR("-1",   STR(format("~0x",  MAKE_INT(-1))));
    TST_TN_EQ_STR("0",    STR(format("~0x",  MAKE_INT(0))));
    TST_TN_EQ_STR("1",    STR(format("~0x",  MAKE_INT(1))));
    TST_TN_EQ_STR("a",    STR(format("~0x",  MAKE_INT(10))));
    TST_TN_EQ_STR("64",   STR(format("~0x",  MAKE_INT(100))));

    TST_TN_EQ_STR("-64",  STR(format("~03x", MAKE_INT(-100))));
    TST_TN_EQ_STR("-0a",  STR(format("~03x", MAKE_INT(-10))));
    TST_TN_EQ_STR("-01",  STR(format("~03x", MAKE_INT(-1))));
    TST_TN_EQ_STR("000",  STR(format("~03x", MAKE_INT(0))));
    TST_TN_EQ_STR("001",  STR(format("~03x", MAKE_INT(1))));
    TST_TN_EQ_STR("00a",  STR(format("~03x", MAKE_INT(10))));
    TST_TN_EQ_STR("064",  STR(format("~03x", MAKE_INT(100))));

    TST_TN_EQ_STR("-64",  STR(format("~3x",  MAKE_INT(-100))));
    TST_TN_EQ_STR(" -a",  STR(format("~3x",  MAKE_INT(-10))));
    TST_TN_EQ_STR(" -1",  STR(format("~3x",  MAKE_INT(-1))));
    TST_TN_EQ_STR("  0",  STR(format("~3x",  MAKE_INT(0))));
    TST_TN_EQ_STR("  1",  STR(format("~3x",  MAKE_INT(1))));
    TST_TN_EQ_STR("  a",  STR(format("~3x",  MAKE_INT(10))));
    TST_TN_EQ_STR(" 64",  STR(format("~3x",  MAKE_INT(100))));
}

TST_CASE("format ~o")
{
    TST_TN_EQ_STR("-144", STR(format("~o",   MAKE_INT(-100))));
    TST_TN_EQ_STR("-12",  STR(format("~o",   MAKE_INT(-10))));
    TST_TN_EQ_STR("-1",   STR(format("~o",   MAKE_INT(-1))));
    TST_TN_EQ_STR("0",    STR(format("~o",   MAKE_INT(0))));
    TST_TN_EQ_STR("1",    STR(format("~o",   MAKE_INT(1))));
    TST_TN_EQ_STR("12",   STR(format("~o",   MAKE_INT(10))));
    TST_TN_EQ_STR("144",  STR(format("~o",   MAKE_INT(100))));

    TST_TN_EQ_STR("-144", STR(format("~0o",  MAKE_INT(-100))));
    TST_TN_EQ_STR("-12",  STR(format("~0o",  MAKE_INT(-10))));
    TST_TN_EQ_STR("-1",   STR(format("~0o",  MAKE_INT(-1))));
    TST_TN_EQ_STR("0",    STR(format("~0o",  MAKE_INT(0))));
    TST_TN_EQ_STR("1",    STR(format("~0o",  MAKE_INT(1))));
    TST_TN_EQ_STR("12",   STR(format("~0o",  MAKE_INT(10))));
    TST_TN_EQ_STR("144",  STR(format("~0o",  MAKE_INT(100))));

    TST_TN_EQ_STR("-144", STR(format("~03o", MAKE_INT(-100))));
    TST_TN_EQ_STR("-12",  STR(format("~03o", MAKE_INT(-10))));
    TST_TN_EQ_STR("-01",  STR(format("~03o", MAKE_INT(-1))));
    TST_TN_EQ_STR("000",  STR(format("~03o", MAKE_INT(0))));
    TST_TN_EQ_STR("001",  STR(format("~03o", MAKE_INT(1))));
    TST_TN_EQ_STR("012",  STR(format("~03o", MAKE_INT(10))));
    TST_TN_EQ_STR("144",  STR(format("~03o", MAKE_INT(100))));

    TST_TN_EQ_STR("-144", STR(format("~3o",  MAKE_INT(-100))));
    TST_TN_EQ_STR("-12",  STR(format("~3o",  MAKE_INT(-10))));
    TST_TN_EQ_STR(" -1",  STR(format("~3o",  MAKE_INT(-1))));
    TST_TN_EQ_STR("  0",  STR(format("~3o",  MAKE_INT(0))));
    TST_TN_EQ_STR("  1",  STR(format("~3o",  MAKE_INT(1))));
    TST_TN_EQ_STR(" 12",  STR(format("~3o",  MAKE_INT(10))));
    TST_TN_EQ_STR("144",  STR(format("~3o",  MAKE_INT(100))));
}

TST_CASE("format ~b")
{
    TST_TN_EQ_STR("-1100100", STR(format("~b",   MAKE_INT(-100))));
    TST_TN_EQ_STR("-1010",    STR(format("~b",   MAKE_INT(-10))));
    TST_TN_EQ_STR("-1",       STR(format("~b",   MAKE_INT(-1))));
    TST_TN_EQ_STR("0",        STR(format("~b",   MAKE_INT(0))));
    TST_TN_EQ_STR("1",        STR(format("~b",   MAKE_INT(1))));
    TST_TN_EQ_STR("1010",     STR(format("~b",   MAKE_INT(10))));
    TST_TN_EQ_STR("1100100",  STR(format("~b",   MAKE_INT(100))));

    TST_TN_EQ_STR("-1100100", STR(format("~0b",  MAKE_INT(-100))));
    TST_TN_EQ_STR("-1010",    STR(format("~0b",  MAKE_INT(-10))));
    TST_TN_EQ_STR("-1",       STR(format("~0b",  MAKE_INT(-1))));
    TST_TN_EQ_STR("0",        STR(format("~0b",  MAKE_INT(0))));
    TST_TN_EQ_STR("1",        STR(format("~0b",  MAKE_INT(1))));
    TST_TN_EQ_STR("1010",     STR(format("~0b",  MAKE_INT(10))));
    TST_TN_EQ_STR("1100100",  STR(format("~0b",  MAKE_INT(100))));

    TST_TN_EQ_STR("-1100100", STR(format("~05b", MAKE_INT(-100))));
    TST_TN_EQ_STR("-1010",    STR(format("~05b", MAKE_INT(-10))));
    TST_TN_EQ_STR("-0001",    STR(format("~05b", MAKE_INT(-1))));
    TST_TN_EQ_STR("00000",    STR(format("~05b", MAKE_INT(0))));
    TST_TN_EQ_STR("00001",    STR(format("~05b", MAKE_INT(1))));
    TST_TN_EQ_STR("01010",    STR(format("~05b", MAKE_INT(10))));
    TST_TN_EQ_STR("1100100",  STR(format("~05b", MAKE_INT(100))));

    TST_TN_EQ_STR("-1100100", STR(format("~5b",  MAKE_INT(-100))));
    TST_TN_EQ_STR("-1010",    STR(format("~5b",  MAKE_INT(-10))));
    TST_TN_EQ_STR("   -1",    STR(format("~5b",  MAKE_INT(-1))));
    TST_TN_EQ_STR("    0",    STR(format("~5b",  MAKE_INT(0))));
    TST_TN_EQ_STR("    1",    STR(format("~5b",  MAKE_INT(1))));
    TST_TN_EQ_STR(" 1010",    STR(format("~5b",  MAKE_INT(10))));
    TST_TN_EQ_STR("1100100",  STR(format("~5b",  MAKE_INT(100))));
}

TST_CASE("format ~f (number)")
{
    TST_TN_EQ_STR("-100", STR(format("~f",   MAKE_INT(-100))));
    TST_TN_EQ_STR("-10",  STR(format("~f",   MAKE_INT(-10))));
    TST_TN_EQ_STR("-1",   STR(format("~f",   MAKE_INT(-1))));
    TST_TN_EQ_STR("0",    STR(format("~f",   MAKE_INT(0))));
    TST_TN_EQ_STR("1",    STR(format("~f",   MAKE_INT(1))));
    TST_TN_EQ_STR("10",   STR(format("~f",   MAKE_INT(10))));
    TST_TN_EQ_STR("100",  STR(format("~f",   MAKE_INT(100))));

    TST_TN_EQ_STR("-100", STR(format("~0f",  MAKE_INT(-100))));
    TST_TN_EQ_STR("-10",  STR(format("~0f",  MAKE_INT(-10))));
    TST_TN_EQ_STR("-1",   STR(format("~0f",  MAKE_INT(-1))));
    TST_TN_EQ_STR("0",    STR(format("~0f",  MAKE_INT(0))));
    TST_TN_EQ_STR("1",    STR(format("~0f",  MAKE_INT(1))));
    TST_TN_EQ_STR("10",   STR(format("~0f",  MAKE_INT(10))));
    TST_TN_EQ_STR("100",  STR(format("~0f",  MAKE_INT(100))));

    TST_TN_EQ_STR("-100", STR(format("~03f", MAKE_INT(-100))));
    TST_TN_EQ_STR("-10",  STR(format("~03f", MAKE_INT(-10))));
    TST_TN_EQ_STR("-01",  STR(format("~03f", MAKE_INT(-1))));
    TST_TN_EQ_STR("000",  STR(format("~03f", MAKE_INT(0))));
    TST_TN_EQ_STR("001",  STR(format("~03f", MAKE_INT(1))));
    TST_TN_EQ_STR("010",  STR(format("~03f", MAKE_INT(10))));
    TST_TN_EQ_STR("100",  STR(format("~03f", MAKE_INT(100))));

    TST_TN_EQ_STR("-100", STR(format("~3f",  MAKE_INT(-100))));
    TST_TN_EQ_STR("-10",  STR(format("~3f",  MAKE_INT(-10))));
    TST_TN_EQ_STR(" -1",  STR(format("~3f",  MAKE_INT(-1))));
    TST_TN_EQ_STR("  0",  STR(format("~3f",  MAKE_INT(0))));
    TST_TN_EQ_STR("  1",  STR(format("~3f",  MAKE_INT(1))));
    TST_TN_EQ_STR(" 10",  STR(format("~3f",  MAKE_INT(10))));
    TST_TN_EQ_STR("100",  STR(format("~3f",  MAKE_INT(100))));

    TST_TN_EQ_STR("-100", STR(format("~3,02f", MAKE_INT(-100))));
    TST_TN_EQ_STR("-10",  STR(format("~3,02f", MAKE_INT(-10))));
    TST_TN_EQ_STR(" -1",  STR(format("~3,02f", MAKE_INT(-1))));
    TST_TN_EQ_STR("  0",  STR(format("~3,02f", MAKE_INT(0))));
    TST_TN_EQ_STR("  1",  STR(format("~3,02f", MAKE_INT(1))));
    TST_TN_EQ_STR(" 10",  STR(format("~3,02f", MAKE_INT(10))));
    TST_TN_EQ_STR("100",  STR(format("~3,02f", MAKE_INT(100))));
}

TST_CASE("format ~f (string)")
{
    TST_TN_EQ_STR("",       STR(format("~f",   CONST_STRING(""))));
    TST_TN_EQ_STR("",       STR(format("~0f",  CONST_STRING(""))));
    TST_TN_EQ_STR(" ",      STR(format("~1f",  CONST_STRING(""))));
    TST_TN_EQ_STR("",       STR(format("~00f", CONST_STRING(""))));
    TST_TN_EQ_STR(" ",      STR(format("~01f", CONST_STRING(""))));
    TST_TN_EQ_STR("\"",     STR(format("~f",   CONST_STRING("\""))));
    TST_TN_EQ_STR("\\",     STR(format("~f",   CONST_STRING("\\"))));
    TST_TN_EQ_STR("a",      STR(format("~f",   CONST_STRING("a"))));
    TST_TN_EQ_STR("aBc",    STR(format("~f",   CONST_STRING("aBc"))));
    TST_TN_EQ_STR("あ",     STR(format("~f",   CONST_STRING("あ"))));
    TST_TN_EQ_STR("あい",   STR(format("~f",   CONST_STRING("あい"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~f",   CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("aBc",    STR(format("~0f",  CONST_STRING("aBc"))));
    TST_TN_EQ_STR("aBc",    STR(format("~1f",  CONST_STRING("aBc"))));
    TST_TN_EQ_STR("aBc",    STR(format("~2f",  CONST_STRING("aBc"))));
    TST_TN_EQ_STR("aBc",    STR(format("~3f",  CONST_STRING("aBc"))));
    TST_TN_EQ_STR(" aBc",   STR(format("~4f",  CONST_STRING("aBc"))));
    TST_TN_EQ_STR("  aBc",  STR(format("~5f",  CONST_STRING("aBc"))));
    TST_TN_EQ_STR("aBc",    STR(format("~00f", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("aBc",    STR(format("~01f", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("aBc",    STR(format("~02f", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("aBc",    STR(format("~03f", CONST_STRING("aBc"))));
    TST_TN_EQ_STR(" aBc",   STR(format("~04f", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("  aBc",  STR(format("~05f", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("aBc",
                           STR(format("~00,01f", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("aBc",
                           STR(format("~01,01f", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("aBc",
                           STR(format("~02,01f", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("aBc",
                           STR(format("~03,01f", CONST_STRING("aBc"))));
    TST_TN_EQ_STR(" aBc",
                           STR(format("~04,01f", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("  aBc",
                           STR(format("~05,01f", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~0f",  CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~1f",  CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~2f",  CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~3f",  CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~4f",  CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~5f",  CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR(" aあBいc",
                           STR(format("~6f",  CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("  aあBいc",
                           STR(format("~7f",  CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~00f", CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~01f", CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~02f", CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~03f", CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~04f", CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("aあBいc",
                           STR(format("~05f", CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR(" aあBいc",
                           STR(format("~06f", CONST_STRING("aあBいc"))));
    TST_TN_EQ_STR("  aあBいc",
                           STR(format("~07f", CONST_STRING("aあBいc"))));
}

TST_CASE("format ~~")
{
    TST_TN_EQ_STR("~", STR(format("~~")));
}

TST_CASE("format ~%")
{
    TST_TN_EQ_STR("\n", STR(format("~%")));
}

TST_CASE("format ~&")
{
    TST_TN_EQ_STR("\n",   STR(format("~&")));
    TST_TN_EQ_STR("\n",   STR(format("~&~&")));
    TST_TN_EQ_STR("\n",   STR(format("~&~&~&")));
    TST_TN_EQ_STR("\n",   STR(format("~%~&")));
    TST_TN_EQ_STR("\n",   STR(format("~%~&~&")));
    TST_TN_EQ_STR("\n\n", STR(format("~&~%")));
    TST_TN_EQ_STR("\n\n", STR(format("~&~%~&")));
    TST_TN_EQ_STR("\n",   STR(format("\n~&")));
    TST_TN_EQ_STR("\n\n", STR(format("~&\n")));
    TST_TN_EQ_STR("\n\n", STR(format("~&\n~&")));
    TST_TN_EQ_STR(" \n",  STR(format(" ~&")));
    TST_TN_EQ_STR("\n \n \n", STR(format("\n ~& ~&")));
}

TST_CASE("format ~t")
{
    TST_TN_EQ_STR("\t", STR(format("~t")));
}

TST_CASE("format ~_")
{
    TST_TN_EQ_STR(" ", STR(format("~_")));
}

TST_CASE("format ~a")
{
    TST_TN_EQ_STR("#t",  STR(format("~a", SCM_TRUE)));
    TST_TN_EQ_STR("123", STR(format("~a", MAKE_INT(123))));
    TST_TN_EQ_STR("a",   STR(format("~a", MAKE_CHAR('a'))));
    TST_TN_EQ_STR("aBc", STR(format("~a", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("(#t 123 a aBc (0))",
                           STR(format("~a", lst)));
}

TST_CASE("format ~s")
{
    TST_TN_EQ_STR("#t",      STR(format("~s", SCM_TRUE)));
    TST_TN_EQ_STR("123",     STR(format("~s", MAKE_INT(123))));
    TST_TN_EQ_STR("#\\a",    STR(format("~s", MAKE_CHAR('a'))));
    TST_TN_EQ_STR("\"aBc\"", STR(format("~s", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("(#t 123 #\\a \"aBc\" (0))",
                           STR(format("~s", lst)));
}

TST_CASE("format ~w")
{
    TST_TN_EQ_STR("#t",      STR(format("~w", SCM_TRUE)));
    TST_TN_EQ_STR("123",     STR(format("~w", MAKE_INT(123))));
    TST_TN_EQ_STR("#\\a",    STR(format("~w", MAKE_CHAR('a'))));
    TST_TN_EQ_STR("\"aBc\"", STR(format("~w", CONST_STRING("aBc"))));
    TST_TN_EQ_STR("(#t 123 #\\a \"aBc\" (0))",
                           STR(format("~w", lst)));
    /* SigScheme starts the index with 1 */
    TST_TN_EQ_STR("#1=(0 1 . #1#)", STR(format("~w", clst)));
}

TST_CASE("format ~y")
{
    TST_TN_EQ_STR("#t",      STR(format("~y", SCM_TRUE)));
    TST_TN_EQ_STR("123",     STR(format("~y", MAKE_INT(123))));
    TST_TN_EQ_STR("#\\a",    STR(format("~y", MAKE_CHAR('a'))));
    TST_TN_EQ_STR("\"aBc\"", STR(format("~y", CONST_STRING("aBc"))));
    /* no pretty-print procedure */
    TST_TN_EQ_STR("(#t 123 #\\a \"aBc\" (0))",
                           STR(format("~y", lst)));
}

TST_CASE("format ~?")
{
    TST_TN_EQ_STR("~",  STR(format("~k", CONST_STRING("~~"), SCM_NULL)));
    TST_TN_EQ_STR(" ",  STR(format("~k", CONST_STRING("~_"), SCM_NULL)));
    TST_TN_EQ_STR("\n", STR(format("~k", CONST_STRING("~%"), SCM_NULL)));
    TST_TN_EQ_STR("\n", STR(format("~k", CONST_STRING("~&"), SCM_NULL)));
#if 0
    /* hard to be this on current port implementation */
    TST_TN_EQ_STR("\n",    STR(format("~?",
                                      CONST_STRING("~%~?"),
                                      LIST_2(CONST_STRING("~&"), SCM_NULL))));
#else
    TST_TN_EQ_STR("\n\n",  STR(format("~?",
                                      CONST_STRING("~%~?"),
                                      LIST_2(CONST_STRING("~&"), SCM_NULL))));
#endif
    TST_TN_EQ_STR("\n \n", STR(format("~?",
                                      CONST_STRING("~% ~?"),
                                      LIST_2(CONST_STRING("~&"), SCM_NULL))));
    TST_TN_EQ_STR("\n \n", STR(format("~?",
                                      CONST_STRING("~%~?"),
                                      LIST_2(CONST_STRING(" ~&"), SCM_NULL))));
    TST_TN_EQ_STR("aBc",   STR(format("~?",
                                      CONST_STRING("aBc"), SCM_NULL)));
    TST_TN_EQ_STR("0aBc1", STR(format("~?",
                                      CONST_STRING("0~a1"),
                                      LIST_1(CONST_STRING("aBc")))));
    TST_TN_EQ_STR("02aBc31",
                  STR(format("~?",
                             CONST_STRING("0~?1"),
                             LIST_2(CONST_STRING("2~a3"),
                                    LIST_1(CONST_STRING("aBc"))))));
    TST_TN_EQ_STR("024aBc531",
                  STR(format("~?",
                             CONST_STRING("0~?1"),
                             LIST_2(CONST_STRING("2~?3"),
                                    LIST_2(CONST_STRING("4~a5"),
                                           LIST_1(CONST_STRING("aBc")))))));
    TST_TN_EQ_STR("#t",    STR(format("~?",
                                      CONST_STRING("~w"),
                                      LIST_1(SCM_TRUE))));
    TST_TN_EQ_STR("123",   STR(format("~?",
                                      CONST_STRING("~w"),
                                      LIST_1(MAKE_INT(123)))));
    TST_TN_EQ_STR("#\\a",  STR(format("~?",
                                      CONST_STRING("~w"),
                                      LIST_1(MAKE_CHAR('a')))));
    TST_TN_EQ_STR("\"\"",  STR(format("~?",
                                      CONST_STRING("~w"),
                                      LIST_1(CONST_STRING("")))));
    TST_TN_EQ_STR("\"\\\"\"",
                  STR(format("~?",
                             CONST_STRING("~w"),
                             LIST_1(CONST_STRING("\"")))));
    TST_TN_EQ_STR("\"aBc\"",
                  STR(format("~?",
                             CONST_STRING("~w"),
                             LIST_1(CONST_STRING("aBc")))));
    TST_TN_EQ_STR("(#t 123 #\\a \"aBc\" (0))",
                  STR(format("~?",
                             CONST_STRING("~w"), LIST_1(lst))));
    /* SigScheme starts the index with 1 */
    TST_TN_EQ_STR("#1=(0 1 . #1#)",
                  STR(format("~?",
                             CONST_STRING("~w"), LIST_1(clst))));
}

TST_CASE("format ~k")
{
    TST_TN_EQ_STR("~",  STR(format("~k", CONST_STRING("~~"), SCM_NULL)));
    TST_TN_EQ_STR(" ",  STR(format("~k", CONST_STRING("~_"), SCM_NULL)));
    TST_TN_EQ_STR("\n", STR(format("~k", CONST_STRING("~%"), SCM_NULL)));
    TST_TN_EQ_STR("\n", STR(format("~k", CONST_STRING("~&"), SCM_NULL)));

    TST_TN_EQ_STR("024aBc531",
                  STR(format("~k",
                             CONST_STRING("0~k1"),
                             LIST_2(CONST_STRING("2~k3"),
                                    LIST_2(CONST_STRING("4~a5"),
                                           LIST_1(CONST_STRING("aBc")))))));
}

TST_CASE("format ~h")
{
    TST_TN_EQ_STR(MSG_SSCM_DIRECTIVE_HELP, STR(format("~h")));
}

TST_CASE("format mixed raw C directives")
{
    TST_TN_EQ_STR("-100 1010a64-01144100-01",
                  STR(format("~D~5QBa~WX~03JD~3LO~ZU~03TD",
                             -100,
                             (int64_t)10,
                             (int32_t)100,
                             (intmax_t)-1,
                             (long)100,
                             (size_t)100,
                             (ptrdiff_t)-1)));

    TST_TN_EQ_STR("-100 1010aa string64-01144あ100b-01",
                  STR(format("~D~5QBa~S~WX~03JD~3LO~C~ZU~C~03TD",
                             -100,
                             (int64_t)10,
                             "a string",
                             (int32_t)100,
                             (intmax_t)-1,
                             (long)100,
                             (scm_ichar_t)0x3042,
                             (size_t)100,
                             (scm_ichar_t)'b',
                             (ptrdiff_t)-1)));
}

TST_CASE("format mixed SRFI directives")
{
    TST_TN_EQ_STR("~\n", STR(format("~~~%")));
    TST_TN_EQ_STR("slashified: #\\a\nany: a\n",
                  STR(format("slashified: ~s~%any: ~a~%",
                             MAKE_CHAR('a'), MAKE_CHAR('a'))));

    TST_TN_EQ_STR("-100 1010aa string64-01144あ100b-01",
                  STR(format("~d~5ba~a~x~03d~3o~c~d~c~03d",
                             MAKE_INT(-100),
                             MAKE_INT(10),
                             CONST_STRING("a string"),
                             MAKE_INT(100),
                             MAKE_INT(-1),
                             MAKE_INT(100),
                             MAKE_CHAR(0x3042),
                             MAKE_INT(100),
                             MAKE_CHAR('b'),
                             MAKE_INT(-1))));
}

TST_CASE("format mixed SRFI & raw C directives")
{
    TST_TN_EQ_STR("-100 1010aa string64another string-01144~あ100b-01",
                  STR(format("~D~5ba~S~WX~a~03JD~3LO~~~c~ZU~C~03TD",
                             -100,
                             MAKE_INT(10),
                             "a string",
                             (int32_t)100,
                             CONST_STRING("another string"),
                             (intmax_t)-1,
                             (long)100,
                             MAKE_CHAR(0x3042),
                             (size_t)100,
                             (scm_ichar_t)'b',
                             (ptrdiff_t)-1)));
}

TST_CASE("format freshline by mixed SRFI & raw C directives")
{
    TST_TN_EQ_STR("\n",     STR(format("~C~&",   (scm_ichar_t)'\n')));
    TST_TN_EQ_STR("\n\n",   STR(format("~&~C~&", (scm_ichar_t)'\n')));
    TST_TN_EQ_STR("\n",     STR(format("~S~&",   "\n")));
    TST_TN_EQ_STR("\n\n",   STR(format("~&~S~&", "\n")));

#if 0
    /* current implementation does not support these behariors */
    TST_TN_EQ_STR("\n",
                  STR(format("~C~?",
                             (scm_ichar_t)'\n',
                             CONST_STRING("~&"), SCM_NULL)));
    TST_TN_EQ_STR("\n\n",
                  STR(format("~&~C~?",
                             (scm_ichar_t)'\n',
                             CONST_STRING("~&"), SCM_NULL)));
    TST_TN_EQ_STR("\n",
                  STR(format("~S~?",
                             "\n",
                             CONST_STRING("~&"), SCM_NULL)));
    TST_TN_EQ_STR("\n\n",
                  STR(format("~&~S~?",
                             "\n",
                             CONST_STRING("~&"), SCM_NULL)));
#endif
}
