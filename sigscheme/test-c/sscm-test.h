/*=====================================================================-*-c-*-
 *  Filename : sscm-test.h
 *  About    : scheme C-level testing utilities
 *
 *  Copyright (C) 2006 Jun Inoue <jun.lambda@gmail.com>
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

#ifndef SSCM_TEST_H
#define SSCM_TEST_H


#include <sigscheme/sigscheme.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

typedef struct _tst_suite_info tst_suite_info;
typedef struct _tst_case_info tst_case_info;

/* ------------------------------
 * Output
 */

static char *tst_format(const char *msg, ...) SCM_UNUSED;
static void tst_puts(tst_suite_info *_, tst_case_info *__,
                     const char *msg) SCM_UNUSED;

static char *
tst_format(const char *msg, ...)
{
    va_list va;
    char *buf;
    int len;

    va_start(va, msg);
    len = vsnprintf(NULL, 0, msg, va);
    if (len < 0)
        abort();
    va_end(va);

    /*
     * C99: 7.15 Variable arguments <stdarg.h>
     *
     * The object ap may be passed as an argument to another function; if that
     * function invokes the va_arg macro with parameter ap, the value of ap in
     * the calling function is indeterminate and shall be passed to the va_end
     * macro prior to any further reference to ap.
     */
    /* x86_64-unknown-linux-gnu crashes if this va_start() is not invoked */
    va_start(va, msg);
    buf = malloc (len + 1);
    if (!buf)
        abort();
    vsnprintf (buf, len + 1, msg, va);
    va_end(va);

    return buf;
}

static void
tst_puts(tst_suite_info *_, tst_case_info *__, const char *msg)
{
    fputs (msg, stderr);
}



/* ------------------------------
 * Test case
 */

struct _tst_case_info {
    void (*fn)(tst_suite_info*, tst_case_info*);
    const char *desc;
    int done;
    int succ;
    int fail;
    int abortp;
};


#define TST_TRAMPOLINE(id) id##_

/* Add TST_PARAMS_DECL to the params list of auxiliary functions if
 * you need to use TST_*() macros in them.  Call those functions with
 * TST_PARAMS in the corresponding position. */
#define TST_PARAMS_DECL    tst_suite_info *TST_SUITE_INFO,      \
                           tst_case_info  *TST_CASE_INFO,       \
                           int TST_FAILED
#define TST_PARAMS         TST_SUITE_INFO, TST_CASE_INFO, TST_FAILED

/* If you're preprocessing with collect.sh, this macro and its
 * arguments has to be written in one line.  C macros can't be used in
 * either argument.  ID is the function name of the test case, DSC is
 * a description of the test case and must be a string
 * literal. */
#define TST_CASE(id, dsc)                               \
static void id(TST_PARAMS_DECL);                        \
static void                                             \
TST_TRAMPOLINE(id)(tst_suite_info *TST_SUITE_INFO,      \
                   tst_case_info  *TST_CASE_INFO)       \
{                                                       \
    int TST_FAILED = 0;                                 \
    TST_CASE_INFO->desc = dsc;                          \
    id(TST_PARAMS);                                     \
}                                                       \
static void                                             \
id(TST_PARAMS_DECL)


/* ------------------------------
 * Invocation
 */

struct _tst_suite_info {
    void (*logger)(tst_suite_info *, tst_case_info *, const char *);
    tst_case_info *results;
    struct {
        int cases;
        int done;               /* Number of individual tests. */
        int succ;
        int fail;
        int aborts;
    } stats;
};

#define TST_DEFAULT_SUITE_SETUP                 \
    {                                           \
        tst_puts, NULL, {0}                     \
    }

#define TST_DEFAULT_SUITE_CLEANUP(suite)        \
    do {                                        \
        free((suite).results);                  \
    } while(0)


#ifdef TST_EXCLUDE_THIS
#define TST_LIST_BEGIN()                        \
int                                             \
main ()                                         \
{                                               \
    puts ("Nothing to test.");                  \
    return 0;                                   \
}
#define TST_REGISTER(fn)        /* Empty */
#define TST_LIST_END()          /* Empty */

#else  /* !defined (TST_EXCLUDE_THIS) */

typedef struct _tst_run_args tst_run_args;
struct _tst_run_args {
    void (*fn)(tst_suite_info *, tst_case_info *);
    tst_suite_info *suite;
    tst_case_info *tcase;
};

#define TST_RUN(fn, s, c)  tst_run((fn), (s), (c))
static void *tst_run_internal(tst_run_args *args);
static void
tst_run(void (*fn)(tst_suite_info *, tst_case_info *),
        tst_suite_info *suite, tst_case_info *tcase)
{
    tst_run_args args;

    args.fn = fn;
    args.suite = suite;
    args.tcase = tcase;
    scm_call_with_gc_ready_stack((ScmGCGateFunc)tst_run_internal, &args);
}

static void *
tst_run_internal(tst_run_args *args)
{
    (*args->fn)(args->suite, args->tcase);
    return NULL;
}

static int tst_main(tst_suite_info *suite);

#define TST_LIST_BEGIN()                                \
/* Returns 1 if any test case fails, otherwise 0. */    \
static int                                              \
tst_main(tst_suite_info *suite)                         \
{                                                       \
    tst_case_info cases[] = {

#define TST_REGISTER(fn) { TST_TRAMPOLINE(fn) },

#define TST_LIST_END()                                          \
        { 0 } /* Dummy in case no test case is present. */      \
    };                                                          \
    size_t i;                                                   \
                                                                \
    puts("testing " __FILE__ "...");                                    \
    for (i = 0; cases[i].fn; i++) {                             \
        TST_RUN(cases[i].fn, suite, &cases[i]);                 \
        tst_analyze(suite, &cases[i]);                          \
    }                                                           \
    tst_summarize(suite);                                       \
                                                                \
    suite->results = malloc(sizeof(cases));                     \
    memcpy(suite->results, cases, sizeof(cases));               \
                                                                \
    return !!suite->stats.fail;                                 \
}                                                               \
TST_MAIN()

#ifdef TST_HAVE_MAIN
#define TST_MAIN()              /* Empty. */
#else  /* not have main() */
#define TST_MAIN()                                      \
int                                                     \
main(int argc, char *argv[])                            \
{                                                       \
    tst_suite_info suite = TST_DEFAULT_SUITE_SETUP;     \
    scm_initialize(NULL);                               \
    tst_main(&suite);                                   \
    scm_finalize();                                     \
    TST_DEFAULT_SUITE_CLEANUP(suite);                   \
    return !!suite.stats.fail;                          \
}
#endif /* not have main() */


static void
tst_analyze(tst_suite_info *suite, tst_case_info *result)
{
    suite->stats.done += result->done;
    suite->stats.succ += result->succ;
    suite->stats.fail += result->fail;
    ++suite->stats.cases;
    if (result->abortp) {
        ++suite->stats.aborts;
        suite->logger(suite, result,
                      tst_format("* ABORTED: %s\n", result->desc));
    } else {
        suite->logger(suite, result,
                      tst_format("%s: %s\n",
                                 result->fail ? "* FAILED"
                                              : "    OK",
                                 result->desc));
    }
}

static void
tst_summarize(tst_suite_info *suite)
{
    suite->logger(suite, NULL,
                  tst_format("%d test cases, %d aborted.  %d individual "
                             "tests, %d succeeded and %d failed.\n",
                             suite->stats.cases, suite->stats.aborts,
                             suite->stats.done,
                             suite->stats.succ, suite->stats.fail));
}

static int tst_count SCM_UNUSED;

static const char *tst_name(const char *testcase_desc, int serial) SCM_UNUSED;

static const char *
tst_name(const char *testcase_desc, int serial)
{
    static char *name = NULL;

    free(name);
    name = tst_format("%s #%d", testcase_desc, serial);

    return name;
}


#endif /* !defined (TST_EXCLUDE_THIS) */



/* ------------------------------
 * Tests
 */
#define TST_LOG(msg)  TST_SUITE_INFO->logger(TST_SUITE_INFO,    \
                                             TST_CASE_INFO,     \
                                             msg)
#define TST_FAIL(msg) (++TST_CASE_INFO->done,   \
                       ++TST_CASE_INFO->fail,   \
                       TST_LOG(msg),            \
                       TST_FAILED = 1,          \
                       0)
#define TST_SUCC()    (++TST_CASE_INFO->done,   \
                       ++TST_CASE_INFO->succ,   \
                       TST_FAILED = 0,          \
                       1)

#define TST_NAME() (tst_name(TST_CASE_INFO->desc, TST_CASE_INFO->done + 1))

#define TST_ABORT()   do { TST_CASE_INFO->abortp = 1; return; } while (0)

#define TST_ASSERT(cond) if (!(cond)) TST_ABORT()

#define TST_COND(cond, desc)                            \
    (((cond)                                            \
      ||                                                \
      TST_FAIL(tst_format(__FILE__ ":%d: %s failed.\n", \
                          __LINE__, desc)))             \
     && TST_SUCC())

#define TST_TRUE(exp, desc)  TST_COND((exp), desc)
#define TST_FALSE(exp, desc) TST_COND(!(exp), desc)

#define TST_EQUALITY(eqp, type, fmt, expect, actual, desc)      \
do {                                                            \
    type _x = (expect);                                         \
    type _a = (actual);                                         \
    if (!eqp(_x, _a)) {                                         \
        TST_FAIL(tst_format(__FILE__ ":%d: %s failed.\n"        \
                            "  expected: " fmt "\n"             \
                            "  but got : " fmt "\n",            \
                            __LINE__, desc, _x, _a));           \
    } else {                                                    \
        TST_SUCC();                                             \
    }                                                           \
} while (0)

/* Comparators. */
#define TST_C_EQUAL(a, b)    ((a) == (b))
#define TST_STR_EQUAL(a, b)  (!strcmp((a), (b)))


/* Equality tests. */
#if HAVE_INTMAX_T
#define TST_EQ_INT(x, a, desc)  TST_EQUALITY(TST_C_EQUAL, intmax_t, \
                                             "%jd", x, a, desc)
#define TST_EQ_UINT(x, a, desc) TST_EQUALITY(TST_C_EQUAL, uintmax_t, \
                                             "%ujd", x, a, desc)
#define TST_NEQ_INT(x, a, desc)  TST_EQUALITY(!TST_C_EQUAL, intmax_t, \
                                              "%jd", x, a, desc)
#define TST_NEQ_UINT(x, a, desc) TST_EQUALITY(!TST_C_EQUAL, uintmax_t, \
                                              "%ujd", x, a, desc)
#else  /* not have intmax_t */
#define TST_EQ_INT(x, a, desc)  TST_EQUALITY(TST_C_EQUAL, long, \
                                             "%ld", x, a, desc)
#define TST_EQ_UINT(x, a, desc) TST_EQUALITY(TST_C_EQUAL, unsigned long, \
                                             "%uld", x, a, desc)
#define TST_NEQ_INT(x, a, desc)  TST_EQUALITY(!TST_C_EQUAL, long, \
                                             "%ld", x, a, desc)
#define TST_NEQ_UINT(x, a, desc) TST_EQUALITY(!TST_C_EQUAL, unsigned long, \
                                             "%uld", x, a, desc)
#endif /* not have intmax_t */

#define TST_EQ_STR(x, a, desc)  TST_EQUALITY(TST_STR_EQUAL, char*,      \
                                             "%s", x, a, desc)
#define TST_NEQ_STR(x, a, desc)  TST_EQUALITY(!TST_STR_EQUAL, char*,    \
                                              "%s", x, a, desc)
#define TST_EQ_PTR(x, a, desc)  TST_EQUALITY(TST_C_EQUAL, void*,        \
                                             "%p", x, a, desc)
#define TST_NEQ_PTR(x, a, desc)  TST_EQUALITY(!TST_C_EQUAL, void*,      \
                                              "%p", x, a, desc)
#define TST_EQ_OBJ(x, a, desc)  TST_EQUALITY(SCM_EQ, scm_uintobj_t,     \
                                             "%lx", (scm_uintobj_t)x,   \
                                             (scm_uintobj_t)a, desc)
#define TST_NEQ_OBJ(x, a, desc)  TST_EQUALITY(!SCM_EQ, scm_uintobj_t,   \
                                              "%lx", (scm_uintobj_t)x,  \
                                              (scm_uintobj_t)a, desc)

/* Function pointers are a bit tricky. The '0UL' is needed to suppress warnings
 * on 64-bit env. */
typedef void (*tst_funcptr_t)();
#define TST_EQ_FPTR(x, a, desc)                                              \
    TST_EQUALITY(TST_C_EQUAL, tst_funcptr_t, "%p",                           \
                 (0 ? (tst_funcptr_t)(0UL | ((x) == (a))) /* Typecheck */    \
                    : (tst_funcptr_t)(x)),                                   \
                 (tst_funcptr_t)(a), desc)

#define TST_NEQ_FPTR(x, a, desc)                                             \
    TST_EQUALITY(!TST_C_EQUAL, tst_funcptr_t, "%p",                          \
                 (0 ? (tst_funcptr_t)(0UL | ((x) == (a))) /* Typecheck */    \
                    : (tst_funcptr_t)(x)),                                   \
                 (tst_funcptr_t)(a), desc)

#define TST_EQ  TST_EQ_OBJ
#define TST_NEQ TST_NEQ_OBJ


/* tests with auto-generated description */
#define TST_TN_SAVE   (tst_count = TST_CASE_INFO->done + 1)
#define TST_TN_NAME() (tst_name(TST_CASE_INFO->desc, tst_count))

/* Since TST_FOO(..., tst_name(TST_CASE_INFO->desc, TST_CASE_INFO->done + 1))
 * returns incorrect serial number, it is saved before evaluating
 * TST_FAIL() or TST_SUCC(). */
#define TST_TN_TRUE(exp)      TST_TN_SAVE; TST_TRUE((exp), TST_TN_NAME())
#define TST_TN_FALSE(exp)     TST_TN_SAVE; TST_FALSE((exp), TST_TN_NAME())
#define TST_TN_EQ_INT(x, a)   TST_TN_SAVE; TST_EQ_INT((x), (a), TST_TN_NAME())
#define TST_TN_EQ_UINT(x, a)  TST_TN_SAVE; TST_EQ_UINT((x), (a), TST_TN_NAME())
#define TST_TN_NEQ_INT(x, a)  TST_TN_SAVE; TST_NEQ_INT((x), (a), TST_TN_NAME())
#define TST_TN_NEQ_UINT(x, a) TST_TN_SAVE; TST_NEQ_UINT((x), (a), TST_TN_NAME())
#define TST_TN_EQ_STR(x, a)   TST_TN_SAVE; TST_EQ_STR((x), (a), TST_TN_NAME())
#define TST_TN_NEQ_STR(x, a)  TST_TN_SAVE; TST_NEQ_STR((x), (a), TST_TN_NAME())
#define TST_TN_EQ_PTR(x, a)   TST_TN_SAVE; TST_EQ_PTR((x), (a), TST_TN_NAME())
#define TST_TN_NEQ_PTR(x, a)  TST_TN_SAVE; TST_NEQ_PTR((x), (a), TST_TN_NAME())
#define TST_TN_EQ_OBJ(x, a)   TST_TN_SAVE; TST_EQ_OBJ((x), (a), TST_TN_NAME())
#define TST_TN_NEQ_OBJ(x, a)  TST_TN_SAVE; TST_NEQ_OBJ((x), (a), TST_TN_NAME())
#define TST_TN_EQ_FPTR(x, a)  TST_TN_SAVE; TST_EQ_FPTR((x), (a), TST_TN_NAME())
#define TST_TN_NEQ_FPTR(x, a) TST_TN_SAVE; TST_NEQ_FPTR((x), (a), TST_TN_NAME())
#define TST_TN_EQ  TST_TN_EQ_OBJ
#define TST_TN_NEQ TST_TN_NEQ_OBJ

#endif /* !def SSCM_TEST_H */
