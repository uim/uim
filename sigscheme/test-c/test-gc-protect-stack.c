/*===========================================================================
 *  Filename : test-gc-protect-stack.c
 *  About    : unit test for scm_gc_protect_stack()
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

#if 1
/* Since introduction of libgcroots to SigScheme, the GC protection interfaces
 * are withdrawn. These tests does not make sense now. -- YamaKen 2006-12-23 */
#define TST_EXCLUDE_THIS
#endif

#define TST_HAVE_MAIN 1
#include "sscm-test.h"

#ifndef TST_EXCLUDE_THIS

#include <stddef.h>
#include <sigscheme/sigscheme.h>

#define SCMOBJ_ALIGNEDP(ptr) (!((uintptr_t)(ptr) % sizeof(ScmObj)))

#define TEST_STACK_START(protected, actual)                                  \
    if (stack_dir == STACK_GROWS_DOWNWARDS) {                                \
        TST_TN_TRUE(actual <= protected);                                    \
    } else {                                                                 \
        TST_TN_TRUE(actual >= protected);                                    \
    }

enum stack_growth_dir {
    STACK_GROWS_DOWNWARDS,
    STACK_GROWS_UPWARDS
};

static enum stack_growth_dir stack_dir;
static void *volatile stack_start_protected;
static void *volatile stack_start_actual;
static void (*volatile fvv_internal)(void);
static int  (*volatile fiv_internal)(void);
static void (*volatile fvi_internal)(int);
static int  (*volatile fii_internal)(int);
static ScmObj *(*volatile fspsp_internal)(ScmObj *dummy);
static ScmObj *(*volatile fspsp2_internal)(ScmObj *dummy);

static enum stack_growth_dir probe_stack_growth_dir(void);
static enum stack_growth_dir probe_stack_growth_dir2(void *upper_frame);
static void fvv(void);
static int  fiv(void);
static void fvi(int dummy);
static int  fii(int dummy);
static ScmObj *fspsp(ScmObj *dummy);
static ScmObj *fspsp2(ScmObj *dummy);


static enum stack_growth_dir
probe_stack_growth_dir(void)
{
    int stack_start;

    return probe_stack_growth_dir2(&stack_start);
}

static enum stack_growth_dir
probe_stack_growth_dir2(void *upper_frame)
{
    int stack_start;

    if ((void *)&stack_start < upper_frame)
        return STACK_GROWS_DOWNWARDS;
    else
        return STACK_GROWS_UPWARDS;
}

static void
fvv(void)
{
    ScmObj stack_start;

    stack_start_actual = &stack_start;
}

static int
fiv(void)
{
    ScmObj stack_start;

    stack_start_actual = &stack_start;
    return 0;
}

static void
fvi(int dummy)
{
    ScmObj stack_start;

    stack_start_actual = &stack_start;
}

static int
fii(int dummy)
{
    ScmObj stack_start;

    stack_start_actual = &stack_start;
    return 0;
}

static ScmObj *
fspsp(ScmObj *dummy)
{
    ScmObj stack_start;

    stack_start_actual = &stack_start;
    return dummy;
}

/* simulates scm_gc_protect_stack_internal() */
static ScmObj *
fspsp2(ScmObj *designated_stack_start)
{
    ScmObj stack_start;

    if (!designated_stack_start)
        designated_stack_start = &stack_start;

    stack_start_actual = designated_stack_start;

    /* enabling this fragment on --enable-debug configuration offsets stack
     * pointer */
#if 1
    SCM_ASSERT(SCMOBJ_ALIGNEDP(stack_start_actual));
#endif

    /* may intentionally be an invalidated local address */
    return designated_stack_start;
}

int
main(int argc, char **argv)
{
    tst_suite_info suite = TST_DEFAULT_SUITE_SETUP;

    scm_initialize(NULL);

    stack_dir = probe_stack_growth_dir();
    fvv_internal = fvv;
    fiv_internal = fiv;
    fvi_internal = fvi;
    fii_internal = fii;
    fspsp_internal = fspsp;
    fspsp2_internal = fspsp2;

    tst_main(&suite);

    scm_finalize();

    TST_DEFAULT_SUITE_CLEANUP(suite);
    return !!suite.stats.fail;
}

TST_CASE("void (*)(void)")
{
    stack_start_protected = scm_gc_current_stack();
    scm_gc_protect_stack(stack_start_protected);
    (*fvv_internal)();
    scm_gc_unprotect_stack(stack_start_protected);

    TEST_STACK_START(stack_start_protected, stack_start_actual);
}

TST_CASE("int (*)(void)")
{
    stack_start_protected = scm_gc_current_stack();
    scm_gc_protect_stack(stack_start_protected);
    (*fiv_internal)();
    scm_gc_unprotect_stack(stack_start_protected);

    TEST_STACK_START(stack_start_protected, stack_start_actual);
}

TST_CASE("void (*)(int)")
{
    stack_start_protected = scm_gc_current_stack();
    scm_gc_protect_stack(stack_start_protected);
    (*fvi_internal)(0);
    scm_gc_unprotect_stack(stack_start_protected);

    TEST_STACK_START(stack_start_protected, stack_start_actual);
}

TST_CASE("int (*)(int)")
{
    stack_start_protected = scm_gc_current_stack();
    scm_gc_protect_stack(stack_start_protected);
    (*fii_internal)(0);
    scm_gc_unprotect_stack(stack_start_protected);

    TEST_STACK_START(stack_start_protected, stack_start_actual);
}

TST_CASE("ScmObj *(*)(ScmObj *)")
{
    stack_start_protected = scm_gc_current_stack();
    scm_gc_protect_stack(stack_start_protected);
    (*fspsp_internal)(NULL);
    scm_gc_unprotect_stack(stack_start_protected);

    TEST_STACK_START(stack_start_protected, stack_start_actual);
}

TST_CASE("ScmObj *(*)(ScmObj *) (2)")
{
    stack_start_protected = scm_gc_current_stack();
    scm_gc_protect_stack(stack_start_protected);
    (*fspsp2_internal)(NULL);
    scm_gc_unprotect_stack(stack_start_protected);

    TEST_STACK_START(stack_start_protected, stack_start_actual);
}

#endif /* TST_EXCLUDE_THIS */
