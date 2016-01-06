/*

  Copyright (c) 2006-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "uim.h"
#include "uim-scm.h"

#define TEST_STACK_START(protected, actual)				     \
  fprintf(stderr, "stack growth dir = %s, protected = %p, actual = %p\n",    \
	  (stack_dir == STACK_GROWS_DOWNWARDS) ? "downwards" : "upwards",    \
	  protected, actual);						     \
  if (stack_dir == STACK_GROWS_DOWNWARDS) {				     \
      assert(actual <= protected);					     \
  } else {								     \
      assert(actual >= protected);					     \
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
static uim_lisp *(*volatile fspv_internal)(void);
static uim_lisp *(*volatile fspsp_internal)(uim_lisp *dummy);

static enum stack_growth_dir probe_stack_growth_dir(void);
static enum stack_growth_dir probe_stack_growth_dir2(void *upper_frame);
static void fvv(void);
static int  fiv(void);
static void fvi(int dummy);
static int  fii(int dummy);
static uim_lisp *fspv(void);
static uim_lisp *fspsp(uim_lisp *dummy);


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
  uim_lisp stack_start;

  stack_start_actual = &stack_start;
}

static int
fiv(void)
{
  uim_lisp stack_start;

  stack_start_actual = &stack_start;
  return 0;
}

static void
fvi(int dummy)
{
  uim_lisp stack_start;

  stack_start_actual = &stack_start;
}

static int
fii(int dummy)
{
  uim_lisp stack_start;

  stack_start_actual = &stack_start;
  return 0;
}

static uim_lisp *
fspv(void)
{
  uim_lisp stack_start;

  stack_start_actual = &stack_start;
  return NULL;
}

static uim_lisp *
fspsp(uim_lisp *dummy)
{
  uim_lisp stack_start;

  stack_start_actual = &stack_start;
  return dummy;
}

int
main(void)
{
  uim_init();

  stack_dir = probe_stack_growth_dir();
  fvv_internal = fvv;
  fiv_internal = fiv;
  fvi_internal = fvi;
  fii_internal = fii;
  fspv_internal = fspv;
  fspsp_internal = fspsp;

  stack_start_protected = uim_scm_gc_current_stack();
  uim_scm_gc_protect_stack(stack_start_protected);
  (*fvv_internal)();
  uim_scm_gc_unprotect_stack(stack_start_protected);
  TEST_STACK_START(stack_start_protected, stack_start_actual);

  stack_start_protected = uim_scm_gc_current_stack();
  uim_scm_gc_protect_stack(stack_start_protected);
  (*fiv_internal)();
  uim_scm_gc_unprotect_stack(stack_start_protected);
  TEST_STACK_START(stack_start_protected, stack_start_actual);

  stack_start_protected = uim_scm_gc_current_stack();
  uim_scm_gc_protect_stack(stack_start_protected);
  (*fvi_internal)(0);
  uim_scm_gc_unprotect_stack(stack_start_protected);
  TEST_STACK_START(stack_start_protected, stack_start_actual);

  stack_start_protected = uim_scm_gc_current_stack();
  uim_scm_gc_protect_stack(stack_start_protected);
  (*fii_internal)(0);
  uim_scm_gc_unprotect_stack(stack_start_protected);
  TEST_STACK_START(stack_start_protected, stack_start_actual);

  stack_start_protected = uim_scm_gc_current_stack();
  uim_scm_gc_protect_stack(stack_start_protected);
  (*fspv_internal)();
  uim_scm_gc_unprotect_stack(stack_start_protected);
  TEST_STACK_START(stack_start_protected, stack_start_actual);

  stack_start_protected = uim_scm_gc_current_stack();
  uim_scm_gc_protect_stack(stack_start_protected);
  (*fspsp_internal)((uim_lisp *)NULL);
  uim_scm_gc_unprotect_stack(stack_start_protected);
  TEST_STACK_START(stack_start_protected, stack_start_actual);

  uim_quit();

  fprintf(stderr, "tests succeeded.\n");

  return EXIT_SUCCESS;
}
