/*

  Copyright (c) 2007 uim Project http://code.google.com/p/uim/

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

#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <setjmp.h>
#include <assert.h>

#include "uim.h"


#if HAVE_SIGSETJMP
#define JMP_BUF           sigjmp_buf
#define SETJMP(env)       sigsetjmp((env), 1)
#define LONGJMP(env, val) siglongjmp((env), (val))
#else
#define JMP_BUF           jmp_buf
#define SETJMP(env)       setjmp(env)
#define LONGJMP(env, val) longjmp((env), (val))
#endif

/* Immediately returns UIM_TRUE if uim is disabled by a fatal error. */

static uim_bool fatal_errored;
static int guarded;
static JMP_BUF env;
static const char *err_msg;


void
uim_init_error(void)
{
  /* For re-initialization of libuim. */
  guarded = 0;

  /* fatal_errored must not be cleared even if libuim is re-initialized. */
}

/* can be nested */
uim_bool
uim_catch_error_begin(void)
{
  assert(guarded >= 0);

  if (fatal_errored)
    return UIM_TRUE;

  if (!guarded++) {
    if (SETJMP(env)) {
      guarded = 0;

      fputs("ERROR: ", stderr);
      if (fatal_errored)
	fputs("fatal: ", stderr);
      fputs(err_msg, stderr);
      fputs("\n", stderr);

      return UIM_TRUE;
    }
  }

  return UIM_FALSE;
}

void
uim_catch_error_end(void)
{
  guarded--;

  assert(guarded >= 0);
}

void
uim_throw_error(const char *msg)
{
  if (!guarded)
    exit(EXIT_FAILURE);

  err_msg = msg;
  LONGJMP(env, guarded);
}

void
uim_fatal_error(const char *msg)
{
  fatal_errored = UIM_TRUE;
  uim_throw_error(msg);
}

void *
uim_malloc(size_t size)
{
  void *p;

  p = malloc(size);
  if (!p)
    uim_fatal_error("malloc() failed");

  return p;
}

void *
uim_realloc(void *p, size_t size)
{
  p = realloc(p, size);
  if (!p)
    uim_fatal_error("realloc() failed");

  return p;
}

void *
uim_calloc(size_t nmemb, size_t size)
{
  void *p;

  p = calloc(nmemb, size);
  if (!p)
    uim_fatal_error("calloc() failed");

  return p;
}

