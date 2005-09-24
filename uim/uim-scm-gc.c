/*

  Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.

*/

#include "siod.h"
#include "uim-scm.h"


#if UIM_SCM_GCC4_READY_GC
/*
 * For ensuring that these function calls be uninlined. Don't access these
 * variables directly.
 *
 * Exporting the variables ensures that a expression (*f)() is certainly real
 * function call since the variables can be updated from outside of
 * libuim. Therefore, be avoid making the variables static by combining libuim
 * into other codes which enables function inlining for them.
 */
uim_lisp *(*uim_scm_gc_protect_stack_ptr)(void)
     = &uim_scm_gc_protect_stack_internal;
uim_func_ptr (*uim_scm_gc_ensure_uninlined_func_ptr)(uim_func_ptr)
     = &uim_scm_gc_ensure_uninlined_func_internal;
#endif /* UIM_SCM_GCC4_READY_GC */

#if UIM_SCM_GCC4_READY_GC
void
uim_scm_gc_protect(uim_lisp *location)
{
  siod_gc_protect((LISP *)location);
}

uim_lisp *
uim_scm_gc_protect_stack_internal(void)
{
  /*
   * &stack_start will be relocated to start of the frame of subsequent
   * function call
   */
  LISP stack_start;

  siod_gc_protect_stack(&stack_start);

  /* intentionally returns invalidated local address */
  return (uim_lisp *)&stack_start;
}

void
uim_scm_gc_unprotect_stack(uim_lisp *stack_start)
{
  siod_gc_unprotect_stack((LISP *)stack_start);
}

uim_func_ptr
uim_scm_gc_ensure_uninlined_func_internal(uim_func_ptr func)
{
  return func;
}
#endif /* UIM_SCM_GCC4_READY_GC */
