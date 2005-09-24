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

#include "sigscheme/sigscheme.h"
#include "uim-scm.h"


#if UIM_SCM_GCC4_READY_GC

#if !SCM_GCC4_READY_GC
#error "UIM_SCM_GCC4_READY_GC requires SCM_GCC4_READY_GC-enabled SigScheme"
#endif

void
uim_scm_gc_protect(uim_lisp *location)
{
  SigScm_GC_Protect((ScmObj *)location);
}

uim_lisp *
uim_scm_gc_protect_stack(void)
{
  /*
   * &stack_start will be relocated to start of the frame of subsequent
   * function call
   */
  ScmObj stack_start;

  return (uim_lisp *)SigScm_GC_ProtectStack(&stack_start);
}

void
uim_scm_gc_unprotect_stack(uim_lisp *stack_start)
{
  SigScm_GC_UnprotectStack((ScmObj *)stack_start);
}

uim_func_ptr
uim_scm_gc_ensure_uninlined_func(uim_func_ptr func)
{
  return func;
}
#endif /* UIM_SCM_GCC4_READY_GC */
