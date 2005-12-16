/*===========================================================================
 *  FileName : storage-protection.c
 *  About    : GC protection interface
 *
 *  Copyright (C) 2005      by YamaKen
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
 *  =========================================================================*/

/* This file will be removed soon */

/* CAUTION: following description is obsoleted. It will be rewritten soon. */

/*
 * **** THE FUNCTIONS OF THIS FILE MUST NOT BE INLINED INTO CALLER ****
 *
 * This file is intentionally separated from datas.c to make the SigScheme GC
 * safe against an advanced storage layout optimization on stack. At least GCC
 * 4.0 performs such optimization (reported in [Anthy-dev 2273] by Jun
 * Inoue. Thank you a lot).
 *
 *
 * The Problem
 *
 *   On the previous stack protection strategy, following problem occurs.(the
 *   explanation is based on [Anthy-dev 2273]).
 *
 *   Take a look in following code fragment.
 *
 *   {
 *       ScmObj stack_start;
 *       ScmObj str_port = SCM_FALSE;
 *       ScmObj ret = SCM_FALSE;
 *
 *       scm_gc_protect_stack(&stack_start);
 *
 *       str_port = scm_make_string_port(exp);
 *       ret = scm_read(str_port);
 *       ret = EVAL(ret, SCM_NULL);
 *
 *       scm_gc_unprotect_stack(stack_start);
 *   }
 *
 *   The previous strategy assumes that stack_start is certainly located at
 *   lower than str_port and ret. But the optimization breaks the assumption as
 *   follows.
 *
 *   At the breakpoint immediately before scm_gc_protect_stack() of the code
 *   compiled with gcc4 ((GCC) 4.0.1 20050617 (prerelease) (Debian 4.0.0-10)
 *   (IA32)), actual storage layout had reported as follows.
 *
 *   (gdb) p &stack_start
 *   $6 = (ScmObj *) 0xbfffe7d8
 *   (gdb) p &str_port
 *   $7 = (ScmObj *) 0xbfffe7dc
 *   (gdb) p $sp
 *   $12 = (void *) 0xbfffe7d0
 *
 *   This result indicates the storage location order inversion:
 *
 *     expected: %esp < &str_port < &stack_start
 *     actual:   %esp < &stack_start < &str_port
 *
 *   So str_port is not protected in the case.
 *
 *
 * Solution
 *
 *   - Allocate the stack_start dummy variable in a separated function
 *
 *   - Ensure that a code fragment that its stack must be protected is
 *     certainly executed on the stack higher than stack_start
 *
 *   To achieve it, a series of macros which turns a function into
 *   stack-protected. See following steps to know how to use it.
 *
 *   1) Split a code fragment that its stack must be protected off into a
 *      function. No stack treatment is appried here. The function must return
 *      a value and cannot be void. Return dummy value if return value is not
 *      needed.
 *
 *      static const char *eval_str(const char *exp)
 *      {
 *          ScmObj str_port = SCM_FALSE;
 *          ScmObj ret = SCM_FALSE;
 *
 *          str_port = scm_make_string_port(exp);
 *          ret = scm_read(str_port);
 *          ret = EVAL(ret, SCM_NULL);
 *          return SCM_STRING_STR(ret);
 *      }
 *
 *   2) Rewrite 1) with the macro as follows. Select appropriate one that
 *      accepts same number of arguments of the function. In this case, single
 *      argument version is selected. Where to be rewritten is only the
 *      function header. Function body is kept untouched. If the function has
 *      no storage class specifier such as static, make 1st argument empty.
 *
 *      SCM_DEFINE_GC_PROTECTED_FUNC1(static, const char *,
 *                                    eval_str, const char *, exp)
 *      {
 *          ScmObj str_port = SCM_FALSE;
 *          ScmObj ret = SCM_FALSE;
 *
 *          str_port = scm_make_string_port(exp);
 *          ret = scm_read(str_port);
 *          ret = EVAL(ret, SCM_NULL);
 *          return SCM_STRING_STR(ret);
 *      }
 *
 *   3) To apply stack protection to the function, simply call it as ordinary
 *      function. The protection is implicitly performed by hidden code
 *      fragments embedded in the function by the macro. See following code
 *      fragment for instance.
 *
 *      caller:
 *      {
 *          const char *str;
 *
 *          <stack is not protected before the function invocation>
 *
 *          str = eval_str("(number->string (* 32 1024))");
 *
 *          <stack is not protected after the function invocation>
 *      }
 *
 *
 * Design Considerations
 *
 *   - Why don't you merge this file into datas.c?
 *
 *     To prevent implicit inlining of the functions. Although the noinline
 *     attribute is specified for GCC, other platforms needs another
 *     method. Compiling into a separated object file is an easy way to prevent
 *     the inlining.
 *
 *
 *   - Why SCM_DEFINE_GC_PROTECTED_FUNCn() does not support void for return
 *     type?
 *
 *     Since my macro programming skill is not enough orz. Would you help me?
 *
 *
 *   - Why don't you use variadic macro?
 *
 *     To support C89 environments. Please don't assume that newest GCC on an
 *     UNIX flavor is the only platform of SigScheme
 *
 *
 *   - Why don't you use SCM_GC_CALL_PROTECTED_FUNC() in client-side code
 *     directly?
 *
 *     Although following form can perform separated gc_protect_stack() and set
 *     the stack_start pointer appropriately, a corruption may occur.
 *
 *       SCM_GC_CALL_PROTECTED_FUNC(str, eval_str("(number->string 32)"));
 *
 *     In this case, the target function (eval_str) may be expanded into the
 *     calee code. This means that its local variables may be relocated into
 *     caller's own stack frame. Once such relocation has occurred, the
 *     stack_start pointer will be set to a wrong point upper then the target's
 *     local variables.
 *
 *     To avoid this condition, gc_protect_stack() and gc_unprotect_stack()
 *     must certainly be invoked in a separated function which will not be
 *     inlined. So scm_gc_call_protected_funcn() are prepared as function.
 *
 *  -- YamaKen 2005-09-07
 */

/*=======================================
  System Include
=======================================*/

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Extern Function Declarations
=======================================*/

/*=======================================
  Function Implementations
=======================================*/
