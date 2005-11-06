/*===========================================================================
 *  FileName : sigschemefunctable.h
 *  About    : Built-in function table
 *
 *  Copyright (C) 2005      by Kazuki Ohta (mover@hct.zaq.ne.jp)
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

/*=======================================
  System Include
=======================================*/

/*=======================================
  Local Include
=======================================*/

/*=======================================
  Macro Definitions
=======================================*/
#define REGISTER_FUNC_TABLE(functable)                                  \
    do {                                                                \
        struct builtin_func_info *info = NULL;                          \
        for (info = functable; info->funcname; info++) {                \
            (*info->reg_func)(info->funcname, info->c_func);            \
        }                                                               \
    } while (/* CONSTCOND */ 0)

/*=======================================
  Type Definitions
=======================================*/
typedef ScmObj (*ScmBuiltinFunc)(void);
typedef void   (*ScmRegisterFunc)(const char *name, ScmBuiltinFunc func);

struct builtin_func_info {
    const char     *funcname;
    ScmBuiltinFunc  c_func;
    ScmRegisterFunc reg_func;
};

/*=======================================
   Variable Declarations
=======================================*/
extern struct builtin_func_info r5rs_func_info_table[];
extern struct builtin_func_info r5rs_deepcadrs_func_info_table[];
extern struct builtin_func_info srfi1_func_info_table[];
extern struct builtin_func_info srfi2_func_info_table[];
extern struct builtin_func_info srfi6_func_info_table[];
extern struct builtin_func_info srfi8_func_info_table[];
extern struct builtin_func_info srfi23_func_info_table[];
extern struct builtin_func_info srfi34_func_info_table[];
extern struct builtin_func_info srfi38_func_info_table[];
extern struct builtin_func_info srfi60_func_info_table[];
extern struct builtin_func_info siod_func_info_table[];
