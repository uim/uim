/*===========================================================================
 *  FileName : sigschemefunctable.h
 *  About    : Built-in function table
 *
 *  Copyright (C) 2005-2006 Kazuki Ohta <mover AT hct.zaq.ne.jp>
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
#ifndef __SIGSCHEME_FUNCTABLE_H
#define __SIGSCHEME_FUNCTABLE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "config.h"

/*=======================================
  System Include
=======================================*/

/*=======================================
  Local Include
=======================================*/

/*=======================================
  Macro Definitions
=======================================*/
#define SCM_REGISTER_FUNC_TABLE(functable)                                   \
    do {                                                                     \
        struct scm_func_registration_info *info = NULL;                      \
        for (info = functable; info->funcname; info++) {                     \
            (*info->reg_func)(info->funcname, info->c_func);                 \
        }                                                                    \
    } while (/* CONSTCOND */ 0)

/*=======================================
  Type Definitions
=======================================*/
typedef ScmObj (*ScmBuiltinFunc)(void);
typedef void   (*ScmRegisterFunc)(const char *name, ScmBuiltinFunc func);

struct scm_func_registration_info {
    const char     *funcname;
    ScmBuiltinFunc  c_func;
    ScmRegisterFunc reg_func;
};

/*=======================================
   Variable Declarations
=======================================*/
extern struct scm_func_registration_info scm_r5rs_syntax_func_info_table[];
extern struct scm_func_registration_info scm_r5rs_procedure_func_info_table[];
extern struct scm_func_registration_info scm_error_func_info_table[];

#if SCM_USE_DEEP_CADRS
extern struct scm_func_registration_info scm_r5rs_deepcadrs_func_info_table[];
#endif
#if SCM_USE_NONSTD_FEATURES
extern struct scm_func_registration_info scm_nonstd_func_info_table[];
#endif
#if SCM_USE_SRFI1
extern struct scm_func_registration_info scm_srfi1_func_info_table[];
#endif
#if SCM_USE_SRFI2
extern struct scm_func_registration_info scm_srfi2_func_info_table[];
#endif
#if SCM_USE_SRFI6
extern struct scm_func_registration_info scm_srfi6_func_info_table[];
#endif
#if SCM_USE_SRFI8
extern struct scm_func_registration_info scm_srfi8_func_info_table[];
#endif
#if SCM_USE_SRFI23
extern struct scm_func_registration_info scm_srfi23_func_info_table[];
#endif
#if SCM_USE_SRFI34
extern struct scm_func_registration_info scm_srfi34_func_info_table[];
#endif
#if SCM_USE_SRFI38
extern struct scm_func_registration_info scm_srfi38_func_info_table[];
#endif
#if SCM_USE_SRFI60
extern struct scm_func_registration_info scm_srfi60_func_info_table[];
#endif
#if SCM_COMPAT_SIOD
extern struct scm_func_registration_info scm_siod_func_info_table[];
#endif

#ifdef __cplusplus
}
#endif

#endif /* __SIGSCHEME_FUNCTABLE_H */
