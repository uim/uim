/*===========================================================================
 *  Filename : storage-common.h
 *  About    : Common part of the fatty and compact storage implementations
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
#ifndef __STORAGE_COMMON_H
#define __STORAGE_COMMON_H

/*
 * Internal representation defined in this file MUST NOT directly touched by
 * libsscm users. Use abstract public APIs defined in sigscheme.h.
 */

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
  Object Creators
=======================================*/
#if !SCM_SAL_HAS_IMMEDIATE_INT_ONLY
#define SCM_SAL_MAKE_INT                      scm_make_int
#endif

#define SCM_SAL_MAKE_CONS                     scm_make_cons

#if SCM_SAL_HAS_IMMUTABLE_CONS
#define SCM_SAL_MAKE_IMMUTABLE_CONS           scm_make_immutable_cons
#else
#define SCM_SAL_MAKE_IMMUTABLE_CONS           scm_make_cons
#endif

#define SCM_SAL_MAKE_SYMBOL                   scm_make_symbol

#if !SCM_SAL_HAS_IMMEDIATE_CHAR_ONLY
#define SCM_SAL_MAKE_CHAR                     scm_make_char
#endif

#define SCM_SAL_MAKE_STRING                   scm_make_string
#define SCM_SAL_MAKE_STRING_COPYING           scm_make_string_copying

#if SCM_SAL_HAS_IMMUTABLE_STRING
#define SCM_SAL_MAKE_IMMUTABLE_STRING         scm_make_immutable_string
#define SCM_SAL_MAKE_IMMUTABLE_STRING_COPYING scm_make_immutable_string_copying
#else
#define SCM_SAL_MAKE_IMMUTABLE_STRING         scm_make_string
#define SCM_SAL_MAKE_IMMUTABLE_STRING_COPYING scm_make_string_copying
#endif

#define SCM_SAL_MAKE_FUNC                     scm_make_func
#define SCM_SAL_MAKE_CLOSURE                  scm_make_closure
#define SCM_SAL_MAKE_VECTOR                   scm_make_vector

#if SCM_SAL_HAS_IMMUTABLE_VECTOR
#define SCM_SAL_MAKE_IMMUTABLE_VECTOR         scm_make_immutable_vector
#else
#define SCM_SAL_MAKE_IMMUTABLE_VECTOR         scm_make_vector
#endif

#define SCM_SAL_MAKE_PORT                     scm_make_port
#define SCM_SAL_MAKE_CONTINUATION             scm_make_continuation

#if SCM_USE_SSCM_EXTENSIONS
#define SCM_SAL_MAKE_C_POINTER                scm_make_cpointer
#define SCM_SAL_MAKE_C_FUNCPOINTER            scm_make_cfunc_pointer
#endif

#ifndef SCM_SAL_MAKE_VALUEPACKET
#define SCM_SAL_MAKE_VALUEPACKET              scm_make_value_packet
#endif

#if SCM_USE_HYGIENIC_MACRO
#define SCM_SAL_MAKE_HMACRO                   scm_make_hmacro
#define SCM_SAL_MAKE_FARSYMBOL                scm_make_farsymbol
#define SCM_SAL_MAKE_SUBPAT                   scm_make_subpat
#endif

/* Don't use these functions directly. Use SCM_MAKE_*() or MAKE_*() instead to
 * allow flexible object allocation. */
SCM_EXPORT ScmObj scm_make_cons(ScmObj kar, ScmObj kdr);
#if SCM_SAL_HAS_IMMUTABLE_CONS
SCM_EXPORT ScmObj scm_make_immutable_cons(ScmObj kar, ScmObj kdr);
#endif
#if (SCM_USE_NUMBER && !SCM_SAL_HAS_IMMEDIATE_INT_ONLY)
SCM_EXPORT ScmObj scm_make_int(scm_int_t val);
#endif
SCM_EXPORT ScmObj scm_make_symbol(char *name, ScmObj val);
#if (SCM_USE_CHAR && !SCM_SAL_HAS_IMMEDIATE_CHAR_ONLY)
SCM_EXPORT ScmObj scm_make_char(scm_ichar_t val);
#endif
#if SCM_USE_STRING
#if SCM_SAL_HAS_IMMUTABLE_STRING
SCM_EXPORT ScmObj scm_make_immutable_string(char *str, scm_int_t len);
SCM_EXPORT ScmObj scm_make_immutable_string_copying(const char *str,
                                                    scm_int_t len);
#endif
SCM_EXPORT ScmObj scm_make_string(char *str, scm_int_t len);
SCM_EXPORT ScmObj scm_make_string_copying(const char *str, scm_int_t len);
#endif /* SCM_USE_STRING */
SCM_EXPORT ScmObj scm_make_func(enum ScmFuncTypeCode type, ScmFuncType func);
SCM_EXPORT ScmObj scm_make_closure(ScmObj exp, ScmObj env);
#if SCM_USE_VECTOR
SCM_EXPORT ScmObj scm_make_vector(ScmObj *vec, scm_int_t len);
#if SCM_SAL_HAS_IMMUTABLE_VECTOR
SCM_EXPORT ScmObj scm_make_immutable_vector(ScmObj *vec, scm_int_t len);
#endif
#endif /* SCM_USE_VECTOR */
#if SCM_USE_PORT
SCM_EXPORT ScmObj scm_make_port(struct ScmCharPort_ *cport,
                                enum ScmPortFlag flag);
#endif /* SCM_USE_PORT */
#if SCM_USE_CONTINUATION
SCM_EXPORT ScmObj scm_make_continuation(void);
#endif /* SCM_USE_CONTINUATION */
#if !SCM_USE_VALUECONS
SCM_EXPORT ScmObj scm_make_value_packet(ScmObj values);
#endif
#if SCM_USE_SSCM_EXTENSIONS
SCM_EXPORT ScmObj scm_make_cpointer(void *ptr);
SCM_EXPORT ScmObj scm_make_cfunc_pointer(ScmCFunc ptr);
#endif
#if SCM_USE_HYGIENIC_MACRO
SCM_EXPORT ScmObj scm_make_hmacro(ScmObj rules, ScmObj defenv);
SCM_EXPORT ScmObj scm_make_farsymbol(ScmObj sym, ScmPackedEnv env);
SCM_EXPORT ScmObj scm_make_subpat(ScmObj x, scm_int_t meta);
#endif

/*===========================================================================
  Environment Specifiers
===========================================================================*/
#define SCM_SAL_INTERACTION_ENV SCM_NULL
/*
 * Current implementation cannot handle scheme-report-environment and
 * null-environment properly. Be careful to use these environemnts.
 */
#define SCM_SAL_R5RS_ENV        SCM_INTERACTION_ENV
#define SCM_SAL_NULL_ENV        SCM_INTERACTION_ENV

/* test NULLP() first for average performance */
#define SCM_SAL_ENVP(env) (NULLP(env) || CONSP(env))


#ifdef __cplusplus
}
#endif

#endif /* __STORAGE_COMMON_H */
