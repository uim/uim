/*===========================================================================
 *  FileName : sigscheme.h
 *  About    : main header file
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
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 *  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
===========================================================================*/
#ifndef __SIGSCHEME_H
#define __SIGSCHEME_H

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
   System Include
=======================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*=======================================
   Local Include
=======================================*/
#include "config.h"

/*=======================================
   Macro Declarations
=======================================*/

/* dependency resolution */
#if SCM_COMPAT_SIOD
#undef SCM_USE_SRFI60
#define SCM_USE_SRFI60          1
#else
#undef SCM_COMPAT_SIOD_BUGS
#define SCM_COMPAT_SIOD_BUGS    0
#endif /* SCM_COMPAT_SIOD */

#ifdef __GNUC__
#define SCM_NOINLINE __attribute__((noinline))
#else
#define SCM_NOINLINE
#endif /* __GNUC__ */

int SigScm_Die(const char *msg, const char *filename, int line); /* error.c */
#define SCM_ASSERT(cond) \
    (cond ? 0 : SigScm_Die("assertion failed.", __FILE__, __LINE__))

#define SCM_SYMBOL_BOUNDP(sym) (SCM_NEQ(SCM_SYMBOL_VCELL(sym), SCM_UNBOUND))

#define SCM_CONS(kar, kdr) (Scm_NewCons(kar, kdr))

#define SCM_LIST_1(elm0) \
    (SCM_CONS((elm0), SCM_NULL))
#define SCM_LIST_2(elm0, elm1) \
    (SCM_CONS((elm0), SCM_LIST_1(elm1)))
#define SCM_LIST_3(elm0, elm1, elm2) \
    (SCM_CONS((elm0), SCM_LIST_2(elm1, elm2)))
#define SCM_LIST_4(elm0, elm1, elm2, elm3) \
    (SCM_CONS((elm0), SCM_LIST_3(elm1, elm2, elm3)))
#define SCM_LIST_5(elm0, elm1, elm2, elm3, elm4) \
    (SCM_CONS((elm0), SCM_LIST_4(elm1, elm2, elm3, elm4)))

#define SCM_EVAL(obj, env) (ScmOp_eval(obj, env))

/*
 * Function Invocation With Stack Protection
 *
 * Users should only use SCM_GC_PROTECTED_FUNC_DECL(),
 * SCM_GC_CALL_PROTECTED_FUNC() and SCM_GC_CALL_PROTECTED_VOID_FUNC().
 */
#if SCM_GCC4_READY_GC

#define SCM_GC_PROTECTED_FUNC_T(func) SigScm_GC_ProtectedType__##func

#define SCM_GC_PROTECTED_FUNC_DECL(ret_type, func, args)                     \
    ret_type func args SCM_NOINLINE;                                         \
    typedef ret_type (*SCM_GC_PROTECTED_FUNC_T(func)) args

#define SCM_GC_CALL_PROTECTED_FUNC(ret, func, args)                          \
    SCM_GC_CALL_PROTECTED_FUNC_INTERNAL(ret = , func, args)

#define SCM_GC_CALL_PROTECTED_VOID_FUNC(func, args)                          \
    SCM_GC_CALL_PROTECTED_FUNC_INTERNAL((void), func, args)

#define SCM_GC_CALL_PROTECTED_FUNC_INTERNAL(exp_ret, func, args)             \
    do {                                                                     \
        SCM_GC_PROTECTED_FUNC_T(func) fp;                                    \
        ScmObj *stack_start;                                                 \
                                                                             \
        stack_start = SigScm_GC_ProtectStack(NULL);                          \
        fp = (SCM_GC_PROTECTED_FUNC_T(func))                                 \
                  SigScm_GC_EnsureUninlinedFunc((ScmCFunc)&func);            \
        exp_ret (*fp)args;                                                   \
        SigScm_GC_UnprotectStack(stack_start);                               \
    } while (/* CONSTCOND */ 0)

#endif /* SCM_GCC4_READY_GC */

/*=======================================
   Struct Declarations
=======================================*/
typedef void (*ScmCFunc)(void);

/* type declaration */
#include "sigschemetype.h"

/*=======================================
   Variable Declarations
=======================================*/
/* storage-protection.c */
#if SCM_GCC4_READY_GC
/*
 * For ensuring that these function calls be uninlined. Dont' access these
 * variables directly.
 */
extern ScmObj *(*scm_gc_protect_stack)(ScmObj *);
extern ScmCFunc (*scm_gc_ensure_uninlined_func)(ScmCFunc);
#endif /* SCM_GCC4_READY_GC */

/*=======================================
   Function Declarations
=======================================*/
/*===========================================================================
   SigScheme : Core Functions
===========================================================================*/
/* sigscheme.c */
void SigScm_Initialize(void);
void SigScm_Finalize(void);
void Scm_DefineAlias(const char *newsym, const char *sym);

/* For compatibility only; slated for removal. */
void Scm_RegisterFunc0(const char *name, ScmFuncType0 func);
void Scm_RegisterFunc1(const char *name, ScmFuncType1 func);
void Scm_RegisterFunc2(const char *name, ScmFuncType2 func);
void Scm_RegisterFunc3(const char *name, ScmFuncType3 func);
void Scm_RegisterFunc4(const char *name, ScmFuncType4 func);
void Scm_RegisterFunc5(const char *name, ScmFuncType5 func);
void Scm_RegisterFuncEvaledList(const char *name, ScmFuncTypeEvaledList func);
void Scm_RegisterFuncRawList(const char *name, ScmFuncTypeRawList func);
void Scm_RegisterFuncRawListTailRec(const char *name, ScmFuncTypeRawListTailRec func);
void Scm_RegisterFuncRawListWithTailFlag(const char *name, ScmFuncTypeRawListWithTailFlag func);

/* Procedure/Syntax Registration */
void Scm_RegisterReductionOperator(const char *name, ScmObj (*func)(ScmObj, ScmObj, enum ScmReductionState*));
void Scm_RegisterSyntaxFixed0(const char *name, ScmObj (*func)(ScmObj));
#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterSyntaxFixed1(const char *name, ScmObj (*func)(ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterSyntaxFixed2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterSyntaxFixed3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterSyntaxFixed4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterSyntaxFixed5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj));
#endif
void Scm_RegisterSyntaxFixedTailRec0(const char *name, ScmObj (*func)(ScmEvalState*));
#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterSyntaxFixedTailRec1(const char *name, ScmObj (*func)(ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterSyntaxFixedTailRec2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterSyntaxFixedTailRec3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterSyntaxFixedTailRec4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterSyntaxFixedTailRec5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
void Scm_RegisterSyntaxVariadic0(const char *name, ScmObj (*func)(ScmObj, ScmObj));
#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterSyntaxVariadic1(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterSyntaxVariadic2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterSyntaxVariadic3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterSyntaxVariadic4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterSyntaxVariadic5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj));
#endif
void Scm_RegisterSyntaxVariadicTailRec0(const char *name, ScmObj (*func)(ScmObj, ScmEvalState*));
#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterSyntaxVariadicTailRec1(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterSyntaxVariadicTailRec2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterSyntaxVariadicTailRec3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterSyntaxVariadicTailRec4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterSyntaxVariadicTailRec5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
void Scm_RegisterProcedureFixed0(const char *name, ScmObj (*func)());
#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterProcedureFixed1(const char *name, ScmObj (*func)(ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterProcedureFixed2(const char *name, ScmObj (*func)(ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterProcedureFixed3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterProcedureFixed4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterProcedureFixed5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj));
#endif
void Scm_RegisterProcedureFixedTailRec0(const char *name, ScmObj (*func)(ScmEvalState*));
#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterProcedureFixedTailRec1(const char *name, ScmObj (*func)(ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterProcedureFixedTailRec2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterProcedureFixedTailRec3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterProcedureFixedTailRec4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterProcedureFixedTailRec5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
void Scm_RegisterProcedureVariadic0(const char *name, ScmObj (*func)(ScmObj));
#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterProcedureVariadic1(const char *name, ScmObj (*func)(ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterProcedureVariadic2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterProcedureVariadic3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterProcedureVariadic4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterProcedureVariadic5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj));
#endif
void Scm_RegisterProcedureVariadicTailRec0(const char *name, ScmObj (*func)(ScmObj, ScmEvalState*));
#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterProcedureVariadicTailRec1(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterProcedureVariadicTailRec2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterProcedureVariadicTailRec3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterProcedureVariadicTailRec4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterProcedureVariadicTailRec5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*));
#endif

/* datas.c */
void   SigScm_InitStorage(void);
void   SigScm_FinalizeStorage(void);
void   SigScm_GC_Protect(ScmObj *var);
#if !SCM_GCC4_READY_GC
void   SigScm_GC_ProtectStack(ScmObj *stack_start);
void   SigScm_GC_UnprotectStack(ScmObj *stack_start);
#endif
ScmObj Scm_NewCons(ScmObj a, ScmObj b);
ScmObj Scm_NewInt(int val);
ScmObj Scm_NewSymbol(char *name, ScmObj v_cell);
ScmObj Scm_NewChar(char *ch);
ScmObj Scm_NewString(char *str);
ScmObj Scm_NewStringCopying(const char *str);
ScmObj Scm_NewStringWithLen(char *str, int len);
ScmObj Scm_NewFunc(enum ScmFuncTypeCode type, ScmFuncType func);
ScmObj Scm_NewClosure(ScmObj exp, ScmObj env);
ScmObj Scm_NewVector(ScmObj *vec, int len);
ScmObj Scm_NewFilePort(FILE *file, const char *filename, enum ScmPortDirection pdireciton);
ScmObj Scm_NewStringPort(const char *str);  /* input only? */
ScmObj Scm_NewContinuation(void);
#if !SCM_USE_VALUECONS
ScmObj Scm_NewValuePacket(ScmObj values);
#endif
#if SCM_USE_NONSTD_FEATURES
ScmObj Scm_NewCPointer(void *data);
ScmObj Scm_NewCFuncPointer(ScmCFunc func);
#endif
ScmObj Scm_Intern(const char *name);
ScmObj Scm_eval_c_string(const char *exp);
#if SCM_COMPAT_SIOD
ScmObj Scm_return_value(void);
#endif

/* storage-protection.c */
#if SCM_GCC4_READY_GC
/*
 * Ordinary programs should not call these functions directly. Use
 * SCM_GC_CALL_PROTECTED_*FUNC() instead.
 */
#ifdef __GNUC__
#define SigScm_GC_ProtectStack SigScm_GC_ProtectStackInternal
#define SigScm_GC_EnsureUninlinedFunc SigScm_GC_EnsureUninlinedFuncInternal
#else /* __GNUC__ */
#define SigScm_GC_ProtectStack (*scm_gc_protect_stack)
#define SigScm_GC_EnsureUninlinedFunc (*scm_gc_ensure_uninlined_func)
#endif /* __GNUC__ */
void SigScm_GC_UnprotectStack(ScmObj *stack_start);

ScmObj *SigScm_GC_ProtectStackInternal(ScmObj *designated_stack_start) SCM_NOINLINE;
ScmCFunc SigScm_GC_EnsureUninlinedFuncInternal(ScmCFunc func) SCM_NOINLINE;
#endif /* SCM_GCC4_READY_GC */

/* eval.c */
ScmObj ScmOp_eval(ScmObj obj, ScmObj env);
ScmObj ScmOp_apply(ScmObj proc, ScmObj arg0, ScmObj rest, ScmEvalState *eval_state);
ScmObj ScmOp_quote(ScmObj datum, ScmObj env);
ScmObj ScmExp_lambda(ScmObj args, ScmObj env);
ScmObj ScmExp_if(ScmObj test, ScmObj conseq, ScmObj rest, ScmEvalState *eval_state);
ScmObj ScmExp_set(ScmObj var, ScmObj val, ScmObj env);
ScmObj ScmExp_cond(ScmObj arg, ScmEvalState *eval_state);
ScmObj ScmExp_case(ScmObj arg, ScmEvalState *eval_state);
ScmObj ScmExp_and(ScmObj arg, ScmEvalState *eval_state);
ScmObj ScmExp_or(ScmObj arg, ScmEvalState *eval_state);
ScmObj ScmExp_let(ScmObj arg, ScmEvalState *eval_state);
ScmObj ScmExp_let_star(ScmObj arg, ScmEvalState *eval_state);
ScmObj ScmExp_letrec(ScmObj arg, ScmEvalState *eval_state);
ScmObj ScmExp_begin(ScmObj arg, ScmEvalState *eval_state);
ScmObj ScmExp_do(ScmObj arg, ScmEvalState *eval_state);
ScmObj ScmOp_delay(ScmObj expr, ScmObj env);
ScmObj ScmOp_quasiquote(ScmObj datum, ScmObj env);
ScmObj ScmOp_unquote(ScmObj dummy, ScmObj env);
ScmObj ScmOp_unquote_splicing(ScmObj dummy, ScmObj env);
ScmObj ScmExp_define(ScmObj var, ScmObj rest, ScmObj env);
ScmObj ScmOp_scheme_report_environment(ScmObj version);
ScmObj ScmOp_null_environment(ScmObj version);
ScmObj ScmOp_interaction_environment(void);

ScmObj Scm_call(ScmObj proc, ScmObj args);

/* operations.c */
ScmObj ScmOp_eqvp(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_eqp(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_equalp(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_add(ScmObj left, ScmObj right, enum ScmReductionState *state);
ScmObj ScmOp_subtract(ScmObj left, ScmObj right, enum ScmReductionState *state);
ScmObj ScmOp_multiply(ScmObj left, ScmObj right, enum ScmReductionState *state);
ScmObj ScmOp_divide(ScmObj left, ScmObj right, enum ScmReductionState *state);
ScmObj ScmOp_equal(ScmObj left, ScmObj right, enum ScmReductionState *state);
ScmObj ScmOp_less(ScmObj left, ScmObj right, enum ScmReductionState *state);
ScmObj ScmOp_less_eq(ScmObj left, ScmObj right, enum ScmReductionState *state);
ScmObj ScmOp_greater(ScmObj left, ScmObj right, enum ScmReductionState *state);
ScmObj ScmOp_greater_eq(ScmObj left, ScmObj right, enum ScmReductionState *state);
ScmObj ScmOp_numberp(ScmObj obj);
ScmObj ScmOp_zerop(ScmObj scm_num);
ScmObj ScmOp_positivep(ScmObj scm_num);
ScmObj ScmOp_negativep(ScmObj scm_num);
ScmObj ScmOp_oddp(ScmObj scm_num);
ScmObj ScmOp_evenp(ScmObj scm_num);
ScmObj ScmOp_max(ScmObj left, ScmObj right, enum ScmReductionState *state);
ScmObj ScmOp_min(ScmObj left, ScmObj right, enum ScmReductionState *state);
ScmObj ScmOp_abs(ScmObj scm_num);
ScmObj ScmOp_quotient(ScmObj scm_n1, ScmObj scm_n2);
ScmObj ScmOp_modulo(ScmObj scm_n1, ScmObj scm_n2);
ScmObj ScmOp_remainder(ScmObj scm_n1, ScmObj scm_n2);
ScmObj ScmOp_number2string (ScmObj args, ScmObj env);
ScmObj ScmOp_string2number(ScmObj string);
ScmObj ScmOp_not(ScmObj obj);
ScmObj ScmOp_booleanp(ScmObj obj);
ScmObj ScmOp_car(ScmObj obj);
ScmObj ScmOp_cdr(ScmObj obj);
ScmObj ScmOp_pairp(ScmObj obj);
ScmObj ScmOp_cons(ScmObj car, ScmObj cdr);
ScmObj ScmOp_setcar(ScmObj pair, ScmObj car);
ScmObj ScmOp_setcdr(ScmObj pair, ScmObj cdr);
ScmObj ScmOp_caar(ScmObj lst);
ScmObj ScmOp_cadr(ScmObj lst);
ScmObj ScmOp_cdar(ScmObj lst);
ScmObj ScmOp_cddr(ScmObj lst);
ScmObj ScmOp_caaar(ScmObj lst);
ScmObj ScmOp_caadr(ScmObj lst);
ScmObj ScmOp_cadar(ScmObj lst);
ScmObj ScmOp_caddr(ScmObj lst);
ScmObj ScmOp_cdaar(ScmObj lst);
ScmObj ScmOp_cdadr(ScmObj lst);
ScmObj ScmOp_cddar(ScmObj lst);
ScmObj ScmOp_cdddr(ScmObj lst);
ScmObj ScmOp_caaaar(ScmObj lst);
ScmObj ScmOp_caaadr(ScmObj lst);
ScmObj ScmOp_caadar(ScmObj lst);
ScmObj ScmOp_caaddr(ScmObj lst);
ScmObj ScmOp_cadaar(ScmObj lst);
ScmObj ScmOp_cadadr(ScmObj lst);
ScmObj ScmOp_caddar(ScmObj lst);
ScmObj ScmOp_cadddr(ScmObj lst);
ScmObj ScmOp_cdaaar(ScmObj lst);
ScmObj ScmOp_cdaadr(ScmObj lst);
ScmObj ScmOp_cdadar(ScmObj lst);
ScmObj ScmOp_cdaddr(ScmObj lst);
ScmObj ScmOp_cddaar(ScmObj lst);
ScmObj ScmOp_cddadr(ScmObj lst);
ScmObj ScmOp_cdddar(ScmObj lst);
ScmObj ScmOp_cddddr(ScmObj lst);
ScmObj ScmOp_list(ScmObj obj, ScmObj env);
ScmObj ScmOp_nullp(ScmObj obj);
ScmObj ScmOp_listp(ScmObj obj);
ScmObj ScmOp_length(ScmObj obj);
ScmObj ScmOp_append(ScmObj args, ScmObj env);
ScmObj ScmOp_reverse(ScmObj lst);
ScmObj ScmOp_list_tail(ScmObj lst, ScmObj scm_k);
ScmObj ScmOp_list_ref(ScmObj lst, ScmObj scm_k);
ScmObj ScmOp_memq(ScmObj obj, ScmObj lst);
ScmObj ScmOp_memv(ScmObj obj, ScmObj lst);
ScmObj ScmOp_member(ScmObj obj, ScmObj lst);
ScmObj ScmOp_assq(ScmObj obj, ScmObj alist);
ScmObj ScmOp_assv(ScmObj obj, ScmObj alist);
ScmObj ScmOp_assoc(ScmObj obj, ScmObj alist);
ScmObj ScmOp_symbolp(ScmObj obj);
ScmObj ScmOp_symbol2string(ScmObj obj);
ScmObj ScmOp_string2symbol(ScmObj str);

ScmObj ScmOp_charp(ScmObj obj);
ScmObj ScmOp_char_equal(ScmObj ch1, ScmObj ch2);
/* TODO : many comparing functions around char is unimplemented */
ScmObj ScmOp_char_alphabeticp(ScmObj obj);
ScmObj ScmOp_char_numericp(ScmObj obj);
ScmObj ScmOp_char_whitespacep(ScmObj obj);
ScmObj ScmOp_char_upper_casep(ScmObj obj);
ScmObj ScmOp_char_lower_casep(ScmObj obj);
ScmObj ScmOp_char_upcase(ScmObj obj);
ScmObj ScmOp_char_downcase(ScmObj obj);

ScmObj ScmOp_stringp(ScmObj obj);
ScmObj ScmOp_make_string(ScmObj arg, ScmObj env);
ScmObj ScmOp_string(ScmObj arg, ScmObj env);
ScmObj ScmOp_string_length(ScmObj str);
ScmObj ScmOp_string_ref(ScmObj str, ScmObj k);
ScmObj ScmOp_string_set(ScmObj str, ScmObj k, ScmObj ch);
ScmObj ScmOp_string_equal(ScmObj str1, ScmObj str2);
/* TODO : many comparing functions around string is unimplemented */
ScmObj ScmOp_string_substring(ScmObj str, ScmObj start, ScmObj end);
ScmObj ScmOp_string_append(ScmObj arg, ScmObj env);
ScmObj ScmOp_string2list(ScmObj string);
ScmObj ScmOp_list2string(ScmObj lst);
ScmObj ScmOp_string_copy(ScmObj string);
ScmObj ScmOp_string_fill(ScmObj string, ScmObj ch);
ScmObj ScmOp_vectorp(ScmObj obj);
ScmObj ScmOp_make_vector(ScmObj arg, ScmObj env );
ScmObj ScmOp_vector(ScmObj arg, ScmObj env );
ScmObj ScmOp_vector_length(ScmObj vec);
ScmObj ScmOp_vector_ref(ScmObj vec, ScmObj scm_k);
ScmObj ScmOp_vector_set(ScmObj vec, ScmObj scm_k, ScmObj obj);
ScmObj ScmOp_vector2list(ScmObj vec);
ScmObj ScmOp_list2vector(ScmObj lst);
ScmObj ScmOp_vector_fill(ScmObj vec, ScmObj fill);
ScmObj ScmOp_procedurep(ScmObj obj);
ScmObj ScmOp_map(ScmObj map_arg, ScmObj env);
ScmObj ScmOp_for_each(ScmObj arg, ScmObj env);
ScmObj ScmOp_force(ScmObj arg, ScmObj env);
ScmObj ScmOp_call_with_current_continuation(ScmObj arg, ScmObj env);
ScmObj ScmOp_values(ScmObj argl, ScmObj env);
ScmObj ScmOp_call_with_values(ScmObj producer, ScmObj consumer);

/* io.c */
void   SigScm_set_lib_path(const char *path);

ScmObj ScmOp_call_with_input_file(ScmObj filepath, ScmObj proc);
ScmObj ScmOp_call_with_output_file(ScmObj filepath, ScmObj proc);
ScmObj ScmOp_input_portp(ScmObj obj);
ScmObj ScmOp_output_portp(ScmObj obj);
ScmObj ScmOp_current_input_port(void);
ScmObj ScmOp_current_output_port(void);
ScmObj ScmOp_with_input_from_file(ScmObj filepath, ScmObj thunk);
ScmObj ScmOp_with_output_to_file(ScmObj filepath, ScmObj thunk);
ScmObj ScmOp_open_input_file(ScmObj filepath);
ScmObj ScmOp_open_output_file(ScmObj filepath);
ScmObj ScmOp_close_input_port(ScmObj port);
ScmObj ScmOp_close_output_port(ScmObj port);

ScmObj ScmOp_read(ScmObj arg, ScmObj env);
ScmObj ScmOp_read_char(ScmObj arg, ScmObj env);
ScmObj ScmOp_peek_char(ScmObj arg, ScmObj env);
ScmObj ScmOp_eof_objectp(ScmObj obj);
ScmObj ScmOp_char_readyp(ScmObj arg, ScmObj env);
ScmObj ScmOp_write(ScmObj arg, ScmObj env);
ScmObj ScmOp_display(ScmObj arg, ScmObj env);
ScmObj ScmOp_newline(ScmObj arg, ScmObj env);
ScmObj ScmOp_write_char(ScmObj arg, ScmObj env);

ScmObj SigScm_load(const char *c_filename);
ScmObj ScmOp_load(ScmObj filename);
#if SCM_USE_NONSTD_FEATURES
/* FIXME: add ScmObj SigScm_require(const char *c_filename); */
ScmObj ScmOp_require(ScmObj filename);
ScmObj ScmOp_provide(ScmObj feature);
ScmObj ScmOp_providedp(ScmObj feature);
ScmObj ScmOp_file_existsp(ScmObj filepath);
ScmObj ScmOp_delete_file(ScmObj filepath);
#endif

/* encoding.c */
int SigScm_default_encoding_strlen(const char *str);
const char* SigScm_default_encoding_str_startpos(const char *str, int k);
const char* SigScm_default_encoding_str_endpos(const char *str, int k);

/* read.c */
ScmObj SigScm_Read(ScmObj port);
ScmObj SigScm_Read_Char(ScmObj port);

/* error.c */
void SigScm_Error(const char *msg, ...);
void SigScm_ErrorObj(const char *msg, ScmObj obj);
void SigScm_ShowBacktrace(void);

/* debug.c */
void SigScm_Display(ScmObj obj);
void SigScm_WriteToPort(ScmObj port, ScmObj obj);
void SigScm_DisplayToPort(ScmObj port, ScmObj obj);
#if SCM_USE_SRFI38
void SigScm_WriteToPortWithSharedStructure(ScmObj port, ScmObj obj);
#endif


/*===========================================================================
   SigScheme : Optional Funtions
===========================================================================*/
#if SCM_USE_SRFI1
/* operations-srfi1.c */
ScmObj ScmOp_SRFI1_xcons(ScmObj a, ScmObj b);
ScmObj ScmOp_SRFI1_cons_star(ScmObj obj, ScmObj env);
ScmObj ScmOp_SRFI1_make_list(ScmObj obj, ScmObj env);
ScmObj ScmOp_SRFI1_list_tabulate(ScmObj arg, ScmObj env);
ScmObj ScmOp_SRFI1_list_copy(ScmObj lst);
ScmObj ScmOp_SRFI1_circular_list(ScmObj lst, ScmObj env);
ScmObj ScmOp_SRFI1_iota(ScmObj args, ScmObj env);
ScmObj ScmOp_SRFI1_proper_listp(ScmObj lst);
ScmObj ScmOp_SRFI1_circular_listp(ScmObj lst);
ScmObj ScmOp_SRFI1_dotted_listp(ScmObj lst);
ScmObj ScmOp_SRFI1_not_pairp(ScmObj pair);
ScmObj ScmOp_SRFI1_null_listp(ScmObj lst);
ScmObj ScmOp_SRFI1_listequal(ScmObj args, ScmObj env);
ScmObj ScmOp_SRFI1_first(ScmObj lst);
ScmObj ScmOp_SRFI1_second(ScmObj lst);
ScmObj ScmOp_SRFI1_third(ScmObj lst);
ScmObj ScmOp_SRFI1_fourth(ScmObj lst);
ScmObj ScmOp_SRFI1_fifth(ScmObj lst);
ScmObj ScmOp_SRFI1_sixth(ScmObj lst);
ScmObj ScmOp_SRFI1_seventh(ScmObj lst);
ScmObj ScmOp_SRFI1_eighth(ScmObj lst);
ScmObj ScmOp_SRFI1_ninth(ScmObj lst);
ScmObj ScmOp_SRFI1_tenth(ScmObj lst);
ScmObj ScmOp_SRFI1_carpluscdr(ScmObj lst);
ScmObj ScmOp_SRFI1_take(ScmObj lst, ScmObj scm_idx);
ScmObj ScmOp_SRFI1_drop(ScmObj lst, ScmObj scm_idx);
ScmObj ScmOp_SRFI1_take_right(ScmObj lst, ScmObj scm_elem);
ScmObj ScmOp_SRFI1_drop_right(ScmObj lst, ScmObj scm_elem);
ScmObj ScmOp_SRFI1_take_d(ScmObj lst, ScmObj scm_idx);
ScmObj ScmOp_SRFI1_drop_right_d(ScmObj lst, ScmObj scm_idx);
ScmObj ScmOp_SRFI1_split_at(ScmObj lst, ScmObj idx);
ScmObj ScmOp_SRFI1_split_at_d(ScmObj lst, ScmObj idx);
ScmObj ScmOp_SRFI1_last(ScmObj lst);
ScmObj ScmOp_SRFI1_last_pair(ScmObj lst);
ScmObj ScmOp_SRFI1_lengthplus(ScmObj lst);
ScmObj ScmOp_SRFI1_concatenate(ScmObj args, ScmObj env);
#endif
#if SCM_USE_SRFI2
ScmObj ScmOp_SRFI2_and_let_star(ScmObj args, ScmEvalState *eval_state);
#endif
#if SCM_USE_SRFI8
/* operations-srfi8.c */
ScmObj ScmOp_SRFI8_receive(ScmObj formals, ScmObj expr, ScmObj body, ScmEvalState *eval_state);
#endif
#if SCM_USE_SRFI23
/* operations-srfi23.c */
ScmObj ScmOp_SRFI23_error(ScmObj args, ScmObj env);
#endif
#if SCM_USE_SRFI38
/* operations-srfi38.c */
ScmObj ScmOp_SRFI38_write_with_shared_structure(ScmObj arg, ScmObj env);
#endif
#if SCM_USE_SRFI60
/* operations-srfi60.c */
ScmObj ScmOp_SRFI60_logand(ScmObj left, ScmObj right,
                           enum ScmReductionState *state);
ScmObj ScmOp_SRFI60_logior(ScmObj left, ScmObj right,
                           enum ScmReductionState *state);
ScmObj ScmOp_SRFI60_logxor(ScmObj left, ScmObj right,
                           enum ScmReductionState *state);
ScmObj ScmOp_SRFI60_lognot(ScmObj n);
ScmObj ScmOp_SRFI60_bitwise_if(ScmObj mask, ScmObj n0, ScmObj n1);
ScmObj ScmOp_SRFI60_logtest(ScmObj j, ScmObj k);
#endif
#if SCM_COMPAT_SIOD
/* operations-siod.c */
ScmObj ScmOp_symbol_boundp(ScmObj obj);
ScmObj ScmOp_symbol_value(ScmObj var);
ScmObj ScmOp_set_symbol_value(ScmObj var, ScmObj val);
ScmObj ScmOp_siod_eql(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_the_environment(ScmObj arg, ScmObj env);
ScmObj ScmOp_closure_code(ScmObj closure);
ScmObj ScmOp_verbose(ScmObj args, ScmObj env);
long   SigScm_GetVerboseLevel(void);
void   SigScm_SetVerboseLevel(long level);
#endif

#ifdef __cplusplus
}
#endif

#endif /* __SIGSCHEME_H */
