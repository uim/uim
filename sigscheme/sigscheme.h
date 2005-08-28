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

/*=======================================
   Struct Declarations
=======================================*/
typedef void (*C_FUNC) (void);

/* type declaration */
#include "sigschemetype.h"

/*=======================================
   Variable Declarations
=======================================*/

/*=======================================
   Macro Declarations
=======================================*/
#define SCM_USE_EUCJP           1  /* use EUC-JP as internal encoding */
#define SCM_USE_SRFI1           0  /* use SRFI-1 procedures writtein in C */
#define SCM_USE_SRFI8           1  /* use SRFI-8 receive procedure writtein in C */
#define SCM_USE_NONSTD_FEATURES 1  /* use Non-R5RS standard features */
#define SCM_COMPAT_SIOD         1  /* use SIOD compatible features */
#define SCM_COMPAT_SIOD_BUGS    1  /* emulate the buggy behaviors of SIOD */
#define SCM_STRICT_R5RS         0  /* use strict R5RS check */
#define SCM_STRICT_ARGCHECK     0  /* enable strict argument check */

int SigScm_Die(const char *msg, const char *filename, int line); /* error.c */
#define sigassert(cond) \
    (cond ? 0 : SigScm_Die("assertion failed.", __FILE__, __LINE__))

/*=======================================
   Function Declarations
=======================================*/
/* sigscheme.c */
void SigScm_Initialize(void);
void SigScm_Finalize(void);
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

/* datas.c */
void   SigScm_InitStorage(void);
void   SigScm_FinalizeStorage(void);
void   SigScm_GC_Protect(ScmObj obj);
void   SigScm_GC_ProtectStack(ScmObj *stack_start);
void   SigScm_GC_UnprotectStack(ScmObj *stack_start);
ScmObj Scm_NewCons(ScmObj a, ScmObj b);
ScmObj Scm_NewInt(int val);
ScmObj Scm_NewSymbol(char *name, ScmObj v_cell);
ScmObj Scm_NewChar(char *ch);
ScmObj Scm_NewString(char *str);
ScmObj Scm_NewStringCopying(const char *str);
ScmObj Scm_NewString_With_StrLen(char *str, int len);
ScmObj Scm_NewFunc(enum ScmFuncTypeCode type, ScmFuncType func);
ScmObj Scm_NewClosure(ScmObj exp, ScmObj env);
ScmObj Scm_NewVector(ScmObj *vec, int len);
ScmObj Scm_NewFilePort(FILE *file, const char *filename, enum ScmPortDirection pdireciton);
ScmObj Scm_NewStringPort(const char *str);  /* input only? */
ScmObj Scm_NewContinuation(void);
ScmObj Scm_NewValuePacket(ScmObj values);
#if SCM_USE_NONSTD_FEATURES
ScmObj Scm_NewCPointer(void *data);
ScmObj Scm_NewCFuncPointer(C_FUNC func);
#endif
ScmObj Scm_Intern(const char *name);
int    Scm_GetInt(ScmObj num);
char*  Scm_GetString(ScmObj str);
#if SCM_USE_NONSTD_FEATURES
void*  Scm_GetCPointer(ScmObj c_ptr);
C_FUNC Scm_GetCFuncPointer(ScmObj c_funcptr);
#endif
ScmObj Scm_eval_c_string(const char *exp);
#if SCM_COMPAT_SIOD
ScmObj Scm_return_value(void);
#endif

/* eval.c */
ScmObj ScmOp_eval(ScmObj obj, ScmObj env);
ScmObj ScmOp_apply(ScmObj args, ScmObj env);
ScmObj ScmOp_quote(ScmObj arglist, ScmObj envp);
ScmObj ScmExp_lambda(ScmObj exp, ScmObj env);
ScmObj ScmExp_if(ScmObj exp, ScmObj *envp);
ScmObj ScmExp_set(ScmObj arg, ScmObj env);
ScmObj ScmExp_cond(ScmObj arg, ScmObj *envp);
ScmObj ScmExp_case(ScmObj arg, ScmObj *envp);
ScmObj ScmExp_and(ScmObj arg, ScmObj *envp, int *tail_flag);
ScmObj ScmExp_or(ScmObj arg, ScmObj *envp, int *tail_flag);
ScmObj ScmExp_let(ScmObj arg, ScmObj *envp);
ScmObj ScmExp_let_star(ScmObj arg, ScmObj *envp);
ScmObj ScmExp_letrec(ScmObj arg, ScmObj *envp);
ScmObj ScmExp_begin(ScmObj arg, ScmObj *envp);
ScmObj ScmExp_do(ScmObj arg, ScmObj *envp);
ScmObj ScmOp_delay(ScmObj arg, ScmObj env);
ScmObj ScmOp_quasiquote(ScmObj obj, ScmObj env);
ScmObj ScmOp_unquote(ScmObj obj, ScmObj env);
ScmObj ScmOp_unquote_splicing(ScmObj obj, ScmObj env);
ScmObj ScmExp_define(ScmObj arg, ScmObj env);
ScmObj ScmOp_scheme_report_environment(ScmObj version);
ScmObj ScmOp_null_environment(ScmObj version);

/* operations.c */
ScmObj ScmOp_eqvp(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_eqp(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_equalp(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_plus(ScmObj args, ScmObj env);
ScmObj ScmOp_times(ScmObj args, ScmObj env);
ScmObj ScmOp_minus(ScmObj args, ScmObj env);
ScmObj ScmOp_divide(ScmObj args, ScmObj env);
ScmObj ScmOp_numberp(ScmObj obj);
ScmObj ScmOp_equal(ScmObj args, ScmObj env);
ScmObj ScmOp_less(ScmObj args, ScmObj env );
ScmObj ScmOp_greater(ScmObj args, ScmObj env );
ScmObj ScmOp_lessEq(ScmObj args, ScmObj env );
ScmObj ScmOp_greaterEq(ScmObj args, ScmObj env );
ScmObj ScmOp_zerop(ScmObj scm_num);
ScmObj ScmOp_positivep(ScmObj scm_num);
ScmObj ScmOp_negativep(ScmObj scm_num);
ScmObj ScmOp_oddp(ScmObj scm_num);
ScmObj ScmOp_evenp(ScmObj scm_num);
ScmObj ScmOp_max(ScmObj args, ScmObj env );
ScmObj ScmOp_min(ScmObj args, ScmObj env );
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
ScmObj ScmOp_caar(ScmObj pair);
ScmObj ScmOp_cadr(ScmObj pair);
ScmObj ScmOp_cdar(ScmObj pair);
ScmObj ScmOp_cddr(ScmObj pair);
ScmObj ScmOp_caaar(ScmObj pair);
ScmObj ScmOp_caadr(ScmObj pair);
ScmObj ScmOp_cadar(ScmObj pair);
ScmObj ScmOp_caddr(ScmObj pair);
ScmObj ScmOp_cdaar(ScmObj pair);
ScmObj ScmOp_cdadr(ScmObj pair);
ScmObj ScmOp_cddar(ScmObj pair);
ScmObj ScmOp_cdddr(ScmObj pair);
ScmObj ScmOp_caaaar(ScmObj pair);
ScmObj ScmOp_caaadr(ScmObj pair);
ScmObj ScmOp_caadar(ScmObj pair);
ScmObj ScmOp_caaddr(ScmObj pair);
ScmObj ScmOp_cadaar(ScmObj pair);
ScmObj ScmOp_cadadr(ScmObj pair);
ScmObj ScmOp_caddar(ScmObj pair);
ScmObj ScmOp_cadddr(ScmObj pair);
ScmObj ScmOp_cdaaar(ScmObj pair);
ScmObj ScmOp_cdaadr(ScmObj pair);
ScmObj ScmOp_cdadar(ScmObj pair);
ScmObj ScmOp_cdaddr(ScmObj pair);
ScmObj ScmOp_cddaar(ScmObj pair);
ScmObj ScmOp_cddadr(ScmObj pair);
ScmObj ScmOp_cdddar(ScmObj pair);
ScmObj ScmOp_cddddr(ScmObj pair);
ScmObj ScmOp_list(ScmObj obj, ScmObj env);
ScmObj ScmOp_nullp(ScmObj obj);
ScmObj ScmOp_listp(ScmObj obj);
ScmObj ScmOp_length(ScmObj obj);
ScmObj ScmOp_append(ScmObj args, ScmObj env);
ScmObj ScmOp_reverse(ScmObj list);
ScmObj ScmOp_list_tail(ScmObj list, ScmObj scm_k);
ScmObj ScmOp_list_ref(ScmObj list, ScmObj scm_k);
ScmObj ScmOp_memq(ScmObj obj, ScmObj list);
ScmObj ScmOp_memv(ScmObj obj, ScmObj list);
ScmObj ScmOp_member(ScmObj obj, ScmObj list);
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
ScmObj ScmOp_list2string(ScmObj list);
ScmObj ScmOp_string_copy(ScmObj string);
ScmObj ScmOp_string_fill(ScmObj string, ScmObj ch);
ScmObj ScmOp_vectorp(ScmObj obj);
ScmObj ScmOp_make_vector(ScmObj arg, ScmObj env );
ScmObj ScmOp_vector(ScmObj arg, ScmObj env );
ScmObj ScmOp_vector_length(ScmObj vec);
ScmObj ScmOp_vector_ref(ScmObj vec, ScmObj scm_k);
ScmObj ScmOp_vector_set(ScmObj vec, ScmObj scm_k, ScmObj obj);
ScmObj ScmOp_vector2list(ScmObj vec);
ScmObj ScmOp_list2vector(ScmObj list);
ScmObj ScmOp_vector_fill(ScmObj vec, ScmObj fill);
ScmObj ScmOp_procedurep(ScmObj obj);
ScmObj ScmOp_map(ScmObj map_arg, ScmObj env);
ScmObj ScmOp_for_each(ScmObj arg, ScmObj env);
ScmObj ScmOp_force(ScmObj arg, ScmObj env);
ScmObj ScmOp_call_with_current_continuation(ScmObj arg, ScmObj env);
ScmObj ScmOp_values(ScmObj argl, ScmObj env);
ScmObj ScmOp_call_with_values(ScmObj argl, ScmObj *envp);

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

#if SCM_USE_SRFI1
/* operations-srfi1.c */
ScmObj ScmOp_SRFI1_xcons(ScmObj a, ScmObj b);
ScmObj ScmOp_SRFI1_cons_star(ScmObj obj, ScmObj env);
ScmObj ScmOp_SRFI1_make_list(ScmObj obj, ScmObj env);
ScmObj ScmOp_SRFI1_list_tabulate(ScmObj arg, ScmObj env);
ScmObj ScmOp_SRFI1_list_copy(ScmObj list);
ScmObj ScmOp_SRFI1_circular_list(ScmObj list, ScmObj env);
ScmObj ScmOp_SRFI1_iota(ScmObj args, ScmObj env);
#endif
#if SCM_USE_SRFI8
/* operations-srfi8.c */
ScmObj ScmOp_SRFI8_receive(ScmObj args, ScmObj *envp);
#endif
#if SCM_COMPAT_SIOD
/* operations-siod.c */
ScmObj ScmOp_symbol_boundp(ScmObj obj);
ScmObj ScmOp_symbol_value(ScmObj var);
ScmObj ScmOp_set_symbol_value(ScmObj var, ScmObj val);
ScmObj ScmOp_bit_and(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_bit_or(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_bit_xor(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_bit_not(ScmObj obj);
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
