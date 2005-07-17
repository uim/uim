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
#include "sigschemetype.h"

/*=======================================
   Variable Declarations
=======================================*/
extern ScmObj *stack_start_pointer;

extern ScmObj current_input_port;
extern ScmObj current_output_port;

/*=======================================
   Macro Declarations
=======================================*/
#define DEBUG_PARSER  0
#define DEBUG_GC      0
#define USE_EUCJP     1

#define CHECK_1_ARG(arg) \
    (SCM_NULLP(arg))

#define CHECK_2_ARGS(arg) \
    (SCM_NULLP(arg) || SCM_NULLP(SCM_CDR(arg)))

#define CHECK_3_ARGS(arg) \
    (SCM_NULLP(arg) || SCM_NULLP(SCM_CDR(arg)) || SCM_NULLP(SCM_CDR(SCM_CDR(arg))))

#define CHECK_4_ARGS(arg) \
    (SCM_NULLP(arg) || SCM_NULLP(SCM_CDR(arg)) || SCM_NULLP(SCM_CDR(SCM_CDR(arg))) \
     || SCM_NULLP(SCM_CDR(SCM_CDR(SCM_CDR(arg)))))

#define CHECK_5_ARGS(arg) \
    (SCM_NULLP(arg) || SCM_NULLP(SCM_CDR(arg)) || SCM_NULLP(SCM_CDR(SCM_CDR(arg))) \
     || SCM_NULLP(SCM_CDR(SCM_CDR(SCM_CDR(arg)))) || SCM_NULLP(SCM_CDR(SCM_CDR(SCM_CDR(SCM_CDR(arg))))))

int SigScm_Die(const char *msg, const char *filename, int line); /* error.c */
#define sigassert(cond) \
    (cond ? 0 : SigScm_Die("assertion failed.", __FILE__, __LINE__))

/*=======================================
   Function Declarations
=======================================*/
/* sigscheme.c */
void SigScm_Initialize(void);
void SigScm_Finalize(void);
void Scm_InitSubr0(char *name, ScmObj (*func) (void));
void Scm_InitSubr1(char *name, ScmObj (*func) (ScmObj));
void Scm_InitSubr2(char *name, ScmObj (*func) (ScmObj, ScmObj));
void Scm_InitSubr3(char *name, ScmObj (*func) (ScmObj, ScmObj, ScmObj));
void Scm_InitSubr4(char *name, ScmObj (*func) (ScmObj, ScmObj, ScmObj, ScmObj));
void Scm_InitSubr5(char *name, ScmObj (*func) (ScmObj, ScmObj, ScmObj, ScmObj, ScmObj));
void Scm_InitSubrL(char *name, ScmObj (*func) (ScmObj, ScmObj env));
void Scm_InitSubrR(char *name, ScmObj (*func) (ScmObj, ScmObj env));
void Scm_InitSubr2N(char *name, ScmObj (*func) (ScmObj, ScmObj));

/* datas.c */
void   SigScm_InitStorage(void);
void   SigScm_FinalizeStorage(void);
ScmObj Scm_NewCons(ScmObj a, ScmObj b);
ScmObj Scm_NewInt(int val);
ScmObj Scm_NewSymbol(char *name, ScmObj v_cell);
ScmObj Scm_NewChar(char *ch);
ScmObj Scm_NewString(char *str);
ScmObj Scm_NewString_With_StrLen(char *str, int len);
ScmObj Scm_NewFunc(enum ScmFuncArgNum num_arg, ScmFuncType func);
ScmObj Scm_NewClosure(ScmObj exp, ScmObj env);
ScmObj Scm_NewVector(ScmObj *vec, ScmObj len);
ScmObj Scm_NewPort(FILE *file, enum ScmPortType ptype);
ScmObj Scm_Intern(const char *name);

/* eval.c */
ScmObj ScmOp_eval(ScmObj obj, ScmObj env);
ScmObj ScmOp_apply(ScmObj arg, ScmObj env);
ScmObj ScmOp_quote(ScmObj obj);
ScmObj ScmExp_lambda(ScmObj exp, ScmObj env);
ScmObj ScmExp_if(ScmObj exp, ScmObj env);
ScmObj ScmExp_set(ScmObj arg, ScmObj env);
ScmObj ScmExp_cond(ScmObj arg, ScmObj env);
ScmObj ScmExp_case(ScmObj arg, ScmObj env);
ScmObj ScmExp_and(ScmObj arg, ScmObj env);
ScmObj ScmExp_or(ScmObj arg, ScmObj env);
ScmObj ScmExp_let(ScmObj arg, ScmObj env);
ScmObj ScmExp_begin(ScmObj arg, ScmObj env);
ScmObj ScmOp_delay(ScmObj arg, ScmObj env);
ScmObj ScmOp_quasiquote(ScmObj temp);
ScmObj ScmOp_unquote(ScmObj exp);
ScmObj ScmOp_unquote_splicint(ScmObj exp);
ScmObj ScmExp_define(ScmObj arg, ScmObj env);
ScmObj ScmOp_scheme_report_environment(ScmObj version);
ScmObj ScmOp_null_environment(ScmObj version);


/* operations.c */
ScmObj ScmOp_eqvp(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_eqp(ScmObj Obj1, ScmObj obj2);
ScmObj ScmOp_numberp(ScmObj obj);
ScmObj ScmOp_equal(ScmObj list, ScmObj env);
ScmObj ScmOp_bigger(ScmObj list, ScmObj env);
ScmObj ScmOp_smaller(ScmObj list, ScmObj env);
ScmObj ScmOp_biggerEq(ScmObj list, ScmObj env);
ScmObj ScmOp_smallerEq(ScmObj list, ScmObj env);
ScmObj ScmOp_zerop(ScmObj num);
ScmObj ScmOp_positivep(ScmObj num);
ScmObj ScmOp_negativep(ScmObj num);
ScmObj ScmOp_oddp(ScmObj num);
ScmObj ScmOp_evenp(ScmObj num);
ScmObj ScmOp_max(ScmObj list, ScmObj env);
ScmObj ScmOp_min(ScmObj list, ScmObj env);
ScmObj ScmOp_plus2n(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_minus2n(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_multi2n(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_divide2n(ScmObj obj1, ScmObj obj2);
ScmObj ScmOp_abs(ScmObj num);
ScmObj ScmOp_quotient(ScmObj n1, ScmObj n2);
ScmObj ScmOp_modulo(ScmObj n1, ScmObj n2);
ScmObj ScmOp_reminder(ScmObj n1, ScmObj n2);
ScmObj ScmOp_not(ScmObj obj);
ScmObj ScmOp_booleanp(ScmObj obj);
ScmObj ScmOp_pairp(ScmObj obj);
ScmObj ScmOp_cons(ScmObj car, ScmObj cdr);
ScmObj ScmOp_car(ScmObj pair);
ScmObj ScmOp_cdr(ScmObj pair);
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
ScmObj ScmOp_nullp(ScmObj obj);
ScmObj ScmOp_listp(ScmObj obj);
ScmObj ScmOp_list(ScmObj obj, ScmObj env);
ScmObj ScmOp_length(ScmObj obj);
ScmObj ScmOp_append(ScmObj start, ScmObj item);
ScmObj ScmOp_reverse(ScmObj obj);
ScmObj ScmOp_listtail(ScmObj list, ScmObj k);
ScmObj ScmOp_listref(ScmObj list, ScmObj k);
ScmObj ScmOp_memq(ScmObj obj, ScmObj list);
ScmObj ScmOp_memv(ScmObj obj, ScmObj list);
ScmObj ScmOp_assq(ScmObj obj, ScmObj alist);
ScmObj ScmOp_assv(ScmObj obj, ScmObj alist);
ScmObj ScmOp_symbolp(ScmObj obj);
ScmObj ScmOp_boundp(ScmObj obj);
ScmObj ScmOp_symbol_to_string(ScmObj obj);
ScmObj ScmOp_string_to_symbol(ScmObj str);

ScmObj ScmOp_charp(ScmObj obj);
ScmObj ScmOp_char_equal(ScmObj ch1, ScmObj ch2);
/* TODO : many comparing functions around char is unimplemented */
ScmObj ScmOp_char_alphabeticp(ScmObj obj);
ScmObj ScmOp_char_numericp(ScmObj obj);
ScmObj ScmOp_char_whitespacep(ScmObj obj);
ScmObj ScmOp_char_upper_casep(ScmObj obj);
ScmObj ScmOp_char_lower_casep(ScmObj obj);

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
ScmObj ScmOp_string_to_list(ScmObj string);
ScmObj ScmOp_list_to_string(ScmObj list);
ScmObj ScmOp_string_copy(ScmObj string);
ScmObj ScmOp_string_fill(ScmObj string, ScmObj ch);

ScmObj ScmOp_vectorp(ScmObj vector);
ScmObj ScmOp_make_vector(ScmObj obj, ScmObj env);
ScmObj ScmOp_vector(ScmObj obj, ScmObj env);
ScmObj ScmOp_vector_length(ScmObj vector);
ScmObj ScmOp_vector_ref(ScmObj vec, ScmObj k);
ScmObj ScmOp_vector_set(ScmObj vec, ScmObj k, ScmObj obj);
ScmObj ScmOp_vector_to_list(ScmObj vec);
ScmObj ScmOp_list_to_vector(ScmObj list);
ScmObj ScmOp_vector_fill(ScmObj vec, ScmObj fill);
ScmObj ScmOp_procedurep(ScmObj obj);
ScmObj ScmOp_map(ScmObj arg, ScmObj env);
ScmObj ScmOp_for_each(ScmObj arg, ScmObj env);
ScmObj ScmOp_force(ScmObj arg, ScmObj env);

/* io.c */
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

ScmObj SigScm_load(char *c_filename);
ScmObj ScmOp_load(ScmObj filename);

/* encoding.c */
int SigScm_default_encoding_strlen(const char *str);
const char* SigScm_default_encoding_str_startpos(const char *str, int k);
const char* SigScm_default_encoding_str_endpos(const char *str, int k);

/* read.c */
ScmObj SigScm_Read(ScmObj port);
ScmObj SigScm_Read_Char(ScmObj port);

/* error.c */
void SigScm_Error(const char *msg);

/* debug.c */
void SigScm_Display(ScmObj obj);
void SigScm_DisplayToPort(ScmObj port, ScmObj obj);


#endif /* __SIGSCHEME_H */
