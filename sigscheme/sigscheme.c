/*===========================================================================
 *  FileName : sigscheme.c
 *  About    : initialization and finalization
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
/*=======================================
  System Include
=======================================*/

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"

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
static void Scm_InitSubr(char *name, enum ScmFuncArgNum argnum, ScmFuncType func);

ScmObj SigScm_nil, SigScm_true, SigScm_false, SigScm_eof;
ScmObj SigScm_quote, SigScm_quasiquote, SigScm_unquote, SigScm_unquote_splicing;
ScmObj SigScm_unbound, SigScm_unspecified, SigScm_undef;
ScmObjInternal SigScm_nil_impl, SigScm_true_impl, SigScm_false_impl, SigScm_eof_impl;
ScmObjInternal SigScm_quote_impl, SigScm_quasiquote_impl, SigScm_unquote_impl, SigScm_unquote_splicing_impl;
ScmObjInternal SigScm_unbound_impl, SigScm_unspecified_impl, SigScm_undef_impl;

/*=======================================
  Function Implementations
=======================================*/
void SigScm_Initialize(void)
{
    ScmObj obj;
    stack_start_pointer = &obj;

    /*=======================================================================
      Etc Variable Initialization
    =======================================================================*/
    SCM_NEW_ETC(SigScm_nil,              SigScm_nil_impl,              1);
    SCM_NEW_ETC(SigScm_true,             SigScm_true_impl,             2);
    SCM_NEW_ETC(SigScm_false,            SigScm_false_impl,            3);
    SCM_NEW_ETC(SigScm_eof,              SigScm_eof_impl,              4);
    SCM_NEW_ETC(SigScm_quote,            SigScm_quote_impl,            5);
    SCM_NEW_ETC(SigScm_quasiquote,       SigScm_quasiquote_impl,       6);
    SCM_NEW_ETC(SigScm_unquote,          SigScm_unquote_impl,          7);
    SCM_NEW_ETC(SigScm_unquote_splicing, SigScm_unquote_splicing_impl, 8);
    SCM_NEW_ETC(SigScm_unbound,          SigScm_unbound_impl,          9);
    SCM_NEW_ETC(SigScm_unspecified,      SigScm_unspecified_impl,      10);
    SCM_NEW_ETC(SigScm_undef,            SigScm_undef_impl,            11);
    /*=======================================================================
      Storage Initialization
    =======================================================================*/
    SigScm_InitStorage();
    /*=======================================================================
      Export Scheme Special Symbols
    =======================================================================*/
    SCM_SYMBOL_VCELL(Scm_Intern("#t"))   = SCM_TRUE;
    SCM_SYMBOL_VCELL(Scm_Intern("#f"))   = SCM_FALSE;
    SCM_SYMBOL_VCELL(Scm_Intern("else")) = SCM_TRUE;

    /*=======================================================================
      Export Scheme Functions
    =======================================================================*/
    /* eval.c */
    Scm_InitSubr2("eval"                 , ScmOp_eval);
    Scm_InitSubrL("apply"                , ScmOp_apply);
    Scm_InitSubrR("lambda"               , ScmExp_lambda);
    Scm_InitSubrR("if"                   , ScmExp_if);
    Scm_InitSubrR("set!"                 , ScmExp_set);
    Scm_InitSubrR("cond"                 , ScmExp_cond);
    Scm_InitSubrR("case"                 , ScmExp_case);
    Scm_InitSubrR("and"                  , ScmExp_and);
    Scm_InitSubrR("or"                   , ScmExp_or);
    Scm_InitSubrR("let"                  , ScmExp_let);
    Scm_InitSubrR("let*"                 , ScmExp_let_star);
    Scm_InitSubrR("letrec"               , ScmExp_let);
    Scm_InitSubrR("begin"                , ScmExp_begin);
    Scm_InitSubrR("do"                   , ScmExp_do);
    Scm_InitSubrR("delay"                , ScmOp_delay);
    Scm_InitSubrR("define"               , ScmExp_define);
    Scm_InitSubr1("scheme-report-environment", ScmOp_scheme_report_environment);
    Scm_InitSubr1("null-environment"         , ScmOp_null_environment);
    /* operations.c */
    Scm_InitSubr1("quote"                , ScmOp_quote);
    Scm_InitSubr2("eqv?"                 , ScmOp_eqvp);
    Scm_InitSubr2("eq?"                  , ScmOp_eqp);
    Scm_InitSubr2("equal?"               , ScmOp_equalp);
    Scm_InitSubr1("number?"              , ScmOp_numberp);
    Scm_InitSubrL("="                    , ScmOp_equal);
    Scm_InitSubrL("<"                    , ScmOp_bigger);
    Scm_InitSubrL(">"                    , ScmOp_smaller);
    Scm_InitSubrL("<="                   , ScmOp_biggerEq);
    Scm_InitSubrL(">="                   , ScmOp_smallerEq);
    Scm_InitSubr1("zero?"                , ScmOp_zerop);
    Scm_InitSubr1("positive?"            , ScmOp_positivep);
    Scm_InitSubr1("negative?"            , ScmOp_negativep);
    Scm_InitSubr1("odd?"                 , ScmOp_oddp);
    Scm_InitSubr1("even?"                , ScmOp_evenp);
    Scm_InitSubrL("max"                  , ScmOp_max);
    Scm_InitSubrL("min"                  , ScmOp_min);
    Scm_InitSubr2N("+"                   , ScmOp_plus2n);
    Scm_InitSubr2N("*"                   , ScmOp_multi2n);
    Scm_InitSubr2N("-"                   , ScmOp_minus2n);
    Scm_InitSubr2N("/"                   , ScmOp_divide2n);
    Scm_InitSubr1("abs"                  , ScmOp_abs);
    Scm_InitSubr2("quotient"             , ScmOp_quotient);
    Scm_InitSubr2("modulo"               , ScmOp_modulo);
    Scm_InitSubr2("remainder"            , ScmOp_remainder);
    Scm_InitSubr1("not"                  , ScmOp_not);
    Scm_InitSubr1("boolean?"             , ScmOp_booleanp);
    Scm_InitSubr1("pair?"                , ScmOp_pairp);
    Scm_InitSubr2("cons"                 , ScmOp_cons);
    Scm_InitSubr1("car"                  , ScmOp_car);
    Scm_InitSubr1("cdr"                  , ScmOp_cdr);
    Scm_InitSubr2("set-car!"             , ScmOp_setcar);
    Scm_InitSubr2("set-cdr!"             , ScmOp_setcdr);
    Scm_InitSubr1("caar"                 , ScmOp_caar);
    Scm_InitSubr1("cadr"                 , ScmOp_cadr);
    Scm_InitSubr1("cdar"                 , ScmOp_cdar);
    Scm_InitSubr1("cddr"                 , ScmOp_cddr);
    Scm_InitSubr1("caaar"                , ScmOp_caaar);
    Scm_InitSubr1("caadr"                , ScmOp_caadr);
    Scm_InitSubr1("cadar"                , ScmOp_cadar);
    Scm_InitSubr1("caddr"                , ScmOp_caddr);
    Scm_InitSubr1("cdaar"                , ScmOp_cdaar);
    Scm_InitSubr1("cdadr"                , ScmOp_cdadr);
    Scm_InitSubr1("cddar"                , ScmOp_cddar);
    Scm_InitSubr1("cdddr"                , ScmOp_cdddr);
    Scm_InitSubr1("caaaar"               , ScmOp_caaaar);
    Scm_InitSubr1("caaadr"               , ScmOp_caaadr);
    Scm_InitSubr1("caadar"               , ScmOp_caadar);
    Scm_InitSubr1("caaddr"               , ScmOp_caaddr);
    Scm_InitSubr1("cadaar"               , ScmOp_cadaar);
    Scm_InitSubr1("cadadr"               , ScmOp_cadadr);
    Scm_InitSubr1("caddar"               , ScmOp_caddar);
    Scm_InitSubr1("cadddr"               , ScmOp_cadddr);
    Scm_InitSubr1("cdaaar"               , ScmOp_cdaaar);
    Scm_InitSubr1("cdaadr"               , ScmOp_cdaadr);
    Scm_InitSubr1("cdadar"               , ScmOp_cdadar);
    Scm_InitSubr1("cdaddr"               , ScmOp_cdaddr);
    Scm_InitSubr1("cddaar"               , ScmOp_cddaar);
    Scm_InitSubr1("cddadr"               , ScmOp_cddadr);
    Scm_InitSubr1("cdddar"               , ScmOp_cdddar);
    Scm_InitSubr1("cddddr"               , ScmOp_cddddr);
    Scm_InitSubr1("null?"                , ScmOp_nullp);
    Scm_InitSubr1("list?"                , ScmOp_listp);
    Scm_InitSubrL("list"                 , ScmOp_list);
    Scm_InitSubr1("length"               , ScmOp_length);
    Scm_InitSubrL("append"               , ScmOp_append);
    Scm_InitSubr1("reverse"              , ScmOp_reverse);
    Scm_InitSubr2("list-tail"            , ScmOp_listtail);
    Scm_InitSubr2("list-ref"             , ScmOp_listref);
    Scm_InitSubr2("memq"                 , ScmOp_memq);
    Scm_InitSubr2("memv"                 , ScmOp_memv);
    Scm_InitSubr2("member"               , ScmOp_member);
    Scm_InitSubr2("assq"                 , ScmOp_assq);
    Scm_InitSubr2("assv"                 , ScmOp_assv);
    Scm_InitSubr2("assoc"                , ScmOp_assoc);
    Scm_InitSubr1("symbol?"              , ScmOp_symbolp);
    Scm_InitSubr1("symbol->string"       , ScmOp_symbol_to_string);
    Scm_InitSubr1("string->symbol"       , ScmOp_string_to_symbol);
    Scm_InitSubr1("char?"                , ScmOp_charp);
    Scm_InitSubr2("char=?"               , ScmOp_char_equal);
    Scm_InitSubr1("char-alphabetic?"     , ScmOp_char_alphabeticp);
    Scm_InitSubr1("char-numeric?"        , ScmOp_char_numericp);
    Scm_InitSubr1("char-whitespace?"     , ScmOp_char_whitespacep);
    Scm_InitSubr1("char-upper-case?"     , ScmOp_char_upper_casep);
    Scm_InitSubr1("char-lower-case?"     , ScmOp_char_lower_casep);
    Scm_InitSubr1("string?"              , ScmOp_stringp);
    Scm_InitSubrL("make-string"          , ScmOp_make_string);
    Scm_InitSubrL("string"               , ScmOp_string);
    Scm_InitSubr2("string-ref"           , ScmOp_string_ref);
    Scm_InitSubr3("string-set!"          , ScmOp_string_set);
    Scm_InitSubr1("string-length"        , ScmOp_string_length);
    Scm_InitSubr2("string=?"             , ScmOp_string_equal);
    Scm_InitSubr3("substring"            , ScmOp_string_substring);
    Scm_InitSubrL("string-append"        , ScmOp_string_append);
    Scm_InitSubr1("string->list"         , ScmOp_string_to_list);
    Scm_InitSubr1("list->string"         , ScmOp_list_to_string);
    Scm_InitSubr1("string-copy"          , ScmOp_string_copy);
    Scm_InitSubr2("string-fill!"         , ScmOp_string_fill);
    Scm_InitSubr1("vector?"              , ScmOp_vectorp);
    Scm_InitSubrL("make-vector"          , ScmOp_make_vector);
    Scm_InitSubrL("vector"               , ScmOp_vector);
    Scm_InitSubr1("vector-length"        , ScmOp_vector_length);
    Scm_InitSubr2("vector-ref"           , ScmOp_vector_ref);
    Scm_InitSubr3("vector-set!"          , ScmOp_vector_set);
    Scm_InitSubr1("vector->list"         , ScmOp_vector_to_list);
    Scm_InitSubr1("list->vector"         , ScmOp_list_to_vector);
    Scm_InitSubr2("vector-fill!"         , ScmOp_vector_fill);
    Scm_InitSubr1("procedure?"           , ScmOp_procedurep);
    Scm_InitSubrL("map"                  , ScmOp_map);
    Scm_InitSubrL("for-each"             , ScmOp_for_each);
    Scm_InitSubrL("force"                , ScmOp_force);
    /* io.c */
    Scm_InitSubr2("call-with-input-file" , ScmOp_call_with_input_file);
    Scm_InitSubr2("call-with-output-file", ScmOp_call_with_output_file);
    Scm_InitSubr1("input-port?"          , ScmOp_input_portp);
    Scm_InitSubr1("output-port?"         , ScmOp_output_portp);
    Scm_InitSubr0("current-input-port"   , ScmOp_current_input_port);
    Scm_InitSubr0("current-output-port"  , ScmOp_current_output_port);
    Scm_InitSubr2("with-input-from-file" , ScmOp_with_input_from_file);
    Scm_InitSubr2("with-output-to-file"  , ScmOp_with_output_to_file);
    Scm_InitSubr1("open-input-file"      , ScmOp_open_input_file);
    Scm_InitSubr1("open-output-file"     , ScmOp_open_output_file);
    Scm_InitSubr1("close-input-port"     , ScmOp_close_input_port);
    Scm_InitSubr1("close-output-port"    , ScmOp_close_output_port);
    Scm_InitSubrL("read"                 , ScmOp_read);
    Scm_InitSubrL("read-char"            , ScmOp_read_char);
    Scm_InitSubr1("eof-object?"          , ScmOp_eof_objectp);
    Scm_InitSubrL("write"                , ScmOp_write);
    Scm_InitSubrL("display"              , ScmOp_display);
    Scm_InitSubrL("print"                , ScmOp_display);
    Scm_InitSubrL("newline"              , ScmOp_newline);
    Scm_InitSubrL("write-char"           , ScmOp_write_char);
    Scm_InitSubr1("load"                 , ScmOp_load);

    /*=======================================================================
      Current Input & Output Initialization
    =======================================================================*/
    current_input_port  = Scm_NewPort(stdin,  PORT_INPUT);
    SigScm_gc_protect(current_input_port);
    current_output_port = Scm_NewPort(stdout, PORT_OUTPUT);
    SigScm_gc_protect(current_output_port);

    stack_start_pointer = NULL;
}

void SigScm_Finalize()
{
    SigScm_FinalizeStorage();
}

/*===========================================================================
  Scheme Function Export Related Functions
===========================================================================*/
static void Scm_InitSubr(char *name, enum ScmFuncArgNum argnum, ScmFuncType c_func)
{
    ScmObj sym  = Scm_Intern(name);
    ScmObj func = Scm_NewFunc(argnum, c_func);

    SCM_SYMBOL_VCELL(sym) = func;
}

void Scm_InitSubr0(char *name, ScmObj (*func) (void))
{
    Scm_InitSubr(name, ARGNUM_0, (ScmFuncType)func);
}

void Scm_InitSubr1(char *name, ScmObj (*func) (ScmObj))
{
    Scm_InitSubr(name, ARGNUM_1, (ScmFuncType)func);
}

void Scm_InitSubr2(char *name, ScmObj (*func) (ScmObj, ScmObj))
{
    Scm_InitSubr(name, ARGNUM_2, (ScmFuncType)func);
}

void Scm_InitSubr3(char *name, ScmObj (*func) (ScmObj, ScmObj, ScmObj))
{
    Scm_InitSubr(name, ARGNUM_3, (ScmFuncType)func);
}

void Scm_InitSubr4(char *name, ScmObj (*func) (ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_InitSubr(name, ARGNUM_4, (ScmFuncType)func);
}

void Scm_InitSubr5(char *name, ScmObj (*func) (ScmObj, ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_InitSubr(name, ARGNUM_5, (ScmFuncType)func);
}

void Scm_InitSubrL(char *name, ScmObj (*func) (ScmObj, ScmObj))
{
    Scm_InitSubr(name, ARGNUM_L, (ScmFuncType)func);
}

void Scm_InitSubrR(char *name, ScmObj (*func) (ScmObj, ScmObj))
{
    Scm_InitSubr(name, ARGNUM_R, (ScmFuncType)func);
}

void Scm_InitSubr2N(char *name, ScmObj (*func) (ScmObj, ScmObj))
{
    Scm_InitSubr(name, ARGNUM_2N, (ScmFuncType)func);
}
