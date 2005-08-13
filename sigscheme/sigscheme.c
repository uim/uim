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
static void Scm_RegisterFunc(const char *name, enum ScmFuncArgType argnum, ScmFuncType func);

ScmObj SigScm_nil, SigScm_true, SigScm_false, SigScm_eof;
ScmObj SigScm_quote, SigScm_quasiquote, SigScm_unquote, SigScm_unquote_splicing;
ScmObj SigScm_unbound, SigScm_unspecified, SigScm_undef;
ScmObjInternal SigScm_nil_impl, SigScm_true_impl, SigScm_false_impl, SigScm_eof_impl;
ScmObjInternal SigScm_quote_impl, SigScm_quasiquote_impl, SigScm_unquote_impl, SigScm_unquote_splicing_impl;
ScmObjInternal SigScm_unbound_impl, SigScm_unspecified_impl, SigScm_undef_impl;

extern ScmObj continuation_thrown_obj, letrec_env;

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
      Externed Variable Initialization
    =======================================================================*/
    continuation_thrown_obj = SCM_NIL;
    letrec_env              = SCM_NIL;
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
    SCM_SYMBOL_VCELL(Scm_Intern("=>"))   = SCM_TRUE;
    /*=======================================================================
      Export Scheme Functions
    =======================================================================*/
    /* eval.c */
    Scm_RegisterFunc2("eval"                 , ScmOp_eval);
    Scm_RegisterFuncL("apply"                , ScmOp_apply);
    Scm_RegisterFunc1("quote"                , ScmOp_quote);
    Scm_RegisterFuncR("lambda"               , ScmExp_lambda);
    Scm_RegisterFuncR("if"                   , ScmExp_if);
    Scm_RegisterFuncR("set!"                 , ScmExp_set);
    Scm_RegisterFuncR("cond"                 , ScmExp_cond);
    Scm_RegisterFuncR("case"                 , ScmExp_case);
    Scm_RegisterFuncR("and"                  , ScmExp_and);
    Scm_RegisterFuncR("or"                   , ScmExp_or);
    Scm_RegisterFuncR("let"                  , ScmExp_let);
    Scm_RegisterFuncR("let*"                 , ScmExp_let_star);
    Scm_RegisterFuncR("letrec"               , ScmExp_letrec);
    Scm_RegisterFuncR("begin"                , ScmExp_begin);
    Scm_RegisterFuncR("do"                   , ScmExp_do);
    Scm_RegisterFuncR("delay"                , ScmOp_delay);
    Scm_RegisterFunc1("quasiquote"           , ScmOp_quasiquote);
    Scm_RegisterFunc1("unquote"              , ScmOp_unquote);
    Scm_RegisterFunc1("unquote-splicing"     , ScmOp_unquote_splicing);
    Scm_RegisterFuncR("define"               , ScmExp_define);
    Scm_RegisterFunc1("scheme-report-environment", ScmOp_scheme_report_environment);
    Scm_RegisterFunc1("null-environment"         , ScmOp_null_environment);
    /* operations.c */
    Scm_RegisterFunc2("eqv?"                 , ScmOp_eqvp);
    Scm_RegisterFunc2("eq?"                  , ScmOp_eqp);
    Scm_RegisterFunc2("equal?"               , ScmOp_equalp);
    Scm_RegisterFunc1("number?"              , ScmOp_numberp);
    Scm_RegisterFuncL("="                    , ScmOp_equal);
    Scm_RegisterFuncL("<"                    , ScmOp_bigger);
    Scm_RegisterFuncL(">"                    , ScmOp_smaller);
    Scm_RegisterFuncL("<="                   , ScmOp_biggerEq);
    Scm_RegisterFuncL(">="                   , ScmOp_smallerEq);
    Scm_RegisterFunc1("zero?"                , ScmOp_zerop);
    Scm_RegisterFunc1("positive?"            , ScmOp_positivep);
    Scm_RegisterFunc1("negative?"            , ScmOp_negativep);
    Scm_RegisterFunc1("odd?"                 , ScmOp_oddp);
    Scm_RegisterFunc1("even?"                , ScmOp_evenp);
    Scm_RegisterFuncL("max"                  , ScmOp_max);
    Scm_RegisterFuncL("min"                  , ScmOp_min);
    Scm_RegisterFunc2N("+"                   , ScmOp_plus2n);
    Scm_RegisterFunc2N("*"                   , ScmOp_multi2n);
    Scm_RegisterFunc2N("-"                   , ScmOp_minus2n);
    Scm_RegisterFunc2N("/"                   , ScmOp_divide2n);
    Scm_RegisterFunc1("abs"                  , ScmOp_abs);
    Scm_RegisterFunc2("quotient"             , ScmOp_quotient);
    Scm_RegisterFunc2("modulo"               , ScmOp_modulo);
    Scm_RegisterFunc2("remainder"            , ScmOp_remainder);
    Scm_RegisterFunc1("number->string"       , ScmOp_number_to_string);
    Scm_RegisterFunc1("string->number"       , ScmOp_string_to_number);
    Scm_RegisterFunc1("not"                  , ScmOp_not);
    Scm_RegisterFunc1("boolean?"             , ScmOp_booleanp);
    Scm_RegisterFunc1("pair?"                , ScmOp_pairp);
    Scm_RegisterFunc2("cons"                 , ScmOp_cons);
    Scm_RegisterFunc1("car"                  , ScmOp_car);
    Scm_RegisterFunc1("cdr"                  , ScmOp_cdr);
    Scm_RegisterFunc2("set-car!"             , ScmOp_setcar);
    Scm_RegisterFunc2("set-cdr!"             , ScmOp_setcdr);
    Scm_RegisterFunc1("caar"                 , ScmOp_caar);
    Scm_RegisterFunc1("cadr"                 , ScmOp_cadr);
    Scm_RegisterFunc1("cdar"                 , ScmOp_cdar);
    Scm_RegisterFunc1("cddr"                 , ScmOp_cddr);
    Scm_RegisterFunc1("caaar"                , ScmOp_caaar);
    Scm_RegisterFunc1("caadr"                , ScmOp_caadr);
    Scm_RegisterFunc1("cadar"                , ScmOp_cadar);
    Scm_RegisterFunc1("caddr"                , ScmOp_caddr);
    Scm_RegisterFunc1("cdaar"                , ScmOp_cdaar);
    Scm_RegisterFunc1("cdadr"                , ScmOp_cdadr);
    Scm_RegisterFunc1("cddar"                , ScmOp_cddar);
    Scm_RegisterFunc1("cdddr"                , ScmOp_cdddr);
    Scm_RegisterFunc1("caaaar"               , ScmOp_caaaar);
    Scm_RegisterFunc1("caaadr"               , ScmOp_caaadr);
    Scm_RegisterFunc1("caadar"               , ScmOp_caadar);
    Scm_RegisterFunc1("caaddr"               , ScmOp_caaddr);
    Scm_RegisterFunc1("cadaar"               , ScmOp_cadaar);
    Scm_RegisterFunc1("cadadr"               , ScmOp_cadadr);
    Scm_RegisterFunc1("caddar"               , ScmOp_caddar);
    Scm_RegisterFunc1("cadddr"               , ScmOp_cadddr);
    Scm_RegisterFunc1("cdaaar"               , ScmOp_cdaaar);
    Scm_RegisterFunc1("cdaadr"               , ScmOp_cdaadr);
    Scm_RegisterFunc1("cdadar"               , ScmOp_cdadar);
    Scm_RegisterFunc1("cdaddr"               , ScmOp_cdaddr);
    Scm_RegisterFunc1("cddaar"               , ScmOp_cddaar);
    Scm_RegisterFunc1("cddadr"               , ScmOp_cddadr);
    Scm_RegisterFunc1("cdddar"               , ScmOp_cdddar);
    Scm_RegisterFunc1("cddddr"               , ScmOp_cddddr);
    Scm_RegisterFunc1("null?"                , ScmOp_nullp);
    Scm_RegisterFunc1("list?"                , ScmOp_listp);
    Scm_RegisterFuncL("list"                 , ScmOp_list);
    Scm_RegisterFunc1("length"               , ScmOp_length);
    Scm_RegisterFuncL("append"               , ScmOp_append);
    Scm_RegisterFunc1("reverse"              , ScmOp_reverse);
    Scm_RegisterFunc2("list-tail"            , ScmOp_listtail);
    Scm_RegisterFunc2("list-ref"             , ScmOp_listref);
    Scm_RegisterFunc2("memq"                 , ScmOp_memq);
    Scm_RegisterFunc2("memv"                 , ScmOp_memv);
    Scm_RegisterFunc2("member"               , ScmOp_member);
    Scm_RegisterFunc2("assq"                 , ScmOp_assq);
    Scm_RegisterFunc2("assv"                 , ScmOp_assv);
    Scm_RegisterFunc2("assoc"                , ScmOp_assoc);
    Scm_RegisterFunc1("symbol?"              , ScmOp_symbolp);
    Scm_RegisterFunc1("symbol->string"       , ScmOp_symbol_to_string);
    Scm_RegisterFunc1("string->symbol"       , ScmOp_string_to_symbol);
    Scm_RegisterFunc1("char?"                , ScmOp_charp);
    Scm_RegisterFunc2("char=?"               , ScmOp_char_equal);
    Scm_RegisterFunc1("char-alphabetic?"     , ScmOp_char_alphabeticp);
    Scm_RegisterFunc1("char-numeric?"        , ScmOp_char_numericp);
    Scm_RegisterFunc1("char-whitespace?"     , ScmOp_char_whitespacep);
    Scm_RegisterFunc1("char-upper-case?"     , ScmOp_char_upper_casep);
    Scm_RegisterFunc1("char-lower-case?"     , ScmOp_char_lower_casep);
    Scm_RegisterFunc1("char-upcase"          , ScmOp_char_upcase);
    Scm_RegisterFunc1("char-downcase"        , ScmOp_char_downcase);
    Scm_RegisterFunc1("string?"              , ScmOp_stringp);
    Scm_RegisterFuncL("make-string"          , ScmOp_make_string);
    Scm_RegisterFuncL("string"               , ScmOp_string);
    Scm_RegisterFunc2("string-ref"           , ScmOp_string_ref);
    Scm_RegisterFunc3("string-set!"          , ScmOp_string_set);
    Scm_RegisterFunc1("string-length"        , ScmOp_string_length);
    Scm_RegisterFunc2("string=?"             , ScmOp_string_equal);
    Scm_RegisterFunc3("substring"            , ScmOp_string_substring);
    Scm_RegisterFuncL("string-append"        , ScmOp_string_append);
    Scm_RegisterFunc1("string->list"         , ScmOp_string_to_list);
    Scm_RegisterFunc1("list->string"         , ScmOp_list_to_string);
    Scm_RegisterFunc1("string-copy"          , ScmOp_string_copy);
    Scm_RegisterFunc2("string-fill!"         , ScmOp_string_fill);
    Scm_RegisterFunc1("vector?"              , ScmOp_vectorp);
    Scm_RegisterFuncL("make-vector"          , ScmOp_make_vector);
    Scm_RegisterFuncL("vector"               , ScmOp_vector);
    Scm_RegisterFunc1("vector-length"        , ScmOp_vector_length);
    Scm_RegisterFunc2("vector-ref"           , ScmOp_vector_ref);
    Scm_RegisterFunc3("vector-set!"          , ScmOp_vector_set);
    Scm_RegisterFunc1("vector->list"         , ScmOp_vector_to_list);
    Scm_RegisterFunc1("list->vector"         , ScmOp_list_to_vector);
    Scm_RegisterFunc2("vector-fill!"         , ScmOp_vector_fill);
    Scm_RegisterFunc1("procedure?"           , ScmOp_procedurep);
    Scm_RegisterFuncL("map"                  , ScmOp_map);
    Scm_RegisterFuncL("for-each"             , ScmOp_for_each);
    Scm_RegisterFuncL("force"                , ScmOp_force);
    Scm_RegisterFuncL("call-with-current-continuation", ScmOp_call_with_current_continuation);
    /* io.c */
    Scm_RegisterFunc2("call-with-input-file" , ScmOp_call_with_input_file);
    Scm_RegisterFunc2("call-with-output-file", ScmOp_call_with_output_file);
    Scm_RegisterFunc1("input-port?"          , ScmOp_input_portp);
    Scm_RegisterFunc1("output-port?"         , ScmOp_output_portp);
    Scm_RegisterFunc0("current-input-port"   , ScmOp_current_input_port);
    Scm_RegisterFunc0("current-output-port"  , ScmOp_current_output_port);
    Scm_RegisterFunc2("with-input-from-file" , ScmOp_with_input_from_file);
    Scm_RegisterFunc2("with-output-to-file"  , ScmOp_with_output_to_file);
    Scm_RegisterFunc1("open-input-file"      , ScmOp_open_input_file);
    Scm_RegisterFunc1("open-output-file"     , ScmOp_open_output_file);
    Scm_RegisterFunc1("close-input-port"     , ScmOp_close_input_port);
    Scm_RegisterFunc1("close-output-port"    , ScmOp_close_output_port);
    Scm_RegisterFuncL("read"                 , ScmOp_read);
    Scm_RegisterFuncL("read-char"            , ScmOp_read_char);
    Scm_RegisterFunc1("eof-object?"          , ScmOp_eof_objectp);
    Scm_RegisterFuncL("write"                , ScmOp_write);
    Scm_RegisterFuncL("display"              , ScmOp_display);
    Scm_RegisterFuncL("print"                , ScmOp_print);
    Scm_RegisterFuncL("newline"              , ScmOp_newline);
    Scm_RegisterFuncL("write-char"           , ScmOp_write_char);
    Scm_RegisterFunc1("load"                 , ScmOp_load);
    Scm_RegisterFunc1("file-exists?"         , ScmOp_file_existsp);
    Scm_RegisterFunc1("delete-file"          , ScmOp_delete_file);
    /*=======================================================================
      Current Input & Output Initialization
    =======================================================================*/
    current_input_port  = Scm_NewFilePort(stdin,  "stdin",  PORT_INPUT);
    SigScm_gc_protect(current_input_port);
    current_output_port = Scm_NewFilePort(stdout, "stdout", PORT_OUTPUT);
    SigScm_gc_protect(current_output_port);
    current_error_port  = Scm_NewFilePort(stderr, "stderr", PORT_OUTPUT);
    SigScm_gc_protect(current_error_port);

#if USE_SRFI1
    /*=======================================================================
      SRFI-1 Procedures
    =======================================================================*/
    Scm_RegisterFunc2("xcons"                , ScmOp_SRFI_1_xcons);
    Scm_RegisterFuncL("cons*"                , ScmOp_SRFI_1_cons_star);
    Scm_RegisterFuncL("make-list"            , ScmOp_SRFI_1_make_list);
    Scm_RegisterFuncL("list-tabulate"        , ScmOp_SRFI_1_list_tabulate);
    Scm_RegisterFunc1("list-copy"            , ScmOp_SRFI_1_list_copy);
    Scm_RegisterFuncL("circular-list"        , ScmOp_SRFI_1_circular_list);
    Scm_RegisterFuncL("iota"                 , ScmOp_SRFI_1_iota);
#endif

    stack_start_pointer = NULL;
}

void SigScm_Finalize()
{
    SigScm_FinalizeStorage();
}

/*===========================================================================
  Scheme Function Export Related Functions
===========================================================================*/
static void Scm_RegisterFunc(const char *name, enum ScmFuncArgType argnum, ScmFuncType c_func)
{
    ScmObj sym  = Scm_Intern(name);
    ScmObj func = Scm_NewFunc(argnum, c_func);

    SCM_SYMBOL_VCELL(sym) = func;
}

void Scm_RegisterFunc0(const char *name, ScmObj (*func) (void))
{
    Scm_RegisterFunc(name, FUNCTYPE_0, (ScmFuncType)func);
}

void Scm_RegisterFunc1(const char *name, ScmObj (*func) (ScmObj))
{
    Scm_RegisterFunc(name, FUNCTYPE_1, (ScmFuncType)func);
}

void Scm_RegisterFunc2(const char *name, ScmObj (*func) (ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, FUNCTYPE_2, (ScmFuncType)func);
}

void Scm_RegisterFunc3(const char *name, ScmObj (*func) (ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, FUNCTYPE_3, (ScmFuncType)func);
}

void Scm_RegisterFunc4(const char *name, ScmObj (*func) (ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, FUNCTYPE_4, (ScmFuncType)func);
}

void Scm_RegisterFunc5(const char *name, ScmObj (*func) (ScmObj, ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, FUNCTYPE_5, (ScmFuncType)func);
}

void Scm_RegisterFuncL(const char *name, ScmObj (*func) (ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, FUNCTYPE_L, (ScmFuncType)func);
}

void Scm_RegisterFuncR(const char *name, ScmObj (*func) (ScmObj, ScmObj*, int *))
{
    Scm_RegisterFunc(name, FUNCTYPE_R, (ScmFuncType)func);
}

void Scm_RegisterFunc2N(const char *name, ScmObj (*func) (ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, FUNCTYPE_2N, (ScmFuncType)func);
}
