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
static void Scm_RegisterFunc(const char *name, enum ScmFuncTypeCode type, ScmFuncType func);

ScmObj SigScm_null, SigScm_true, SigScm_false, SigScm_eof;
ScmObj SigScm_quote, SigScm_quasiquote, SigScm_unquote, SigScm_unquote_splicing;
ScmObj SigScm_unbound, SigScm_undef;
ScmObjInternal SigScm_null_impl, SigScm_true_impl, SigScm_false_impl, SigScm_eof_impl;
ScmObjInternal SigScm_quote_impl, SigScm_quasiquote_impl, SigScm_unquote_impl, SigScm_unquote_splicing_impl;
ScmObjInternal SigScm_unbound_impl, SigScm_undef_impl;

#if SCM_COMPAT_SIOD
extern ScmObj scm_return_value;
#endif

/*=======================================
  Function Implementations
=======================================*/
void SigScm_Initialize(void)
{
    ScmObj obj;
    SigScm_GC_ProtectStack(&obj);

    /*=======================================================================
      Etc Variable Initialization
    =======================================================================*/
    SCM_ETC_SET_IMPL(SigScm_null,             SigScm_null_impl   );
    SCM_ETC_SET_IMPL(SigScm_true,             SigScm_true_impl   );
    SCM_ETC_SET_IMPL(SigScm_false,            SigScm_false_impl  );
    SCM_ETC_SET_IMPL(SigScm_eof,              SigScm_eof_impl    );
    SCM_ETC_SET_IMPL(SigScm_unbound,          SigScm_unbound_impl);
    SCM_ETC_SET_IMPL(SigScm_undef,            SigScm_undef_impl  );

#if SCM_COMPAT_SIOD_BUGS
    SigScm_false = SigScm_null;
#endif

    /*=======================================================================
      Externed Variable Initialization
    =======================================================================*/
    scm_continuation_thrown_obj = SCM_NULL;
    scm_letrec_env              = SCM_NULL;
    /*=======================================================================
      Storage Initialization
    =======================================================================*/
    SigScm_InitStorage();
    /*=======================================================================
      Interned Variable Initialization
    =======================================================================*/
    SigScm_quote            = Scm_Intern("quote");
    SigScm_quasiquote       = Scm_Intern("quasiquote");
    SigScm_unquote          = Scm_Intern("unquote");
    SigScm_unquote_splicing = Scm_Intern("unquote-splicing");
#if SCM_USE_NONSTD_FEATURES
    SigScm_features         = Scm_Intern("*features*");
#endif
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
    Scm_RegisterFunc2("eval"                     , ScmOp_eval);
    Scm_RegisterFuncEvaledList("apply"           , ScmOp_apply);
    Scm_RegisterFuncRawList("quote"              , ScmOp_quote);
    Scm_RegisterFuncRawList("lambda"             , ScmExp_lambda);
    Scm_RegisterFuncRawList("set!"               , ScmExp_set);
    Scm_RegisterFuncRawList("delay"              , ScmOp_delay);
    Scm_RegisterFuncRawList("quasiquote"         , ScmOp_quasiquote);
    Scm_RegisterFuncRawList("unquote"            , ScmOp_unquote);
    Scm_RegisterFuncRawList("unquote-splicing"   , ScmOp_unquote_splicing);
    Scm_RegisterFuncRawList("define"             , ScmExp_define);
    Scm_RegisterFuncRawListTailRec("if"          , ScmExp_if);
    Scm_RegisterFuncRawListTailRec("cond"        , ScmExp_cond);
    Scm_RegisterFuncRawListTailRec("case"        , ScmExp_case);
    Scm_RegisterFuncRawListTailRec("let"         , ScmExp_let);
    Scm_RegisterFuncRawListTailRec("let*"        , ScmExp_let_star);
    Scm_RegisterFuncRawListTailRec("letrec"      , ScmExp_letrec);
    Scm_RegisterFuncRawListTailRec("begin"       , ScmExp_begin);
    Scm_RegisterFuncRawListTailRec("do"          , ScmExp_do);
    Scm_RegisterFuncRawListWithTailFlag("and"    , ScmExp_and);
    Scm_RegisterFuncRawListWithTailFlag("or"     , ScmExp_or);
    Scm_RegisterFunc1("scheme-report-environment", ScmOp_scheme_report_environment);
    Scm_RegisterFunc1("null-environment"         , ScmOp_null_environment);
    /* operations.c */
    Scm_RegisterFunc2("eqv?"                     , ScmOp_eqvp);
    Scm_RegisterFunc2("eq?"                      , ScmOp_eqp);
    Scm_RegisterFunc2("equal?"                   , ScmOp_equalp);
    Scm_RegisterFunc1("number?"                  , ScmOp_numberp);
    Scm_RegisterFunc1("integer?"                 , ScmOp_numberp);
    Scm_RegisterFuncEvaledList("="               , ScmOp_equal);
    Scm_RegisterFuncEvaledList("<"               , ScmOp_less);
    Scm_RegisterFuncEvaledList(">"               , ScmOp_greater);
    Scm_RegisterFuncEvaledList("<="              , ScmOp_lessEq);
    Scm_RegisterFuncEvaledList(">="              , ScmOp_greaterEq);
    Scm_RegisterFunc1("zero?"                    , ScmOp_zerop);
    Scm_RegisterFunc1("positive?"                , ScmOp_positivep);
    Scm_RegisterFunc1("negative?"                , ScmOp_negativep);
    Scm_RegisterFunc1("odd?"                     , ScmOp_oddp);
    Scm_RegisterFunc1("even?"                    , ScmOp_evenp);
    Scm_RegisterFuncRawList("max"                , ScmOp_max);
    Scm_RegisterFuncRawList("min"                , ScmOp_min);
    Scm_RegisterFuncRawList("+"                  , ScmOp_plus);
    Scm_RegisterFuncRawList("*"                  , ScmOp_times);
    Scm_RegisterFuncRawList("-"                  , ScmOp_minus);
    Scm_RegisterFuncRawList("/"                  , ScmOp_divide);
    Scm_RegisterFunc1("abs"                      , ScmOp_abs);
    Scm_RegisterFunc2("quotient"                 , ScmOp_quotient);
    Scm_RegisterFunc2("modulo"                   , ScmOp_modulo);
    Scm_RegisterFunc2("remainder"                , ScmOp_remainder);
    Scm_RegisterFuncEvaledList("number->string"  , ScmOp_number2string);
    Scm_RegisterFunc1("string->number"           , ScmOp_string2number);
    Scm_RegisterFunc1("not"                      , ScmOp_not);
    Scm_RegisterFunc1("boolean?"                 , ScmOp_booleanp);
    Scm_RegisterFunc1("pair?"                    , ScmOp_pairp);
    Scm_RegisterFunc2("cons"                     , ScmOp_cons);
    Scm_RegisterFunc1("car"                      , ScmOp_car);
    Scm_RegisterFunc1("cdr"                      , ScmOp_cdr);
    Scm_RegisterFunc2("set-car!"                 , ScmOp_setcar);
    Scm_RegisterFunc2("set-cdr!"                 , ScmOp_setcdr);
    Scm_RegisterFunc1("caar"                     , ScmOp_caar);
    Scm_RegisterFunc1("cadr"                     , ScmOp_cadr);
    Scm_RegisterFunc1("cdar"                     , ScmOp_cdar);
    Scm_RegisterFunc1("cddr"                     , ScmOp_cddr);
    Scm_RegisterFunc1("caaar"                    , ScmOp_caaar);
    Scm_RegisterFunc1("caadr"                    , ScmOp_caadr);
    Scm_RegisterFunc1("cadar"                    , ScmOp_cadar);
    Scm_RegisterFunc1("caddr"                    , ScmOp_caddr);
    Scm_RegisterFunc1("cdaar"                    , ScmOp_cdaar);
    Scm_RegisterFunc1("cdadr"                    , ScmOp_cdadr);
    Scm_RegisterFunc1("cddar"                    , ScmOp_cddar);
    Scm_RegisterFunc1("cdddr"                    , ScmOp_cdddr);
    Scm_RegisterFunc1("caaaar"                   , ScmOp_caaaar);
    Scm_RegisterFunc1("caaadr"                   , ScmOp_caaadr);
    Scm_RegisterFunc1("caadar"                   , ScmOp_caadar);
    Scm_RegisterFunc1("caaddr"                   , ScmOp_caaddr);
    Scm_RegisterFunc1("cadaar"                   , ScmOp_cadaar);
    Scm_RegisterFunc1("cadadr"                   , ScmOp_cadadr);
    Scm_RegisterFunc1("caddar"                   , ScmOp_caddar);
    Scm_RegisterFunc1("cadddr"                   , ScmOp_cadddr);
    Scm_RegisterFunc1("cdaaar"                   , ScmOp_cdaaar);
    Scm_RegisterFunc1("cdaadr"                   , ScmOp_cdaadr);
    Scm_RegisterFunc1("cdadar"                   , ScmOp_cdadar);
    Scm_RegisterFunc1("cdaddr"                   , ScmOp_cdaddr);
    Scm_RegisterFunc1("cddaar"                   , ScmOp_cddaar);
    Scm_RegisterFunc1("cddadr"                   , ScmOp_cddadr);
    Scm_RegisterFunc1("cdddar"                   , ScmOp_cdddar);
    Scm_RegisterFunc1("cddddr"                   , ScmOp_cddddr);
    Scm_RegisterFunc1("null?"                    , ScmOp_nullp);
    Scm_RegisterFunc1("list?"                    , ScmOp_listp);
    Scm_RegisterFunc1("length"                   , ScmOp_length);
    Scm_RegisterFuncEvaledList("list"            , ScmOp_list);
    Scm_RegisterFuncEvaledList("append"          , ScmOp_append);
    Scm_RegisterFunc1("reverse"                  , ScmOp_reverse);
    Scm_RegisterFunc2("list-tail"                , ScmOp_list_tail);
    Scm_RegisterFunc2("list-ref"                 , ScmOp_list_ref);
    Scm_RegisterFunc2("memq"                     , ScmOp_memq);
    Scm_RegisterFunc2("memv"                     , ScmOp_memv);
    Scm_RegisterFunc2("member"                   , ScmOp_member);
    Scm_RegisterFunc2("assq"                     , ScmOp_assq);
    Scm_RegisterFunc2("assv"                     , ScmOp_assv);
    Scm_RegisterFunc2("assoc"                    , ScmOp_assoc);
    Scm_RegisterFunc1("symbol?"                  , ScmOp_symbolp);
    Scm_RegisterFunc1("symbol->string"           , ScmOp_symbol2string);
    Scm_RegisterFunc1("string->symbol"           , ScmOp_string2symbol);
    Scm_RegisterFunc1("char?"                    , ScmOp_charp);
    Scm_RegisterFunc2("char=?"                   , ScmOp_char_equal);
    Scm_RegisterFunc1("char-alphabetic?"         , ScmOp_char_alphabeticp);
    Scm_RegisterFunc1("char-numeric?"            , ScmOp_char_numericp);
    Scm_RegisterFunc1("char-whitespace?"         , ScmOp_char_whitespacep);
    Scm_RegisterFunc1("char-upper-case?"         , ScmOp_char_upper_casep);
    Scm_RegisterFunc1("char-lower-case?"         , ScmOp_char_lower_casep);
    Scm_RegisterFunc1("char-upcase"              , ScmOp_char_upcase);
    Scm_RegisterFunc1("char-downcase"            , ScmOp_char_downcase);
    Scm_RegisterFunc1("string?"                  , ScmOp_stringp);
    Scm_RegisterFuncEvaledList("make-string"     , ScmOp_make_string);
    Scm_RegisterFuncEvaledList("string"          , ScmOp_string);
    Scm_RegisterFunc2("string-ref"               , ScmOp_string_ref);
    Scm_RegisterFunc3("string-set!"              , ScmOp_string_set);
    Scm_RegisterFunc1("string-length"            , ScmOp_string_length);
    Scm_RegisterFunc2("string=?"                 , ScmOp_string_equal);
    Scm_RegisterFunc3("substring"                , ScmOp_string_substring);
    Scm_RegisterFuncEvaledList("string-append"   , ScmOp_string_append);
    Scm_RegisterFunc1("string->list"             , ScmOp_string2list);
    Scm_RegisterFunc1("list->string"             , ScmOp_list2string);
    Scm_RegisterFunc1("string-copy"              , ScmOp_string_copy);
    Scm_RegisterFunc2("string-fill!"             , ScmOp_string_fill);
    Scm_RegisterFunc1("vector?"                  , ScmOp_vectorp);
    Scm_RegisterFuncEvaledList("make-vector"     , ScmOp_make_vector);
    Scm_RegisterFuncEvaledList("vector"          , ScmOp_vector);
    Scm_RegisterFunc1("vector-length"            , ScmOp_vector_length);
    Scm_RegisterFunc2("vector-ref"               , ScmOp_vector_ref);
    Scm_RegisterFunc3("vector-set!"              , ScmOp_vector_set);
    Scm_RegisterFunc1("vector->list"             , ScmOp_vector2list);
    Scm_RegisterFunc1("list->vector"             , ScmOp_list2vector);
    Scm_RegisterFunc2("vector-fill!"             , ScmOp_vector_fill);
    Scm_RegisterFunc1("procedure?"               , ScmOp_procedurep);
    Scm_RegisterFuncEvaledList("map"             , ScmOp_map);
    Scm_RegisterFuncEvaledList("for-each"        , ScmOp_for_each);
    Scm_RegisterFuncEvaledList("force"           , ScmOp_force);
    Scm_RegisterFuncEvaledList("values"          , ScmOp_values);
    Scm_RegisterFuncEvaledList("call-with-current-continuation", ScmOp_call_with_current_continuation);
    Scm_RegisterFuncRawListTailRec("call-with-values" , ScmOp_call_with_values);
    /* io.c */
    Scm_RegisterFunc2("call-with-input-file"     , ScmOp_call_with_input_file);
    Scm_RegisterFunc2("call-with-output-file"    , ScmOp_call_with_output_file);
    Scm_RegisterFunc1("input-port?"              , ScmOp_input_portp);
    Scm_RegisterFunc1("output-port?"             , ScmOp_output_portp);
    Scm_RegisterFunc0("current-input-port"       , ScmOp_current_input_port);
    Scm_RegisterFunc0("current-output-port"      , ScmOp_current_output_port);
    Scm_RegisterFunc2("with-input-from-file"     , ScmOp_with_input_from_file);
    Scm_RegisterFunc2("with-output-to-file"      , ScmOp_with_output_to_file);
    Scm_RegisterFunc1("open-input-file"          , ScmOp_open_input_file);
    Scm_RegisterFunc1("open-output-file"         , ScmOp_open_output_file);
    Scm_RegisterFunc1("close-input-port"         , ScmOp_close_input_port);
    Scm_RegisterFunc1("close-output-port"        , ScmOp_close_output_port);
    Scm_RegisterFunc1("eof-object?"              , ScmOp_eof_objectp);
    Scm_RegisterFuncEvaledList("read"            , ScmOp_read);
    Scm_RegisterFuncEvaledList("read-char"       , ScmOp_read_char);
    Scm_RegisterFuncEvaledList("write"           , ScmOp_write);
    Scm_RegisterFuncEvaledList("display"         , ScmOp_display);
    Scm_RegisterFuncEvaledList("newline"         , ScmOp_newline);
    Scm_RegisterFuncEvaledList("write-char"      , ScmOp_write_char);
    Scm_RegisterFunc1("load"                     , ScmOp_load);
#if SCM_USE_NONSTD_FEATURES
    Scm_RegisterFunc1("require"                  , ScmOp_require);
    Scm_RegisterFunc1("provide"                  , ScmOp_provide);
    Scm_RegisterFunc1("provided?"                , ScmOp_providedp);
    Scm_RegisterFunc1("file-exists?"             , ScmOp_file_existsp);
    Scm_RegisterFunc1("delete-file"              , ScmOp_delete_file);
#endif

    /*=======================================================================
      Current Input & Output Initialization
    =======================================================================*/
    scm_current_input_port  = Scm_NewFilePort(stdin,  "stdin",  PORT_INPUT);
    scm_current_output_port = Scm_NewFilePort(stdout, "stdout", PORT_OUTPUT);
    scm_current_error_port  = Scm_NewFilePort(stderr, "stderr", PORT_OUTPUT);
    SigScm_GC_Protect(scm_current_input_port);
    SigScm_GC_Protect(scm_current_output_port);
    SigScm_GC_Protect(scm_current_error_port);

#if SCM_USE_SRFI1
    /*=======================================================================
      SRFI-1 Procedures
    =======================================================================*/
    Scm_RegisterFunc1("list-copy"            , ScmOp_SRFI1_list_copy);
    Scm_RegisterFunc2("xcons"                , ScmOp_SRFI1_xcons);
    Scm_RegisterFuncEvaledList("circular-list"  , ScmOp_SRFI1_circular_list);
    Scm_RegisterFuncEvaledList("iota"           , ScmOp_SRFI1_iota);
    Scm_RegisterFuncEvaledList("cons*"          , ScmOp_SRFI1_cons_star);
    Scm_RegisterFuncEvaledList("make-list"      , ScmOp_SRFI1_make_list);
    Scm_RegisterFuncEvaledList("list-tabulate"  , ScmOp_SRFI1_list_tabulate);
#endif
#if SCM_USE_SRFI8
    /*=======================================================================
      SRFI-8 Procedure
    =======================================================================*/
    Scm_RegisterFuncRawListTailRec("receive", ScmOp_SRFI8_receive);
#endif

#if SCM_COMPAT_SIOD
    /*=======================================================================
      SIOD Compatible Variables and Procedures
    =======================================================================*/
    /* operations-siod.c */
    Scm_RegisterFunc1("symbol-bound?"        , ScmOp_symbol_boundp);
    Scm_RegisterFunc1("symbol-value"         , ScmOp_symbol_value);
    Scm_RegisterFunc2("set-symbol-value!"    , ScmOp_set_symbol_value);
    Scm_RegisterFunc2("bit-and"              , ScmOp_bit_and);
    Scm_RegisterFunc2("bit-or"               , ScmOp_bit_or);
    Scm_RegisterFunc2("bit-xor"              , ScmOp_bit_xor);
    Scm_RegisterFunc1("bit-not"              , ScmOp_bit_not);
    Scm_RegisterFuncEvaledList("the-environment"      , ScmOp_the_environment);
    Scm_RegisterFunc1("%%closure-code"       , ScmOp_closure_code);
    Scm_RegisterFuncEvaledList("verbose"              , ScmOp_verbose);
    /* datas.c */
    scm_return_value = SCM_NULL;
#endif

    SigScm_GC_UnprotectStack(&obj);
}

void SigScm_Finalize()
{
    SigScm_FinalizeStorage();
}

/*===========================================================================
  Scheme Function Export Related Functions
===========================================================================*/
static void Scm_RegisterFunc(const char *name, enum ScmFuncTypeCode type, ScmFuncType c_func)
{
    ScmObj sym  = Scm_Intern(name);
    ScmObj func = Scm_NewFunc(type, c_func);

    SCM_SYMBOL_VCELL(sym) = func;
}

void Scm_RegisterFunc0(const char *name, ScmFuncType0 func)
{
    Scm_RegisterFunc(name, FUNCTYPE_0, func);
}

void Scm_RegisterFunc1(const char *name, ScmFuncType1 func)
{
    Scm_RegisterFunc(name, FUNCTYPE_1, func);
}

void Scm_RegisterFunc2(const char *name, ScmFuncType2 func)
{
    Scm_RegisterFunc(name, FUNCTYPE_2, func);
}

void Scm_RegisterFunc3(const char *name, ScmFuncType3 func)
{
    Scm_RegisterFunc(name, FUNCTYPE_3, func);
}

void Scm_RegisterFunc4(const char *name, ScmFuncType4 func)
{
    Scm_RegisterFunc(name, FUNCTYPE_4, func);
}

void Scm_RegisterFunc5(const char *name, ScmFuncType5 func)
{
    Scm_RegisterFunc(name, FUNCTYPE_5, func);
}

void Scm_RegisterFuncEvaledList(const char *name, ScmFuncTypeEvaledList func)
{
    Scm_RegisterFunc(name, FUNCTYPE_EVALED_LIST, func);
}

void Scm_RegisterFuncRawList(const char *name, ScmFuncTypeRawList func)
{
    Scm_RegisterFunc(name, FUNCTYPE_RAW_LIST, func);
}

void Scm_RegisterFuncRawListTailRec(const char *name, ScmFuncTypeRawListTailRec func)
{
    Scm_RegisterFunc(name, FUNCTYPE_RAW_LIST_TAIL_REC, func);
}

void Scm_RegisterFuncRawListWithTailFlag(const char *name, ScmFuncTypeRawListWithTailFlag func)
{
    Scm_RegisterFunc(name, FUNCTYPE_RAW_LIST_WITH_TAIL_FLAG, func);
}
