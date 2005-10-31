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
#if SCM_USE_NEWPORT
#include "baseport.h"
#include "sbcport.h"
#include "fileport.h"
#include "strport.h"
#endif

/*=======================================
  File Local Struct Declarations
=======================================*/
struct module_info {
    const char *name;
    void (*initializer)(void);
};

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
ScmObj SigScm_quote, SigScm_quasiquote, SigScm_unquote, SigScm_unquote_splicing;
#if SCM_COMPAT_SIOD
static ScmObj scm_return_value    = NULL;
#endif

static struct module_info module_info_table[] = {
#if SCM_USE_NONSTD_FEATURES
#if 0
    {"sscm", SigScm_Initialize_NONSTD_FEATURES},
#endif
#endif
#if SCM_USE_SRFI1
    {"srfi-1", SigScm_Initialize_SRFI1},
#endif
#if SCM_USE_SRFI2
    {"srfi-2", SigScm_Initialize_SRFI2},
#endif
#if SCM_USE_SRFI6
    {"srfi-6", SigScm_Initialize_SRFI6},
#endif
#if SCM_USE_SRFI8
    {"srfi-8", SigScm_Initialize_SRFI8},
#endif
#if SCM_USE_SRFI23
    {"srfi-23", SigScm_Initialize_SRFI23},
#endif
#if SCM_USE_SRFI34
    {"srfi-34", SigScm_Initialize_SRFI34},
#endif
#if SCM_USE_SRFI38
    {"srfi-38", SigScm_Initialize_SRFI38},
#endif
#if SCM_USE_SRFI60
    {"srfi-60", SigScm_Initialize_SRFI60},
#endif
#if SCM_COMPAT_SIOD
    {"siod", SigScm_Initialize_SIOD},
#endif
    {NULL, NULL}
};

/*=======================================
  File Local Function Declarations
=======================================*/
static void SigScm_Initialize_internal(void);
static int Scm_RegisterFunc(const char *name, ScmFuncType func, enum ScmFuncTypeCode type);
static ScmObj Scm_eval_c_string_internal(const char *exp);

/*=======================================
  Function Implementations
=======================================*/
void SigScm_Initialize(void)
{
#if SCM_GCC4_READY_GC
    SCM_GC_PROTECTED_CALL_VOID(SigScm_Initialize_internal, ());
#else
    ScmObj stack_start = NULL;

    SigScm_GC_ProtectStack(&stack_start);
    SigScm_Initialize_internal();
    SigScm_GC_UnprotectStack(&stack_start);
#endif
}

static void SigScm_Initialize_internal(void)
{
    /*=======================================================================
      Core
    =======================================================================*/
    SigScm_SetDebugCategories(SCM_DBG_ERRMSG | SCM_DBG_BACKTRACE
                              | SigScm_PredefinedDebugCategories());
    SigScm_InitStorage();

    /*=======================================================================
      Predefined Symbols and Variables
    =======================================================================*/
    SigScm_quote            = Scm_Intern("quote");
    SigScm_quasiquote       = Scm_Intern("quasiquote");
    SigScm_unquote          = Scm_Intern("unquote");
    SigScm_unquote_splicing = Scm_Intern("unquote-splicing");
#if 0
    /* FIXME: Rewrite ScmExp_cond() and ScmExp_case(), and enable this */
    SigScm_else             = Scm_Intern("else");
    SigScm_foo              = Scm_Intern("=>");
#else
    /* FIXME: obsolete this. don't set SCM_TRUE and rely on the value */
    SCM_SYMBOL_SET_VCELL(Scm_Intern("else"), SCM_TRUE);
    SCM_SYMBOL_SET_VCELL(Scm_Intern("=>"),   SCM_TRUE);
#endif

#if SCM_USE_NONSTD_FEATURES
    SigScm_features         = Scm_Intern("*features*");
    SCM_SYMBOL_SET_VCELL(SigScm_features, SCM_NULL);
#endif

    /*=======================================================================
      Preallocated Ports
    =======================================================================*/
#if SCM_USE_NEWPORT
    Scm_fileport_init();
    Scm_sbcport_init();

    scm_current_input_port
        = Scm_NewPort(ScmSingleByteCharPort_new(ScmFilePort_new(stdin)),
                      SCM_PORTFLAG_INPUT);
    scm_current_output_port
        = Scm_NewPort(ScmSingleByteCharPort_new(ScmFilePort_new(stdout)),
                      SCM_PORTFLAG_OUTPUT);
    scm_current_error_port
        = Scm_NewPort(ScmSingleByteCharPort_new(ScmFilePort_new(stderr)),
                      SCM_PORTFLAG_OUTPUT);
#else /* SCM_USE_NEWPORT */
    scm_current_input_port  = Scm_NewFilePort(stdin,  "stdin",  PORT_INPUT);
    scm_current_output_port = Scm_NewFilePort(stdout, "stdout", PORT_OUTPUT);
    scm_current_error_port  = Scm_NewFilePort(stderr, "stderr", PORT_OUTPUT);
#endif /* SCM_USE_NEWPORT */
    SigScm_GC_Protect(&scm_current_input_port);
    SigScm_GC_Protect(&scm_current_output_port);
    SigScm_GC_Protect(&scm_current_error_port);

    /*=======================================================================
      R5RS Syntaxes and Procedures
    =======================================================================*/
    /* eval.c */
    Scm_RegisterProcedureFixed2("eval"                     , ScmOp_eval);
    Scm_RegisterProcedureVariadicTailRec2("apply"           , ScmOp_apply);
    Scm_RegisterSyntaxFixed1("quote"              , ScmOp_quote);
    Scm_RegisterSyntaxVariadic2("lambda"             , ScmExp_lambda);
    Scm_RegisterSyntaxFixed2("set!"               , ScmExp_set);
    Scm_RegisterSyntaxFixed1("delay"              , ScmOp_delay);
    Scm_RegisterSyntaxFixed1("quasiquote"         , ScmOp_quasiquote);
    Scm_RegisterSyntaxFixed1("unquote"            , ScmOp_unquote);
    Scm_RegisterSyntaxFixed1("unquote-splicing"   , ScmOp_unquote_splicing);
    Scm_RegisterSyntaxVariadic1("define"             , ScmExp_define);
    Scm_RegisterSyntaxVariadicTailRec2("if"          , ScmExp_if);
    Scm_RegisterSyntaxVariadicTailRec0("cond"        , ScmExp_cond); /* FIXME */
    Scm_RegisterSyntaxVariadicTailRec1("case"        , ScmExp_case);
    Scm_RegisterSyntaxVariadicTailRec0("let"         , ScmExp_let); /* FIXME */
    Scm_RegisterSyntaxVariadicTailRec1("let*"        , ScmExp_let_star);
    Scm_RegisterSyntaxVariadicTailRec1("letrec"      , ScmExp_letrec);
    Scm_RegisterSyntaxVariadicTailRec0("begin"       , ScmExp_begin);
    Scm_RegisterSyntaxVariadicTailRec2("do"          , ScmExp_do);
    Scm_RegisterSyntaxVariadicTailRec0("and"    , ScmExp_and);
    Scm_RegisterSyntaxVariadicTailRec0("or"     , ScmExp_or);
    Scm_RegisterProcedureFixed1("scheme-report-environment", ScmOp_scheme_report_environment);
    Scm_RegisterProcedureFixed1("null-environment"         , ScmOp_null_environment);
    Scm_RegisterProcedureFixed0("interaction-environment"  , ScmOp_interaction_environment);
    /* operations.c */
    Scm_RegisterProcedureFixed2("eqv?"                     , ScmOp_eqvp);
    Scm_RegisterProcedureFixed2("eq?"                      , ScmOp_eqp);
    Scm_RegisterProcedureFixed2("equal?"                   , ScmOp_equalp);
    Scm_RegisterProcedureFixed1("number?"                  , ScmOp_numberp);
    Scm_DefineAlias("integer?"                  , "number?");
    Scm_RegisterReductionOperator("="               , ScmOp_equal);
    Scm_RegisterReductionOperator("<"               , ScmOp_less);
    Scm_RegisterReductionOperator(">"               , ScmOp_greater);
    Scm_RegisterReductionOperator("<="              , ScmOp_less_eq);
    Scm_RegisterReductionOperator(">="              , ScmOp_greater_eq);
    Scm_RegisterProcedureFixed1("zero?"                    , ScmOp_zerop);
    Scm_RegisterProcedureFixed1("positive?"                , ScmOp_positivep);
    Scm_RegisterProcedureFixed1("negative?"                , ScmOp_negativep);
    Scm_RegisterProcedureFixed1("odd?"                     , ScmOp_oddp);
    Scm_RegisterProcedureFixed1("even?"                    , ScmOp_evenp);
    Scm_RegisterReductionOperator("max"                , ScmOp_max);
    Scm_RegisterReductionOperator("min"                , ScmOp_min);
    Scm_RegisterReductionOperator("+"                  , ScmOp_add);
    Scm_RegisterReductionOperator("*"                  , ScmOp_multiply);
    Scm_RegisterReductionOperator("-"                  , ScmOp_subtract);
    Scm_RegisterReductionOperator("/"                  , ScmOp_divide);
    Scm_RegisterProcedureFixed1("abs"                      , ScmOp_abs);
    Scm_RegisterProcedureFixed2("quotient"                 , ScmOp_quotient);
    Scm_RegisterProcedureFixed2("modulo"                   , ScmOp_modulo);
    Scm_RegisterProcedureFixed2("remainder"                , ScmOp_remainder);
    Scm_RegisterProcedureVariadic1("number->string" , ScmOp_number2string);
    Scm_RegisterProcedureFixed1("string->number"           , ScmOp_string2number);
    Scm_RegisterProcedureFixed1("not"                      , ScmOp_not);
    Scm_RegisterProcedureFixed1("boolean?"                 , ScmOp_booleanp);
    Scm_RegisterProcedureFixed1("pair?"                    , ScmOp_pairp);
    Scm_RegisterProcedureFixed2("cons"                     , ScmOp_cons);
    Scm_RegisterProcedureFixed1("car"                      , ScmOp_car);
    Scm_RegisterProcedureFixed1("cdr"                      , ScmOp_cdr);
    Scm_RegisterProcedureFixed2("set-car!"                 , ScmOp_setcar);
    Scm_RegisterProcedureFixed2("set-cdr!"                 , ScmOp_setcdr);
    Scm_RegisterProcedureFixed1("caar"                     , ScmOp_caar);
    Scm_RegisterProcedureFixed1("cadr"                     , ScmOp_cadr);
    Scm_RegisterProcedureFixed1("cdar"                     , ScmOp_cdar);
    Scm_RegisterProcedureFixed1("cddr"                     , ScmOp_cddr);
    Scm_RegisterProcedureFixed1("caddr"                    , ScmOp_caddr);
    Scm_RegisterProcedureFixed1("cdddr"                    , ScmOp_cdddr);
#if SCM_USE_DEEP_CADRS
    Scm_RegisterProcedureFixed1("caaar"                    , ScmOp_caaar);
    Scm_RegisterProcedureFixed1("caadr"                    , ScmOp_caadr);
    Scm_RegisterProcedureFixed1("cadar"                    , ScmOp_cadar);
    Scm_RegisterProcedureFixed1("cdaar"                    , ScmOp_cdaar);
    Scm_RegisterProcedureFixed1("cdadr"                    , ScmOp_cdadr);
    Scm_RegisterProcedureFixed1("cddar"                    , ScmOp_cddar);
    Scm_RegisterProcedureFixed1("caaaar"                   , ScmOp_caaaar);
    Scm_RegisterProcedureFixed1("caaadr"                   , ScmOp_caaadr);
    Scm_RegisterProcedureFixed1("caadar"                   , ScmOp_caadar);
    Scm_RegisterProcedureFixed1("caaddr"                   , ScmOp_caaddr);
    Scm_RegisterProcedureFixed1("cadaar"                   , ScmOp_cadaar);
    Scm_RegisterProcedureFixed1("cadadr"                   , ScmOp_cadadr);
    Scm_RegisterProcedureFixed1("caddar"                   , ScmOp_caddar);
    Scm_RegisterProcedureFixed1("cadddr"                   , ScmOp_cadddr);
    Scm_RegisterProcedureFixed1("cdaaar"                   , ScmOp_cdaaar);
    Scm_RegisterProcedureFixed1("cdaadr"                   , ScmOp_cdaadr);
    Scm_RegisterProcedureFixed1("cdadar"                   , ScmOp_cdadar);
    Scm_RegisterProcedureFixed1("cdaddr"                   , ScmOp_cdaddr);
    Scm_RegisterProcedureFixed1("cddaar"                   , ScmOp_cddaar);
    Scm_RegisterProcedureFixed1("cddadr"                   , ScmOp_cddadr);
    Scm_RegisterProcedureFixed1("cdddar"                   , ScmOp_cdddar);
    Scm_RegisterProcedureFixed1("cddddr"                   , ScmOp_cddddr);
#endif /* SCM_USE_DEEP_CADRS */
    Scm_RegisterProcedureFixed1("null?"                    , ScmOp_nullp);
    Scm_RegisterProcedureFixed1("list?"                    , ScmOp_listp);
    Scm_RegisterProcedureFixed1("length"                   , ScmOp_length);
    Scm_RegisterProcedureVariadic0("list"        , ScmOp_list);
    Scm_RegisterProcedureVariadic0("append"      , ScmOp_append);
    Scm_RegisterProcedureFixed1("reverse"                  , ScmOp_reverse);
    Scm_RegisterProcedureFixed2("list-tail"                , ScmOp_list_tail);
    Scm_RegisterProcedureFixed2("list-ref"                 , ScmOp_list_ref);
    Scm_RegisterProcedureFixed2("memq"                     , ScmOp_memq);
    Scm_RegisterProcedureFixed2("memv"                     , ScmOp_memv);
    Scm_RegisterProcedureFixed2("member"                   , ScmOp_member);
    Scm_RegisterProcedureFixed2("assq"                     , ScmOp_assq);
    Scm_RegisterProcedureFixed2("assv"                     , ScmOp_assv);
    Scm_RegisterProcedureFixed2("assoc"                    , ScmOp_assoc);
    Scm_RegisterProcedureFixed1("symbol?"                  , ScmOp_symbolp);
    Scm_RegisterProcedureFixed1("symbol->string"           , ScmOp_symbol2string);
    Scm_RegisterProcedureFixed1("string->symbol"           , ScmOp_string2symbol);
    Scm_RegisterProcedureFixed1("char?"                    , ScmOp_charp);
    Scm_RegisterProcedureFixed2("char=?"                   , ScmOp_char_equal);
    Scm_RegisterProcedureFixed1("char-alphabetic?"         , ScmOp_char_alphabeticp);
    Scm_RegisterProcedureFixed1("char-numeric?"            , ScmOp_char_numericp);
    Scm_RegisterProcedureFixed1("char-whitespace?"         , ScmOp_char_whitespacep);
    Scm_RegisterProcedureFixed1("char-upper-case?"         , ScmOp_char_upper_casep);
    Scm_RegisterProcedureFixed1("char-lower-case?"         , ScmOp_char_lower_casep);
    Scm_RegisterProcedureFixed1("char-upcase"              , ScmOp_char_upcase);
    Scm_RegisterProcedureFixed1("char-downcase"            , ScmOp_char_downcase);
    Scm_RegisterProcedureFixed1("string?"                  , ScmOp_stringp);
    Scm_RegisterProcedureVariadic1("make-string" , ScmOp_make_string);
    Scm_RegisterProcedureVariadic0("string"      , ScmOp_string);
    Scm_RegisterProcedureFixed2("string-ref"               , ScmOp_string_ref);
    Scm_RegisterProcedureFixed3("string-set!"              , ScmOp_string_set);
    Scm_RegisterProcedureFixed1("string-length"            , ScmOp_string_length);
    Scm_RegisterProcedureFixed2("string=?"                 , ScmOp_string_equal);
    Scm_RegisterProcedureFixed3("substring"                , ScmOp_string_substring);
    Scm_RegisterProcedureVariadic0("string-append" , ScmOp_string_append);
    Scm_RegisterProcedureFixed1("string->list"             , ScmOp_string2list);
    Scm_RegisterProcedureFixed1("list->string"             , ScmOp_list2string);
    Scm_RegisterProcedureFixed1("string-copy"              , ScmOp_string_copy);
    Scm_RegisterProcedureFixed2("string-fill!"             , ScmOp_string_fill);
    Scm_RegisterProcedureFixed1("vector?"                  , ScmOp_vectorp);
    Scm_RegisterProcedureVariadic1("make-vector" , ScmOp_make_vector);
    Scm_RegisterProcedureVariadic0("vector"      , ScmOp_vector);
    Scm_RegisterProcedureFixed1("vector-length"            , ScmOp_vector_length);
    Scm_RegisterProcedureFixed2("vector-ref"               , ScmOp_vector_ref);
    Scm_RegisterProcedureFixed3("vector-set!"              , ScmOp_vector_set);
    Scm_RegisterProcedureFixed1("vector->list"             , ScmOp_vector2list);
    Scm_RegisterProcedureFixed1("list->vector"             , ScmOp_list2vector);
    Scm_RegisterProcedureFixed2("vector-fill!"             , ScmOp_vector_fill);
    Scm_RegisterProcedureFixed1("procedure?"               , ScmOp_procedurep);
    Scm_RegisterProcedureVariadic1("map"         , ScmOp_map);
    Scm_RegisterProcedureVariadic1("for-each"    , ScmOp_for_each);
    Scm_RegisterProcedureFixed1("force"          , ScmOp_force);
    Scm_RegisterProcedureVariadic0("values"          , ScmOp_values);
    Scm_RegisterProcedureFixedTailRec1("call-with-current-continuation", ScmOp_call_with_current_continuation);
    Scm_RegisterProcedureFixedTailRec2("call-with-values", ScmOp_call_with_values);
    Scm_RegisterProcedureFixed3("dynamic-wind", ScmOp_dynamic_wind);
    /* io.c */
    Scm_RegisterProcedureFixed2("call-with-input-file"     , ScmOp_call_with_input_file);
    Scm_RegisterProcedureFixed2("call-with-output-file"    , ScmOp_call_with_output_file);
    Scm_RegisterProcedureFixed1("input-port?"              , ScmOp_input_portp);
    Scm_RegisterProcedureFixed1("output-port?"             , ScmOp_output_portp);
    Scm_RegisterProcedureFixed0("current-input-port"       , ScmOp_current_input_port);
    Scm_RegisterProcedureFixed0("current-output-port"      , ScmOp_current_output_port);
    Scm_RegisterProcedureFixed2("with-input-from-file"     , ScmOp_with_input_from_file);
    Scm_RegisterProcedureFixed2("with-output-to-file"      , ScmOp_with_output_to_file);
    Scm_RegisterProcedureFixed1("open-input-file"          , ScmOp_open_input_file);
    Scm_RegisterProcedureFixed1("open-output-file"         , ScmOp_open_output_file);
    Scm_RegisterProcedureFixed1("close-input-port"         , ScmOp_close_input_port);
    Scm_RegisterProcedureFixed1("close-output-port"        , ScmOp_close_output_port);
    Scm_RegisterProcedureFixed1("eof-object?"              , ScmOp_eof_objectp);
    Scm_RegisterProcedureVariadic0("read"        , ScmOp_read);
    Scm_RegisterProcedureVariadic0("read-char"   , ScmOp_read_char);
    Scm_RegisterProcedureVariadic0("peek-char"   , ScmOp_peek_char);
    Scm_RegisterProcedureVariadic0("char-ready?" , ScmOp_char_readyp);
    Scm_RegisterProcedureVariadic1("write"       , ScmOp_write);
    Scm_RegisterProcedureVariadic1("display"     , ScmOp_display);
    Scm_RegisterProcedureVariadic0("newline"     , ScmOp_newline);
    Scm_RegisterProcedureVariadic1("write-char"      , ScmOp_write_char);
    Scm_RegisterProcedureFixed1("load"                     , ScmOp_load);

    /*=======================================================================
      Optional Syntaxes and Procedures
    =======================================================================*/
#if SCM_USE_NONSTD_FEATURES
    Scm_RegisterProcedureVariadic1("symbol-bound?"     , ScmOp_symbol_boundp);

    Scm_RegisterProcedureFixed0("load-path"                , ScmOp_load_path);
    Scm_RegisterProcedureFixed1("require"                  , ScmOp_require);
    Scm_RegisterProcedureFixed1("provide"                  , ScmOp_provide);
    Scm_RegisterProcedureFixed1("provided?"                , ScmOp_providedp);
    Scm_RegisterProcedureFixed1("file-exists?"             , ScmOp_file_existsp);
    Scm_RegisterProcedureFixed1("delete-file"              , ScmOp_delete_file);

    Scm_RegisterSyntaxFixed1("use"                         , ScmExp_use);
    Scm_DefineAlias("call/cc", "call-with-current-continuation");
#endif

#if SCM_EXCEPTION_HANDLING
    ScmExp_use(Scm_Intern("srfi-34"), SCM_INTERACTION_ENV);
#endif
}

void SigScm_Finalize()
{
    SigScm_FinalizeStorage();
}

void Scm_DefineAlias(const char *newsym, const char *sym)
{
    SCM_SYMBOL_SET_VCELL(Scm_Intern(newsym),
                         SCM_SYMBOL_VCELL(Scm_Intern(sym)));
}

/*
 * TODO:
 * - Make the interface and semantics of 'use' similar to other Scheme
 *   implementations such as Gauche. This is important to make *.scm file
 *   portable
 * - Make a *.scm file loadable via this interface (if necessary to make
 *   similar to other Scheme implementations), and make consistent with
 *   'require'
 * - Make the 'module' concept similar to other Scheme implementations and R6RS
 * - Make the module_info_table dynamically registerable for dynamic loadable
 *   objects (if necessary)
 */
ScmObj ScmExp_use(ScmObj feature, ScmObj env)
{
    struct module_info *mod = NULL;
    ScmObj feature_str = SCM_FALSE;
    DECLARE_FUNCTION("use", SyntaxFixed1);

    ASSERT_SYMBOLP(feature);

    for (mod = module_info_table; mod->name; mod++) {
        if (EQ(feature, Scm_Intern(mod->name))) {
            feature_str = ScmOp_symbol2string(feature);
            if (FALSEP(ScmOp_providedp(feature_str))) {
                (*mod->initializer)();
                ScmOp_provide(feature_str);
            }
            return SCM_TRUE;
        }
    }

    return SCM_FALSE;
}

ScmObj Scm_eval_c_string(const char *exp)
{
#if !SCM_GCC4_READY_GC
    ScmObj stack_start = NULL;
#endif
    ScmObj ret         = SCM_NULL;

#if SCM_GCC4_READY_GC
    SCM_GC_PROTECTED_CALL(ret, ScmObj, Scm_eval_c_string_internal, (exp));
#else
    /* start protecting stack */
    SigScm_GC_ProtectStack(&stack_start);

    ret = Scm_eval_c_string_internal(exp);

    /* now no need to protect stack */
    SigScm_GC_UnprotectStack(&stack_start);
#endif

    return ret;
}

ScmObj Scm_eval_c_string_internal(const char *exp)
{
    ScmObj str_port    = SCM_FALSE;
    ScmObj ret         = SCM_FALSE;
#if SCM_USE_NEWPORT
    ScmBytePort *bport;
    ScmCharPort *cport;

    bport = ScmInputStrPort_new_const(exp, NULL);
    cport = ScmSingleByteCharPort_new(bport);
    str_port = Scm_NewPort(cport, SCM_PORTFLAG_INPUT);
#else
    str_port = Scm_NewStringPort(exp, PORT_INPUT);
#endif

    ret = SigScm_Read(str_port);
    ret = EVAL(ret, SCM_INTERACTION_ENV);

#if SCM_COMPAT_SIOD
    scm_return_value = ret;
#endif

    return ret;
}

#if SCM_COMPAT_SIOD
ScmObj Scm_return_value(void)
{
    return scm_return_value;
}
#endif

/*===========================================================================
  Scheme Function Export Related Functions
===========================================================================*/
/* New Interfaces */
static int Scm_RegisterFunc(const char *name, ScmFuncType c_func, enum ScmFuncTypeCode type)
{
    ScmObj sym  = Scm_Intern(name);
    ScmObj func = Scm_NewFunc(type, c_func);

    /* TODO: reject bad TYPE */
    SCM_SYMBOL_SET_VCELL(sym, func);
    return 1;
}

/* Not implemented yet. */
void Scm_RegisterReductionOperator(const char *name, ScmObj (*func)(ScmObj, ScmObj, enum ScmReductionState*))
{
    Scm_RegisterFunc(name, func, SCM_REDUCTION_OPERATOR);
}

/* So, yeah, um... this isn't really such a big deal if you think
 * about W32.... */
void Scm_RegisterSyntaxFixed0(const char *name, ScmObj (*func)(ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_FIXED | 0);
}

#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterSyntaxFixed1(const char *name, ScmObj (*func)(ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_FIXED | 1);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterSyntaxFixed2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_FIXED | 2);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterSyntaxFixed3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_FIXED | 3);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterSyntaxFixed4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_FIXED | 4);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterSyntaxFixed5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_FIXED | 5);
}
#endif

void Scm_RegisterSyntaxFixedTailRec0(const char *name, ScmObj (*func)(ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_FIXED_TAIL_REC | 0);
}

#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterSyntaxFixedTailRec1(const char *name, ScmObj (*func)(ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_FIXED_TAIL_REC | 1);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterSyntaxFixedTailRec2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_FIXED_TAIL_REC | 2);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterSyntaxFixedTailRec3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_FIXED_TAIL_REC | 3);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterSyntaxFixedTailRec4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_FIXED_TAIL_REC | 4);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterSyntaxFixedTailRec5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_FIXED_TAIL_REC | 5);
}
#endif

void Scm_RegisterSyntaxVariadic0(const char *name, ScmObj (*func)(ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_VARIADIC | 0);
}

#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterSyntaxVariadic1(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_VARIADIC | 1);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterSyntaxVariadic2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_VARIADIC | 2);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterSyntaxVariadic3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_VARIADIC | 3);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterSyntaxVariadic4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_VARIADIC | 4);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterSyntaxVariadic5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_VARIADIC | 5);
}
#endif

void Scm_RegisterSyntaxVariadicTailRec0(const char *name, ScmObj (*func)(ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_VARIADIC_TAIL_REC | 0);
}

#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterSyntaxVariadicTailRec1(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_VARIADIC_TAIL_REC | 1);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterSyntaxVariadicTailRec2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_VARIADIC_TAIL_REC | 2);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterSyntaxVariadicTailRec3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_VARIADIC_TAIL_REC | 3);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterSyntaxVariadicTailRec4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_VARIADIC_TAIL_REC | 4);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterSyntaxVariadicTailRec5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_SYNTAX_VARIADIC_TAIL_REC | 5);
}
#endif

void Scm_RegisterProcedureFixed0(const char *name, ScmObj (*func)())
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_FIXED | 0);
}

#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterProcedureFixed1(const char *name, ScmObj (*func)(ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_FIXED | 1);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterProcedureFixed2(const char *name, ScmObj (*func)(ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_FIXED | 2);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterProcedureFixed3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_FIXED | 3);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterProcedureFixed4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_FIXED | 4);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterProcedureFixed5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_FIXED | 5);
}
#endif

void Scm_RegisterProcedureFixedTailRec0(const char *name, ScmObj (*func)(ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_FIXED_TAIL_REC | 0);
}

#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterProcedureFixedTailRec1(const char *name, ScmObj (*func)(ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_FIXED_TAIL_REC | 1);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterProcedureFixedTailRec2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_FIXED_TAIL_REC | 2);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterProcedureFixedTailRec3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_FIXED_TAIL_REC | 3);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterProcedureFixedTailRec4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_FIXED_TAIL_REC | 4);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterProcedureFixedTailRec5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_FIXED_TAIL_REC | 5);
}
#endif

void Scm_RegisterProcedureVariadic0(const char *name, ScmObj (*func)(ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_VARIADIC | 0);
}

#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterProcedureVariadic1(const char *name, ScmObj (*func)(ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_VARIADIC | 1);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterProcedureVariadic2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_VARIADIC | 2);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterProcedureVariadic3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_VARIADIC | 3);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterProcedureVariadic4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_VARIADIC | 4);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterProcedureVariadic5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_VARIADIC | 5);
}
#endif

void Scm_RegisterProcedureVariadicTailRec0(const char *name, ScmObj (*func)(ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_VARIADIC_TAIL_REC | 0);
}

#if SCM_FUNCTYPE_MAND_MAX >= 1
void Scm_RegisterProcedureVariadicTailRec1(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_VARIADIC_TAIL_REC | 1);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 2
void Scm_RegisterProcedureVariadicTailRec2(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_VARIADIC_TAIL_REC | 2);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 3
void Scm_RegisterProcedureVariadicTailRec3(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_VARIADIC_TAIL_REC | 3);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 4
void Scm_RegisterProcedureVariadicTailRec4(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_VARIADIC_TAIL_REC | 4);
}
#endif

#if SCM_FUNCTYPE_MAND_MAX >= 5
void Scm_RegisterProcedureVariadicTailRec5(const char *name, ScmObj (*func)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState*))
{
    Scm_RegisterFunc(name, func, SCM_PROCEDURE_VARIADIC_TAIL_REC | 5);
}
#endif
