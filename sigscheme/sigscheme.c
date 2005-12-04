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
#include <stdlib.h>
#include <stdio.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"
#include "baseport.h"
#include "fileport.h"
#include "strport.h"
#if SCM_USE_MULTIBYTE_CHAR
#include "mbcport.h"
#else /* SCM_USE_MULTIBYTE_CHAR */
#include "sbcport.h"
#endif /* SCM_USE_MULTIBYTE_CHAR */

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
ScmObj Scm_sym_quote, Scm_sym_quasiquote;
ScmObj Scm_sym_unquote, Scm_sym_unquote_splicing;
ScmObj Scm_sym_else, Scm_sym_yields;

static int scm_initialized;
static ScmObj features;

#if SCM_COMPAT_SIOD
static ScmObj scm_return_value    = NULL;
#endif

static struct module_info module_info_table[] = {
#if SCM_USE_NONSTD_FEATURES
    {"sscm", SigScm_Initialize_NONSTD_FEATURES},
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
    SigScm_InitError();

    /*=======================================================================
      Predefined Symbols and Variables
    =======================================================================*/
    Scm_sym_quote            = Scm_Intern("quote");
    Scm_sym_quasiquote       = Scm_Intern("quasiquote");
    Scm_sym_unquote          = Scm_Intern("unquote");
    Scm_sym_unquote_splicing = Scm_Intern("unquote-splicing");
    Scm_sym_else             = Scm_Intern("else");
    Scm_sym_yields           = Scm_Intern("=>");

    features = SCM_NULL;

    /*=======================================================================
      Preallocated Ports
    =======================================================================*/
    Scm_fileport_init();
#if SCM_USE_MULTIBYTE_CHAR
    Scm_mbcport_init();
#else
    Scm_sbcport_init();
#endif

    scm_current_input_port  = Scm_MakeSharedFilePort(stdin, "stdin",
                                                     SCM_PORTFLAG_INPUT);
    scm_current_output_port = Scm_MakeSharedFilePort(stdout, "stdout",
                                                     SCM_PORTFLAG_OUTPUT);
    scm_current_error_port  = Scm_MakeSharedFilePort(stderr, "stderr",
                                                     SCM_PORTFLAG_OUTPUT);
    SigScm_GC_Protect(&scm_current_input_port);
    SigScm_GC_Protect(&scm_current_output_port);
    SigScm_GC_Protect(&scm_current_error_port);

    /*=======================================================================
      Register Built-in Functions
    =======================================================================*/
    /* R5RS Functions */
    REGISTER_FUNC_TABLE(r5rs_func_info_table);
    Scm_DefineAlias("integer?"                  , "number?");

#if SCM_USE_DEEP_CADRS
    /* Deep c[ad]+r Functions */
    REGISTER_FUNC_TABLE(r5rs_deepcadrs_func_info_table);
#endif

    /*=======================================================================
      Fixing up
    =======================================================================*/
#if SCM_USE_NONSTD_FEATURES
    Scm_Use("sscm");
#endif
    /* to evaluate SigScheme-dependent codes conditionally */
    Scm_Provide(Scm_NewImmutableStringCopying("sigscheme"));
#if SCM_STRICT_R5RS
    Scm_Provide(Scm_NewImmutableStringCopying("strict-r5rs"));
#endif
#if SCM_COMPAT_SIOD_BUGS
    Scm_Provide(Scm_NewImmutableStringCopying("siod-bugs"));
#endif
    scm_initialized = TRUE;
}

void SigScm_Finalize()
{
    SigScm_FinalizeStorage();
    scm_initialized = FALSE;
}

void Scm_DefineAlias(const char *newsym, const char *sym)
{
    SCM_SYMBOL_SET_VCELL(Scm_Intern(newsym),
                         SCM_SYMBOL_VCELL(Scm_Intern(sym)));
}

void Scm_Provide(ScmObj feature)
{
    features = CONS(feature, features);
}

int Scm_Providedp(ScmObj feature)
{
    return NFALSEP(ScmOp_member(feature, features));
}

int Scm_Use(const char *feature)
{
    ScmObj ok;
    SCM_ASSERT(feature);

    ok = ScmExp_use(Scm_Intern(feature), SCM_INTERACTION_ENV);
    return NFALSEP(ok);
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
    ScmBytePort *bport;

    bport = ScmInputStrPort_new_const(exp, NULL);
    str_port = Scm_NewPort(Scm_NewCharPort(bport), SCM_PORTFLAG_INPUT);

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

/* TODO: parse properly */
/* don't access ScmObj if (!scm_initialized) */
char **Scm_InterpretArgv(char **argv)
{
    char **argp, **rest;
    const char *encoding;
    ScmCharCodec *specified_codec;
    ScmObj err_obj; /* dont' initialize */
    DECLARE_INTERNAL_FUNCTION("Scm_InterpretArgv");

    encoding = NULL;
    argp = (strcmp(argv[0], "/usr/bin/env") == 0) ? &argv[2] : &argv[1];

    for (; *argp; argp++) {
        /* script name appeared */
        if (*argp[0] != '-')
            break;

        /* character encoding */
        if (strcmp(*argp, "-C") == 0) {
            encoding = *++argp;
            if (!*argp) {
                if (scm_initialized) {
                    ERR("no encoding name specified");
                } else {
                    fprintf(stderr, "%sno encoding name specified\n",
                            SCM_ERR_HEADER);
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    rest = argp;

    if (encoding) {
        specified_codec = Scm_mb_find_codec(encoding);
        if (!specified_codec) {
            if (scm_initialized) {
                err_obj = Scm_NewImmutableStringCopying(encoding);
                Scm_FreeArgv(argv);
                ERR_OBJ("unsupported encoding", err_obj);
            } else {
                fprintf(stderr, "%sunsupported encoding: %s\n",
                        SCM_ERR_HEADER, encoding);
                exit(EXIT_FAILURE);
            }
        }
        Scm_current_char_codec = specified_codec;
    }

    return rest;
}

void Scm_FreeArgv(char **argv)
{
    char **argp;

    for (argp = &argv[0]; *argp; argp++) {
        free(*argp);
    }
    free(argv);
}

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
