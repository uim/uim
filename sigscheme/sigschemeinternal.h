/*===========================================================================
 *  FileName : sigschemeinternal.h
 *  About    : variable and function definitions for internal use
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
#ifndef __SIGSCHEMEINTERNAL_H
#define __SIGSCHEMEINTERNAL_H

/*=======================================
   System Include
=======================================*/
#include <stdarg.h>

/*=======================================
   Local Include
=======================================*/
#include "sigscheme.h"

/*=======================================
   Type Definitions
=======================================*/
/* for debugging */
struct trace_frame {
    struct trace_frame *prev;
    ScmObj obj;
    ScmObj env;    
};

typedef struct ScmSpecialCharInfo_ ScmSpecialCharInfo;
struct ScmSpecialCharInfo_ {
    unsigned int code;    /* character code as ASCII/Unicode */
    const char *esc_seq;  /* escape sequence as string */
    const char *lex_rep;  /* lexical representation as character object */
};

/*=======================================
   Variable Declarations
=======================================*/
/* datas.c */
extern ScmObj *scm_stack_start_pointer;

/* eval.c */
extern struct trace_frame *scm_trace_root;

/* io.c */
extern ScmObj scm_current_input_port;
extern ScmObj scm_current_output_port;
extern ScmObj scm_current_error_port;
extern ScmObj SigScm_features;
extern const ScmSpecialCharInfo Scm_special_char_table[];

/* datas.c */
#if SCM_USE_VALUECONS
extern ScmObj SigScm_null_values;
#endif

/*=======================================
   Macro Declarations
=======================================*/
#ifndef FALSE
#define FALSE 0
#endif /* FALSE */
#ifndef TRUE
#define TRUE  (!FALSE)
#endif /* TRUE */

/* FreeCell Handling Macros */
#if SCM_OBJ_COMPACT
#define SCM_FREECELLP(a)            (SCM_CONSP(a))
#define SCM_AS_FREECELL(a)          (SCM_ASSERT_TYPE(SCM_CONSP(a), (a)))
#define SCM_FREECELL_CAR(a)         (SCM_CAR(a))
#define SCM_FREECELL_CDR(a)         (SCM_CDR(a))
#define SCM_ENTYPE_FREECELL(a)      (SCM_ENTYPE_CONS(a))
#define SCM_FREECELL_SET_CAR(a,car) (SCM_CONS_SET_CAR(a))
#define SCM_FREECELL_SET_CDR(a,cdr) (SCM_CONS_SET_CDR(a))
#else
#define SCM_FREECELLP(a)            (SCM_TYPE(a) == ScmFreeCell)
#define SCM_AS_FREECELL(a)          (SCM_ASSERT_TYPE(SCM_FREECELLP(a), (a)))
#define SCM_FREECELL_CAR(a)         (SCM_AS_FREECELL(a)->obj.cons.car)
#define SCM_FREECELL_CDR(a)         (SCM_AS_FREECELL(a)->obj.cons.cdr)
#define SCM_ENTYPE_FREECELL(a)      (SCM_ENTYPE((a), ScmFreeCell))
#define SCM_FREECELL_SET_CAR(a,car) (SCM_FREECELL_CAR(a) = car)
#define SCM_FREECELL_SET_CDR(a,cdr) (SCM_FREECELL_CDR(a) = cdr)
#endif

/* Prefix-less Abbreviation Names For Convenient Internal Use */
#define EQ             SCM_EQ
#define NULLP          SCM_NULLP
#define FALSEP         SCM_FALSEP
#define NFALSEP        SCM_NFALSEP
#define EOFP           SCM_EOFP

#define CAR            SCM_CAR
#define CDR            SCM_CDR
#define SET_CAR        SCM_CONS_SET_CAR
#define SET_CDR        SCM_CONS_SET_CDR
#define CAAR           SCM_CAAR
#define CADR           SCM_CADR
#define CDAR           SCM_CDAR
#define CDDR           SCM_CDDR

#define CONS           SCM_CONS
#define LIST_1         SCM_LIST_1
#define LIST_2         SCM_LIST_2
#define LIST_3         SCM_LIST_3
#define LIST_4         SCM_LIST_4
#define LIST_5         SCM_LIST_5

#define DEREF          SCM_DEREF
#define SET            SCM_SET
#define REF_CAR        SCM_REF_CAR
#define REF_CDR        SCM_REF_CDR

#define EVAL           SCM_EVAL

#define INTP           SCM_INTP
#define CONSP          SCM_CONSP
#define SYMBOLP        SCM_SYMBOLP
#define CHARP          SCM_CHARP
#define STRINGP        SCM_STRINGP
#define FUNCP          SCM_FUNCP
#define SYNTAXP        SCM_SYNTAXP
#define CLOSUREP       SCM_CLOSUREP
#define PROCEDUREP     SCM_PROCEDUREP
#define VECTORP        SCM_VECTORP
#define PORTP          SCM_PORTP
#define CONTINUATIONP  SCM_CONTINUATIONP
#if SCM_USE_VALUECONS
#define NULLVALUESP    SCM_NULLVALUESP
#endif /* SCM_USE_VALUECONS */
#define VALUEPACKETP   SCM_VALUEPACKETP
#define FREECELLP      SCM_FREECELLP
#define C_POINTERP     SCM_C_POINTERP
#define C_FUNCPOINTERP SCM_C_FUNCPOINTERP
#define ENVP           SCM_ENVP

#define CDBG           SCM_CDBG
#define DBG            SCM_DBG

/*
 * Abbrev name for these constants are not provided since it involves some
 * consistency problems and confusions. Use the canonical names always.
 *
 * SCM_NULL
 * SCM_TRUE
 * SCM_FALSE
 * SCM_EOF
 * SCM_QUOTE
 * SCM_QUASIQUOTE
 * SCM_UNQUOTE
 * SCM_UNQUOTE_SPLICING
 * SCM_UNBOUND
 * SCM_UNDEF
 */

/* Macros For Argnument Number Checking */
/*
 * TODO: rename appropriately
 * Since 'CHECK' sounds a positive check as like as 'ASSERT', its opposite
 * meaning may confuse users. So I suggest another name such as 'UNFILLED'.
 *   -- YamaKen 2005-09-05
 */
#define CHECK_1_ARG(arg)  (NULLP(arg))
#define CHECK_2_ARGS(arg) (CHECK_1_ARG(arg)  || NULLP(CDR(arg)))
#define CHECK_3_ARGS(arg) (CHECK_2_ARGS(arg) || NULLP(CDDR(arg)))
#define CHECK_4_ARGS(arg) (CHECK_3_ARGS(arg) || NULLP(CDR(CDDR(arg))))
#define CHECK_5_ARGS(arg) (CHECK_4_ARGS(arg) || NULLP(CDDR(CDDR(arg))))

/*
 * Macros For List Element Extraction With Safety Check
 *
 * SCM_SHIFT_*() safely and efficiently extracts elements of a list into
 * arbitrary storages (Suppose 'shift' function of scripting languages).
 *
 * The macro overwrites the argument variable 'lst' as list iterator, and
 * returns rest list after requested number of elements have been
 * extracted. Caller can test whether the list has been empty or not by
 * applying NULLP to the result. If a shotage of the list has been occurred
 * before extracting all elements, the iteration stops with false value, and
 * the lst becomes to empty list. The macro itself does not have any error
 * handlings. Caller must do it appropriately by referencing the result value.
 */
#define SCM_SHIFT_RAW(elm, lst)                                              \
    ((!NULLP(lst)) && ((elm) = CAR(lst), (lst) = CDR(lst), (lst)))

#define SCM_SHIFT_RAW_1(elm0, lst)                                           \
    (SCM_SHIFT_RAW(elm0, lst) ? (lst) : 0)

#define SCM_SHIFT_RAW_2(elm0, elm1, lst)                                     \
    ((SCM_SHIFT_RAW(elm0, lst)                                               \
      && SCM_SHIFT_RAW(elm1, lst)) ? (lst) : 0)

#define SCM_SHIFT_RAW_3(elm0, elm1, elm2, lst)                               \
    ((SCM_SHIFT_RAW(elm0, lst)                                               \
      && SCM_SHIFT_RAW(elm1, lst)                                            \
      && SCM_SHIFT_RAW(elm2, lst)) ? (lst) : 0)

/* Obscures identifier ID. */
#define SCM_MANGLE(id) Scm_internal_##id

#define VALIDP(obj)   (!EQ((obj), SCM_INVALID))
#define INVALIDP(obj) (EQ((obj), SCM_INVALID))

/* Declares the current function name as seen by Scheme codes.  TYPE
 * is ignored, but we may use it in the future to implement a stub
 * generator.  This macro can be invoked only at the beginning of a
 * function body, right after local variable declarations. */
#define DECLARE_FUNCTION(func_name, type) \
    const char *SCM_MANGLE(name); \
    ScmObj SCM_MANGLE(tmp); \
    SCM_MANGLE(name) = func_name; \
    SCM_MANGLE(tmp)  = SCM_INVALID /* No semicolon here. */

/* DECLARE_FUNCTION without the functype.
 * FIXME: is there a better name? */
#define DECLARE_INTERNAL_FUNCTION(name) DECLARE_FUNCTION((name), ignored)

/* Signals an error.  The current function name and the message are
   sent to the error port.  The message is formatted by vfprintf(). */
#define ERR SigScm_Error

/* Signals an error that occured on an object.  The current function
 * name, the message, then the object, are written (with `write') to
 * the error port. */
#define ERR_OBJ(msg, obj) Scm_ErrorObj(SCM_MANGLE(name), msg, obj)

/* ASSERT_NO_MORE_ARG() asserts that the variadic argument list has
 * been exhausted.  The assertion is implicit in NO_MORE_ARG(), so
 * usually you don't have to call it explicitly.
 * ASSERT_PROPER_ARG_LIST() should be used when scanning is ended
 * prematurely, e.g. if an argument to "and" evaluates to #f.  Both
 * macros expand to no-ops #if !SCM_STRICT_ARGCHECK.
 */
#if SCM_STRICT_ARGCHECK
#define NO_MORE_ARG(args) \
    (!CONSP(args) \
     && (NULLP(args) \
         || (ERR_OBJ("improper argument list terminator", (args)), 1)))
#define ASSERT_NO_MORE_ARG(args) \
    (NO_MORE_ARG(args) || (ERR_OBJ("superfluous argument(s)", (args)), 1))
#define ASSERT_PROPER_ARG_LIST(args) \
    (ScmOp_c_length(args) >= 0 \
     || (ERR_OBJ("bad argument list", (args)), 1))
#else  /* not SCM_STRICT_ARGCHECK */
#define NO_MORE_ARG(args) (!CONSP(args))
#define ASSERT_NO_MORE_ARG(args)
#define ASSERT_PROPER_ARG_LIST(args)
#endif /* not SCM_STRICT_ARGCHECK */

/* Destructively retreives the first element of an argument list.  If
 * ARGS doesn't contain enough arguments, return SCM_INVALID. */
#define POP_ARG(args) \
     (CONSP(args) \
      ? (SCM_MANGLE(tmp) = CAR(args), (args) = CDR(args), SCM_MANGLE(tmp)) \
      : SCM_INVALID)

/* Like POP_ARG(), but signals an error if no argument is
   available. */
#define MUST_POP_ARG(args) \
     (CONSP(args) \
      ? (SCM_MANGLE(tmp) = CAR(args), (args) = CDR(args), SCM_MANGLE(tmp)) \
      : (ERR("missing argument(s)"), NULL))

#define ASSERT_TYPE(pred, typename, obj) \
    (pred(obj) || (ERR_OBJ(typename " required but got", (obj)), 1))

#define ASSERT_INTP(obj)     ASSERT_TYPE(INTP, "integer", (obj))
#define ASSERT_CONSP(obj)    ASSERT_TYPE(CONSP, "pair", (obj))
#define ASSERT_SYMBOLP(obj)  ASSERT_TYPE(SYMBOLP, "symbol", (obj))
#define ASSERT_CHARP(obj)    ASSERT_TYPE(CHARP, "character", (obj))
#define ASSERT_STRINGP(obj)  ASSERT_TYPE(STRINGP, "string", (obj))
#define ASSERT_FUNCP(obj)    ASSERT_TYPE(FUNCP, "function", (obj))
#define ASSERT_CLOSUREP(obj) ASSERT_TYPE(CLOSUREP, "closure", (obj))
#define ASSERT_VECTORP(obj)  ASSERT_TYPE(VECTORP, "vector", (obj))
#define ASSERT_PORTP(obj)    ASSERT_TYPE(PORTP, "port", (obj))
#define ASSERT_CONTINUATIONP(obj) ASSERT_TYPE(CONTINUATIONP, "continuation", (obj))
#define ASSERT_PROCEDUREP(obj) ASSERT_TYPE(PROCEDUREP, "procedure", (obj))
#define ASSERT_ENVP(obj)     ASSERT_TYPE(ENVP, "environment specifier", (obj))

#if SCM_USE_SRFI34
/* Macros and Variables For Handling Exception Handlers based on SRFI-34 */
extern ScmObj scm_exception_handlers;
extern ScmObj scm_exception_continuations;

#define CURRENT_EXCEPTION_HANDLER()                             \
    (FALSEP(scm_exception_handlers)                             \
     ? scm_exception_handlers : CAR(scm_exception_handlers))
#define PUSH_EXCEPTION_HANDLER(handler)                                 \
    (scm_exception_handlers = CONS((handler), scm_exception_handlers))
#define POP_EXCEPTION_HANDLER()                 \
    (scm_exception_handlers = CDR(scm_exception_handlers))

#define CURRENT_EXCEPTION_CONTINUATION()        \
    (FALSEP(scm_exception_continuations) ? scm_exception_continuations : CAR(scm_exception_continuations))
#define PUSH_EXCEPTION_CONTINUATION(cont)                               \
    (scm_exception_continuations = CONS((cont), scm_exception_continuations))
#define POP_EXCEPTION_CONTINUATION()            \
    (scm_exception_continuations = CDR(scm_exception_continuations))
#endif /* SCM_USE_SRFI34 */

/*=======================================
   Function Declarations
=======================================*/
/* datas.c */
void SigScm_InitStorage(void);
void SigScm_FinalizeStorage(void);
ScmObj Scm_CallWithCurrentContinuation(ScmObj proc, ScmEvalState *eval_state);
void Scm_CallContinuation(ScmObj cont, ScmObj ret);
ScmObj Scm_DynamicWind(ScmObj before, ScmObj thunk, ScmObj after);

/* eval.c */
/* environment related functions */
ScmObj Scm_ExtendEnvironment(ScmObj vars, ScmObj vals, ScmObj env);
ScmObj Scm_AddEnvironment(ScmObj var, ScmObj val, ScmObj env);
ScmObj Scm_LookupEnvironment(ScmObj var, ScmObj env);
ScmObj Scm_SymbolValue(ScmObj var, ScmObj env);

ScmObj Scm_eval(ScmObj obj, ScmObj env);
ScmObj Scm_tailcall(ScmObj proc, ScmObj args, ScmEvalState *eval_state);

/* error.c */
void SigScm_ShowErrorHeader(void);
/* TODO: Move these functions to io.c */
void SigScm_PortPrintf(ScmObj port, const char *fmt, ...);
void SigScm_VPortPrintf(ScmObj port, const char *fmt, va_list args);
void SigScm_ErrorPrintf(const char *fmt, ...);
void SigScm_VErrorPrintf(const char *fmt, va_list args);
void SigScm_ErrorNewline(void);
void Scm_ErrorObj(const char *func_name, const char *msg, ScmObj obj);

/* operations.c */
int ScmOp_c_length(ScmObj lst);

#endif /* __SIGSCHEMEINTERNAL_H */
