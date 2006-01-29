/*===========================================================================
 *  FileName : sigschemeinternal.h
 *  About    : variable and function definitions for internal use
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
#ifndef __SIGSCHEMEINTERNAL_H
#define __SIGSCHEMEINTERNAL_H

/*=======================================
   System Include
=======================================*/
#include <limits.h>
#include <stddef.h>
#include <stdarg.h>

/*=======================================
   Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemefunctable.h"
#include "encoding.h"

/*=======================================
   Type Definitions
=======================================*/
typedef struct ScmSpecialCharInfo_ ScmSpecialCharInfo;
struct ScmSpecialCharInfo_ {
    scm_ichar_t code;     /* character code as ASCII/Unicode */
    const char *esc_seq;  /* escape sequence as string */
    const char *lex_rep;  /* lexical representation as character object */
};

/*=======================================
   Variable Declarations
=======================================*/
/* sigscheme.c */
extern ScmCharCodec *scm_identifier_codec;

/* io.c */
extern ScmObj scm_in;
extern ScmObj scm_out;
extern ScmObj scm_err;

/* write.c */
extern void (*scm_writess_func)(ScmObj port, ScmObj obj);

/* read.c */
extern const ScmSpecialCharInfo scm_special_char_table[];

/* storage.c */
#if SCM_USE_VALUECONS
extern ScmObj scm_null_values;
#endif

/*=======================================
   Macro Declarations
=======================================*/
/* trace stack for debugging */
#define MAKE_TRACE_FRAME(obj, env) CONS((obj), (env))
#define TRACE_FRAME_OBJ CAR
#define TRACE_FRAME_ENV CDR

#define SCM_ENTYPE_INT(o)            SCM_SAL_ENTYPE_INT(o)
#define SCM_ENTYPE_CONS(o)           SCM_SAL_ENTYPE_CONS(o)
#define SCM_ENTYPE_SYMBOL(o)         SCM_SAL_ENTYPE_SYMBOL(o)
#define SCM_ENTYPE_CHAR(o)           SCM_SAL_ENTYPE_CHAR(o)
#define SCM_ENTYPE_STRING(o)         SCM_SAL_ENTYPE_STRING(o)
#define SCM_ENTYPE_FUNC(o)           SCM_SAL_ENTYPE_FUNC(o)
#define SCM_ENTYPE_CLOSURE(o)        SCM_SAL_ENTYPE_CLOSURE(o)
#define SCM_ENTYPE_VECTOR(o)         SCM_SAL_ENTYPE_VECTOR(o)
#define SCM_ENTYPE_PORT(o)           SCM_SAL_ENTYPE_PORT(o)
#define SCM_ENTYPE_CONTINUATION(o)   SCM_SAL_ENTYPE_CONTINUATION(o)
#define SCM_ENTYPE_C_POINTER(o)      SCM_SAL_ENTYPE_C_POINTER(o)
#define SCM_ENTYPE_C_FUNCPOINTER(o)  SCM_SAL_ENTYPE_C_FUNCPOINTER(o)

/* Extraction of a valuepacket is granted only for SigScheme-internals */
#define SCM_ENTYPE_VALUEPACKET(o)    SCM_SAL_ENTYPE_VALUEPACKET(o)
#define SCM_VALUEPACKET_VALUES(o)    SCM_SAL_VALUEPACKET_VALUES(o)
#if SCM_USE_VALUECONS
#define SCM_NULLVALUESP(o)           SCM_SAL_NULLVALUESP(o)
#define SCM_VALUECONS_CAR(o)         SCM_SAL_VALUECONS_CAR(o)
#define SCM_VALUECONS_CDR(o)         SCM_SAL_VALUECONS_CDR(o)
#else /* SCM_USE_VALUECONS */
#define SCM_VALUEPACKET_SET_VALUES(o, vals)                                  \
    SCM_SAL_VALUEPACKET_SET_VALUES((o), (vals))
#endif /* SCM_USE_VALUECONS */

#define SCM_ENTYPE_FREECELL(o)          SCM_SAL_ENTYPE_FREECELL(o)
#define SCM_AS_FREECELL(o)              SCM_SAL_AS_FREECELL(o)

#define SCM_FREECELLP(o)                SCM_SAL_FREECELLP(o)
#define SCM_FREECELL_NEXT(o)            SCM_SAL_FREECELL_NEXT(o)
#define SCM_FREECELL_FREESLOT(o)        SCM_SAL_FREECELL_FREESLOT(o)
#define SCM_FREECELL_SET_NEXT(o, next)  SCM_SAL_FREECELL_SET_NEXT((o), (next))
#define SCM_FREECELL_SET_FREESLOT(o, v) SCM_SAL_FREECELL_SET_FREESLOT((o), (v))
#define SCM_FREECELL_CLEAR_FREESLOT(o)  SCM_SAL_FREECELL_CLEAR_FREESLOT((o))

/* For optimized operation: Cleanup a destructed ScmCell *cell to a freecell
 * and chain it into freelist. */
#define SCM_RECLAIM_CELL(cell, next)    SCM_SAL_RECLAIM_CELL((cell), (next))

/* FIXME: rename appropriately */
#define SCM_IS_MARKED(o)             SCM_SAL_IS_MARKED(o)
#define SCM_IS_UNMARKED(o)           SCM_SAL_IS_UNMARKED(o)
#define SCM_DO_MARK(o)               SCM_SAL_DO_MARK(o)
#define SCM_DO_UNMARK(o)             SCM_SAL_DO_UNMARK(o)

/* Prefix-less Abbreviation Names For Convenient Internal Use */
#define SYM_QUOTE            SCM_SYM_QUOTE
#define SYM_QUASIQUOTE       SCM_SYM_QUASIQUOTE
#define SYM_UNQUOTE          SCM_SYM_UNQUOTE
#define SYM_UNQUOTE_SPLICING SCM_SYM_UNQUOTE_SPLICING

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
#define IMMUTABLE_CONS SCM_IMMUTABLE_CONS
#define LIST_1         SCM_LIST_1
#define LIST_2         SCM_LIST_2
#define LIST_3         SCM_LIST_3
#define LIST_4         SCM_LIST_4
#define LIST_5         SCM_LIST_5

#define DEREF          SCM_DEREF
#define SET            SCM_SET
#define REF_CAR        SCM_REF_CAR
#define REF_CDR        SCM_REF_CDR
#define REF_OFF_HEAP   SCM_REF_OFF_HEAP

#define EVAL           SCM_EVAL

#define MAKE_BOOL                     SCM_MAKE_BOOL
#define MAKE_INT                      SCM_MAKE_INT
#define MAKE_CONS                     SCM_MAKE_CONS
#define MAKE_IMMUTABLE_CONS           SCM_MAKE_IMMUTABLE_CONS
#define MAKE_SYMBOL                   SCM_MAKE_SYMBOL
#define MAKE_CHAR                     SCM_MAKE_CHAR

#define MAKE_STRING                   SCM_MAKE_STRING
#define MAKE_STRING_COPYING           SCM_MAKE_STRING_COPYING
#define MAKE_IMMUTABLE_STRING         SCM_MAKE_IMMUTABLE_STRING
#define MAKE_IMMUTABLE_STRING_COPYING SCM_MAKE_IMMUTABLE_STRING_COPYING
#define CONST_STRING                  SCM_CONST_STRING
#define STRLEN_UNKNOWN                SCM_STRLEN_UNKNOWN

#define MAKE_FUNC                     SCM_MAKE_FUNC
#define MAKE_CLOSURE                  SCM_MAKE_CLOSURE
#define MAKE_VECTOR                   SCM_MAKE_VECTOR
#define MAKE_IMMUTABLE_VECTOR         SCM_MAKE_IMMUTABLE_VECTOR
#define MAKE_PORT                     SCM_MAKE_PORT
#define MAKE_CONTINUATION             SCM_MAKE_CONTINUATION
#if SCM_USE_NONSTD_FEATURES
#define MAKE_C_POINTER                SCM_MAKE_C_POINTER
#define MAKE_C_FUNCPOINTER            SCM_MAKE_C_FUNCPOINTER
#endif /* SCM_USE_NONSTD_FEATURES */
#define MAKE_VALUEPACKET              SCM_MAKE_VALUEPACKET

#define NUMBERP        SCM_NUMBERP
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
#define VALID_ENVP     SCM_VALID_ENVP
#define ERROBJP        SCM_ERROBJP

#define LISTP          SCM_LISTP
#define LIST_1_P       SCM_LIST_1_P
#define LIST_2_P       SCM_LIST_2_P
#define LIST_3_P       SCM_LIST_3_P
#define LIST_4_P       SCM_LIST_4_P
#define LIST_5_P       SCM_LIST_5_P
#define PROPER_LISTP   SCM_PROPER_LISTP
#define DOTTED_LISTP   SCM_DOTTED_LISTP
#define CIRCULAR_LISTP SCM_CIRCULAR_LISTP

#define CDBG           SCM_CDBG
#define DBG            SCM_DBG

#define ENSURE_PROPER_LIST_TERMINATION SCM_ENSURE_PROPER_LIST_TERMINATION
#define CHECK_PROPER_LIST_TERMINATION  SCM_CHECK_PROPER_LIST_TERMINATION


/*
 * Abbrev name for these constants are not provided since it involves some
 * consistency problems and confusions. Use the canonical names always.
 *
 * SCM_NULL
 * SCM_TRUE
 * SCM_FALSE
 * SCM_EOF
 * SCM_UNBOUND
 * SCM_UNDEF
 */

/* Obscures identifier ID. */
#define SCM_MANGLE(id) scm_internal_##id

#define VALIDP(obj)   (!EQ((obj), SCM_INVALID))

/* Declares the current function name as seen by Scheme codes.  TYPE
 * is ignored, but we may use it in the future to implement a stub
 * generator.  This macro can be invoked only at the beginning of a
 * function body, right after local variable declarations. */
#define DECLARE_FUNCTION(func_name, type)                                    \
    const char *SCM_MANGLE(name);                                            \
    ScmObj SCM_MANGLE(tmp);                                                  \
    SCM_MANGLE(name) = func_name;                                            \
    SCM_MANGLE(tmp)  = SCM_INVALID /* No semicolon here. */

/* DECLARE_FUNCTION without the functype.
 * FIXME: is there a better name? */
#define DECLARE_INTERNAL_FUNCTION(name) DECLARE_FUNCTION((name), ignored)

/* Signals an error.  The current function name and the message are
   sent to the error port.  The message is formatted by vfprintf(). */
#define ERR scm_error

/* Signals an error that occured on an object.  The current function
 * name, the message, then the object, are written (with `write') to
 * the error port. */
#define ERR_OBJ(msg, obj) scm_error_obj(SCM_MANGLE(name), (msg), (obj))

/* ASSERT_NO_MORE_ARG() asserts that the variadic argument list has
 * been exhausted.  The assertion is implicit in NO_MORE_ARG(), so
 * usually you don't have to call it explicitly.
 * ASSERT_PROPER_ARG_LIST() should be used when scanning is ended
 * prematurely, e.g. if an argument to "and" evaluates to #f.  Both
 * macros expand to no-ops #if !SCM_STRICT_ARGCHECK.
 */
#define ENSURE_NO_MORE_ARG(args)                                             \
    (NO_MORE_ARG(args) || (ERR_OBJ("superfluous argument(s)", (args)), 1))
#define ENSURE_PROPER_ARG_LIST(args)                                         \
    (PROPER_LISTP(args) || (ERR_OBJ("bad argument list", (args)), 1))
#if SCM_STRICT_ARGCHECK
#define NO_MORE_ARG(args)                                                    \
    (!CONSP(args)                                                            \
     && (NULLP(args)                                                         \
         || (ERR_OBJ("improper argument list terminator", (args)), 1)))
#define ASSERT_NO_MORE_ARG(args)     ENSURE_NO_MORE_ARG(args)
#define ASSERT_PROPER_ARG_LIST(args) ENSURE_PROPER_ARG_LIST(args)
#else  /* not SCM_STRICT_ARGCHECK */
#define NO_MORE_ARG(args) (!CONSP(args))
#define ASSERT_NO_MORE_ARG(args)
#define ASSERT_PROPER_ARG_LIST(args)
#endif /* not SCM_STRICT_ARGCHECK */

/* Destructively retreives the first element of a list. */
#define POP(_lst)                                                            \
    (SCM_MANGLE(tmp) = CAR(_lst), (_lst) = CDR(_lst), SCM_MANGLE(tmp))

/* POP() with safety check. */
#define SAFE_POP(_lst)                                                       \
    (CONSP((_lst)) ? POP((_lst)) : SCM_INVALID)

/* Like POP(), but signals an error if no argument is available. */
#define MUST_POP_ARG(_lst)                                                   \
    (CONSP(_lst) ? POP(_lst) : (ERR("missing argument(s)"), NULL))

#define FOR_EACH_WHILE(_kar, _lst, _cond)                                    \
    while ((_cond) && ((_kar) = POP((_lst)), 1))

#define FOR_EACH(_kar, _lst) FOR_EACH_WHILE((_kar), (_lst), CONSP(_lst))

#define FOR_EACH_PAIR(_subls, _lst)                                          \
    for ((_subls) = (_lst); CONSP((_subls)); (_subls) = CDR(_subls))

/*
 * - expression part for the syntax is evaluated for each element except for
 *   the last one
 * - _elm holds the last element after an overall iteration
 * - _lst holds the terminal cdr after an overall iteration
 */
#define FOR_EACH_BUTLAST(_elm, _lst)                                         \
    SCM_ASSERT(CONSP(_lst));                                                 \
    while ((_elm) = POP(_lst), CONSP(_lst))

#define ENSURE_TYPE(pred, typename, obj)                                     \
    (pred(obj) || (ERR_OBJ(typename " required but got", (obj)), 1))

#define ENSURE_INT(obj)     ENSURE_TYPE(INTP, "integer", (obj))
#define ENSURE_CONS(obj)    ENSURE_TYPE(CONSP, "pair", (obj))
#define ENSURE_SYMBOL(obj)  ENSURE_TYPE(SYMBOLP, "symbol", (obj))
#define ENSURE_CHAR(obj)    ENSURE_TYPE(CHARP, "character", (obj))
#define ENSURE_STRING(obj)  ENSURE_TYPE(STRINGP, "string", (obj))
#define ENSURE_FUNC(obj)    ENSURE_TYPE(FUNCP, "function", (obj))
#define ENSURE_CLOSURE(obj) ENSURE_TYPE(CLOSUREP, "closure", (obj))
#define ENSURE_VECTOR(obj)  ENSURE_TYPE(VECTORP, "vector", (obj))
#define ENSURE_PORT(obj)    ENSURE_TYPE(PORTP, "port", (obj))
#define ENSURE_CONTINUATION(obj) ENSURE_TYPE(CONTINUATIONP, "continuation", (obj))
#define ENSURE_PROCEDURE(obj) ENSURE_TYPE(PROCEDUREP, "procedure", (obj))
#define ENSURE_ENV(obj)     ENSURE_TYPE(ENVP, "environment specifier", (obj))
#define ENSURE_VALID_ENV(obj)                                                \
    ENSURE_TYPE(VALID_ENVP, "valid environment specifier", (obj))
#define ENSURE_ERROBJ(obj)  ENSURE_TYPE(ERROBJP, "error object", (obj))
#define ENSURE_LIST(obj)    ENSURE_TYPE(LISTP, "list", (obj))

#define ENSURE_MUTABLE_CONS(kons)                                            \
    (SCM_CONS_MUTABLEP(kons)                                                 \
     || (ERR_OBJ("attempted to modify immutable pair", kons), 1))

#define ENSURE_MUTABLE_STRING(str)                                           \
    (SCM_STRING_MUTABLEP(str)                                                \
     || (ERR_OBJ("attempted to modify immutable string", str), 1))

#define ENSURE_MUTABLE_VECTOR(vec)                                           \
    (SCM_VECTOR_MUTABLEP(vec)                                                \
     || (ERR_OBJ("attempted to modify immutable vector", vec), 1))

#define ENSURE_STATEFUL_CODEC(codec)                                         \
    (SCM_CHARCODEC_STATEFULP(codec)                                          \
     || (ERR("%s: stateful character codec required but got: %s",            \
             SCM_MANGLE(name), SCM_CHARCODEC_ENCODING(codec)), 0))
#define ENSURE_STATELESS_CODEC(codec)                                        \
    (!SCM_CHARCODEC_STATEFULP(codec)                                         \
     || (ERR("%s: stateless character codec required but got: %s",           \
             SCM_MANGLE(name), SCM_CHARCODEC_ENCODING(codec)), 0))

#define ENSURE_ALLOCATED SCM_ENSURE_ALLOCATED

/* Macros For Handling Continuation Object */
#define INVALID_CONTINUATION_OPAQUE  NULL

/* error handlings */
#define SCM_ERR_HEADER "Error: "

#define EQVP(a, b)   (NFALSEP(scm_p_eqvp((a), (b))))
#define EQUALP(a, b) (NFALSEP(scm_p_equalp((a), (b))))
#define STRING_EQUALP(str1, str2)                                            \
    (EQ((str1), (str2))                                                      \
     || (SCM_STRING_LEN(str1) == SCM_STRING_LEN(str2)  /* rough rejection */ \
         && strcmp(SCM_STRING_STR(str1), SCM_STRING_STR(str2)) == 0))

/* result encoders for scm_length() */
#define SCM_LISTLEN_ENCODE_DOTTED(len)   (-(len))
#define SCM_LISTLEN_ENCODE_CIRCULAR(len) (SCM_INT_T_MIN)
#define SCM_LISTLEN_ENCODE_ERROR         SCM_LISTLEN_ENCODE_CIRCULAR

/*=======================================
   List Constructor
=======================================*/
typedef ScmRef ScmQueue;
#define SCM_QUEUE_INVALIDATE(_q) ((_q) = NULL)
#define SCM_QUEUE_VALIDP(_q)     (_q)
#define SCM_QUEUE_POINT_TO(_q, _out) ((_q) = SCM_REF_OFF_HEAP(_out))
#define SCM_QUEUE_ADD(_q, _dat) (SET((_q), LIST_1(_dat)),                    \
                                 (_q) = REF_CDR(DEREF(_q)))
#define SCM_QUEUE_CONST_ADD(_q, _dat)                                        \
    (SET((_q), IMMUTABLE_CONS((_dat), SCM_NULL)),                            \
     (_q) = REF_CDR(DEREF(_q)))
#define SCM_QUEUE_APPEND(_q, _lst)                                           \
    do {                                                                     \
        SET((_q), (_lst));                                                   \
        while (CONSP(DEREF(_q)))                                             \
            (_q) = REF_CDR(DEREF(_q));                                       \
    } while (/* CONSTCOND */ 0)
#define SCM_QUEUE_TERMINATOR(_q)          (DEREF(_q))
#define SCM_QUEUE_SLOPPY_APPEND(_q, _lst) (SET((_q), (_lst)))

/*=======================================
   Local Buffer Allocator
=======================================*/
/* don't touch inside directly */
#define ScmLBuf(T)                                                           \
    struct ScmLBuf_##T##_ {                                                  \
        T *buf;                                                              \
        size_t size;                                                         \
        T *init_buf;                                                         \
        size_t init_size;                                                    \
        size_t extended_cnt;                                                 \
    }

ScmLBuf(void);

/* lvalue access is permitted */
#define LBUF_BUF(lbuf)       ((lbuf).buf)

/* lvalue access is not permitted */
#define LBUF_END(lbuf)       (&LBUF_BUF(lbuf)[LBUF_SIZE(lbuf)])
#define LBUF_SIZE(lbuf)      ((lbuf).size)
#define LBUF_INIT_SIZE(lbuf) ((lbuf).init_size)
#define LBUF_EXT_CNT(lbuf)   ((lbuf).extended_cnt)

#define LBUF_INIT(lbuf, init_buf, init_size)                                 \
    scm_lbuf_init((void *)&(lbuf), (init_buf), (init_size))

#define LBUF_FREE(lbuf)                                                      \
    scm_lbuf_free((void *)&(lbuf))

#define LBUF_ALLOC(lbuf, size)                                               \
    scm_lbuf_alloc((void *)&(lbuf), (size))

#define LBUF_REALLOC(lbuf, size)                                             \
    scm_lbuf_realloc((void *)&(lbuf), (size))

#define LBUF_EXTEND(lbuf, f, least_size)                                     \
    scm_lbuf_extend((void *)&(lbuf), (f), (least_size))

void scm_lbuf_init(struct ScmLBuf_void_ *lbuf,
                   void *init_buf, size_t init_size);
void scm_lbuf_free(struct ScmLBuf_void_ *lbuf);
void scm_lbuf_alloc(struct ScmLBuf_void_ *lbuf, size_t size);
void scm_lbuf_realloc(struct ScmLBuf_void_ *lbuf, size_t size);
void scm_lbuf_extend(struct ScmLBuf_void_ *lbuf,
                     size_t (*f)(struct ScmLBuf_void_ *), size_t least_size);

/*
 * extended size functions:
 * define your own version if more optimized version is needed
 */
size_t scm_lbuf_f_linear(struct ScmLBuf_void_ *lbuf);
size_t scm_lbuf_f_exponential(struct ScmLBuf_void_ *lbuf);

/*=======================================
   Function Declarations
=======================================*/
/* storage.c */
void scm_init_storage(const ScmStorageConf *conf);
void scm_finalize_storage(void);

/* storage-gc.c */
void scm_init_gc(const ScmStorageConf *conf);
void scm_finalize_gc(void);
ScmObj scm_alloc_cell(void);

/* storage-continuation.c */
void scm_init_continuation(void);
void scm_finalize_continuation(void);
void scm_destruct_continuation(ScmObj cont);
ScmObj scm_call_with_current_continuation(ScmObj proc,
                                          ScmEvalState *eval_state);
void scm_call_continuation(ScmObj cont, ScmObj ret) SCM_NORETURN;
ScmObj scm_dynamic_wind(ScmObj before, ScmObj thunk, ScmObj after);
void scm_push_trace_frame(ScmObj obj, ScmObj env);
void scm_pop_trace_frame(void);
ScmObj scm_trace_stack(void);

/* storage-symbol.c */
void scm_init_symbol(const ScmStorageConf *conf);
void scm_finalize_symbol(void);

/* env.c */
scm_bool scm_toplevel_environmentp(ScmObj env);
ScmObj scm_extend_environment(ScmObj formals, ScmObj actuals, ScmObj env);
ScmObj scm_replace_environment(ScmObj formals, ScmObj actuals, ScmObj env);
ScmObj scm_update_environment(ScmObj actuals, ScmObj env);
ScmObj scm_add_environment(ScmObj var, ScmObj val, ScmObj env);
ScmRef scm_lookup_environment(ScmObj var, ScmObj env);

scm_bool scm_valid_environmentp(ScmObj env);
scm_bool scm_valid_environment_extensionp(ScmObj formals, ScmObj actuals);
scm_bool scm_valid_environment_extension_lengthp(scm_int_t formals_len,
                                                 scm_int_t actuals_len);
scm_int_t scm_validate_formals(ScmObj formals);
scm_int_t scm_validate_actuals(ScmObj actuals);

/* eval.c */
ScmObj scm_symbol_value(ScmObj var, ScmObj env);
ScmObj scm_tailcall(ScmObj proc, ScmObj args, ScmEvalState *eval_state);
ScmObj scm_eval(ScmObj obj, ScmObj env);

/* syntax.c */
void scm_init_syntax(void);
ScmObj scm_s_body(ScmObj body, ScmEvalState *eval_state);
ScmObj scm_s_cond_internal(ScmObj args, ScmObj case_key,
                           ScmEvalState *eval_state);

/* error.c */
void scm_init_error(void);

/* operations.c */
scm_int_t scm_finite_length(ScmObj lst);
scm_int_t scm_length(ScmObj lst);

/* port.c */
void scm_init_port(void);
ScmObj scm_prepare_port(ScmObj args, ScmObj default_port);
ScmCharPort *scm_make_char_port(ScmBytePort *bport);

/* sigscheme.c */
char **scm_interpret_argv(char **argv);
void scm_free_argv(char **argv);

#endif /* __SIGSCHEMEINTERNAL_H */
