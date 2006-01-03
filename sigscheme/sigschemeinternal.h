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
#include "sigschemefunctable.h"
#include "encoding.h"

/*=======================================
   Type Definitions
=======================================*/
typedef struct ScmSpecialCharInfo_ ScmSpecialCharInfo;
struct ScmSpecialCharInfo_ {
    int code;             /* character code as ASCII/Unicode */
    const char *esc_seq;  /* escape sequence as string */
    const char *lex_rep;  /* lexical representation as character object */
};

/*=======================================
   Variable Declarations
=======================================*/
/* sigscheme.c */
extern ScmObj scm_sym_else, scm_sym_yields;
extern ScmCharCodec *scm_identifier_codec;

/* io.c */
extern ScmObj scm_in;
extern ScmObj scm_out;
extern ScmObj scm_err;

/* print.c */
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
#ifndef FALSE
#define FALSE 0
#endif /* FALSE */
#ifndef TRUE
#define TRUE  (!FALSE)
#endif /* TRUE */

/* trace stack for debugging */
#define MAKE_TRACE_FRAME(obj, env) CONS(obj, env)
#define TRACE_FRAME_OBJ CAR
#define TRACE_FRAME_ENV CDR

#if SCM_USE_STORAGE_ABSTRACTION_LAYER
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

#define SCM_ENTYPE_VALUEPACKET(o)    SCM_SAL_ENTYPE_VALUEPACKET(o)
#if SCM_USE_VALUECONS
#define SCM_NULLVALUESP(o)           SCM_SAL_NULLVALUESP(o)
#define SCM_VALUECONS_CAR(o)         SCM_SAL_VALUECONS_CAR(o)
#define SCM_VALUECONS_CDR(o)         SCM_SAL_VALUECONS_CDR(o)
#else /* SCM_USE_VALUECONS */
#define SCM_VALUEPACKET_SET_VALUES(o, vals)                                  \
    SCM_SAL_VALUEPACKET_SET_VALUES((o), (vals))
#endif /* SCM_USE_VALUECONS */

#define SCM_ENTYPE_FREECELL(o)       SCM_SAL_ENTYPE_FREECELL(o)
#define SCM_AS_FREECELL(o)           SCM_SAL_AS_FREECELL(o)

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

#else /* SCM_USE_STORAGE_ABSTRACTION_LAYER */

/* FreeCell Handling Macros */
#if SCM_OBJ_COMPACT
#define SCM_FREECELLP(a)            (SCM_CONSP(a))
#define SCM_AS_FREECELL(a)          (SCM_ASSERT_TYPE(SCM_CONSP(a), (a)))
#define SCM_FREECELL_CAR(a)         (SCM_CAR(a))
#define SCM_FREECELL_CDR(a)         (SCM_CDR(a))
#define SCM_ENTYPE_FREECELL(a)      (SCM_ENTYPE_CONS(a))
#define SCM_FREECELL_SET_CAR(a, car) (SCM_CONS_SET_CAR((a), (car)))
#define SCM_FREECELL_SET_CDR(a, cdr) (SCM_CONS_SET_CDR((a), (cdr)))
#else
#error "Use the Storage Abstraction Layer"
#endif
#endif /* SCM_USE_STORAGE_ABSTRACTION_LAYER */

/* Prefix-less Abbreviation Names For Convenient Internal Use */
#define SYM_QUOTE            SCM_SYM_QUOTE
#define SYM_QUASIQUOTE       SCM_SYM_QUASIQUOTE
#define SYM_UNQUOTE          SCM_SYM_UNQUOTE
#define SYM_UNQUOTE_SPLICING SCM_SYM_UNQUOTE_SPLICING
#define SYM_ELSE             scm_sym_else
#define SYM_YIELDS           scm_sym_yields

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
#define REF_OFF_HEAP   SCM_REF_OFF_HEAP

#define EVAL           SCM_EVAL

#define MAKE_BOOL                     SCM_MAKE_BOOL
#define MAKE_INT                      SCM_MAKE_INT
#define MAKE_CONS                     SCM_MAKE_CONS
#define MAKE_SYMBOL                   SCM_MAKE_SYMBOL
#define MAKE_CHAR                     SCM_MAKE_CHAR
#define MAKE_STRING                   SCM_MAKE_STRING
#define MAKE_STRING_COPYING           SCM_MAKE_STRING_COPYING
#define MAKE_IMMUTABLE_STRING         SCM_MAKE_IMMUTABLE_STRING
#define MAKE_IMMUTABLE_STRING_COPYING SCM_MAKE_IMMUTABLE_STRING_COPYING
#define MAKE_FUNC                     SCM_MAKE_FUNC
#define MAKE_CLOSURE                  SCM_MAKE_CLOSURE
#define MAKE_VECTOR                   SCM_MAKE_VECTOR
#define MAKE_PORT                     SCM_MAKE_PORT
#define MAKE_CONTINUATION             SCM_MAKE_CONTINUATION
#if SCM_USE_NONSTD_FEATURES
#define MAKE_C_POINTER                SCM_MAKE_C_POINTER
#define MAKE_C_FUNCPOINTER            SCM_MAKE_C_FUNCPOINTER
#endif /* SCM_USE_NONSTD_FEATURES */
#define MAKE_VALUEPACKET              SCM_MAKE_VALUEPACKET

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
#define ERROBJP        SCM_ERROBJP

#define LISTP          SCM_LISTP
#define LIST_1_P       SCM_LIST_1_P
#define LIST_2_P       SCM_LIST_2_P
#define LIST_3_P       SCM_LIST_3_P
#define LIST_4_P       SCM_LIST_4_P
#define LIST_5_P       SCM_LIST_5_P

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
/* FIXME: replace ASSERT_NO_MORE_ARG() and ASSERT_PROPER_ARG_LIST() with these
   appropriately */
#define ENSURE_NO_MORE_ARG(args)                                             \
    (NO_MORE_ARG(args) || (ERR_OBJ("superfluous argument(s)", (args)), 1))
#define ENSURE_PROPER_ARG_LIST(args)                                         \
    (scm_p_c_length(args) >= 0 || (ERR_OBJ("bad argument list", (args)), 1))
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
#define ENSURE_ERROBJ(obj)  ENSURE_TYPE(ERROBJP, "error object", (obj))
#define ENSURE_LIST(obj)    ENSURE_TYPE(LISTP, "list", (obj))

#define ENSURE_MUTABLE(str)                                                  \
    (SCM_STRING_MUTABLEP(str)                                                \
     || (ERR_OBJ("attempted to modify immutable string", str), 1))

#define ENSURE_STATEFUL_CODEC(codec)                                         \
    (SCM_CHARCODEC_STATEFULP(codec)                                          \
     || (ERR("%s: stateful character codec required but got: %s",            \
             SCM_MANGLE(name), SCM_CHARCODEC_ENCODING(codec)), 0))
#define ENSURE_STATELESS_CODEC(codec)                                        \
    (!SCM_CHARCODEC_STATEFULP(codec)                                         \
     || (ERR("%s: stateless character codec required but got: %s",           \
             SCM_MANGLE(name), SCM_CHARCODEC_ENCODING(codec)), 0))

/* Macros For Handling Continuation Object */
#define INVALID_CONTINUATION_OPAQUE  NULL

/* Symbol Name Hash Size */
#define NAMEHASH_SIZE 1024

/* error handlings */
#define SCM_ERR_HEADER "Error: "

#define ENSURE_ALLOCATED SCM_ENSURE_ALLOCATED

/*=======================================
   List Constructor
=======================================*/
typedef ScmRef ScmQueue;
#define SCM_QUEUE_INVALIDATE(_q) ((_q) = NULL)
#define SCM_QUEUE_VALIDP(_q)     (_q)
#define SCM_QUEUE_POINT_TO(_q, _out) ((_q) = SCM_REF_OFF_HEAP(_out))
#define SCM_QUEUE_ADD(_q, _dat) (SET((_q), LIST_1(_dat)),       \
                                 (_q) = REF_CDR(DEREF(_q)))
#define SCM_QUEUE_APPEND(_q, _lst)              \
    do {                                        \
        SET((_q), (_lst));                      \
        while (CONSP(DEREF(_q)))                \
            (_q) = REF_CDR(DEREF(_q));          \
    } while (/* CONSTCOND */ 0)
#define SCM_QUEUE_TERMINATOR(_q)          (DEREF(_q))
#define SCM_QUEUE_SLOPPY_APPEND(_q, _lst) (SET((_q), (_lst)))

/*=======================================
   Local Buffer Allocator
=======================================*/
/* don't touch inside directly */
#define ScmLBuf(T)                                                           \
    struct ScmLBuf_##T##_ {                                                  \
        T *_buf;                                                             \
        size_t _size;                                                        \
        T *_init_buf;                                                        \
        size_t _init_size;                                                   \
        int _extended_cnt;                                                   \
    }

/* lvalue access is permitted */
#define LBUF_BUF(lbuf)       ((lbuf)._buf)

/* lvalue access is not permitted */
#define LBUF_END(lbuf)       (&LBUF_BUF(lbuf)[LBUF_SIZE(lbuf)])
#define LBUF_SIZE(lbuf)      ((lbuf)._size)
#define LBUF_INIT_SIZE(lbuf) ((lbuf)._init_size)
#define LBUF_EXT_CNT(lbuf)   ((lbuf)._extended_cnt)

#define LBUF_INIT(lbuf, init_buf, init_size)                                 \
    do {                                                                     \
        (lbuf)._buf  = (lbuf)._init_buf  = init_buf;                         \
        (lbuf)._size = (lbuf)._init_size = init_size;                        \
        (lbuf)._extended_cnt = 0;                                            \
    } while (/* CONSTCOND */ 0)

#define LBUF_FREE(lbuf)                                                      \
    do {                                                                     \
        if ((lbuf)._buf != (lbuf)._init_buf)                                 \
            free((lbuf)._buf);                                               \
    } while (/* CONSTCOND */ 0)

#define LBUF_ALLOC(lbuf, size)                                               \
    do {                                                                     \
        (lbuf)._buf = scm_malloc(size);                                      \
        (lbuf)._size = (size);                                               \
    } while (/* CONSTCOND */ 0)

#define LBUF_REALLOC(lbuf, size)                                             \
    do {                                                                     \
        if ((lbuf)._buf == (lbuf)._init_buf) {                               \
            (lbuf)._buf = memcpy(scm_malloc(size), LBUF_BUF(lbuf), LBUF_SIZE(lbuf)); \
        } else {                                                             \
            (lbuf)._buf = scm_realloc((lbuf)._buf, (size));                  \
        }                                                                    \
        (lbuf)._size = (size);                                               \
    } while (/* CONSTCOND */ 0)

#define LBUF_EXTEND(lbuf, f, min_size)                                       \
    do {                                                                     \
        size_t new_size;                                                     \
                                                                             \
        if (LBUF_SIZE(lbuf) < (min_size)) {                                  \
            new_size = f(lbuf);                                              \
            if (new_size < LBUF_SIZE(lbuf))                                  \
                ERR("local buffer exceeded");                                \
            if (new_size < (size_t)min_size)                                 \
                new_size = (size_t)min_size;                                 \
            LBUF_REALLOC((lbuf), new_size);                                  \
            (lbuf)._extended_cnt++;                                          \
        }                                                                    \
    } while (/* CONSTCOND */ 0)

/*
 * extended size functions:
 * define your own version if more optimized version is needed
 */
#define LBUF_F_LINEAR(lbuf)      (LBUF_SIZE(lbuf) + LBUF_INIT_SIZE(lbuf))
#define LBUF_F_EXPONENTIAL(lbuf) (LBUF_SIZE(lbuf) << 1)

/*=======================================
   Function Declarations
=======================================*/
/* storage.c */
void scm_init_storage(size_t heap_size, size_t heap_alloc_threshold,
                        int n_heaps_max, int n_heaps_init);
void scm_finalize_storage(void);

/* storage-gc.c */
void   scm_init_gc(size_t heap_size, size_t heap_alloc_threshold,
                     int n_heaps_max, int n_heaps_init);
void   scm_finalize_gc(void);
ScmObj scm_alloc_cell(void);

/* storage-continuation.c */
void   scm_init_continuation(void);
void   scm_finalize_continuation(void);
void   scm_destruct_continuation(ScmObj cont);
ScmObj scm_call_with_current_continuation(ScmObj proc, ScmEvalState *eval_state);
void   scm_call_continuation(ScmObj cont, ScmObj ret);
ScmObj scm_dynamic_wind(ScmObj before, ScmObj thunk, ScmObj after);
void scm_push_trace_frame(ScmObj obj, ScmObj env);
void scm_pop_trace_frame(void);
ScmObj scm_trace_stack(void);

/* storage-symbol.c */
void   scm_init_symbol(void);
void   scm_finalize_symbol(void);

/* eval.c */
/* environment related functions */
ScmObj scm_extend_environment(ScmObj vars, ScmObj vals, ScmObj env);
ScmObj scm_add_environment(ScmObj var, ScmObj val, ScmObj env);
ScmRef scm_lookup_environment(ScmObj var, ScmObj env);
ScmObj scm_symbol_value(ScmObj var, ScmObj env);

ScmObj scm_eval(ScmObj obj, ScmObj env);
ScmObj scm_tailcall(ScmObj proc, ScmObj args, ScmEvalState *eval_state);

ScmObj scm_s_cond_internal(ScmObj args, ScmObj case_key, ScmEvalState *eval_state);

/* error.c */
void scm_init_error(void);
void scm_throw_exception(ScmObj errorobj) SCM_NORETURN;
void scm_show_error_header(void);
void scm_error_obj(const char *func_name, const char *msg, ScmObj obj) SCM_NORETURN;

/* operations.c */
int scm_p_c_length(ScmObj lst);

/* io.c */
void scm_init_io(void);
ScmCharPort *scm_make_char_port(ScmBytePort *bport);

/* sigscheme.c */
char **scm_interpret_argv(char **argv);
void scm_free_argv(char **argv);

#endif /* __SIGSCHEMEINTERNAL_H */
