/*===========================================================================
 *  Filename : sigschemeinternal.h
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

#include <config.h>

#include <stddef.h>
#include <string.h>

#include "global.h"
#include "sigscheme.h"
#if SCM_USE_MULTIBYTE_CHAR
#include "encoding.h"
#else
#include "encoding-dummy.h"
#endif
#if SCM_USE_PORT
#include "scmport.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
  Prefix-less Abbreviation Names
=======================================*/
/* TODO: generate these automatically and maybe put them in an optional public
 * header file. */

#define SYM_QUOTE            SCM_SYM_QUOTE
#define SYM_QUASIQUOTE       SCM_SYM_QUASIQUOTE
#define SYM_UNQUOTE          SCM_SYM_UNQUOTE
#define SYM_UNQUOTE_SPLICING SCM_SYM_UNQUOTE_SPLICING
#define SYM_ELLIPSIS         SCM_SYM_ELLIPSIS

#define EQ             SCM_EQ
#define NULLP          SCM_NULLP
#define FALSEP         SCM_FALSEP
#define TRUEP          SCM_TRUEP
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
#define MAKE_C_POINTER                SCM_MAKE_C_POINTER
#define MAKE_C_FUNCPOINTER            SCM_MAKE_C_FUNCPOINTER
#define MAKE_VALUEPACKET              SCM_MAKE_VALUEPACKET

#define MAKE_HMACRO                   SCM_MAKE_HMACRO
#define MAKE_FARSYMBOL                SCM_MAKE_FARSYMBOL
#define MAKE_SUBPAT                   SCM_MAKE_SUBPAT

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
#define SYNTACTIC_OBJECTP SCM_SYNTACTIC_OBJECTP
#define VECTORP        SCM_VECTORP
#define PORTP          SCM_PORTP
#define CONTINUATIONP  SCM_CONTINUATIONP
#define NULLVALUESP    SCM_NULLVALUESP
#define VALUEPACKETP   SCM_VALUEPACKETP
#define HMACROP        SCM_HMACROP
#define FARSYMBOLP     SCM_FARSYMBOLP
#define SUBPATP        SCM_SUBPATP
#define FREECELLP      SCM_FREECELLP
#define C_POINTERP     SCM_C_POINTERP
#define C_FUNCPOINTERP SCM_C_FUNCPOINTERP
#define ENVP           SCM_ENVP
#define VALID_ENVP     SCM_VALID_ENVP
#define ERROBJP        SCM_ERROBJP
#define IDENTIFIERP    SCM_IDENTIFIERP

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

#define ENSURE_ALLOCATED SCM_ENSURE_ALLOCATED
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

/*=======================================
  Macro Definitions
=======================================*/
#define SCM_ERR_HEADER "Error: "

#define ERRMSG_UNHANDLED_EXCEPTION "unhandled exception"
#define SCM_ERRMSG_IMPROPER_ARGS                                             \
    "proper list required for function call but got"
#define SCM_ERRMSG_NULL_IN_STRING                                            \
    "null character in a middle of string is not enabled"

#if HAVE___ALIGNOF__
#define ALIGNOF_SCMOBJ (__alignof__ (ScmObj))
#else
/* FIXME: m68k compiled without __alignof__ -ready cc will crash on GC */
#define ALIGNOF_SCMOBJ SIZEOF_SCMOBJ
#endif

#if SCM_STRICT_TOPLEVEL_DEFINITIONS
/* FIXME: temporary hack. SCM_EOF is only used as an unique ID. */
#define SCM_INTERACTION_ENV_INDEFINABLE SCM_EOF
#endif

/* specifies whether the storage abstraction layer can only handle nested
 * (stacked) continuation or R5RS-conformant full implementation. But current
 * implementation only supports '1'. */
#define SCM_NESTED_CONTINUATION_ONLY 1
#define INVALID_CONTINUATION_OPAQUE  NULL

/* trace stack for debugging */
#define MAKE_TRACE_FRAME(obj, env) CONS((obj), (env))
#define TRACE_FRAME_OBJ CAR
#define TRACE_FRAME_ENV CDR

/* Extraction of a valuepacket is granted only for SigScheme-internals */
#define SCM_VALUEPACKET_VALUES(o)    SCM_SAL_VALUEPACKET_VALUES(o)
#if SCM_USE_VALUECONS
#define SCM_NULLVALUESP(o)           SCM_SAL_NULLVALUESP(o)
#define SCM_VALUECONS_CAR(o)         SCM_SAL_VALUECONS_CAR(o)
#define SCM_VALUECONS_CDR(o)         SCM_SAL_VALUECONS_CDR(o)
#else /* SCM_USE_VALUECONS */
#define SCM_VALUEPACKET_SET_VALUES(o, vals)                                  \
    SCM_SAL_VALUEPACKET_SET_VALUES((o), (vals))
#endif /* SCM_USE_VALUECONS */

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

#if 0
/* for future cleanup */
#define SCM_CELL_MARKEDP(cell)   SCM_SAL_CELL_MARKEDP(cell)
#define SCM_CELL_UNMARKEDP(cell) SCM_SAL_CELL_UNMARKEDP(cell)
#define SCM_CELL_MARK(cell)      SCM_SAL_CELL_MARK(cell)
#define SCM_CELL_UNMARK(cell)    SCM_SAL_CELL_UNMARK(cell)
#else
#define SCM_MARKEDP(o)   SCM_SAL_MARKEDP(o)
#define SCM_UNMARKEDP(o) SCM_SAL_UNMARKEDP(o)
#define SCM_MARK(o)      SCM_SAL_MARK(o)
#define SCM_UNMARK(o)    SCM_SAL_UNMARK(o)
#endif

#define EQVP(a, b)   (SCM_EQVP((a), (b)))
#define EQUALP(a, b) (TRUEP(scm_p_equalp((a), (b))))
#define STRING_EQUALP(str1, str2)                                            \
    (EQ((str1), (str2))                                                      \
     || (SCM_STRING_LEN(str1) == SCM_STRING_LEN(str2)  /* rough rejection */ \
         && strcmp(SCM_STRING_STR(str1), SCM_STRING_STR(str2)) == 0))

/* result encoders for scm_length() */
/* Dotted list length (follows SRFI-1 definition) is encoded as
 * (-length - 1) */
#define SCM_LISTLEN_ENCODE_DOTTED(len)   (~(len))  /* (-len - 1) */
#define SCM_LISTLEN_ENCODE_CIRCULAR(len) (SCM_INT_T_MIN)
#define SCM_LISTLEN_ENCODE_ERROR         SCM_LISTLEN_ENCODE_CIRCULAR

/*=======================================
  Utils for Procedure Implementation
=======================================*/
/*
 * TODO: export these macros to sigscheme.h after:
 *
 * - Argument type information is encoded into ScmFuncTypeCode
 * - Dynamically loadable binary module which allows user-written procedure is
 *   provided
 */

/* Obscures identifier ID. */
#define SCM_MANGLE(id) scm_internal_##id

#define VALIDP(obj)   (!EQ((obj), SCM_INVALID))

/* Declares the current function name as seen by Scheme codes.  TYPE
 * is ignored, but we use it to implement a stub generator.  This
 * macro can be invoked only at the beginning of a function body,
 * right after local variable declarations. */
#define DECLARE_FUNCTION(func_name, type)                                    \
    const char *SCM_MANGLE(name);                                            \
    ScmObj SCM_MANGLE(tmp);                                                  \
    SCM_MANGLE(name) = func_name;                                            \
    SCM_MANGLE(tmp)  = SCM_INVALID /* No semicolon here. */

/* DECLARE_FUNCTION without the functype.
 * FIXME: is there a better name? */
#define DECLARE_INTERNAL_FUNCTION(name) DECLARE_FUNCTION((name), ignored)

/* Signals an error without function name. The message is formatted by
 * scm_vformat(). */
#define PLAIN_ERR scm_plain_error

/* Signals an error.  The current function name and the message are
   sent to the error port.  The message is formatted by scm_vformat(). */
/* FIXME: check variadic macro availability with autoconf */
#if HAVE_C99_VARIADIC_MACRO
#define ERR(fmt, ...)     (scm_error(SCM_MANGLE(name), fmt, __VA_ARGS__))
#elif HAVE_GNU_VARIADIC_MACRO
#define ERR(fmt, args...) (scm_error(SCM_MANGLE(name), fmt, args))
#else
SCM_GLOBAL_VARS_BEGIN(error);
const char *scm_err_funcname;
SCM_GLOBAL_VARS_END(error);
#define scm_err_funcname SCM_GLOBAL_VAR(error, scm_err_funcname)
SCM_DECLARE_EXPORTED_VARS(error);

SCM_EXPORT void scm_error_with_implicit_func(const char *msg, ...) SCM_NORETURN;
#define ERR (scm_err_funcname = SCM_MANGLE(name)), scm_error_with_implicit_func
#endif


/* Signals an error that occured on an object.  The current function
 * name, the message, then the object, are written (with `write') to
 * the error port. */
#define ERR_OBJ(msg, obj) scm_error_obj(SCM_MANGLE(name), (msg), (obj))

#define SCM_ENSURE_PROPER_LIST_TERMINATION(term, lst)                        \
    (NULLP(term) || (ERR_OBJ("proper list required but got", (lst)), 1))

#if SCM_STRICT_ARGCHECK
#define SCM_CHECK_PROPER_LIST_TERMINATION SCM_ENSURE_PROPER_LIST_TERMINATION
#else
#define SCM_CHECK_PROPER_LIST_TERMINATION(term, lst) SCM_EMPTY_EXPR
#endif

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
#define ASSERT_NO_MORE_ARG(args)     SCM_EMPTY_EXPR
#define ASSERT_PROPER_ARG_LIST(args) SCM_EMPTY_EXPR
#endif /* not SCM_STRICT_ARGCHECK */

/* Destructively retreives the first element of a list. */
#define POP(_lst)                                                            \
    (SCM_MANGLE(tmp) = CAR(_lst), (_lst) = CDR(_lst), SCM_MANGLE(tmp))

/* POP() with safety check. */
#define SAFE_POP(_lst)                                                       \
    (CONSP((_lst)) ? POP((_lst)) : SCM_INVALID)

/* Like POP(), but signals an error if no argument is available. */
#define MUST_POP_ARG(_lst)                                                   \
    (CONSP(_lst) ? POP(_lst) : (ERR("missing argument(s)"), SCM_NULL))

#define FOR_EACH_WHILE(_kar, _lst, _cond)                                    \
    while ((_cond) && ((_kar) = CAR((_lst)), (_lst) = CDR(_lst), 1))

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

#define ENSURE_TYPE(pred, _typename, obj)                                    \
    (pred(obj) || (ERR_OBJ(_typename " required but got", (obj)), 1))

#define ENSURE_INT(o)          ENSURE_TYPE(INTP,          "integer",      (o))
#define ENSURE_CONS(o)         ENSURE_TYPE(CONSP,         "pair",         (o))
#define ENSURE_SYMBOL(o)       ENSURE_TYPE(SYMBOLP,       "symbol",       (o))
#define ENSURE_CHAR(o)         ENSURE_TYPE(CHARP,         "character",    (o))
#define ENSURE_STRING(o)       ENSURE_TYPE(STRINGP,       "string",       (o))
#define ENSURE_FUNC(o)         ENSURE_TYPE(FUNCP,         "function",     (o))
#define ENSURE_CLOSURE(o)      ENSURE_TYPE(CLOSUREP,      "closure",      (o))
#define ENSURE_VECTOR(o)       ENSURE_TYPE(VECTORP,       "vector",       (o))
#define ENSURE_PORT(o)         ENSURE_TYPE(PORTP,         "port",         (o))
#define ENSURE_CONTINUATION(o) ENSURE_TYPE(CONTINUATIONP, "continuation", (o))
#define ENSURE_PROCEDURE(o)    ENSURE_TYPE(PROCEDUREP,    "procedure",    (o))
#define ENSURE_ENV(o)          ENSURE_TYPE(ENVP, "environment specifier", (o))
#define ENSURE_VALID_ENV(o)                                                \
    ENSURE_TYPE(VALID_ENVP, "valid environment specifier", (o))
#define ENSURE_ERROBJ(o)       ENSURE_TYPE(ERROBJP,       "error object", (o))
#define ENSURE_LIST(o)         ENSURE_TYPE(LISTP,         "list",         (o))
#define ENSURE_IDENTIFIER(o)   ENSURE_TYPE(IDENTIFIERP,   "identifier",   (o))

#if SCM_HAS_IMMUTABLE_CONS
#define ENSURE_MUTABLE_CONS(kons)                                            \
    (SCM_CONS_MUTABLEP(kons)                                                 \
     || (ERR_OBJ("attempted to modify immutable pair", (kons)), 1))
#else /* SCM_HAS_IMMUTABLE_CONS */
#define ENSURE_MUTABLE_CONS(kons) SCM_EMPTY_EXPR
#endif /* SCM_HAS_IMMUTABLE_CONS */

#if SCM_HAS_IMMUTABLE_STRING
#define ENSURE_MUTABLE_STRING(str)                                           \
    (SCM_STRING_MUTABLEP(str)                                                \
     || (ERR_OBJ("attempted to modify immutable string", (str)), 1))
#else /* SCM_HAS_IMMUTABLE_STRING */
#define ENSURE_MUTABLE_STRING(str) SCM_EMPTY_EXPR
#endif /* SCM_HAS_IMMUTABLE_STRING */

#if SCM_HAS_IMMUTABLE_VECTOR
#define ENSURE_MUTABLE_VECTOR(vec)                                           \
    (SCM_VECTOR_MUTABLEP(vec)                                                \
     || (ERR_OBJ("attempted to modify immutable vector", (vec)), 1))
#else /* SCM_HAS_IMMUTABLE_VECTOR */
#define ENSURE_MUTABLE_VECTOR(vec) SCM_EMPTY_EXPR
#endif /* SCM_HAS_IMMUTABLE_VECTOR */

#if SCM_USE_MULTIBYTE_CHAR
#define ENSURE_STATEFUL_CODEC(codec)                                         \
    (SCM_CHARCODEC_STATEFULP(codec)                                          \
     || (ERR("stateful character codec required but got: ~S",                \
             SCM_CHARCODEC_ENCODING(codec)), 0))
#define ENSURE_STATELESS_CODEC(codec)                                        \
    (!SCM_CHARCODEC_STATEFULP(codec)                                         \
     || (ERR("stateless character codec required but got: ~S",               \
             SCM_CHARCODEC_ENCODING(codec)), 0))
#endif /* SCM_USE_MULTIBYTE_CHAR */

#if SCM_STRICT_ARGCHECK
#define CHECK_VALID_EVALED_VALUE(x)                                          \
    do {                                                                     \
        if (SYNTACTIC_OBJECTP(x))                                            \
            ERR_OBJ("syntactic keyword is evaluated as value", x);           \
        if (VALUEPACKETP(x))                                                 \
            ERR_OBJ("multiple values are not allowed here", x);              \
    } while (/* CONSTCOND */ 0)
#else
#define CHECK_VALID_EVALED_VALUE(x) SCM_EMPTY_EXPR
#endif

/*=======================================
  Characters
=======================================*/
/* accepts SCM_ICHAR_EOF */
/* assumes ASCII */
#define ICHAR_ASCIIP(c)      (0 <= (c) && (c) <= 127)
#define ICHAR_CONTROLP(c)    ((0 <= (c) && (c) <= 31) || (c) == 127)
#define ICHAR_WHITESPACEP(c) ((c) == ' ' || ('\t' <= (c) && (c) <= '\r'))
#define ICHAR_NUMERICP(c)    ('0' <= (c) && (c) <= '9')
#define ICHAR_HEXA_NUMERICP(c) (ICHAR_NUMERICP(c)                            \
                                || ('a' <= (c) && (c) <= 'f')                \
                                || ('A' <= (c) && (c) <= 'F'))
#define ICHAR_ALPHABETICP(c) (ICHAR_LOWER_CASEP(c) || ICHAR_UPPER_CASEP(c))
#define ICHAR_UPPER_CASEP(c) ('A' <= (c) && (c) <= 'Z')
#define ICHAR_LOWER_CASEP(c) ('a' <= (c) && (c) <= 'z')

/*
 * SigScheme's case-insensitive character comparison conforms to the
 * foldcase'ed comparison described in SRFI-75 and SRFI-13, although R5RS does
 * not define comparison between alphabetic and non-alphabetic char.
 *
 * This specification is needed to produce natural result on sort functions
 * with these case-insensitive predicates as comparator.
 *
 *   (a-sort '(#\a #\c #\B #\D #\1 #\[ #\$ #\_) char-ci<?)
 *     => (#\$ #\1 #\a #\B #\c #\D #\[ #\_)  ;; the "natural result"
 *
 *     => (#\$ #\1 #\B #\D #\[ #\_ #\a #\c)  ;; "unnatural result"
 *
 * See also:
 *
 *   - Description around 'char-foldcase' in SRFI-75
 *   - "Case mapping and case-folding" and "Comparison" section of SRFI-13
 */
/* FIXME: support SRFI-75 */
#define ICHAR_DOWNCASE(c) (ICHAR_UPPER_CASEP(c) ? (c) + ('a' - 'A') : (c))
#define ICHAR_UPCASE(c)   (ICHAR_LOWER_CASEP(c) ? (c) - ('a' - 'A') : (c))
/* foldcase for case-insensitive character comparison is done by downcase as
 * described in SRFI-75. Although SRFI-13 expects (char-downcase (char-upcase
 * c)), this implementation is sufficient for ASCII range. */
#define ICHAR_FOLDCASE(c) (ICHAR_DOWNCASE(c))

/*=======================================
  List Constructor
=======================================*/
typedef ScmRef ScmQueue;
#define SCM_QUEUE_INVALIDATE(_q)     ((_q) = SCM_INVALID_REF)
#define SCM_QUEUE_VALIDP(_q)         ((_q) != SCM_INVALID_REF)
#define SCM_QUEUE_POINT_TO(_q, _out) ((_q) = SCM_REF_OFF_HEAP(_out))
#define SCM_QUEUE_ADD(_q, _dat)      (SET((_q), LIST_1(_dat)),               \
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
/* don't touch member variables directly */
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

SCM_EXPORT void scm_lbuf_init(struct ScmLBuf_void_ *lbuf,
                              void *init_buf, size_t init_size);
SCM_EXPORT void scm_lbuf_free(struct ScmLBuf_void_ *lbuf);
SCM_EXPORT void scm_lbuf_alloc(struct ScmLBuf_void_ *lbuf, size_t size);
SCM_EXPORT void scm_lbuf_realloc(struct ScmLBuf_void_ *lbuf, size_t size);
SCM_EXPORT void scm_lbuf_extend(struct ScmLBuf_void_ *lbuf,
                                size_t (*f)(struct ScmLBuf_void_ *),
                                size_t least_size);

/*
 * extended size functions:
 * define your own one if more optimized version is needed
 */
SCM_EXPORT size_t scm_lbuf_f_linear(struct ScmLBuf_void_ *lbuf);
SCM_EXPORT size_t scm_lbuf_f_exponential(struct ScmLBuf_void_ *lbuf);

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
/* procedure.c */
SCM_GLOBAL_VARS_BEGIN(procedure);
ScmCharCodec *scm_identifier_codec;
ScmObj scm_values_applier;
SCM_GLOBAL_VARS_END(procedure);
#define scm_identifier_codec SCM_GLOBAL_VAR(procedure, scm_identifier_codec)
#define scm_values_applier   SCM_GLOBAL_VAR(procedure, scm_values_applier)
SCM_DECLARE_EXPORTED_VARS(procedure);

/* port.c */
SCM_GLOBAL_VARS_BEGIN(port);
ScmObj scm_in;   /* current-input-port */
ScmObj scm_out;  /* current-output-port */
ScmObj scm_err;  /* current error port */
SCM_GLOBAL_VARS_END(port);
#define scm_in  SCM_GLOBAL_VAR(port, scm_in)
#define scm_out SCM_GLOBAL_VAR(port, scm_out)
#define scm_err SCM_GLOBAL_VAR(port, scm_err)
SCM_DECLARE_EXPORTED_VARS(port);
SCM_EXTERN(const ScmSpecialCharInfo scm_special_char_table[]);

/* write.c */
SCM_GLOBAL_VARS_BEGIN(write);
void (*scm_write_ss_func)(ScmObj port, ScmObj obj);
SCM_GLOBAL_VARS_END(write);
#define scm_write_ss_func SCM_GLOBAL_VAR(write, scm_write_ss_func)
SCM_DECLARE_EXPORTED_VARS(write);

/* storage.c */
#if SCM_USE_VALUECONS
SCM_GLOBAL_VARS_BEGIN(storage);
ScmObj scm_null_values;
SCM_GLOBAL_VARS_END(storage);
#define scm_null_values SCM_GLOBAL_VAR(storage, scm_null_values)
SCM_DECLARE_EXPORTED_VARS(storage);
#endif

/* symbol.c */
/* Only permitted to storage-gc.c */
SCM_GLOBAL_VARS_BEGIN(symbol);
ScmObj *scm_symbol_hash;
size_t scm_symbol_hash_size;
SCM_GLOBAL_VARS_END(symbol);
#define scm_symbol_hash      SCM_GLOBAL_VAR(symbol, scm_symbol_hash)
#define scm_symbol_hash_size SCM_GLOBAL_VAR(symbol, scm_symbol_hash_size)
SCM_DECLARE_EXPORTED_VARS(symbol);

/*=======================================
  Function Declarations
=======================================*/
/* strcasecmp.c */
#if !HAVE_STRCASECMP
#define strcasecmp scm_strcasecmp
SCM_EXPORT int scm_strcasecmp(const char *s1, const char *s2);
#endif /* !HAVE_STRCASECMP */

/* storage.c */
SCM_EXPORT void scm_init_storage(const ScmStorageConf *conf);
SCM_EXPORT void scm_fin_storage(void);

/* storage-gc.c */
SCM_EXPORT void scm_init_gc(const ScmStorageConf *conf);
SCM_EXPORT void scm_fin_gc(void);
SCM_EXPORT ScmObj scm_alloc_cell(void);

/* continuation.c */
#if SCM_USE_CONTINUATION
SCM_EXPORT void scm_init_continuation(void);
SCM_EXPORT void scm_fin_continuation(void);
SCM_EXPORT void scm_destruct_continuation(ScmObj cont);
SCM_EXPORT ScmObj scm_call_with_current_continuation(ScmObj proc,
                                                     ScmEvalState *eval_state);
SCM_EXPORT void scm_call_continuation(ScmObj cont, ScmObj ret) SCM_NORETURN;
SCM_EXPORT ScmObj scm_dynamic_wind(ScmObj before, ScmObj thunk, ScmObj after);
#if SCM_USE_BACKTRACE
SCM_EXPORT void scm_push_trace_frame(ScmObj obj, ScmObj env);
SCM_EXPORT void scm_pop_trace_frame(void);
#endif /* SCM_USE_BACKTRACE */
SCM_EXPORT ScmObj scm_trace_stack(void);
#endif /* SCM_USE_CONTINUATION */

/* symbol.c */
SCM_EXPORT void scm_init_symbol(const ScmStorageConf *conf);
SCM_EXPORT void scm_fin_symbol(void);

/* env.c */
SCM_EXPORT scm_bool scm_toplevel_environmentp(ScmObj env);
SCM_EXPORT ScmObj scm_extend_environment(ScmObj formals, ScmObj actuals,
                                         ScmObj env);
SCM_EXPORT ScmObj scm_replace_environment(ScmObj formals, ScmObj actuals,
                                          ScmObj env);
SCM_EXPORT ScmObj scm_update_environment(ScmObj actuals, ScmObj env);
SCM_EXPORT ScmObj scm_add_environment(ScmObj var, ScmObj val, ScmObj env);
SCM_EXPORT ScmRef scm_lookup_environment(ScmObj var, ScmObj env);
SCM_EXPORT ScmRef scm_lookup_frame(ScmObj var, ScmObj frame);
#if SCM_USE_HYGIENIC_MACRO
SCM_EXPORT ScmPackedEnv scm_pack_env(ScmObj env);
SCM_EXPORT ScmObj scm_unpack_env(ScmPackedEnv penv, ScmObj context);
SCM_EXPORT scm_bool scm_subenvp(ScmObj env, ScmPackedEnv sub);
SCM_EXPORT scm_bool scm_identifierequalp(ScmObj x, ScmPackedEnv xpenv,
                                         ScmObj y,
                                         ScmPackedEnv penv, ScmObj env);
SCM_EXPORT ScmObj scm_wrap_identifier(ScmObj id, ScmPackedEnv penv,
                                      ScmObj env);
#endif

SCM_EXPORT scm_bool scm_valid_environmentp(ScmObj env);
SCM_EXPORT scm_bool scm_valid_environment_extensionp(ScmObj formals,
                                                     ScmObj actuals);
SCM_EXPORT scm_bool scm_valid_environment_extension_lengthp(scm_int_t formals_len, scm_int_t actuals_len);
SCM_EXPORT scm_int_t scm_validate_formals(ScmObj formals);
SCM_EXPORT scm_int_t scm_validate_actuals(ScmObj actuals);

/* syntax.c */
SCM_EXPORT void scm_init_syntax(void);
#if SCM_USE_INTERNAL_DEFINITIONS
SCM_EXPORT ScmObj scm_s_body(ScmObj body, ScmEvalState *eval_state);
#else
#define scm_s_body scm_s_begin
#endif
SCM_EXPORT ScmObj scm_s_cond_internal(ScmObj clauses,
                                      ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_s_let_internal(enum ScmObjType permitted,
                                     ScmObj bindings, ScmObj body,
                                     ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_s_letrec_internal(enum ScmObjType permitted,
                                        ScmObj bindings, ScmObj body,
                                        ScmEvalState *eval_state);
SCM_EXPORT void scm_s_define_internal(enum ScmObjType permitted,
                                      ScmObj var, ScmObj exp, ScmObj env);

/* macro.c */
#if SCM_USE_HYGIENIC_MACRO
SCM_EXPORT void scm_init_macro(void);
SCM_EXPORT ScmObj scm_expand_macro(ScmObj macro, ScmObj args,
                                   ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_p_reversex(ScmObj in); /* To be relocated. */
SCM_EXPORT void scm_macro_bad_scope(ScmObj sym);
#endif /* SCM_USE_HYGIENIC_MACRO */

/* error.c */
SCM_EXPORT void scm_init_error(void);

/* list.c */
SCM_EXPORT scm_int_t scm_finite_length(ScmObj lst);

/* port.c */
#if SCM_USE_PORT
SCM_EXPORT void scm_init_port(void);
SCM_EXPORT ScmObj scm_prepare_port(ScmObj args, ScmObj default_port);
SCM_EXPORT ScmCharPort *scm_make_char_port(ScmBytePort *bport);
#endif /* SCM_USE_PORT */

/* write.c */
#if SCM_USE_WRITER
SCM_EXPORT void scm_init_writer(void);
SCM_EXPORT void scm_display_errobj_ss(ScmObj port, ScmObj errobj);
#endif /* SCM_USE_WRITER */

/* format.c */
#if SCM_USE_FORMAT
SCM_EXPORT void scm_init_format(void);
#endif /* SCM_USE_FORMAT */

/* load.c */
#if SCM_USE_LOAD
SCM_EXPORT void scm_init_load(void);
SCM_EXPORT void scm_fin_load(void);
#endif /* SCM_USE_LOAD */

/* module.c */
SCM_EXPORT void scm_init_module(void);
SCM_EXPORT void scm_fin_module(void);

/* sigscheme.c */
SCM_EXPORT char **scm_interpret_argv(char **argv);
SCM_EXPORT void scm_free_argv(char **argv);

/*
 * modules
 */

/* module-sscm-ext.c */
#if SCM_USE_SSCM_EXTENSIONS
SCM_EXPORT void scm_initialize_sscm_extensions(void);
#endif

/* module-siod.c */
#if SCM_COMPAT_SIOD
SCM_EXPORT void scm_initialize_siod(void);
#endif

/* module-srfi1.c */
#if SCM_USE_SRFI1
SCM_EXPORT void scm_initialize_srfi1(void);
#endif

/* module-srfi2.c */
#if SCM_USE_SRFI2
SCM_EXPORT void scm_initialize_srfi2(void);
#endif

/* module-srfi6.c */
#if SCM_USE_SRFI6
SCM_EXPORT void scm_initialize_srfi6(void);
#endif

/* module-srfi8.c */
#if SCM_USE_SRFI8
SCM_EXPORT void scm_initialize_srfi8(void);
#endif

/* module-srfi23.c */
#if SCM_USE_SRFI23
SCM_EXPORT void scm_initialize_srfi23(void);
#endif

/* module-srfi28.c */
#if SCM_USE_SRFI28
SCM_EXPORT void scm_initialize_srfi28(void);
#endif

/* module-srfi34.c */
#if SCM_USE_SRFI34
SCM_EXPORT void scm_initialize_srfi34(void);
#endif

/* module-srfi38.c */
#if SCM_USE_SRFI38
SCM_EXPORT void scm_initialize_srfi38(void);
#endif

/* module-srfi48.c */
#if SCM_USE_SRFI48
SCM_EXPORT void scm_initialize_srfi48(void);
#endif

/* module-srfi60.c */
#if SCM_USE_SRFI60
SCM_EXPORT void scm_initialize_srfi60(void);
#endif


#ifdef __cplusplus
}
#endif

#endif /* __SIGSCHEMEINTERNAL_H */
