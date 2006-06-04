/*===========================================================================
 *  Filename : sigscheme.h
 *  About    : Public header file
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
#ifndef __SIGSCHEME_H
#define __SIGSCHEME_H

#include <sigscheme/config.h>

#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdarg.h>

#include "sigscheme-stdint.h"
#include "scmint.h"
#include "global.h"
#include "encoding.h"

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
  Macro Definitions
=======================================*/
/* An empty definition is generally not a good idea.  (Consider for
 * example (FOO(), bar) where FOO() expands to only whitespaces.)
 * Simply using 0 prompts warnings everywhere, so we cast it to void.
 * There may be a better solution out there, so use this macro instead
 * of crudely writing out the same expression.  The idea is taken from
 * glibc's assert.h but it's barely even a line of code so it should
 * be OK as the copyright goes... anyone know for sure? */
#ifndef SCM_EMPTY_EXPR
#define SCM_EMPTY_EXPR ((void)0)
#endif

#define SCM_ERRMSG_UNHANDLED_EXCEPTION "unhandled exception"
#define SCM_ERRMSG_MEMORY_EXHAUSTED    "memory exhausted"
#define SCM_ERRMSG_IMPROPER_ARGS                                             \
    "proper list required for function call but got"
#define SCM_ERRMSG_NULL_IN_STRING                                            \
    "null character in a middle of string is not enabled"

/* FIXME: Confirm appropriate workaround about the noinline attribute vanishing
 * problem for Linux environments */
#if HAVE___ATTRIBUTE__
#define SCM_NOINLINE __attribute__((noinline))
#define SCM_NORETURN __attribute__((noreturn))
#else /* HAVE___ATTRIBUTE__ */
#define SCM_NOINLINE
#define SCM_NORETURN
#endif /* HAVE___ATTRIBUTE__ */

/* RFC: better names for the debug printing */
#if SCM_DEBUG
#define SCM_CDBG(args) (scm_categorized_debug args)
#define SCM_DBG(args)  (scm_debug args)
#else /* SCM_DEBUG */
#define SCM_CDBG(args) SCM_EMPTY_EXPR
#define SCM_DBG(args)  SCM_EMPTY_EXPR
#endif /* SCM_DEBUG */

/*
 * Condition testers
 *
 * SigScheme uses these three types of condition testers.
 *
 * ASSERT: Asserts a condition that is expected as always true, as a contract
 * programming. No actual check is performed when !SCM_DEBUG.
 *
 * ENSURE: Mandatory runtime check involving uncertain data. An exception is
 * raised if failed. Actual check is always performed regaradless of debug
 * configurations.
 *
 * CHECK: Optional runtime check. Actual check is performed only when
 * configured to do so. Since the behavior, codes that include a CHECK must be
 * sane even if the condition is false with no actual check.
 *
 */

#if SCM_DEBUG
#if SCM_CHICKEN_DEBUG
/* allows survival recovery */
#define SCM_ASSERT(cond)                                                     \
    ((cond) || (scm_die("assertion failed", __FILE__, __LINE__), 1))
#else /* SCM_CHICKEN_DEBUG */
#include <assert.h>
#define SCM_ASSERT(cond) (assert(cond))
#endif /* SCM_CHICKEN_DEBUG */
#else /* SCM_DEBUG */
#define SCM_ASSERT(cond) SCM_EMPTY_EXPR
#endif /* SCM_DEBUG */
#define SCM_ENSURE(cond)                                                     \
    ((cond) || (scm_die("invalid condition", __FILE__, __LINE__), 1))

#define SCM_ENSURE_PROPER_LIST_TERMINATION(term, lst)                        \
    (NULLP(term) || (ERR_OBJ("proper list required but got", (lst)), 1))

#if SCM_STRICT_ARGCHECK
#define SCM_CHECK_PROPER_LIST_TERMINATION SCM_ENSURE_PROPER_LIST_TERMINATION
#else
#define SCM_CHECK_PROPER_LIST_TERMINATION(term, lst)
#endif

#define SCM_ENSURE_ALLOCATED(p)                                              \
    ((p) || (scm_fatal_error(SCM_ERRMSG_MEMORY_EXHAUSTED), 1))

#define SCM_VALID_ENVP(obj)    (scm_valid_environmentp(env))

#define SCM_ERROBJP(obj)       (NFALSEP(scm_p_error_objectp(obj)))

#define SCM_SYMBOL_BOUNDP(sym) (!SCM_EQ(SCM_SYMBOL_VCELL(sym), SCM_UNBOUND))

#define SCM_CONS(kar, kdr)           (SCM_MAKE_CONS((kar), (kdr)))
#define SCM_IMMUTABLE_CONS(kar, kdr) (SCM_MAKE_IMMUTABLE_CONS((kar), (kdr)))
#define SCM_CAR(kons)  (SCM_CONS_CAR(kons))
#define SCM_CDR(kons)  (SCM_CONS_CDR(kons))
#define SCM_CAAR(kons) (SCM_CAR(SCM_CAR(kons)))
#define SCM_CADR(kons) (SCM_CAR(SCM_CDR(kons)))
#define SCM_CDAR(kons) (SCM_CDR(SCM_CAR(kons)))
#define SCM_CDDR(kons) (SCM_CDR(SCM_CDR(kons)))

#define SCM_LIST_1(elm0)                                                     \
    (SCM_CONS((elm0), SCM_NULL))
#define SCM_LIST_2(elm0, elm1)                                               \
    (SCM_CONS((elm0), SCM_LIST_1(elm1)))
#define SCM_LIST_3(elm0, elm1, elm2)                                         \
    (SCM_CONS((elm0), SCM_LIST_2((elm1), (elm2))))
#define SCM_LIST_4(elm0, elm1, elm2, elm3)                                   \
    (SCM_CONS((elm0), SCM_LIST_3((elm1), (elm2), (elm3))))
#define SCM_LIST_5(elm0, elm1, elm2, elm3, elm4)                             \
    (SCM_CONS((elm0), SCM_LIST_4((elm1), (elm2), (elm3), (elm4))))

#define SCM_LISTP(obj)    (SCM_CONSP(obj) || SCM_NULLP(obj))
#define SCM_LIST_1_P(lst) (SCM_CONSP(lst) && SCM_NULLP(SCM_CDR(lst)))
#define SCM_LIST_2_P(lst) (SCM_CONSP(lst) && SCM_LIST_1_P(SCM_CDR(lst)))
#define SCM_LIST_3_P(lst) (SCM_CONSP(lst) && SCM_LIST_2_P(SCM_CDR(lst)))
#define SCM_LIST_4_P(lst) (SCM_CONSP(lst) && SCM_LIST_3_P(SCM_CDR(lst)))
#define SCM_LIST_5_P(lst) (SCM_CONSP(lst) && SCM_LIST_4_P(SCM_CDR(lst)))

#define SCM_PROPER_LISTP(obj)   (SCM_LISTLEN_PROPERP(scm_length(obj)))
#define SCM_DOTTED_LISTP(obj)   (CONSP(obj)                                  \
                                 && SCM_LISTLEN_DOTTEDP(scm_length(obj)))
#define SCM_CIRCULAR_LISTP(obj) (SCM_LISTLEN_CIRCULARP(scm_length(obj)))

/* result decoders for scm_length() */
#define SCM_LISTLEN_PROPERP(len)    (0 <= (len))
#define SCM_LISTLEN_CIRCULARP(len)  ((len) == SCM_INT_T_MIN)
#define SCM_LISTLEN_ERRORP          SCM_LISTLEN_CIRCULARP
#define SCM_LISTLEN_DOTTEDP(len)    ((len) < 0                               \
                                     && !SCM_LISTLEN_CIRCULARP(len))
#define SCM_LISTLEN_DOTTED(len)     (-(len))
#define SCM_LISTLEN_BEFORE_DOT(len) (~(len))  /* -(len) - 1 */

#define SCM_EVAL(obj, env) (scm_eval((obj), (env)))

#define SCM_ASSERT_FUNCTYPE(type, c_func)                                    \
    do {                                                                     \
        type f;                                                              \
        if (0) f = (c_func);  /* compile-time type check */                  \
    } while (/* CONSTCOND */ 0)

#if 0
#define SCM_REGISTER_FUNC(name, c_func, type)                                \
    do {                                                                     \
        enum ScmFuncTypeCode typecode;                                       \
                                                                             \
        SCM_ASSERT_FUNCTYPE(scm_##type, (c_func));                           \
        typecode = scm_funccode_##type;                                      \
        scm_register_func(name, c_func, typecode);                           \
    } while (/* CONSTCOND */ 0)
#endif

#if SCM_GCC4_READY_GC
/*
 * Function caller with protecting Scheme objects on stack from GC
 *
 * The protection is safe against with variable reordering on a stack
 * frame performed in some compilers as anti-stack smashing or
 * optimization.
 *
 * Users should only use SCM_GC_PROTECTED_CALL() and
 * SCM_GC_PROTECTED_CALL_VOID().
 */
#define SCM_GC_PROTECTED_CALL(ret, ret_type, func, args)                     \
    SCM_GC_PROTECTED_CALL_INTERNAL(ret = , ret_type, func, args)

#define SCM_GC_PROTECTED_CALL_VOID(func, args)                               \
    SCM_GC_PROTECTED_CALL_INTERNAL((void), void, func, args)

#define SCM_GC_PROTECTED_CALL_INTERNAL(exp_ret, ret_type, func, args)        \
    do {                                                                     \
        /* ensure that func is uninlined */                                  \
        ret_type (*volatile fp)() = (ret_type (*)())func;                    \
        ScmObj *stack_start;                                                 \
                                                                             \
        if (0) exp_ret func args;  /* compile-time type check */             \
        stack_start = scm_gc_current_stack();                                \
        scm_gc_protect_stack(stack_start);                                   \
        exp_ret (*fp)args;                                                   \
        scm_gc_unprotect_stack(stack_start);                                 \
    } while (/* CONSTCOND */ 0)

#endif /* SCM_GCC4_READY_GC */

#if SCM_USE_PORT
#define SCM_ENSURE_LIVE_PORT(port)                                           \
    (SCM_PORT_IMPL(port)                                                     \
     || (scm_error_obj("(unknown)", "operated on closed port", port), 1))
#endif

#if SCM_USE_WRITER
#define SCM_WRITE_SS(port, obj) ((*scm_write_ss_func)(port, obj))
#endif

/*===========================================================================
  Type Definitions
===========================================================================*/
/*=======================================
   Enums
=======================================*/
enum ScmDebugCategory {
    SCM_DBG_NONE         = 0,
    SCM_DBG_ERRMSG       = 1 << 0,   /* the "ERROR: foo bar" style msgs */
    SCM_DBG_BACKTRACE    = 1 << 1,
    SCM_DBG_GC           = 1 << 2,
    SCM_DBG_FILE         = 1 << 3,   /* file loading */
    SCM_DBG_PARSER       = 1 << 4,
    SCM_DBG_READ         = 1 << 5,   /* print each parsed expression + misc */
    SCM_DBG_MACRO        = 1 << 6,
    SCM_DBG_ARGS         = 1 << 7,   /* number of arguments, type and so on */
    SCM_DBG_EVAL         = 1 << 8,   /* evaluation-related things */
    SCM_DBG_CONTINUATION = 1 << 9,
    SCM_DBG_EXCEPTION    = 1 << 10,
    SCM_DBG_EXPERIMENTAL = 1 << 11,  /* developed but experimental features */
    SCM_DBG_DEVEL        = 1 << 12,  /* under development */
    SCM_DBG_COMPAT       = 1 << 13,  /* warns compatibility-sensitive code */
    SCM_DBG_ENCODING     = 1 << 14,  /* multibyte handling */
    SCM_DBG_OTHER        = 1 << 30   /* all other messages */
};

enum ScmObjType {
    /* sorted by majority to make immediate number encoding optimal */
    ScmCons         = 0,
    ScmInt          = 1,
    ScmChar         = 2,
    ScmSymbol       = 3,

    ScmString       = 4,
    ScmFunc         = 5,
    ScmClosure      = 6,
    ScmVector       = 7,

    ScmRational     = 8,
    ScmReal         = 9,
    ScmComplex      = 10,
    ScmConstant     = 11,
    ScmContinuation = 12,
    ScmValuePacket  = 13,
    ScmPort         = 14,
    ScmFreeCell     = 15,
#if 0
    /* Reserved for future extension of argument types-encoded
     * ScmFuncTypeCode. The types for it should not exceed 4-bit.  */
    ScmNumber       = ScmConstant,
    ScmProc         = ScmValuePacket,
    ScmAny          = ScmFreeCell,
#endif

    ScmMacro        = 16,
    ScmFarsymbol    = 17,
    ScmSubpat       = 18,

    ScmCFuncPointer = 30,
    ScmCPointer     = 31
};

/*
 * Function types:
 *
 * Function objects must tag themselves with proper information so
 * that the evaluator can correctly invoke them.
 */
#define SCM_FUNCTYPE_MAND_MAX 5
enum ScmFuncTypeCode {
    SCM_FUNCTYPE_MAND_BITS = 4,
    SCM_FUNCTYPE_MAND_MASK = (1 << SCM_FUNCTYPE_MAND_BITS) - 1,
    SCM_FUNCTYPE_PROCEDURE = 0 << SCM_FUNCTYPE_MAND_BITS,
    SCM_FUNCTYPE_SYNTAX    = 1 << SCM_FUNCTYPE_MAND_BITS,

    SCM_FUNCTYPE_FIXED     = 0 << (SCM_FUNCTYPE_MAND_BITS + 1),
    SCM_FUNCTYPE_VARIADIC  = 1 << (SCM_FUNCTYPE_MAND_BITS + 1),
    SCM_FUNCTYPE_TAILREC   = 1 << (SCM_FUNCTYPE_MAND_BITS + 2),

    SCM_FUNCTYPE_ODDBALL   = 1 << (SCM_FUNCTYPE_MAND_BITS + 10),

    /* Compound types. */
    SCM_PROCEDURE_FIXED             = SCM_FUNCTYPE_FIXED,
    SCM_PROCEDURE_FIXED_TAILREC     = SCM_FUNCTYPE_TAILREC,
    SCM_PROCEDURE_VARIADIC          = SCM_FUNCTYPE_VARIADIC,
    SCM_PROCEDURE_VARIADIC_TAILREC  = (SCM_FUNCTYPE_VARIADIC
                                       | SCM_FUNCTYPE_TAILREC),

    SCM_SYNTAX_FIXED             = (SCM_FUNCTYPE_SYNTAX
                                    | SCM_PROCEDURE_FIXED),
    SCM_SYNTAX_FIXED_TAILREC     = (SCM_FUNCTYPE_SYNTAX
                                    | SCM_PROCEDURE_FIXED_TAILREC),
    SCM_SYNTAX_VARIADIC          = (SCM_FUNCTYPE_SYNTAX
                                    | SCM_PROCEDURE_VARIADIC),
    SCM_SYNTAX_VARIADIC_TAILREC  = (SCM_FUNCTYPE_SYNTAX
                                    | SCM_PROCEDURE_VARIADIC_TAILREC),

    /* Proper combinations */
    SCM_SYNTAX_FIXED_0               = (SCM_SYNTAX_FIXED               | 0),
    SCM_SYNTAX_FIXED_TAILREC_0       = (SCM_SYNTAX_FIXED_TAILREC       | 0),
    SCM_SYNTAX_VARIADIC_0            = (SCM_SYNTAX_VARIADIC            | 0),
    SCM_SYNTAX_VARIADIC_TAILREC_0    = (SCM_SYNTAX_VARIADIC_TAILREC    | 0),
    SCM_PROCEDURE_FIXED_0            = (SCM_PROCEDURE_FIXED            | 0),
    SCM_PROCEDURE_FIXED_TAILREC_0    = (SCM_PROCEDURE_FIXED_TAILREC    | 0),
    SCM_PROCEDURE_VARIADIC_0         = (SCM_PROCEDURE_VARIADIC         | 0),
    SCM_PROCEDURE_VARIADIC_TAILREC_0 = (SCM_PROCEDURE_VARIADIC_TAILREC | 0),
#if SCM_FUNCTYPE_MAND_MAX >= 1
    SCM_SYNTAX_FIXED_1               = (SCM_SYNTAX_FIXED               | 1),
    SCM_SYNTAX_FIXED_TAILREC_1       = (SCM_SYNTAX_FIXED_TAILREC       | 1),
    SCM_SYNTAX_VARIADIC_1            = (SCM_SYNTAX_VARIADIC            | 1),
    SCM_SYNTAX_VARIADIC_TAILREC_1    = (SCM_SYNTAX_VARIADIC_TAILREC    | 1),
    SCM_PROCEDURE_FIXED_1            = (SCM_PROCEDURE_FIXED            | 1),
    SCM_PROCEDURE_FIXED_TAILREC_1    = (SCM_PROCEDURE_FIXED_TAILREC    | 1),
    SCM_PROCEDURE_VARIADIC_1         = (SCM_PROCEDURE_VARIADIC         | 1),
    SCM_PROCEDURE_VARIADIC_TAILREC_1 = (SCM_PROCEDURE_VARIADIC_TAILREC | 1),
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 2
    SCM_SYNTAX_FIXED_2               = (SCM_SYNTAX_FIXED               | 2),
    SCM_SYNTAX_FIXED_TAILREC_2       = (SCM_SYNTAX_FIXED_TAILREC       | 2),
    SCM_SYNTAX_VARIADIC_2            = (SCM_SYNTAX_VARIADIC            | 2),
    SCM_SYNTAX_VARIADIC_TAILREC_2    = (SCM_SYNTAX_VARIADIC_TAILREC    | 2),
    SCM_PROCEDURE_FIXED_2            = (SCM_PROCEDURE_FIXED            | 2),
    SCM_PROCEDURE_FIXED_TAILREC_2    = (SCM_PROCEDURE_FIXED_TAILREC    | 2),
    SCM_PROCEDURE_VARIADIC_2         = (SCM_PROCEDURE_VARIADIC         | 2),
    SCM_PROCEDURE_VARIADIC_TAILREC_2 = (SCM_PROCEDURE_VARIADIC_TAILREC | 2),
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 3
    SCM_SYNTAX_FIXED_3               = (SCM_SYNTAX_FIXED               | 3),
    SCM_SYNTAX_FIXED_TAILREC_3       = (SCM_SYNTAX_FIXED_TAILREC       | 3),
    SCM_SYNTAX_VARIADIC_3            = (SCM_SYNTAX_VARIADIC            | 3),
    SCM_SYNTAX_VARIADIC_TAILREC_3    = (SCM_SYNTAX_VARIADIC_TAILREC    | 3),
    SCM_PROCEDURE_FIXED_3            = (SCM_PROCEDURE_FIXED            | 3),
    SCM_PROCEDURE_FIXED_TAILREC_3    = (SCM_PROCEDURE_FIXED_TAILREC    | 3),
    SCM_PROCEDURE_VARIADIC_3         = (SCM_PROCEDURE_VARIADIC         | 3),
    SCM_PROCEDURE_VARIADIC_TAILREC_3 = (SCM_PROCEDURE_VARIADIC_TAILREC | 3),
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 4
    SCM_SYNTAX_FIXED_4               = (SCM_SYNTAX_FIXED               | 4),
    SCM_SYNTAX_FIXED_TAILREC_4       = (SCM_SYNTAX_FIXED_TAILREC       | 4),
    SCM_SYNTAX_VARIADIC_4            = (SCM_SYNTAX_VARIADIC            | 4),
    SCM_SYNTAX_VARIADIC_TAILREC_4    = (SCM_SYNTAX_VARIADIC_TAILREC    | 4),
    SCM_PROCEDURE_FIXED_4            = (SCM_PROCEDURE_FIXED            | 4),
    SCM_PROCEDURE_FIXED_TAILREC_4    = (SCM_PROCEDURE_FIXED_TAILREC    | 4),
    SCM_PROCEDURE_VARIADIC_4         = (SCM_PROCEDURE_VARIADIC         | 4),
    SCM_PROCEDURE_VARIADIC_TAILREC_4 = (SCM_PROCEDURE_VARIADIC_TAILREC | 4),
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 5
    SCM_SYNTAX_FIXED_5               = (SCM_SYNTAX_FIXED               | 5),
    SCM_SYNTAX_FIXED_TAILREC_5       = (SCM_SYNTAX_FIXED_TAILREC       | 5),
    SCM_SYNTAX_VARIADIC_5            = (SCM_SYNTAX_VARIADIC            | 5),
    SCM_SYNTAX_VARIADIC_TAILREC_5    = (SCM_SYNTAX_VARIADIC_TAILREC    | 5),
    SCM_PROCEDURE_FIXED_5            = (SCM_PROCEDURE_FIXED            | 5),
    SCM_PROCEDURE_FIXED_TAILREC_5    = (SCM_PROCEDURE_FIXED_TAILREC    | 5),
    SCM_PROCEDURE_VARIADIC_5         = (SCM_PROCEDURE_VARIADIC         | 5),
    SCM_PROCEDURE_VARIADIC_TAILREC_5 = (SCM_PROCEDURE_VARIADIC_TAILREC | 5),
#endif

    /* Special type. */
    SCM_REDUCTION_OPERATOR = SCM_FUNCTYPE_ODDBALL,

    SCM_FUNCTYPE_INVALID   = (SCM_FUNCTYPE_ODDBALL | 1)
};

/* Where we are in a reduction process. */
enum ScmReductionState {
    SCM_REDUCE_0,       /* No argument was given. */
    SCM_REDUCE_1,       /* Only 1 argument was given. */
    SCM_REDUCE_PARTWAY, /* We have more arguments pending. */
    SCM_REDUCE_LAST,    /* The callee must finalize. */
    SCM_REDUCE_STOP     /* Callee wants to stop. */
};

enum ScmPortFlag {
    SCM_PORTFLAG_NONE        = 0,
    SCM_PORTFLAG_OUTPUT      = 1 << 0,
    SCM_PORTFLAG_INPUT       = 1 << 1,
    SCM_PORTFLAG_LIVE_OUTPUT = 1 << 2,
    SCM_PORTFLAG_LIVE_INPUT  = 1 << 3,

    SCM_PORTFLAG_DIR_MASK = (SCM_PORTFLAG_OUTPUT | SCM_PORTFLAG_INPUT),
    SCM_PORTFLAG_ALIVENESS_MASK = (SCM_PORTFLAG_LIVE_OUTPUT
                                   | SCM_PORTFLAG_LIVE_INPUT)
};

typedef void (*ScmCFunc)(void);


#if SCM_USE_HYGIENIC_MACRO
/* Environment for looking up a free variable inserted by a hygienic
 * macro's template.  References in syntax-rules are only looked up in
 * environments enclosed by the definition's, so we need only record
 * the number of frames.  That doesn't work in the face of syntax-case
 * though, so we abstract the representation here. */
typedef scm_int_t ScmPackedEnv;
#define SCM_PENV_EQ(x, y)  ((x) == (y))
#endif

/*
 * 64-bit support of SigScheme
 *
 * SigScheme supports all data models of ILP32, LL64, LLP64, LP64 and
 * ILP64. Default settings automatically configure both ABI and the underlying
 * storage implementation appropriately, if the storage implementation is
 * storage-fatty or storage-compact. On the case, the integer size Scheme can
 * handle is determined by sizeof(long), and heap capacity and addressable
 * space are determined by the pointer size.
 *
 * Other storage implementations (currently not exist) may need some manual
 * settings to fit to the specified data model.
 */

/*
 * Interface to an implementation for the Storage Abstraction Layer:
 *
 * A storage implementation defines following types.
 *
 * typedef <hidden> ScmCell;
 * typedef <hidden> ScmObj;
 * typedef <hidden> ScmRef;
 *
 * typedef ScmObj (*ScmFuncType)();
 */
#if SCM_USE_STORAGE_FATTY
#include "storage-fatty.h"
#elif SCM_USE_STORAGE_COMPACT
#include "storage-compact.h"
#else
#error "specify a storage layer implementation"
#endif

typedef struct ScmStorageConf_ ScmStorageConf;
struct ScmStorageConf_ {
    /* heap */
    size_t heap_size;             /* number of ScmCell in a heap */
    size_t heap_alloc_threshold;  /* minimum number of freecells after a GC */
    size_t n_heaps_max;           /* max number of heaps */
    size_t n_heaps_init;          /* initial number of heaps */

    /* symbol table */
    size_t symbol_hash_size;      /* hash size of symbol table */
};

#define SCM_FULLY_ADDRESSABLEP                                               \
    (SCM_PTR_BITS == (sizeof(void *) * CHAR_BIT))

#ifndef SCM_DEFAULT_N_HEAPS_MAX
#define SCM_DEFAULT_N_HEAPS_MAX                                              \
    (((SCM_FULLY_ADDRESSABLEP) ? (uintptr_t)-1 : (1 << SCM_PTR_BITS))        \
     / (SCM_DEFAULT_HEAP_SIZE * sizeof(ScmCell)))
#endif

/*=======================================
  Object Representation Information
=======================================*/
#define SCM_HAS_CHAR     SCM_SAL_HAS_CHAR
#define SCM_HAS_INT      1
#define SCM_HAS_RATIONAL SCM_SAL_HAS_RATIONAL
#define SCM_HAS_REAL     SCM_SAL_HAS_REAL
#define SCM_HAS_COMPLEX  SCM_SAL_HAS_COMPLEX
#define SCM_HAS_STRING   SCM_SAL_HAS_STRING
#define SCM_HAS_VECTOR   SCM_SAL_HAS_VECTOR

#define SCM_HAS_IMMUTABLE_CONS   SCM_SAL_HAS_IMMUTABLE_CONS
#define SCM_HAS_IMMUTABLE_STRING SCM_SAL_HAS_IMMUTABLE_STRING
#define SCM_HAS_IMMUTABLE_VECTOR SCM_SAL_HAS_IMMUTABLE_VECTOR

/* for optimization */
#define SCM_HAS_IMMEDIATE_CHAR_ONLY     SCM_SAL_HAS_IMMEDIATE_CHAR
#define SCM_HAS_IMMEDIATE_NUMBER_ONLY   SCM_SAL_HAS_IMMEDIATE_NUMBER_ONLY
#define SCM_HAS_IMMEDIATE_INT_ONLY      SCM_SAL_HAS_IMMEDIATE_INT_ONLY
#define SCM_HAS_IMMEDIATE_RATIONAL_ONLY SCM_SAL_HAS_IMMEDIATE_RATIONAL_ONLY
#define SCM_HAS_IMMEDIATE_REAL_ONLY     SCM_SAL_HAS_IMMEDIATE_REAL_ONLY
#define SCM_HAS_IMMEDIATE_COMPLEX_ONLY  SCM_SAL_HAS_IMMEDIATE_COMPLEX_ONLY

/* addressable space: tag bits multiplexed in alignment part is also counted */
#define SCM_PTR_BITS    SCM_SAL_PTR_BITS

#define SCM_CHAR_BITS   SCM_SAL_CHAR_BITS
#define SCM_CHAR_MAX    SCM_SAL_CHAR_MAX
#define SCM_CHAR_MIN    0

#define SCM_INT_BITS    SCM_SAL_INT_BITS
#define SCM_INT_MAX     SCM_SAL_INT_MAX
#define SCM_INT_MIN     SCM_SAL_INT_MIN

/* string length */
#define SCM_STRLEN_BITS SCM_SAL_STRLEN_BITS
#define SCM_STRLEN_MAX  SCM_SAL_STRLEN_MAX
#define SCM_STRLEN_MIN  0

/* vector length */
#define SCM_VECLEN_BITS SCM_SAL_VECLEN_BITS
#define SCM_VECLEN_MAX  SCM_SAL_VECLEN_MAX
#define SCM_VECLEN_MIN  0

/*=======================================
  Object Creators
=======================================*/
/* TODO: add this check to all accessor and creator macros. */
#define SCM_TYPESAFE_MACRO(macro, rettype, types, args) \
    (0 ? (*(rettype (*)types)NULL)args                  \
       : 0 ? ((*(rettype *)NULL) = macro args)          \
           : macro args)

/* For macros enclosed in do-while(0). */
#define SCM_TYPESAFE_MACRO_VOID(macro, types, args)     \
    do {                                                \
        if (0)                                          \
            (*(void (*)types)NULL) args;                \
        else                                            \
            macro args;                                 \
    } while (0)

#define SCM_MAKE_BOOL(x) ((x) ? SCM_TRUE : SCM_FALSE)

#define SCM_MAKE_CONS(kar, kdr)                                              \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_CONS,                                    \
                       ScmObj,                                               \
                       (ScmObj, ScmObj),                                     \
                       ((kar), (kdr)))

#define SCM_MAKE_IMMUTABLE_CONS(kar, kdr)                                    \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_IMMUTABLE_CONS,                          \
                       ScmObj,                                               \
                       (ScmObj, ScmObj),                                     \
                       ((kar), (kdr)))

#if SCM_USE_NUMBER
#define SCM_MAKE_INT(val)                                                    \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_INT,                                     \
                       ScmObj,                                               \
                       (scm_int_t),                                          \
                       (val))
#endif /* SCM_USE_NUMBER */

#if SCM_USE_CHAR
#define SCM_MAKE_CHAR(val)                                                   \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_CHAR,                                    \
                       ScmObj,                                               \
                       (scm_ichar_t),                                        \
                       (val))
#endif /* SCM_USE_CHAR */

#define SCM_MAKE_SYMBOL(name, val)                                           \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_SYMBOL,                                  \
                       ScmObj,                                               \
                       (char *, ScmObj),                                     \
                       ((name), (val)))

#if SCM_USE_STRING
#define SCM_MAKE_STRING(str, len)                                            \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_STRING,                                  \
                       ScmObj,                                               \
                       (char *, scm_int_t),                                  \
                       ((str), (len)))

#define SCM_MAKE_STRING_COPYING(str, len)                                    \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_STRING_COPYING,                          \
                       ScmObj,                                               \
                       (const char *, scm_int_t),                            \
                       ((str), (len)))

#define SCM_MAKE_IMMUTABLE_STRING(str, len)                                  \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_IMMUTABLE_STRING,                        \
                       ScmObj,                                               \
                       (char *, scm_int_t),                                  \
                       ((str), (len)))

#define SCM_MAKE_IMMUTABLE_STRING_COPYING(str, len)                          \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_IMMUTABLE_STRING_COPYING,                \
                       ScmObj,                                               \
                       (const char *, scm_int_t),                            \
                       ((str), (len)))

#define SCM_CONST_STRING(str)                                                \
    SCM_MAKE_IMMUTABLE_STRING_COPYING((str), SCM_STRLEN_UNKNOWN)
#define SCM_STRLEN_UNKNOWN -1
#endif /* SCM_USE_STRING */

#define SCM_MAKE_FUNC(type, func)                                            \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_FUNC,                                    \
                       ScmObj,                                               \
                       (enum ScmFuncTypeCode, ScmFuncType),                  \
                       ((type), (func)))

#define SCM_MAKE_CLOSURE(exp, env)                                           \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_CLOSURE,                                 \
                       ScmObj,                                               \
                       (ScmObj, ScmObj),                                     \
                       ((exp), (env)))

#if SCM_USE_VECTOR
#define SCM_MAKE_VECTOR(vec, len)                                            \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_VECTOR,                                  \
                       ScmObj,                                               \
                       (ScmObj *, scm_int_t),                                \
                       ((vec), (len)))

#define SCM_MAKE_IMMUTABLE_VECTOR(vec, len)                                  \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_IMMUTABLE_VECTOR,                        \
                       ScmObj,                                               \
                       (ScmObj *, scm_int_t),                                \
                       ((vec), (len)))
#endif /* SCM_USE_VECTOR */

#if SCM_USE_CONTINUATION
#define SCM_MAKE_CONTINUATION()                                              \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_CONTINUATION,                            \
                       ScmObj,                                               \
                       (void),                                               \
                       ())
#endif /* SCM_USE_CONTINUATION */

#define SCM_MAKE_VALUEPACKET(vals)                                           \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_VALUEPACKET,                             \
                       ScmObj,                                               \
                       (ScmObj),                                             \
                       (vals))

#if SCM_USE_PORT
#define SCM_MAKE_PORT(cport, flag)                                           \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_PORT,                                    \
                       ScmObj,                                               \
                       (struct ScmCharPort_ *, enum ScmPortFlag),            \
                       ((cport), (flag)))
#endif /* SCM_USE_PORT */

#if SCM_USE_HYGIENIC_MACRO
#define SCM_MAKE_HMACRO(r, e)                                                \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_HMACRO,                                  \
                       ScmObj,                                               \
                       (ScmObj, ScmObj),                                     \
                       ((r), (e)))

#define SCM_MAKE_FARSYMBOL(s, e)                                             \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_FARSYMBOL,                               \
                       ScmObj,                                               \
                       (ScmObj, ScmPackedEnv),                               \
                       ((s), (e)))

#define SCM_MAKE_SUBPAT(x, m)                                                \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_SUBPAT,                                  \
                       ScmObj,                                               \
                       (ScmObj, scm_int_t),                                  \
                       ((x), (m)))
#endif /* SCM_USE_HYGIENIC_MACRO */

#if SCM_USE_SSCM_EXTENSIONS
#define SCM_MAKE_C_POINTER(ptr)                                              \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_C_POINTER,                               \
                       ScmObj,                                               \
                       (void *),                                             \
                       (ptr))

#define SCM_MAKE_C_FUNCPOINTER(ptr)                                          \
    SCM_TYPESAFE_MACRO(SCM_SAL_MAKE_C_FUNCPOINTER,                           \
                       ScmObj,                                               \
                       (ScmCFunc),                                           \
                       (ptr))
#endif /* SCM_USE_SSCM_EXTENSIONS */

/*=======================================
  Object Accessors
=======================================*/
/* ScmObj Global Attribute */
#define SCM_TYPE(o) SCM_SAL_TYPE(o)

/* Type Confirmation */
#if SCM_ACCESSOR_ASSERT
#define SCM_ASSERT_TYPE(cond, o) (SCM_ASSERT(cond), (o))
#else /* SCM_ACCESSOR_ASSERT */
#define SCM_ASSERT_TYPE(cond, o) (o)
#endif /* SCM_ACCESSOR_ASSERT */
#define SCM_AS_NUMBER(o)        (SCM_ASSERT_TYPE(SCM_NUMBERP(o),        (o)))
#define SCM_AS_INT(o)           (SCM_ASSERT_TYPE(SCM_INTP(o),           (o)))
#define SCM_AS_CONS(o)          (SCM_ASSERT_TYPE(SCM_CONSP(o),          (o)))
#define SCM_AS_SYMBOL(o)        (SCM_ASSERT_TYPE(SCM_SYMBOLP(o),        (o)))
#define SCM_AS_CHAR(o)          (SCM_ASSERT_TYPE(SCM_CHARP(o),          (o)))
#define SCM_AS_STRING(o)        (SCM_ASSERT_TYPE(SCM_STRINGP(o),        (o)))
#define SCM_AS_FUNC(o)          (SCM_ASSERT_TYPE(SCM_FUNCP(o),          (o)))
#define SCM_AS_CLOSURE(o)       (SCM_ASSERT_TYPE(SCM_CLOSUREP(o),       (o)))
#define SCM_AS_VECTOR(o)        (SCM_ASSERT_TYPE(SCM_VECTORP(o),        (o)))
#define SCM_AS_PORT(o)          (SCM_ASSERT_TYPE(SCM_PORTP(o),          (o)))
#define SCM_AS_CONTINUATION(o)  (SCM_ASSERT_TYPE(SCM_CONTINUATIONP(o),  (o)))
#define SCM_AS_VALUEPACKET(o)   (SCM_ASSERT_TYPE(SCM_VALUEPACKETP(o),   (o)))
#define SCM_AS_C_POINTER(o)     (SCM_ASSERT_TYPE(SCM_C_POINTERP(o),     (o)))
#define SCM_AS_C_FUNCPOINTER(o) (SCM_ASSERT_TYPE(SCM_C_FUNCPOINTERP(o), (o)))
#if SCM_USE_HYGIENIC_MACRO
#define SCM_AS_HMACRO(o)        (SCM_ASSERT_TYPE(SCM_HMACROP(o),        (o)))
#define SCM_AS_FARSYMBOL(o)     (SCM_ASSERT_TYPE(SCM_FARSYMBOLP(o),     (o)))
#define SCM_AS_SUBPAT(o)        (SCM_ASSERT_TYPE(SCM_SUBPATP(o),        (o)))
#endif

#define SCM_NUMBERP(o)                  SCM_SAL_NUMBERP(o)

#define SCM_INTP(o)                     SCM_SAL_INTP(o)
#define SCM_INT_VALUE(o)                SCM_SAL_INT_VALUE(o)
#define SCM_INT_SET_VALUE(o, val)       SCM_SAL_INT_SET_VALUE((o), (val))

#define SCM_CONSP(o)                    SCM_SAL_CONSP(o)
#define SCM_CONS_CAR(o)                 SCM_SAL_CONS_CAR(o)
#define SCM_CONS_CDR(o)                 SCM_SAL_CONS_CDR(o)
#define SCM_CONS_SET_CAR(o, kar)        SCM_SAL_CONS_SET_CAR((o), (kar))
#define SCM_CONS_SET_CDR(o, kdr)        SCM_SAL_CONS_SET_CDR((o), (kdr))
#define SCM_CONS_MUTABLEP(o)            SCM_SAL_CONS_MUTABLEP(o)
#define SCM_CONS_SET_MUTABLE(o)         SCM_SAL_CONS_SET_MUTABLE(o)
#define SCM_CONS_SET_IMMUTABLE(o)       SCM_SAL_CONS_SET_IMMUTABLE(o)

#define SCM_SYMBOLP(o)                  SCM_SAL_SYMBOLP(o)
#define SCM_SYMBOL_NAME(o)              SCM_SAL_SYMBOL_NAME(o)
#define SCM_SYMBOL_VCELL(o)             SCM_SAL_SYMBOL_VCELL(o)
#define SCM_SYMBOL_SET_NAME(o, name)    SCM_SAL_SYMBOL_SET_NAME((o), (name))
#define SCM_SYMBOL_SET_VCELL(o, val)    SCM_SAL_SYMBOL_SET_VCELL((o), (val))

#define SCM_CHARP(o)                    SCM_SAL_CHARP(o)
#define SCM_CHAR_VALUE(o)               SCM_SAL_CHAR_VALUE(o)
#define SCM_CHAR_SET_VALUE(o, val)      SCM_SAL_CHAR_SET_VALUE((o), (val))

#define SCM_STRINGP(o)                  SCM_SAL_STRINGP(o)
#define SCM_STRING_STR(o)               SCM_SAL_STRING_STR(o)
#define SCM_STRING_LEN(o)               SCM_SAL_STRING_LEN(o)
#define SCM_STRING_SET_STR(o, str)      SCM_SAL_STRING_SET_STR((o), (str))
#define SCM_STRING_SET_LEN(o, len)      SCM_SAL_STRING_SET_LEN((o), (len))
#define SCM_STRING_MUTABLEP(o)          SCM_SAL_STRING_MUTABLEP(o)
#define SCM_STRING_SET_MUTABLE(o)       SCM_SAL_STRING_SET_MUTABLE(o)
#define SCM_STRING_SET_IMMUTABLE(o)     SCM_SAL_STRING_SET_IMMUTABLE(o)

#define SCM_FUNCP(o)                    SCM_SAL_FUNCP(o)
#define SCM_FUNC_TYPECODE(o)            SCM_SAL_FUNC_TYPECODE(o)
#define SCM_FUNC_CFUNC(o)               SCM_SAL_FUNC_CFUNC(o)
#define SCM_FUNC_SET_TYPECODE(o, type)  SCM_SAL_FUNC_SET_TYPECODE((o), (type))
#define SCM_FUNC_SET_CFUNC(o, func)     SCM_SAL_FUNC_SET_CFUNC((o), (func))
#define SCM_SYNTAXP(o)    (SCM_FUNCP(o)                                      \
                           && (SCM_FUNC_TYPECODE(o) & SCM_FUNCTYPE_SYNTAX))
#define SCM_PROCEDUREP(o) ((SCM_FUNCP(o)                                     \
                            && !(SCM_FUNC_TYPECODE(o) & SCM_FUNCTYPE_SYNTAX)) \
                           || SCM_CLOSUREP(o)                                \
                           || SCM_CONTINUATIONP(o))

#define SCM_CLOSUREP(o)                 SCM_SAL_CLOSUREP(o)
#define SCM_CLOSURE_EXP(o)              SCM_SAL_CLOSURE_EXP(o)
#define SCM_CLOSURE_SET_EXP(o, exp)     SCM_SAL_CLOSURE_SET_EXP((o), (exp))
#define SCM_CLOSURE_ENV(o)              SCM_SAL_CLOSURE_ENV(o)
#define SCM_CLOSURE_SET_ENV(o, env)     SCM_SAL_CLOSURE_SET_ENV((o), (env))

#define SCM_VECTORP(o)                  SCM_SAL_VECTORP(o)
#define SCM_VECTOR_VEC(o)               SCM_SAL_VECTOR_VEC(o)
#define SCM_VECTOR_LEN(o)               SCM_SAL_VECTOR_LEN(o)
#define SCM_VECTOR_SET_VEC(o, vec)      SCM_SAL_VECTOR_SET_VEC((o), (vec))
#define SCM_VECTOR_SET_LEN(o, len)      SCM_SAL_VECTOR_SET_LEN((o), (len))
#define SCM_VECTOR_MUTABLEP(o)          SCM_SAL_VECTOR_MUTABLEP(o)
#define SCM_VECTOR_SET_MUTABLE(o)       SCM_SAL_VECTOR_SET_MUTABLE(o)
#define SCM_VECTOR_SET_IMMUTABLE(o)     SCM_SAL_VECTOR_SET_IMMUTABLE(o)
#define SCM_VECTOR_VALID_INDEXP(o, i)   (0 <= (i) && (i) < SCM_VECTOR_LEN(o))

#define SCM_PORTP(o)                    SCM_SAL_PORTP(o)
#define SCM_PORT_FLAG(o)                SCM_SAL_PORT_FLAG(o)
#define SCM_PORT_IMPL(o)                SCM_SAL_PORT_IMPL(o)
#define SCM_PORT_SET_FLAG(o, flag)      SCM_SAL_PORT_SET_FLAG((o), (flag))
#define SCM_PORT_SET_IMPL(o, impl)      SCM_SAL_PORT_SET_IMPL((o), (impl))

#define SCM_CONTINUATIONP(o)            SCM_SAL_CONTINUATIONP(o)
#define SCM_CONTINUATION_OPAQUE(o)      SCM_SAL_CONTINUATION_OPAQUE(o)
#define SCM_CONTINUATION_TAG(o)         SCM_SAL_CONTINUATION_TAG(o)
#define SCM_CONTINUATION_SET_OPAQUE(o, val)                                  \
    SCM_SAL_CONTINUATION_SET_OPAQUE((o), (val))
#define SCM_CONTINUATION_SET_TAG(o, val)                                     \
    SCM_SAL_CONTINUATION_SET_TAG((o), (val))

#define SCM_VALUEPACKETP(o)             SCM_SAL_VALUEPACKETP(o)

#define SCM_CONSTANTP(o)                SCM_SAL_CONSTANTP(o)

#define SCM_C_POINTERP(o)               SCM_SAL_C_POINTERP(o)
#define SCM_C_POINTER_VALUE(o)          SCM_SAL_C_POINTER_VALUE(o)
#define SCM_C_POINTER_SET_VALUE(o, ptr) SCM_SAL_C_POINTER_SET_VALUE((o), (ptr))

#define SCM_C_FUNCPOINTERP(o)           SCM_SAL_C_FUNCPOINTERP(o)
#define SCM_C_FUNCPOINTER_VALUE(o)      SCM_SAL_C_FUNCPOINTER_VALUE(o)
#define SCM_C_FUNCPOINTER_SET_VALUE(o, funcptr)                              \
    SCM_SAL_C_FUNCPOINTER_SET_VALUE((o), (funcptr))

#if SCM_USE_HYGIENIC_MACRO
#define SCM_HMACROP(o)                  SCM_SAL_HMACROP(o)
#define SCM_HMACRO_RULES(o)             SCM_SAL_HMACRO_RULES(o)
#define SCM_HMACRO_SET_RULES(o, r)      SCM_SAL_HMACRO_SET_RULES((o), (r))
#define SCM_HMACRO_ENV(o)               SCM_SAL_HMACRO_ENV(o)
#define SCM_HMACRO_SET_ENV(o, e)        SCM_SAL_HMACRO_SET_ENV((o), (e))
#define SCM_FARSYMBOLP(o)               SCM_SAL_FARSYMBOLP(o)
#define SCM_FARSYMBOL_SYM(o)            SCM_SAL_FARSYMBOL_SYM(o)
#define SCM_FARSYMBOL_SET_SYM(o, s)     SCM_SAL_FARSYMBOL_SET_SYM((o), (s))
#define SCM_FARSYMBOL_ENV(o)            SCM_SAL_FARSYMBOL_ENV(o)
#define SCM_FARSYMBOL_SET_ENV(o, e)     SCM_SAL_FARSYMBOL_SET_ENV((o), (e))
#define SCM_SUBPATP(o)                  SCM_SAL_SUBPATP(o)
#define SCM_SUBPAT_OBJ(o)               SCM_SAL_SUBPAT_OBJ(o)
#define SCM_SUBPAT_META(o)              SCM_SAL_SUBPAT_META(o)
#define SCM_SUBPAT_SET_OBJ(o, x)        SCM_SAL_SUBPAT_SET_OBJ((o), (x))
#define SCM_SUBPAT_SET_META(o, m)       SCM_SAL_SUBPAT_SET_META((o), (m))


/* Marks where pattern variable were present in the original pattern.
 * Records the symbol and index of the pattern variable.  See macro.c
 * for the terminology.  It's generally not a good idea to directly
 * manipulate compiled macro bodies, though. */
#if SCM_DEBUG_MACRO
#define SCM_SUBPAT_MAKE_PVAR(obj, idx)  (MAKE_SUBPAT((obj), (idx)))
#else  /* not SCM_DEBUG_MACRO */
#define SCM_SUBPAT_MAKE_PVAR(obj, idx)  MAKE_SUBPAT(SCM_NULL, (idx))
#endif /* not SCM_DEBUG_MACRO */
#define SCM_SUBPAT_PVAR_INDEX(obj)      SCM_SUBPAT_META(obj)
#define SCM_SUBPAT_PVARP(obj)           (SCM_SUBPAT_PVAR_INDEX(obj) >= 0)

/* Repeatable subpattern.  Contains the subpattern and the number of
 * pattern variables within it. */
#define SCM_SUBPAT_MAKE_REPPAT(subpat, pvcount) \
    (MAKE_SUBPAT((subpat), ~(pvcount)))
#define SCM_SUBPAT_REPPAT_PAT(subpat)     SCM_SUBPAT_OBJ(subpat)
#define SCM_SUBPAT_REPPAT_PVCOUNT(subpat) (~(SCM_SUBPAT_META(subpat)))
#define SCM_SUBPAT_REPPATP(obj)           (SCM_SUBPAT_PVAR_INDEX(obj) < 0)

#define SCM_IDENTIFIERP(o) (SCM_SYMBOLP(o) || SCM_FARSYMBOLP(o))
#else  /* not SCM_USE_HYGIENIC_MACRO */
#define SCM_IDENTIFIERP(o) SCM_SYMBOLP(o)
#endif /* not SCM_USE_HYGIENIC_MACRO */

/*===========================================================================
  Hygienic Macro
===========================================================================*/
#if SCM_USE_HYGIENIC_MACRO
/**
 * Strips the argument of binding information.  Syntaxes that take
 * verbatim data as their argument (e.g. quote, case) must unwrap that
 * argument before using it.  UNWRAP_SYNTAX() may or may not
 * destructively unwrap the input, so the return value must always be
 * used.  The caller shouldn't assume the input is equal? before and
 * after unwrapping.
 */
#define SCM_UNWRAP_SYNTAX(o)  scm_unwrap_syntaxx((o))

/**
 * If the argument is an identifier, it is stripped of binding
 * information and returned.  Otherwise, the argument is returned
 * without any modification.
 */
#define SCM_UNWRAP_KEYWORD(o) scm_unwrap_keyword(o)
#else  /* not SCM_USE_HYGIENIC_MACRO */
#define SCM_UNWRAP_SYNTAX(o)  (o)
#define SCM_UNWRAP_KEYWORD(o) (o)
#endif

/*===========================================================================
  Environment Specifiers
===========================================================================*/
#define SCM_INTERACTION_ENV SCM_SAL_INTERACTION_ENV
/*
 * Current implementation cannot handle scheme-report-environment and
 * null-environment properly. Be careful to use these environemnts.
 */
#define SCM_R5RS_ENV        SCM_SAL_R5RS_ENV
#define SCM_NULL_ENV        SCM_SAL_NULL_ENV

#define SCM_ENVP(env)       SCM_SAL_ENVP(env)

/*===========================================================================
  Abstract ScmObj Reference For Storage-Representation Independent Efficient
  List Operations
===========================================================================*/
#define SCM_INVALID_REF       SCM_SAL_INVALID_REF

#define SCM_REF_CAR(kons)     SCM_SAL_REF_CAR(kons)
#define SCM_REF_CDR(kons)     SCM_SAL_REF_CDR(kons)
#define SCM_REF_OFF_HEAP(obj) SCM_SAL_REF_OFF_HEAP(obj)

/* SCM_DEREF(ref) is not permitted to be used as lvalue */
#define SCM_DEREF(ref)        SCM_SAL_DEREF(ref)

/* RFC: Is there a better name? */
#define SCM_SET(ref, obj)     SCM_SAL_SET((ref), (obj))

/*===========================================================================
  Special Constants and Predicates
===========================================================================*/
#define SCM_INVALID SCM_SAL_INVALID
#define SCM_NULL    SCM_SAL_NULL
#define SCM_TRUE    SCM_SAL_TRUE
#define SCM_FALSE   SCM_SAL_FALSE
#define SCM_EOF     SCM_SAL_EOF
#define SCM_UNBOUND SCM_SAL_UNBOUND
#define SCM_UNDEF   SCM_SAL_UNDEF

#define SCM_EQ(a, b)   (SCM_SAL_EQ((a), (b)))
#define SCM_NULLP(o)   (SCM_EQ((o),  SCM_NULL))
#define SCM_FALSEP(o)  (SCM_EQ((o),  SCM_FALSE))
#define SCM_NFALSEP(o) (!SCM_EQ((o), SCM_FALSE))
#define SCM_EOFP(o)    (SCM_EQ((o),  SCM_EOF))

/*===========================================================================
  Predefined Symbols
===========================================================================*/
/* for list construction */
#define SCM_SYM_QUOTE            SCM_SAL_SYM_QUOTE
#define SCM_SYM_QUASIQUOTE       SCM_SAL_SYM_QUASIQUOTE
#define SCM_SYM_UNQUOTE          SCM_SAL_SYM_UNQUOTE
#define SCM_SYM_UNQUOTE_SPLICING SCM_SAL_SYM_UNQUOTE_SPLICING

/*=======================================
   Evaluator's State
=======================================*/
enum ScmValueType {
    SCM_VALTYPE_AS_IS     = scm_false,
    SCM_VALTYPE_NEED_EVAL = scm_true
};

typedef struct ScmEvalState_ ScmEvalState;
struct ScmEvalState_ {
    ScmObj env;
    enum ScmValueType ret_type;
};

/* Use these constructors instead of manually initialize each members because
 * another member may be added. Such member will implicitly be initialized
 * properly as long as the constructors are used. */
#define SCM_EVAL_STATE_INIT(state)                                           \
    SCM_EVAL_STATE_INIT2((state), SCM_INTERACTION_ENV, SCM_VALTYPE_NEED_EVAL)

#define SCM_EVAL_STATE_INIT1(state, env)                                     \
    SCM_EVAL_STATE_INIT2((state), (env), SCM_VALTYPE_NEED_EVAL)

#define SCM_EVAL_STATE_INIT2(state, _env, _ret_type)                         \
    do {                                                                     \
        (state).env      = (_env);                                           \
        (state).ret_type = (_ret_type);                                      \
    } while (/* CONSTCOND */ 0)

#define SCM_FINISH_TAILREC_CALL(obj, eval_state)                             \
    (((eval_state)->ret_type == SCM_VALTYPE_NEED_EVAL)                       \
     ? ((eval_state)->ret_type = SCM_VALTYPE_AS_IS,                          \
        EVAL((obj), (eval_state)->env))                                      \
     : (obj))

/*=======================================
   Format Strings
=======================================*/
enum ScmFormatCapability {
    SCM_FMT_NONE            = 0,
    SCM_FMT_RAW_C           = 1 << 0,  /* take raw C values from va_list */
    SCM_FMT_SRFI28          = 1 << 1,
    SCM_FMT_SRFI48_ADDENDUM = 1 << 2,
    SCM_FMT_LEADING_ZEROS   = 1 << 3,  /* padding with zeros "00034" */
    SCM_FMT_PREFIXED_RADIX  = 1 << 4,  /* "8x" 65536 => "    ffff" */

    SCM_FMT_SRFI48        = (SCM_FMT_SRFI28 | SCM_FMT_SRFI48_ADDENDUM),
    SCM_FMT_SSCM_ADDENDUM = (SCM_FMT_LEADING_ZEROS | SCM_FMT_PREFIXED_RADIX),
    SCM_FMT_SSCM          = (SCM_FMT_SRFI48 | SCM_FMT_SSCM_ADDENDUM),
    SCM_FMT_INTERNAL      = (SCM_FMT_RAW_C | SCM_FMT_SSCM)
};

typedef struct ScmValueFormat_ ScmValueFormat;
struct ScmValueFormat_ {
    signed char width;       /* integer part width */
    signed char frac_width;  /* fractional part width */
    char pad;                /* char for padding prefix */
    char signedp;
};

#define SCM_VALUE_FORMAT_INIT(vfmt)                                          \
    SCM_VALUE_FORMAT_INIT4(vfmt, -1, -1, ' ', scm_true)

#define SCM_VALUE_FORMAT_INIT4(vfmt, w, fw, p, s)                            \
    do {                                                                     \
        vfmt.width = w;                                                      \
        vfmt.frac_width = fw;                                                \
        vfmt.pad = p;                                                        \
        vfmt.signedp = s;                                                    \
    } while (/* CONSTCOND */ 0)

#define SCM_VALUE_FORMAT_SPECIFIEDP(vfmt)                                    \
    (vfmt.width > 0 || vfmt.frac_width > 0 || vfmt.pad != ' ' || !vfmt.signedp)

/*=======================================
  Function types
=======================================*/
struct scm_func_registration_info {
    const char *funcname;
    ScmFuncType c_func;
    enum ScmFuncTypeCode typecode;
};

typedef ScmObj (*scm_reduction_operator)(ScmObj, ScmObj, enum ScmReductionState *);
typedef ScmObj (*scm_syntax_fixed_0)(ScmObj);
typedef ScmObj (*scm_syntax_fixed_tailrec_0)(ScmEvalState *);
typedef ScmObj (*scm_syntax_variadic_0)(ScmObj, ScmObj);
typedef ScmObj (*scm_syntax_variadic_tailrec_0)(ScmObj, ScmEvalState *);
typedef ScmObj (*scm_procedure_fixed_0)(void);
typedef ScmObj (*scm_procedure_fixed_tailrec_0)(ScmEvalState *);
typedef ScmObj (*scm_procedure_variadic_0)(ScmObj);
typedef ScmObj (*scm_procedure_variadic_tailrec_0)(ScmObj, ScmEvalState *);
#if SCM_FUNCTYPE_MAND_MAX >= 1
typedef ScmObj (*scm_syntax_fixed_1)(ScmObj, ScmObj);
typedef ScmObj (*scm_syntax_fixed_tailrec_1)(ScmObj, ScmEvalState *);
typedef ScmObj (*scm_syntax_variadic_1)(ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_syntax_variadic_tailrec_1)(ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_procedure_fixed_1)(ScmObj);
typedef ScmObj (*scm_procedure_fixed_tailrec_1)(ScmObj, ScmEvalState *);
typedef ScmObj (*scm_procedure_variadic_1)(ScmObj, ScmObj);
typedef ScmObj (*scm_procedure_variadic_tailrec_1)(ScmObj, ScmObj, ScmEvalState *);
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 2
typedef ScmObj (*scm_syntax_fixed_2)(ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_syntax_fixed_tailrec_2)(ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_syntax_variadic_2)(ScmObj, ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_syntax_variadic_tailrec_2)(ScmObj, ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_procedure_fixed_2)(ScmObj, ScmObj);
typedef ScmObj (*scm_procedure_fixed_tailrec_2)(ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_procedure_variadic_2)(ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_procedure_variadic_tailrec_2)(ScmObj, ScmObj, ScmObj, ScmEvalState *);
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 3
typedef ScmObj (*scm_syntax_fixed_3)(ScmObj, ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_syntax_fixed_tailrec_3)(ScmObj, ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_syntax_variadic_3)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_syntax_variadic_tailrec_3)(ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_procedure_fixed_3)(ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_procedure_fixed_tailrec_3)(ScmObj, ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_procedure_variadic_3)(ScmObj, ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_procedure_variadic_tailrec_3)(ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState *);
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 4
typedef ScmObj (*scm_syntax_fixed_4)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_syntax_fixed_tailrec_4)(ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_syntax_variadic_4)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_syntax_variadic_tailrec_4)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_procedure_fixed_4)(ScmObj, ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_procedure_fixed_tailrec_4)(ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_procedure_variadic_4)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_procedure_variadic_tailrec_4)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState *);
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 5
typedef ScmObj (*scm_syntax_fixed_5)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_syntax_fixed_tailrec_5)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_syntax_variadic_5)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_syntax_variadic_tailrec_5)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_procedure_fixed_5)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_procedure_fixed_tailrec_5)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState *);
typedef ScmObj (*scm_procedure_variadic_5)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj);
typedef ScmObj (*scm_procedure_variadic_tailrec_5)(ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj, ScmEvalState *);
#endif

/*=======================================
  Variable Declarations
=======================================*/
/* storage-gc.c */
#if SCM_GCC4_READY_GC
/*
 * The variable to ensure that a call of scm_gc_protect_stack() is
 * uninlined in portable way through (*f)().
 *
 * Don't access this variables directly. Use SCM_GC_PROTECTED_CALL*() instead.
 */
SCM_GLOBAL_VARS_BEGIN(gc);
ScmObj *(*volatile scm_gc_current_stack_fp)(void);
ScmObj *(*volatile scm_gc_protect_stack_fp)(ScmObj *);
SCM_GLOBAL_VARS_END(gc);
SCM_DECLARE_EXPORTED_VARS(gc);
#define scm_gc_current_stack_fp SCM_GLOBAL_VAR(gc, scm_gc_current_stack_fp)
#define scm_gc_protect_stack_fp SCM_GLOBAL_VAR(gc, scm_gc_protect_stack_fp)
#endif /* SCM_GCC4_READY_GC */

/*=======================================
  Function Declarations
=======================================*/
/*===========================================================================
   SigScheme: Core Functions
===========================================================================*/
/* sigscheme.c */
SCM_EXPORT void scm_initialize(const ScmStorageConf *storage_conf);
SCM_EXPORT void scm_finalize(void);
#if SCM_USE_EVAL_C_STRING
SCM_EXPORT ScmObj scm_eval_c_string(const char *exp);
#endif
#if SCM_COMPAT_SIOD
SCM_EXPORT ScmObj scm_return_value(void);
#endif

/* module.c */
SCM_EXPORT void scm_provide(ScmObj feature);
SCM_EXPORT scm_bool scm_providedp(ScmObj feature);
SCM_EXPORT scm_bool scm_use(const char *feature);
SCM_EXPORT ScmObj scm_s_use(ScmObj feature, ScmObj env);
SCM_EXPORT ScmObj scm_register_func(const char *name, ScmFuncType func,
                         enum ScmFuncTypeCode type);
SCM_EXPORT void scm_register_funcs(const struct scm_func_registration_info *table);
SCM_EXPORT void scm_define_alias(const char *newsym, const char *sym);

/* alloc.c */
SCM_EXPORT void *scm_malloc_aligned(size_t size);
SCM_EXPORT void *scm_malloc(size_t size);
SCM_EXPORT void *scm_calloc(size_t number, size_t size);
SCM_EXPORT void *scm_realloc(void *ptr, size_t size);
SCM_EXPORT char *scm_strdup(const char *str);

/* storage-gc.c */
SCM_EXPORT void scm_gc_protect(ScmObj *var);
SCM_EXPORT void scm_gc_protect_with_init(ScmObj *var, ScmObj init_val);
SCM_EXPORT void scm_gc_unprotect(ScmObj *var);
#if SCM_GCC4_READY_GC
/*
 * Ordinary programs should not call these functions directly. Use
 * SCM_GC_PROTECTED_CALL*() instead.
 */
#ifdef __GNUC__
#define scm_gc_current_stack scm_gc_current_stack_internal
#define scm_gc_protect_stack scm_gc_protect_stack_internal
#else /* __GNUC__ */
#define scm_gc_current_stack (*scm_gc_current_stack_fp)
#define scm_gc_protect_stack (*scm_gc_protect_stack_fp)
#endif /* __GNUC__ */

SCM_EXPORT ScmObj *scm_gc_current_stack_internal(void) SCM_NOINLINE;
SCM_EXPORT ScmObj *scm_gc_protect_stack_internal(ScmObj *designated_stack_start) SCM_NOINLINE;
#else /* SCM_GCC4_READY_GC */
SCM_EXPORT void scm_gc_protect_stack(ScmObj *stack_start);
#endif /* SCM_GCC4_READY_GC */
SCM_EXPORT void scm_gc_unprotect_stack(ScmObj *stack_start);

/* symbol.c */
SCM_EXPORT ScmObj scm_intern(const char *name);
SCM_EXPORT ScmObj scm_symbol_bound_to(ScmObj obj);

/* error.c */
SCM_EXPORT int  scm_debug_categories(void);
SCM_EXPORT void scm_set_debug_categories(int categories);
SCM_EXPORT int  scm_predefined_debug_categories(void);
SCM_EXPORT void scm_categorized_debug(int category, const char *msg, ...);
SCM_EXPORT void scm_debug(const char *msg, ...);
SCM_EXPORT void scm_die(const char *msg, const char *filename, int line) SCM_NORETURN;
SCM_EXPORT void scm_plain_error(const char *msg, ...) SCM_NORETURN;
SCM_EXPORT void scm_error(const char *funcname, const char *msg, ...) SCM_NORETURN;
SCM_EXPORT void scm_error_obj(const char *funcname, const char *msg,
                   ScmObj obj) SCM_NORETURN;
SCM_EXPORT void scm_show_backtrace(ScmObj trace_stack);
SCM_EXPORT ScmObj scm_make_error_obj(ScmObj reason, ScmObj objs);
SCM_EXPORT void scm_raise_error(ScmObj err_obj) SCM_NORETURN;
SCM_EXPORT void scm_fatal_error(const char *msg) SCM_NORETURN;
SCM_EXPORT void scm_set_fatal_error_callback(void (*cb)(void));
SCM_EXPORT ScmObj scm_p_error_objectp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_fatal_error(ScmObj err_obj) SCM_NORETURN;
SCM_EXPORT ScmObj scm_p_inspect_error(ScmObj err_obj);
SCM_EXPORT ScmObj scm_p_backtrace(void);

/* eval.c */
SCM_EXPORT ScmObj scm_call(ScmObj proc, ScmObj args);
SCM_EXPORT ScmObj scm_p_eval(ScmObj obj, ScmObj env);
SCM_EXPORT ScmObj scm_p_apply(ScmObj proc, ScmObj arg0, ScmObj rest,
                              ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_p_scheme_report_environment(ScmObj version);
SCM_EXPORT ScmObj scm_p_null_environment(ScmObj version);
SCM_EXPORT ScmObj scm_p_interaction_environment(void);

/* syntax.c */
SCM_EXPORT ScmObj scm_s_quote(ScmObj datum, ScmObj env);
SCM_EXPORT ScmObj scm_s_lambda(ScmObj formals, ScmObj body, ScmObj env);
SCM_EXPORT ScmObj scm_s_if(ScmObj test, ScmObj conseq, ScmObj rest,
                           ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_s_setx(ScmObj var, ScmObj val, ScmObj env);
SCM_EXPORT ScmObj scm_s_cond(ScmObj args, ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_s_case(ScmObj key, ScmObj args,
                             ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_s_and(ScmObj args, ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_s_or(ScmObj args, ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_s_let(ScmObj args, ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_s_letstar(ScmObj bindings, ScmObj body,
                                ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_s_letrec(ScmObj bindings, ScmObj body,
                               ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_s_begin(ScmObj args, ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_s_do(ScmObj bindings, ScmObj test_exps, ScmObj commands,
                           ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_s_delay(ScmObj expr, ScmObj env);
SCM_EXPORT ScmObj scm_s_quasiquote(ScmObj datum, ScmObj env);
SCM_EXPORT ScmObj scm_s_unquote(ScmObj dummy, ScmObj env);
SCM_EXPORT ScmObj scm_s_unquote_splicing(ScmObj dummy, ScmObj env);
SCM_EXPORT ScmObj scm_s_define(ScmObj var, ScmObj rest, ScmObj env);

/* procedure.c */
SCM_EXPORT ScmObj scm_p_eqp(ScmObj obj1, ScmObj obj2);
SCM_EXPORT ScmObj scm_p_eqvp(ScmObj obj1, ScmObj obj2);
SCM_EXPORT ScmObj scm_p_equalp(ScmObj obj1, ScmObj obj2);
SCM_EXPORT ScmObj scm_p_not(ScmObj obj);
SCM_EXPORT ScmObj scm_p_booleanp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_symbolp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_symbol2string(ScmObj sym);
SCM_EXPORT ScmObj scm_p_string2symbol(ScmObj str);
SCM_EXPORT ScmObj scm_p_procedurep(ScmObj obj);
SCM_EXPORT ScmObj scm_p_map(ScmObj proc, ScmObj args);
SCM_EXPORT ScmObj scm_p_for_each(ScmObj proc, ScmObj args);
SCM_EXPORT ScmObj scm_p_force(ScmObj closure);
SCM_EXPORT ScmObj scm_p_call_with_current_continuation(ScmObj proc, ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_p_values(ScmObj args);
SCM_EXPORT ScmObj scm_p_call_with_values(ScmObj producer, ScmObj consumer,
                                         ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_p_dynamic_wind(ScmObj before, ScmObj thunk,
                                     ScmObj after);

/* list.c */
SCM_EXPORT ScmObj scm_p_car(ScmObj obj);
SCM_EXPORT ScmObj scm_p_cdr(ScmObj obj);
SCM_EXPORT ScmObj scm_p_pairp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_cons(ScmObj car, ScmObj cdr);
SCM_EXPORT ScmObj scm_p_set_carx(ScmObj pair, ScmObj car);
SCM_EXPORT ScmObj scm_p_set_cdrx(ScmObj pair, ScmObj cdr);
SCM_EXPORT ScmObj scm_p_caar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cadr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cdar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cddr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_caddr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cdddr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_list(ScmObj args);
SCM_EXPORT ScmObj scm_p_nullp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_listp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_length(ScmObj obj);
SCM_EXPORT ScmObj scm_p_append(ScmObj args);
SCM_EXPORT ScmObj scm_p_reverse(ScmObj lst);
SCM_EXPORT ScmObj scm_p_list_tail(ScmObj lst, ScmObj k);
SCM_EXPORT ScmObj scm_p_list_ref(ScmObj lst, ScmObj k);
SCM_EXPORT ScmObj scm_p_memq(ScmObj obj, ScmObj lst);
SCM_EXPORT ScmObj scm_p_memv(ScmObj obj, ScmObj lst);
SCM_EXPORT ScmObj scm_p_member(ScmObj obj, ScmObj lst);
SCM_EXPORT ScmObj scm_p_assq(ScmObj obj, ScmObj alist);
SCM_EXPORT ScmObj scm_p_assv(ScmObj obj, ScmObj alist);
SCM_EXPORT ScmObj scm_p_assoc(ScmObj obj, ScmObj alist);

#if SCM_USE_NUMBER
/* number.c */
SCM_EXPORT char *scm_int2string(ScmValueFormat vfmt, uintmax_t n, int radix);
SCM_EXPORT ScmObj scm_p_add(ScmObj left, ScmObj right,
                            enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_subtract(ScmObj left, ScmObj right,
                                 enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_multiply(ScmObj left, ScmObj right,
                                 enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_divide(ScmObj left, ScmObj right,
                               enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_equal(ScmObj left, ScmObj right,
                              enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_less(ScmObj left, ScmObj right,
                             enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_less_equal(ScmObj left, ScmObj right,
                                   enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_greater(ScmObj left, ScmObj right,
                                enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_greater_equal(ScmObj left, ScmObj right,
                                      enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_numberp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_integerp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_zerop(ScmObj n);
SCM_EXPORT ScmObj scm_p_positivep(ScmObj n);
SCM_EXPORT ScmObj scm_p_negativep(ScmObj n);
SCM_EXPORT ScmObj scm_p_oddp(ScmObj n);
SCM_EXPORT ScmObj scm_p_evenp(ScmObj n);
SCM_EXPORT ScmObj scm_p_max(ScmObj left, ScmObj right,
                            enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_min(ScmObj left, ScmObj right,
                            enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_abs(ScmObj _n);
SCM_EXPORT ScmObj scm_p_quotient(ScmObj _n1, ScmObj _n2);
SCM_EXPORT ScmObj scm_p_modulo(ScmObj _n1, ScmObj _n2);
SCM_EXPORT ScmObj scm_p_remainder(ScmObj _n1, ScmObj _n2);
SCM_EXPORT ScmObj scm_p_number2string (ScmObj num, ScmObj args);
SCM_EXPORT ScmObj scm_p_string2number(ScmObj str, ScmObj args);
#endif /* SCM_USE_NUMBER */

#if SCM_USE_CHAR
/* char.c */
SCM_EXPORT ScmObj scm_p_charp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_char_equalp(ScmObj ch1, ScmObj ch2);
SCM_EXPORT ScmObj scm_p_char_lessp(ScmObj ch1, ScmObj ch2);
SCM_EXPORT ScmObj scm_p_char_greaterp(ScmObj ch1, ScmObj ch2);
SCM_EXPORT ScmObj scm_p_char_less_equalp(ScmObj ch1, ScmObj ch2);
SCM_EXPORT ScmObj scm_p_char_greater_equalp(ScmObj ch1, ScmObj ch2);
SCM_EXPORT ScmObj scm_p_char_ci_equalp(ScmObj ch1, ScmObj ch2);
SCM_EXPORT ScmObj scm_p_char_ci_lessp(ScmObj ch1, ScmObj ch2);
SCM_EXPORT ScmObj scm_p_char_ci_greaterp(ScmObj ch1, ScmObj ch2);
SCM_EXPORT ScmObj scm_p_char_ci_less_equalp(ScmObj ch1, ScmObj ch2);
SCM_EXPORT ScmObj scm_p_char_ci_greater_equalp(ScmObj ch1, ScmObj ch2);
SCM_EXPORT ScmObj scm_p_char_alphabeticp(ScmObj ch);
SCM_EXPORT ScmObj scm_p_char_numericp(ScmObj ch);
SCM_EXPORT ScmObj scm_p_char_whitespacep(ScmObj ch);
SCM_EXPORT ScmObj scm_p_char_upper_casep(ScmObj ch);
SCM_EXPORT ScmObj scm_p_char_lower_casep(ScmObj ch);
SCM_EXPORT ScmObj scm_p_char2integer(ScmObj ch);
SCM_EXPORT ScmObj scm_p_integer2char(ScmObj n);
SCM_EXPORT ScmObj scm_p_char_upcase(ScmObj ch);
SCM_EXPORT ScmObj scm_p_char_downcase(ScmObj ch);
#endif /* SCM_USE_CHAR */

#if SCM_USE_STRING
/* string.c */
SCM_EXPORT ScmObj scm_p_stringp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_make_string(ScmObj length, ScmObj args);
SCM_EXPORT ScmObj scm_p_string(ScmObj args);
SCM_EXPORT ScmObj scm_p_string_length(ScmObj str);
SCM_EXPORT ScmObj scm_p_string_ref(ScmObj str, ScmObj k);
SCM_EXPORT ScmObj scm_p_string_setx(ScmObj str, ScmObj k, ScmObj ch);
SCM_EXPORT ScmObj scm_p_stringequalp(ScmObj str1, ScmObj str2);
SCM_EXPORT ScmObj scm_p_string_ci_equalp(ScmObj str1, ScmObj str2);
SCM_EXPORT ScmObj scm_p_string_greaterp(ScmObj str1, ScmObj str2);
SCM_EXPORT ScmObj scm_p_string_lessp(ScmObj str1, ScmObj str2);
SCM_EXPORT ScmObj scm_p_string_greater_equalp(ScmObj str1, ScmObj str2);
SCM_EXPORT ScmObj scm_p_string_less_equalp(ScmObj str1, ScmObj str2);
SCM_EXPORT ScmObj scm_p_string_ci_greaterp(ScmObj str1, ScmObj str2);
SCM_EXPORT ScmObj scm_p_string_ci_lessp(ScmObj str1, ScmObj str2);
SCM_EXPORT ScmObj scm_p_string_ci_greater_equalp(ScmObj str1, ScmObj str2);
SCM_EXPORT ScmObj scm_p_string_ci_less_equalp(ScmObj str1, ScmObj str2);
SCM_EXPORT ScmObj scm_p_substring(ScmObj str, ScmObj start, ScmObj end);
SCM_EXPORT ScmObj scm_p_string_append(ScmObj args);
SCM_EXPORT ScmObj scm_p_string2list(ScmObj str);
SCM_EXPORT ScmObj scm_p_list2string(ScmObj lst);
SCM_EXPORT ScmObj scm_p_string_copy(ScmObj str);
SCM_EXPORT ScmObj scm_p_string_fillx(ScmObj str, ScmObj ch);
#endif /* SCM_USE_STRING */

#if SCM_USE_VECTOR
/* vector.c */
SCM_EXPORT ScmObj scm_p_vectorp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_make_vector(ScmObj scm_len, ScmObj args);
SCM_EXPORT ScmObj scm_p_vector(ScmObj args);
SCM_EXPORT ScmObj scm_p_vector_length(ScmObj vec);
SCM_EXPORT ScmObj scm_p_vector_ref(ScmObj vec, ScmObj _k);
SCM_EXPORT ScmObj scm_p_vector_setx(ScmObj vec, ScmObj _k, ScmObj obj);
SCM_EXPORT ScmObj scm_p_vector2list(ScmObj vec);
SCM_EXPORT ScmObj scm_p_list2vector(ScmObj lst);
SCM_EXPORT ScmObj scm_p_vector_fillx(ScmObj vec, ScmObj fill);
#endif /* SCM_USE_VECTOR */

#if SCM_USE_DEEP_CADRS
/* deep-cadrs.c */
SCM_EXPORT ScmObj scm_p_caaar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_caadr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cadar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cdaar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cdadr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cddar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_caaaar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_caaadr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_caadar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_caaddr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cadaar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cadadr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_caddar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cadddr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cdaaar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cdaadr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cdadar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cdaddr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cddaar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cddadr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cdddar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_cddddr(ScmObj lst);
#endif /* SCM_USE_DEEP_CADRS */

/* macro.c */
#if SCM_USE_HYGIENIC_MACRO
SCM_EXPORT ScmObj scm_s_match(ScmObj form, ScmObj patterns,
                              ScmEvalState *state);
SCM_EXPORT ScmObj scm_s_syntax_rules(ScmObj rules, ScmObj env);
SCM_EXPORT ScmObj scm_s_expand_macro(ScmObj macro, ScmObj args,
                                     ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_unwrap_syntaxx(ScmObj obj);
SCM_EXPORT ScmObj scm_unwrap_keyword(ScmObj obj);
#endif

#if SCM_USE_PORT
/* port.c */
SCM_EXPORT ScmObj scm_make_shared_file_port(FILE *file, const char *aux_info,
                                            enum ScmPortFlag flag);
SCM_EXPORT int scm_port_close(ScmObj port);
SCM_EXPORT ScmCharCodec *scm_port_codec(ScmObj port);
SCM_EXPORT char *scm_port_inspect(ScmObj port);
SCM_EXPORT int scm_port_get_char(ScmObj port);
SCM_EXPORT int scm_port_peek_char(ScmObj port);
SCM_EXPORT scm_bool scm_port_char_readyp(ScmObj port);
SCM_EXPORT int scm_port_puts(ScmObj port, const char *str);
SCM_EXPORT int scm_port_put_char(ScmObj port, scm_ichar_t ch);
SCM_EXPORT int scm_port_newline(ScmObj port);
SCM_EXPORT int scm_port_flush(ScmObj port);
SCM_EXPORT ScmObj scm_p_call_with_input_file(ScmObj filepath, ScmObj proc);
SCM_EXPORT ScmObj scm_p_call_with_output_file(ScmObj filepath, ScmObj proc);
SCM_EXPORT ScmObj scm_p_input_portp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_output_portp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_current_input_port(void);
SCM_EXPORT ScmObj scm_p_current_output_port(void);
SCM_EXPORT ScmObj scm_p_with_input_from_file(ScmObj filepath, ScmObj thunk);
SCM_EXPORT ScmObj scm_p_with_output_to_file(ScmObj filepath, ScmObj thunk);
SCM_EXPORT ScmObj scm_p_open_input_file(ScmObj filepath);
SCM_EXPORT ScmObj scm_p_open_output_file(ScmObj filepath);
SCM_EXPORT ScmObj scm_p_close_input_port(ScmObj port);
SCM_EXPORT ScmObj scm_p_close_output_port(ScmObj port);
SCM_EXPORT ScmObj scm_p_read_char(ScmObj args);
SCM_EXPORT ScmObj scm_p_peek_char(ScmObj args);
SCM_EXPORT ScmObj scm_p_eof_objectp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_char_readyp(ScmObj args);
SCM_EXPORT ScmObj scm_p_newline(ScmObj args);
SCM_EXPORT ScmObj scm_p_write_char(ScmObj obj, ScmObj args);
#endif /* SCM_USE_PORT */

#if SCM_USE_READER
/* read.c */
SCM_EXPORT ScmObj scm_read(ScmObj port);
SCM_EXPORT ScmObj scm_read_char(ScmObj port);
SCM_EXPORT ScmObj scm_p_read(ScmObj args);
#endif /* SCM_USE_READER */

#if SCM_USE_WRITER
/* write.c */
SCM_EXPORT void scm_write(ScmObj port, ScmObj obj);
SCM_EXPORT void scm_display(ScmObj port, ScmObj obj);
#if SCM_USE_SRFI38
SCM_EXPORT void scm_write_ss(ScmObj port, ScmObj obj);
#endif /* SCM_USE_SRFI38 */
SCM_EXPORT ScmObj scm_p_write(ScmObj obj, ScmObj args);
SCM_EXPORT ScmObj scm_p_display(ScmObj obj, ScmObj args);
#endif /* SCM_USE_WRITER */

#if SCM_USE_LOAD
/* load.c */
SCM_EXPORT void scm_set_lib_path(const char *path);
SCM_EXPORT void scm_load(const char *filename);
SCM_EXPORT ScmObj scm_p_load(ScmObj filename);
#endif /* SCM_USE_LOAD */

#if SCM_USE_FORMAT
/* format.c */
SCM_EXPORT void scm_pretty_print(ScmObj port, ScmObj obj);
SCM_EXPORT ScmObj scm_lformat(ScmObj port, enum ScmFormatCapability fcap,
                              const char *fmt, ScmObj scm_args);
SCM_EXPORT ScmObj scm_vformat(ScmObj port, enum ScmFormatCapability fcap,
                              const char *fmt, va_list c_args);
SCM_EXPORT ScmObj scm_format(ScmObj port, enum ScmFormatCapability fcap,
                             const char *fmt, ...);
#endif /* SCM_USE_FORMAT */

/*===========================================================================
   SigScheme: Optional Funtions
===========================================================================*/
#if SCM_USE_SSCM_EXTENSIONS
/* module-sscm-ext.c */
SCM_EXPORT void scm_initialize_sscm_extensions(void);
SCM_EXPORT ScmObj scm_p_symbol_boundp(ScmObj sym, ScmObj rest);
SCM_EXPORT ScmObj scm_p_least_fixnum(void);
SCM_EXPORT ScmObj scm_p_greatest_fixnum(void);
SCM_EXPORT ScmObj scm_p_load_path(void);
SCM_EXPORT void scm_require(const char *filename);
SCM_EXPORT ScmObj scm_p_require(ScmObj filename);
SCM_EXPORT ScmObj scm_p_provide(ScmObj feature);
SCM_EXPORT ScmObj scm_p_providedp(ScmObj feature);
SCM_EXPORT ScmObj scm_p_lengthstar(ScmObj lst);
SCM_EXPORT ScmObj scm_p_exit(ScmObj args) SCM_NORETURN;
#endif /* SCM_USE_SSCM_EXTENSIONS */

#if SCM_COMPAT_SIOD
/* module-siod.c */
SCM_EXPORT void   scm_initialize_siod(void);
SCM_EXPORT ScmObj scm_p_symbol_value(ScmObj var);
SCM_EXPORT ScmObj scm_p_set_symbol_valuex(ScmObj var, ScmObj val);
SCM_EXPORT ScmObj scm_p_siod_equal(ScmObj obj1, ScmObj obj2);
SCM_EXPORT ScmObj scm_p_the_environment(ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_p_closure_code(ScmObj closure);
SCM_EXPORT ScmObj scm_p_verbose(ScmObj args);
SCM_EXPORT ScmObj scm_p_eof_val(void);
SCM_EXPORT ScmObj scm_s_undefine(ScmObj var, ScmObj env);
SCM_EXPORT long   scm_get_verbose_level(void);
SCM_EXPORT void   scm_set_verbose_level(long level);
#endif /* SCM_COMPAT_SIOD */

#if SCM_USE_SRFI1
/* module-srfi1.c */
SCM_EXPORT void   scm_initialize_srfi1(void);
SCM_EXPORT ScmObj scm_p_srfi1_xcons(ScmObj a, ScmObj b);
SCM_EXPORT ScmObj scm_p_srfi1_consstar(ScmObj args);
SCM_EXPORT ScmObj scm_p_srfi1_make_list(ScmObj length, ScmObj args);
SCM_EXPORT ScmObj scm_p_srfi1_list_tabulate(ScmObj _n, ScmObj args);
SCM_EXPORT ScmObj scm_p_srfi1_list_copy(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_circular_list(ScmObj args);
SCM_EXPORT ScmObj scm_p_srfi1_iota(ScmObj scm_count, ScmObj args);
SCM_EXPORT ScmObj scm_p_srfi1_proper_listp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_srfi1_circular_listp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_srfi1_dotted_listp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_srfi1_not_pairp(ScmObj obj);
SCM_EXPORT ScmObj scm_p_srfi1_null_listp(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_listequal(ScmObj eqproc, ScmObj args);
SCM_EXPORT ScmObj scm_p_srfi1_first(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_second(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_third(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_fourth(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_fifth(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_sixth(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_seventh(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_eighth(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_ninth(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_tenth(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_carpluscdr(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_take(ScmObj lst, ScmObj scm_idx);
SCM_EXPORT ScmObj scm_p_srfi1_drop(ScmObj lst, ScmObj scm_idx);
SCM_EXPORT ScmObj scm_p_srfi1_take_right(ScmObj lst, ScmObj scm_elem);
SCM_EXPORT ScmObj scm_p_srfi1_drop_right(ScmObj lst, ScmObj scm_elem);
SCM_EXPORT ScmObj scm_p_srfi1_takex(ScmObj lst, ScmObj scm_idx);
SCM_EXPORT ScmObj scm_p_srfi1_drop_rightx(ScmObj lst, ScmObj scm_idx);
SCM_EXPORT ScmObj scm_p_srfi1_split_at(ScmObj lst, ScmObj idx);
SCM_EXPORT ScmObj scm_p_srfi1_split_atx(ScmObj lst, ScmObj idx);
SCM_EXPORT ScmObj scm_p_srfi1_last(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_last_pair(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_lengthplus(ScmObj lst);
SCM_EXPORT ScmObj scm_p_srfi1_concatenate(ScmObj args);
#endif

#if SCM_USE_SRFI2
/* module-srfi2.c */
SCM_EXPORT void   scm_initialize_srfi2(void);
SCM_EXPORT ScmObj scm_s_srfi2_and_letstar(ScmObj claws, ScmObj body,
                                          ScmEvalState *eval_state);
#endif

#if SCM_USE_SRFI6
/* module-srfi6.c */
SCM_EXPORT void   scm_initialize_srfi6(void);
SCM_EXPORT ScmObj scm_p_srfi6_open_input_string(ScmObj str);
SCM_EXPORT ScmObj scm_p_srfi6_open_output_string(void);
SCM_EXPORT ScmObj scm_p_srfi6_get_output_string(ScmObj port);
#endif

#if SCM_USE_SRFI8
/* module-srfi8.c */
SCM_EXPORT void   scm_initialize_srfi8(void);
SCM_EXPORT ScmObj scm_s_srfi8_receive(ScmObj formals, ScmObj expr, ScmObj body,
                                      ScmEvalState *eval_state);
#endif

#if SCM_USE_SRFI23
/* module-srfi23.c */
SCM_EXPORT void   scm_initialize_srfi23(void);
SCM_EXPORT ScmObj scm_p_srfi23_error(ScmObj reason, ScmObj args);
#endif

#if SCM_USE_SRFI28
/* module-srfi28.c */
SCM_EXPORT void   scm_initialize_srfi28(void);
SCM_EXPORT ScmObj scm_p_srfi28_format(ScmObj fmt, ScmObj objs);
#endif

#if SCM_USE_SRFI34
/* module-srfi34.c */
SCM_EXPORT void   scm_initialize_srfi34(void);
SCM_EXPORT ScmObj scm_p_srfi34_with_exception_handler(ScmObj handler,
                                                      ScmObj thunk);
SCM_EXPORT ScmObj scm_s_srfi34_guard(ScmObj cond_catch, ScmObj body,
                                     ScmEvalState *eval_state);
SCM_EXPORT ScmObj scm_p_srfi34_raise(ScmObj obj);
#endif

#if SCM_USE_SRFI38
/* module-srfi38.c */
SCM_EXPORT void   scm_initialize_srfi38(void);
SCM_EXPORT ScmObj scm_p_srfi38_write_with_shared_structure(ScmObj obj,
                                                           ScmObj args);
#endif

#if SCM_USE_SRFI48
/* module-srfi48.c */
SCM_EXPORT void   scm_initialize_srfi48(void);
SCM_EXPORT ScmObj scm_p_srfi48_format(ScmObj fmt_or_port, ScmObj rest);
SCM_EXPORT ScmObj scm_p_formatplus(ScmObj fmt_or_port, ScmObj rest);
#endif

#if SCM_USE_SRFI60
/* module-srfi60.c */
SCM_EXPORT void   scm_initialize_srfi60(void);
SCM_EXPORT ScmObj scm_p_srfi60_logand(ScmObj left, ScmObj right,
                                      enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_srfi60_logior(ScmObj left, ScmObj right,
                                      enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_srfi60_logxor(ScmObj left, ScmObj right,
                                      enum ScmReductionState *state);
SCM_EXPORT ScmObj scm_p_srfi60_lognot(ScmObj n);
SCM_EXPORT ScmObj scm_p_srfi60_bitwise_if(ScmObj mask, ScmObj n0, ScmObj n1);
SCM_EXPORT ScmObj scm_p_srfi60_logtest(ScmObj j, ScmObj k);
#endif

#ifdef __cplusplus
}
#endif

#endif /* __SIGSCHEME_H */
