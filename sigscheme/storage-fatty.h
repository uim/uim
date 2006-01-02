/*===========================================================================
 *  FileName : storage-fatty.h
 *  About    : Storage abstraction (fatty representation)
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
#ifndef __STORAGE_FATTY_H
#define __STORAGE_FATTY_H

/*
 * Internal representation defined in this file MUST NOT directly touched by
 * libsscm users. Use abstract public APIs defined in sigscheme.h.
 */

/*=======================================
   System Include
=======================================*/
#include <stddef.h>

/*=======================================
   Local Include
=======================================*/

/*=======================================
   Type Declarations
=======================================*/
typedef struct ScmCell_ ScmCell;
typedef ScmCell *ScmObj;
typedef ScmObj *ScmRef;

typedef ScmObj (*ScmFuncType)();

/*=======================================
   Struct Declarations
=======================================*/
enum ScmStrMutability {
    SCM_STR_IMMUTABLE = 0,
    SCM_STR_MUTABLE   = 1
};

/* Scheme Object */
struct ScmCell_ {
    enum ScmObjType type;
    int gcmark;

    union {
        struct {
            int value;
        } int_value;

        struct {
            ScmObj car;
            ScmObj cdr;
        } cons;

        struct {
            char *sym_name;
            ScmObj v_cell;
        } symbol;

        struct {
            int value;
        } ch;

        struct {
            char *str;
            int len;
        } string;

        struct {
            enum ScmFuncTypeCode type;
            ScmFuncType func;
        } func;

        struct ScmClosure {
            ScmObj exp;
            ScmObj env;
        } closure;

        struct ScmVector {
            ScmObj *vec;
            int len;
        } vector;

        struct ScmPort {
            enum ScmPortFlag flag;
            ScmCharPort *impl;
        } port;

        struct ScmContinuation {
            void *opaque;
            int tag;
        } continuation;

#if !SCM_USE_VALUECONS
        struct ScmValuePacket {
            ScmObj values;
        } value_pack;
#endif

        struct ScmCPointer {
            void *data;
        } c_pointer;

        struct ScmCFuncPointer {
            ScmCFunc func;
        } c_func_pointer;
    } obj;
};

/*=======================================
  Object Creators
=======================================*/
#define SCM_SAL_MAKE_INT                      scm_make_int
#define SCM_SAL_MAKE_CONS                     scm_make_cons
#define SCM_SAL_MAKE_SYMBOL                   scm_make_symbol
#define SCM_SAL_MAKE_CHAR                     scm_make_char
#define SCM_SAL_MAKE_STRING                   scm_make_string
#define SCM_SAL_MAKE_STRING_COPYING           scm_make_string_copying
#define SCM_SAL_MAKE_IMMUTABLE_STRING         scm_make_immutable_string
#define SCM_SAL_MAKE_IMMUTABLE_STRING_COPYING scm_make_immutable_string_copying
#define SCM_SAL_MAKE_FUNC                     scm_make_func
#define SCM_SAL_MAKE_CLOSURE                  scm_make_closure
#define SCM_SAL_MAKE_VECTOR                   scm_make_vector
#define SCM_SAL_MAKE_PORT                     scm_make_port
#define SCM_SAL_MAKE_CONTINUATION             scm_make_continuation
#if SCM_USE_NONSTD_FEATURES
#define SCM_SAL_MAKE_C_POINTER                scm_make_cpointer
#define SCM_SAL_MAKE_C_FUNCPOINTER            scm_make_cfunc_pointer
#endif /* SCM_USE_NONSTD_FEATURES */
#if SCM_USE_VALUECONS
#define SCM_SAL_MAKE_VALUEPACKET(vals) (NULLP(vals) ? scm_null_values :       \
                                    (SCM_ENTYPE_VALUEPACKET(vals), (vals)))
#else /* SCM_USE_VALUECONS */
#define SCM_SAL_MAKE_VALUEPACKET(vals) (scm_make_value_packet(vals))
#endif /* SCM_USE_VALUECONS */

/* Don't use these functions directly. Use SCM_MAKE_*() or MAKE_*() instead to
 * allow flexible object allocation. */
ScmObj scm_make_cons(ScmObj kar, ScmObj kdr);
ScmObj scm_make_int(int val);
ScmObj scm_make_symbol(char *name, ScmObj val);
ScmObj scm_make_char(int val);
ScmObj scm_make_immutable_string(char *str);
ScmObj scm_make_immutable_string_copying(const char *str);
ScmObj scm_make_string(char *str);
ScmObj scm_make_string_copying(const char *str);
ScmObj scm_make_func(enum ScmFuncTypeCode type, ScmFuncType func);
ScmObj scm_make_closure(ScmObj exp, ScmObj env);
ScmObj scm_make_vector(ScmObj *vec, int len);
ScmObj scm_make_port(ScmCharPort *cport, enum ScmPortFlag flag);
ScmObj scm_make_continuation(void);
#if !SCM_USE_VALUECONS
ScmObj scm_make_value_packet(ScmObj values);
#endif
#if SCM_USE_NONSTD_FEATURES
ScmObj scm_make_cpointer(void *ptr);
ScmObj scm_make_cfunc_pointer(ScmCFunc ptr);
#endif

/*=======================================
   Accessors For Scheme Objects
=======================================*/
/* ScmObj Global Attribute */
#define SCM_SAL_TYPE(o) ((o)->type)
#define SCM_ENTYPE(a, objtype) ((a)->type = (objtype))

/* Real Accessors */
#define SCM_SAL_INTP(a)  (SCM_TYPE(a) == ScmInt)
#define SCM_SAL_ENTYPE_INT(a)    (SCM_ENTYPE((a), ScmInt))
#define SCM_SAL_INT_VALUE(a) (SCM_AS_INT(a)->obj.int_value.value)
#define SCM_SAL_INT_SET_VALUE(a, val) (SCM_INT_VALUE(a) = (val))

#define SCM_SAL_CONSP(a) (SCM_TYPE(a) == ScmCons)
#define SCM_SAL_ENTYPE_CONS(a) (SCM_ENTYPE((a), ScmCons))
#if SCM_DEBUG
/* don't use as lvalue */
#define SCM_SAL_CONS_CAR(a)               (SCM_AS_CONS(a)->obj.cons.car + 0)
#define SCM_SAL_CONS_CDR(a)               (SCM_AS_CONS(a)->obj.cons.cdr + 0)
#else /* SCM_DEBUG */
#define SCM_SAL_CONS_CAR(a)               (SCM_AS_CONS(a)->obj.cons.car)
#define SCM_SAL_CONS_CDR(a)               (SCM_AS_CONS(a)->obj.cons.cdr)
#endif /* SCM_DEBUG */
#define SCM_SAL_CONS_SET_CAR(a, kar) (SCM_AS_CONS(a)->obj.cons.car = (kar))
#define SCM_SAL_CONS_SET_CDR(a, kdr) (SCM_AS_CONS(a)->obj.cons.cdr = (kdr))

#define SCM_SAL_SYMBOLP(a)      (SCM_TYPE(a) == ScmSymbol)
#define SCM_SAL_ENTYPE_SYMBOL(a)    (SCM_ENTYPE((a), ScmSymbol))
#define SCM_SAL_SYMBOL_NAME(a)  (SCM_AS_SYMBOL(a)->obj.symbol.sym_name)
#define SCM_SAL_SYMBOL_SET_NAME(a, name)   (SCM_SYMBOL_NAME(a) = (name))
#define SCM_SAL_SYMBOL_VCELL(a) (SCM_AS_SYMBOL(a)->obj.symbol.v_cell)
#define SCM_SAL_SYMBOL_SET_VCELL(a, vcell) (SCM_SYMBOL_VCELL(a) = (vcell))

#define SCM_SAL_CHARP(a) (SCM_TYPE(a) == ScmChar)
#define SCM_SAL_ENTYPE_CHAR(a) (SCM_ENTYPE((a), ScmChar))
#define SCM_SAL_CHAR_VALUE(a) (SCM_AS_CHAR(a)->obj.ch.value)
#define SCM_SAL_CHAR_SET_VALUE(a, val) (SCM_CHAR_VALUE(a) = (val))

/* String object uses a tagged pointer to multiplex its mutability.
 * LSB of ScmCell.obj.string.str represents the value. */
#define SCM_STRING_MUTABILITY_MASK     0x1UL
#define SCM_STRING_STR_VALUE_MASK      ~SCM_STRING_MUTABILITY_MASK
#define SCM_SAL_STRINGP(o)             (SCM_TYPE(o) == ScmString)
#define SCM_SAL_ENTYPE_STRING(o)       (SCM_ENTYPE((o), ScmString))
#define SCM_SAL_STRING_STR(o)                                                \
    ((char *)((unsigned long)SCM_AS_STRING(o)->obj.string.str                \
              & SCM_STRING_STR_VALUE_MASK))
#define SCM_SAL_STRING_SET_STR(o, val)                                       \
    (SCM_AS_STRING(o)->obj.string.str =                                      \
     (char *)((unsigned long)(val) | (unsigned)SCM_STRING_MUTABILITY(o)))
#define SCM_SAL_STRING_LEN(o)          (SCM_AS_STRING(o)->obj.string.len)
#define SCM_SAL_STRING_SET_LEN(o, len) (SCM_STRING_LEN(o) = (len))
#define SCM_STRING_MUTABILITY(o)                                             \
  ((enum ScmStrMutability)((unsigned long)SCM_AS_STRING(o)->obj.string.str   \
                           & SCM_STRING_MUTABILITY_MASK))
#define SCM_SAL_STRING_MUTABLEP(o)                                           \
    (SCM_STRING_MUTABILITY(o) == SCM_STR_MUTABLE)
#define SCM_SAL_STRING_SET_MUTABLE(o)                                        \
    (SCM_AS_STRING(o)->obj.string.str =                                      \
     (char *)((unsigned long)SCM_AS_STRING(o)->obj.string.str                \
              | SCM_STR_MUTABLE))
#define SCM_SAL_STRING_SET_IMMUTABLE(o)                                      \
    (SCM_AS_STRING(o)->obj.string.str =                                      \
     (char *)((unsigned long)SCM_AS_STRING(o)->obj.string.str                \
              | SCM_STR_IMMUTABLE))

#define SCM_SAL_FUNCP(a) (SCM_TYPE(a) == ScmFunc)
#define SCM_SAL_ENTYPE_FUNC(a)     (SCM_ENTYPE((a), ScmFunc))
#define SCM_SAL_FUNC_TYPECODE(a) (SCM_AS_FUNC(a)->obj.func.type)
#define SCM_SAL_FUNC_SET_TYPECODE(a, type) (SCM_FUNC_TYPECODE(a) = (type))
#define SCM_SAL_FUNC_CFUNC(a)   (SCM_AS_FUNC(a)->obj.func.func)
#define SCM_SAL_FUNC_SET_CFUNC(a, func)     (SCM_FUNC_CFUNC(a) = (ScmFuncType)(func))

#define SCM_SAL_CLOSUREP(a) (SCM_TYPE(a) == ScmClosure)
#define SCM_SAL_ENTYPE_CLOSURE(a) (SCM_ENTYPE((a), ScmClosure))
#define SCM_SAL_CLOSURE_EXP(a) (SCM_AS_CLOSURE(a)->obj.closure.exp)
#define SCM_SAL_CLOSURE_SET_EXP(a, exp) (SCM_CLOSURE_EXP(a) = (exp))
#define SCM_SAL_CLOSURE_ENV(a) (SCM_AS_CLOSURE(a)->obj.closure.env)
#define SCM_SAL_CLOSURE_SET_ENV(a, env) (SCM_CLOSURE_ENV(a) = (env))

#define SCM_SAL_VECTORP(a) (SCM_TYPE(a) == ScmVector)
#define SCM_SAL_ENTYPE_VECTOR(a) (SCM_ENTYPE((a), ScmVector))
#define SCM_SAL_VECTOR_VEC(a) (SCM_AS_VECTOR(a)->obj.vector.vec)
#define SCM_SAL_VECTOR_SET_VEC(a, vec) (SCM_VECTOR_VEC(a) = (vec))
#define SCM_SAL_VECTOR_LEN(a) (SCM_AS_VECTOR(a)->obj.vector.len)
#define SCM_SAL_VECTOR_SET_LEN(a, len) (SCM_VECTOR_LEN(a) = (len))
#define SCM_SAL_VECTOR_VALID_INDEXP(o, i) (0 <= (i) && (i) < SCM_VECTOR_LEN(o))

#define SCM_SAL_PORTP(a) (SCM_TYPE(a) == ScmPort)
#define SCM_SAL_ENTYPE_PORT(a) (SCM_ENTYPE((a), ScmPort))
#define SCM_SAL_PORT_FLAG(a)           (SCM_AS_PORT(a)->obj.port.flag)
#define SCM_SAL_PORT_SET_FLAG(a, flag) (SCM_PORT_FLAG(a) = (flag))
#define SCM_SAL_PORT_IMPL(a)           (SCM_AS_PORT(a)->obj.port.impl)
#define SCM_SAL_PORT_SET_IMPL(a, impl) (SCM_PORT_IMPL(a) = (impl))

#define SCM_SAL_CONTINUATIONP(a) (SCM_TYPE(a) == ScmContinuation)
#define SCM_SAL_ENTYPE_CONTINUATION(a) (SCM_ENTYPE((a), ScmContinuation))
#define SCM_SAL_CONTINUATION_OPAQUE(a) (SCM_AS_CONTINUATION(a)->obj.continuation.opaque)
#define SCM_SAL_CONTINUATION_SET_OPAQUE(a, val) (SCM_CONTINUATION_OPAQUE(a) = (val))
#define SCM_SAL_CONTINUATION_TAG(a) (SCM_AS_CONTINUATION(a)->obj.continuation.tag)
#define SCM_SAL_CONTINUATION_SET_TAG(a, val) (SCM_CONTINUATION_TAG(a) = (val))

#if SCM_USE_VALUECONS
/* to modify a VALUECONS, rewrite its type to cons by SCM_ENTYPE_CONS(vcons) */
#define SCM_SAL_VALUEPACKETP(a)       (SCM_TYPE(a) == ScmValuePacket)
#define SCM_SAL_NULLVALUESP(a)        (EQ((a), scm_null_values))
#define SCM_SAL_ENTYPE_VALUEPACKET(a) (SCM_ENTYPE((a), ScmValuePacket))
#define SCM_SAL_VALUEPACKET_VALUES(a) ((SCM_NULLVALUESP(a)) ? SCM_NULL :         \
                                   (SCM_ENTYPE_CONS(a), (a)))
#define SCM_SAL_VALUECONS_CAR(a)      (SCM_AS_VALUEPACKET(a)->obj.cons.car)
#define SCM_SAL_VALUECONS_CDR(a)      (SCM_AS_VALUEPACKET(a)->obj.cons.cdr)
#else /* SCM_USE_VALUECONS */
#define SCM_SAL_VALUEPACKETP(a)          (SCM_TYPE(a) == ScmValuePacket)
#define SCM_SAL_ENTYPE_VALUEPACKET(a)        (SCM_ENTYPE((a), ScmValuePacket))
#define SCM_SAL_VALUEPACKET_VALUES(a)    (SCM_AS_VALUEPACKET(a)->obj.value_pack.values)
#define SCM_SAL_VALUEPACKET_SET_VALUES(a, v) (SCM_VALUEPACKET_VALUES(a) = (v))
#endif /* SCM_USE_VALUECONS */

/*============================================================================
  Special Constants (such as SCM_NULL)
============================================================================*/
#define SCM_SAL_CONSTANTP(a) (SCM_TYPE(a) == ScmConstant)

/*============================================================================
  C Pointer Object
============================================================================*/
#define SCM_SAL_C_POINTERP(a) (SCM_TYPE(a) == ScmCPointer)
#define SCM_SAL_ENTYPE_C_POINTER(a) (SCM_ENTYPE((a), ScmCPointer))
#define SCM_SAL_C_POINTER_VALUE(a) (SCM_AS_C_POINTER(a)->obj.c_pointer.data)
#define SCM_SAL_C_POINTER_SET_VALUE(a, ptr) (SCM_C_POINTER_VALUE(a) = (ptr))

#define SCM_SAL_C_FUNCPOINTERP(a) (SCM_TYPE(a) == ScmCFuncPointer)
#define SCM_SAL_ENTYPE_C_FUNCPOINTER(a) (SCM_ENTYPE((a), ScmCFuncPointer))
#define SCM_SAL_C_FUNCPOINTER_VALUE(a) (SCM_AS_C_FUNCPOINTER(a)->obj.c_func_pointer.func)
#define SCM_SAL_C_FUNCPOINTER_SET_VALUE(a, funcptr) (SCM_C_FUNCPOINTER_VALUE(a) = (funcptr))

/*============================================================================
  GC Related Operations
============================================================================*/
#define SCM_SAL_FREECELLP(a)            (SCM_TYPE(a) == ScmFreeCell)
#define SCM_SAL_AS_FREECELL(a)          (SCM_ASSERT_TYPE(SCM_FREECELLP(a), (a)))
#define SCM_SAL_FREECELL_NEXT(a)         (SCM_AS_FREECELL(a)->obj.cons.car)
#define SCM_SAL_FREECELL_FREESLOT(a)         (SCM_AS_FREECELL(a)->obj.cons.cdr)
#define SCM_SAL_ENTYPE_FREECELL(a)      (SCM_ENTYPE((a), ScmFreeCell))
#define SCM_SAL_FREECELL_SET_NEXT(a, next) (SCM_FREECELL_NEXT(a) = (next))
#define SCM_SAL_FREECELL_SET_FREESLOT(a, v) (SCM_FREECELL_FREESLOT(a) = (v))
#define SCM_SAL_FREECELL_CLEAR_FREESLOT(o)                                   \
    SCM_SAL_FREECELL_SET_FREESLOT((o), SCM_FALSE)

#define SCM_SAL_RECLAIM_CELL(cell, next)                                     \
    do {                                                                     \
        SCM_ENTYPE_FREECELL(cell);                                           \
        SCM_DO_UNMARK(cell);                                                 \
        SCM_FREECELL_SET_NEXT((cell), (next));                               \
        SCM_FREECELL_CLEAR_FREESLOT(cell);                                   \
    } while (/* CONSTCOND */ 0)

#define SCM_SAL_IS_MARKED(o)   ((o)->gcmark)
#define SCM_SAL_IS_UNMARKED(o) (!SCM_IS_MARKED(o))
#define SCM_SAL_DO_MARK(o)     ((o)->gcmark = 1)
#define SCM_SAL_DO_UNMARK(o)   ((o)->gcmark = 0)

/*============================================================================
  Environment Specifiers
============================================================================*/
#define SCM_SAL_INTERACTION_ENV SCM_NULL
/*
 * Current implementation cannot handle scheme-report-environment and
 * null-environment properly. Be careful to use these environemnts.
 */
#define SCM_SAL_R5RS_ENV        SCM_INTERACTION_ENV
#define SCM_SAL_NULL_ENV        SCM_INTERACTION_ENV

#define SCM_SAL_ENVP(env) (NULLP(env) || CONSP(env))

/*============================================================================
  Abstract ScmObj Reference For Storage-Representation Independent Efficient
  List Operations
============================================================================*/
#define SCM_SAL_INVALID_REF   NULL

#define SCM_SAL_REF_CAR(kons)     (&SCM_AS_CONS(kons)->obj.cons.car)
#define SCM_SAL_REF_CDR(kons)     (&SCM_AS_CONS(kons)->obj.cons.cdr)
#define SCM_SAL_REF_OFF_HEAP(obj) (&(obj))

/* SCM_DEREF(ref) is not permitted to be used as lvalue */
#if SCM_DEBUG
#define SCM_SAL_DEREF(ref)    (*(ref) + 0)
#else /* SCM_DEBUG */
#define SCM_SAL_DEREF(ref)    (*(ref))
#endif /* SCM_DEBUG */

/* RFC: Is there a better name? */
#define SCM_SAL_SET(ref, obj) (*(ref) = (obj))

/*============================================================================
  Special Constants and Predicates
============================================================================*/
#define SCM_SAL_INVALID          NULL
#define SCM_SAL_NULL             scm_null
#define SCM_SAL_TRUE             scm_true
#define SCM_SAL_FALSE            scm_false
#define SCM_SAL_EOF              scm_eof
#define SCM_SAL_UNBOUND          scm_unbound
#define SCM_SAL_UNDEF            scm_undef

#define SCM_SAL_EQ(a, b)   ((a) == (b))

/* storage.c */
extern ScmObj scm_null, scm_true, scm_false, scm_eof;
extern ScmObj scm_unbound, scm_undef;

/*============================================================================
  Predefined Symbols
============================================================================*/
/* for list construction */
#define SCM_SAL_SYM_QUOTE            scm_sym_quote
#define SCM_SAL_SYM_QUASIQUOTE       scm_sym_quasiquote
#define SCM_SAL_SYM_UNQUOTE          scm_sym_unquote
#define SCM_SAL_SYM_UNQUOTE_SPLICING scm_sym_unquote_splicing

/* sigscheme.c */
extern ScmObj scm_sym_quote, scm_sym_quasiquote;
extern ScmObj scm_sym_unquote, scm_sym_unquote_splicing;

#endif /* __STORAGE_FATTY_H */
