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
#include <stdint.h> /* FIXME: make C99-independent */

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
enum ScmMutability {
    SCM_IMMUTABLE = 0,
    SCM_MUTABLE   = 1
};

/* Scheme Object */
struct ScmCell_ {
    enum ScmObjType type;
    int gcmark;

    union {
        struct {
            int value;
        } integer;

        struct {
            ScmObj car;
            ScmObj cdr;
        } cons;

        struct {
            char *name;
            ScmObj value;
        } symbol;

        struct {
            int value;
        } character;

        struct {
            char *str;
            int len;  /* number of (multibyte) chars */
        } string;

        struct {
            enum ScmFuncTypeCode type;
            ScmFuncType ptr;
        } function;

        struct {
            ScmObj exp;
            ScmObj env;
        } closure;

        struct {
            ScmObj *vec;
            int len;
        } vector;

        struct {
            enum ScmPortFlag flag;
            ScmCharPort *impl;
        } port;

        struct {
            void *opaque;
            int tag;
        } continuation;

#if !SCM_USE_VALUECONS
        struct {
            ScmObj lst;
        } value_packet;
#endif

        struct {
            void *value;
        } c_pointer;

        struct {
            ScmCFunc value;
        } c_func_pointer;
    } obj;
};

/* object representation information for optimization */
#define SCM_SAL_HAS_IMMEDIATE_INT  0
#define SCM_SAL_HAS_IMMEDIATE_CHAR 0

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
#define SCM_SAL_MAKE_VALUEPACKET(vals)                                       \
    (NULLP(vals) ? scm_null_values : (SCM_ENTYPE_VALUEPACKET(vals), (vals)))
#else /* SCM_USE_VALUECONS */
#define SCM_SAL_MAKE_VALUEPACKET(vals) (scm_make_value_packet(vals))
#endif /* SCM_USE_VALUECONS */

/* Don't use these functions directly. Use SCM_MAKE_*() or MAKE_*() instead to
 * allow flexible object allocation. */
ScmObj scm_make_cons(ScmObj kar, ScmObj kdr);
ScmObj scm_make_int(int val);
ScmObj scm_make_symbol(char *name, ScmObj val);
ScmObj scm_make_char(int val);
ScmObj scm_make_immutable_string(char *str, int len);
ScmObj scm_make_immutable_string_copying(const char *str, int len);
ScmObj scm_make_string(char *str, int len);
ScmObj scm_make_string_copying(const char *str, int len);
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
#define SCM_SAL_TYPE(o)        ((o)->type)
#define SCM_ENTYPE(o, objtype) ((o)->type = (objtype))

/* Real Accessors */
#define SCM_SAL_INTP(o)                (SCM_TYPE(o) == ScmInt)
#define SCM_SAL_ENTYPE_INT(o)          (SCM_ENTYPE((o), ScmInt))
#define SCM_SAL_INT_VALUE(o)           (SCM_AS_INT(o)->obj.integer.value)
#define SCM_SAL_INT_SET_VALUE(o, val)  (SCM_INT_VALUE(o) = (val))

#define SCM_SAL_CONSP(o)               (SCM_TYPE(o) == ScmCons)
#define SCM_SAL_ENTYPE_CONS(o)         (SCM_ENTYPE((o), ScmCons))
#if SCM_DEBUG
/* don't use as lvalue */
#define SCM_SAL_CONS_CAR(o)            (SCM_AS_CONS(o)->obj.cons.car + 0)
#define SCM_SAL_CONS_CDR(o)            (SCM_AS_CONS(o)->obj.cons.cdr + 0)
#else /* SCM_DEBUG */
#define SCM_SAL_CONS_CAR(o)            (SCM_AS_CONS(o)->obj.cons.car)
#define SCM_SAL_CONS_CDR(o)            (SCM_AS_CONS(o)->obj.cons.cdr)
#endif /* SCM_DEBUG */
#define SCM_SAL_CONS_SET_CAR(o, kar)   (SCM_AS_CONS(o)->obj.cons.car = (kar))
#define SCM_SAL_CONS_SET_CDR(o, kdr)   (SCM_AS_CONS(o)->obj.cons.cdr = (kdr))

#define SCM_SAL_SYMBOLP(o)             (SCM_TYPE(o) == ScmSymbol)
#define SCM_SAL_ENTYPE_SYMBOL(o)       (SCM_ENTYPE((o), ScmSymbol))
#define SCM_SAL_SYMBOL_NAME(o)         (SCM_AS_SYMBOL(o)->obj.symbol.name)
#define SCM_SAL_SYMBOL_SET_NAME(o, _name)  (SCM_SYMBOL_NAME(o) = (_name))
#define SCM_SAL_SYMBOL_VCELL(o)        (SCM_AS_SYMBOL(o)->obj.symbol.value)
#define SCM_SAL_SYMBOL_SET_VCELL(o, vcell) (SCM_SYMBOL_VCELL(o) = (vcell))

#define SCM_SAL_CHARP(o)               (SCM_TYPE(o) == ScmChar)
#define SCM_SAL_ENTYPE_CHAR(o)         (SCM_ENTYPE((o), ScmChar))
#define SCM_SAL_CHAR_VALUE(o)          (SCM_AS_CHAR(o)->obj.character.value)
#define SCM_SAL_CHAR_SET_VALUE(o, val) (SCM_CHAR_VALUE(o) = (val))

/* String object uses a tagged pointer to multiplex its mutability.
 * LSB of ScmCell.obj.string.str represents the value. */
#define SCM_STRING_MUTABILITY_MASK     0x1UL
#define SCM_STRING_STR_VALUE_MASK      ~SCM_STRING_MUTABILITY_MASK
#define SCM_SAL_STRINGP(o)             (SCM_TYPE(o) == ScmString)
#define SCM_SAL_ENTYPE_STRING(o)       (SCM_ENTYPE((o), ScmString))
#define SCM_SAL_STRING_STR(o)                                                \
    ((char *)((uintptr_t)SCM_AS_STRING(o)->obj.string.str                    \
              & SCM_STRING_STR_VALUE_MASK))
#define SCM_SAL_STRING_SET_STR(o, val)                                       \
    (SCM_AS_STRING(o)->obj.string.str =                                      \
     (char *)((uintptr_t)(val) | (unsigned)SCM_STRING_MUTABILITY(o)))
#define SCM_SAL_STRING_LEN(o)          (SCM_AS_STRING(o)->obj.string.len)
#define SCM_SAL_STRING_SET_LEN(o, len) (SCM_STRING_LEN(o) = (len))
#define SCM_STRING_MUTABILITY(o)                                             \
  ((enum ScmMutability)((uintptr_t)SCM_AS_STRING(o)->obj.string.str          \
                        & SCM_STRING_MUTABILITY_MASK))
#define SCM_SAL_STRING_MUTABLEP(o)                                           \
    (SCM_STRING_MUTABILITY(o) == SCM_MUTABLE)
#define SCM_SAL_STRING_SET_MUTABLE(o)                                        \
    (SCM_AS_STRING(o)->obj.string.str =                                      \
     (char *)((uintptr_t)SCM_AS_STRING(o)->obj.string.str | SCM_MUTABLE))
#define SCM_SAL_STRING_SET_IMMUTABLE(o)                                      \
    (SCM_AS_STRING(o)->obj.string.str =                                      \
     (char *)((uintptr_t)SCM_AS_STRING(o)->obj.string.str | SCM_IMMUTABLE))

#define SCM_SAL_FUNCP(o)                   (SCM_TYPE(o) == ScmFunc)
#define SCM_SAL_ENTYPE_FUNC(o)             (SCM_ENTYPE((o), ScmFunc))
#define SCM_SAL_FUNC_TYPECODE(o)           (SCM_AS_FUNC(o)->obj.function.type)
#define SCM_SAL_FUNC_SET_TYPECODE(o, type) (SCM_FUNC_TYPECODE(o) = (type))
#define SCM_SAL_FUNC_CFUNC(o)              (SCM_AS_FUNC(o)->obj.function.ptr)
#define SCM_SAL_FUNC_SET_CFUNC(o, func)                                      \
    (SCM_FUNC_CFUNC(o) = (ScmFuncType)(func))

#define SCM_SAL_CLOSUREP(o)               (SCM_TYPE(o) == ScmClosure)
#define SCM_SAL_ENTYPE_CLOSURE(o)         (SCM_ENTYPE((o), ScmClosure))
#define SCM_SAL_CLOSURE_EXP(o)            (SCM_AS_CLOSURE(o)->obj.closure.exp)
#define SCM_SAL_CLOSURE_SET_EXP(o, exp)   (SCM_CLOSURE_EXP(o) = (exp))
#define SCM_SAL_CLOSURE_ENV(o)            (SCM_AS_CLOSURE(o)->obj.closure.env)
#define SCM_SAL_CLOSURE_SET_ENV(o, env)   (SCM_CLOSURE_ENV(o) = (env))

#define SCM_SAL_VECTORP(o)                (SCM_TYPE(o) == ScmVector)
#define SCM_SAL_ENTYPE_VECTOR(o)          (SCM_ENTYPE((o), ScmVector))
#define SCM_SAL_VECTOR_VEC(o)             (SCM_AS_VECTOR(o)->obj.vector.vec)
#define SCM_SAL_VECTOR_SET_VEC(o, vec)    (SCM_VECTOR_VEC(o) = (vec))
#define SCM_SAL_VECTOR_LEN(o)             (SCM_AS_VECTOR(o)->obj.vector.len)
#define SCM_SAL_VECTOR_SET_LEN(o, len)    (SCM_VECTOR_LEN(o) = (len))
#define SCM_SAL_VECTOR_VALID_INDEXP(o, i) (0 <= (i) && (i) < SCM_VECTOR_LEN(o))

#define SCM_SAL_PORTP(o)               (SCM_TYPE(o) == ScmPort)
#define SCM_SAL_ENTYPE_PORT(o)         (SCM_ENTYPE((o), ScmPort))
#define SCM_SAL_PORT_FLAG(o)           (SCM_AS_PORT(o)->obj.port.flag)
#define SCM_SAL_PORT_SET_FLAG(o, flag) (SCM_PORT_FLAG(o) = (flag))
#define SCM_SAL_PORT_IMPL(o)           (SCM_AS_PORT(o)->obj.port.impl)
#define SCM_SAL_PORT_SET_IMPL(o, impl) (SCM_PORT_IMPL(o) = (impl))

#define SCM_SAL_CONTINUATIONP(o)       (SCM_TYPE(o) == ScmContinuation)
#define SCM_SAL_ENTYPE_CONTINUATION(o) (SCM_ENTYPE((o), ScmContinuation))
#define SCM_SAL_CONTINUATION_OPAQUE(o)                                       \
    (SCM_AS_CONTINUATION(o)->obj.continuation.opaque)
#define SCM_SAL_CONTINUATION_SET_OPAQUE(o, val)                              \
    (SCM_CONTINUATION_OPAQUE(o) = (val))
#define SCM_SAL_CONTINUATION_TAG(o)                                          \
    (SCM_AS_CONTINUATION(o)->obj.continuation.tag)
#define SCM_SAL_CONTINUATION_SET_TAG(o, val)                                 \
    (SCM_CONTINUATION_TAG(o) = (val))

#if SCM_USE_VALUECONS
/* to modify a VALUECONS, rewrite its type to cons by SCM_ENTYPE_CONS(vcons) */
#define SCM_SAL_VALUEPACKETP(o)        (SCM_TYPE(o) == ScmValuePacket)
#define SCM_SAL_NULLVALUESP(o)         (EQ((o), scm_null_values))
#define SCM_SAL_ENTYPE_VALUEPACKET(o)  (SCM_ENTYPE((o), ScmValuePacket))
#define SCM_SAL_VALUEPACKET_VALUES(o)                                        \
    ((SCM_NULLVALUESP(o)) ? SCM_NULL : (SCM_ENTYPE_CONS(o), (o)))
#define SCM_SAL_VALUECONS_CAR(o)       (SCM_AS_VALUEPACKET(o)->obj.cons.car)
#define SCM_SAL_VALUECONS_CDR(o)       (SCM_AS_VALUEPACKET(o)->obj.cons.cdr)
#else /* SCM_USE_VALUECONS */
#define SCM_SAL_VALUEPACKETP(o)        (SCM_TYPE(o) == ScmValuePacket)
#define SCM_SAL_ENTYPE_VALUEPACKET(o)  (SCM_ENTYPE((o), ScmValuePacket))
#define SCM_SAL_VALUEPACKET_VALUES(o)                                        \
    (SCM_AS_VALUEPACKET(o)->obj.value_packet.lst)
#define SCM_SAL_VALUEPACKET_SET_VALUES(o, v) (SCM_VALUEPACKET_VALUES(o) = (v))
#endif /* SCM_USE_VALUECONS */

/*============================================================================
  Special Constants (such as SCM_NULL)
============================================================================*/
#define SCM_SAL_CONSTANTP(o)           (SCM_TYPE(o) == ScmConstant)

/*============================================================================
  C Pointer Object
============================================================================*/
#define SCM_SAL_C_POINTERP(o)           (SCM_TYPE(o) == ScmCPointer)
#define SCM_SAL_ENTYPE_C_POINTER(o)     (SCM_ENTYPE((o), ScmCPointer))
#define SCM_SAL_C_POINTER_VALUE(o)                                           \
    (SCM_AS_C_POINTER(o)->obj.c_pointer.value)
#define SCM_SAL_C_POINTER_SET_VALUE(o, ptr)                                  \
    (SCM_C_POINTER_VALUE(o) = (ptr))
#define SCM_SAL_C_FUNCPOINTERP(o)       (SCM_TYPE(o) == ScmCFuncPointer)
#define SCM_SAL_ENTYPE_C_FUNCPOINTER(o) (SCM_ENTYPE((o), ScmCFuncPointer))
#define SCM_SAL_C_FUNCPOINTER_VALUE(o)                                       \
    (SCM_AS_C_FUNCPOINTER(o)->obj.c_func_pointer.value)
#define SCM_SAL_C_FUNCPOINTER_SET_VALUE(o, ptr)                              \
    (SCM_C_FUNCPOINTER_VALUE(o) = (ptr))

/*============================================================================
  GC Related Operations
============================================================================*/
#define SCM_SAL_FREECELLP(o)           (SCM_TYPE(o) == ScmFreeCell)
#define SCM_SAL_AS_FREECELL(o)         (SCM_ASSERT_TYPE(SCM_FREECELLP(o), (o)))
#define SCM_SAL_FREECELL_NEXT(o)       (SCM_AS_FREECELL(o)->obj.cons.car)
#define SCM_SAL_FREECELL_FREESLOT(o)   (SCM_AS_FREECELL(o)->obj.cons.cdr)
#define SCM_SAL_ENTYPE_FREECELL(o)     (SCM_ENTYPE((o), ScmFreeCell))
#define SCM_SAL_FREECELL_SET_NEXT(o, next)  (SCM_FREECELL_NEXT(o) = (next))
#define SCM_SAL_FREECELL_SET_FREESLOT(o, v) (SCM_FREECELL_FREESLOT(o) = (v))
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
#define SCM_SAL_INVALID  NULL
#define SCM_SAL_NULL     scm_const_null
#define SCM_SAL_TRUE     scm_const_true
#define SCM_SAL_FALSE    scm_const_false
#define SCM_SAL_EOF      scm_const_eof
#define SCM_SAL_UNBOUND  scm_const_unbound
#define SCM_SAL_UNDEF    scm_const_undef

#define SCM_SAL_EQ(a, b) ((a) == (b))

/* storage.c */
extern ScmObj scm_const_null, scm_const_true, scm_const_false, scm_const_eof;
extern ScmObj scm_const_unbound, scm_const_undef;

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
