/*===========================================================================
 *  FileName : sigschemetype-compact.h
 *  About    : compacted scheme object type definition
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
#ifndef __SIGSCMTYPE_COMPACT_H
#define __SIGSCMTYPE_COMPACT_H

/*
 * Object Representation Mechanism
 *
 * First, we assume ScmObj "S" which contains two ScmObj "X" and
 * "Y" (e.g. ScmObj S { X, Y }).
 *
 * (0) LSB(Least Significant Bit) of "S" is called G-bit.
 *
 * (1) if S == "...00G", S is ConsCell. G-bit of S->X is used as
 *     marking bit of GC and G-bit of S-Y is always 0 for sweeping
 *     phase.
 *
 * (2) if S == "...01G", S is imeediate value. Imeediate value is
 *     separated into these types by the value of least 2 or 5 bits of
 *     ((unsigned int S) >> 3).
 *
 *           S        Type
 *     .....01|01G : Integer
 *     .....11|01G : Char
 *     ------------------------------
 *     ..00000|01G : #f
 *     ..00010|01G : #t
 *     ..00100|01G : ()
 *     ..00110|01G : EOF
 *     ..01000|01G : Quote
 *     ..01010|01G : Quasiquote
 *     ..01100|01G : Unquote
 *     ..01110|01G : UnquoteSplicing
 *     ..10000|01G : Unbound
 *     ..10010|01G : Undef
 *
 * (3) if S == "...10G", S is Closure. G-bit of S->X is used as
 *     marking bit of GC and G-bit of S-Y is always 0 for sweeping
 *     phase.
 *
 * (4) if S == "...11G", S is other types. Type is separated by the
 *     value of least n bits of S->Y. Anyway, G-bit of S-Y is always
 *     1 for sweeping phase..
 *
 *        S->Y        Type
 *     ...0000|1 : Symbol
 *     ...0001|1 : String
 *     ...0010|1 : Func
 *     ...0011|1 : Vector
 *     ...0100|1 : Port
 *     ...0101|1 : Continuation
 *     ...0110|1 : Values
 *     ...0111|1 : FreeCell
 *     ...1000|1 : C Pointer
 *     ...1001|1 : C Function Pointer
 */

/*=======================================
   System Include
=======================================*/
#include <stdio.h>

/*=======================================
   Local Include
=======================================*/

/*=======================================
   Type Declarations
=======================================*/
typedef struct ScmCell_ ScmCell;
typedef ScmCell *ScmObj;
typedef ScmObj *ScmRef;
typedef struct _ScmPortInfo ScmPortInfo;
typedef struct ScmEvalState_ ScmEvalState;
typedef ScmObj (*ScmFuncType)();

/*=======================================
   Struct Declarations
=======================================*/
struct ScmCell_ {
    ScmObj X;
    ScmObj Y;
};

/*=======================================
   Accessors For Scheme Objects
=======================================*/
/* G bit Accessor */
#define G_BIT(a)            ((unsigned int)(a) & 0x1)
#define SCM_DO_MARK(a)      ((a) = (ScmObj)((unsigned int)(a) | 0x1))
#define SCM_DO_UNMARK(a)    ((a) = (ScmObj)((unsigned int)(a) & ~0x1))

/* S bit Accessor */
#define SCM_S_MASK(a)       ((unsigned int)(a) & ~0x7)
#define SCM_S_CONSP(a)      (((unsigned int)(a) & 0x6) == 0x0)
#define SCM_S_IMMEDIATEP(a) (((unsigned int)(a) & 0x6) == 0x2)
#define SCM_S_CLOSUREP(a)   (((unsigned int)(a) & 0x6) == 0x4)
#define SCM_S_OTHERSP(a)    (((unsigned int)(a) & 0x6) == 0x6)

#define SCM_S_ENTYPE_CONS(a)      ((a) = (ScmObj)((unsigned int)(a) | ((0x0 & 0x03) << 1)))
#define SCM_S_ENTYPE_IMMEDIATE(a) ((a) = (ScmObj)((unsigned int)(a) | ((0x1 & 0x03) << 1)))
#define SCM_S_ENTYPE_CLOSURE(a)   ((a) = (ScmObj)((unsigned int)(a) | ((0x2 & 0x03) << 1)))
#define SCM_S_ENTYPE_OTHERS(a)    ((a) = (ScmObj)((unsigned int)(a) | ((0x3 & 0x03) << 1)))

#define SCM_S_IMMEDIATE_TYPEBITS(a)             (((unsigned int)(a)) >> 3)
#define SCM_S_OTHERS_TYPEBITS(a)                (((unsigned int)((a)->Y)) >> 1)
#define SCM_S_ENTYPE_IMMEDIATE_TYPEBITS(a, val) ((a) = (ScmObj)(((unsigned int)(a)) | (val << 3)))
#define SCM_S_ENTYPE_OTHERS_TYPEBITS(a, val)    ((a)->Y = (ScmObj)(((unsigned int)((a)->Y)) | ((val & 0xf) << 1)))

#define SCM_S_ENTYPE_IMMEDIATE_VAL(a, val) (SCM_S_ENTYPE_IMMEDIATE(a), SCM_S_ENTYPE_IMMEDIATE_TYPEBITS(a, val))
#define SCM_S_ENTYPE_OTHERS_VAL(a, val)    (SCM_S_ENTYPE_OTHERS(a), SCM_S_ENTYPE_OTHERS_TYPEBITS(a, val))


/* Type Confirmation */
#if SCM_ACCESSOR_ASSERT
#define SCM_ASSERT_TYPE(cond, x) (SCM_ASSERT(cond), (ScmObj)SCM_S_MASK(x))
#else
#define SCM_ASSERT_TYPE(cond, x) ((ScmObj)SCM_S_MASK(x))
#endif /* SCM_ACCESSOR_ASSERT */
#define SCM_AS_INT(a)            (SCM_ASSERT_TYPE(SCM_INTP(a),           (a)))
#define SCM_AS_CONS(a)           (SCM_ASSERT_TYPE(SCM_CONSP(a),          (a)))
#define SCM_AS_SYMBOL(a)         (SCM_ASSERT_TYPE(SCM_SYMBOLP(a),        (a)))
#define SCM_AS_CHAR(a)           (SCM_ASSERT_TYPE(SCM_CHARP(a),          (a)))
#define SCM_AS_STRING(a)         (SCM_ASSERT_TYPE(SCM_STRINGP(a),        (a)))
#define SCM_AS_FUNC(a)           (SCM_ASSERT_TYPE(SCM_FUNCP(a),          (a)))
#define SCM_AS_CLOSURE(a)        (SCM_ASSERT_TYPE(SCM_CLOSUREP(a),       (a)))
#define SCM_AS_VECTOR(a)         (SCM_ASSERT_TYPE(SCM_VECTORP(a),        (a)))
#define SCM_AS_PORT(a)           (SCM_ASSERT_TYPE(SCM_PORTP(a),          (a)))
#define SCM_AS_CONTINUATION(a)   (SCM_ASSERT_TYPE(SCM_CONTINUATIONP(a),  (a)))
#define SCM_AS_VALUEPACKET(a)    (SCM_ASSERT_TYPE(SCM_VALUEPACKETP(a),   (a)))
#define SCM_AS_C_POINTER(a)      (SCM_ASSERT_TYPE(SCM_C_POINTERP(a),     (a)))
#define SCM_AS_C_FUNCPOINTER(a)  (SCM_ASSERT_TYPE(SCM_C_FUNCPOINTERP(a), (a)))

/* Type Predicates */
#define SCM_INTP(a)              (SCM_S_IMMEDIATEP(a) && (SCM_S_IMMEDIATE_TYPEBITS(a) & 0x3) == 0x1)
#define SCM_CHARP(a)             (SCM_S_IMMEDIATEP(a) && (SCM_S_IMMEDIATE_TYPEBITS(a) & 0x3) == 0x2)
#define SCM_SYMBOLP(a)           (SCM_S_OTHERSP(a) && SCM_S_OTHERS_TYPEBITS(a) == 0x0)
#define SCM_STRINGP(a)           (SCM_S_OTHERSP(a) && SCM_S_OTHERS_TYPEBITS(a) == 0x1)
#define SCM_FUNCP(a)             (SCM_S_OTHERSP(a) && SCM_S_OTHERS_TYPEBITS(a) == 0x2)
#define SCM_VECTORP(a)           (SCM_S_OTHERSP(a) && SCM_S_OTHERS_TYPEBITS(a) == 0x3)
#define SCM_PORTP(a)             (SCM_S_OTHERSP(a) && SCM_S_OTHERS_TYPEBITS(a) == 0x4)
#define SCM_CONTINUATIONP(a)     (SCM_S_OTHERSP(a) && SCM_S_OTHERS_TYPEBITS(a) == 0x5)
#define SCM_VALUESP(a)           (SCM_S_OTHERSP(a) && SCM_S_OTHERS_TYPEBITS(a) == 0x6)
#define SCM_FREECELLP(a)         (SCM_S_OTHERSP(a) && SCM_S_OTHERS_TYPEBITS(a) == 0x7)
#define SCM_C_POINTERP(a)        (SCM_S_OTHERSP(a) && SCM_S_OTHERS_TYPEBITS(a) == 0x8)
#define SCM_C_FUNCPOINTERP(a)    (SCM_S_OTHERSP(a) && SCM_S_OTHERS_TYPEBITS(a) == 0x9) 

/* Entyping Macros */
#define SCM_CLEAR(a)                  (a = (void*)0)
#define SCM_ENTYPE_INT(a)             (SCM_CLEAR(a), SCM_S_ENTYPE_IMMEDIATE_VAL(a, 0x1))
#define SCM_ENTYPE_CHAR(a)            (SCM_CLEAR(a), SCM_S_ENTYPE_IMMEDIATE_VAL(a, 0x2))
#define SCM_ENTYPE_FALSE(a)           (SCM_CLEAR(a), SCM_S_ENTYPE_IMMEDIATE_VAL(a, 0x0))
#define SCM_ENTYPE_TRUE(a)            (SCM_CLEAR(a), SCM_S_ENTYPE_IMMEDIATE_VAL(a, 0x2))
#define SCM_ENTYPE_NULL(a)            (SCM_CLEAR(a), SCM_S_ENTYPE_IMMEDIATE_VAL(a, 0x4))
#define SCM_ENTYPE_EOF(a)             (SCM_CLEAR(a), SCM_S_ENTYPE_IMMEDIATE_VAL(a, 0x6))
#define SCM_ENTYPE_QUOTE(a)           (SCM_CLEAR(a), SCM_S_ENTYPE_IMMEDIATE_VAL(a, 0x8))
#define SCM_ENTYPE_QUASIQUOTE(a)      (SCM_CLEAR(a), SCM_S_ENTYPE_IMMEDIATE_VAL(a, 0xa))
#define SCM_ENTYPE_UNQUOTE(a)         (SCM_CLEAR(a), SCM_S_ENTYPE_IMMEDIATE_VAL(a, 0xc))
#define SCM_ENTYPE_UNQUOTESPLICING(a) (SCM_CLEAR(a), SCM_S_ENTYPE_IMMEDIATE_VAL(a, 0xe))
#define SCM_ENTYPE_UNBOUND(a)         (SCM_CLEAR(a), SCM_S_ENTYPE_IMMEDIATE_VAL(a, 0xf))
#define SCM_ENTYPE_UNDEF(a)           (SCM_CLEAR(a), SCM_S_ENTYPE_IMMEDIATE_VAL(a, 0xf2))
#define SCM_ENTYPE_SYMBOL(a)          (SCM_CLEAR(a), SCM_S_ENTYPE_OTHERS_VAL(a, 0x0))
#define SCM_ENTYPE_STRING(a)          (SCM_CLEAR(a), SCM_S_ENTYPE_OTHERS_VAL(a, 0x1))
#define SCM_ENTYPE_FUNC(a)            (SCM_CLEAR(a), SCM_S_ENTYPE_OTHERS_VAL(a, 0x2))
#define SCM_ENTYPE_VECTOR(a)          (SCM_CLEAR(a), SCM_S_ENTYPE_OTHERS_VAL(a, 0x03))
#define SCM_ENTYPE_PORT(a)            (SCM_CLEAR(a), SCM_S_ENTYPE_OTHERS_VAL(a, 0x04))
#define SCM_ENTYPE_CONTINUATION(a)    (SCM_CLEAR(a), SCM_S_ENTYPE_OTHERS_VAL(a, 0x05))
#define SCM_ENTYPE_VALUES(a)          (SCM_CLEAR(a), SCM_S_ENTYPE_OTHERS_VAL(a, 0x06))
#define SCM_ENTYPE_FREECELL(a)        (SCM_CLEAR(a), SCM_S_ENTYPE_OTHERS_VAL(a, 0x07))
#define SCM_ENTYPE_C_POINTER(a)       (SCM_CLEAR(a), SCM_S_ENTYPE_OTHERS_VAL(a, 0x08))
#define SCM_ENTYPE_C_FUNC_POINTER(a)  (SCM_CLEAR(a), SCM_S_ENTYPE_OTHERS_VAL(a, 0x09))

/* Real Accessors */
#define SCM_INT_VALUE(a)              (((int)SCM_AS_INT(a)) >> 5)
#define SCM_INT_SET_VALUE(a, val)     ((a) = (ScmObj)(((unsigned int)a & 0x1f) | (val << 5)))

#endif /* __SIGSCMTYPE_COMPACT_H */
