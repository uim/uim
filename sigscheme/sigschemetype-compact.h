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
 *     S->Y's G bit is always set to 0, which helps determine the
 *     finalization semantics without a pointer. 
 *
 * (2) if S == "...01G", S is Closure. G-bit of S->X is used as
 *     marking bit of GC.
 *     S->Y's G bit is always set to 0, which helps determine the
 *     finalization semantics without a pointer. 
 *
 * (4) if S == "...10G", S is other types. Type is separated by the
 *     value of least n bits of S->Y.
 *     S->Y's G bit is always set to 1, which helps determine the
 *     finalization semantics without a pointer. 
 *
 *        S->Y              Type                content of S->Y
 *     .....|00|1 : Symbol              : symbol name
 *     .....|01|1 : String              : string length
 *     .....|10|1 : Vector              : vector length
 *     ..000|11|1 : Values              : all 0 (for efficiency)
 *     ..001|11|1 : Func                : ScmFuncTypeCode
 *     ..010|11|1 : Port                : ScmPortDirection
 *     ..011|11|1 : Continuation        : all 0 (for efficiency)
 *     ..100|11|1 : C Pointer           : pointer type
 *                                      :   0 = void*, 1 = ScmFuncType
 *     ..101|11|1 : Reserved            :
 *     ..110|11|1 : Special Constant    : constant ID
 *                                          0: EOF
 *                                          1: Undef
 *     ..111|11|1 : FreeCell            : all 0 (for efficiency)
 *
 * (4) if S == "...11G", S is an immediate value. Immediate values are
 *     separated into these types by the value of least 1-5 bits of
 *     ((unsigned int S) >> 3).
 *
 *           S        Type
 *     ......0|11G : Integer
 *     .....01|11G : Char
 *     .....11|11G : Constant
 *     ------------------------------
 *     Constants
 *     ..00|11|11G : INVALID
 *     ..01|11|11G : UNBOUND
 *     ..10|11|11G : #f
 *     ..11|11|11G : #t
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
   Masks and Offsets
=======================================*/
#define SCM_VALUE_MASK      (~0 ^ (SCM_TAG_MASK | SCM_GCBIT_MASK))

#define SCM_GCBIT_MASK      0x1
#define SCM_GCBIT_UNMARKED  0x0
#define SCM_GCBIT_MARKED    0x1

#define SCM_TAG_MASK        0x6
#define SCM_TAG_CONS        0x0
#define SCM_TAG_CLOSURE     0x2
#define SCM_TAG_OTHERS      0x4
#define SCM_TAG_IMM         0x6 /* 'IMM' stands for 'Immediate' */

#define SCM_TAG_OTHERS_MASK_1                    0x7
#define SCM_TAG_OTHERS_MASK_2                    0x3f
#define SCM_TAG_OTHERS_MASK_3                    0x7f
#define SCM_TAG_OTHERS_MASK_SYMBOL               SCM_TAG_OTHERS_MASK_1
#define SCM_TAG_OTHERS_MASK_STRING               SCM_TAG_OTHERS_MASK_1
#define SCM_TAG_OTHERS_MASK_VECTOR               SCM_TAG_OTHERS_MASK_1
#define SCM_TAG_OTHERS_MASK_VALUES               SCM_TAG_OTHERS_MASK_2
#define SCM_TAG_OTHERS_MASK_FUNC                 SCM_TAG_OTHERS_MASK_2
#define SCM_TAG_OTHERS_MASK_PORT                 SCM_TAG_OTHERS_MASK_2
#define SCM_TAG_OTHERS_MASK_CONTINUATION         SCM_TAG_OTHERS_MASK_2
#define SCM_TAG_OTHERS_MASK_C_POINTER            SCM_TAG_OTHERS_MASK_3
#define SCM_TAG_OTHERS_MASK_SPECIALCONST         SCM_TAG_OTHERS_MASK_3
#define SCM_TAG_OTHERS_MASK_FREECELL             SCM_TAG_OTHERS_MASK_2
#define SCM_TAG_OTHERS_VALUE_OFFSET_1            3
#define SCM_TAG_OTHERS_VALUE_OFFSET_2            6
#define SCM_TAG_OTHERS_VALUE_OFFSET_SYMBOL       SCM_TAG_OTHERS_VALUE_OFFSET_1
#define SCM_TAG_OTHERS_VALUE_OFFSET_STRING       SCM_TAG_OTHERS_VALUE_OFFSET_1
#define SCM_TAG_OTHERS_VALUE_OFFSET_VECTOR       SCM_TAG_OTHERS_VALUE_OFFSET_1
#define SCM_TAG_OTHERS_VALUE_OFFSET_VALUES       SCM_TAG_OTHERS_VALUE_OFFSET_2
#define SCM_TAG_OTHERS_VALUE_OFFSET_FUNC         SCM_TAG_OTHERS_VALUE_OFFSET_2
#define SCM_TAG_OTHERS_VALUE_OFFSET_PORT         SCM_TAG_OTHERS_VALUE_OFFSET_2
#define SCM_TAG_OTHERS_VALUE_OFFSET_CONTINUATION SCM_TAG_OTHERS_VALUE_OFFSET_2
#define SCM_TAG_OTHERS_VALUE_OFFSET_C_POINTER    SCM_TAG_OTHERS_VALUE_OFFSET_2
#define SCM_TAG_OTHERS_VALUE_OFFSET_SPECIALCONST SCM_TAG_OTHERS_VALUE_OFFSET_2
#define SCM_TAG_OTHERS_SYMBOL         0x1
#define SCM_TAG_OTHERS_STRING         0x3
#define SCM_TAG_OTHERS_VECTOR         0x5
#define SCM_TAG_OTHERS_VALUES         0x7
#define SCM_TAG_OTHERS_FUNC           0xf
#define SCM_TAG_OTHERS_PORT           0x17
#define SCM_TAG_OTHERS_CONTINUATION   0x1f
#define SCM_TAG_OTHERS_C_POINTER      0x27
#define SCM_TAG_OTHERS_C_FUNC_POINTER 0x67
#define SCM_TAG_OTHERS_SPECIALCONST   0x37
#define SCM_TAG_OTHERS_EOF            0x37
#define SCM_TAG_OTHERS_UNDEF          0x77
#define SCM_TAG_OTHERS_FREECELL       0x3f

#define SCM_TAG_IMM_MASK_1            0xe
#define SCM_TAG_IMM_MASK_2            0x1e
#define SCM_TAG_IMM_MASK_3            0x7e
#define SCM_TAG_IMM_MASK_INT          SCM_TAG_IMM_MASK_1
#define SCM_TAG_IMM_MASK_CHAR         SCM_TAG_IMM_MASK_2
#define SCM_TAG_IMM_MASK_CONST        SCM_TAG_IMM_MASK_3
#define SCM_TAG_IMM_VALUE_OFFSET_INT  4
#define SCM_TAG_IMM_VALUE_OFFSET_CHAR 5
#define SCM_TAG_IMM_INT     0x6
#define SCM_TAG_IMM_CHAR    0xe
#define SCM_TAG_IMM_CONST   0x1e
#define SCM_TAG_IMM_INVALID 0x1e
#define SCM_TAG_IMM_UNBOUND 0x3e
#define SCM_TAG_IMM_TRUE    0x5e
#define SCM_TAG_IMM_TRUE    0x7e

/*=======================================
   Accessors For Scheme Objects
=======================================*/
/* GC bit Accessor */
#define SCM_GC_BIT(a)       ((unsigned int)(a) & SCM_GCBIT_MASK)
#define SCM_DO_MARK(a)      ((a) = (ScmObj)((unsigned int)(a) | SCM_GCBIT_MASK))
#define SCM_DO_UNMARK(a)    ((a) = (ScmObj)((unsigned int)(a) & ~SCM_GCBIT_MASK))

/* Tag Accessor */
#define SCM_TAG_CONSP(a)      (((unsigned int)(a) & SCM_TAG_MASK) == SCM_TAG_CONS)
#define SCM_TAG_CLOSUREP(a)   (((unsigned int)(a) & SCM_TAG_MASK) == SCM_TAG_CLOSURE)
#define SCM_TAG_OTHERSP(a)    (((unsigned int)(a) & SCM_TAG_MASK) == SCM_TAG_OTHERS)
#define SCM_TAG_IMMEDIATEP(a) (((unsigned int)(a) & SCM_TAG_MASK) == SCM_TAG_IMM)

/* Tag -> Others */
#define SCM_TAG_OTHERS_SYMBOLP(a)         (((unsigned int)(a->Y) & SCM_TAG_OTHERS_MASK_SYMBOL) == SCM_TAG_OTHERS_SYMBOL)
#define SCM_TAG_OTHERS_STRINGP(a)         (((unsigned int)(a->Y) & SCM_TAG_OTHERS_MASK_STRING) == SCM_TAG_OTHERS_STRING)
#define SCM_TAG_OTHERS_VECTORP(a)         (((unsigned int)(a->Y) & SCM_TAG_OTHERS_MASK_VECTOR) == SCM_TAG_OTHERS_VECTOR))
#define SCM_TAG_OTHERS_VALUESP(a)         (((unsigned int)(a->Y) & SCM_TAG_OTHERS_MASK_VALUES) == SCM_TAG_OTHERS_VALUES))
#define SCM_TAG_OTHERS_FUNCP(a)           (((unsigned int)(a->Y) & SCM_TAG_OTHERS_MASK_FUNC) == SCM_TAG_OTHERS_FUNC))
#define SCM_TAG_OTHERS_PORTP(a)           (((unsigned int)(a->Y) & SCM_TAG_OTHERS_MASK_PORT) == SCM_TAG_OTHERS_PORT))
#define SCM_TAG_OTHERS_CONTINUATIONP(a)   (((unsigned int)(a->Y) & SCM_TAG_OTHERS_MASK_CONTINUATION) == SCM_TAG_OTHERS_CONTINUATION))
#define SCM_TAG_OTHERS_C_POINTERP(a)      (((unsigned int)(a->Y) & SCM_TAG_OTHERS_MASK_C_POINTER) == SCM_TAG_OTHERS_C_POINTER))
#define SCM_TAG_OTHERS_C_FUNC_POINTERP(a) (((unsigned int)(a->Y) & SCM_TAG_OTHERS_MASK_C_POINTER) == SCM_TAG_OTHERS_C_FUNC_POINTER))
#define SCM_TAG_OTHERS_EOFP(a)            (((unsigned int)(a->Y) & SCM_TAG_OTHERS_MASK_SPECIALCONST) == SCM_TAG_OTHERS_EOF))
#define SCM_TAG_OTHERS_UNDEFP(a)          (((unsigned int)(a->Y) & SCM_TAG_OTHERS_MASK_SPECIALCONST) == SCM_TAG_OTHERS_UNDEF))
#define SCM_TAG_OTHERS_FREECELLP(a)       (((unsigned int)(a->Y) & SCM_TAG_OTHERS_MASK_FREECELL) == SCM_TAG_OTHERS_FREECELL))
                                        
/* Tag -> Imm */
#define SCM_TAG_IMM_INTP(a)               (((unsigned int)(a) & SCM_TAG_IMM_MASK_INT)   == SCM_TAG_IMM_INT)
#define SCM_TAG_IMM_CHARP(a)              (((unsigned int)(a) & SCM_TAG_IMM_MASK_CHAR)  == SCM_TAG_IMM_CHAR)
#define SCM_TAG_IMM_INVALIDP(a)           (((unsigned int)(a) & SCM_TAG_IMM_MASK_CONST) == SCM_TAG_IMM_INVALID)
#define SCM_TAG_IMM_TRUEP(a)              (((unsigned int)(a) & SCM_TAG_IMM_MASK_CONST) == SCM_TAG_IMM_TRUE)
#define SCM_TAG_IMM_FALSEP(a)             (((unsigned int)(a) & SCM_TAG_IMM_MASK_CONST) == SCM_TAG_IMM_FALSE)

/* Type Predicates */
#define SCM_CONSP(a)             (SCM_TAG_CONSP(a))
#define SCM_CLOSUREP(a)          (SCM_TAG_CLOSUREP(a))
#define SCM_SYMBOLP(a)           (SCM_TAG_OTHERS_SYMBOLP(a))
#define SCM_STRINGP(a)           (SCM_TAG_OTHERS_STRINGP(a))
#define SCM_VECTORP(a)           (SCM_TAG_OTHERS_VECTORP(a))
#define SCM_VALUESP(a)           (SCM_TAG_OTHERS_VALUESP(a))
#define SCM_FUNCP(a)             (SCM_TAG_OTHERS_FUNCP(a))
#define SCM_PORTP(a)             (SCM_TAG_OTHERS_PORTP(a))
#define SCM_CONTINUATIONP(a)     (SCM_TAG_OTHERS_CONTINUATIONP(a))
#define SCM_C_POINTERP(a)        (SCM_TAG_OTHERS_C_POINTERP(a))
#define SCM_C_FUNCPOINTERP(a)    (SCM_TAG_OTHERS_C_FUNC_POINTERP(a))
#define SCM_FREECELLP(a)         (SCM_TAG_OTHERS_FREECELLP(a))
#define SCM_INTP(a)              (SCM_TAG_IMM_INTP(a))
#define SCM_CHARP(a)             (SCM_TAG_IMM_CHARP(a))

/* Type Confirmation */
#if SCM_ACCESSOR_ASSERT
#define SCM_ASSERT_TYPE(cond, x) (SCM_ASSERT(cond), (ScmObj)(((unsigned int)x) & SCM_VALUE_MASK))
#else
#define SCM_ASSERT_TYPE(cond, x) ((ScmObj)(((unsigned int)x) & SCM_VALUE_MASK))
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

/* Entyping Macros */
#define SCM_ENTYPE_SYMBOL(a)          (a = (ScmObj)((unsigned int)a & (SCM_VALUE_MASK | SCM_TAG_OTHERS_SYMBOL)))
#define SCM_ENTYPE_STRING(a)          (a = (ScmObj)((unsigned int)a & (SCM_VALUE_MASK | SCM_TAG_OTHERS_STRING)))
#define SCM_ENTYPE_VECTOR(a)          (a = (ScmObj)((unsigned int)a & (SCM_VALUE_MASK | SCM_TAG_OTHERS_VECTOR)))
#define SCM_ENTYPE_VALUES(a)          (a = (ScmObj)((unsigned int)a & (SCM_VALUE_MASK | SCM_TAG_OTHERS_VALUES)))
#define SCM_ENTYPE_FUNC(a)            (a = (ScmObj)((unsigned int)a & (SCM_VALUE_MASK | SCM_TAG_OTHERS_FUNC)))
#define SCM_ENTYPE_PORT(a)            (a = (ScmObj)((unsigned int)a & (SCM_VALUE_MASK | SCM_TAG_OTHERS_PORT)))
#define SCM_ENTYPE_CONTINUATION(a)    (a = (ScmObj)((unsigned int)a & (SCM_VALUE_MASK | SCM_TAG_OTHERS_CONTINUATION)))
#define SCM_ENTYPE_C_POINTER(a)       (a = (ScmObj)((unsigned int)a & (SCM_VALUE_MASK | SCM_TAG_OTHERS_C_POINTER)))
#define SCM_ENTYPE_C_FUNC_POINTER(a)  (a = (ScmObj)((unsigned int)a & (SCM_VALUE_MASK | SCM_TAG_OTHERS_C_FUNC_POINTERP)))
#define SCM_ENTYPE_EOF(a)             (a = (ScmObj)((unsigned int)a & (SCM_VALUE_MASK | SCM_TAG_OTHERS_EOF)))
#define SCM_ENTYPE_UNDEF(a)           (a = (ScmObj)((unsigned int)a & (SCM_VALUE_MASK | SCM_TAG_OTHERS_UNDEF)))
#define SCM_ENTYPE_FREECELL(a)        (a = (ScmObj)((unsigned int)a & (SCM_VALUE_MASK | SCM_TAG_OTHERS_FREECELL)))
#define SCM_ENTYPE_INT(a)             (a = (ScmObj)SCM_TAG_IMM_INT)
#define SCM_ENTYPE_CHAR(a)            (a = (ScmObj)SCM_TAG_IMM_CHAR)
#define SCM_ENTYPE_INVALID(a)         (a = (ScmObj)SCM_TAG_IMM_INVALID)
#define SCM_ENTYPE_UNBOUND(a)         (a = (ScmObj)SCM_TAG_IMM_UNBOUND)
#define SCM_ENTYPE_FALSE(a)           (a = (ScmObj)SCM_TAG_IMM_FALSE)
#define SCM_ENTYPE_TRUE(a)            (a = (ScmObj)SCM_TAG_IMM_TRUE)

/* Real Accessors */
/*
#define SCM_INT_VALUE(a)              (((int)SCM_AS_INT(a)) >> 5)
#define SCM_INT_SET_VALUE(a, val)     ((a) = (ScmObj)(((unsigned int)a & 0x1f) | (val << 5)))
*/

#endif /* __SIGSCMTYPE_COMPACT_H */
