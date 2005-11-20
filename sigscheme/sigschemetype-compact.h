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
 * (1) if S == "...00G", S is ConsCell. G-bit of S->car is used as
 *     S->cdr's G bit is always set to 0, which helps determine the
 *     finalization semantics without a pointer.
 *
 * (2) if S == "...01G", S is Closure. G-bit of S->car is used as
 *     marking bit of GC.
 *     S->cdr's G bit is always set to 0, which helps determine the
 *     finalization semantics without a pointer.
 *
 * (4) if S == "...10G", S is other types. Type is separated by the
 *     value of least n bits of S->cdr.
 *     S->cdr's G bit is always set to 1, which helps determine the
 *     finalization semantics without a pointer.
 *
 *        S->cdr              Type                content of S->cdr
 *     .....|00|1 : Symbol              : symbol name
 *     .....|01|1 : String              : string length
 *     .....|10|1 : Vector              : vector length
 *     ..000|11|1 : Values              : all 0 (for efficiency)
 *     ..001|11|1 : Func                : ScmFuncTypeCode
 *     ..010|11|1 : Port                : ScmPortDirection
 *     ..011|11|1 : Continuation        : tag
 *     ..100|11|1 : C Pointer           : pointer type
 *                                      :   0 = void*, 1 = ScmFuncType
 *     ..101|11|1 : Reserved            :
 *     ..110|11|1 : Reserved            :
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
 *     .000|11|11G : ()
 *     .001|11|11G : INVALID
 *     .010|11|11G : UNBOUND
 *     .011|11|11G : #f
 *     .100|11|11G : #t
 *     .101|11|11G : EOF
 *     .110|11|11G : UNDEF
 */

/*=======================================
   System Include
=======================================*/
#include <stdio.h>

/*=======================================
   Local Include
=======================================*/
#include "baseport.h"

/*=======================================
   Type Declarations
=======================================*/
typedef struct ScmCell_ ScmCell;
typedef ScmCell *ScmObj;
typedef ScmObj *ScmRef;
typedef struct ScmEvalState_ ScmEvalState;
typedef ScmObj (*ScmFuncType)();

/*=======================================
   Struct Declarations
=======================================*/
/*
 * Internal representation of these types MUST NOT directly touched by libsscm
 * users. What libsscm users allowed is referring the types and constant values
 * in declarations and definitions.
 *
 * All operations touching the internal representation such as accessing a
 * member of a struct must be performed through the accessor macros defined in
 * the section "Accessors For Scheme Objects" below. Otherwise the client code
 * of libsscm will be broken when SigScheme has change internal object
 * representations. The macros abstract the difference.
 */

struct ScmCell_ {
    ScmObj car;
    ScmObj cdr;
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

/*
 * Function types:
 *
 * Function objects must tag themselves with proper information so
 * that the evaluator can correctly invoke them.  See doc/invocation
 * for details.
 */
enum ScmFuncTypeCode {
    SCM_FUNCTYPE_MAND_BITS = 4,
    SCM_FUNCTYPE_MAND_MASK = (1 << SCM_FUNCTYPE_MAND_BITS)-1,
#define SCM_FUNCTYPE_MAND_MAX 5
    /* SCM_FUNCTYPE_MAND_MAX  = 5, */
    SCM_FUNCTYPE_SYNTAX    = 1 << SCM_FUNCTYPE_MAND_BITS,

    SCM_FUNCTYPE_FIXED     = 0 << (SCM_FUNCTYPE_MAND_BITS+1),
    SCM_FUNCTYPE_VARIADIC  = 1 << (SCM_FUNCTYPE_MAND_BITS+1),
    SCM_FUNCTYPE_TAIL_REC  = 1 << (SCM_FUNCTYPE_MAND_BITS+2),

    SCM_FUNCTYPE_ODDBALL   = 1 << (SCM_FUNCTYPE_MAND_BITS+10),

    /* Compound types. */
    SCM_PROCEDURE_FIXED              = SCM_FUNCTYPE_FIXED,
    SCM_PROCEDURE_FIXED_TAIL_REC     = SCM_FUNCTYPE_TAIL_REC,
    SCM_PROCEDURE_VARIADIC           = SCM_FUNCTYPE_VARIADIC,
    SCM_PROCEDURE_VARIADIC_TAIL_REC  = SCM_FUNCTYPE_VARIADIC | SCM_FUNCTYPE_TAIL_REC,

    SCM_SYNTAX_FIXED          = SCM_PROCEDURE_FIXED | SCM_FUNCTYPE_SYNTAX,
    SCM_SYNTAX_FIXED_TAIL_REC = SCM_PROCEDURE_FIXED_TAIL_REC | SCM_FUNCTYPE_SYNTAX,
    SCM_SYNTAX_VARIADIC       = SCM_PROCEDURE_VARIADIC | SCM_FUNCTYPE_SYNTAX,
    SCM_SYNTAX_VARIADIC_TAIL_REC = SCM_PROCEDURE_VARIADIC_TAIL_REC | SCM_FUNCTYPE_SYNTAX,

    /* Special type. */
    SCM_REDUCTION_OPERATOR = SCM_FUNCTYPE_ODDBALL
};

/* Where we are in a reduction process. */
enum ScmReductionState {
    SCM_REDUCE_0,               /* No argument was given. */
    SCM_REDUCE_1,               /* Only 1 argument was given. */
    SCM_REDUCE_PARTWAY,         /* We have more arguments pending. */
    SCM_REDUCE_LAST,            /* The callee must finalize. */
    SCM_REDUCE_STOP             /* Callee wants to stop. */
};

enum ScmReturnType {
    SCM_RETTYPE_AS_IS           = 0,
    SCM_RETTYPE_NEED_EVAL       = 1
};

/* The evaluator's state */
struct ScmEvalState_ {
    ScmObj env;
    enum ScmReturnType ret_type;
};

/*=======================================
   Masks Offsets, and Tags
=======================================*/
#define SCM_GCBIT_WIDTH     1
#define SCM_GCBIT_OFFSET    0
#define SCM_GCBIT_MASK      (0x1 << SCM_GCBIT_OFFSET)
#define SCM_GCBIT_UNMARKED  (0x0 << SCM_GCBIT_OFFSET)
#define SCM_GCBIT_MARKED    (0x1 << SCM_GCBIT_OFFSET)

/* 'IMM' stands for 'Immediate' */
#define SCM_TAG_WIDTH       2
#define SCM_TAG_OFFSET      1
#define SCM_TAG_MASK        (0x3 << SCM_TAG_OFFSET)
#define SCM_TAG_CONS        (0x0 << SCM_TAG_OFFSET)
#define SCM_TAG_CLOSURE     (0x1 << SCM_TAG_OFFSET)
#define SCM_TAG_OTHERS      (0x2 << SCM_TAG_OFFSET)
#define SCM_TAG_IMM         (0x3 << SCM_TAG_OFFSET)

#define SCM_VALUE_OFFSET    (SCM_TAG_WIDTH + SCM_GCBIT_WIDTH)
#define SCM_VALUE_MASK      (~0U << SCM_VALUE_OFFSET)

/*==============================================================================
  Masks Offsets, and Tags : Others
==============================================================================*/
/* mask */
#define SCM_TAG_OTHERS_MASK_SYMBOL               (0x1 | (0x3 << SCM_GCBIT_WIDTH))
#define SCM_TAG_OTHERS_MASK_STRING               (0x1 | (0x3 << SCM_GCBIT_WIDTH))
#define SCM_TAG_OTHERS_MASK_VECTOR               (0x1 | (0x3 << SCM_GCBIT_WIDTH))
#define SCM_TAG_OTHERS_MASK_VALUES               (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x7 << 3))
#define SCM_TAG_OTHERS_MASK_FUNC                 (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x7 << 3))
#define SCM_TAG_OTHERS_MASK_PORT                 (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x7 << 3))
#define SCM_TAG_OTHERS_MASK_CONTINUATION         (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x7 << 3))
#define SCM_TAG_OTHERS_MASK_C_POINTER            (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x7 << 3) | (0x1 << 6))
/* #define SCM_TAG_OTHERS_MASK_FREECELL             (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x7 << 3)) */

/* tag */
#define SCM_TAG_OTHERS_SYMBOL                    (0x1 | (0x0 << SCM_GCBIT_WIDTH))
#define SCM_TAG_OTHERS_STRING                    (0x1 | (0x1 << SCM_GCBIT_WIDTH))
#define SCM_TAG_OTHERS_VECTOR                    (0x1 | (0x2 << SCM_GCBIT_WIDTH))
#define SCM_TAG_OTHERS_VALUES                    (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x0 << 3))
#define SCM_TAG_OTHERS_FUNC                      (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x1 << 3))
#define SCM_TAG_OTHERS_PORT                      (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x2 << 3))
#define SCM_TAG_OTHERS_CONTINUATION              (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x3 << 3))
#define SCM_TAG_OTHERS_C_POINTER                 (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x4 << 3) | (0x0 << 6))
#define SCM_TAG_OTHERS_C_FUNCPOINTER             (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x4 << 3) | (0x1 << 6))
/* #define SCM_TAG_OTHERS_FREECELL                  (0x1 | (0x3 << SCM_GCBIT_WIDTH) | (0x7 << 3)) */

/* offset */
#define SCM_TAG_OTHERS_VALUE_OFFSET_STRING       (SCM_GCBIT_WIDTH + SCM_TAG_WIDTH)
#define SCM_TAG_OTHERS_VALUE_OFFSET_VECTOR       (SCM_GCBIT_WIDTH + SCM_TAG_WIDTH)
#define SCM_TAG_OTHERS_VALUE_OFFSET_FUNC         (SCM_GCBIT_WIDTH + SCM_TAG_WIDTH + 3)
#define SCM_TAG_OTHERS_VALUE_OFFSET_PORT         (SCM_GCBIT_WIDTH + SCM_TAG_WIDTH + 3)
#define SCM_TAG_OTHERS_VALUE_OFFSET_CONTINUATION (SCM_GCBIT_WIDTH + SCM_TAG_WIDTH + 3)

/*==============================================================================
  Masks Offsets, and Tags : IMM
==============================================================================*/
/* mask */
#define SCM_TAG_IMM_MASK_INT                     (SCM_TAG_MASK | (0x1 << 3))
#define SCM_TAG_IMM_MASK_CHAR                    (SCM_TAG_MASK | (0x3 << 3))
#define SCM_TAG_IMM_MASK_CONST                   (SCM_TAG_MASK | (0x3 << 3))
#define SCM_TAG_IMM_MASK_CONST_VALUE             (SCM_TAG_MASK | (0x3 << 3)  | (0x7 << 5))

/* tag */
#define SCM_TAG_IMM_INT                          (SCM_TAG_IMM  | (0x0 << 3))
#define SCM_TAG_IMM_CHAR                         (SCM_TAG_IMM  | (0x1 << 3))
#define SCM_IMM_NULL                             (SCM_TAG_IMM  | (0x3 << 3)  | (0x0 << 5))
#define SCM_IMM_INVALID                          (SCM_TAG_IMM  | (0x3 << 3)  | (0x1 << 5))
#define SCM_IMM_UNBOUND                          (SCM_TAG_IMM  | (0x3 << 3)  | (0x2 << 5))
#define SCM_IMM_FALSE                            (SCM_TAG_IMM  | (0x3 << 3)  | (0x3 << 5))
#define SCM_IMM_TRUE                             (SCM_TAG_IMM  | (0x3 << 3)  | (0x4 << 5))
#define SCM_IMM_EOF                              (SCM_TAG_IMM  | (0x3 << 3)  | (0x5 << 5))
#define SCM_IMM_UNDEF                            (SCM_TAG_IMM  | (0x3 << 3)  | (0x6 << 5))

/* offset */
#define SCM_TAG_IMM_VALUE_OFFSET_INT             (SCM_GCBIT_WIDTH + SCM_TAG_WIDTH + 1)
#define SCM_TAG_IMM_VALUE_OFFSET_CHAR            (SCM_GCBIT_WIDTH + SCM_TAG_WIDTH + 2)


/*=======================================
   Getter & Setter
=======================================*/
/* Aware GC Bit */
#define SCM_GET_VALUE_AS_OBJ_DISCARDS_GCBIT(a, mask)   ((ScmObj)(SCM_CAST_UINT(a) & (mask & ~SCM_GCBIT_MASK)))
#define SCM_GET_VALUE_AS_INT_DISCARDS_GCBIT(a, offset) ((int)   (SCM_CAST_UINT(a) >> (offset)))
#define SCM_GET_VALUE_AS_PTR_DISCARDS_GCBIT(a, mask)   ((void*) (SCM_CAST_UINT(a) & (mask & ~SCM_GCBIT_MASK)))
#define SCM_GET_VALUE_AS_STR_DISCARDS_GCBIT(a, mask)   ((char*) (SCM_GET_VALUE_AS_PTR_DISCARDS_GCBIT((a), (mask & ~SCM_GCBIT_MASK))))

#define SCM_SET_VALUE_AS_OBJ_REMAIN_GCBIT(a, val)                       \
    ((a) = (ScmObj)((SCM_CAST_UINT(a) & SCM_GCBIT_MASK) | (SCM_CAST_UINT(val) & ~SCM_GCBIT_MASK)))
#define SCM_SET_VALUE_AS_INT_REMAIN_GCBIT(a, val, offset, tag)          \
    (SCM_SET_VALUE_AS_OBJ_REMAIN_GCBIT((a), ((SCM_CAST_UINT(val) << (offset)) | (tag))))
#define SCM_SET_VALUE_AS_PTR_REMAIN_GCBIT(a, val, tag)                  \
    (SCM_SET_VALUE_AS_OBJ_REMAIN_GCBIT((a), (SCM_CAST_UINT(val) | (tag))))
#define SCM_SET_VALUE_AS_STR_REMAIN_GCBIT(a, val, tag)                  \
    (SCM_SET_VALUE_AS_PTR_REMAIN_GCBIT((a), (val), (tag))

/* Primary Obj */
#define SCM_PRIMARY_GET_VALUE_AS_OBJ(a)         (SCM_GET_VALUE_AS_OBJ_DISCARDS_GCBIT((a), SCM_VALUE_MASK))
#define SCM_PRIMARY_GET_VALUE_AS_INT(a, offset) (SCM_GET_VALUE_AS_INT_DISCARDS_GCBIT((a), (offset)))
#define SCM_PRIMARY_GET_VALUE_AS_PTR(a, mask)   (SCM_GET_VALUE_AS_PTR_DISCARDS_GCBIT((a), (mask)))
#define SCM_PRIMARY_GET_VALUE_AS_STR(a, mask)   ((char*)SCM_PRIMARY_GET_VALUE_AS_PTR((a), (mask)))

/* Not Used
#define SCM_PRIMARY_SET_VALUE_AS_OBJ(a, val)              (SCM_SET_VALUE_AS_OBJ_REMAIN_GCBIT((a), (val))) */
#define SCM_PRIMARY_SET_VALUE_AS_INT(a, val, offset, tag) (SCM_SET_VALUE_AS_INT_REMAIN_GCBIT((a), (val), (offset), (tag)))
#define SCM_PRIMARY_SET_VALUE_AS_PTR(a, val, tag)         (SCM_SET_VALUE_AS_PTR_REMAIN_GCBIT((a), (val), (tag)))
#define SCM_PRIMARY_SET_VALUE_AS_STR(a, val, tag)         (SCM_PRIMARY_SET_VALUE_AS_PTR((a), (val), (tag)))

/* CAR & CDR Direct Accessor */
#define SCM_GET_DIRECT_CAR(a)      (SCM_PRIMARY_GET_VALUE_AS_OBJ(a)->car)
#define SCM_GET_DIRECT_CDR(a)      (SCM_PRIMARY_GET_VALUE_AS_OBJ(a)->cdr)
#define SCM_SET_DIRECT_CAR(a, val) (SCM_GET_DIRECT_CAR(a) = (ScmObj)(val))
#define SCM_SET_DIRECT_CDR(a, val) (SCM_GET_DIRECT_CDR(a) = (ScmObj)(val))

/* CAR */
#define SCM_CAR_GET_VALUE_AS_OBJ(a)         (SCM_GET_VALUE_AS_OBJ_DISCARDS_GCBIT(SCM_GET_DIRECT_CAR(a), ~0U))
#define SCM_CAR_GET_VALUE_AS_INT(a, offset) (SCM_GET_VALUE_AS_INT_DISCARDS_GCBIT(SCM_GET_DIRECT_CAR(a), (offset)))
#define SCM_CAR_GET_VALUE_AS_PTR(a)         (SCM_GET_VALUE_AS_PTR_DISCARDS_GCBIT(SCM_GET_DIRECT_CAR(a), ~0U))
#define SCM_CAR_GET_VALUE_AS_STR(a)         ((char*)SCM_CAR_GET_VALUE_AS_PTR(a))

#define SCM_CAR_SET_VALUE_AS_OBJ(a, val)         (SCM_SET_VALUE_AS_OBJ_REMAIN_GCBIT(SCM_GET_DIRECT_CAR(a), (val)))
#define SCM_CAR_SET_VALUE_AS_INT(a, val, offset) (SCM_SET_VALUE_AS_INT_REMAIN_GCBIT(SCM_GET_DIRECT_CAR(a), (val), (offset), 0))
#define SCM_CAR_SET_VALUE_AS_PTR(a, val)         (SCM_SET_VALUE_AS_PTR_REMAIN_GCBIT(SCM_GET_DIRECT_CAR(a), (val), 0))
#define SCM_CAR_SET_VALUE_AS_STR(a, val)         (SCM_CAR_SET_VALUE_AS_PTR((a), (val)))

/* CDR */
#define SCM_CDR_GET_VALUE_AS_OBJ(a)         (SCM_GET_VALUE_AS_OBJ_DISCARDS_GCBIT(SCM_GET_DIRECT_CDR(a), ~0U))
#define SCM_CDR_GET_VALUE_AS_INT(a, offset) (SCM_GET_VALUE_AS_INT_DISCARDS_GCBIT(SCM_GET_DIRECT_CDR(a), (offset)))
#define SCM_CDR_GET_VALUE_AS_PTR(a, mask)   (SCM_GET_VALUE_AS_PTR_DISCARDS_GCBIT(SCM_GET_DIRECT_CDR(a), (mask)))
#define SCM_CDR_GET_VALUE_AS_STR(a, mask)   ((char*)SCM_CDR_GET_VALUE_AS_PTR((a), (mask)))

#define SCM_CDR_SET_VALUE_AS_OBJ(a, val)              (SCM_SET_VALUE_AS_OBJ_REMAIN_GCBIT(SCM_GET_DIRECT_CDR(a), (val)))
#define SCM_CDR_SET_VALUE_AS_INT(a, val, offset, tag) (SCM_SET_VALUE_AS_INT_REMAIN_GCBIT(SCM_GET_DIRECT_CDR(a), (val), (offset), (tag)))
#define SCM_CDR_SET_VALUE_AS_PTR(a, val, tag)         (SCM_SET_VALUE_AS_PTR_REMAIN_GCBIT(SCM_GET_DIRECT_CDR(a), (val), (tag)))
#define SCM_CDR_SET_VALUE_AS_STR(a, val, tag)         (SCM_CDR_SET_VALUE_AS_PTR((a), (val), (tag)))

/*=======================================
   Casting to unsigned int
=======================================*/
#define SCM_CAST_UINT(a)     ((unsigned int)(a))
#define SCM_CAST_CAR_UINT(a) SCM_CAST_UINT(SCM_GET_DIRECT_CAR(a))
#define SCM_CAST_CDR_UINT(a) SCM_CAST_UINT(SCM_GET_DIRECT_CDR(a))

/*=======================================
   GC bit Accessor
=======================================*/
#define SCM_GC_BIT(a)       (SCM_CAST_UINT(a) & SCM_GCBIT_MASK)

/*=======================================
   Type Predicates
=======================================*/
/* Tag Accessor */
#define SCM_TAG_CONSP(a)      ((SCM_CAST_UINT(a) & SCM_TAG_MASK) == SCM_TAG_CONS)
#define SCM_TAG_CLOSUREP(a)   ((SCM_CAST_UINT(a) & SCM_TAG_MASK) == SCM_TAG_CLOSURE)
#define SCM_TAG_OTHERSP(a)    ((SCM_CAST_UINT(a) & SCM_TAG_MASK) == SCM_TAG_OTHERS)
#define SCM_TAG_IMMP(a)       ((SCM_CAST_UINT(a) & SCM_TAG_MASK) == SCM_TAG_IMM)

/* Tag -> Others */
#define SCM_TAG_OTHERS_SYMBOLP(a)         ((SCM_CAST_CDR_UINT(a) & SCM_TAG_OTHERS_MASK_SYMBOL) \
                                             == SCM_TAG_OTHERS_SYMBOL)
#define SCM_TAG_OTHERS_STRINGP(a)         ((SCM_CAST_CDR_UINT(a) & SCM_TAG_OTHERS_MASK_STRING) \
                                             == SCM_TAG_OTHERS_STRING)
#define SCM_TAG_OTHERS_VECTORP(a)         ((SCM_CAST_CDR_UINT(a) & SCM_TAG_OTHERS_MASK_VECTOR) \
                                             == SCM_TAG_OTHERS_VECTOR)
#define SCM_TAG_OTHERS_VALUESP(a)         ((SCM_CAST_CDR_UINT(a) & SCM_TAG_OTHERS_MASK_VALUES) \
                                             == SCM_TAG_OTHERS_VALUES)
#define SCM_TAG_OTHERS_FUNCP(a)           ((SCM_CAST_CDR_UINT(a) & SCM_TAG_OTHERS_MASK_FUNC) \
                                             == SCM_TAG_OTHERS_FUNC)
#define SCM_TAG_OTHERS_PORTP(a)           ((SCM_CAST_CDR_UINT(a) & SCM_TAG_OTHERS_MASK_PORT) \
                                             == SCM_TAG_OTHERS_PORT)
#define SCM_TAG_OTHERS_CONTINUATIONP(a)   ((SCM_CAST_CDR_UINT(a) & SCM_TAG_OTHERS_MASK_CONTINUATION) \
                                             == SCM_TAG_OTHERS_CONTINUATION)
#define SCM_TAG_OTHERS_C_POINTERP(a)      ((SCM_CAST_CDR_UINT(a) & SCM_TAG_OTHERS_MASK_C_POINTER) \
                                             == SCM_TAG_OTHERS_C_POINTER)
#define SCM_TAG_OTHERS_C_FUNCPOINTERP(a) ((SCM_CAST_CDR_UINT(a) & SCM_TAG_OTHERS_MASK_C_POINTER) \
                                             == SCM_TAG_OTHERS_C_FUNCPOINTER)
/*
#define SCM_TAG_OTHERS_FREECELLP(a)       ((SCM_CAST_CDR_UINT(a) & SCM_TAG_OTHERS_MASK_FREECELL) \
*/

/* Tag -> Imm */
#define SCM_TAG_IMM_INTP(a)               ((SCM_CAST_UINT(a) & SCM_TAG_IMM_MASK_INT)   == SCM_TAG_IMM_INT)
#define SCM_TAG_IMM_CHARP(a)              ((SCM_CAST_UINT(a) & SCM_TAG_IMM_MASK_CHAR)  == SCM_TAG_IMM_CHAR)

#define SCM_TAG_IMM_CONSTANTP(a)          ((SCM_CAST_UINT(a) & SCM_TAG_IMM_MASK_CONST) == (SCM_TAG_IMM | (0x3 << 3)))
#define SCM_TAG_IMM_NULLP(a)              (SCM_CAST_UINT(a) == SCM_IMM_NULL)
#define SCM_TAG_IMM_INVALIDP(a)           (SCM_CAST_UINT(a) == SCM_IMM_INVALID)
#define SCM_TAG_IMM_UNBOUNDP(a)           (SCM_CAST_UINT(a) == SCM_IMM_UNBOUND)
#define SCM_TAG_IMM_FALSEP(a)             (SCM_CAST_UINT(a) == SCM_IMM_FALSE)
#define SCM_TAG_IMM_TRUEP(a)              (SCM_CAST_UINT(a) == SCM_IMM_TRUE)
#define SCM_TAG_IMM_EOFP(a)               (SCM_CAST_UINT(a) == SCM_IMM_EOF)
#define SCM_TAG_IMM_UNDEFP(a)             (SCM_CAST_UINT(a) == SCM_IMM_UNDEF)

/* Type Predicates */
#define SCM_CONSP(a)             (SCM_TAG_CONSP(a))
#define SCM_CLOSUREP(a)          (SCM_TAG_CLOSUREP(a))

#define SCM_SYMBOLP(a)           (SCM_TAG_OTHERSP(a) && SCM_TAG_OTHERS_SYMBOLP(a))
#define SCM_STRINGP(a)           (SCM_TAG_OTHERSP(a) && SCM_TAG_OTHERS_STRINGP(a))
#define SCM_VECTORP(a)           (SCM_TAG_OTHERSP(a) && SCM_TAG_OTHERS_VECTORP(a))
#define SCM_VALUEPACKETP(a)      (SCM_TAG_OTHERSP(a) && SCM_TAG_OTHERS_VALUESP(a))
#define SCM_FUNCP(a)             (SCM_TAG_OTHERSP(a) && SCM_TAG_OTHERS_FUNCP(a))
#define SCM_PORTP(a)             (SCM_TAG_OTHERSP(a) && SCM_TAG_OTHERS_PORTP(a))
#define SCM_CONTINUATIONP(a)     (SCM_TAG_OTHERSP(a) && SCM_TAG_OTHERS_CONTINUATIONP(a))
#define SCM_C_POINTERP(a)        (SCM_TAG_OTHERSP(a) && SCM_TAG_OTHERS_C_POINTERP(a))
#define SCM_C_FUNCPOINTERP(a)    (SCM_TAG_OTHERSP(a) && SCM_TAG_OTHERS_C_FUNCPOINTERP(a))
#define SCM_INTP(a)              (SCM_TAG_IMM_INTP(a))
#define SCM_CHARP(a)             (SCM_TAG_IMM_CHARP(a))

#define SCM_CONSTANTP(a)         (SCM_TAG_IMM_CONSTANTP(a))

/*=======================================
   Type Confirmation
=======================================*/
/*
#if SCM_ACCESSOR_ASSERT
#define SCM_ASSERT_TYPE(cond, a) (SCM_ASSERT(cond), SCM_GET_VALUE_AS_OBJ((a)))
#else
#define SCM_ASSERT_TYPE(cond, a) (SCM_GET_VALUE_AS_OBJ((a)))
#endif
#define SCM_AS_CONS(a)           (SCM_ASSERT_TYPE(SCM_CONSP((a)),          (a)))
#define SCM_AS_CLOSURE(a)        (SCM_ASSERT_TYPE(SCM_CLOSUREP((a)),       (a)))
#define SCM_AS_SYMBOL(a)         (SCM_ASSERT_TYPE(SCM_SYMBOLP((a)),        (a)))
#define SCM_AS_STRING(a)         (SCM_ASSERT_TYPE(SCM_STRINGP((a)),        (a)))
#define SCM_AS_VECTOR(a)         (SCM_ASSERT_TYPE(SCM_VECTORP((a)),        (a)))
#define SCM_AS_VALUEPACKET(a)    (SCM_ASSERT_TYPE(SCM_VALUEPACKETP((a)),   (a)))
#define SCM_AS_FUNC(a)           (SCM_ASSERT_TYPE(SCM_FUNCP((a)),          (a)))
#define SCM_AS_PORT(a)           (SCM_ASSERT_TYPE(SCM_PORTP((a)),          (a)))
#define SCM_AS_CONTINUATION(a)   (SCM_ASSERT_TYPE(SCM_CONTINUATIONP((a)),  (a)))
#define SCM_AS_C_POINTER(a)      (SCM_ASSERT_TYPE(SCM_C_POINTERP((a)),     (a)))
#define SCM_AS_C_FUNCPOINTER(a)  (SCM_ASSERT_TYPE(SCM_C_FUNCPOINTERP((a)), (a)))
#define SCM_AS_INT(a)            (SCM_ASSERT_TYPE(SCM_INTP((a)),           (a)))
#define SCM_AS_CHAR(a)           (SCM_ASSERT_TYPE(SCM_CHARP((a)),          (a)))
*/

/*=======================================
   Entyping Macros
=======================================*/
#define SCM_ENTYPE_TAG(a, tag, mask)      ((a) = (ScmObj)((SCM_CAST_UINT(a) & mask) | (tag)))
#define SCM_ENTYPE_PRIMARY_TAG(a, tag)    (SCM_ENTYPE_TAG((a), (tag),  ~SCM_TAG_MASK))
#define SCM_ENTYPE_PRIMARY_TAG_CONS(a)    (SCM_ENTYPE_PRIMARY_TAG((a), SCM_TAG_CONS))
#define SCM_ENTYPE_PRIMARY_TAG_CLOSURE(a) (SCM_ENTYPE_PRIMARY_TAG((a), SCM_TAG_CLOSURE))
#define SCM_ENTYPE_PRIMARY_TAG_OTHERS(a)  (SCM_ENTYPE_PRIMARY_TAG((a), SCM_TAG_OTHERS))

#define SCM_ENTYPE_CONS(a)          (SCM_ENTYPE_PRIMARY_TAG_CONS(a),    SCM_DO_UNMARK(a), SCM_SET_DIRECT_CDR(a, 0x0))
#define SCM_ENTYPE_CLOSURE(a)       (SCM_ENTYPE_PRIMARY_TAG_CLOSURE(a), SCM_DO_UNMARK(a), SCM_SET_DIRECT_CDR(a, 0x0))
#define SCM_ENTYPE_SYMBOL(a)        (SCM_ENTYPE_PRIMARY_TAG_OTHERS(a),  SCM_DO_UNMARK(a), SCM_SET_DIRECT_CDR(a, SCM_TAG_OTHERS_SYMBOL))
#define SCM_ENTYPE_STRING(a)        (SCM_ENTYPE_PRIMARY_TAG_OTHERS(a),  SCM_DO_UNMARK(a), SCM_SET_DIRECT_CDR(a, SCM_TAG_OTHERS_STRING))
#define SCM_ENTYPE_VECTOR(a)        (SCM_ENTYPE_PRIMARY_TAG_OTHERS(a),  SCM_DO_UNMARK(a), SCM_SET_DIRECT_CDR(a, SCM_TAG_OTHERS_VECTOR))
#define SCM_ENTYPE_VALUEPACKET(a)   (SCM_ENTYPE_PRIMARY_TAG_OTHERS(a),  SCM_DO_UNMARK(a), SCM_SET_DIRECT_CDR(a, SCM_TAG_OTHERS_VALUES))
#define SCM_ENTYPE_FUNC(a)          (SCM_ENTYPE_PRIMARY_TAG_OTHERS(a),  SCM_DO_UNMARK(a), SCM_SET_DIRECT_CDR(a, SCM_TAG_OTHERS_FUNC))
#define SCM_ENTYPE_PORT(a)          (SCM_ENTYPE_PRIMARY_TAG_OTHERS(a),  SCM_DO_UNMARK(a), SCM_SET_DIRECT_CDR(a, SCM_TAG_OTHERS_PORT))
#define SCM_ENTYPE_CONTINUATION(a)  (SCM_ENTYPE_PRIMARY_TAG_OTHERS(a),  SCM_DO_UNMARK(a), SCM_SET_DIRECT_CDR(a, SCM_TAG_OTHERS_CONTINUATION))
#define SCM_ENTYPE_C_POINTER(a)     (SCM_ENTYPE_PRIMARY_TAG_OTHERS(a),  SCM_DO_UNMARK(a), SCM_SET_DIRECT_CDR(a, SCM_TAG_OTHERS_C_POINTER))
#define SCM_ENTYPE_C_FUNCPOINTER(a) (SCM_ENTYPE_PRIMARY_TAG_OTHERS(a),  SCM_DO_UNMARK(a), SCM_SET_DIRECT_CDR(a, SCM_TAG_OTHERS_C_FUNCPOINTER))
#define SCM_ENTYPE_INT(a)           (SCM_ENTYPE_TAG((a), SCM_TAG_IMM_INT,  ~SCM_TAG_IMM_MASK_INT))
#define SCM_ENTYPE_CHAR(a)          (SCM_ENTYPE_TAG((a), SCM_TAG_IMM_CHAR, ~SCM_TAG_IMM_MASK_CHAR))
#define SCM_ENTYPE_NULL(a)          (SCM_ENTYPE_TAG((a), SCM_IMM_NULL,     ~SCM_TAG_IMM_MASK_CONST_VALUE))
#define SCM_ENTYPE_INVALID(a)       (SCM_ENTYPE_TAG((a), SCM_IMM_INVALID,  ~SCM_TAG_IMM_MASK_CONST_VALUE))
#define SCM_ENTYPE_UNBOUND(a)       (SCM_ENTYPE_TAG((a), SCM_IMM_UNBOUND,  ~SCM_TAG_IMM_MASK_CONST_VALUE))
#define SCM_ENTYPE_FALSE(a)         (SCM_ENTYPE_TAG((a), SCM_IMM_FALSE,    ~SCM_TAG_IMM_MASK_CONST_VALUE))
#define SCM_ENTYPE_TRUE(a)          (SCM_ENTYPE_TAG((a), SCM_IMM_TRUE,     ~SCM_TAG_IMM_MASK_CONST_VALUE))
#define SCM_ENTYPE_EOF(a)           (SCM_ENTYPE_TAG((a), SCM_IMM_EOF,      ~SCM_TAG_IMM_MASK_CONST_VALUE))
#define SCM_ENTYPE_UNDEF(a)         (SCM_ENTYPE_TAG((a), SCM_IMM_UNDEF,    ~SCM_TAG_IMM_MASK_CONST_VALUE))

/*=======================================
   Real Accessors
=======================================*/
#define SCM_CAR(a)                       (SCM_CAR_GET_VALUE_AS_OBJ(a))
#define SCM_CDR(a)                       (SCM_CDR_GET_VALUE_AS_OBJ(a))
#define SCM_CONS_SET_CAR(a, car)         (SCM_CAR_SET_VALUE_AS_OBJ((a), (car)))
#define SCM_CONS_SET_CDR(a, cdr)         (SCM_CDR_SET_VALUE_AS_OBJ((a), (cdr)))
#define SCM_CAAR(a)                      (SCM_CAR(SCM_CAR(a)))
#define SCM_CADR(a)                      (SCM_CAR(SCM_CDR(a)))
#define SCM_CDAR(a)                      (SCM_CDR(SCM_CAR(a)))
#define SCM_CDDR(a)                      (SCM_CDR(SCM_CDR(a)))

#define SCM_CLOSURE_EXP(a)               (SCM_CAR_GET_VALUE_AS_OBJ(a))
#define SCM_CLOSURE_ENV(a)               (SCM_CDR_GET_VALUE_AS_OBJ(a))
#define SCM_CLOSURE_SET_EXP(a, exp)      (SCM_CAR_SET_VALUE_AS_OBJ((a), (exp)))
#define SCM_CLOSURE_SET_ENV(a, env)      (SCM_CDR_SET_VALUE_AS_OBJ((a), (env)))

#define SCM_SYMBOL_VCELL(a)              (SCM_CAR_GET_VALUE_AS_OBJ(a))
#define SCM_SYMBOL_NAME(a)               (SCM_CDR_GET_VALUE_AS_STR((a), ~SCM_TAG_OTHERS_MASK_SYMBOL))
#define SCM_SYMBOL_SET_VCELL(a, vcell)   (SCM_CAR_SET_VALUE_AS_OBJ((a), (vcell)))
#define SCM_SYMBOL_SET_NAME(a, name)     (SCM_CDR_SET_VALUE_AS_STR((a), (name), SCM_TAG_OTHERS_SYMBOL))

#define SCM_STRING_LEN(a)                (SCM_CAR_GET_VALUE_AS_INT((a), SCM_TAG_OTHERS_VALUE_OFFSET_STRING))
#define SCM_STRING_STR(a)                (SCM_CDR_GET_VALUE_AS_STR((a), ~SCM_TAG_OTHERS_MASK_STRING))
#define SCM_STRING_SET_LEN(a, len)       (SCM_CAR_SET_VALUE_AS_INT((a), (len), SCM_TAG_OTHERS_VALUE_OFFSET_STRING))
#define SCM_STRING_SET_STR(a, str)       (SCM_CDR_SET_VALUE_AS_STR((a), (str), SCM_TAG_OTHERS_STRING))

#define SCM_VECTOR_VEC(a)                ((ScmObj*)(SCM_CAR_GET_VALUE_AS_PTR((a))))
#define SCM_VECTOR_LEN(a)                (SCM_CDR_GET_VALUE_AS_INT((a), SCM_TAG_OTHERS_VALUE_OFFSET_VECTOR))
#define SCM_VECTOR_SET_VEC(a, vec)       (SCM_CAR_SET_VALUE_AS_PTR((a), (vec)))
#define SCM_VECTOR_SET_LEN(a, len)       (SCM_CDR_SET_VALUE_AS_INT((a), (len), SCM_TAG_OTHERS_VALUE_OFFSET_VECTOR, SCM_TAG_OTHERS_VECTOR))
#define SCM_VECTOR_CREF(a, idx)          ((SCM_VECTOR_VEC(a))[idx])
#define SCM_VECTOR_SET_CREF(a, idx, b)   (SCM_VECTOR_CREF((a), (idx)) = (b))
#define SCM_VECTOR_REF(a, idx)           (SCM_VECTOR_CREF((a), SCM_INT_VALUE(idx)))
#define SCM_VECTOR_SET_REF(a, idx, b)    (SCM_VECTOR_REF((a), (idx)) = (b))

#define SCM_MAKE_VALUEPACKET(vals)       (Scm_NewValuePacket(vals))
#define SCM_VALUEPACKET_VALUES(a)        (SCM_CAR_GET_VALUE_AS_OBJ(a))
#define SCM_VALUEPACKET_SET_VALUES(a, v) (SCM_CAR_SET_VALUE_AS_OBJ((a), (v)))

/* FIXME: This is a workaround way, because CAR part overwrites GC bit.
 * How can we deal with this? */
#define SCM_FUNC_CFUNC(a)                (SCM_WORD_CAST(ScmFuncType, SCM_GET_DIRECT_CAR(a)))
#define SCM_FUNC_TYPECODE(a)             ((enum ScmFuncTypeCode)SCM_CDR_GET_VALUE_AS_INT((a), SCM_TAG_OTHERS_VALUE_OFFSET_FUNC))
#define SCM_FUNC_SET_CFUNC(a, fptr)      (SCM_SET_DIRECT_CAR((a), SCM_WORD_CAST(ScmObj, (fptr))))
#define SCM_FUNC_SET_TYPECODE(a, code)   (SCM_CDR_SET_VALUE_AS_INT((a), (code), SCM_TAG_OTHERS_VALUE_OFFSET_FUNC, SCM_TAG_OTHERS_FUNC))
#define SCM_SYNTAXP(a) (SCM_FUNCP(a)                                         \
                        && (SCM_FUNC_TYPECODE(a) & SCM_FUNCTYPE_SYNTAX))
#define SCM_PROCEDUREP(a) ((SCM_FUNCP(a)                                     \
                            && !(SCM_FUNC_TYPECODE(a) & SCM_FUNCTYPE_SYNTAX)) \
                           || SCM_CLOSUREP(a)                                \
                           || SCM_CONTINUATIONP(a))

#define SCM_PORT_IMPL(a)                 ((ScmCharPort*)SCM_CAR_GET_VALUE_AS_PTR(a))
#define SCM_PORT_FLAG(a)                 ((enum ScmPortFlag)SCM_CDR_GET_VALUE_AS_INT((a), SCM_TAG_OTHERS_VALUE_OFFSET_PORT))
#define SCM_PORT_SET_IMPL(a, impl)       (SCM_CAR_SET_VALUE_AS_PTR((a), (ScmObj)(impl)))
#define SCM_PORT_SET_FLAG(a, flag)       (SCM_CDR_SET_VALUE_AS_INT((a), (flag), SCM_TAG_OTHERS_VALUE_OFFSET_PORT, SCM_TAG_OTHERS_PORT))

#define SCM_CONTINUATION_OPAQUE(a)          ((void*)SCM_CAR_GET_VALUE_AS_PTR(a))
#define SCM_CONTINUATION_TAG(a)             (SCM_CDR_GET_VALUE_AS_INT((a), SCM_TAG_OTHERS_VALUE_OFFSET_CONTINUATION))
#define SCM_CONTINUATION_SET_OPAQUE(a, val) (SCM_CAR_SET_VALUE_AS_PTR((a), (val)))
#define SCM_CONTINUATION_SET_TAG(a, val)    (SCM_CDR_SET_VALUE_AS_INT((a), (val), SCM_TAG_OTHERS_VALUE_OFFSET_CONTINUATION, SCM_TAG_OTHERS_CONTINUATION))

#define SCM_C_POINTER_VALUE(a)              ((void*)SCM_CAR_GET_VALUE_AS_PTR(a))
#define SCM_C_POINTER_SET_VALUE(a, val)     (SCM_CAR_SET_VALUE_AS_PTR((a), (val)))

#define SCM_C_FUNCPOINTER_VALUE(a)          (SCM_WORD_CAST(ScmCFunc, SCM_CAR_GET_VALUE_AS_PTR(a)))
#define SCM_C_FUNCPOINTER_SET_VALUE(a, val) (SCM_CAR_SET_VALUE_AS_PTR((a), SCM_WORD_CAST(ScmObj, (val))))

#define SCM_CHAR_VALUE(a)         (SCM_PRIMARY_GET_VALUE_AS_STR((a), ~SCM_TAG_IMM_MASK_CHAR))
#define SCM_CHAR_SET_VALUE(a, ch) (SCM_PRIMARY_SET_VALUE_AS_STR((a), (ch), SCM_TAG_IMM_CHAR))

/*
 * Integer need to preserve 'singed' or 'unsigned', so need special accessor.
 * Current pack and unpack algorithm is like this.
 *
 * int pack(int a) {
 *   return (a < 0) ? (~a << OFFSET) | SIGNED_MARK
 *                  : (a << OFFSET);
 * }
 *
 * int unpack(int a) {
 *   return (a & SIGN_BIT_MASK) ? ~((a & SIGN_VALUE_MASK) >> OFFSET) | SIGNED_MARK
 *                              : (a >> OFFSET);
 * }
 *
 */
#define BITS_PER_BITE             8
#define SIZEOF_INT                sizeof(int)
#define SIGN_BIT_MASK             (0x1 << (SIZEOF_INT * BITS_PER_BITE - 1))
#define SIGN_VALUE_MASK           ~SIGN_BIT_MASK
#define SIGNED_MARK               (0x1 << (SIZEOF_INT * BITS_PER_BITE - 1))

#define SCM_INT_VALUE(a)          ((SCM_CAST_UINT(a) & SIGN_BIT_MASK)   \
                                   ? ~((SCM_CAST_UINT(a) & SIGN_VALUE_MASK) >> SCM_TAG_IMM_VALUE_OFFSET_INT) | SIGNED_MARK \
                                   : (SCM_CAST_UINT(a) >> SCM_TAG_IMM_VALUE_OFFSET_INT))
    
#define SCM_INT_SET_VALUE(a, val) (SCM_SET_VALUE_AS_OBJ_REMAIN_GCBIT((a), \
                                                                     ((val) >= 0) \
                                                                     ? (val) << SCM_TAG_IMM_VALUE_OFFSET_INT | SCM_TAG_IMM_INT \
                                                                     : (~(val) << SCM_TAG_IMM_VALUE_OFFSET_INT) | SIGNED_MARK | SCM_TAG_IMM_INT))

/*=======================================
   Scheme Special Constants
=======================================*/
#define SCM_NULL        ((ScmObj)(SCM_IMM_NULL))
#define SCM_EOF         ((ScmObj)(SCM_IMM_EOF))
#define SCM_UNDEF       ((ScmObj)(SCM_IMM_UNDEF))
#define SCM_INVALID     ((ScmObj)(SCM_IMM_INVALID))
#define SCM_UNBOUND     ((ScmObj)(SCM_IMM_UNBOUND))
#define SCM_FALSE       ((ScmObj)(SCM_IMM_FALSE))
#define SCM_TRUE        ((ScmObj)(SCM_IMM_TRUE))

#define SCM_EQ(a, b)    ((a) == (b))
#define SCM_VALIDP(a)   (!SCM_TAG_IMM_INVALIDP(a))
#define SCM_INVALIDP(a) (SCM_TAG_IMM_INVALIDP(a))
#define SCM_NULLP(a)    (SCM_TAG_IMM_NULLP(a))
#define SCM_FALSEP(a)   (SCM_TAG_IMM_FALSEP(a))
#define SCM_NFALSEP(a)  (!SCM_TAG_IMM_FALSEP(a))
#define SCM_EOFP(a)     (SCM_TAG_IMM_EOFP(a))

/*============================================================================
  GC Marking & Unmarking
============================================================================*/
#define SCM_DO_MARK(a)     (SCM_GET_DIRECT_CAR(a) = (ScmObj)(SCM_CAST_UINT(SCM_GET_DIRECT_CAR(a)) & ~0x1))
#define SCM_DO_UNMARK(a)   (SCM_GET_DIRECT_CAR(a) = (ScmObj)(SCM_CAST_UINT(SCM_GET_DIRECT_CAR(a)) | 0x1))

#define SCM_IS_MARKED(a)   ((SCM_CAST_UINT(SCM_GET_DIRECT_CAR(a)) & SCM_GCBIT_MASK) == 0x0)
#define SCM_IS_UNMARKED(a) (!SCM_IS_MARKED(obj))

/*============================================================================
  Predefined Symbols
============================================================================*/
/* for list construction */
#define SCM_SYM_QUOTE            Scm_sym_quote
#define SCM_SYM_QUASIQUOTE       Scm_sym_quasiquote
#define SCM_SYM_UNQUOTE          Scm_sym_unquote
#define SCM_SYM_UNQUOTE_SPLICING Scm_sym_unquote_splicing

/*============================================================================
  Internal Declarations For Predefined Symbols
============================================================================*/
/*
 * These declarations are dedicated to internal use. libsscm users MUST NOT
 * refer these internal representations directly.
 *
 * It may be changed when SigScheme's internal storage model or accessing
 * method for the constants has been changed. To avoid suffering code
 * incompatibility from it, use the abstract macro such as SCM_SYM_QUOTE
 * defined above. They safely hides the internal model against such change.
 */
/* sigscheme.c */
extern ScmObj Scm_sym_quote, Scm_sym_quasiquote;
extern ScmObj Scm_sym_unquote, Scm_sym_unquote_splicing;

/*============================================================================
  Environment Specifiers
============================================================================*/
#define SCM_INTERACTION_ENV SCM_NULL
/*
 * Current implementation cannot handle scheme-report-environment and
 * null-environment properly. Be careful to use these environemnts.
 */
#define SCM_R5RS_ENV        SCM_INTERACTION_ENV
#define SCM_NULL_ENV        SCM_INTERACTION_ENV

#define SCM_ENVP(env) (NULLP(env) || CONSP(env))

/*============================================================================
  Abstract ScmObj Reference For Storage-Representation Independent Efficient
  List Operations
============================================================================*/
#define SCM_INVALID_REF   NULL

#define SCM_REF_CAR(cons) (&SCM_GET_DIRECT_CAR(cons))
#define SCM_REF_CDR(cons) (&SCM_GET_DIRECT_CDR(cons))
#define SCM_REF_OFF_HEAP(obj) (&(obj))
#define SCM_DEREF(ref)    (*(ref))

/* RFC: Is there a better name? */
#define SCM_SET(ref, obj) (*(ref) = (ScmObj)((SCM_CAST_UINT(*(ref)) & SCM_GCBIT_MASK) | SCM_CAST_UINT(obj)))

/*============================================================================
  Compatibility for non-compact code
============================================================================*/
/* Scheme Object Type */
enum ScmObjType {
    ScmInt          = 0,
    ScmCons         = 1,
    ScmSymbol       = 2,
    ScmChar         = 3,
    ScmString       = 4,
    ScmFunc         = 5,
    ScmClosure      = 6,
    ScmVector       = 7,
    ScmPort         = 8,
    ScmContinuation = 9,
    ScmConstant     = 10,
    ScmValuePacket  = 11,
    ScmFreeCell     = 12,

    ScmCPointer     = 20,
    ScmCFuncPointer = 21
};

/* storage.c */
#define SCM_TYPE(a) Scm_Type(a)
extern enum ScmObjType Scm_Type(ScmObj obj);

#endif /* __SIGSCMTYPE_COMPACT_H */
