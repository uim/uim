/*===========================================================================
 *  FileName : storage-compact.h
 *  About    : Storage abstraction (compact representation)
 *
 *  Copyright (C) 2005 Kazuki Ohta <mover AT hct.zaq.ne.jp>
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
#ifndef __STORAGE_COMPACT_H
#define __STORAGE_COMPACT_H

/*
 * Internal representation defined in this file MUST NOT directly touched by
 * libsscm users. Use abstract public APIs defined in sigscheme.h.
 */

/*
 * Object Representation Mechanism
 *
 * First, we assume ScmObj "S" which contains two ScmObj "X" and "Y" (e.g.
 * ScmObj S { X, Y }).
 *
 * (0) LSB(Least Significant Bit) of "S" is called G-bit.
 *
 * (1) if S == "...00G", S is ConsCell. G-bit of S->car is used as S->cdr's G
 *     bit is always set to 0, which helps determine the finalization semantics
 *     without a pointer.
 *
 * (2) if S == "...01G", S is Closure. G-bit of S->car is used as marking bit
 *     of GC. S->cdr's G bit is always set to 0, which helps determine the
 *     finalization semantics without a pointer.
 *
 * (4) if S == "...10G", S is other types. Type is separated by the value of
 *     least n bits of S->cdr. S->cdr's G bit is always set to 1, which helps
 *     determine the finalization semantics without a pointer.
 *
 *        S->car   |     Type     |             content of S->car
 *     -------------------------------------------------------------------------
 *     .......|I|G : String       : I bit is used to represent mutable or
 *                                  immutable string. G bit is used to GC mark
 *                                  information (Mutation Bit). The other bits
 *                                  are used to store string pointer value.
 *     .........|G : Otherwise    : LSB is used to GC mark information. The value
 *                                  of each type is stored in the other bits.
 *
 *        S->cdr   |     Type     |             content of S->cdr
 *     -------------------------------------------------------------------------
 *     ......|00|1 : Symbol       : symbol name
 *     ......|01|1 : String       : string length
 *     ......|10|1 : Vector       : vector length
 *     ...000|11|1 : Values       : all 0 (for efficiency)
 *     ...001|11|1 : Func         : ScmFuncTypeCode and LSB of stored Func address
 *     ...010|11|1 : Port         : ScmPortDirection
 *     ...011|11|1 : Continuation : tag
 *     ...100|11|1 : Pointer      : pointer type id
 *     .00100|11|1 :  - C Ptr     :
 *     .01100|11|1 :  - C FuncPtr :
 *     .10100|11|1 :  - Reserved  :
 *     .11100|11|1 :  - Reserved  :
 *     ...101|11|1 : Reserved5    :
 *     ...110|11|1 : Reserved6    :
 *     ...111|11|1 : FreeCell     : all 0 (for efficiency)
 *
 *     And we splitted the tag and named each part as follows.
 *
 *     ......|ZZ|Z : primary tag
 *     ...ZZZ|..|. : sub tag
 *     ...ZZZ|ZZ|Z : extended tag (combines primary and sub tag)
 *     .ZZ...|..|. : pointer tag
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
 *
 * Notice:
 *   Some data must be aligned properly for compaction.
 *   Required Alignments are listed below.
 *
 * Required Data Aligment:
 *
 *     Symbol
 *         name (char*)        : 8 byte
 *     String
 *         str (char*)         : 4 byte
 *     Vector
 *         vec (ScmObj*)       : 2 byte
 *     Port
 *         impl (ScmCharPort*) : 2 byte
 *     Continuation
 *         opaque (void*)      : 2 byte
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

typedef ScmObj (*ScmFuncType)();

/*=======================================
  Struct Declarations
=======================================*/
enum ScmStrMutationType {
    SCM_STR_IMMUTABLE           = 0,
    SCM_STR_MUTABLE             = 1
};

/* Scheme Object Cell */
struct ScmCell_ {
    ScmObj car;
    ScmObj cdr;
};

/*==============================================================================
                               Internal Macros
==============================================================================*/
/*=======================================
  Masks Offsets, and Tags
=======================================*/
#define SCM_GCBIT_WIDTH     1
#define SCM_GCBIT_OFFSET    0
#define SCM_GCBIT_MASK      (0x1 << SCM_GCBIT_OFFSET)
#define SCM_GCBIT_MARKED    1
#define SCM_GCBIT_UNMARKED  0

/* 'IMM' stands for 'Immediate' */
#define SCM_TAG_WIDTH       2
#define SCM_TAG_OFFSET      (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH)
#define SCM_TAG_MASK        (0x3 << SCM_TAG_OFFSET)
#define SCM_TAG_CONS        (0x0 << SCM_TAG_OFFSET)
#define SCM_TAG_CLOSURE     (0x1 << SCM_TAG_OFFSET)
#define SCM_TAG_OTHERS      (0x2 << SCM_TAG_OFFSET)
#define SCM_TAG_IMM         (0x3 << SCM_TAG_OFFSET)

/*==============================================================================
  Masks Offsets, and Tags : Others' CAR
==============================================================================*/
#define SCM_OTHERS_CAR_VAL_OFFSET_SYMBOL        \
    (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH)
#define SCM_OTHERS_CAR_VAL_OFFSET_STRING        \
    (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH)
#define SCM_OTHERS_CAR_VAL_OFFSET_VECTOR        \
    (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH)
#define SCM_OTHERS_CAR_VAL_OFFSET_VALUES        \
    (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH)
#define SCM_OTHERS_CAR_VAL_OFFSET_FUNC          \
    (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH)
#define SCM_OTHERS_CAR_VAL_OFFSET_PORT          \
    (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH)
#define SCM_OTHERS_CAR_VAL_OFFSET_CONTINUATION  \
    (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH)
#define SCM_OTHERS_CAR_VAL_OFFSET_C_POINTER     \
    (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH)
#define SCM_OTHERS_CAR_VAL_OFFSET_C_FUNCPOINTER \
    (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH)
#define SCM_OTHERS_CAR_VAL_OFFSET_FREECELL      \
    (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH)

/* stored value alignment */
#define SCM_ALIGN_SCMCELL (0x1 << (SCM_TAG_OFFSET + SCM_TAG_WIDTH))
#define SCM_ALIGN_EVEN    (0x1 << 2)
#define SCM_ALIGN_NONE    (0x1 << 1)

#define SCM_OTHERS_CAR_VAL_ALIGNMENT_SYMBOL     \
    SCM_ALIGN_SCMCELL
#define SCM_OTHERS_CAR_VAL_ALIGNMENT_STRING     \
    SCM_ALIGN_EVEN
#define SCM_OTHERS_CAR_VAL_ALIGNMENT_VECTOR     \
    SCM_ALIGN_EVEN
#define SCM_OTHERS_CAR_VAL_ALIGNMENT_VALUES     \
    SCM_ALIGN_SCMCELL
#define SCM_OTHERS_CAR_VAL_ALIGNMENT_FUNC       \
    SCM_ALIGN_NONE
#define SCM_OTHERS_CAR_VAL_ALIGNMENT_PORT       \
    SCM_ALIGN_SCMCELL
#define SCM_OTHERS_CAR_VAL_ALIGNMENT_CONTINUATION       \
    SCM_ALIGN_EVEN
#define SCM_OTHERS_CAR_VAL_ALIGNMENT_C_POINTER          \
    SCM_ALIGN_NONE
#define SCM_OTHERS_CAR_VAL_ALIGNMENT_C_FUNCPOINTER      \
    SCM_ALIGN_NONE
#define SCM_OTHERS_CAR_VAL_ALIGNMENT_FREECELL   \
    SCM_ALIGN_SCMCELL

/*==============================================================================
  Masks Offsets, and Tags : Others' CDR
==============================================================================*/
/* primary tag */
#define SCM_OTHERS_CDR_PRIMARY_TAG_WIDTH  3
#define SCM_OTHERS_CDR_PRIMARY_TAG_OFFSET 0
#define SCM_OTHERS_CDR_PRIMARY_TAG_MASK         \
    (0x7 << SCM_OTHERS_CDR_PRIMARY_TAG_OFFSET)
#define SCM_OTHERS_CDR_PRIMARY_TAG_SYMBOL       \
    (0x1 << SCM_OTHERS_CDR_PRIMARY_TAG_OFFSET)
#define SCM_OTHERS_CDR_PRIMARY_TAG_STRING       \
    (0x3 << SCM_OTHERS_CDR_PRIMARY_TAG_OFFSET)
#define SCM_OTHERS_CDR_PRIMARY_TAG_VECTOR       \
    (0x5 << SCM_OTHERS_CDR_PRIMARY_TAG_OFFSET)
#define SCM_OTHERS_CDR_PRIMARY_TAG_EXT          \
    (0x7 << SCM_OTHERS_CDR_PRIMARY_TAG_OFFSET)

/* sub tag */
#define SCM_OTHERS_CDR_SUB_TAG_WIDTH 3
#define SCM_OTHERS_CDR_SUB_TAG_OFFSET           \
    (SCM_OTHERS_CDR_PRIMARY_TAG_OFFSET + SCM_OTHERS_CDR_SUB_TAG_WIDTH)
#define SCM_OTHERS_CDR_SUB_TAG_MASK             \
    (0x7 << SCM_OTHERS_CDR_SUB_TAG_OFFSET)
#define SCM_OTHERS_CDR_SUB_TAG_VALUES           \
    (0x0 << SCM_OTHERS_CDR_SUB_TAG_OFFSET)
#define SCM_OTHERS_CDR_SUB_TAG_FUNC             \
    (0x1 << SCM_OTHERS_CDR_SUB_TAG_OFFSET)
#define SCM_OTHERS_CDR_SUB_TAG_PORT             \
    (0x2 << SCM_OTHERS_CDR_SUB_TAG_OFFSET)
#define SCM_OTHERS_CDR_SUB_TAG_CONTINUATION     \
    (0x3 << SCM_OTHERS_CDR_SUB_TAG_OFFSET)
#define SCM_OTHERS_CDR_SUB_TAG_POINTER          \
    (0x4 << SCM_OTHERS_CDR_SUB_TAG_OFFSET)
#define SCM_OTHERS_CDR_SUB_TAG_RESERVED5        \
    (0x5 << SCM_OTHERS_CDR_SUB_TAG_OFFSET)
#define SCM_OTHERS_CDR_SUB_TAG_RESERVED6        \
    (0x6 << SCM_OTHERS_CDR_SUB_TAG_OFFSET)
#define SCM_OTHERS_CDR_SUB_TAG_FREECELL         \
    (0x7 << SCM_OTHERS_CDR_SUB_TAG_OFFSET)

/* extended tag (combines primary tag and sub tag) */
#define SCM_OTHERS_CDR_EXT_TAG_WIDTH                                    \
    (SCM_OTHERS_CDR_PRIMARY_TAG_WIDTH + SCM_OTHERS_CDR_SUB_TAG_WIDTH)
#define SCM_OTHERS_CDR_EXT_TAG_OFFSET                                   \
    (SCM_OTHERS_CDR_PRIMARY_TAG_OFFSET)
#define SCM_OTHERS_CDR_EXT_TAG_MASK                                     \
    (SCM_OTHERS_CDR_PRIMARY_TAG_MASK | SCM_OTHERS_CDR_SUB_TAG_MASK)
#define SCM_OTHERS_CDR_EXT_TAG_VALUES                                   \
    (SCM_OTHERS_CDR_PRIMARY_TAG_EXT | SCM_OTHERS_CDR_SUB_TAG_VALUES)
#define SCM_OTHERS_CDR_EXT_TAG_FUNC                                     \
    (SCM_OTHERS_CDR_PRIMARY_TAG_EXT | SCM_OTHERS_CDR_SUB_TAG_FUNC)
#define SCM_OTHERS_CDR_EXT_TAG_PORT                                     \
    (SCM_OTHERS_CDR_PRIMARY_TAG_EXT | SCM_OTHERS_CDR_SUB_TAG_PORT)
#define SCM_OTHERS_CDR_EXT_TAG_CONTINUATION                             \
    (SCM_OTHERS_CDR_PRIMARY_TAG_EXT | SCM_OTHERS_CDR_SUB_TAG_CONTINUATION)
#define SCM_OTHERS_CDR_EXT_TAG_POINTER                                  \
    (SCM_OTHERS_CDR_PRIMARY_TAG_EXT | SCM_OTHERS_CDR_SUB_TAG_POINTER)
#define SCM_OTHERS_CDR_EXT_TAG_RESERVED5                                \
    (SCM_OTHERS_CDR_PRIMARY_TAG_EXT | SCM_OTHERS_CDR_SUB_TAG_RESERVED5)
#define SCM_OTHERS_CDR_EXT_TAG_RESERVED6                                \
    (SCM_OTHERS_CDR_PRIMARY_TAG_EXT | SCM_OTHERS_CDR_SUB_TAG_RESERVED6)
#define SCM_OTHERS_CDR_EXT_TAG_FREECELL                                 \
    (SCM_OTHERS_CDR_PRIMARY_TAG_EXT | SCM_OTHERS_CDR_SUB_TAG_FREECELL)

/* pointer tag */
#define SCM_OTHERS_CDR_PTR_TAG_WIDTH  2
#define SCM_OTHERS_CDR_PTR_TAG_OFFSET           \
    (SCM_OTHERS_CDR_EXT_TAG_OFFSET + SCM_OTHERS_CDR_EXT_TAG_WIDTH)
#define SCM_OTHERS_CDR_PTR_TAG_MASK             \
    (0x3 << SCM_OTHERS_CDR_PTR_TAG_OFFSET)
#define SCM_OTHERS_CDR_PTR_TAG_C_POINTER        \
    (0x0 << SCM_OTHERS_CDR_PTR_TAG_OFFSET)
#define SCM_OTHERS_CDR_PTR_TAG_C_FUNCPOINTER    \
    (0x1 << SCM_OTHERS_CDR_PTR_TAG_OFFSET)

/* value offsets */
#define SCM_OTHERS_CDR_PRIMARY_VAL_OFFSET                             \
    (SCM_OTHERS_CDR_PRIMARY_TAG_OFFSET + SCM_OTHERS_CDR_PRIMARY_TAG_WIDTH)
#define SCM_OTHERS_CDR_EXT_VAL_OFFSET                                 \
    (SCM_OTHERS_CDR_EXT_TAG_OFFSET + SCM_OTHERS_CDR_EXT_TAG_WIDTH)
#define SCM_OTHERS_CDR_PTR_VAL_OFFSET                                 \
    (SCM_OTHERS_CDR_PTR_TAG_OFFSET + SCM_OTHERS_CDR_PTR_TAG_WIDTH)

/* for specific types */
#define SCM_OTHERS_CDR_TAG_SYMBOL               \
    SCM_OTHERS_CDR_PRIMARY_TAG_SYMBOL
#define SCM_OTHERS_CDR_TAG_MASK_SYMBOL          \
    SCM_OTHERS_CDR_PRIMARY_TAG_MASK
#define SCM_OTHERS_CDR_TAG_WIDTH_SYMBOL         \
    SCM_OTHERS_CDR_PRIMARY_TAG_WIDTH
#define SCM_OTHERS_CDR_TAG_OFFSET_SYMBOL        \
    SCM_OTHERS_CDR_PRIMARY_TAG_OFFSET
#define SCM_OTHERS_CDR_VAL_OFFSET_SYMBOL        \
    0 /* Note: str ptr value shouldn't be shifted */
#define SCM_OTHERS_CDR_VAL_MASK_SYMBOL          \
    (~0U << SCM_OTHERS_CDR_PRIMARY_VAL_OFFSET)

#define SCM_OTHERS_CDR_TAG_STRING               \
    SCM_OTHERS_CDR_PRIMARY_TAG_STRING
#define SCM_OTHERS_CDR_TAG_MASK_STRING          \
    SCM_OTHERS_CDR_PRIMARY_TAG_MASK
#define SCM_OTHERS_CDR_TAG_WIDTH_STRING         \
    SCM_OTHERS_CDR_PRIMARY_TAG_WIDTH
#define SCM_OTHERS_CDR_TAG_OFFSET_STRING        \
    SCM_OTHERS_CDR_PRIMARY_TAG_OFFSET
#define SCM_OTHERS_CDR_VAL_OFFSET_STRING        \
    SCM_OTHERS_CDR_PRIMARY_VAL_OFFSET
#define SCM_OTHERS_CDR_VAL_MASK_STRING          \
    (~0U << SCM_OTHERS_CDR_PRIMARY_VAL_OFFSET)

#define SCM_OTHERS_CDR_TAG_VECTOR               \
    SCM_OTHERS_CDR_PRIMARY_TAG_VECTOR
#define SCM_OTHERS_CDR_TAG_MASK_VECTOR          \
    SCM_OTHERS_CDR_PRIMARY_TAG_MASK
#define SCM_OTHERS_CDR_TAG_WIDTH_VECTOR         \
    SCM_OTHERS_CDR_PRIMARY_TAG_WIDTH
#define SCM_OTHERS_CDR_TAG_OFFSET_VECTOR        \
    SCM_OTHERS_CDR_PRIMARY_TAG_OFFSET
#define SCM_OTHERS_CDR_VAL_OFFSET_VECTOR        \
    SCM_OTHERS_CDR_PRIMARY_VAL_OFFSET
#define SCM_OTHERS_CDR_VAL_MASK_VECTOR          \
    (~0U << SCM_OTHERS_CDR_PRIMARY_VAL_OFFSET)

#define SCM_OTHERS_CDR_TAG_VALUES               \
    SCM_OTHERS_CDR_EXT_TAG_VALUES
#define SCM_OTHERS_CDR_TAG_MASK_VALUES          \
    SCM_OTHERS_CDR_EXT_TAG_MASK
#define SCM_OTHERS_CDR_TAG_WIDTH_VALUES         \
    SCM_OTHERS_CDR_EXT_TAG_WIDTH
#define SCM_OTHERS_CDR_TAG_OFFSET_VALUES        \
    SCM_OTHERS_CDR_EXT_TAG_OFFSET
#define SCM_OTHERS_CDR_VAL_OFFSET_VALUES        \
    SCM_OTHERS_CDR_EXT_VAL_OFFSET
#define SCM_OTHERS_CDR_VAL_MASK_VALUES          \
    (~0U << SCM_OTHERS_CDR_EXT_VAL_OFFSET)

#define SCM_OTHERS_CDR_TAG_FUNC                 \
    SCM_OTHERS_CDR_EXT_TAG_FUNC
#define SCM_OTHERS_CDR_TAG_MASK_FUNC            \
    SCM_OTHERS_CDR_EXT_TAG_MASK
#define SCM_OTHERS_CDR_TAG_WIDTH_FUNC           \
    SCM_OTHERS_CDR_EXT_TAG_WIDTH
#define SCM_OTHERS_CDR_TAG_OFFSET_FUNC          \
    SCM_OTHERS_CDR_EXT_TAG_OFFSET
#define SCM_OTHERS_CDR_VAL_OFFSET_FUNC          \
    SCM_OTHERS_CDR_EXT_VAL_OFFSET
#define SCM_OTHERS_CDR_VAL_MASK_FUNC            \
    (~0U << SCM_OTHERS_CDR_EXT_VAL_OFFSET)

#define SCM_OTHERS_CDR_TAG_PORT                 \
    SCM_OTHERS_CDR_EXT_TAG_PORT
#define SCM_OTHERS_CDR_TAG_MASK_PORT            \
    SCM_OTHERS_CDR_EXT_TAG_MASK
#define SCM_OTHERS_CDR_TAG_WIDTH_PORT           \
    SCM_OTHERS_CDR_EXT_TAG_WIDTH
#define SCM_OTHERS_CDR_TAG_OFFSET_PORT          \
    SCM_OTHERS_CDR_EXT_TAG_OFFSET
#define SCM_OTHERS_CDR_VAL_OFFSET_PORT          \
    SCM_OTHERS_CDR_EXT_VAL_OFFSET
#define SCM_OTHERS_CDR_VAL_MASK_PORT            \
    (~0U << SCM_OTHERS_CDR_EXT_VAL_OFFSET)

#define SCM_OTHERS_CDR_TAG_CONTINUATION         \
    SCM_OTHERS_CDR_EXT_TAG_CONTINUATION
#define SCM_OTHERS_CDR_TAG_MASK_CONTINUATION    \
    SCM_OTHERS_CDR_EXT_TAG_MASK
#define SCM_OTHERS_CDR_TAG_WIDTH_CONTINUATION   \
    SCM_OTHERS_CDR_EXT_TAG_WIDTH
#define SCM_OTHERS_CDR_TAG_OFFSET_CONTINUATION  \
    SCM_OTHERS_CDR_EXT_TAG_OFFSET
#define SCM_OTHERS_CDR_VAL_OFFSET_CONTINUATION  \
    SCM_OTHERS_CDR_EXT_VAL_OFFSET
#define SCM_OTHERS_CDR_VAL_MASK_CONTINUATION    \
    (~0U << SCM_OTHERS_CDR_EXT_VAL_OFFSET)

#define SCM_OTHERS_CDR_TAG_C_POINTER            \
    (SCM_OTHERS_CDR_EXT_TAG_POINTER | SCM_OTHERS_CDR_PTR_TAG_C_POINTER)
#define SCM_OTHERS_CDR_TAG_MASK_C_POINTER       \
    (SCM_OTHERS_CDR_EXT_TAG_MASK | SCM_OTHERS_CDR_PTR_TAG_MASK)
#define SCM_OTHERS_CDR_TAG_WIDTH_C_POINTER      \
    (SCM_OTHERS_CDR_EXT_TAG_WIDTH + SCM_OTHERS_CDR_PTR_TAG_WIDTH)
#define SCM_OTHERS_CDR_TAG_OFFSET_C_POINTER     \
    SCM_OTHERS_CDR_PTR_TAG_OFFSET
#define SCM_OTHERS_CDR_VAL_OFFSET_C_POINTER     \
    SCM_OTHERS_CDR_PTR_VAL_OFFSET
#define SCM_OTHERS_CDR_VAL_MASK_C_POINTER       \
    (~0U << SCM_OTHERS_CDR_PTR_VAL_OFFSET)

#define SCM_OTHERS_CDR_TAG_C_FUNCPOINTER        \
    (SCM_OTHERS_CDR_EXT_TAG_POINTER | SCM_OTHERS_CDR_PTR_TAG_C_FUNCPOINTER)
#define SCM_OTHERS_CDR_TAG_MASK_C_FUNCPOINTER   \
    (SCM_OTHERS_CDR_EXT_TAG_MASK | SCM_OTHERS_CDR_PTR_TAG_MASK)
#define SCM_OTHERS_CDR_TAG_WIDTH_C_FUNCPOINTER  \
    (SCM_OTHERS_CDR_EXT_TAG_WIDTH + SCM_OTHERS_CDR_PTR_TAG_WIDTH)
#define SCM_OTHERS_CDR_TAG_OFFSET_C_FUNCPOINTER \
    SCM_OTHERS_CDR_PTR_TAG_OFFSET
#define SCM_OTHERS_CDR_VAL_OFFSET_C_FUNCPOINTER \
    SCM_OTHERS_CDR_PTR_VAL_OFFSET
#define SCM_OTHERS_CDR_VAL_MASK_C_FUNCPOINTER   \
    (~0U << SCM_OTHERS_CDR_PTR_VAL_OFFSET)

#define SCM_OTHERS_CDR_TAG_FREECELL             \
    SCM_OTHERS_CDR_EXT_TAG_FREECELL
#define SCM_OTHERS_CDR_TAG_MASK_FREECELL        \
    SCM_OTHERS_CDR_EXT_TAG_MASK
#define SCM_OTHERS_CDR_TAG_WIDTH_FREECELL       \
    SCM_OTHERS_CDR_EXT_TAG_WIDTH
#define SCM_OTHERS_CDR_TAG_OFFSET_FREECELL      \
    SCM_OTHERS_CDR_EXT_TAG_OFFSET
#define SCM_OTHERS_CDR_VAL_OFFSET_FREECELL      \
    SCM_OTHERS_CDR_EXT_VAL_OFFSET
#define SCM_OTHERS_CDR_VAL_MASK_FREECELL        \
    (~0U << SCM_OTHERS_CDR_EXT_VAL_OFFSET)

/*==============================================================================
  Masks Offsets, and Tags : Immediates
==============================================================================*/
/* mask */
#define SCM_IMM_TAG_MASK_INT      (SCM_TAG_MASK | (0x1 << 3))
#define SCM_IMM_TAG_MASK_CHAR     (SCM_TAG_MASK | (0x3 << 3))
#define SCM_IMM_TAG_MASK_CONSTANT (SCM_TAG_MASK | (0x3 << 3))

/* tag */
#define SCM_IMM_TAG_INT      (SCM_TAG_IMM | (0x0 << 3))
#define SCM_IMM_TAG_CHAR     (SCM_TAG_IMM | (0x1 << 3))
#define SCM_IMM_TAG_CONSTANT (SCM_TAG_IMM | (0x3 << 3))
#define SCM_IMM_TAG_NULL     (SCM_TAG_IMM | (0x3 << 3) | (0x0 << 5))
#define SCM_IMM_TAG_INVALID  (SCM_TAG_IMM | (0x3 << 3) | (0x1 << 5))
#define SCM_IMM_TAG_UNBOUND  (SCM_TAG_IMM | (0x3 << 3) | (0x2 << 5))
#define SCM_IMM_TAG_FALSE    (SCM_TAG_IMM | (0x3 << 3) | (0x3 << 5))
#define SCM_IMM_TAG_TRUE     (SCM_TAG_IMM | (0x3 << 3) | (0x4 << 5))
#define SCM_IMM_TAG_EOF      (SCM_TAG_IMM | (0x3 << 3) | (0x5 << 5))
#define SCM_IMM_TAG_UNDEF    (SCM_TAG_IMM | (0x3 << 3) | (0x6 << 5))

/* for each type */
#define SCM_IMM_VAL_MASK_INT (~(SCM_IMM_TAG_MASK_INT  | SCM_GCBIT_MASK))
#define SCM_IMM_VAL_OFFSET_INT \
    (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH + SCM_TAG_WIDTH + 1)

#define SCM_IMM_VAL_MASK_CHAR (~(SCM_IMM_TAG_MASK_CHAR | SCM_GCBIT_MASK))
#define SCM_IMM_VAL_OFFSET_CHAR \
    (SCM_GCBIT_OFFSET + SCM_GCBIT_WIDTH + SCM_TAG_WIDTH + 2)

/* constant mask and offset are no need */

/*=======================================
  Casting Macros
=======================================*/
#define SCM_CAST_INT(a)    ((int)(a))
#define SCM_CAST_UINT(a)   ((unsigned int)(a))
#define SCM_CAST_SCMOBJ(a) ((ScmObj)(a))
#define SCM_CAST_CHARP(a)  ((char*)(a))

/*=======================================
   Strip Tag Macros
=======================================*/
#define SCM_STRIP_GCBIT(a) (SCM_CAST_UINT(a) & ~SCM_GCBIT_MASK)
/* FIXME: we need to prepare both SCM_STRIP_TAG and SCM_STRIP_TAG_AND_GCBIT macro? */
#define SCM_STRIP_TAG(a)   (SCM_CAST_UINT(a) & ~(SCM_GCBIT_MASK | SCM_TAG_MASK))

/*=======================================
  GC bit Operator
=======================================*/
#define SCM_GCBIT_ON(a)  ((a) = (ScmObj)(SCM_STRIP_GCBIT(a) | 1))
#define SCM_GCBIT_OFF(a) ((a) = (ScmObj)(SCM_STRIP_GCBIT(a) | 0))

/*=======================================
  Tag Accessor
=======================================*/
#define SCM_GCBIT(a) (SCM_CAST_UINT(a) & SCM_GCBIT_MASK)
#define SCM_TAG(a)   (SCM_CAST_UINT(a) & SCM_TAG_MASK)

/*=======================================
  Cell CAR & CDR accessor
=======================================*/
#define SCM_CELL_CAR(a)          (((ScmObj)(SCM_STRIP_TAG(a)))->car)
#define SCM_CELL_CDR(a)          (((ScmObj)(SCM_STRIP_TAG(a)))->cdr)
#define SCM_CELL_SET_CAR(a, val) (SCM_CELL_CAR(a) = (ScmObj)(val))
#define SCM_CELL_SET_CDR(a, val) (SCM_CELL_CDR(a) = (ScmObj)(val))

/*=======================================
  Type Predicates
=======================================*/
/* Tag Predicates */
#define SCM_TAG_CONSP(a)                        \
    (SCM_TAG(a) == SCM_TAG_CONS)
#define SCM_TAG_CLOSUREP(a)                     \
    (SCM_TAG(a) == SCM_TAG_CLOSURE)
#define SCM_TAG_OTHERSP(a)                      \
    (SCM_TAG(a) == SCM_TAG_OTHERS)
#define SCM_TAG_IMMP(a)                         \
    (SCM_TAG(a) == SCM_TAG_IMM)

/* Tag Predicates with specifing Type */
#define SCM_TAG_OTHERS_TYPEP(a, type)                                   \
    ((SCM_TAG(a) == SCM_TAG_OTHERS)                                     \
     && (SCM_CAST_UINT(SCM_CELL_CDR(a)) & SCM_OTHERS_CDR_TAG_MASK_##type) \
         == SCM_OTHERS_CDR_TAG_##type)
#define SCM_TAG_IMM_TYPEP(a, type)                                      \
    ((SCM_TAG(a) == SCM_TAG_IMM)                                        \
     && ((SCM_CAST_UINT(a) & SCM_IMM_TAG_MASK_##type) == SCM_IMM_TAG_##type))

/* Constant Predicates */
#define SCM_IMM_TAG_NULLP(a)    (SCM_EQ((a), SCM_IMM_TAG_NULL))
#define SCM_IMM_TAG_INVALIDP(a) (SCM_EQ((a), SCM_IMM_TAG_INVALID))
#define SCM_IMM_TAG_UNBOUNDP(a) (SCM_EQ((a), SCM_IMM_TAG_UNBOUND))
#define SCM_IMM_TAG_FALSEP(a)   (SCM_EQ((a), SCM_IMM_TAG_FALSE))
#define SCM_IMM_TAG_TRUEP(a)    (SCM_EQ((a), SCM_IMM_TAG_TRUE))
#define SCM_IMM_TAG_EOFP(a)     (SCM_EQ((a), SCM_IMM_TAG_EOF))
#define SCM_IMM_TAG_UNDEFP(a)   (SCM_EQ((a), SCM_IMM_TAG_UNDEF))

/*=======================================
  Accessor API
=======================================*/
/*
 * for Cons
 */
#define SCM_CONS_CAR_VAL(a) ((ScmObj)(SCM_STRIP_GCBIT(SCM_CELL_CAR(a))))
#define SCM_CONS_CDR_VAL(a) ((ScmObj)(SCM_STRIP_GCBIT(SCM_CELL_CDR(a))))
#define SCM_CONS_SET_CAR_VAL(a, val)                            \
    SCM_CELL_SET_CAR((a), SCM_STRIP_GCBIT(val) | SCM_GCBIT(SCM_CELL_CAR(a)))
#define SCM_CONS_SET_CDR_VAL(a, val)                            \
    SCM_CELL_SET_CDR((a), SCM_STRIP_GCBIT(val) | SCM_GCBIT(SCM_CELL_CDR(a)))

/*
 * for Closure
 */
#define SCM_CLOSURE_CAR_VAL(a) ((ScmObj)(SCM_STRIP_GCBIT(SCM_CELL_CAR(a))))
#define SCM_CLOSURE_CDR_VAL(a) ((ScmObj)(SCM_STRIP_GCBIT(SCM_CELL_CDR(a))))
#define SCM_CLOSURE_SET_CAR_VAL(a, val)                         \
    SCM_CELL_SET_CAR((a), SCM_STRIP_GCBIT(val) | SCM_GCBIT(SCM_CELL_CAR(a)))
#define SCM_CLOSURE_SET_CDR_VAL(a, val)                         \
    SCM_CELL_SET_CDR((a), SCM_STRIP_GCBIT(val) | SCM_GCBIT(SCM_CELL_CDR(a)))

/*
 * for Others
 */
/* car */
#define SCM_OTHERS_CAR_VAL_ALIGN_NONEP(type)    \
    (SCM_OTHERS_CAR_VAL_ALIGNMENT_##type == SCM_ALIGN_NONE)
#define SCM_OTHERS_CDR_CARLSB_VAL_OFFSET(type)  \
    (SCM_OTHERS_CDR_VAL_OFFSET_##type)
#define SCM_OTHERS_CDR_CARLSB_VAL_MASK(type)    \
    (0x1 << SCM_OTHERS_CDR_CARLSB_VAL_OFFSET(type))
#define SCM_OTHERS_CDR_CARLSB_VAL(a, type)                              \
    ((SCM_CAST_UINT(SCM_CELL_CDR(a)) >> SCM_OTHERS_CDR_CARLSB_VAL_OFFSET(type)) & 0x1)

#define SCM_OTHERS_CAR_VAL(a, type)                             \
    ((SCM_OTHERS_CAR_VAL_ALIGN_NONEP(type))                     \
     ? ((ScmObj)((SCM_STRIP_GCBIT(SCM_CELL_CAR(a)))             \
                 | SCM_OTHERS_CDR_CARLSB_VAL((a), type)))       \
     : ((ScmObj)((SCM_STRIP_GCBIT(SCM_CELL_CAR(a))))))

#define SCM_OTHERS_SET_CAR_VAL(a, type, val)                            \
    do {                                                                \
        SCM_CELL_SET_CAR((a), (SCM_STRIP_GCBIT(val)                     \
                               | SCM_GCBIT(SCM_CELL_CAR(a))));          \
                                                                        \
        if (SCM_OTHERS_CAR_VAL_ALIGN_NONEP(type)) {                     \
            /* store val's GCBIT to the CDR */                          \
            SCM_CELL_SET_CDR((a),                                       \
                             ((SCM_CAST_UINT(SCM_CELL_CDR(a))           \
                               & ~SCM_OTHERS_CDR_CARLSB_VAL_MASK(type)) \
                              | (SCM_GCBIT(val)                         \
                                 << SCM_OTHERS_CDR_VAL_OFFSET_##type))); \
        }                                                               \
    } while (/*CONSTCOND*/ 0)

/* cdr */
#define SCM_OTHERS_CDR_TAGGING(a, type, val)                            \
    ((SCM_OTHERS_CAR_VAL_ALIGN_NONEP(type))                             \
     ? ((SCM_CAST_UINT(val) << (SCM_OTHERS_CDR_VAL_OFFSET_##type + 1))  \
        | (SCM_CAST_UINT(SCM_CELL_CDR(a))                               \
           & (~SCM_OTHERS_CDR_VAL_MASK_##type                           \
              | SCM_OTHERS_CDR_CARLSB_VAL_MASK(type))))                 \
     : ((SCM_CAST_UINT(val) << SCM_OTHERS_CDR_VAL_OFFSET_##type)        \
        | (SCM_CAST_UINT(SCM_CELL_CDR(a))                               \
           & (~SCM_OTHERS_CDR_VAL_MASK_##type))))

#define SCM_OTHERS_CDR_VAL(a, type)                                     \
    ((SCM_CAST_UINT(SCM_CELL_CDR(a)) & SCM_OTHERS_CDR_VAL_MASK_##type)  \
     >> ((SCM_OTHERS_CAR_VAL_ALIGN_NONEP(type))                         \
         ? (SCM_OTHERS_CDR_VAL_OFFSET_##type + 1)                       \
         : (SCM_OTHERS_CDR_VAL_OFFSET_##type)))
#define SCM_OTHERS_SET_CDR_VAL(a, type, val)                            \
    SCM_CELL_SET_CDR((a), SCM_OTHERS_CDR_TAGGING((a), type, (val)))

/*
 * for Immediates
 */
#define SCM_IMM_TAGGING(a, type, val)                                   \
    (SCM_CAST_UINT(val) | SCM_IMM_TAG_##type | SCM_GCBIT(a))
/* we need to 'signed' bit shifting for integer, so cast to 'int', not
 * 'unsigned int' */
#define SCM_IMM_VAL(a, type)                                            \
    ((int)((SCM_CAST_UINT(a) & SCM_IMM_VAL_MASK_##type))                \
     >> SCM_IMM_VAL_OFFSET_##type)
#define SCM_IMM_SET_VAL(a, type, val)                                   \
    ((a) = (ScmObj)(SCM_IMM_TAGGING((a),                                \
                                    type,                               \
                                    (((int)(val))                       \
                                     << SCM_IMM_VAL_OFFSET_##type))))

/*==============================================================================
                               SAL Macros
==============================================================================*/
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
#define SCM_SAL_MAKE_VALUEPACKET              scm_make_value_packet

/* Don't use these functions directly. Use SCM_MAKE_*() or MAKE_*() instead to
 * allow flexible object allocation. */
ScmObj scm_make_cons(ScmObj kar, ScmObj kdr);
#if 1
/* FIXME: directly create by SCM_SAL_MAKE_*() */
ScmObj scm_make_int(int val);
ScmObj scm_make_char(int val);
#endif
ScmObj scm_make_symbol(char *name, ScmObj val);
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
  Type Predicates
=======================================*/
#define SCM_SAL_CONSP(a)          SCM_TAG_CONSP(a)
#define SCM_SAL_CLOSUREP(a)       SCM_TAG_CLOSUREP(a)
/* Others */
#define SCM_SAL_SYMBOLP(a)        SCM_TAG_OTHERS_TYPEP((a), SYMBOL)
#define SCM_SAL_STRINGP(a)        SCM_TAG_OTHERS_TYPEP((a), STRING)
#define SCM_SAL_VECTORP(a)        SCM_TAG_OTHERS_TYPEP((a), VECTOR)
#define SCM_SAL_VALUEPACKETP(a)   SCM_TAG_OTHERS_TYPEP((a), VALUES)
#define SCM_SAL_FUNCP(a)          SCM_TAG_OTHERS_TYPEP((a), FUNC)
#define SCM_SAL_PORTP(a)          SCM_TAG_OTHERS_TYPEP((a), PORT)
#define SCM_SAL_CONTINUATIONP(a)  SCM_TAG_OTHERS_TYPEP((a), CONTINUATION)
#define SCM_SAL_C_POINTERP(a)     SCM_TAG_OTHERS_TYPEP((a), C_POINTER)
#define SCM_SAL_C_FUNCPOINTERP(a) SCM_TAG_OTHERS_TYPEP((a), C_FUNCPOINTER)
#define SCM_SAL_FREECELLP(a)      SCM_TAG_OTHERS_TYPEP((a), FREECELL)
/* Immediates */
#define SCM_SAL_INTP(a)           SCM_TAG_IMM_TYPEP((a), INT)
#define SCM_SAL_CHARP(a)          SCM_TAG_IMM_TYPEP((a), CHAR)
#define SCM_SAL_CONSTANTP(a)      SCM_TAG_IMM_TYPEP((a), CONSTANT)

/*=======================================
   Entyping Macros
=======================================*/
#define SCM_ENTYPE_TAG(a, tag) \
    ((a) = (ScmObj)(SCM_CAST_UINT(SCM_STRIP_TAG(a)) | (tag)))
#define SCM_ENTYPE_OTHERS_CDR_TAG(a, tag) \
    SCM_CELL_SET_CDR((a), (tag))

/* for each tag type */
#define SCM_ENTYPE_TAG_CONS(a)                  \
    do {                                        \
        SCM_ENTYPE_TAG((a), SCM_TAG_CONS);      \
        SCM_DO_UNMARK(a);                       \
        SCM_GCBIT_OFF(SCM_CELL_CDR(a));         \
    } while (/*CONSTCOND*/ 0)

#define SCM_ENTYPE_TAG_CLOSURE(a)               \
    do {                                        \
        SCM_ENTYPE_TAG((a), SCM_TAG_CLOSURE);   \
        SCM_DO_UNMARK(a);                       \
        SCM_GCBIT_OFF(SCM_CELL_CDR(a));         \
    } while (/*CONSTCOND*/ 0)

#define SCM_ENTYPE_TAG_OTHERS(a, type)                                  \
    do {                                                                \
        SCM_ENTYPE_TAG((a), SCM_TAG_OTHERS);                            \
        SCM_DO_UNMARK(a);                                               \
        SCM_ENTYPE_OTHERS_CDR_TAG((a), SCM_OTHERS_CDR_TAG_##type);      \
    } while (/*CONSTCOND*/ 0)

#define SCM_ENTYPE_TAG_IMM(a, type)             \
    ((a) = (ScmObj)(SCM_IMM_TAG_##type))

/* for each scheme object type */
#define SCM_SAL_ENTYPE_CONS(a)          SCM_ENTYPE_TAG_CONS(a)
#define SCM_SAL_ENTYPE_CLOSURE(a)       SCM_ENTYPE_TAG_CLOSURE(a)
#define SCM_SAL_ENTYPE_SYMBOL(a)        SCM_ENTYPE_TAG_OTHERS((a), SYMBOL)
#define SCM_SAL_ENTYPE_STRING(a)        SCM_ENTYPE_TAG_OTHERS((a), STRING)
#define SCM_SAL_ENTYPE_VECTOR(a)        SCM_ENTYPE_TAG_OTHERS((a), VECTOR)
#define SCM_SAL_ENTYPE_VALUEPACKET(a)   SCM_ENTYPE_TAG_OTHERS((a), VALUES)
#define SCM_SAL_ENTYPE_FUNC(a)          SCM_ENTYPE_TAG_OTHERS((a), FUNC)
#define SCM_SAL_ENTYPE_PORT(a)          SCM_ENTYPE_TAG_OTHERS((a), PORT)
#define SCM_SAL_ENTYPE_CONTINUATION(a)  SCM_ENTYPE_TAG_OTHERS((a), CONTINUATION)
#define SCM_SAL_ENTYPE_C_POINTER(a)     SCM_ENTYPE_TAG_OTHERS((a), C_POINTER)
#define SCM_SAL_ENTYPE_C_FUNCPOINTER(a) SCM_ENTYPE_TAG_OTHERS((a), C_FUNCPOINTER)
#define SCM_SAL_ENTYPE_FREECELL(a)      SCM_ENTYPE_TAG_OTHERS((a), FREECELL)
#define SCM_SAL_ENTYPE_INT(a)           SCM_ENTYPE_TAG_IMM((a), INT)
#define SCM_SAL_ENTYPE_CHAR(a)          SCM_ENTYPE_TAG_IMM((a), CHAR)

/*=======================================
  Accessors For Scheme Objects
=======================================*/
/* ScmObj Global Attribute */
#define SCM_SAL_TYPE(a) scm_type(a)
extern enum ScmObjType scm_type(ScmObj obj);

/*==============================================================================
  Accessors For Scheme Objects : Cons
==============================================================================*/
#define SCM_SAL_CONS_CAR(a) ((ScmObj)(SCM_CONS_CAR_VAL(a)))
#define SCM_SAL_CONS_CDR(a) ((ScmObj)(SCM_CONS_CDR_VAL(a)))
#define SCM_SAL_CONS_SET_CAR(a, val) SCM_CONS_SET_CAR_VAL((a), (val))
#define SCM_SAL_CONS_SET_CDR(a, val) SCM_CONS_SET_CDR_VAL((a), (val))

/*==============================================================================
  Accessors For Scheme Objects : Closure
==============================================================================*/
#define SCM_SAL_CLOSURE_EXP(a) ((ScmObj)(SCM_CLOSURE_CAR_VAL(a)))
#define SCM_SAL_CLOSURE_ENV(a) ((ScmObj)(SCM_CLOSURE_CDR_VAL(a)))
#define SCM_SAL_CLOSURE_SET_EXP(a, val) SCM_CLOSURE_SET_CAR_VAL((a), (val))
#define SCM_SAL_CLOSURE_SET_ENV(a, val) SCM_CLOSURE_SET_CDR_VAL((a), (val))

/*==============================================================================
  Accessors For Scheme Objects : Others
==============================================================================*/
/*
 * Symbol
 */
#define SCM_SAL_SYMBOL_VCELL(a) ((ScmObj)SCM_OTHERS_CAR_VAL((a), SYMBOL))
#define SCM_SAL_SYMBOL_NAME(a)  ((char*) SCM_OTHERS_CDR_VAL((a), SYMBOL))
#define SCM_SAL_SYMBOL_SET_VCELL(a, val)        \
    SCM_OTHERS_SET_CAR_VAL((a), SYMBOL, (val))
#define SCM_SAL_SYMBOL_SET_NAME(a, val)         \
    SCM_OTHERS_SET_CDR_VAL((a), SYMBOL, (val))

/*
 * String
 *
 * Note: LSB of car is used to store mutation type and called 'MUTATIONBIT'.
 */
#define SCM_OTHERS_CAR_STRING_MUTATIONBIT_OFFSET        \
    SCM_OTHERS_CAR_VAL_OFFSET_STRING
#define SCM_OTHERS_CAR_STRING_MUTATIONBIT_MASK          \
    (0x1 << SCM_OTHERS_CAR_STRING_MUTATIONBIT_OFFSET)
#define SCM_OTHERS_CAR_STRING_MUTATIONBIT(a)                            \
    (SCM_CAST_UINT(SCM_CELL_CAR(a)) & SCM_OTHERS_CAR_STRING_MUTATIONBIT_MASK)
#define SCM_OTHERS_STRING_STRIP_MUTATIONBIT(a)                          \
    (SCM_CAST_UINT(a) & ~SCM_OTHERS_CAR_STRING_MUTATIONBIT_MASK)

#define SCM_STRING_MUTATION_TYPE(a)                                     \
    ((enum ScmStrMutationType)(SCM_OTHERS_CAR_STRING_MUTATIONBIT(a)     \
                               >> SCM_OTHERS_CAR_STRING_MUTATIONBIT_OFFSET))
#define SCM_STRING_SET_MUTATION_TYPE(a, type)                           \
    SCM_OTHERS_SET_CAR_VAL((a),                                         \
                           STRING,                                      \
                           SCM_OTHERS_STRING_STRIP_MUTATIONBIT(SCM_OTHERS_CAR_VAL((a), STRING)) \
                           | ((type)                                    \
                              << SCM_OTHERS_CAR_STRING_MUTATIONBIT_OFFSET))

#define SCM_SAL_STRING_MUTABLEP(a)                      \
    (SCM_STRING_MUTATION_TYPE(a) == SCM_STR_MUTABLE)
#define SCM_SAL_STRING_SET_MUTABLE(a)                   \
    SCM_STRING_SET_MUTATION_TYPE((a), SCM_STR_MUTABLE)
#define SCM_SAL_STRING_SET_IMMUTABLE(a)                 \
    SCM_STRING_SET_MUTATION_TYPE((a), SCM_STR_IMMUTABLE)
#define SCM_SAL_STRING_STR(a)                                           \
    ((char*)SCM_OTHERS_STRING_STRIP_MUTATIONBIT(SCM_OTHERS_CAR_VAL((a), STRING)))
#define SCM_SAL_STRING_SET_STR(a, val)                                  \
    SCM_OTHERS_SET_CAR_VAL((a),                                         \
                           STRING,                                      \
                           SCM_CAST_UINT(val)                           \
                           | SCM_OTHERS_CAR_STRING_MUTATIONBIT(a))

#define SCM_SAL_STRING_LEN(a)                   \
    ((int)SCM_OTHERS_CDR_VAL((a), STRING))
#define SCM_SAL_STRING_SET_LEN(a, val)          \
    SCM_OTHERS_SET_CDR_VAL((a), STRING, (val))

/*
 * Vector
 */
#define SCM_SAL_VECTOR_VEC(a) ((ScmObj*)SCM_OTHERS_CAR_VAL((a), VECTOR))
#define SCM_SAL_VECTOR_LEN(a) ((int)SCM_OTHERS_CDR_VAL((a), VECTOR))
#define SCM_SAL_VECTOR_SET_VEC(a, val)          \
    SCM_OTHERS_SET_CAR_VAL((a), VECTOR, (val))
#define SCM_SAL_VECTOR_SET_LEN(a, val)          \
    SCM_OTHERS_SET_CDR_VAL((a), VECTOR, (val))
#define SCM_SAL_VECTOR_VALID_INDEXP(o, i) (0 <= (i) && (i) < SCM_VECTOR_LEN(o))

/*
 * ValuePacket
 */
#define SCM_SAL_VALUEPACKET_VALUES(a)           \
    ((ScmObj)SCM_OTHERS_CAR_VAL((a), VALUES))
#define SCM_SAL_VALUEPACKET_SET_VALUES(a, val)  \
    SCM_OTHERS_SET_CAR_VAL((a), VALUES, (val))

/*
 * Func
 */
#define SCM_SAL_FUNC_CFUNC(a)                   \
    (SCM_WORD_CAST(ScmFuncType, SCM_OTHERS_CAR_VAL((a), FUNC)))
#define SCM_SAL_FUNC_SET_CFUNC(a, val)          \
    SCM_OTHERS_SET_CAR_VAL((a), FUNC, (val))

#define SCM_SAL_FUNC_TYPECODE(a)                \
    ((enum ScmFuncTypeCode)SCM_OTHERS_CDR_VAL((a), FUNC))
#define SCM_SAL_FUNC_SET_TYPECODE(a, val)       \
    SCM_OTHERS_SET_CDR_VAL((a), FUNC, (val))

/*
 * Port
 */
#define SCM_SAL_PORT_IMPL(a) ((ScmCharPort*)    SCM_OTHERS_CAR_VAL((a), PORT))
#define SCM_SAL_PORT_FLAG(a) ((enum ScmPortFlag)SCM_OTHERS_CDR_VAL((a), PORT))
#define SCM_SAL_PORT_SET_IMPL(a, val) SCM_OTHERS_SET_CAR_VAL((a), PORT, (val))
#define SCM_SAL_PORT_SET_FLAG(a, val) SCM_OTHERS_SET_CDR_VAL((a), PORT, (val))

/*
 * Continuation
 */
#define SCM_SAL_CONTINUATION_OPAQUE(a)          \
    ((void*)SCM_OTHERS_CAR_VAL((a), CONTINUATION))
#define SCM_SAL_CONTINUATION_TAG(a)             \
    ((int)  SCM_OTHERS_CDR_VAL((a), CONTINUATION))
#define SCM_SAL_CONTINUATION_SET_OPAQUE(a, val) \
    SCM_OTHERS_SET_CAR_VAL((a), CONTINUATION, (val))
#define SCM_SAL_CONTINUATION_SET_TAG(a, val)    \
    SCM_OTHERS_SET_CDR_VAL((a), CONTINUATION, (val))

/*
 * CPointer
 */
#define SCM_SAL_C_POINTER_VALUE(a)              \
    ((void*)SCM_OTHERS_CAR_VAL((a), C_POINTER))
#define SCM_SAL_C_POINTER_SET_VALUE(a, val)     \
    SCM_OTHERS_SET_CAR_VAL((a), C_POINTER, (val))

/*
 * CFuncPointer
 */
#define SCM_SAL_C_FUNCPOINTER_VALUE(a)                  \
    (SCM_WORD_CAST(ScmCFunc, SCM_OTHERS_CAR_VAL((a), C_FUNCPOINTER)))
#define SCM_SAL_C_FUNCPOINTER_SET_VALUE(a, val)         \
    SCM_OTHERS_SET_CAR_VAL((a), C_FUNCPOINTER, (val))

/*
 * FreeCell
 */
#define SCM_SAL_FREECELL_NEXT(a)                \
    ((ScmObj)SCM_OTHERS_CAR_VAL((a), FREECELL))
#define SCM_SAL_FREECELL_SET_NEXT(a, val)       \
    SCM_OTHERS_SET_CAR_VAL((a), FREECELL, (val))

/*==============================================================================
  Accessors For Scheme Objects : Immediates
==============================================================================*/
/* Int */
#define SCM_SAL_INT_VALUE(a)          ((int)(SCM_IMM_VAL((a), INT)))
#define SCM_SAL_INT_SET_VALUE(a, val) SCM_IMM_SET_VAL((a), INT, (val))

/* Char */
#define SCM_SAL_CHAR_VALUE(a)         ((int)(SCM_IMM_VAL((a), CHAR)))
#define SCM_SAL_CHAR_SET_VALUE(a, ch) SCM_IMM_SET_VAL((a), CHAR, (ch))

/*============================================================================
  GC Related Operations
============================================================================*/
#define SCM_SAL_RECLAIM_CELL(cell, next)                                     \
    do {                                                                     \
        SCM_ENTYPE_FREECELL(cell);                                           \
        SCM_DO_UNMARK(cell);                                                 \
        SCM_FREECELL_SET_NEXT((cell), (next));                               \
    } while (/* CONSTCOND */ 0)

#define SCM_CANBE_MARKED(a)   (SCM_TAG(a) != SCM_TAG_IMM)
#define SCM_STRIP_TAG_INFO(a) (SCM_STRIP_TAG(a))

/* When we sweep the object, we have no type information because the pointer is
 * not tagged (raw pointer to heap). So, we see the S->cdr's GC bit and its
 * value is 1, the object contains the pointer to be freed. */
#define SCM_NEED_SWEEPP(a)               (SCM_GCBIT(SCM_CELL_CDR(a)))
/* To pass the normal tag check, directly see the S->cdr's tag */
#define SCM_TAG_SWEEP_PHASE_OTHERSP(a, type)                           \
    ((SCM_CAST_UINT(SCM_CELL_CDR(a)) & SCM_OTHERS_CDR_TAG_MASK_##type) \
      == SCM_OTHERS_CDR_TAG_##type)
#define SCM_SWEEP_PHASE_SYMBOLP(a)              \
    (SCM_TAG_SWEEP_PHASE_OTHERSP((a), SYMBOL))
#define SCM_SWEEP_PHASE_STRINGP(a)              \
    (SCM_TAG_SWEEP_PHASE_OTHERSP((a), STRING))
#define SCM_SWEEP_PHASE_VECTORP(a)              \
    (SCM_TAG_SWEEP_PHASE_OTHERSP((a), VECTOR))
#define SCM_SWEEP_PHASE_PORTP(a)                \
    (SCM_TAG_SWEEP_PHASE_OTHERSP((a), PORT))
#define SCM_SWEEP_PHASE_CONTINUATIONP(a)        \
    (SCM_TAG_SWEEP_PHASE_OTHERSP((a), CONTINUATION))

#define SCM_SAL_IS_MARKED(a)                    \
    (SCM_GCBIT(SCM_CELL_CAR(a)) == SCM_GCBIT_MARKED)
#define SCM_SAL_IS_UNMARKED(a)                  \
    (!SCM_IS_MARKED(a))
#define SCM_SAL_DO_MARK(a)                                              \
    (SCM_CELL_SET_CAR((a),                                              \
                      SCM_STRIP_GCBIT(SCM_CELL_CAR(a)) | SCM_GCBIT_MARKED))
#define SCM_SAL_DO_UNMARK(a)                                            \
    (SCM_CELL_SET_CAR((a),                                              \
                      SCM_STRIP_GCBIT(SCM_CELL_CAR(a)) | SCM_GCBIT_UNMARKED))

/*============================================================================
  Environment Specifiers
============================================================================*/
#define SCM_SAL_INTERACTION_ENV SCM_NULL
/*
 * Current implementation cannot handle scheme-report-environment and
 * null-environment properly. Be careful to use these environemnts.
 */
#define SCM_SAL_R5RS_ENV  SCM_INTERACTION_ENV
#define SCM_SAL_NULL_ENV  SCM_INTERACTION_ENV

#define SCM_SAL_ENVP(env) (NULLP(env) || CONSP(env))

/*============================================================================
  Abstract ScmObj Reference For Storage-Representation Independent Efficient
  List Operations
============================================================================*/
#define SCM_SAL_INVALID_REF NULL

#define SCM_SAL_REF_CAR(cons)     (&SCM_CELL_CAR(cons))
#define SCM_SAL_REF_CDR(cons)     (&SCM_CELL_CDR(cons))
#define SCM_SAL_REF_OFF_HEAP(obj) (&(obj))

/* SCM_DEREF(ref) is not permitted to be used as lvalue */
#define SCM_SAL_DEREF(ref) ((ScmObj)(SCM_CAST_UINT(*(ref))))

/* RFC: Is there a better name? */
#define SCM_SAL_SET(ref, obj) \
    (*(ref) = (ScmObj)(SCM_GCBIT(*(ref)) | SCM_STRIP_GCBIT(obj)))

/*============================================================================
  Special Constants and Predicates
============================================================================*/
#define SCM_SAL_NULL    ((ScmObj)(SCM_IMM_TAG_NULL))
#define SCM_SAL_EOF     ((ScmObj)(SCM_IMM_TAG_EOF))
#define SCM_SAL_UNDEF   ((ScmObj)(SCM_IMM_TAG_UNDEF))
#define SCM_SAL_INVALID ((ScmObj)(SCM_IMM_TAG_INVALID))
#define SCM_SAL_UNBOUND ((ScmObj)(SCM_IMM_TAG_UNBOUND))
#define SCM_SAL_FALSE   ((ScmObj)(SCM_IMM_TAG_FALSE))
#define SCM_SAL_TRUE    ((ScmObj)(SCM_IMM_TAG_TRUE))

#define SCM_SAL_EQ(a, b) (SCM_STRIP_GCBIT(a) == SCM_STRIP_GCBIT(b))

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

#endif /* __STORAGE_COMPACT_H */
