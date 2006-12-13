/*===========================================================================
 *  Filename : storage-compact.h
 *  About    : Storage abstraction (compact representation)
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
#ifndef __STORAGE_COMPACT_H
#define __STORAGE_COMPACT_H

/*
 * Internal representation defined in this file MUST NOT directly touched by
 * libsscm users. Use abstract public APIs defined in sigscheme.h.
 */

/*
 * Object Representation
 *
 * In following descriptions, we represent that ScmObj "S" points to a ScmCell
 * on the heap which contains two ScmObj field "X" and "Y" (suppose S = &{ X, Y
 * }).
 *
 * (0) LSB of "S" is called G-bit. And bit 1..2 of S is called 'primary tag',
 *     which roughly distinguishes the type of the object as follows.
 *
 *           S      |      Type        | content of remainder bits
 *     -------------+------------------+---------------------------
 *     .......|00|G : cons cell (pair) : pointer to the cell
 *     .......|01|G : closure          : pointer to the cell
 *     .......|10|G : 'misc' object    : pointer to the cell
 *     .......|11|G : immediate        : value
 *
 * (1) If S == "...00G", S points to a cons cell (pair). G-bit of S->X is used
 *     as the GC mark bit. And G bit of S->Y is always set to 0, to help
 *     determining its own type without the pointer S on the object
 *     finalization.
 *
 *          S->X     |     Type     |             content of S->X
 *     --------------+--------------+------------------------------------------
 *     ...........|G : cons cell    : car (ScmObj)
 *
 *          S->Y     |     Type     |             content of S->Y
 *     --------------+--------------+------------------------------------------
 *     ...........|0 : cons cell    : cdr (ScmObj)
 *
 * (2) If S == "...01G", S points to a closure. G-bit of S->X is used as the GC
 *     mark bit. And G bit of S->Y is always set to 0, to help determining its
 *     own type without the pointer S on the object finalization.
 *
 *          S->X     |     Type     |             content of S->X
 *     --------------+--------------+------------------------------------------
 *     ...........|G : closure      : exp (ScmObj)
 *
 *          S->Y     |     Type     |             content of S->Y
 *     --------------+--------------+------------------------------------------
 *     ...........|0 : closure      : env (ScmObj)
 *
 * (3) If S == "...10G", S points to a 'miscellaneous' object. Its particular
 *     type is determined by the value of some lower bits of S->Y. G-bit of
 *     S->X is used as the GC mark bit. And G bit of S->Y is always set to 1,
 *     to help determining its own type without the pointer S on the object
 *     finalization.
 *
 *          S->X     |     Type     |             content of S->X
 *     --------------+--------------+------------------------------------------
 *     ...........|G : symbol       : symbol value (ScmObj)
 *     ...........|G : string       : C string (char *)
 *     ...........|G : vector       : vector objects (ScmObj *)
 *     ...........|G : valuepacket  : values list (ScmObj)
 *     ...........|G : func         : function pointer (LSB is stored in S->Y)
 *     ...........|G : port         : char port instance (ScmCharPort *)
 *     ...........|G : continuation : opaque (void *)
 *     ...........|G : pointer
 *     ...........|G :  - C ptr     : pointer (void *)
 *     ...........|G :  - C funcptr : function pointer (ScmCFunc)
 *     ...........|G : wrapper      : abstract obj (ScmObj)
 *     ...........|G :  - subpat    : object (ScmObj)
 *     ...........|G :  - far symbol: symbol (ScmObj)
 *     ...........|G :  - macro     : rules (ScmObj)
 *     ...........|G : freecell     : next cell (ScmObj)
 *
 *          S->Y     |     Type     |             content of S->Y
 *     --------------+--------------+------------------------------------------
 *     ........|00|1 : symbol       : symbol name (char *)
 *     .......M|01|1 : string       : string length, 'mutable' bit M
 *     .......M|10|1 : vector       : vector length, 'mutable' bit M
 *     ....|000|11|1 : valuepacket  : unused (all 0 for efficiency)
 *     ...P|001|11|1 : func         : type code, LSB P of the pointer (S->X)
 *     ....|010|11|1 : port         : flags (enum ScmPortFlag)
 *     ....|011|11|1 : continuation : tag (scm_int_t)
 *     ....|100|11|1 : pointer
 *     P|00|100|11|1 :  - C ptr     : LSB P of the pointer (S->X)
 *     P|01|100|11|1 :  - C funcptr : LSB P of the pointer (S->X)
 *     .|10|100|11|1 :  - (reserved):
 *     .|11|100|11|1 :  - (reserved):
 *     ....|101|11|1 : wrapper      : inaccessible
 *     .|00|101|11|1 :  - subpat    : metainformation about the wrapped object
 *     .|01|101|11|1 :  - far symbol: [#if !SCM_USE_UNHYGIENIC_MACRO] env depth
 *     .|10|101|11|1 :  - macro     : [#if !SCM_USE_UNHYGIENIC_MACRO] env depth
 *     .|11|101|11|1 :  - (reserved):
 *     ....|110|11|1 : (reserved)   :
 *     ....|111|11|1 : freecell     : unused (all 0)
 *
 *     Misc. types' tags come in several levels, including the GC bit:
 *
 *     .|..|...|ZZ|Z : level 1
 *     .|..|ZZZ|ZZ|Z : level 2
 *     .|ZZ|ZZZ|ZZ|Z : level 3
 *
 *     Required data aligments:
 *
 *       symbol
 *           name (char *)        : 8 byte (S->Y)
 *       string
 *           str (char *)         : 2 byte (S->X)
 *       vector
 *           vec (ScmObj *)       : 2 byte (S->X)
 *       port
 *           impl (ScmCharPort *) : 2 byte (S->X)
 *       continuation
 *           opaque (void *)      : 2 byte (S->X)
 *       func
 *           ptr (ScmFuncType)    : 1 byte (S->X)
 *       C ptr
 *           value (void *)       : 1 byte (S->X)
 *       C funcptr
 *           value (ScmCFunc)     : 1 byte (S->X)
 *
 * (4) If S == "...11G", S is an immediate value. Immediate values are
 *     separated into these subtypes by the value of bit 3..7 of S.
 *
 *           S      |   Type
 *     -------------+------------
 *     ......0|11|G : integer
 *     .....01|11|G : char
 *     .....11|11|G : constant
 *     .000|11|11|G :  - ()
 *     .001|11|11|G :  - INVALID
 *     .010|11|11|G :  - UNBOUND
 *     .011|11|11|G :  - #f
 *     .100|11|11|G :  - #t
 *     .101|11|11|G :  - EOF
 *     .110|11|11|G :  - UNDEF
 *
 */

#include <limits.h>
#include <stddef.h>
#include <stdlib.h>

/* Don't include scmport.h. The implementations are internal and should not be
 * exposed to libsscm users via installation of this file. */

#ifdef __cplusplus
/* extern "C" { */
#endif


/* Aux. */
#define SCM_MAKE_MASK(offset, width)                                         \
    (((scm_uintobj_t)1 << ((offset) + (width)))                              \
     - ((scm_uintobj_t)1 << (offset)))

#define SCM_SIGNED_TYPEP(t) ((t)(-1) < (t)0)
#define SCM_SIGN_BIT(x) ((x)                                                 \
                         & ((scm_uintobj_t)1 << (sizeof(x) * CHAR_BIT - 1)))

#if HAVE_ARITHMETIC_RSHIFT
#define SCM_ARSHIFT(x, n)    ((scm_uintobj_t)((scm_intobj_t)(x) >> (n)))
#else  /* not HAVE_ARITHMETIC_RSHIFT */
/* Emulate a right arithmetic shift. */
#define SCM_ARSHIFT(x, n)                                       \
   (((scm_uintobj_t)(x) >> (n)) | -(SCM_SIGN_BIT(x) >> (n)))
#endif /* not HAVE_ARITHMETIC_RSHIFT */


/* ------------------------------------------------------------
 * Crude representation.
 */

typedef struct ScmCell_ ScmCell;

/* Note that this is unsigned.  Signed operations are desirable only
 * in a few, specific cases. */
typedef scm_uintobj_t  ScmObj;
#define ALIGNOF_SCMOBJ ALIGNOF_SCM_INTOBJ_T
#define SIZEOF_SCMOBJ  SIZEOF_SCM_INTOBJ_T

struct ScmCell_ {
    /* The field names have some redundancy to avoid conflict with
     * macros' formal arguments and stuff. */
    ScmObj obj_x;
    ScmObj obj_y;
};

typedef ScmObj (*ScmFuncType)();

/* ScmObj = .....|PP|G
 * G = GC bit
 * P = Primary tag (ptag)
 */

#define SCM_GCBIT_OFFSET     0  /* More or less hardcoded. */
#define SCM_GCBIT_WIDTH      1
#define SCM_GCBIT_MASK       SCM_MAKE_MASK(SCM_GCBIT_OFFSET, SCM_GCBIT_WIDTH)
#define SCM_GCBIT(o)         ((o) & SCM_GCBIT_MASK)
#define SCM_GCBIT_MARKED     1  /* More or less hardcoded. */
#define SCM_GCBIT_UNMARKED   0  /* Ditto. */

#define SCM_PTAG_OFFSET      (SCM_GCBIT_WIDTH + SCM_GCBIT_OFFSET)
#define SCM_PTAG_WIDTH       2
#define SCM_PTAG_MASK        SCM_MAKE_MASK(SCM_PTAG_OFFSET, SCM_PTAG_WIDTH)
#define SCM_MAKE_PTAG(id)    ((scm_uintobj_t)(id) << SCM_PTAG_OFFSET)
#define SCM_PTAG(o)          ((o) & SCM_PTAG_MASK)
#define SCM_PTAG_SET(o, tag) ((o) = ((o) & ~SCM_PTAG_MASK) | (tag))

#define SCM_DROP_PTAG(o)     ((o) & ~SCM_PTAG_MASK)
#define SCM_DROP_GCBIT(o)    ((o) & ~SCM_GCBIT_MASK)
#define SCM_DROP_TAG(o)      ((o) & ~(SCM_GCBIT_MASK | SCM_PTAG_MASK))

#define SCM_UNTAGGEDP(o)     (!((o) & (SCM_GCBIT_MASK | SCM_PTAG_MASK)))

#define SCM_UNTAG_PTR(o)     ((ScmCell *)SCM_DROP_TAG(o))

/* Raw accessors. */
#define SCM_PTR(o)      ((ScmCell *)(o))
#define SCM_X(o)        (SCM_PTR(o)->obj_x)
#define SCM_Y(o)        (SCM_PTR(o)->obj_y)
#define SCM_SET_X(o, x) (SCM_X(o) = (x))
#define SCM_SET_Y(o, y) (SCM_Y(o) = (y))
#define SCM_INIT(o, x, y, ptag)                 \
    (SCM_SET_X((o), (x)),                       \
     SCM_SET_Y((o), (y)),                       \
     (o) |= (ptag))

#define SCM_SAL_EQ(a, b) ((a) == (b))

/* ------------------------------------------------------------
 * Garbage collection
 */

#define SCM_SAL_MARK(o)                                                  \
    SCM_SET_X(SCM_DROP_TAG(o),                                           \
              SCM_DROP_GCBIT(SCM_X(SCM_DROP_TAG(o))) | SCM_GCBIT_MARKED)

/* O is always untagged, so no need to strip it. */
#define SCM_SAL_UNMARK(o)                                               \
    SCM_SET_X((o), SCM_DROP_GCBIT(SCM_X(o)) | SCM_GCBIT_UNMARKED)
#define SCM_SAL_MARKEDP(o)   (SCM_GCBIT(SCM_X(SCM_DROP_TAG(o)))        \
                              == SCM_GCBIT_MARKED)
#define SCM_SAL_UNMARKEDP(o) (!SCM_GC_MARKEDP(o))

/* See if O's tag and the content of the cell C it references are
 * consistent.  O must be a tagged ScmObj and SCM_DROP_TAG(O) == &C. */
#define SCM_TAG_CONSISTENTP(o, c) (!!SCM_SYMMETRICP(o)          \
                                   != !!SCM_CELL_MISCP(c))

/* ------------------------------------------------------------
 * Symmetric types (both obj_x and obj_y point to some other ScmCell).
 * Pairs and closures are chosen for their prevalence.
 */

/* SCM_TAG_CONSISTENTP() needs this. The PTAG mask value '2' depends on the
 * hardcoded value of SCM_PTAG_{CONS,CLOSURE}. */
#define SCM_SYMMETRICP(o)       (!(SCM_PTAG(o) & SCM_MAKE_PTAG(2)))

/* Pairs.  Immutable pairs are not supported. */
#define SCM_PTAG_CONS                  SCM_MAKE_PTAG(0)  /* hardcoded */
/* Bypass ptag stripping. */
#define SCM_CONS_PTR(o)                SCM_PTR(SCM_AS_CONS(o))

#define SCM_SAL_CONSP(o)               (SCM_PTAG(o) == SCM_PTAG_CONS)
#define SCM_SAL_CONS_CAR(o)            SCM_X(SCM_CONS_PTR(o))
#define SCM_SAL_CONS_CDR(o)            SCM_Y(SCM_CONS_PTR(o))
#define SCM_SAL_CONS_SET_CAR(o, kar)   SCM_SET_X(SCM_CONS_PTR(o), (kar))
#define SCM_SAL_CONS_SET_CDR(o, kdr)   SCM_SET_Y(SCM_CONS_PTR(o), (kdr))
#define SCM_ISAL_CONS_INIT(o, ar, dr)  SCM_INIT((o), (ar), (dr), SCM_PTAG_CONS)
#define SCM_SAL_CONS_MUTABLEP(o)       scm_true
#define SCM_SAL_CONS_SET_MUTABLE(o)    SCM_EMPTY_EXPR
#define SCM_SAL_CONS_SET_IMMUTABLE(o)  SCM_EMPTY_EXPR

/* Closures. */
#define SCM_PTAG_CLOSURE               SCM_MAKE_PTAG(1)  /* hardcoded */
#define SCM_CLOSURE_PTR(o)             SCM_UNTAG_PTR(SCM_AS_CLOSURE(o))

#define SCM_SAL_CLOSUREP(o)            (SCM_PTAG(o) == SCM_PTAG_CLOSURE)
#define SCM_SAL_CLOSURE_EXP(o)         SCM_X(SCM_CLOSURE_PTR(o))
#define SCM_SAL_CLOSURE_ENV(o)         SCM_Y(SCM_CLOSURE_PTR(o))
#define SCM_SAL_CLOSURE_SET_EXP(o, c)  SCM_SET_X(SCM_CLOSURE_PTR(o), (c))
#define SCM_SAL_CLOSURE_SET_ENV(o, e)  SCM_SET_Y(SCM_CLOSURE_PTR(o), (e))
#define SCM_ISAL_CLOSURE_INIT(o, c, e) SCM_INIT((o), (c), (e),          \
                                                SCM_PTAG_CLOSURE)
/* ------------------------------------------------------------
 * Immediate types (ones that fit on the pointer including type tags).
 */

/* Immediate ScmObj = VVVVIIPPG
 * V = Numerical value of the object.
 * I = Immediate type ID; further distinguishes types.  Only 1 bit
 *     wide for integers, 2 bits for others.
 * P = 3 (signature for immediates)
 */
#define SCM_PTAG_IMM                SCM_MAKE_PTAG(3)
#define SCM_IMMP(o)                 (SCM_PTAG(o) == SCM_PTAG_IMM)
#define SCM_IMMID_OFFSET            (SCM_PTAG_OFFSET + SCM_PTAG_WIDTH)
#define SCM_MAKE_IMMID(val)         ((scm_uintobj_t)(val) << SCM_IMMID_OFFSET)
#define SCM_MAKE_ITAG(id)           ((id) | SCM_PTAG_IMM)
#define SCM_MAKE_ITAG_MASK(id_w)    SCM_MAKE_MASK(SCM_PTAG_OFFSET,         \
                                                  (id_w) + SCM_PTAG_WIDTH)
#define SCM_MAKE_VAL_OFFSET(id_w)   (SCM_IMMID_OFFSET + (id_w))

/* Integers. */
#define SCM_IMMID_INT            SCM_MAKE_IMMID(0)
#define SCM_IMMID_WIDTH_INT      1
#define SCM_ITAG_INT             SCM_MAKE_ITAG(SCM_IMMID_INT)
#define SCM_ITAG_MASK_INT        SCM_MAKE_ITAG_MASK(SCM_IMMID_WIDTH_INT)
#define SCM_INT_VAL_OFFSET       (SCM_IMMID_OFFSET + SCM_IMMID_WIDTH_INT)
#define SCM_SAL_INTP(o)          (((o) & SCM_ITAG_MASK_INT) == SCM_ITAG_INT)
#define SCM_SAL_MAKE_INT(i)      ((ScmObj)                                   \
                                  (((scm_intobj_t)(i) << SCM_INT_VAL_OFFSET) \
                                   | SCM_ITAG_INT))
#define SCM_SAL_INT_VALUE(o)     ((scm_int_t)                           \
                                  SCM_ARSHIFT(SCM_AS_INT(o),            \
                                              SCM_INT_VAL_OFFSET))

#define SCM_SAL_NUMBERP          SCM_SAL_INTP

/* Characters. */
#define SCM_IMMID_CHAR          SCM_MAKE_IMMID(1)
#define SCM_IMMID_WIDTH_CHAR    2
#define SCM_ITAG_CHAR           SCM_MAKE_ITAG(SCM_IMMID_CHAR)
#define SCM_ITAG_MASK_CHAR      SCM_MAKE_ITAG_MASK(SCM_IMMID_WIDTH_CHAR)
#define SCM_CHAR_VAL_OFFSET     (SCM_IMMID_OFFSET + SCM_IMMID_WIDTH_CHAR)
#define SCM_SAL_CHARP(o)        (((o) & SCM_ITAG_MASK_CHAR) == SCM_ITAG_CHAR)
#define SCM_SAL_MAKE_CHAR(c)    ((ScmObj)                                     \
                                 (((scm_uintobj_t)(c) << SCM_CHAR_VAL_OFFSET) \
                                  | SCM_ITAG_CHAR))
#define SCM_SAL_CHAR_VALUE(o)   ((scm_ichar_t)                               \
                                 (SCM_AS_CHAR(o) >> SCM_CHAR_VAL_OFFSET))

/* Singleton constants. */
#define SCM_IMMID_CONST         SCM_MAKE_IMMID(3)
#define SCM_IMMID_WIDTH_CONST   2
#define SCM_ITAG_CONST          SCM_MAKE_ITAG(SCM_IMMID_CONST)
#define SCM_ITAG_MASK_CONST     SCM_MAKE_ITAG_MASK(SCM_IMMID_WIDTH_CONST)
#define SCM_CONST_VAL_OFFSET    SCM_MAKE_VAL_OFFSET(SCM_IMMID_WIDTH_CONST)
#define SCM_MAKE_CONST(i)       ((ScmObj)                                    \
                                 ((scm_uintobj_t)(i) << SCM_CONST_VAL_OFFSET \
                                  | SCM_ITAG_CONST))
#define SCM_SAL_CONSTANTP(o)    (((o) & SCM_ITAG_MASK_CONST) == SCM_ITAG_CONST)

#define SCM_SAL_NULL        SCM_MAKE_CONST(0)
#define SCM_SAL_INVALID     SCM_MAKE_CONST(1)
#define SCM_SAL_UNBOUND     SCM_MAKE_CONST(2)
#if SCM_COMPAT_SIOD_BUGS
#define SCM_SAL_FALSE       SCM_SAL_NULL
#else
#define SCM_SAL_FALSE       SCM_MAKE_CONST(3)
#endif
#define SCM_SAL_TRUE        SCM_MAKE_CONST(4)
#define SCM_SAL_EOF         SCM_MAKE_CONST(5)
#define SCM_SAL_UNDEF       SCM_MAKE_CONST(6)


/* ------------------------------------------------------------
 * Miscellaneous types; most refer to one ScmCell or less, or
 * otherwise uncommon enough to warrant the use of a pair to hold the
 * two (perhaps more) ScmObj references.
 */
#define SCM_PTAG_MISC       SCM_MAKE_PTAG(2)
#define SCM_MISCP(o)        (SCM_PTAG(o) == SCM_PTAG_MISC)
#define SCM_MISC_Y_GCBIT    SCM_GCBIT_MARKED
#define SCM_CELL_MISCP(c)   (SCM_GCBIT(SCM_Y(&c)) == SCM_MISC_Y_GCBIT)

/* scmobj_y = ...CC|BBB|AA|G
 * G       = GC bit
 * A,G     = L1 Misc tag bits
 * A,B,G   = L2 Misc tag bits
 * A,B,C,G = L3 Misc tag bits
 * Note that misc tags include the GC bit (which is always 1).
 */
#define SCM_MTAG_OFFSET      SCM_GCBIT_OFFSET
#define SCM_MTAG_L1_WIDTH    (SCM_GCBIT_WIDTH + 2)
#define SCM_MTAG_L2_WIDTH    (SCM_MTAG_L1_WIDTH + 3)
#define SCM_MTAG_L3_WIDTH    (SCM_MTAG_L2_WIDTH + 2)
#define SCM_MTAG_WIDTH(lv)   (((lv) == 1) ? SCM_MTAG_L1_WIDTH : \
                              ((lv) == 2) ? SCM_MTAG_L2_WIDTH : \
                              SCM_MTAG_L3_WIDTH)
#define SCM_MTAG_L1_MASK     SCM_MAKE_MASK(SCM_MTAG_OFFSET, SCM_MTAG_L1_WIDTH)
#define SCM_MTAG_L2_MASK     SCM_MAKE_MASK(SCM_MTAG_OFFSET, SCM_MTAG_L2_WIDTH)
#define SCM_MTAG_L3_MASK     SCM_MAKE_MASK(SCM_MTAG_OFFSET, SCM_MTAG_L3_WIDTH)
#define SCM_MTAG_MASK(lv)    (((lv) == 1) ? SCM_MTAG_L1_MASK :  \
                              ((lv) == 2) ? SCM_MTAG_L2_MASK :  \
                              SCM_MTAG_L3_MASK)

#define SCM_MTAG(o, lv)        (SCM_Y(o) & SCM_MTAG_MASK(lv))
#define SCM_MTAG_SET(o, lv, t) SCM_SET_Y((o),                                 \
                                         (SCM_Y(o) & ~SCM_MTAG_MASK(lv)) | (t))

#define SCM_MAKE_MTAG_L1(t)                                                  \
    (((scm_uintobj_t)(t) << (SCM_MTAG_OFFSET + SCM_GCBIT_WIDTH))             \
     | SCM_MISC_Y_GCBIT)
#define SCM_MAKE_MTAG_L2(t2, t1)                                             \
    (((scm_uintobj_t)(t2) << (SCM_MTAG_OFFSET + SCM_MTAG_L1_WIDTH))          \
     | SCM_MAKE_MTAG_L1(t1))
#define SCM_MAKE_MTAG_L3(t3, t2, t1)                                         \
    (((scm_uintobj_t)(t3) << (SCM_MTAG_OFFSET + SCM_MTAG_L2_WIDTH))          \
     | SCM_MAKE_MTAG_L2((t2), (t1)))


/* Split X at B bits from LSB, store the upper half in obj_x, and
 * multiplex the remainder with obj_y. */
/* result must properly be cast to the original type of x by caller */
#define SCM_MISC_X_SPLITX(o, lv, b)             \
    (SCM_X(o)                                   \
     | ((SCM_Y(o) >> SCM_MTAG_WIDTH(lv))        \
        & SCM_MAKE_MASK(0, (b))))

/* x must properly be extended to sizeof(ScmObj) before this invocation. */
#define SCM_MISC_SET_X_SPLITX(o, x, lv, b)                                \
    (SCM_SET_X((o), (x) & ~SCM_MAKE_MASK(0, (b))),                        \
     SCM_SET_Y((o),                                                       \
               (SCM_Y(o) & ~SCM_MAKE_MASK(SCM_MTAG_WIDTH(lv), (b)))       \
                | (((x) & SCM_MAKE_MASK(0, (b))) << SCM_MTAG_WIDTH(lv))))

/* result must properly be cast to the original type of y by caller */
#define SCM_MISC_Y_SPLITX(o, ytyp, lv, b)                                    \
    SCM_MISC_RSHIFT_Y(SCM_Y(o), ytyp, (SCM_MTAG_WIDTH(lv) + (b)))

/* y must properly be extended to sizeof(ScmObj) before this invocation. */
#define SCM_MISC_SET_Y_SPLITX(o, y, lv, b)                      \
    SCM_SET_Y((o),                                              \
              (SCM_Y(o)                                         \
               & (SCM_MTAG_MASK(lv)                             \
                  | SCM_MAKE_MASK(SCM_MTAG_WIDTH(lv), (b))))    \
              | (y) << (SCM_MTAG_WIDTH(lv) + (b)))

/* x and y must properly be extended to sizeof(ScmObj) before this
 * invocation. */
#define SCM_MISC_INIT_SPLITX(o, x, y, lv, tag, b)                            \
    SCM_INIT((o),                                                            \
             (x) & ~SCM_MAKE_MASK(0, (b)),                                   \
             ((((y) << (b)) | ((x) & SCM_MAKE_MASK(0, (b))))                 \
              << SCM_MTAG_WIDTH(lv))                                         \
             | (tag), SCM_PTAG_MISC)


/* A convenient declarator for misc. subtypes.  This macro covertly
 * defines parameters for the macros defined below.
 *
 * name   - Name of the type in uppercase.  STRING, SYMBOL, etc.
 *
 * lv     - The level "invoked" with tag values.  L2(1, 3) for example.
 *
 * xtype  - The type to be stored in obj_x.  void *, ScmFuncType, etc.
 *
 * xalign - Base-2 logarithm of the minimum alignment guaranteed for
 *          values stored in x.  0 means not aligned (or 1-byte aligned), 2
 *          means 4-byte aligned, and so on.
 *
 * ytype  - The type to be stored in obj_y.
 */
#define SCM_MISC_DECLARE_TYPE(name, lv, xtype, xalign, ytype)                 \
    enum SCM_MISC_##name##_PARAMS {                                           \
        SCM_MISC_##name##_LV = SCM_MISC_LEVEL_##lv,                           \
        SCM_MISC_##name##_X_UNUSED_BITS                                       \
            = (SIZEOF_SCM_INTOBJ_T - sizeof(xtype)) * CHAR_BIT,               \
        SCM_MISC_##name##_XALIGN = (xalign),                                  \
        SCM_MISC_##name##_XSPILL = (SCM_GCBIT_WIDTH - (xalign) < 0)           \
                                   ? 0                                        \
                                   : SCM_GCBIT_WIDTH - (xalign),              \
        SCM_MTAG_##name = SCM_MAKE_MTAG_##lv,                                 \
        SCM_MISC_##name##_XDIRECTP = (SCM_MISC_##name##_XSPILL <= 0),         \
        SCM_MISC_##name##_XSHIFTP = (!SCM_MISC_##name##_XDIRECTP              \
                                     && (SCM_MISC_##name##_XSPILL             \
                                         < SCM_MISC_##name##_X_UNUSED_BITS)), \
        SCM_MISC_##name##_XSPLITP = !(SCM_MISC_##name##_XDIRECTP              \
                                      || SCM_MISC_##name##_XSHIFTP)           \
    };                                                                        \
    typedef xtype SCM_MISC_##name##_XTYPE;                                    \
    typedef ytype SCM_MISC_##name##_YTYPE /* No semicolon here. */

#define SCM_MISC_LEVEL_L1(t1)         1
#define SCM_MISC_LEVEL_L2(t2, t1)     2
#define SCM_MISC_LEVEL_L3(t3, t2, t1) 3

/* Dummies to make the declaration more verbose. */
#define SCM_MISC_XTYPE(t)       t
#define SCM_MISC_YTYPE(t)       t
#define SCM_MISC_Y_UNUSED       SCM_MISC_YTYPE(scm_int_t) /* Dummy. */
#define SCM_MISC_XALIGN(n)      n
#define SCM_MISC_XALIGN_SCMOBJ  SCM_GCBIT_WIDTH /* If storing ScmObj. */

#define SCM_MISC_INIT(o, x, y, typ)                                          \
    do {                                                                     \
        if (SCM_MISC_##typ##_XDIRECTP)                                       \
            SCM_INIT((o),                                                    \
                     SCM_MISC_CAST_X((x), typ),                              \
                     SCM_MISC_ENCODE_Y((y), typ),                            \
                     SCM_PTAG_MISC);                                         \
        else if (SCM_MISC_##typ##_XSHIFTP)                                   \
            SCM_INIT((o),                                                    \
                     SCM_MISC_CAST_X((x), typ) << SCM_MISC_##typ##_XSPILL,   \
                     SCM_MISC_ENCODE_Y((y), typ),                            \
                     SCM_PTAG_MISC);                                         \
        else                                                                 \
            SCM_MISC_INIT_SPLITX((o),                                        \
                                 SCM_MISC_CAST_X((x), typ),                  \
                                 SCM_MISC_CAST_Y((y), typ),                  \
                                 SCM_MISC_##typ##_LV,                        \
                                 SCM_MTAG_##typ,                             \
                                 SCM_MISC_##typ##_XSPILL);                   \
    } while (0)


/* Cast shorter integer types such as char to ScmObj with proper sign extension
 * and 64-bit safety. Especially on LP64 env, casting user-written integer
 * constant by simple (ScmObj)-1 causes information loss. It must be written as
 * -1L by user, or cast by receiver side by (ScmObj)(scmint_t)-1. This macro
 * applies latter method safely. Invoke this macro for any X and Y input for
 * misc object.  -- YamaKen 2006-12-11 */
#define SCM_MISC_CAST_X(x, typ) ((ScmObj)(SCM_MISC_##typ##_XTYPE)(x))
#define SCM_MISC_CAST_Y(y, typ) ((ScmObj)(SCM_MISC_##typ##_YTYPE)(y))

#define SCM_MISC_ENCODE_Y(y, typ)                                            \
    ((SCM_MISC_CAST_Y((y), typ) << SCM_MTAG_WIDTH(SCM_MISC_##typ##_LV))      \
     | SCM_MTAG_##typ)

/* Does (y) >> (n), paying attention to y's signedness. */
#define SCM_MISC_RSHIFT_Y(y, typ, n)                                         \
    ((SCM_SIGNED_TYPEP(SCM_MISC_##typ##_YTYPE))                              \
     ? SCM_ARSHIFT((y), (n)) : (y) >> (n))

/* The NASSERT macros skip access assertions and tag removal.  This is
 * needed for GC where we don't have ptags on the pointers. */

/* Signedness doesn't matter for XSHIFTP, as the top bits get truncated. */
#define SCM_MISC_X_NASSERT(o, typ)                      \
    ((SCM_MISC_##typ##_XTYPE)                           \
     (SCM_MISC_##typ##_XDIRECTP                         \
      ? SCM_X(o)                                        \
      : SCM_MISC_##typ##_XSHIFTP                        \
        ? (SCM_X(o) >> SCM_MISC_##typ##_XSPILL)         \
        : SCM_MISC_X_SPLITX((o),                        \
                            SCM_MISC_##typ##_LV,        \
                            SCM_MISC_##typ##_XSPILL)))

#define SCM_MISC_SET_X_NASSERT(o, x, typ)                                     \
    (SCM_MISC_##typ##_XDIRECTP                                                \
     ? SCM_SET_X((o), SCM_MISC_CAST_X((x), typ))                              \
     : SCM_MISC_##typ##_XSHIFTP                                               \
       ? SCM_SET_X((o), SCM_MISC_CAST_X((x), typ) << SCM_MISC_##typ##_XSPILL) \
       : SCM_MISC_SET_X_SPLITX((o), SCM_MISC_CAST_X((x), typ),                \
                               SCM_MISC_##typ##_LV,                           \
                               SCM_MISC_##typ##_XSPILL))

#define SCM_MISC_Y_NASSERT(o, typ)                                      \
    ((SCM_MISC_##typ##_YTYPE)                                           \
     (SCM_MISC_##typ##_XSPLITP                                          \
      ? SCM_MISC_Y_SPLITX((o), typ,                                     \
                          SCM_MISC_##typ##_LV, SCM_MISC_##typ##_XSPILL) \
      : SCM_MISC_RSHIFT_Y(SCM_Y(o), typ,                                \
                          SCM_MTAG_WIDTH(SCM_MISC_##typ##_LV))))

#define SCM_MISC_SET_Y_NASSERT(o, y, typ)                               \
    (SCM_MISC_##typ##_XSPLITP                                           \
     ? SCM_MISC_SET_Y_SPLITX((o), SCM_MISC_CAST_Y((y), typ),            \
                             SCM_MISC_##typ##_LV,                       \
                             SCM_MISC_##typ##_XSPILL)                   \
     : SCM_SET_Y((o),                                                   \
                 (SCM_MISC_CAST_Y((y), typ)                             \
                  << SCM_MTAG_WIDTH(SCM_MISC_##typ##_LV))               \
                 | SCM_MTAG_##typ))

#define SCM_MISC_X(o, typ)        SCM_MISC_X_NASSERT(SCM_##typ##_PTR(o), typ)
#define SCM_MISC_SET_X(o, x, typ) SCM_MISC_SET_X_NASSERT(SCM_##typ##_PTR(o), \
                                                         (x), typ)
#define SCM_MISC_Y(o, typ)        SCM_MISC_Y_NASSERT(SCM_##typ##_PTR(o), typ)
#define SCM_MISC_SET_Y(o, y, typ) SCM_MISC_SET_Y_NASSERT(SCM_##typ##_PTR(o), \
                                                         (y), typ)

#define SCM_MISC_PTR(o, typ)    SCM_UNTAG_PTR(SCM_AS_##typ(o))
#define SCM_MISC_CELL_TYPEP(c, typ)                           \
    (SCM_MTAG((&(c)), SCM_MISC_##typ##_LV) == SCM_MTAG_##typ)
#define SCM_MISC_TYPEP(o, typ)                                          \
    (SCM_MISCP(o) && SCM_MISC_CELL_TYPEP(*SCM_UNTAG_PTR(o), typ))


/* ------------------------------
 * And finally, the types....
 */
/* Symbols. */
SCM_MISC_DECLARE_TYPE(SYMBOL, L1(0),
                      SCM_MISC_XTYPE(ScmObj), SCM_MISC_XALIGN_SCMOBJ,
                      SCM_MISC_YTYPE(char *));

#define SCM_SYMBOL_PTR(o)              SCM_MISC_PTR((o), SYMBOL)
#define SCM_SYMBOL_NAME_ALIGN          SCM_MTAG_WIDTH(SCM_MISC_SYMBOL_LV)
#define SCM_SAL_SYMBOLP(o)             SCM_MISC_TYPEP((o), SYMBOL)
#define SCM_SAL_SYMBOL_VCELL(o)        SCM_MISC_X((o), SYMBOL)
#define SCM_SAL_SYMBOL_SET_VCELL(o, c) SCM_MISC_SET_X((o), (c), SYMBOL)

/* Symbols is the only misc type that has a pointer on Y, which
 * doesn't fit well in the data model of other types.  Hence we treat
 * it rather ad-hocly. */
#define SCM_ALIGNED_SYMBOL_NAME(n)                                           \
    (!((uintptr_t)(n) & SCM_MAKE_MASK(0, SCM_SYMBOL_NAME_ALIGN)))
#define SCM_SAL_SYMBOL_NAME(o)                                               \
    ((char *)(SCM_Y(SCM_SYMBOL_PTR(o)) & ~SCM_MTAG_SYMBOL))
#define SCM_SAL_SYMBOL_SET_NAME(o, n)                                        \
    (SCM_ASSERT(SCM_ALIGNED_SYMBOL_NAME(n)),                                 \
     SCM_SET_Y(SCM_SYMBOL_PTR(o), (scm_uintobj_t)(n) | SCM_MTAG_SYMBOL))
#define SCM_ISAL_SYMBOL_INIT(o, n, c)                                        \
    (SCM_ASSERT(SCM_ALIGNED_SYMBOL_NAME(n)),                                 \
     SCM_INIT((o),                                                           \
              (c),                                                           \
              (scm_uintobj_t)(n) | SCM_MTAG_SYMBOL,                          \
              SCM_PTAG_MISC))
#define SCM_CELL_SYMBOLP(c)            SCM_MISC_CELL_TYPEP((c), SYMBOL)
#define SCM_CELL_SYMBOL_FIN(c)                                               \
    do {                                                                     \
        char *_s = (char *)(SCM_Y(&(c)) & ~SCM_MTAG_SYMBOL);                 \
        free(_s);                                                            \
    } while (0)

/* Strings. */
SCM_MISC_DECLARE_TYPE(STRING, L1(1),
                      SCM_MISC_XTYPE(char *), SCM_MISC_XALIGN(1),
                      SCM_MISC_YTYPE(scm_int_t));

#define SCM_STRING_PTR(o)            SCM_MISC_PTR((o), STRING)
#define SCM_SAL_STRINGP(o)           SCM_MISC_TYPEP((o), STRING)
#define SCM_STRING_MUTABLE_BIT       ((scm_int_t)1)
#define SCM_STRING_MUTABLE_BIT_WIDTH 1
#define SCM_STRING_MUTABILITY(o)                           \
    (SCM_MISC_Y((o), STRING) & SCM_STRING_MUTABLE_BIT)
#define SCM_SAL_STRING_MUTABLEP(o)   SCM_STRING_MUTABILITY(o)
#define SCM_SAL_STRING_SET_MUTABLE(o)                                     \
    SCM_MISC_SET_Y((o), SCM_MISC_Y((o), STRING) | SCM_STRING_MUTABLE_BIT, \
                   STRING)
#define SCM_SAL_STRING_SET_IMMUTABLE(o)                                    \
    SCM_MISC_SET_Y((o), SCM_MISC_Y((o), STRING) & ~SCM_STRING_MUTABLE_BIT, \
                   STRING)
#define SCM_SAL_STRING_STR(o)        SCM_MISC_X((o), STRING)
#define SCM_SAL_STRING_LEN(o)        (SCM_MISC_Y((o), STRING)                \
                                      >> SCM_STRING_MUTABLE_BIT_WIDTH)
#define SCM_SAL_STRING_SET_STR(o, s) SCM_MISC_SET_X((o), (s), STRING)
#define SCM_SAL_STRING_SET_LEN(o, l)                                         \
    SCM_MISC_SET_Y((o),                                                      \
                   (((scm_int_t)(l) << SCM_STRING_MUTABLE_BIT_WIDTH)         \
                    | SCM_STRING_MUTABILITY(o)),                             \
                   STRING)
#define SCM_ISAL_STRING_INIT(o, s, l, mut)                                   \
    SCM_MISC_INIT((o), (s),                                                  \
                  ((scm_int_t)(l) << SCM_STRING_MUTABLE_BIT_WIDTH)           \
                  | ((mut) ? SCM_STRING_MUTABLE_BIT : 0),                    \
                  STRING)
#define SCM_ISAL_MUTABLE_STRING_INIT(o, s, l)                                \
    SCM_ISAL_STRING_INIT((o), (s), (l), scm_true)
#define SCM_ISAL_IMMUTABLE_STRING_INIT(o, s, l)                              \
    SCM_ISAL_STRING_INIT((o), (s), (l), scm_false)
#define SCM_CELL_STRINGP(c)      SCM_MISC_CELL_TYPEP((c), STRING)
#define SCM_CELL_STRING_FIN(c)                                  \
    do {                                                        \
        char *_s = SCM_MISC_X_NASSERT(&(c), STRING);            \
        free(_s);                                               \
    } while (0)


/* Vectors. */
SCM_MISC_DECLARE_TYPE(VECTOR, L1(2),
                      SCM_MISC_XTYPE(ScmObj *), SCM_MISC_XALIGN(1),
                      SCM_MISC_YTYPE(scm_int_t));

#define SCM_VECTOR_PTR(o)            SCM_MISC_PTR((o), VECTOR)
#define SCM_SAL_VECTORP(o)           SCM_MISC_TYPEP((o), VECTOR)
#define SCM_VECTOR_MUTABLE_BIT       ((scm_int_t)1)
#define SCM_VECTOR_MUTABLE_BIT_WIDTH 1
#define SCM_VECTOR_MUTABILITY(o)                           \
    (SCM_MISC_Y((o), VECTOR) & SCM_VECTOR_MUTABLE_BIT)
#define SCM_SAL_VECTOR_MUTABLEP(o)   SCM_VECTOR_MUTABILITY(o)
#define SCM_SAL_VECTOR_SET_MUTABLE(o)                                     \
    SCM_MISC_SET_Y((o), SCM_MISC_Y((o), VECTOR) | SCM_VECTOR_MUTABLE_BIT, \
                   VECTOR)
#define SCM_SAL_VECTOR_SET_IMMUTABLE(o)                                    \
    SCM_MISC_SET_Y((o), SCM_MISC_Y((o), VECTOR) & ~SCM_VECTOR_MUTABLE_BIT, \
                   VECTOR)
#define SCM_SAL_VECTOR_VEC(o)        SCM_MISC_X((o), VECTOR)
#define SCM_SAL_VECTOR_LEN(o)        (SCM_MISC_Y((o), VECTOR)                \
                                      >> SCM_VECTOR_MUTABLE_BIT_WIDTH)
#define SCM_SAL_VECTOR_SET_VEC(o, v) SCM_MISC_SET_X((o), (v), VECTOR)
#define SCM_SAL_VECTOR_SET_LEN(o, l)                                         \
    SCM_MISC_SET_Y((o),                                                      \
                   (((scm_int_t)(l) << SCM_VECTOR_MUTABLE_BIT_WIDTH)         \
                    | SCM_VECTOR_MUTABILITY(o)),                             \
                   VECTOR)
#define SCM_ISAL_VECTOR_INIT(o, v, l, mut)                                   \
    SCM_MISC_INIT((o), (v),                                                  \
                  (((scm_int_t)(l) << SCM_VECTOR_MUTABLE_BIT_WIDTH)          \
                   | ((mut) ? SCM_VECTOR_MUTABLE_BIT : 0)),                  \
                  VECTOR)
#define SCM_ISAL_MUTABLE_VECTOR_INIT(o, v, l)                                \
    SCM_ISAL_VECTOR_INIT((o), (v), (l), scm_true)
#define SCM_ISAL_IMMUTABLE_VECTOR_INIT(o, v, l)                              \
    SCM_ISAL_VECTOR_INIT((o), (v), (l), scm_false)
#define SCM_CELL_VECTORP(c)      SCM_MISC_CELL_TYPEP((c), VECTOR)
#define SCM_CELL_VECTOR_FIN(c)                                  \
    do {                                                        \
        ScmObj *_vec = SCM_MISC_X_NASSERT(&(c), VECTOR);        \
        free(_vec);                                             \
    } while (0)

/* Multiple Values. */
SCM_MISC_DECLARE_TYPE(VALUEPACKET, L2(0, 3),
                      SCM_MISC_XTYPE(ScmObj), SCM_MISC_XALIGN_SCMOBJ,
                      SCM_MISC_Y_UNUSED);
#if SCM_USE_VALUECONS
#error "SCM_USE_VALUECONS is not supported by storage-compact."
#endif

#define SCM_VALUEPACKET_PTR(o)         SCM_MISC_PTR((o), VALUEPACKET)
#define SCM_SAL_VALUEPACKETP(o)        SCM_MISC_TYPEP((o), VALUEPACKET)
#define SCM_SAL_VALUEPACKET_VALUES(o)  SCM_MISC_X((o), VALUEPACKET)
#define SCM_SAL_VALUEPACKET_SET_VALUES(o, v)    \
    SCM_MISC_SET_X((o), (v), VALUEPACKET)
#define SCM_ISAL_VALUEPACKET_INIT(o, v) SCM_MISC_INIT((o), (v), 0, VALUEPACKET)

/* Builtin functions. */
SCM_MISC_DECLARE_TYPE(FUNC, L2(1, 3),
                      SCM_MISC_XTYPE(ScmFuncType), SCM_MISC_XALIGN(0),
                      SCM_MISC_YTYPE(enum ScmFuncTypeCode));

#define SCM_FUNC_PTR(o)                 SCM_MISC_PTR((o), FUNC)
#define SCM_SAL_FUNCP(o)                SCM_MISC_TYPEP((o), FUNC)
#define SCM_SAL_FUNC_CFUNC(o)           SCM_MISC_X((o), FUNC)
#define SCM_SAL_FUNC_TYPECODE(o)        SCM_MISC_Y((o), FUNC)
#define SCM_SAL_FUNC_SET_CFUNC(o, f)    SCM_MISC_SET_X((o), (f), FUNC)
#define SCM_SAL_FUNC_SET_TYPECODE(o, t) SCM_MISC_SET_Y((o), (t), FUNC)
#define SCM_ISAL_FUNC_INIT(o, t, f)     SCM_MISC_INIT((o), (f), (t), FUNC)

/* Ports. */
struct ScmCharPort_;

SCM_MISC_DECLARE_TYPE(PORT, L2(2, 3),
                      SCM_MISC_XTYPE(struct ScmCharPort_ *), SCM_MISC_XALIGN(1),
                      SCM_MISC_YTYPE(enum ScmPortFlag));

#define SCM_PORT_PTR(o)             SCM_MISC_PTR((o), PORT)
#define SCM_SAL_PORTP(o)            SCM_MISC_TYPEP((o), PORT)
#define SCM_SAL_PORT_IMPL(o)        SCM_MISC_X((o), PORT)
#define SCM_SAL_PORT_FLAG(o)        SCM_MISC_Y((o), PORT)
#define SCM_SAL_PORT_SET_IMPL(o, i) SCM_MISC_SET_X((o), (i), PORT)
#define SCM_SAL_PORT_SET_FLAG(o, f) SCM_MISC_SET_Y((o), (f), PORT)
#define SCM_ISAL_PORT_INIT(o, i, f) SCM_MISC_INIT((o), (i), (f), PORT)
#define SCM_CELL_PORTP(c)           SCM_MISC_CELL_TYPEP((c), PORT)
#define SCM_CELL_PORT_FIN(c)                            \
    do {                                                \
        struct ScmCharPort_ *impl;                      \
        impl = SCM_MISC_X_NASSERT(&(c), PORT);          \
        if (impl)                                       \
            SCM_CHARPORT_CLOSE(impl);                   \
    } while (0)


/* Continuation. */
SCM_MISC_DECLARE_TYPE(CONTINUATION, L2(3, 3),
                      SCM_MISC_XTYPE(void *), SCM_MISC_XALIGN(1),
                      SCM_MISC_YTYPE(scm_int_t));

#define SCM_CONTINUATION_PTR(o)           SCM_MISC_PTR((o), CONTINUATION)
#define SCM_SAL_CONTINUATIONP(o)          SCM_MISC_TYPEP((o), CONTINUATION)
#define SCM_SAL_CONTINUATION_OPAQUE(o)    SCM_MISC_X((o), CONTINUATION)
#define SCM_SAL_CONTINUATION_TAG(o)       SCM_MISC_Y((o), CONTINUATION)
#define SCM_SAL_CONTINUATION_SET_OPAQUE(o, a)                                \
    SCM_MISC_SET_X((o), (a), CONTINUATION)
#define SCM_SAL_CONTINUATION_SET_TAG(o, t)                                   \
    SCM_MISC_SET_Y((o), (t), CONTINUATION)
#define SCM_ISAL_CONTINUATION_INIT(o, a, t)                                  \
    SCM_MISC_INIT((o), (a), (t), CONTINUATION)
#define SCM_CELL_CONTINUATIONP(c)                                            \
    SCM_MISC_CELL_TYPEP((c), CONTINUATION)
/*
 * Since continuations aren't so common, the cost of function call for
 * destroying one is acceptable.  In turn, it eases continuation
 * module substitution without requiring module-specific destructors.
 */
#define SCM_CELL_CONTINUATION_FIN(c)                            \
    scm_destruct_continuation((ScmObj)&(c) | SCM_PTAG_MISC)


#if SCM_USE_SSCM_EXTENSIONS

/* C datum pointer */
SCM_MISC_DECLARE_TYPE(C_POINTER, L3(0, 4, 3),
                      SCM_MISC_XTYPE(void *), SCM_MISC_XALIGN(0),
                      SCM_MISC_Y_UNUSED);

#define SCM_C_POINTER_PTR(o)              SCM_MISC_PTR((o), C_POINTER)
#define SCM_SAL_C_POINTERP(o)             SCM_MISC_TYPEP((o), C_POINTER)
#define SCM_SAL_C_POINTER_VALUE(o)        SCM_MISC_X((o), C_POINTER)
#define SCM_SAL_C_POINTER_SET_VALUE(o, p) SCM_MISC_SET_X((o), (p), C_POINTER)
#define SCM_ISAL_C_POINTER_INIT(o, p)     SCM_MISC_INIT((o), (p), 0, C_POINTER)

/* C function pointer */
SCM_MISC_DECLARE_TYPE(C_FUNCPOINTER, L3(1, 4, 3),
                      SCM_MISC_XTYPE(ScmCFunc), SCM_MISC_XALIGN(0),
                      SCM_MISC_Y_UNUSED);

#define SCM_C_FUNCPOINTER_PTR(o)       SCM_MISC_PTR((o), C_FUNCPOINTER)
#define SCM_SAL_C_FUNCPOINTERP(o)      SCM_MISC_TYPEP((o), C_FUNCPOINTER)
#define SCM_SAL_C_FUNCPOINTER_VALUE(o) SCM_MISC_X((o), C_FUNCPOINTER)
#define SCM_SAL_C_FUNCPOINTER_SET_VALUE(o, f)   \
    SCM_MISC_SET_X((o), (f), C_FUNCPOINTER)
#define SCM_ISAL_C_FUNCPOINTER_INIT(o, f)       \
    SCM_MISC_INIT((o), (f), 0, C_FUNCPOINTER)

#endif /* SCM_USE_SSCM_EXTENSIONS */


#if SCM_USE_HYGIENIC_MACRO

#if SCM_USE_UNHYGIENIC_MACRO
#error "Not implemented (you need to change the representations of hmacro and farsymbol)."
#endif

/* Wrapper is an abstract supertype of the macro-related types whose
 * definitions follow.  Wrapper itself is provided for GC and
 * shouldn't be utilized in user code. */
SCM_MISC_DECLARE_TYPE(WRAPPER, L2(5, 3),
                      SCM_MISC_XTYPE(ScmObj), SCM_MISC_XALIGN_SCMOBJ,
                      SCM_MISC_YTYPE(scm_int_t));

#define SCM_WRAPPERP(o)               SCM_MISC_TYPEP((o), WRAPPER)
#define SCM_WRAPPER_PTR(o)            SCM_UNTAG_PTR(o)
#define SCM_WRAPPER_OBJ(o)            SCM_MISC_X((o), WRAPPER)

/* Compiled repeatable subpattern or subtemplate. */
SCM_MISC_DECLARE_TYPE(SUBPAT, L3(0, 5, 3),
                      SCM_MISC_XTYPE(ScmObj), SCM_MISC_XALIGN_SCMOBJ,
                      SCM_MISC_YTYPE(scm_int_t));

#define SCM_SUBPAT_PTR(o)             SCM_MISC_PTR((o), SUBPAT)
#define SCM_SAL_SUBPATP(o)            SCM_MISC_TYPEP((o), SUBPAT)
#define SCM_SAL_SUBPAT_OBJ(o)         SCM_MISC_X((o), SUBPAT)
#define SCM_SAL_SUBPAT_META(o)        SCM_MISC_Y((o), SUBPAT)
#define SCM_SAL_SUBPAT_SET_OBJ(o, p)  SCM_MISC_SET_X((o), (p), SUBPAT)
#define SCM_SAL_SUBPAT_SET_META(o, m) SCM_MISC_SET_Y((o), (m), SUBPAT)
#define SCM_ISAL_SUBPAT_INIT(o, p, m) SCM_MISC_INIT((o), (p), (m), SUBPAT)

/* Compiled macro. */
SCM_MISC_DECLARE_TYPE(HMACRO, L3(1, 5, 3),
                      SCM_MISC_XTYPE(ScmObj), SCM_MISC_XALIGN_SCMOBJ,
                      SCM_MISC_YTYPE(ScmPackedEnv));

#define SCM_HMACRO_PTR(o)              SCM_MISC_PTR((o), HMACRO)
#define SCM_SAL_HMACROP(o)             SCM_MISC_TYPEP((o), HMACRO)
#define SCM_SAL_HMACRO_RULES(o)        SCM_MISC_X((o), HMACRO)
#define SCM_SAL_HMACRO_ENV(o)          SCM_MISC_Y((o), HMACRO)
#define SCM_SAL_HMACRO_SET_RULES(o, r) SCM_MISC_SET_X((o), (r), HMACRO)
#define SCM_SAL_HMACRO_SET_ENV(o, e)   SCM_MISC_SET_Y((o), (e), HMACRO)
#define SCM_ISAL_HMACRO_INIT(o, r, e)  SCM_MISC_INIT((o), (r), (e), HMACRO)

/* Far symbol. */
SCM_MISC_DECLARE_TYPE(FARSYMBOL, L3(2, 5, 3),
                      SCM_MISC_XTYPE(ScmObj), SCM_MISC_XALIGN_SCMOBJ,
                      SCM_MISC_YTYPE(ScmPackedEnv));

#define SCM_FARSYMBOL_PTR(o)            SCM_MISC_PTR((o), FARSYMBOL)
#define SCM_SAL_FARSYMBOLP(o)           SCM_MISC_TYPEP((o), FARSYMBOL)
#define SCM_SAL_FARSYMBOL_SYM(o)        SCM_MISC_X((o), FARSYMBOL)
#define SCM_SAL_FARSYMBOL_ENV(o)        SCM_MISC_Y((o), FARSYMBOL)
#define SCM_SAL_FARSYMBOL_SET_SYM(o, s) SCM_MISC_SET_X((o), (s), FARSYMBOL)
#define SCM_SAL_FARSYMBOL_SET_ENV(o, e) SCM_MISC_SET_Y((o), (e), FARSYMBOL)
#define SCM_ISAL_FARSYMBOL_INIT(o, s, e) SCM_MISC_INIT((o), (s), (e), FARSYMBOL)

#endif /* SCM_USE_HYGIENIC_MACRO */


/* Each argument must be an untagged pointer to a cell.
 *
 * TODO: If we assume, as we currently safely can, that the GC never
 * marks a free cell (GC takes place until all freecells are used up),
 * we can leave obj_y untouched.  That optimization, however, has to
 * be coordinated with storage-gc.c.
 */
#define SCM_MTAG_FREECELL         SCM_MAKE_MTAG_L2(7, 3)
#define SCM_SAL_FREECELL_NEXT(o)  (SCM_X(o))
#define SCM_SAL_FREECELLP(o)      (SCM_Y(o) == SCM_MTAG_FREECELL)
#define SCM_SAL_RECLAIM_CELL(o, next)                           \
    (SCM_SET_X((o), (ScmObj)(next)), SCM_SET_Y((o), SCM_MTAG_FREECELL))


/* Typecode determination (slow but universally applicable). */
SCM_EXPORT enum ScmObjType scm_type(ScmObj obj);
#define SCM_SAL_TYPE scm_type


/*=======================================
  Object Representation Information
=======================================*/
#define SCM_SAL_HAS_CHAR     1
#define SCM_SAL_HAS_RATIONAL 0
#define SCM_SAL_HAS_REAL     0
#define SCM_SAL_HAS_COMPLEX  0
#define SCM_SAL_HAS_STRING   1
#define SCM_SAL_HAS_VECTOR   1

#define SCM_SAL_HAS_IMMUTABLE_CONS   0
#define SCM_SAL_HAS_IMMUTABLE_STRING 1
#define SCM_SAL_HAS_IMMUTABLE_VECTOR 1

/* for optimization */
#define SCM_SAL_HAS_IMMEDIATE_CHAR_ONLY     1
#define SCM_SAL_HAS_IMMEDIATE_NUMBER_ONLY   1
#define SCM_SAL_HAS_IMMEDIATE_INT_ONLY      1
#define SCM_SAL_HAS_IMMEDIATE_RATIONAL_ONLY 0
#define SCM_SAL_HAS_IMMEDIATE_REAL_ONLY     0
#define SCM_SAL_HAS_IMMEDIATE_COMPLEX_ONLY  0

#define SCM_SAL_OBJ_BITS    (sizeof(ScmObj) * CHAR_BIT)
#define SCM_SAL_PTR_BITS    (sizeof(void *) * CHAR_BIT)

#define SCM_SAL_CHAR_BITS   SCM_MIN((SCM_SAL_OBJ_BITS - SCM_CHAR_VAL_OFFSET), \
                                    (sizeof(scm_ichar_t) * CHAR_BIT))
#define SCM_SAL_CHAR_MAX    SCM_MIN((scm_ichar_t)                            \
                                    SCM_MAKE_MASK(0, SCM_SAL_CHAR_BITS),     \
                                    SCM_ICHAR_T_MAX)

#define SCM_SAL_INT_BITS    SCM_MIN((SCM_SAL_OBJ_BITS - SCM_INT_VAL_OFFSET), \
                                    (sizeof(scm_int_t) * CHAR_BIT))
#define SCM_SAL_INT_MAX     SCM_MIN((scm_int_t)                              \
                                    (SCM_INT_T_MAX >> SCM_INT_VAL_OFFSET),   \
                                    SCM_INT_T_MAX))
#define SCM_SAL_INT_MIN     SCM_MAX((scm_int_t)                              \
                                    SCM_ARSHIFT(SCM_INT_T_MIN,               \
                                                SCM_INT_VAL_OFFSET),         \
                                    SCM_INT_T_MIN)

/* string length */
#define SCM_SAL_STRLEN_BITS SCM_INT_BITS
#define SCM_SAL_STRLEN_MAX  SCM_INT_MAX

/* vector length */
#define SCM_SAL_VECLEN_BITS SCM_INT_BITS
#define SCM_SAL_VECLEN_MAX  SCM_INT_MAX

/*===========================================================================
  Abstract ScmObj Reference For Storage-Representation Independent Efficient
  List Operations
===========================================================================*/
typedef ScmObj *ScmRef;
#define SCM_SAL_INVALID_REF NULL

#define SCM_SAL_REF_CAR(cons)     (&SCM_X(cons))
#define SCM_SAL_REF_CDR(cons)     (&SCM_Y(cons))
#define SCM_SAL_REF_OFF_HEAP(obj) (&(obj))

/* SCM_DEREF(ref) is not permitted to be used as lvalue */
#define SCM_SAL_DEREF(ref) (*(ref) + 0)

/* RFC: Is there a better name? */
#define SCM_SAL_SET(ref, obj) (*(ref) = (ScmObj)(obj))

#ifdef __cplusplus
/* } */
#endif

#include "storage-common.h"

#endif /* __STORAGE_COMPACT_H */
