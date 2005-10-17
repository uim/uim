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
 *     ..011|11|1 : Continuation        : all 0 (for efficiency)
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

/* ScmPort direction */
enum ScmPortDirection {
    PORT_INPUT  = 0,
    PORT_OUTPUT = 1
};

/* ScmPort type */
enum ScmPortType {
    PORT_FILE   = 0,
    PORT_STRING = 1
};

/* ScmPort Info */
struct _ScmPortInfo {
    enum ScmPortType port_type; /* (PORT_FILE  | PORT_STRING) */
    
    union {
        struct {
            FILE *file;
            char *filename;            
            int line;
        } file_port;
        
        struct {
            char *port_str;
            const char *str_currentpos;
        } str_port;
    } info;

    int  (*getc_func) (ScmObj port);
    void (*print_func) (ScmObj port, const char* str);    
    int ungottenchar;
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
   Masks and Offsets
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

/* FIXME: hardcoded width 32 */
#define SCM_VALUE_WIDTH     (32 - (SCM_TAG_WIDTH + SCM_GCBIT_WIDTH))
#define SCM_VALUE_OFFSET    (SCM_TAG_WIDTH + SCM_GCBIT_WIDTH)
#define SCM_VALUE_MASK      (~0U << SCM_VALUE_OFFSET)

#define SCM_TAG_OTHERS_VALUE_OFFSET_SYMBOL       SCM_GCBIT_WIDTH + 2
#define SCM_TAG_OTHERS_VALUE_OFFSET_STRING       SCM_GCBIT_WIDTH + 2
#define SCM_TAG_OTHERS_VALUE_OFFSET_VECTOR       SCM_GCBIT_WIDTH + 2
#define SCM_TAG_OTHERS_VALUE_OFFSET_VALUES       SCM_GCBIT_WIDTH + 2 + 3
#define SCM_TAG_OTHERS_VALUE_OFFSET_FUNC         SCM_GCBIT_WIDTH + 2 + 3
#define SCM_TAG_OTHERS_VALUE_OFFSET_PORT         SCM_GCBIT_WIDTH + 2 + 3
#define SCM_TAG_OTHERS_VALUE_OFFSET_CONTINUATION SCM_GCBIT_WIDTH + 2 + 3
#define SCM_TAG_OTHERS_VALUE_OFFSET_C_POINTER    SCM_GCBIT_WIDTH + 2 + 3 + 1
#define SCM_TAG_OTHERS_VALUE_OFFSET_FREECELL     SCM_GCBIT_WIDTH + 2 + 3
#define SCM_TAG_OTHERS_MASK_SYMBOL               ((0x1 << SCM_TAG_OTHERS_VALUE_OFFSET_SYMBOL)       - 1)
#define SCM_TAG_OTHERS_MASK_STRING               ((0x1 << SCM_TAG_OTHERS_VALUE_OFFSET_STRING)       - 1)
#define SCM_TAG_OTHERS_MASK_VECTOR               ((0x1 << SCM_TAG_OTHERS_VALUE_OFFSET_VECTOR)       - 1)
#define SCM_TAG_OTHERS_MASK_VALUES               ((0x1 << SCM_TAG_OTHERS_VALUE_OFFSET_VALUES)       - 1)
#define SCM_TAG_OTHERS_MASK_FUNC                 ((0x1 << SCM_TAG_OTHERS_VALUE_OFFSET_FUNC)         - 1)
#define SCM_TAG_OTHERS_MASK_PORT                 ((0x1 << SCM_TAG_OTHERS_VALUE_OFFSET_PORT)         - 1)
#define SCM_TAG_OTHERS_MASK_CONTINUATION         ((0x1 << SCM_TAG_OTHERS_VALUE_OFFSET_CONTINUATION) - 1)
#define SCM_TAG_OTHERS_MASK_C_POINTER            ((0x1 << SCM_TAG_OTHERS_VALUE_OFFSET_C_POINTER)    - 1)
#define SCM_TAG_OTHERS_MASK_FREECELL             ((0x1 << SCM_TAG_OTHERS_VALUE_OFFSET_FREECELL)     - 1)
#define SCM_TAG_OTHERS_SYMBOL                    (SCM_TAG_OTHERS | (0x0 << SCM_GCBIT_WIDTH))
#define SCM_TAG_OTHERS_STRING                    (SCM_TAG_OTHERS | (0x1 << SCM_GCBIT_WIDTH))
#define SCM_TAG_OTHERS_VECTOR                    (SCM_TAG_OTHERS | (0x2 << SCM_GCBIT_WIDTH))
#define SCM_TAG_OTHERS_VALUES                    (SCM_TAG_OTHERS | (0x3 << SCM_GCBIT_WIDTH) | (0x0 << 6))
#define SCM_TAG_OTHERS_FUNC                      (SCM_TAG_OTHERS | (0x3 << SCM_GCBIT_WIDTH) | (0x1 << 6))
#define SCM_TAG_OTHERS_PORT                      (SCM_TAG_OTHERS | (0x3 << SCM_GCBIT_WIDTH) | (0x2 << 6))
#define SCM_TAG_OTHERS_CONTINUATION              (SCM_TAG_OTHERS | (0x3 << SCM_GCBIT_WIDTH) | (0x3 << 6))
#define SCM_TAG_OTHERS_C_POINTER                 (SCM_TAG_OTHERS | (0x3 << SCM_GCBIT_WIDTH) | (0x4 << 6) | (0x0 << 7))
#define SCM_TAG_OTHERS_C_FUNC_POINTER            (SCM_TAG_OTHERS | (0x3 << SCM_GCBIT_WIDTH) | (0x4 << 6) | (0x1 << 7))
#define SCM_TAG_OTHERS_FREECELL                  (SCM_TAG_OTHERS | (0x3 << SCM_GCBIT_WIDTH) | (0x7 << 6))

#define SCM_TAG_IMM_VALUE_OFFSET_INT             SCM_GCBIT_WIDTH + 2 + 1
#define SCM_TAG_IMM_VALUE_OFFSET_CHAR            SCM_GCBIT_WIDTH + 2 + 2
#define SCM_TAG_IMM_VALUE_OFFSET_CONST           SCM_GCBIT_WIDTH + 2 + 2 + 3
#define SCM_TAG_IMM_MASK_INT                     ((0x1 << SCM_TAG_IMM_VALUE_OFFSET_INT)   - 1)
#define SCM_TAG_IMM_MASK_CHAR                    ((0x1 << SCM_TAG_IMM_VALUE_OFFSET_CHAR)  - 1)
#define SCM_TAG_IMM_MASK_CONST                   ((0x1 << SCM_TAG_IMM_VALUE_OFFSET_CONST) - 1)
#define SCM_TAG_IMM_INT                          (SCM_TAG_IMM | (0x0 << 3))
#define SCM_TAG_IMM_CHAR                         (SCM_TAG_IMM | (0x1 << 3))
#define SCM_TAG_IMM_NULL                         (SCM_TAG_IMM | (0x3 << 3) | (0x0 << 5))
#define SCM_TAG_IMM_INVALID                      (SCM_TAG_IMM | (0x3 << 3) | (0x1 << 5))
#define SCM_TAG_IMM_UNBOUND                      (SCM_TAG_IMM | (0x3 << 3) | (0x2 << 5))
#define SCM_TAG_IMM_FALSE                        (SCM_TAG_IMM | (0x3 << 3) | (0x3 << 5))
#define SCM_TAG_IMM_TRUE                         (SCM_TAG_IMM | (0x3 << 3) | (0x4 << 5))
#define SCM_TAG_IMM_EOF                          (SCM_TAG_IMM | (0x3 << 3) | (0x5 << 5))
#define SCM_TAG_IMM_UNDEF                        (SCM_TAG_IMM | (0x3 << 3) | (0x6 << 5))

/*=======================================
   GC bit Accessor
=======================================*/
#define SCM_GC_BIT(a)       (((unsigned int)(a)) & SCM_GCBIT_MASK)
#define SCM_DO_MARK(a)      ((a) = (ScmObj)(((unsigned int)(a)) | SCM_GCBIT_MASK))
#define SCM_DO_UNMARK(a)    ((a) = (ScmObj)(((unsigned int)(a)) & ~SCM_GCBIT_MASK))

/*=======================================
   Type Predicates
=======================================*/
/* Tag Accessor */
#define SCM_TAG_CONSP(a)      ((((unsigned int)(a)) & SCM_TAG_MASK) == SCM_TAG_CONS)
#define SCM_TAG_CLOSUREP(a)   ((((unsigned int)(a)) & SCM_TAG_MASK) == SCM_TAG_CLOSURE)
#define SCM_TAG_OTHERSP(a)    ((((unsigned int)(a)) & SCM_TAG_MASK) == SCM_TAG_OTHERS)
#define SCM_TAG_IMMEDIATEP(a) ((((unsigned int)(a)) & SCM_TAG_MASK) == SCM_TAG_IMM)

/* Tag -> Others */
#define SCM_TAG_OTHERS_SYMBOLP(a)         ((((unsigned int)(SCM_GET_AS_OBJ(a)->cdr)) & SCM_TAG_OTHERS_MASK_SYMBOL) == SCM_TAG_OTHERS_SYMBOL)
#define SCM_TAG_OTHERS_STRINGP(a)         ((((unsigned int)(SCM_GET_AS_OBJ(a)->cdr)) & SCM_TAG_OTHERS_MASK_STRING) == SCM_TAG_OTHERS_STRING)
#define SCM_TAG_OTHERS_VECTORP(a)         ((((unsigned int)(SCM_GET_AS_OBJ(a)->cdr)) & SCM_TAG_OTHERS_MASK_VECTOR) == SCM_TAG_OTHERS_VECTOR)
#define SCM_TAG_OTHERS_VALUESP(a)         ((((unsigned int)(SCM_GET_AS_OBJ(a)->cdr)) & SCM_TAG_OTHERS_MASK_VALUES) == SCM_TAG_OTHERS_VALUES)
#define SCM_TAG_OTHERS_FUNCP(a)           ((((unsigned int)(SCM_GET_AS_OBJ(a)->cdr)) & SCM_TAG_OTHERS_MASK_FUNC) == SCM_TAG_OTHERS_FUNC)
#define SCM_TAG_OTHERS_PORTP(a)           ((((unsigned int)(SCM_GET_AS_OBJ(a)->cdr)) & SCM_TAG_OTHERS_MASK_PORT) == SCM_TAG_OTHERS_PORT)
#define SCM_TAG_OTHERS_CONTINUATIONP(a)   ((((unsigned int)(SCM_GET_AS_OBJ(a)->cdr)) & SCM_TAG_OTHERS_MASK_CONTINUATION) == SCM_TAG_OTHERS_CONTINUATION)
#define SCM_TAG_OTHERS_C_POINTERP(a)      ((((unsigned int)(SCM_GET_AS_OBJ(a)->cdr)) & SCM_TAG_OTHERS_MASK_C_POINTER) == SCM_TAG_OTHERS_C_POINTER)
#define SCM_TAG_OTHERS_C_FUNC_POINTERP(a) ((((unsigned int)(SCM_GET_AS_OBJ(a)->cdr)) & SCM_TAG_OTHERS_MASK_C_POINTER) == SCM_TAG_OTHERS_C_FUNC_POINTER)
#define SCM_TAG_OTHERS_FREECELLP(a)       ((((unsigned int)(SCM_GET_AS_OBJ(a)->cdr)) & SCM_TAG_OTHERS_MASK_FREECELL) == SCM_TAG_OTHERS_FREECELL)

/* Tag -> Imm */
#define SCM_TAG_IMM_INTP(a)               ((((unsigned int)(a)) & SCM_TAG_IMM_MASK_INT)   == SCM_TAG_IMM_INT)
#define SCM_TAG_IMM_CHARP(a)              ((((unsigned int)(a)) & SCM_TAG_IMM_MASK_CHAR)  == SCM_TAG_IMM_CHAR)
#define SCM_TAG_IMM_NULLP(a)              ((((unsigned int)(a)) & SCM_TAG_IMM_MASK_CONST) == SCM_TAG_IMM_NULL)
#define SCM_TAG_IMM_INVALIDP(a)           ((((unsigned int)(a)) & SCM_TAG_IMM_MASK_CONST) == SCM_TAG_IMM_INVALID)
#define SCM_TAG_IMM_UNBOUNDP(a)           ((((unsigned int)(a)) & SCM_TAG_IMM_MASK_CONST) == SCM_TAG_IMM_UNBOUND)
#define SCM_TAG_IMM_FALSEP(a)             ((((unsigned int)(a)) & SCM_TAG_IMM_MASK_CONST) == SCM_TAG_IMM_FALSE)
#define SCM_TAG_IMM_TRUEP(a)              ((((unsigned int)(a)) & SCM_TAG_IMM_MASK_CONST) == SCM_TAG_IMM_TRUE)
#define SCM_TAG_IMM_EOFP(a)               ((((unsigned int)(a)) & SCM_TAG_IMM_MASK_CONST) == SCM_TAG_IMM_EOF)
#define SCM_TAG_IMM_UNDEFP(a)             ((((unsigned int)(a)) & SCM_TAG_IMM_MASK_CONST) == SCM_TAG_IMM_UNDEF)

/* Type Predicates */
#define SCM_CONSP(a)             (SCM_TAG_CONSP(a))
#define SCM_CLOSUREP(a)          (SCM_TAG_CLOSUREP(a))
#define SCM_SYMBOLP(a)           (SCM_TAG_OTHERS_SYMBOLP(a))
#define SCM_STRINGP(a)           (SCM_TAG_OTHERS_STRINGP(a))
#define SCM_VECTORP(a)           (SCM_TAG_OTHERS_VECTORP(a))
#define SCM_VALUEPACKETP(a)      (SCM_TAG_OTHERS_VALUESP(a))
#define SCM_FUNCP(a)             (SCM_TAG_OTHERS_FUNCP(a))
#define SCM_PORTP(a)             (SCM_TAG_OTHERS_PORTP(a))
#define SCM_CONTINUATIONP(a)     (SCM_TAG_OTHERS_CONTINUATIONP(a))
#define SCM_C_POINTERP(a)        (SCM_TAG_OTHERS_C_POINTERP(a))
#define SCM_C_FUNCPOINTERP(a)    (SCM_TAG_OTHERS_C_FUNC_POINTERP(a))
#define SCM_FREECELLP(a)         (SCM_TAG_OTHERS_FREECELLP(a))
#define SCM_INTP(a)              (SCM_TAG_IMM_INTP(a))
#define SCM_CHARP(a)             (SCM_TAG_IMM_CHARP(a))

/*=======================================
   Type Confirmation
=======================================*/
#if SCM_ACCESSOR_ASSERT
#define SCM_ASSERT_TYPE(cond, a) (SCM_ASSERT(cond), SCM_GET_VALUE((a)))
#else
#define SCM_ASSERT_TYPE(cond, a) (SCM_GET_VALUE((a)))
#endif /* SCM_ACCESSOR_ASSERT */
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

/*=======================================
   Entyping Macros
=======================================*/
#define SCM_ENTYPE_SYMBOL(a)          (a = (ScmObj)SCM_TAG_OTHERS_SYMBOL)
#define SCM_ENTYPE_STRING(a)          (a = (ScmObj)SCM_TAG_OTHERS_STRING)
#define SCM_ENTYPE_VECTOR(a)          (a = (ScmObj)SCM_TAG_OTHERS_VECTOR)
#define SCM_ENTYPE_VALUES(a)          (a = (ScmObj)SCM_TAG_OTHERS_VALUES)
#define SCM_ENTYPE_FUNC(a)            (a = (ScmObj)SCM_TAG_OTHERS_FUNC)
#define SCM_ENTYPE_PORT(a)            (a = (ScmObj)SCM_TAG_OTHERS_PORT)
#define SCM_ENTYPE_CONTINUATION(a)    (a = (ScmObj)SCM_TAG_OTHERS_CONTINUATION)
#define SCM_ENTYPE_C_POINTER(a)       (a = (ScmObj)SCM_TAG_OTHERS_C_POINTER)
#define SCM_ENTYPE_C_FUNC_POINTER(a)  (a = (ScmObj)SCM_TAG_OTHERS_C_FUNC_POINTERP)
#define SCM_ENTYPE_FREECELL(a)        (a = (ScmObj)SCM_TAG_OTHERS_FREECELL)
#define SCM_ENTYPE_INT(a)             (a = (ScmObj)SCM_TAG_IMM_INT)
#define SCM_ENTYPE_CHAR(a)            (a = (ScmObj)SCM_TAG_IMM_CHAR)
#define SCM_ENTYPE_NULL(a)            (a = (ScmObj)SCM_TAG_IMM_NULL)
#define SCM_ENTYPE_INVALID(a)         (a = (ScmObj)SCM_TAG_IMM_INVALID)
#define SCM_ENTYPE_UNBOUND(a)         (a = (ScmObj)SCM_TAG_IMM_UNBOUND)
#define SCM_ENTYPE_FALSE(a)           (a = (ScmObj)SCM_TAG_IMM_FALSE)
#define SCM_ENTYPE_TRUE(a)            (a = (ScmObj)SCM_TAG_IMM_TRUE)
#define SCM_ENTYPE_EOF(a)             (a = (ScmObj)SCM_TAG_IMM_EOF)
#define SCM_ENTYPE_UNDEF(a)           (a = (ScmObj)SCM_TAG_IMM_UNDEF)

/*=======================================
   Real Accessors
=======================================*/
#define SCM_GET_AS_OBJ(a)         ((ScmObj)(((unsigned int)(a)) & SCM_VALUE_MASK))
#define SCM_GET_AS_INT(a, offset) ((int)(((unsigned int)(a)) >> offset))
#define SCM_GET_AS_PTR(a, mask)   ((void*)(((unsigned int)(a)) & mask))
#define SCM_GET_AS_STR(a, mask)   ((char*)SCM_GET_AS_PTR(a, mask))
#define SCM_SET_AS_OBJ(a, b)      (a = (ScmObj)(((unsigned int)(a)) & SCM_GCBIT_MASK) | (((unsigned int)b) & ~SCM_GCBIT_MASK))
#define SCM_SET_AS_INT(a, val, offset, tag) (a = (ScmObj)(tag | (val << offset)))
#define SCM_SET_AS_PTR(a, val, tag)         (a = (ScmObj)(tag | val))
#define SCM_SET_AS_STR(a, val, tag)         (SCM_SET_AS_PTR(a, val, tag))

#define SCM_CAR(a)                     (SCM_AS_CONS(a)->car)
#define SCM_CDR(a)                     (SCM_AS_CONS(a)->cdr)
#define SCM_CONS_SET_CAR(a, car)       (SCM_SET_AS_OBJ(SCM_CAR(a), car))
#define SCM_CONS_SET_CDR(a, cdr)       (SCM_SET_AS_OBJ(SCM_CDR(a), cdr))
#define SCM_CAAR(a)                    (SCM_CAR(SCM_CAR(a)))
#define SCM_CADR(a)                    (SCM_CAR(SCM_CDR(a)))
#define SCM_CDAR(a)                    (SCM_CDR(SCM_CAR(a)))
#define SCM_CDDR(a)                    (SCM_CDR(SCM_CDR(a)))

#define SCM_CLOSURE_EXP(a)             (SCM_AS_CLOSURE(a)->car)
#define SCM_CLOSURE_ENV(a)             (SCM_AS_CLOSURE(a)->cdr)
#define SCM_CLOSURE_SET_EXP(a, exp)    (SCM_SET_AS_OBJ(SCM_CLOSURE_EXP(a), exp))
#define SCM_CLOSURE_SET_ENV(a, exp)    (SCM_SET_AS_OBJ(SCM_CLOSURE_EXP(a), env))

#define SCM_SYMBOL_VCELL(a)            (SCM_AS_SYMBOL(a)->car)
#define SCM_SYMBOL_NAME(a)             (SCM_GET_AS_STR(SCM_AS_SYMBOL(a)->cdr, ~SCM_TAG_OTHERS_MASK_SYMBOL))
#define SCM_SYMBOL_SET_VCELL(a, vcell) (SCM_SET_AS_OBJ(SCM_SYMBOL_VCELL(a), vcell))
#define SCM_SYMBOL_SET_NAME(a, name)   (SCM_SET_AS_STR(SCM_AS_SYMBOL(a)->cdr, name, SCM_TAG_OTHERS_SYMBOL))

#define SCM_STRING_LEN(a)              (SCM_GET_AS_INT(SCM_AS_STRING(a)->car, SCM_TAG_OTHERS_VALUE_OFFSET_STRING))
#define SCM_STRING_STR(a)              (SCM_GET_AS_STR(SCM_AS_STRING(a)->cdr, ~SCM_TAG_OTHERS_MASK_STRING))
#define SCM_STRING_SET_LEN(a, len)     (SCM_SET_AS_INT(SCM_AS_STRING(a)->car, len, SCM_TAG_OTHERS_VALUE_OFFSET_STRING, SCM_TAG_OTHERS_STRING))
#define SCM_STRING_SET_STR(a, str)     (SCM_SET_AS_STR(SCM_AS_STRING(a)->cdr, str, SCM_TAG_OTHERS_STRING))

#define SCM_VECTOR_VEC(a)              (SCM_GET_AS_PTR(SCM_AS_VECTOR(a)->car, ~SCM_TAG_OTHERS_MASK_VECTOR))
#define SCM_VECTOR_LEN(a)              (SCM_GET_AS_INT(SCM_AS_VECTOR(a)->cdr, SCM_TAG_OTHERS_VALUE_OFFSET_VECTOR))
#define SCM_VECTOR_SET_VEC(a, vec)     (SCM_SET_AS_PTR(SCM_AS_VECTOR(a)->car, vec, SCM_TAG_OTHERS_VECTOR))
#define SCM_VECTOR_SET_LEN(a, len)     (SCM_SET_AS_INT(a, len, SCM_TAG_OTHERS_VALUE_OFFSET_VECTOR, SCM_TAG_OTHERS_VECTOR))
#define SCM_VECTOR_CREF(a, idx)        (SCM_VECTOR_VEC(a)[idx])
#define SCM_VECTOR_SET_CREF(a, idx, b) (SCM_VECTOR_CREF((a), (idx)) = (b))
#define SCM_VECTOR_REF(a, idx)         (SCM_VECTOR_CREF((a), SCM_INT_VALUE(idx)))
#define SCM_VECTOR_SET_REF(a, idx, b)  (SCM_VECTOR_REF((a), (idx)) = (b))

#define SCM_VALUEPACKET_VALUES(a)        (SCM_AS_VALUEPACKET(a)->car)
#define SCM_VALUEPACKET_SET_VALUES(a, v) (SCM_SET_AS_OBJ(SCM_AS_VALUEPACKET(a)-X, v))

#define SCM_FUNC_CFUNC(a)              ((ScmFuncType)SCM_GET_AS_PTR(SCM_AS_FUNC(a)->car, ~SCM_TAG_OTHERS_MASK_FUNC))
#define SCM_FUNC_TYPECODE(a)           ((ScmFuncTypeCode)SCM_GET_AS_INT(SCM_AS_FUNC(a)->cdr, SCM_TAG_OTHERS_VALUE_OFFSET_FUNC))
#define SCM_FUNC_SET_CFUNC(a, fptr)    (SCM_SET_AS_PTR(SCM_AS_FUNC(a)->car, fptr, SCM_TAG_OTHERS_FUNC))
#define SCM_FUNC_SET_TYPECODE(a, code) (SCM_SET_AS_INT(SCM_AS_FUNC(a)->cdr, code, SCM_TAG_OTHERS_VALUE_OFFSET_FUNC, SCM_TAG_OTHERS_FUNC))

#define SCM_PORT_PORTINFO(a)                (SCM_GET_AS_PTR(SCM_AS_PORT(a)->car, ~SCM_TAG_OTHERS_MASK_PORT))
#define SCM_PORT_PORTDIRECTION(a)           (SCM_GET_AS_INT(SCM_AS_PORT(a)->cdr, SCM_TAG_OTHERS_VALUE_OFFSET_PORT))
#define SCM_PORT_SET_PORTINFO(a, info)      (SCM_SET_AS_PTR(SCM_AS_PORT(a)->car, info, SCM_TAG_OTHERS_PORT))
#define SCM_PORT_SET_PORTDIRECTION(a, dir)  (SCM_SET_AS_INT(SCM_AS_PORT(a)->cdr, dir, SCM_TAG_OTHERS_VALUE_OFFSET_PORT, SCM_TAG_OTHERS_PORT))

#define SCM_PORT_PORTTYPE(a)                (SCM_PORT_PORTINFO(a)->port_type)
#define SCM_PORT_SET_PORTTYPE(a, type)      (SCM_PORT_PORTTYPE(a) = type)
#define SCM_PORT_UNGOTTENCHAR(a)            (SCM_PORT_PORTINFO(a)->ungottenchar)
#define SCM_PORT_SET_UNGOTTENCHAR(a, ch)    (SCM_PORT_UNGOTTENCHAR(a) = ch)
#define SCM_PORT_GETC_FUNC(a)               (SCM_PORT_PORTINFO(a)->getc_func)
#define SCM_PORT_SET_GETC_FUNC(a, func)     (SCM_PORT_GETC_FUNC(a) = func)
#define SCM_PORT_PRINT_FUNC(a)              (SCM_PORT_PORTINFO(a)->print_func)
#define SCM_PORT_SET_PRINT_FUNC(a, func)    (SCM_PORT_PRINT_FUNC(a) = func)
/* File Port */
#define SCM_PORT_FILE(a)                    (SCM_PORT_PORTINFO(a)->info.file_port.file)
#define SCM_PORT_SET_FILE(a, file)          (SCM_PORT_FILE(a) = file)
#define SCM_PORT_FILENAME(a)                (SCM_PORT_PORTINFO(a)->info.file_port.filename)
#define SCM_PORT_SET_FILENAME(a, filename)  (SCM_PORT_FILENAME(a) = filename)
#define SCM_PORT_LINE(a)                    (SCM_PORT_PORTINFO(a)->info.file_port.line)
#define SCM_PORT_SET_LINE(a, line)          (SCM_PORT_LINE(a) = line)
/* String Port */
#define SCM_PORT_STR(a)                     (SCM_PORT_PORTINFO(a)->info.str_port.port_str)
#define SCM_PORT_SET_STR(a, str)            (SCM_PORT_STR(a) = str)
#define SCM_PORT_STR_CURRENTPOS(a)          (SCM_PORT_PORTINFO(a)->info.str_port.str_currentpos)
#define SCM_PORT_SET_STR_CURRENTPOS(a, pos) (SCM_PORT_STR_CURRENTPOS(a) = pos)

#define SCM_CONTINUATION_ENV(a)             (SCM_GET_AS_PTR(a, ~SCM_TAG_OTHERS_MASK_CONTINUATION))
#define SCM_CONTINUATION_JMPENV(a)          (SCM_CONTINUATION_ENV(a)->jmpenv)
#define SCM_CONTINUATION_DYNEXT(a)          (SCM_CONTINUATION_ENV(a)->dynext)
#define SCM_CONTINUATION_SET_ENV(a, env)    (SCM_SET_AS_PTR(a, env, SCM_TAG_OTHERS_CONTINUATION))
#define SCM_CONTINUATION_SET_JMPENV(a, jmp) (SCM_CONTINUATION_JMPENV(a) = jmp)
#define SCM_CONTINUATION_SET_DYNEXT(a, ext) (SCM_CONTINUATION_DYNEXT(a) = ext)

#define SCM_INT_VALUE(a)               (SCM_GET_AS_INT(a, SCM_TAG_IMM_VALUE_OFFSET_INT))
#define SCM_INT_SET_VALUE(a, val)      (SCM_SET_AS_INT(a, val, SCM_TAG_IMM_VALUE_OFFSET_INT, SCM_TAG_IMM_INT))

#define SCM_CHAR_VALUE(a)              (SCM_GET_AS_STR(a, ~SCM_TAG_IMM_MASK_CHAR))
#define SCM_CHAR_SET_VALUE(a, ch)      (SCM_SET_AS_STR(a, ch, SCM_TAG_IMM_CHAR))

/*=======================================
   Scheme Special Constants
=======================================*/
#define SCM_EOF        SigScm_eof
#define SCM_UNDEF      SigScm_undef
#define SCM_INVALID    SigScm_invalid
#define SCM_UNBOUND    SigScm_unbound
#define SCM_FALSE      SigScm_false
#define SCM_TRUE       SigScm_true

#define SCM_EQ(a, b)   ((a) == (b))
#define SCM_NULLP(a)   (SCM_EQ((a),  SCM_NULL))
#define SCM_FALSEP(a)  (SCM_EQ((a),  SCM_FALSE))
#define SCM_NFALSEP(a) (!SCM_EQ((a), SCM_FALSE))
#define SCM_EOFP(a)    (SCM_EQ((a),  SCM_EOF))

/*============================================================================
  Predefined Symbols
============================================================================*/
/* for list construction */
/*
 * TODO:
 * - Rename to SCM_SYM_* to indicate that these macro are not pointing to
 *   syntax but symbol
 */
#define SCM_QUOTE            SigScm_quote
#define SCM_QUASIQUOTE       SigScm_quasiquote
#define SCM_UNQUOTE          SigScm_unquote
#define SCM_UNQUOTE_SPLICING SigScm_unquote_splicing

/*============================================================================
  Internal Declarations For Special Constants And Predefined Symbols
============================================================================*/
/*
 * These declarations are dedicated to internal use. libsscm users MUST NOT
 * refer these internal representations directly.
 *
 * It may be changed when SigScheme's internal storage model or accessing
 * method for the constants has been changed. To avoid suffering code
 * incompatibility from it, use the abstract macro such as SCM_NULL defined
 * above. They safely hides the internal model against such change.
 */
/* datas.c */
extern ScmObj SigScm_null, SigScm_true, SigScm_false, SigScm_eof;
extern ScmObj SigScm_unbound, SigScm_undef;

/* sigscheme.c */
extern ScmObj SigScm_quote, SigScm_quasiquote, SigScm_unquote;
extern ScmObj SigScm_unquote_splicing;

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
#define SCM_REF_CAR(cons) (&SCM_CAR(cons))
#define SCM_REF_CDR(cons) (&SCM_CDR(cons))
#define SCM_DEREF(ref)    (*(ref))
/* RFC: Is there a better name? */
#define SCM_SET(ref, obj) (*(ref) = (obj))

#endif /* __SIGSCMTYPE_COMPACT_H */
