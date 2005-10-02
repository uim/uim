/*===========================================================================
 *  FileName : sigschemetype.h
 *  About    : scheme object type definition
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
#ifndef __SIGSCMTYPE_H
#define __SIGSCMTYPE_H

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
typedef struct ScmObjInternal_ ScmObjInternal;
typedef ScmObjInternal *ScmObj;
typedef ScmObj *ScmRef;
typedef struct _ScmPortInfo ScmPortInfo;
typedef struct ScmEvalState_ ScmEvalState;
typedef ScmObj (*ScmFuncType)();
typedef ScmObj (*ScmFuncType0)(void);
typedef ScmObj (*ScmFuncType1)(ScmObj arg1);
typedef ScmObj (*ScmFuncType2)(ScmObj arg1, ScmObj arg2);
typedef ScmObj (*ScmFuncType3)(ScmObj arg1, ScmObj arg2, ScmObj arg3);
typedef ScmObj (*ScmFuncType4)(ScmObj arg1, ScmObj arg2, ScmObj arg3, ScmObj arg4);
typedef ScmObj (*ScmFuncType5)(ScmObj arg1, ScmObj arg2, ScmObj arg3, ScmObj arg4, ScmObj arg5);
typedef ScmObj (*ScmFuncTypeEvaledList)(ScmObj args, ScmObj env);
typedef ScmObj (*ScmFuncTypeRawList)(ScmObj arglist, ScmObj env);
typedef ScmObj (*ScmFuncTypeRawListTailRec)(ScmObj arglist, ScmEvalState *eval_state);
typedef ScmObj (*ScmFuncTypeRawListWithTailFlag)(ScmObj arglist, ScmEvalState *eval_state);

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
    ScmEtc          = 10,
    ScmValuePacket  = 11,
    ScmFreeCell     = 12,

    ScmCPointer     = 20,
    ScmCFuncPointer = 21
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
            const char *str_current;
        } str_port;
    } info;
    
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

    /* Compatiblility defs.  To be nuked after complete transition. */
    ,
    FUNCTYPE_0 = SCM_PROCEDURE_FIXED | 0,
    FUNCTYPE_1 = SCM_PROCEDURE_FIXED | 1,
    FUNCTYPE_2 = SCM_PROCEDURE_FIXED | 2,
    FUNCTYPE_3 = SCM_PROCEDURE_FIXED | 3,
    FUNCTYPE_4 = SCM_PROCEDURE_FIXED | 4,
    FUNCTYPE_5 = SCM_PROCEDURE_FIXED | 5,
    FUNCTYPE_EVALED_LIST = SCM_PROCEDURE_VARIADIC | 0,
    FUNCTYPE_RAW_LIST = SCM_SYNTAX_VARIADIC | 0,
    FUNCTYPE_RAW_LIST_TAIL_REC = SCM_SYNTAX_VARIADIC,
    FUNCTYPE_RAW_LIST_WITH_TAIL_FLAG = SCM_FUNCTYPE_TAIL_REC | SCM_FUNCTYPE_SYNTAX
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

/* Scheme Object */
struct ScmObjInternal_ {
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
            char *ch;
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
            enum ScmPortDirection port_direction; /* (PORT_INPUT | PORT_OUTPUT) */
            ScmPortInfo *port_info;
        } port;

        struct ScmContinuation {
            void *opaque0;
            void *opaque1;
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
   Accessors For Scheme Objects
=======================================*/
/* ScmObj Global Attribute */
#define SCM_TYPE(a) ((a)->type)
#define SCM_ENTYPE(a, objtype) ((a)->type = (objtype))
#define SCM_MARK(a) ((a)->gcmark)

/* Type Confirmation */
#if SCM_ACCESSOR_ASSERT
#define SCM_ASSERT_TYPE(cond, x) (SCM_ASSERT(cond), (x))
#else
#define SCM_ASSERT_TYPE(cond, x) (x)
#endif /* SCM_ACCESSOR_ASSERT */
#define SCM_AS_INT(a)           (SCM_ASSERT_TYPE(SCM_INTP(a),           (a)))
#define SCM_AS_CONS(a)          (SCM_ASSERT_TYPE(SCM_CONSP(a),          (a)))
#define SCM_AS_SYMBOL(a)        (SCM_ASSERT_TYPE(SCM_SYMBOLP(a),        (a)))
#define SCM_AS_CHAR(a)          (SCM_ASSERT_TYPE(SCM_CHARP(a),          (a)))
#define SCM_AS_STRING(a)        (SCM_ASSERT_TYPE(SCM_STRINGP(a),        (a)))
#define SCM_AS_FUNC(a)          (SCM_ASSERT_TYPE(SCM_FUNCP(a),          (a)))
#define SCM_AS_CLOSURE(a)       (SCM_ASSERT_TYPE(SCM_CLOSUREP(a),       (a)))
#define SCM_AS_VECTOR(a)        (SCM_ASSERT_TYPE(SCM_VECTORP(a),        (a)))
#define SCM_AS_PORT(a)          (SCM_ASSERT_TYPE(SCM_PORTP(a),          (a)))
#define SCM_AS_CONTINUATION(a)  (SCM_ASSERT_TYPE(SCM_CONTINUATIONP(a),  (a)))
#define SCM_AS_VALUEPACKET(a)   (SCM_ASSERT_TYPE(SCM_VALUEPACKETP(a),   (a)))
#define SCM_AS_C_POINTER(a)     (SCM_ASSERT_TYPE(SCM_C_POINTERP(a),     (a)))
#define SCM_AS_C_FUNCPOINTER(a) (SCM_ASSERT_TYPE(SCM_C_FUNCPOINTERP(a), (a)))

/* Real Accessors */
#define SCM_INTP(a)  (SCM_TYPE(a) == ScmInt)
#define SCM_ENTYPE_INT(a)    (SCM_ENTYPE((a), ScmInt))
#define SCM_INT_VALUE(a) (SCM_AS_INT(a)->obj.int_value.value)
#define SCM_INT_SET_VALUE(a, val) (SCM_INT_VALUE(a) = (val))

#define SCM_CONSP(a) (SCM_TYPE(a) == ScmCons)
#define SCM_ENTYPE_CONS(a) (SCM_ENTYPE((a), ScmCons))
#define SCM_CAR(a)   (SCM_AS_CONS(a)->obj.cons.car)
#define SCM_CONS_SET_CAR(a,car)   (SCM_CAR(a) = car)
#define SCM_CDR(a)   (SCM_AS_CONS(a)->obj.cons.cdr)
#define SCM_CONS_SET_CDR(a,cdr)   (SCM_CDR(a) = cdr)
#define SCM_CAAR(a)  (SCM_CAR(SCM_CAR(a)))
#define SCM_CADR(a)  (SCM_CAR(SCM_CDR(a)))
#define SCM_CDAR(a)  (SCM_CDR(SCM_CAR(a)))
#define SCM_CDDR(a)  (SCM_CDR(SCM_CDR(a)))

#define SCM_SYMBOLP(a)      (SCM_TYPE(a) == ScmSymbol)
#define SCM_ENTYPE_SYMBOL(a)    (SCM_ENTYPE((a), ScmSymbol))
#define SCM_SYMBOL_NAME(a)  (SCM_AS_SYMBOL(a)->obj.symbol.sym_name)
#define SCM_SYMBOL_SET_NAME(a, name)   (SCM_SYMBOL_NAME(a) = (name))
#define SCM_SYMBOL_VCELL(a) (SCM_AS_SYMBOL(a)->obj.symbol.v_cell)
#define SCM_SYMBOL_SET_VCELL(a, vcell) (SCM_SYMBOL_VCELL(a) = (vcell))

#define SCM_CHARP(a) (SCM_TYPE(a) == ScmChar)
#define SCM_ENTYPE_CHAR(a) (SCM_ENTYPE((a), ScmChar))
#define SCM_CHAR_VALUE(a) (SCM_AS_CHAR(a)->obj.ch.ch)
#define SCM_CHAR_SET_VALUE(a, chr) (SCM_CHAR_VALUE(a) = (chr))

#define SCM_STRINGP(a) (SCM_TYPE(a) == ScmString)
#define SCM_ENTYPE_STRING(a)  (SCM_ENTYPE((a), ScmString))
#define SCM_STRING_STR(a) (SCM_AS_STRING(a)->obj.string.str)
#define SCM_STRING_SET_STR(a, str) (SCM_STRING_STR(a) = (str))
#define SCM_STRING_LEN(a) (SCM_AS_STRING(a)->obj.string.len)
#define SCM_STRING_SET_LEN(a, len) (SCM_STRING_LEN(a) = (len))

#define SCM_FUNCP(a) (SCM_TYPE(a) == ScmFunc)
#define SCM_ENTYPE_FUNC(a)     (SCM_ENTYPE((a), ScmFunc))
#define SCM_FUNC_TYPECODE(a) (SCM_AS_FUNC(a)->obj.func.type)
#define SCM_FUNC_SET_TYPECODE(a, type) (SCM_FUNC_TYPECODE(a) = (type))
#define SCM_FUNC_CFUNC(a)   (SCM_AS_FUNC(a)->obj.func.func)
#define SCM_FUNC_SET_CFUNC(a, func)     (SCM_FUNC_CFUNC(a) = (ScmFuncType)(func))
#define SCM_SYNTAXP(a) (SCM_FUNCP(a)                                         \
                        && (SCM_FUNC_TYPECODE(a) & SCM_FUNCTYPE_SYNTAX))
#define SCM_PROCEDUREP(a) ((SCM_FUNCP(a)                                     \
                            && !(SCM_FUNC_TYPECODE(a) & SCM_FUNCTYPE_SYNTAX)) \
                           || SCM_CLOSUREP(a)                                \
                           || SCM_CONTINUATIONP(a))

#define SCM_CLOSUREP(a) (SCM_TYPE(a) == ScmClosure)
#define SCM_ENTYPE_CLOSURE(a) (SCM_ENTYPE((a), ScmClosure))
#define SCM_CLOSURE_EXP(a) (SCM_AS_CLOSURE(a)->obj.closure.exp)
#define SCM_CLOSURE_SET_EXP(a, exp) (SCM_CLOSURE_EXP(a) = exp)
#define SCM_CLOSURE_ENV(a) (SCM_AS_CLOSURE(a)->obj.closure.env)
#define SCM_CLOSURE_SET_ENV(a, env) (SCM_CLOSURE_ENV(a) = env)

#define SCM_VECTORP(a) (SCM_TYPE(a) == ScmVector)
#define SCM_ENTYPE_VECTOR(a) (SCM_ENTYPE((a), ScmVector))
#define SCM_VECTOR_VEC(a) (SCM_AS_VECTOR(a)->obj.vector.vec)
#define SCM_VECTOR_SET_VEC(a, vec) (SCM_VECTOR_VEC(a) = (vec))
#define SCM_VECTOR_LEN(a) (SCM_AS_VECTOR(a)->obj.vector.len)
#define SCM_VECTOR_SET_LEN(a, len) (SCM_VECTOR_LEN(a) = (len))
#define SCM_VECTOR_CREF(a, idx) (SCM_VECTOR_VEC(a)[idx])
#define SCM_VECTOR_SET_CREF(a, idx, b) (SCM_VECTOR_CREF((a), (idx)) = (b))
#define SCM_VECTOR_REF(a, idx)  (SCM_VECTOR_CREF((a), SCM_INT_VALUE(idx)))
#define SCM_VECTOR_SET_REF(a, idx, b)  (SCM_VECTOR_REF((a), (idx)) = (b))
#define SCM_VECTOR_CHECK_IDX(a, idx) ()

#define SCM_PORTP(a) (SCM_TYPE(a) == ScmPort)
#define SCM_ENTYPE_PORT(a) (SCM_ENTYPE((a), ScmPort))
#define SCM_PORT_PORTDIRECTION(a) (SCM_AS_PORT(a)->obj.port.port_direction)
#define SCM_PORT_SET_PORTDIRECTION(a, pdirection) (SCM_PORT_PORTDIRECTION(a) = pdirection)
#define SCM_PORT_PORTINFO(a) (SCM_AS_PORT(a)->obj.port.port_info)
#define SCM_PORT_SET_PORTINFO(a, pinfo) (SCM_PORT_PORTINFO(a) = (pinfo))
#define SCM_PORTINFO_PORTTYPE(a) (SCM_PORT_PORTINFO(a)->port_type)
#define SCM_PORTINFO_FILE(a) (SCM_PORT_PORTINFO(a)->info.file_port.file)
#define SCM_PORTINFO_FILENAME(a) (SCM_PORT_PORTINFO(a)->info.file_port.filename)
#define SCM_PORTINFO_LINE(a) (SCM_PORT_PORTINFO(a)->info.file_port.line)
#define SCM_PORTINFO_STR(a) (SCM_PORT_PORTINFO(a)->info.str_port.port_str)
#define SCM_PORTINFO_STR_CURRENT(a) (SCM_PORT_PORTINFO(a)->info.str_port.str_current)
#define SCM_PORTINFO_UNGOTTENCHAR(a) (SCM_PORT_PORTINFO(a)->ungottenchar)

#define SCM_CONTINUATIONP(a) (SCM_TYPE(a) == ScmContinuation)
#define SCM_ENTYPE_CONTINUATION(a) (SCM_ENTYPE((a), ScmContinuation))
#define SCM_CONTINUATION_OPAQUE0(a) (SCM_AS_CONTINUATION(a)->obj.continuation.opaque0)
#define SCM_CONTINUATION_SET_OPAQUE0(a, val) (SCM_CONTINUATION_OPAQUE0(a) = (val))
#define SCM_CONTINUATION_OPAQUE1(a) (SCM_AS_CONTINUATION(a)->obj.continuation.opaque1)
#define SCM_CONTINUATION_SET_OPAQUE1(a, val) (SCM_CONTINUATION_OPAQUE1(a) = (val))

#if SCM_USE_VALUECONS
/* to modify a VALUECONS, rewrite its type to cons by SCM_ENTYPE_CONS(vcons) */
#define SCM_VALUEPACKETP(a)       (SCM_TYPE(a) == ScmValuePacket)
#define SCM_NULLVALUESP(a)        (EQ(a, SigScm_null_values))
#define SCM_ENTYPE_VALUEPACKET(a) (SCM_ENTYPE((a), ScmValuePacket))
#define SCM_VALUEPACKET_VALUES(a) ((SCM_NULLVALUESP(a)) ? SCM_NULL :         \
                                   (SCM_CONS(SCM_VALUECONS_CAR(a),           \
                                             SCM_VALUECONS_CDR(a))))
#define SCM_VALUECONS_CAR(a)      (SCM_AS_VALUEPACKET(a)->obj.cons.car)
#define SCM_VALUECONS_CDR(a)      (SCM_AS_VALUEPACKET(a)->obj.cons.cdr)
#else /* SCM_USE_VALUECONS */
#define SCM_VALUEPACKETP(a)          (SCM_TYPE(a) == ScmValuePacket)
#define SCM_ENTYPE_VALUEPACKET(a)        (SCM_ENTYPE((a), ScmValuePacket))
#define SCM_VALUEPACKET_VALUES(a)    (SCM_AS_VALUEPACKET(a)->obj.value_pack.values)
#define SCM_VALUEPACKET_SET_VALUES(a, v) (SCM_VALUEPACKET_VALUES(a) = (v))
#endif /* SCM_USE_VALUECONS */

/*============================================================================
  Etcetra Variables (Special Constants like SCM_NULL)
============================================================================*/
#define SCM_ETCP(a) (SCM_TYPE(a) == ScmEtc)

/*============================================================================
  C Pointer Object
============================================================================*/
#define SCM_C_POINTERP(a) (SCM_TYPE(a) == ScmCPointer)
#define SCM_ENTYPE_C_POINTER(a) (SCM_ENTYPE((a), ScmCPointer))
#define SCM_C_POINTER_VALUE(a) (SCM_AS_C_POINTER(a)->obj.c_pointer.data)
#define SCM_C_POINTER_SET_VALUE(a, ptr) (SCM_C_POINTER_VALUE(a) = ptr)

#define SCM_C_FUNCPOINTERP(a) (SCM_TYPE(a) == ScmCFuncPointer)
#define SCM_ENTYPE_C_FUNCPOINTER(a) (SCM_ENTYPE((a), ScmCFuncPointer))
#define SCM_C_FUNCPOINTER_VALUE(a) (SCM_AS_C_FUNCPOINTER(a)->obj.c_func_pointer.func)
#define SCM_C_FUNCPOINTER_SET_VALUE(a, funcptr) (SCM_C_FUNCPOINTER_VALUE(a) = funcptr)

/*============================================================================
  Abstract ScmObj Reference For Storage-Representation Independent Efficient
  List Operations
============================================================================*/
#define SCM_REF_CAR(cons) (&SCM_CAR(cons))
#define SCM_REF_CDR(cons) (&SCM_CDR(cons))
#define SCM_DEREF(ref)    (*(ref))
/* RFC: Is there a better name? */
#define SCM_SET(ref, obj) (*(ref) = (obj))

/*============================================================================
  Special Constants
============================================================================*/
#define SCM_NULL             SigScm_null
#define SCM_TRUE             SigScm_true
#define SCM_FALSE            SigScm_false
#define SCM_EOF              SigScm_eof
#define SCM_QUOTE            SigScm_quote
#define SCM_QUASIQUOTE       SigScm_quasiquote
#define SCM_UNQUOTE          SigScm_unquote
#define SCM_UNQUOTE_SPLICING SigScm_unquote_splicing
#define SCM_UNBOUND          SigScm_unbound
#define SCM_UNDEF            SigScm_undef

#define SCM_EQ(a, b)   ((a) == (b))
#define SCM_NEQ(a, b)  ((a) != (b))
#define SCM_NULLP(a)   (SCM_EQ((a),  SCM_NULL))
#define SCM_NNULLP(a)  (SCM_NEQ((a), SCM_NULL))
#define SCM_FALSEP(a)  (SCM_EQ((a),  SCM_FALSE))
#define SCM_NFALSEP(a) (SCM_NEQ((a), SCM_FALSE))
#define SCM_EOFP(a)    (SCM_EQ((a),  SCM_EOF))

/*============================================================================
  Internal Declarations For Special Constants
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
extern ScmObj SigScm_null, SigScm_true, SigScm_false, SigScm_eof;
extern ScmObj SigScm_unbound, SigScm_undef;

#endif /* __SIGSCMTYPE_H */
