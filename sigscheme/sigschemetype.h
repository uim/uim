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
#include <setjmp.h>

/*=======================================
   Local Include
=======================================*/

/*=======================================
   Struct Declarations
=======================================*/
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
    ScmFreeCell     = 10,
    ScmEtc          = 11,

    ScmCPointer     = 20,
    ScmCFuncPointer = 21
};

/* Function Type by argnuments */
enum ScmFuncArgType {
    FUNCTYPE_0  = 0, /* no arg */
    FUNCTYPE_1  = 1, /* require 1 arg  */
    FUNCTYPE_2  = 2, /* require 2 args */
    FUNCTYPE_3  = 3, /* require 3 args */
    FUNCTYPE_4  = 4, /* require 4 args */
    FUNCTYPE_5  = 5, /* require 5 args */
    FUNCTYPE_L  = 6, /* all args are already evaluated, and pass the arg-list to the func*/
    FUNCTYPE_R  = 7, /* all args are "not" evaluated */
    FUNCTYPE_2N = 9  /* all args are evaluated with each 2 objs */
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
typedef struct _ScmPortInfo ScmPortInfo;
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
    
    char ungottenchar;
};

typedef struct _ScmContInfo ScmContInfo;
struct _ScmContInfo {
    jmp_buf jmp_env;
};


/* Scheme Object */
typedef struct ScmObjInternal_ ScmObjInternal;
typedef ScmObjInternal *ScmObj;
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
            union {
                struct {
                    ScmObj (*func) (void);
                } subr0;

                struct {
                    ScmObj (*func) (ScmObj);
                } subr1;

                struct {
                    ScmObj (*func) (ScmObj, ScmObj);
                } subr2;

                struct {
                    ScmObj (*func) (ScmObj, ScmObj, ScmObj);
                } subr3;

                struct {
                    ScmObj (*func) (ScmObj, ScmObj, ScmObj, ScmObj);
                } subr4;

                struct {
                    ScmObj (*func) (ScmObj, ScmObj, ScmObj, ScmObj, ScmObj);
                } subr5;
                
                struct {
                    ScmObj (*func) (ScmObj, ScmObj*, int*);
                } subrr;
                
            } subrs;

            enum ScmFuncArgType num_arg;
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
            ScmContInfo *cont_info;
        } continuation;

        struct ScmEtc {
            int type;
        } etc;

        struct ScmCPointer {
            void *data;            
        } c_pointer;

        struct ScmCFuncPointer {
            C_FUNC func;            
        } c_func_pointer;
    } obj;
};

#define SCM_GETTYPE(a)       ((a)->type)
#define SCM_SETTYPE(a, objtype) ((a)->type = (objtype))
#define SCM_MARK(a) ((a)->gcmark)

#define SCM_INTP(a)  (SCM_GETTYPE(a) == ScmInt)
#define SCM_INT(a)   (sigassert(SCM_INTP(a)), (a))
#define SCM_INT_VALUE(a) (SCM_INT(a)->obj.int_value.value)
#define SCM_SETINT(a)    (SCM_SETTYPE((a), ScmInt))
#define SCM_SETINT_VALUE(a, val) (SCM_INT_VALUE(a) = (val))

#define SCM_CONSP(a) (SCM_GETTYPE(a) == ScmCons)
#define SCM_CONS(a)  (sigassert(SCM_CONSP(a)), (a))
#define SCM_CAR(a)   (SCM_CONS(a)->obj.cons.car)
#define SCM_CDR(a)   (SCM_CONS(a)->obj.cons.cdr)
#define SCM_SETCONS(a) (SCM_SETTYPE((a), ScmCons))
#define SCM_SETCAR(a,car)   (SCM_CAR(a) = car)
#define SCM_SETCDR(a,cdr)   (SCM_CDR(a) = cdr)

#define SCM_FREECELLP(a)     (SCM_GETTYPE(a) == ScmFreeCell)
#define SCM_FREECELL(a)      (sigassert(SCM_FREECELLP(a)), (a))
#define SCM_FREECELL_CAR(a)  (SCM_FREECELL(a)->obj.cons.car)
#define SCM_FREECELL_CDR(a)  (SCM_FREECELL(a)->obj.cons.cdr)
#define SCM_SETFREECELL(a)    (SCM_SETTYPE((a), ScmFreeCell))
#define SCM_SETFREECELL_CAR(a,car) (SCM_FREECELL_CAR(a) = car)
#define SCM_SETFREECELL_CDR(a,cdr) (SCM_FREECELL_CDR(a) = cdr)

#define SCM_SYMBOLP(a)      (SCM_GETTYPE(a) == ScmSymbol)
#define SCM_SYMBOL(a)       (sigassert(SCM_SYMBOLP(a)), (a))
#define SCM_SYMBOL_NAME(a)  (SCM_SYMBOL(a)->obj.symbol.sym_name)
#define SCM_SYMBOL_VCELL(a) (SCM_SYMBOL(a)->obj.symbol.v_cell)
#define SCM_SETSYMBOL(a)    (SCM_SETTYPE((a), ScmSymbol))
#define SCM_SETSYMBOL_NAME(a, name)   (SCM_SYMBOL_NAME(a) = (name))
#define SCM_SETSYMBOL_VCELL(a, vcell) (SCM_SYMBOL_VCELL(a) = (vcell))

#define SCM_CHARP(a) (SCM_GETTYPE(a) == ScmChar)
#define SCM_CHAR(a)  (sigassert(SCM_CHARP(a)), (a))
#define SCM_CHAR_CH(a) (SCM_CHAR(a)->obj.ch.ch)
#define SCM_SETCHAR(a) (SCM_SETTYPE((a), ScmChar))
#define SCM_SETCHAR_CH(a, chr) (SCM_CHAR_CH(a) = (chr))

#define SCM_STRINGP(a) (SCM_GETTYPE(a) == ScmString)
#define SCM_STRING(a)  (sigassert(SCM_STRINGP(a)), (a))
#define SCM_STRING_STR(a) (SCM_STRING(a)->obj.string.str)
#define SCM_STRING_LEN(a) (SCM_STRING(a)->obj.string.len)
#define SCM_SETSTRING(a)  (SCM_SETTYPE((a), ScmString))
#define SCM_SETSTRING_STR(a, str) (SCM_STRING_STR(a) = (str))
#define SCM_SETSTRING_LEN(a, len) (SCM_STRING_LEN(a) = (len))

typedef ScmObj (*ScmFuncType) (void);
#define SCM_FUNCP(a) (SCM_GETTYPE(a) == ScmFunc)
#define SCM_FUNC(a) (sigassert(SCM_FUNCP(a)), (a))
#define SCM_FUNC_NUMARG(a) (SCM_FUNC(a)->obj.func.num_arg)
#define SCM_FUNC_FUNC(a)   (SCM_FUNC(a)->obj.func.subrs.subr0.func)
#define SCM_SETFUNC(a)     (SCM_SETTYPE((a), ScmFunc))
#define SCM_SETFUNC_NUMARG(a, numarg) (SCM_FUNC_NUMARG(a) = (numarg))
#define SCM_SETFUNC_FUNC(a, func)     (SCM_FUNC_FUNC(a) = (ScmFuncType)(func))

#define SCM_FUNC_EXEC_SUBR0(a)                               ((*(a)->obj.func.subrs.subr0.func) ())
#define SCM_FUNC_EXEC_SUBR1(a, arg1)                         ((*(a)->obj.func.subrs.subr1.func) (arg1))
#define SCM_FUNC_EXEC_SUBR2(a, arg1, arg2)                   ((*(a)->obj.func.subrs.subr2.func) ((arg1), (arg2)))
#define SCM_FUNC_EXEC_SUBR3(a, arg1, arg2, arg3)             ((*(a)->obj.func.subrs.subr3.func) ((arg1), (arg2), (arg3)))
#define SCM_FUNC_EXEC_SUBR4(a, arg1, arg2, arg3, arg4)       ((*(a)->obj.func.subrs.subr4.func) ((arg1), (arg2), (arg3), (arg4)))
#define SCM_FUNC_EXEC_SUBR5(a, arg1, arg2, arg3, arg4, arg5) ((*(a)->obj.func.subrs.subr5.func) ((arg1), (arg2), (arg3), (arg4), (arg5)))
#define SCM_FUNC_EXEC_SUBRL(a, arg1, arg2)                   ((*(a)->obj.func.subrs.subr2.func) ((arg1), (arg2)))
#define SCM_FUNC_EXEC_SUBRR(a, arg1, arg2, arg3)             ((*(a)->obj.func.subrs.subrr.func) ((arg1), (arg2), (arg3)))
#define SCM_FUNC_EXEC_SUBR2N(a, arg1, arg2)                  ((*(a)->obj.func.subrs.subr2.func) ((arg1), (arg2)))

#define SCM_CLOSUREP(a) (SCM_GETTYPE(a) == ScmClosure)
#define SCM_CLOSURE(a)  (sigassert(SCM_CLOSUREP(a)), (a))
#define SCM_CLOSURE_EXP(a) (SCM_CLOSURE(a)->obj.closure.exp)
#define SCM_CLOSURE_ENV(a) (SCM_CLOSURE(a)->obj.closure.env)
#define SCM_SETCLOSURE(a) (SCM_SETTYPE((a), ScmClosure))
#define SCM_SETCLOSURE_EXP(a, formals) (SCM_CLOSURE_EXP(a) = exp)
#define SCM_SETCLOSURE_ENV(a, body) (SCM_CLOSURE_ENV(a) = env)

#define SCM_VECTORP(a) (SCM_GETTYPE(a) == ScmVector)
#define SCM_VECTOR(a)  (sigassert(SCM_VECTORP(a)), (a))
#define SCM_VECTOR_VEC(a) (SCM_VECTOR(a)->obj.vector.vec)
#define SCM_VECTOR_LEN(a) (SCM_VECTOR(a)->obj.vector.len)
#define SCM_VECTOR_CHECK_IDX(a, idx) ()
#define SCM_VECTOR_CREF(a, idx) (SCM_VECTOR_VEC(a)[idx])
#define SCM_VECTOR_REF(a, idx)  (SCM_VECTOR_CREF((a), SCM_INT_VALUE(idx)))
#define SCM_SETVECTOR(a) (SCM_SETTYPE((a), ScmVector))
#define SCM_SETVECTOR_VEC(a, vec) (SCM_VECTOR_VEC(a) = (vec))
#define SCM_SETVECTOR_LEN(a, len) (SCM_VECTOR_LEN(a) = (len))
#define SCM_SETVECTOR_CREF(a, idx, b) (SCM_VECTOR_CREF((a), (idx)) = (b))
#define SCM_SETVECTOR_REF(a, idx, b)  (SCM_VECTOR_REF((a), (idx)) = (b))

#define SCM_PORTP(a) (SCM_GETTYPE(a) == ScmPort)
#define SCM_PORT(a)  (sigassert(SCM_PORTP(a)), (a))
#define SCM_PORT_PORTDIRECTION(a) (SCM_PORT(a)->obj.port.port_direction)
#define SCM_PORT_PORTINFO(a) (SCM_PORT(a)->obj.port.port_info)
#define SCM_SETPORT(a) (SCM_SETTYPE((a), ScmPort))
#define SCM_SETPORT_PORTDIRECTION(a, pdirection) (SCM_PORT_PORTDIRECTION(a) = pdirection)
#define SCM_SETPORT_PORTINFO(a, pinfo) (SCM_PORT_PORTINFO(a) = (pinfo))
#define SCM_PORTINFO_PORTTYPE(a) (SCM_PORT_PORTINFO(a)->port_type)
#define SCM_PORTINFO_FILE(a) (SCM_PORT_PORTINFO(a)->info.file_port.file)
#define SCM_PORTINFO_FILENAME(a) (SCM_PORT_PORTINFO(a)->info.file_port.filename)
#define SCM_PORTINFO_LINE(a) (SCM_PORT_PORTINFO(a)->info.file_port.line)
#define SCM_PORTINFO_STR(a) (SCM_PORT_PORTINFO(a)->info.str_port.port_str)
#define SCM_PORTINFO_STR_CURRENT(a) (SCM_PORT_PORTINFO(a)->info.str_port.str_current)
#define SCM_PORTINFO_UNGOTTENCHAR(a) (SCM_PORT_PORTINFO(a)->ungottenchar)

#define SCM_CONTINUATIONP(a) (SCM_GETTYPE(a) == ScmContinuation)
#define SCM_CONTINUATION(a)  (sigassert(SCM_CONTINUATIONP(a)), (a))
#define SCM_CONTINUATION_CONTINFO(a) (SCM_CONTINUATION(a)->obj.continuation.cont_info)
#define SCM_CONTINUATION_JMPENV(a) (SCM_CONTINUATION(a)->obj.continuation.cont_info->jmp_env)
#define SCM_SETCONTINUATION(a) (SCM_SETTYPE((a), ScmContinuation))
#define SCM_SETCONTINUATION_CONTINFO(a, cinfo) (SCM_CONTINUATION_CONTINFO(a) = (cinfo))

/*============================================================================
  Etcetra variables (Special Symbols like NIL)
============================================================================*/
#define SCM_ETCP(a) (SCM_GETTYPE(a) == ScmEtc)
#define SCM_ETC(a) (sigassert(SCM_ETCP(a)), (a))
#define SCM_ETC_TYPE(a) (SCM_ETC(a)->obj.etc.type)
#define SCM_SETETC_TYPE(a, etctype) (SCM_ETC_TYPE(a) = (etctype))
#define SCM_NEW_ETC(a, impl, etctype)     \
    do {                                  \
        (a) = &(impl);                    \
        SCM_SETTYPE((a), ScmEtc);         \
        SCM_SETETC_TYPE((a), (etctype));  \
    } while(0)

/*============================================================================
  For C-Interface
============================================================================*/
#define SCM_C_POINTERP(a) (SCM_GETTYPE(a) == ScmCPointer)
#define SCM_C_POINTER(a)  (sigassert(SCM_C_POINTERP(a)), (a))
#define SCM_C_POINTER_DATA(a) (SCM_C_POINTER(a)->obj.c_pointer.data)
#define SCM_SETC_POINTER(a) (SCM_SETTYPE((a), ScmCPointer))
#define SCM_SETC_POINTER_DATA(a, ptr) (SCM_C_POINTER_DATA(a) = ptr)

#define SCM_C_FUNCPOINTERP(a) (SCM_GETTYPE(a) == ScmCFuncPointer)
#define SCM_C_FUNCPOINTER(a)  (sigassert(SCM_C_FUNCPOINTERP(a)), (a))
#define SCM_C_FUNCPOINTER_FUNC(a) (SCM_C_POINTER(a)->obj.c_func_pointer.func)
#define SCM_SETC_FUNCPOINTER(a) (SCM_SETTYPE((a), ScmCFuncPointer))
#define SCM_SETC_FUNCPOINTER_FUNC(a, funcptr) (SCM_C_FUNCPOINTER_FUNC(a) = funcptr)

extern ScmObj SigScm_nil, SigScm_true, SigScm_false, SigScm_eof;
extern ScmObj SigScm_quote, SigScm_quasiquote, SigScm_unquote, SigScm_unquote_splicing;
extern ScmObj SigScm_unbound, SigScm_unspecified, SigScm_undef;

#define SCM_NIL              SigScm_nil
#define SCM_TRUE             SigScm_true
#define SCM_FALSE            SigScm_false
#define SCM_EOF              SigScm_eof
#define SCM_QUOTE            SigScm_quote
#define SCM_QUASIQUOTE       SigScm_quasiquote
#define SCM_UNQUOTE          SigScm_unquote
#define SCM_UNQUOTE_SPLICING SigScm_unquote_splicing
#define SCM_UNBOUND          SigScm_unbound
#define SCM_UNSPECIFIED      SigScm_unspecified
#define SCM_UNDEF            SigScm_undef

#define EQ(a, b) ((a) == (b))
#define NEQ(a, b) !(EQ((a), (b)))

#define SCM_EQ(a, b)  (EQ((a), (b)))
#define SCM_NEQ(a, b) (NEQ((a), (b)))
#define SCM_NULLP(a)  (EQ((a), SCM_NIL))
#define SCM_TRUEP(a)  (EQ((a), SCM_TRUE))
#define SCM_FALSEP(a) (EQ((a), SCM_FALSE))
#define SCM_EOFP(a)   (EQ((a), SCM_EOF))

#endif /* __SIGSCMTYPE_H */
