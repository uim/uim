/*===========================================================================
 *  FileName : sigschemeinternal.h
 *  About    : variable and function definitions for internal use
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
#ifndef __SIGSCHEMEINTERNAL_H
#define __SIGSCHEMEINTERNAL_H

/*=======================================
   System Include
=======================================*/
#include "sigscheme.h"

/*=======================================
   Local Include
=======================================*/

/*=======================================
   Struct Declarations
=======================================*/
/* for debugging */
struct trace_frame {
    struct trace_frame *prev;
    ScmObj obj;
};

/*=======================================
   Variable Declarations
=======================================*/
/* datas.c */
extern ScmObj *scm_stack_start_pointer;

/* error.c*/
extern ScmObj scm_current_error_port;

/* eval.c */
extern ScmObj scm_continuation_thrown_obj;
extern ScmObj scm_letrec_env;
extern struct trace_frame *scm_trace_root;

/* io.c */
extern ScmObj scm_current_input_port;
extern ScmObj scm_current_output_port;
extern ScmObj SigScm_features;


/*=======================================
   Macro Declarations
=======================================*/
/* Debugging Flags */
#define DEBUG_PARSER  0
#define DEBUG_GC      0

/* FreeCell Handling Macros */
#define SCM_FREECELLP(a)     (SCM_TYPE(a) == ScmFreeCell)
#define SCM_FREECELL(a)      (sigassert(SCM_FREECELLP(a)), (a))
#define SCM_FREECELL_CAR(a)  (SCM_FREECELL(a)->obj.cons.car)
#define SCM_FREECELL_CDR(a)  (SCM_FREECELL(a)->obj.cons.cdr)
#define SCM_ENTYPE_FREECELL(a)     (SCM_ENTYPE((a), ScmFreeCell))
#define SCM_SETFREECELL_CAR(a,car) (SCM_FREECELL_CAR(a) = car)
#define SCM_SETFREECELL_CDR(a,cdr) (SCM_FREECELL_CDR(a) = cdr)

/* Internal Macros with no prefix */
#define EQ             SCM_EQ
#define NEQ            SCM_NEQ
#define NULLP          SCM_NULLP
#define NNULLP         SCM_NNULLP
#define FALSEP         SCM_FALSEP
#define NFALSEP        SCM_NFALSEP
#define EOFP           SCM_EOFP

#define CAR            SCM_CAR
#define CDR            SCM_CDR
#define SET_CAR        SCM_CONS_SET_CAR
#define SET_CDR        SCM_CONS_SET_CDR
#define CAAR           SCM_CAAR
#define CADR           SCM_CADR
#define CDAR           SCM_CDAR
#define CDDR           SCM_CDDR

#define INTP           SCM_INTP
#define CONSP          SCM_CONSP
#define SYMBOLP        SCM_SYMBOLP
#define CHARP          SCM_CHARP
#define STRINGP        SCM_STRINGP
#define FUNCP          SCM_FUNCP
#define CLOSUREP       SCM_CLOSUREP
#define VECTORP        SCM_VECTORP
#define PORTP          SCM_PORTP
#define CONTINUATIONP  SCM_CONTINUATIONP
#define VALUEPACKETP   SCM_VALUEPACKETP
#define FREECELLP      SCM_FREECELLP
#define C_POINTERP     SCM_C_POINTERP
#define C_FUNCPOINTERP SCM_C_FUNCPOINTERP

/*
 * Abbrev name for these constants are not provided since it involves some
 * consistency problems and confusions. Use the canonical names always.
 *
 * SCM_NULL
 * SCM_TRUE
 * SCM_FALSE
 * SCM_EOF
 * SCM_QUOTE
 * SCM_QUASIQUOTE
 * SCM_UNQUOTE
 * SCM_UNQUOTE_SPLICING
 * SCM_UNBOUND
 * SCM_UNDEF
 */

/* Macros For Argnument Number Checking */
#define CHECK_1_ARG(arg) \
    (SCM_NULLP(arg))
#define CHECK_2_ARGS(arg) \
    (SCM_NULLP(arg) || SCM_NULLP(SCM_CDR(arg)))
#define CHECK_3_ARGS(arg) \
    (SCM_NULLP(arg) || SCM_NULLP(SCM_CDR(arg)) || SCM_NULLP(SCM_CDR(SCM_CDR(arg))))
#define CHECK_4_ARGS(arg) \
    (SCM_NULLP(arg) || SCM_NULLP(SCM_CDR(arg)) || SCM_NULLP(SCM_CDR(SCM_CDR(arg))) \
     || SCM_NULLP(SCM_CDR(SCM_CDR(SCM_CDR(arg)))))
#define CHECK_5_ARGS(arg) \
    (SCM_NULLP(arg) || SCM_NULLP(SCM_CDR(arg)) || SCM_NULLP(SCM_CDR(SCM_CDR(arg))) \
     || SCM_NULLP(SCM_CDR(SCM_CDR(SCM_CDR(arg)))) || SCM_NULLP(SCM_CDR(SCM_CDR(SCM_CDR(SCM_CDR(arg))))))

/* Macros For Creating List */
#define SCM_LIST_1(obj1) \
    (Scm_NewCons((obj1), SCM_NIL))
#define SCM_LIST_2(obj1, obj2) \
    (Scm_NewCons((obj1), \
                 Scm_NewCons((obj2), SCM_NIL)))
#define SCM_LIST_3(obj1, obj2, obj3) \
    (Scm_NewCons((obj1), \
                 Scm_NewCons((obj2), \
                             Scm_NewCons((obj3), SCM_NIL))))
#define SCM_LIST_4(obj1, obj2, obj3, obj4) \
    (Scm_NewCons((obj1), \
                 Scm_NewCons((obj2), \
                             Scm_NewCons((obj3), \
                                         Scm_NewCons((obj4), SCM_NIL)))))
#define SCM_LIST_5(obj1, obj2, obj3, obj4, obj5) \
    (Scm_NewCons((obj1), \
                 Scm_NewCons((obj2), \
                             Scm_NewCons((obj3), \
                                         Scm_NewCons((obj4), \
                                                     Scm_NewCons((obj5), SCM_NIL))))))

/*=======================================
   Function Declarations
=======================================*/
/* eval.c */
/* environment related functions */
ScmObj extend_environment(ScmObj vars, ScmObj vals, ScmObj env);
ScmObj add_environment(ScmObj var, ScmObj val, ScmObj env);
ScmObj lookup_environment(ScmObj var, ScmObj env);
ScmObj lookup_frame(ScmObj var, ScmObj frame);
ScmObj symbol_value(ScmObj var, ScmObj env);


#endif /* __SIGSCHEMEINTERNAL_H */
