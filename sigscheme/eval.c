/*===========================================================================
 *  FileName : eval.c
 *  About    : Evaluation and basic Syntactic Expression
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

/*
 * Descrioption of Environment
 *
 * [1] Data Structure of Environment
 *     Environment is the simple list that is formed as below.
 *
 *     - Frame = (cons (var1 var2 var3 ...)
 *                     (val1 val2 val3 ...))
 *     - Env   = (Frame1 Frame2 Frame3 ...)
 *
 */

/*=======================================
  System Include
=======================================*/

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/
#define EVAL_ARGS          0
#define SUPPRESS_EVAL_ARGS 1

#define IS_LIST_LEN_1(args)  (CONSP(args) && NULLP(CDR(args)))
/* for the quasiquote family */
#define QQUOTE_SET_VERBATIM(x) ((x) = SCM_INVALID)
#define QQUOTE_IS_VERBATIM(x)  (EQ((x), SCM_INVALID))

#define SCM_ERRMSG_WRONG_NR_ARG " Wrong number of arguments "
#define SCM_ERRMSG_NON_R5RS_ENV " the environment is not conformed to R5RS"

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmRef lookup_frame(ScmObj var, ScmObj frame);
static ScmObj reduce(ScmObj (*func)(), ScmObj args, ScmObj env,
                     int suppress_eval);
static ScmObj call_closure(ScmObj proc, ScmObj args, ScmEvalState *eval_state);
static ScmObj call(ScmObj proc, ScmObj args, ScmEvalState *eval_state,
                   int suppress_eval);
static ScmObj map_eval(ScmObj args, ScmObj env);
static void define_internal(ScmObj var, ScmObj exp, ScmObj env);

/* Quasiquotation. */
typedef struct _qquote_result qquote_result;
static qquote_result qquote_internal(ScmObj input, ScmObj env, int nest);
static qquote_result qquote_vector(ScmObj vec, ScmObj env, int nest);

/*=======================================
  Function Implementations
=======================================*/
/**
 * Construct new frame on an env
 *
 * @a vars and @a vals must surely be a list.
 *
 * @param vars Symbol list as variable names of new frame. It accepts dot list
 *             to handle function arguments directly.
 * @param vals Arbitrary Scheme object list as values of new frame. Side
 *             effect: destructively modifyies the vals when vars is a dot
 *             list.
 * @see Scm_eval()
 */
ScmObj Scm_ExtendEnvironment(ScmObj vars, ScmObj vals, ScmObj env)
{
    ScmObj frame, rest_vars, rest_vals;
    DECLARE_INTERNAL_FUNCTION("Scm_ExtendEnvironment");

#if SCM_STRICT_ARGCHECK
    if (!LISTP(env))
        ERR("broken environment");

    for (rest_vars = vars, rest_vals = vals;
         CONSP(rest_vars) && !NULLP(rest_vals);
         rest_vars = CDR(rest_vars), rest_vals = CDR(rest_vals))
    {
        if (!SYMBOLP(CAR(rest_vars)))
            break;
    }
    if (!(NULLP(rest_vars) || SYMBOLP(rest_vars)))
        ERR_OBJ("broken environment extension", rest_vars);
#endif /* SCM_STRICT_ARGCHECK */

    /* create new frame */
    frame = CONS(vars, vals);

    return CONS(frame, env);
}

/** Add a binding to newest frame of an env */
ScmObj Scm_AddEnvironment(ScmObj var, ScmObj val, ScmObj env)
{
    ScmObj newest_frame;
    ScmObj new_vars, new_vals;
    DECLARE_INTERNAL_FUNCTION("Scm_AddEnvironment");

    /* sanity check */
    if (!SYMBOLP(var))
        ERR_OBJ("broken environment handling", var);

    /* add (var, val) pair to the newest frame in env */
    if (NULLP(env)) {
        newest_frame = CONS(LIST_1(var), LIST_1(val));
        env = LIST_1(newest_frame);
    } else if (CONSP(env)) {
        newest_frame = CAR(env);
        new_vars = CONS(var, CAR(newest_frame));
        new_vals = CONS(val, CDR(newest_frame));

        SET_CAR(env, CONS(new_vars, new_vals));
    } else {
        ERR_OBJ("broken environent", env);
    }
    return env;
}

/**
 * Lookup a variable of an env
 *
 * @return Reference to the variable. SCM_INVALID_REF if not found.
 */
ScmRef Scm_LookupEnvironment(ScmObj var, ScmObj env)
{
    ScmObj frame;
    ScmRef ref;
    DECLARE_INTERNAL_FUNCTION("Scm_LookupEnvironment");

    /* lookup in frames */
    for (; CONSP(env); env = CDR(env)) {
        frame = CAR(env);
        ref   = lookup_frame(var, frame);
        if (ref != SCM_INVALID_REF)
            return ref;
    }

#if SCM_STRICT_ARGCHECK
    if (!NULLP(env))
        ERR_OBJ("broken environent", env);
#endif

    return SCM_INVALID_REF;
}

/** Lookup a variable of a frame */
static ScmRef lookup_frame(ScmObj var, ScmObj frame)
{
    ScmObj vars;
    ScmRef vals;
    DECLARE_INTERNAL_FUNCTION("lookup_frame");

#if SCM_STRICT_ARGCHECK
    ASSERT_SYMBOLP(var);
    ASSERT_CONSP(frame);
#endif

    for (vars = CAR(frame), vals = REF_CDR(frame);
         CONSP(vars);
         vars = CDR(vars), vals = REF_CDR(DEREF(vals)))
    {
#if 1 && SCM_STRICT_ARGCHECK
        /*
         * This is required to reject hand-maid broken frame:
         *   (eval '(+ x y) '((x . 4)
         *                    (y . 6)))
         *
         * It can be removed once the typed environment object is implemented.
         */
        ASSERT_CONSP(DEREF(vals));
#endif
        if (EQ(var, CAR(vars)))
            return REF_CAR(DEREF(vals));
    }
    if (EQ(vars, var))
        return vals;

    return SCM_INVALID_REF;
}

/* A wrapper for call() for internal proper tail recursion */
ScmObj Scm_tailcall(ScmObj proc, ScmObj args, ScmEvalState *eval_state)
{
    eval_state->ret_type = SCM_RETTYPE_AS_IS;
    return call(proc, args, eval_state, SUPPRESS_EVAL_ARGS);
}

/* Wrapper for call().  Just like ScmOp_apply(), except ARGS is used
 * as given---nothing special is done about the last item in the
 * list. */
ScmObj Scm_call(ScmObj proc, ScmObj args)
{
    ScmEvalState state;
    ScmObj ret;

    /* We don't need a nonempty environemnt, because this function
     * will never be called directly from Scheme code.  If PROC is a
     * closure, it'll have its own environment, if it's a syntax, it's
     * an error, and if it's a C procedure, it doesn't have any free
     * variables at the Scheme level. */
    state.env       = SCM_INTERACTION_ENV;
    state.ret_type  = SCM_RETTYPE_AS_IS;

    ret = call(proc, args, &state, SUPPRESS_EVAL_ARGS);
    if (state.ret_type == SCM_RETTYPE_NEED_EVAL)
        ret = EVAL(ret, state.env);
    return ret;
}

/* ARGS should NOT have been evaluated yet. */
static ScmObj reduce(ScmObj (*func)(), ScmObj args, ScmObj env, int suppress_eval)
{
    ScmObj left;
    ScmObj right;
    enum ScmReductionState state;
    DECLARE_INTERNAL_FUNCTION("(reduction)");

    state = SCM_REDUCE_0;
    if (NO_MORE_ARG(args))
        return (*func)(SCM_INVALID, SCM_INVALID, &state);

    state = SCM_REDUCE_1;
    left = POP_ARG(args);
    if (!suppress_eval)
        left = EVAL(left, env);
    if (NO_MORE_ARG(args))
        return (*func)(left, left, &state);

    /* Reduce upto all but the last argument. */
    state = SCM_REDUCE_PARTWAY;
    while (right = POP_ARG(args), !NO_MORE_ARG(args)) {
        if (!suppress_eval)
            right = EVAL(right, env);
        left = (*func)(left, right, &state);
        if (state == SCM_REDUCE_STOP)
            return left;
    }

    /* Make the last call. */
    state = SCM_REDUCE_LAST;
    if (!suppress_eval)
        right = EVAL(right, env);
    return (*func)(left, right, &state);
}

/* ARGS should already be evaluated. */
static ScmObj call_closure(ScmObj proc, ScmObj args, ScmEvalState *eval_state)
{
    ScmObj formals;
    DECLARE_INTERNAL_FUNCTION("call_closure");

    /*
     * Description of the ScmClosure handling
     *
     * (lambda <formals> <body>)
     *
     * <formals> should have 3 forms.
     *
     *   (1) : <variable>
     *   (2) : (<variable1> <variable2> ...)
     *   (3) : (<variable1> <variable2> ... <variable n-1> . <variable n>)
     */
    formals = CAR(SCM_CLOSURE_EXP(proc));

    if (SYMBOLP(formals)) {
        /* (1) : <variable> */
        eval_state->env = Scm_ExtendEnvironment(LIST_1(formals),
                                             LIST_1(args),
                                             SCM_CLOSURE_ENV(proc));
    } else if (CONSP(formals)) {
        /*
         * (2) : (<variable1> <variable2> ...)
         * (3) : (<variable1> <variable2> ... <variable n-1> . <variable n>)
         *
         *  - dot list is handled in lookup_frame().
         */
        eval_state->env = Scm_ExtendEnvironment(formals,
                                             args,
                                             SCM_CLOSURE_ENV(proc));
    } else if (NULLP(formals)) {
        /*
         * (2') : <variable> is '()
         */
        eval_state->env
            = Scm_ExtendEnvironment(SCM_NULL,
                                 SCM_NULL,
                                 SCM_CLOSURE_ENV(proc));
    } else {
        ERR_OBJ("lambda : bad formals list", formals);
    }

    eval_state->ret_type = SCM_RETTYPE_NEED_EVAL;
    return ScmExp_begin(CDR(SCM_CLOSURE_EXP(proc)), eval_state);
}

/**
 * @param proc The procedure or syntax to call.
 *
 * @param args The argument list.
 *
 * @param eval_state The calling evaluator's state.
 *
 * @param suppress_eval PROC and ARGS are assumed to have already gone
 * through all necessary evaluations if this flag is nonzero.
 */
static ScmObj call(ScmObj proc, ScmObj args,
                   ScmEvalState *eval_state, int suppress_eval)
{
    ScmObj env                = eval_state->env;
    ScmObj (*func)()          = NULL;
    enum ScmFuncTypeCode type = -1;
    int mand_count            = 0; /* Number of mandatory args. */

    /* The +2 is for rest and env/eval_state. */
    void *argbuf[SCM_FUNCTYPE_MAND_MAX + 2] = {0};
    int i = 0;     /* Number of arguments already stored in argbuf. */
    DECLARE_INTERNAL_FUNCTION("(function call)");

    if (!suppress_eval)
        proc = EVAL(proc, env);

    switch (SCM_TYPE(proc)) {
    case ScmFunc:
        break;

    case ScmClosure:
        return call_closure(proc,
                            suppress_eval ? args : map_eval(args, env),
                            eval_state);

    case ScmContinuation:
        if (!CONSP(args) || !NULLP(CDR(args)))
            ERR("continuation takes exactly one argument");
        Scm_CallContinuation(proc,
                             suppress_eval ? CAR(args) : EVAL(CAR(args), env));
        /* NOTREACHED */
    default:
        ERR_OBJ("bad operator", proc);
    }

    /* We have a C function. */

    type = SCM_FUNC_TYPECODE(proc);
    func = SCM_FUNC_CFUNC(proc);

    if (type == SCM_REDUCTION_OPERATOR)
        return reduce(func, args, env, suppress_eval);

    /* Suppress argument evaluation for syntaxes. */
    if (suppress_eval) {
        if (type & SCM_FUNCTYPE_SYNTAX)
            ERR_OBJ("can't apply/map a syntax", proc);
    } else {
        suppress_eval = type & SCM_FUNCTYPE_SYNTAX;
    }

    /* Collect mandatory arguments. */
    mand_count = type & SCM_FUNCTYPE_MAND_MASK;
    if (mand_count > SCM_FUNCTYPE_MAND_MAX)
        SigScm_Error("Corrupted function: typecode=0x%x", type);
    for (i = 0; i < mand_count; i++) {
        argbuf[i] = MUST_POP_ARG(args);
        if (!suppress_eval)
            argbuf[i] = EVAL(argbuf[i], env);
#if SCM_STRICT_ARGCHECK
        if (VALUEPACKETP((ScmObj)argbuf[i]))
            ERR_OBJ("multiple values are not allowed here", (ScmObj)argbuf[i]);
#endif
    }

    if (type & SCM_FUNCTYPE_VARIADIC) {
        if (!suppress_eval)
            args = map_eval(args, env);
        argbuf[i++] = args;
    } else {
        ASSERT_NO_MORE_ARG(args);
    }

    if (type & SCM_FUNCTYPE_TAIL_REC) {
        eval_state->ret_type = SCM_RETTYPE_NEED_EVAL;
        argbuf[i++] = eval_state;
    } else {
        eval_state->ret_type = SCM_RETTYPE_AS_IS;
        if (type & SCM_FUNCTYPE_SYNTAX)
            argbuf[i++] = env;
    }

    switch (i) {
    case 0:
        return (*func)();
    case 1:
        return (*func)(argbuf[0]);
    case 2:
        return (*func)(argbuf[0], argbuf[1]);
#if SCM_FUNCTYPE_MAND_MAX >= 1
    case 3:
        return (*func)(argbuf[0], argbuf[1], argbuf[2]);
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 2
    case 4:
        return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3]);
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 3
    case 5:
        return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3], argbuf[4]);
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 4
    case 6:
        return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3], argbuf[4], argbuf[5]);
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 5
    case 7:
        return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3], argbuf[4], argbuf[5], argbuf[6]);
#endif
    default:
        SigScm_Error("Corrupted function: typecode=0x%x", type);
    }
    return SCM_INVALID;
}

/*===========================================================================
  S-Expression Evaluation
===========================================================================*/
ScmObj ScmOp_eval(ScmObj obj, ScmObj env)
{
    DECLARE_FUNCTION("eval", ProcedureFixed2);

    ASSERT_ENVP(env);

    return Scm_eval(obj, env);
}

ScmObj Scm_eval(ScmObj obj, ScmObj env)
{
    ScmObj ret  = SCM_NULL;
    ScmEvalState state = {0};

#if SCM_DEBUG
    Scm_PushTraceFrame(obj, env);
#endif

    state.env = env;

eval_loop:
#if SCM_STRICT_R5RS
    /* () is allowed by default for efficiency */
    if (NULLP(obj))
        SigScm_Error("() is not a valid R5RS form. use '() instead");
#endif
    switch (SCM_TYPE(obj)) {
    case ScmSymbol:
        ret = Scm_SymbolValue(obj, state.env);
        break;

    case ScmCons:
        obj = call(CAR(obj), CDR(obj), &state, EVAL_ARGS);
        if (state.ret_type == SCM_RETTYPE_NEED_EVAL)
            goto eval_loop;
        /* FALLTHROUGH */
    default:
        ret = obj;
        break;
    }

#if SCM_DEBUG
    Scm_PopTraceFrame();
#endif
    return ret;
}

ScmObj ScmOp_apply(ScmObj proc, ScmObj arg0, ScmObj rest, ScmEvalState *eval_state)
{
    ScmQueue q;
    ScmObj args, arg, last;
    DECLARE_FUNCTION("apply", ProcedureVariadicTailRec2);

    if (NULLP(rest)) {
        args = last = arg0;
    } else {
        /* More than one argument given. */
        args = LIST_1(arg0);
        q = REF_CDR(args);
        while (arg = POP_ARG(rest), !NO_MORE_ARG(rest))
            SCM_QUEUE_ADD(q, arg);
        /* The last one is spliced. */
        SCM_QUEUE_SLOPPY_APPEND(q, arg);
        last = arg;
    }

    ASSERT_LISTP(last);

    /* The last argument inhibits argument re-evaluation. */
    return call(proc, args, eval_state, SUPPRESS_EVAL_ARGS);
}

/* 'var' must be a symbol as precondition */
ScmObj Scm_SymbolValue(ScmObj var, ScmObj env)
{
    ScmRef ref;
    ScmObj val;
    DECLARE_INTERNAL_FUNCTION("Scm_SymbolValue");

    /* first, lookup the environment */
    ref = Scm_LookupEnvironment(var, env);
    if (ref != SCM_INVALID_REF) {
        /* variable is found in environment, so returns its value */
        return DEREF(ref);
    }

    /* finally, look at the VCELL */
    val = SCM_SYMBOL_VCELL(var);
    if (EQ(val, SCM_UNBOUND))
        ERR_OBJ("unbound variable", var);

    return val;
}

static ScmObj map_eval(ScmObj args, ScmObj env)
{
    ScmQueue q;
    ScmObj res, elm;
    DECLARE_INTERNAL_FUNCTION("(function call)");

    if (NULLP(args))
        return SCM_NULL;

    res = SCM_NULL;
    SCM_QUEUE_POINT_TO(q, res);
    /* does not use POP_ARG() to increace performance */
    for (; CONSP(args); args = CDR(args)) {
        elm = EVAL(CAR(args), env);
#if SCM_STRICT_ARGCHECK
        if (VALUEPACKETP(elm))
            ERR_OBJ("multiple values are not allowed here", elm);
#endif
        SCM_QUEUE_ADD(q, elm);
    }
    /* dot list */
    if (!NULLP(args))
        SCM_QUEUE_SLOPPY_APPEND(q, EVAL(args, env));

    return res;
}

/*===========================================================================
  Utilities: Compound Data Translators
===========================================================================*/

/* Providing also a vector translator would make sense, but currently
 * there's no compelling reason to take the trouble of designing a
 * clean interface for it. */

/**
 * This structure aids in copying a list with modifications to some
 * parts of it.  It's currently used for handling quasiquotation, and
 * planned to be used to implement run-time macro expansion.
 *
 * The translator has an input, an output, and a current position.
 * Only the input is supplied by the user.  Conceptually, the
 * translator replicates the input list and places it at the output.
 * The user then CDRs down the input by successively calling
 * TRL_NEXT().  When an element is encountered that shall be modified,
 * the user calls listrans() with a suitable message (of type tr_msg)
 * along with the modified object.
 *
 * @see tr_msg
 */
typedef struct {
    ScmObj output;
    ScmObj src;                 /* Uncopied portion of input. */
    ScmObj ptr;                 /* Current position. */
    ScmQueue q;
} list_translator;

typedef enum {
    TR_MSG_NOP,

    /** Substitute OBJ for all the cons cells at or after the current
     * position. */
    TR_MSG_SET_TAIL,

    /** Put OBJ in place of the current element. */
    TR_MSG_REPLACE_CAR,

    /** Splice OBJ into the current position. */
    TR_MSG_SPLICE,

    /* Aliases. */
    TR_MSG_REUSE_CAR = TR_MSG_NOP,
    TR_MSG_CURTAIL = TR_MSG_NOP,
    TR_MSG_REPLACE_CONS = TR_MSG_SPLICE
} tr_msg;

#define LISTRAN_INIT(_t, _in)  ((_t).output = SCM_INVALID,               \
                                SCM_QUEUE_POINT_TO((_t).q, (_t).output), \
                                (_t).src = (_t).ptr = (_in))
#define LISTRAN_CURPOS(_t)     ((_t).ptr)
#define LISTRAN_NEXT(_t)       ((_t).ptr = CDR((_t).ptr))
#define LISTRAN_EXTRACT(_t)    ((_t).output)

/**
 * Performs (relatively) complex operations on a list translator.
 * 
 * @see list_translator, tr_msg
 */
static void listran(list_translator *t, tr_msg msg, ScmObj obj)
{
    DECLARE_INTERNAL_FUNCTION("(list translator)");
    switch (msg) {
    case TR_MSG_NOP:
        break;

    case TR_MSG_REPLACE_CAR:
        obj = LIST_1(obj);
        /* Fall through. */
    case TR_MSG_SET_TAIL:
    case TR_MSG_SPLICE:

        /* Let src cath up with ptr, copying elements in between. */
        for (; !EQ(t->src, t->ptr); t->src = CDR(t->src))
            SCM_QUEUE_ADD(t->q, CAR(t->src));

        if (msg != TR_MSG_SET_TAIL) {
            SCM_QUEUE_APPEND(t->q, obj);
#if SCM_STRICT_R5RS
            if (!NULLP(SCM_QUEUE_TERMINATOR(t->q)))
                ERR_OBJ("bad splice list", obj);
#endif
            obj = t->src = CDR(t->src);
        }
        SCM_QUEUE_SLOPPY_APPEND(t->q, obj);
            
        break;
    }
}

/*=======================================
  R5RS : 4.1 Primitive expression types
=======================================*/
/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.2 Literal expressions
===========================================================================*/
ScmObj ScmExp_quote(ScmObj datum, ScmObj env)
{
    DECLARE_FUNCTION("quote", SyntaxFixed1);
    return datum;
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.4 Procedures
===========================================================================*/
ScmObj ScmExp_lambda(ScmObj formals, ScmObj body, ScmObj env)
{
    DECLARE_FUNCTION("lambda", SyntaxVariadic1);
    if (!CONSP(formals) && !NULLP(formals) && !SYMBOLP(formals))
        ERR_OBJ("bad formals", formals);
    if (!CONSP(body))
        ERR_OBJ("at least one expression required", body);

    return Scm_NewClosure(CONS(formals, body), env);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.5 Conditionals
===========================================================================*/
ScmObj ScmExp_if(ScmObj test, ScmObj conseq, ScmObj rest, ScmEvalState *eval_state)
{
    ScmObj env = eval_state->env;
    ScmObj alt;
    DECLARE_FUNCTION("if", SyntaxVariadicTailRec2);

    /*========================================================================
      (if <test> <consequent>)
      (if <test> <consequent> <alternate>)
    ========================================================================*/

    if (NFALSEP(EVAL(test, env))) {
#if SCM_STRICT_ARGCHECK
        POP_ARG(rest);
        ASSERT_NO_MORE_ARG(rest);
#endif
        return conseq;
    } else {
        /* does not use POP_ARG() for efficiency since 'if' syntax is
           frequently used */
        alt = (CONSP(rest)) ? CAR(rest) : SCM_UNDEF;
#if SCM_STRICT_ARGCHECK
        POP_ARG(rest);
        ASSERT_NO_MORE_ARG(rest);
#endif
        return alt;
    }
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.6 Assignment
===========================================================================*/
ScmObj ScmExp_setd(ScmObj sym, ScmObj exp, ScmObj env)
{
    ScmObj evaled        = SCM_FALSE;
    ScmRef locally_bound;
    DECLARE_FUNCTION("set!", SyntaxFixed2);

    evaled = EVAL(exp, env);
    locally_bound = Scm_LookupEnvironment(sym, env);
    if (locally_bound == SCM_INVALID_REF) {
        if (!SYMBOLP(sym))
            ERR_OBJ("symbol required but got", sym);
        /* Not found in the environment
           If symbol is not bound, error occurs */
        if (!SCM_SYMBOL_BOUNDP(sym))
            ERR_OBJ("unbound variable:", sym);

        SCM_SYMBOL_SET_VCELL(sym, evaled);
    } else {
        /* found in the environment*/
        SET(locally_bound, evaled);
    }

#if SCM_STRICT_R5RS
    return SCM_UNDEF;
#else
    return evaled;
#endif
}


/*=======================================
  R5RS : 4.2 Derived expression types
=======================================*/
/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.1 Conditionals
===========================================================================*/
/* body of 'cond' and also invoked from 'case' and 'guard' of SRFI-34 */
ScmObj ScmExp_cond_internal(ScmObj args, ScmObj case_key, ScmEvalState *eval_state)
{
    /*
     * (cond <clause1> <clause2> ...)
     *
     * <clause> should be the form:
     *     (<test> <expression1> <expression2> ...)
     *
     * <clause> may be of the form
     *     (<test> => <expression>)
     *
     * last <clause> may be of the form
     *     (else <expression1> <expression2> ...)
     */
    ScmObj env    = eval_state->env;
    ScmObj clause = SCM_FALSE;
    ScmObj test   = SCM_FALSE;
    ScmObj exps   = SCM_FALSE;
    ScmObj proc   = SCM_FALSE;
    DECLARE_INTERNAL_FUNCTION("cond" /* , SyntaxVariadicTailRec0 */);

    /* dirty hack to replace internal function name */
    if (VALIDP(case_key))
        SCM_MANGLE(name) = "case";

    if (NO_MORE_ARG(args))
        ERR("cond: syntax error: at least one clause required");

    /* looping in each clause */
    while (clause = POP_ARG(args), VALIDP(clause)) {
        if (!CONSP(clause))
            ERR_OBJ("bad clause", clause);

        test = CAR(clause);
        exps = CDR(clause);

        if (EQ(test, SYM_ELSE)) {
            ASSERT_NO_MORE_ARG(args);
        } else {
            if (VALIDP(case_key))
                test = (NFALSEP(ScmOp_memv(case_key, test))) ? case_key : SCM_FALSE;
            else
                test = EVAL(test, env);
        }

        if (NFALSEP(test)) {
            /*
             * if the selected <clause> contains only the <test> and no
             * <expression>s, then the value of the <test> is returned as the
             * result.
             */
            if (NULLP(exps)) {
                if (EQ(test, SYM_ELSE)) {
                    ERR_OBJ("bad clause: else with no expressions", clause);
                } else {
                    eval_state->ret_type = SCM_RETTYPE_AS_IS;
                    return test;
                }
            }

            /*
             * If the selected <clause> uses the => alternate form, then the
             * <expression> is evaluated. Its value must be a procedure that
             * accepts one argument; this procedure is then called on the value
             * of the <test> and the value returned by this procedure is
             * returned by the cond expression.
             */
            if (EQ(SYM_YIELDS, CAR(exps)) && CONSP(CDR(exps))
                && !EQ(test, SYM_ELSE))
            {
                if (!NULLP(CDDR(exps)))
                    ERR_OBJ("bad clause", clause);
                proc = EVAL(CADR(exps), env);
                if (!PROCEDUREP(proc))
                    ERR_OBJ("exp after => must be the procedure but got", proc);

                eval_state->ret_type = SCM_RETTYPE_AS_IS;
                return Scm_call(proc, LIST_1(test));
            }

            return ScmExp_begin(exps, eval_state);
        }
    }

    /*
     * To distinguish unmatched status from SCM_UNDEF from a clause, pure
     * internal value SCM_INVALID is returned. Don't pass it to Scheme world.
     */
    return SCM_INVALID;
}

ScmObj ScmExp_cond(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj ret;
    DECLARE_FUNCTION("cond", SyntaxVariadicTailRec0);

    ret = ScmExp_cond_internal(args, SCM_INVALID, eval_state);
    return (VALIDP(ret)) ? ret : SCM_UNDEF;
}

ScmObj ScmExp_case(ScmObj key, ScmObj clauses, ScmEvalState *eval_state)
{
    ScmObj ret;
    DECLARE_FUNCTION("case", SyntaxVariadicTailRec1);

    key = EVAL(key, eval_state->env);
    ret = ScmExp_cond_internal(clauses, key, eval_state);
    return (VALIDP(ret)) ? ret : SCM_UNDEF;
}

ScmObj ScmExp_and(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env  = eval_state->env;
    ScmObj expr = SCM_INVALID;
    ScmObj val  = SCM_FALSE;
    DECLARE_FUNCTION("and", SyntaxVariadicTailRec0);

    if (NO_MORE_ARG(args))
        return SCM_TRUE;

    while (expr = POP_ARG(args), !NO_MORE_ARG(args)) {
        val = EVAL(expr, env);
        if (FALSEP(val)) {
            ASSERT_PROPER_ARG_LIST(args);
            eval_state->ret_type = SCM_RETTYPE_AS_IS;
            return SCM_FALSE;
        }
    }

    return expr;
}

ScmObj ScmExp_or(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env  = eval_state->env;
    ScmObj expr = SCM_INVALID;
    ScmObj val  = SCM_INVALID;
    DECLARE_FUNCTION("or", SyntaxVariadicTailRec0);

    if (NO_MORE_ARG(args))
        return SCM_FALSE;

    while (expr = POP_ARG(args), !NO_MORE_ARG(args)) {
        val = EVAL(expr, env);
        if (!FALSEP(val)) {
            ASSERT_PROPER_ARG_LIST(args);
            eval_state->ret_type = SCM_RETTYPE_AS_IS;
            return val;
        }
    }

    return expr;
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.2 Binding constructs
===========================================================================*/
/*
 * FIXME:
 * - Write the test for the named let spec:
 *   <init>s should be evaluated in an environment where <procname> is not
 *   bound to the closure.  <procname>'s scope must not penetrate to the
 *   surrounding environment.
 */
ScmObj ScmExp_let(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env           = eval_state->env;
    ScmObj named_let_sym = SCM_FALSE;
    ScmObj proc          = SCM_FALSE;
    ScmObj bindings      = SCM_FALSE;
    ScmObj body          = SCM_FALSE;
    ScmObj binding       = SCM_FALSE;
    ScmObj var           = SCM_FALSE;
    ScmObj val           = SCM_FALSE;
    ScmObj vars          = SCM_NULL;
    ScmObj vals          = SCM_NULL;
    ScmQueue varq, valq;
    DECLARE_FUNCTION("let", SyntaxVariadicTailRec0);

    /*========================================================================
      normal let:

      (let <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    /*========================================================================
      named let:

      (let <procname> <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/

    if (NULLP(args))
        SigScm_Error("let : invalid form");
    bindings = POP_ARG(args);

    /* named let */
    if (SYMBOLP(bindings)) {
        named_let_sym = bindings;

        if (NULLP(args))
            SigScm_Error("let : invalid named let form");
        bindings = POP_ARG(args);
    }

    body = args;

    SCM_QUEUE_POINT_TO(varq, vars);
    SCM_QUEUE_POINT_TO(valq, vals);
    for (; CONSP(bindings); bindings = CDR(bindings)) {
        binding = CAR(bindings);
#if SCM_COMPAT_SIOD_BUGS
        /* temporary solution. the inefficiency is not a problem */
        if (LIST_1_P(binding))
            binding = LIST_2(CAR(binding), SCM_FALSE);
#endif

        if (!LIST_2_P(binding) || !SYMBOLP(var = CAR(binding)))
            ERR_OBJ("invalid binding form", binding);
        val = EVAL(CADR(binding), env);

        SCM_QUEUE_ADD(varq, var);
        SCM_QUEUE_ADD(valq, val);
    }

    if (!NULLP(bindings))
        ERR_OBJ("invalid bindings form", bindings);

    env = Scm_ExtendEnvironment(vars, vals, env);
    eval_state->env = env;

    /* named let */
    if (SYMBOLP(named_let_sym)) {
        proc = Scm_NewClosure(CONS(vars, body), env);
        define_internal(named_let_sym, proc, env);
    }

    return ScmExp_begin(body, eval_state);
}

ScmObj ScmExp_letstar(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj env     = eval_state->env;
    ScmObj var     = SCM_FALSE;
    ScmObj val     = SCM_FALSE;
    ScmObj binding = SCM_FALSE;
    DECLARE_FUNCTION("let*", SyntaxVariadicTailRec1);

    /*========================================================================
      (let* <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    if (!CONSP(bindings) && !NULLP(bindings))
        SigScm_Error("let* : syntax error");

    for (; CONSP(bindings); bindings = CDR(bindings)) {
        binding = CAR(bindings);
#if SCM_COMPAT_SIOD_BUGS
        /* temporary solution. the inefficiency is not a problem */
        if (LIST_1_P(binding))
            binding = LIST_2(CAR(binding), SCM_FALSE);
#endif

        if (!LIST_2_P(binding) || !SYMBOLP(var = CAR(binding)))
            ERR_OBJ("invalid binding form", binding);
        val = EVAL(CADR(binding), env);

        /* extend env for each variable */
        env = Scm_ExtendEnvironment(LIST_1(var), LIST_1(val), env);
    }

    if (!NULLP(bindings))
        ERR_OBJ("invalid bindings form", bindings);

    eval_state->env = env;

    /* evaluate body */
    return ScmExp_begin(body, eval_state);
}

ScmObj ScmExp_letrec(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj env      = eval_state->env;
    ScmObj frame    = SCM_FALSE;
    ScmObj vars     = SCM_NULL;
    ScmObj vals     = SCM_NULL;
    ScmObj binding  = SCM_FALSE;
    ScmObj var      = SCM_FALSE;
    ScmObj val      = SCM_FALSE;
    DECLARE_FUNCTION("letrec", SyntaxVariadicTailRec1);

    /*========================================================================
      (letrec <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    if (!CONSP(bindings) && !NULLP(bindings))
        SigScm_Error("letrec : syntax error");

    /* extend env by placeholder frame for subsequent lambda evaluations */
    frame = CONS(SCM_NULL, SCM_NULL);
    env = CONS(frame, env);
    eval_state->env = env;

    for (; CONSP(bindings); bindings = CDR(bindings)) {
        binding = CAR(bindings);
#if SCM_COMPAT_SIOD_BUGS
        /* temporary solution. the inefficiency is not a problem */
        if (LIST_1_P(binding))
            binding = LIST_2(CAR(binding), SCM_FALSE);
#endif

        if (!LIST_2_P(binding) || !SYMBOLP(var = CAR(binding)))
            ERR_OBJ("invalid binding form", binding);
        val = EVAL(CADR(binding), env);

        /* construct vars and vals list: any <init> must not refer a
           <variable> at this time */
        vars = CONS(var, vars);
        vals = CONS(val, vals);
    }

    if (!NULLP(bindings))
        ERR_OBJ("invalid bindings form", bindings);

    /* fill the placeholder frame */
    SET_CAR(frame, vars);
    SET_CDR(frame, vals);

    /* evaluate body */
    return ScmExp_begin(body, eval_state);
}


/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.3 Sequencing
===========================================================================*/
ScmObj ScmExp_begin(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env  = eval_state->env;
    ScmObj expr = SCM_INVALID;
    DECLARE_FUNCTION("begin", SyntaxVariadicTailRec0);

    if (NO_MORE_ARG(args))
        return SCM_UNDEF;

    while (expr = POP_ARG(args), !NO_MORE_ARG(args))
        EVAL(expr, env);

    /* Return tail expression. */
    return expr;
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.4 Iteration
===========================================================================*/
ScmObj ScmExp_do(ScmObj bindings, ScmObj testframe, ScmObj commands, ScmEvalState *eval_state)
{
    /*
     * (do ((<variable1> <init1> <step1>)
     *      (<variable2> <init2> <step2>)
     *      ...)
     *     (<test> <expression> ...)
     *   <command> ...)
     */
    ScmObj env        = eval_state->env;
    ScmObj binding    = SCM_FALSE;
    ScmObj var        = SCM_FALSE;
    ScmObj val        = SCM_FALSE;
    ScmObj vars       = SCM_NULL;
    ScmObj vals       = SCM_NULL;
    ScmObj steps      = SCM_NULL;
    ScmObj test       = SCM_FALSE;
    ScmObj expression = SCM_FALSE;
    ScmObj tmp_steps  = SCM_FALSE;
    ScmObj tmp_vars   = SCM_FALSE;
    ScmRef obj;
    DECLARE_FUNCTION("do", SyntaxVariadicTailRec2);

    /* construct Environment and steps */
    for (; !NULLP(bindings); bindings = CDR(bindings)) {
        binding = CAR(bindings);
        if (NULLP(binding))
            ERR("invalid bindings");

        var = MUST_POP_ARG(binding);
        ASSERT_SYMBOLP(var);
        val = MUST_POP_ARG(binding);

        vars = CONS(var, vars);
        vals = CONS(EVAL(val, env), vals);

        /* append <step> to steps */
        if (NO_MORE_ARG(binding))
            steps = CONS(var, steps);
        else
            steps = CONS(POP_ARG(binding), steps);

        ASSERT_NO_MORE_ARG(binding);
    }

    /* now extend environment */
    env = Scm_ExtendEnvironment(vars, vals, env);

    /* construct test */
    if (NULLP(testframe))
        ERR("invalid testframe");
    test       = CAR(testframe);
    expression = CDR(testframe);

    /* now execution phase! */
    while (FALSEP(EVAL(test, env))) {
        /* execute commands */
        EVAL(ScmExp_begin(commands, eval_state), env);

        /*
         * Notice
         *
         * the result of the execution of <step>s must not depend on each other's
         * results. each execution must be done independently. So, we store the
         * results to the "vals" variable and set it in hand.
         */
        vals = SCM_NULL;
        for (tmp_steps = steps;
             !NULLP(tmp_steps);
             tmp_steps = CDR(tmp_steps))
        {
            vals = CONS(EVAL(CAR(tmp_steps), env), vals);
        }
        vals = ScmOp_reverse(vals);

        /* set it */
        for (tmp_vars = vars;
             !NULLP(tmp_vars) && !NULLP(vals);
             tmp_vars = CDR(tmp_vars), vals = CDR(vals))
        {
            obj = Scm_LookupEnvironment(CAR(tmp_vars), env);
            if (obj != SCM_INVALID_REF) {
                SET(obj, CAR(vals));
            } else {
                SigScm_Error("do : broken env");
            }
        }
    }

    eval_state->env = env;

    return NULLP(expression) ? EVAL(test, env) : ScmExp_begin(expression, eval_state);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.5 Delayed evaluation
===========================================================================*/
ScmObj ScmExp_delay(ScmObj expr, ScmObj env)
{
    DECLARE_FUNCTION("delay", SyntaxFixed1);

    /* (lambda () exp) */
    return Scm_NewClosure(SCM_LIST_2(SCM_NULL, expr), env);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.6 Quasiquotation
===========================================================================*/

struct _qquote_result {
    ScmObj obj;
    tr_msg insn;
};

static qquote_result qquote_vector(ScmObj input, ScmObj env, int nest)
{
    ScmObj replacements;
    ScmObj obj;
    ScmObj *copy_buf;
    int splice_len;
    int growth;
    int len;
    int i, cpi;
    int next_rindex;
    qquote_result tmp;
    qquote_result my_result;
    DECLARE_INTERNAL_FUNCTION("(quasiquote:vector)");

    len          = SCM_VECTOR_LEN(input);
    replacements = SCM_NULL;
    obj          = SCM_INVALID;
    copy_buf     = NULL;
    growth       = 0;

    for (i = len - 1; i >= 0; i--) {
        obj = SCM_VECTOR_CREF(input, i);
        tmp = qquote_internal(obj, env, nest);
        switch (tmp.insn) {
        case TR_MSG_REPLACE_CAR:
            replacements = CONS(CONS(Scm_NewInt(i),
                                     tmp.obj),
                                replacements);
            break;
        case TR_MSG_SPLICE:
            replacements = CONS(CONS(Scm_NewInt(-i-1), /* Mark as splice. */
                                     tmp.obj),
                                replacements);
            splice_len = ScmOp_c_length(tmp.obj);
#if SCM_STRICT_R5RS
            if (splice_len < 0)
                ERR_OBJ("got bad splice list from", obj);
#endif
            growth += splice_len - 1;
            break;
        default:
            break;
        }
    }

    if (NULLP(replacements)) {
        my_result.obj = SCM_INVALID;
        my_result.insn = TR_MSG_REUSE_CAR;
        return my_result;
    }

    copy_buf = malloc((len + growth) * sizeof(ScmObj));

    /* i indexes input and cpi indexes copy_buf. */
    next_rindex = SCM_INT_VALUE(CAAR(replacements));
    for (i = cpi = 0; i < len; i++) {
        if (i == next_rindex) {
            copy_buf[cpi++] = CDAR(replacements);
        } else if (-i-1 == next_rindex) {
            ScmObj tmp;
            for (tmp = CDAR(replacements); CONSP(tmp); tmp = CDR(tmp))
                copy_buf[cpi++] = CAR(tmp);
        } else {
            copy_buf[cpi++] = SCM_VECTOR_CREF(input, i);
            continue;
        }
        replacements = CDR(replacements);
        if (NULLP(replacements))
            next_rindex = len;   /* Invalidate. */
        else
            next_rindex = SCM_INT_VALUE(CAAR(replacements));
    }

    my_result.obj = Scm_NewVector(copy_buf, len + growth);
    my_result.insn = TR_MSG_REPLACE_CAR;
    return my_result;
}

/**
 * Interpret a quasiquoted expression.
 *
 * @see qquote_vector()
 */
static qquote_result qquote_internal(ScmObj input, ScmObj env, int nest)
{
    ScmObj ptr;
    list_translator tr;
    qquote_result tmp_result;
    qquote_result my_result;
    DECLARE_INTERNAL_FUNCTION("(quasiquote)");

    LISTRAN_INIT(tr, input);
    ptr = LISTRAN_CURPOS(tr);        /* This will be our traverser. */

    if (VECTORP(input))
        return qquote_vector(input, env, nest);

    if (!CONSP(input))
        goto end;

#define EXPAND(_datum)                                                  \
        (tmp_result = qquote_internal((_datum), env, nest),             \
         listran(&tr, tmp_result.insn, tmp_result.obj))

#define SET_TAIL(_datum) \
        listran(&tr, TR_MSG_SET_TAIL, (_datum))

    /* INPUT can't be a syntactical expression if length < 2. */
    if (!CONSP(CDR(ptr)))
        goto one_cons;

    /* Process up to the penultimate element (not counting the
     * list terminator). */
    for (; CONSP(CDDR(ptr)); ptr = LISTRAN_NEXT(tr))
        EXPAND(CAR(ptr));

    if (NULLP(CDDR(ptr))) {
        if (EQ(CAR(ptr), SYM_QUASIQUOTE)) {
            /* PTR == `x */
            ++nest;
        } else if (EQ(CAR(ptr), SYM_UNQUOTE)) {
            /* PTR == ,x */
            if (--nest == 0) {
                SET_TAIL(EVAL(CADR(ptr), env));
                my_result.obj = LISTRAN_EXTRACT(tr);
                my_result.insn = TR_MSG_REPLACE_CAR;
                return my_result;
            }
        } else if (EQ(CAR(ptr), SYM_UNQUOTE_SPLICING)) {
            /* PTR == ,@x */
            if (!EQ(ptr, input)) /* (a . ,@b) */
                ERR_OBJ(",@ in wrong context", input);
            if (--nest == 0) {
                my_result.insn = TR_MSG_SPLICE;
                my_result.obj  = EVAL(CADR(ptr), env);
                return my_result;
            }
        }
    }

  one_cons:
    do {
        EXPAND(CAR(ptr));
        ptr = LISTRAN_NEXT(tr);
    } while (CONSP(ptr));

    /* Handle list terminator; '() for proper lists. */
    tmp_result = qquote_internal(ptr, env, nest);
    if (tmp_result.insn != TR_MSG_REUSE_CAR)
        SET_TAIL(tmp_result.obj);

  end:
    my_result.obj = LISTRAN_EXTRACT(tr);
    my_result.insn = VALIDP(my_result.obj)
        ? TR_MSG_REPLACE_CAR
        : TR_MSG_REUSE_CAR;
    return my_result;
#undef EXPAND
#undef SET_TAIL
}

ScmObj ScmExp_quasiquote(ScmObj datum, ScmObj env)
{
    qquote_result ret = qquote_internal(datum, env, 1);
    DECLARE_FUNCTION("quasiquote", SyntaxFixed1);

    switch (ret.insn) {
    case TR_MSG_REUSE_CAR:
        return datum;
    case TR_MSG_SPLICE:
#if SCM_STRICT_R5RS
        ERR_OBJ("unquote-splicing in invalid context", datum);
#endif
        /* Otherwise fall through. */
    case TR_MSG_REPLACE_CAR:
        return ret.obj;
    default:
        ERR_OBJ("bug in quasiquote", datum);
    }
}

ScmObj ScmExp_unquote(ScmObj dummy, ScmObj env)
{
    DECLARE_FUNCTION("unquote", SyntaxFixed1);

    SigScm_Error("unquote outside quasiquote");
    return SCM_NULL;
}

ScmObj ScmExp_unquote_splicing(ScmObj dummy, ScmObj env)
{
    DECLARE_FUNCTION("unquote-splicing", SyntaxFixed1);

    SigScm_Error("unquote-splicing outside quasiquote");
    return SCM_NULL;
}


/*=======================================
  R5RS : 5.2 Definitions
=======================================*/
static void define_internal(ScmObj var, ScmObj exp, ScmObj env)
{
    if (NULLP(env)) {
        /* given top-level environment */
        SCM_SYMBOL_SET_VCELL(var, EVAL(exp, env));
    } else {
        /* add val to the environment */
        env = Scm_AddEnvironment(var, EVAL(exp, env), env);
    }
}

ScmObj ScmExp_define(ScmObj var, ScmObj rest, ScmObj env)
{
    ScmObj procname = SCM_FALSE;
    ScmObj body     = SCM_FALSE;
    ScmObj formals  = SCM_FALSE;
    DECLARE_FUNCTION("define", SyntaxVariadic1);

    /*========================================================================
      (define <variable> <expression>)
    ========================================================================*/
    if (SYMBOLP(var)) {
        if (!LIST_1_P(rest))
            ERR_OBJ("exactly 1 arg required but got", rest);

        define_internal(var, POP_ARG(rest), env);
        ASSERT_NO_MORE_ARG(rest);
    }

    /*========================================================================
      (define (<variable> . <formals>) <body>)

      => (define <variable>
             (lambda (<formals>) <body>))
    ========================================================================*/
    else if (CONSP(var)) {
        procname   = CAR(var);
        formals    = CDR(var);
        body       = rest;

        if (NULLP(body))
            ERR("define: missing function body");
#if SCM_STRICT_ARGCHECK
        /* this is not necessary because checked in closure call */
        if (!CONSP(body))
            ERR_OBJ("proper list is required as <body> but got", body);
#endif

        ASSERT_SYMBOLP(procname);

        define_internal(procname,
                        Scm_NewClosure(CONS(formals, body), env),
                        env);
    } else {
        ERR_OBJ("syntax error", var);
    }

#if SCM_STRICT_R5RS
    return SCM_UNDEF;
#else
    return var;
#endif
}

/*=======================================
  R5RS : 6.5 Eval
=======================================*/
ScmObj ScmOp_scheme_report_environment(ScmObj version)
{
    DECLARE_FUNCTION("scheme-report-environment", ProcedureFixed1);

    /* sanity check */
    ASSERT_INTP(version);
    if (SCM_INT_VALUE(version) != 5)
        ERR_OBJ("version must be 5 but got", version);

#if SCM_STRICT_R5RS
    SigScm_Error("scheme-report-environment :" SCM_ERRMSG_NON_R5RS_ENV);
#else
    CDBG((SCM_DBG_COMPAT,
          "scheme-report-environment : warning:" SCM_ERRMSG_NON_R5RS_ENV));
#endif

    return SCM_INTERACTION_ENV;
}

ScmObj ScmOp_null_environment(ScmObj version)
{
    DECLARE_FUNCTION("null-environment", ProcedureFixed1);

    /* sanity check */
    ASSERT_INTP(version);
    if (SCM_INT_VALUE(version) != 5)
        ERR_OBJ("version must be 5 but got", version);

#if SCM_STRICT_R5RS
    SigScm_Error("null-environment :" SCM_ERRMSG_NON_R5RS_ENV);
#else
    CDBG((SCM_DBG_COMPAT,
          "null-environment : warning:" SCM_ERRMSG_NON_R5RS_ENV));
#endif

    return SCM_INTERACTION_ENV;
}

ScmObj ScmOp_interaction_environment(void)
{
    DECLARE_FUNCTION("interaction-environment", ProcedureFixed0);
    return SCM_INTERACTION_ENV;
}
