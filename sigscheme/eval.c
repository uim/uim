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
 * @see scm_eval()
 */
ScmObj
scm_extend_environment(ScmObj vars, ScmObj vals, ScmObj env)
{
    ScmObj frame, rest_vars, rest_vals;
    DECLARE_INTERNAL_FUNCTION("scm_extend_environment");

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
ScmObj
scm_add_environment(ScmObj var, ScmObj val, ScmObj env)
{
    ScmObj newest_frame;
    ScmObj new_vars, new_vals;
    DECLARE_INTERNAL_FUNCTION("scm_add_environment");

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
ScmRef
scm_lookup_environment(ScmObj var, ScmObj env)
{
    ScmObj frame;
    ScmRef ref;
    DECLARE_INTERNAL_FUNCTION("scm_lookup_environment");

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
static ScmRef
lookup_frame(ScmObj var, ScmObj frame)
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
#if SCM_STRICT_ARGCHECK
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
ScmObj
scm_tailcall(ScmObj proc, ScmObj args, ScmEvalState *eval_state)
{
    eval_state->ret_type = SCM_RETTYPE_AS_IS;
    return call(proc, args, eval_state, SUPPRESS_EVAL_ARGS);
}

/* Wrapper for call().  Just like scm_p_apply(), except ARGS is used
 * as given---nothing special is done about the last item in the
 * list. */
ScmObj
scm_call(ScmObj proc, ScmObj args)
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
static ScmObj
reduce(ScmObj (*func)(), ScmObj args, ScmObj env, int suppress_eval)
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
static ScmObj
call_closure(ScmObj proc, ScmObj args, ScmEvalState *eval_state)
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
        eval_state->env = scm_extend_environment(LIST_1(formals),
                                             LIST_1(args),
                                             SCM_CLOSURE_ENV(proc));
    } else if (CONSP(formals)) {
        /*
         * (2) : (<variable1> <variable2> ...)
         * (3) : (<variable1> <variable2> ... <variable n-1> . <variable n>)
         *
         *  - dot list is handled in lookup_frame().
         */
        eval_state->env = scm_extend_environment(formals,
                                             args,
                                             SCM_CLOSURE_ENV(proc));
    } else if (NULLP(formals)) {
        /*
         * (2') : <variable> is '()
         */
        eval_state->env
            = scm_extend_environment(SCM_NULL,
                                 SCM_NULL,
                                 SCM_CLOSURE_ENV(proc));
    } else {
        ERR_OBJ("lambda : bad formals list", formals);
    }

    eval_state->ret_type = SCM_RETTYPE_NEED_EVAL;
    return scm_s_begin(CDR(SCM_CLOSURE_EXP(proc)), eval_state);
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
static ScmObj
call(ScmObj proc, ScmObj args, ScmEvalState *eval_state, int suppress_eval)
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
        scm_call_continuation(proc,
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
        ERR("corrupted function: typecode=0x%x", type);
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
        ERR("corrupted function: typecode=0x%x", type);
    }
    return SCM_INVALID;
}

/*===========================================================================
  S-Expression Evaluation
===========================================================================*/
ScmObj
scm_p_eval(ScmObj obj, ScmObj env)
{
    DECLARE_FUNCTION("eval", procedure_fixed_2);

    ASSERT_ENVP(env);

    return scm_eval(obj, env);
}

ScmObj
scm_eval(ScmObj obj, ScmObj env)
{
    ScmObj ret  = SCM_NULL;
    ScmEvalState state = {0};

#if SCM_DEBUG
    scm_push_trace_frame(obj, env);
#endif

    state.env = env;

eval_loop:
#if SCM_STRICT_R5RS
    /* () is allowed by default for efficiency */
    if (NULLP(obj))
        ERR("eval: () is not a valid R5RS form. use '() instead");
#endif
    switch (SCM_TYPE(obj)) {
    case ScmSymbol:
        ret = scm_symbol_value(obj, state.env);
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
    scm_pop_trace_frame();
#endif
    return ret;
}

ScmObj
scm_p_apply(ScmObj proc, ScmObj arg0, ScmObj rest, ScmEvalState *eval_state)
{
    ScmQueue q;
    ScmObj args, arg, last;
    DECLARE_FUNCTION("apply", procedure_variadic_tailrec_2);

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
ScmObj
scm_symbol_value(ScmObj var, ScmObj env)
{
    ScmRef ref;
    ScmObj val;
    DECLARE_INTERNAL_FUNCTION("scm_symbol_value");

    /* first, lookup the environment */
    ref = scm_lookup_environment(var, env);
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

static ScmObj
map_eval(ScmObj args, ScmObj env)
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
  Utilities: Sequential Datum Translators
===========================================================================*/
/**
 * These utilities aid in copying a sequence with modifications to
 * some parts of it.  It's currently used for handling quasiquotation,
 * and planned to be used to implement run-time macro expansion.  The
 * translator works as a copy-on-write iterator for lists or vectors.
 *
 * First, initialize the proper type of translator with either
 * TRL_INIT() or TRV_INIT(), supplying the datum to be duplicated.
 * Then, traverse over the `copy' by successively and alternately
 * calling TR_GET_OBJ() and TR_NEXT().  If an item returned by
 * TR_GET_OBJ() should be replaced, then call TR_CALL() with the
 * message TR_REPLACE or TR_SPLICE (see their definition for details).
 * When TR_ENDP() returns true, stop and obtain the duplicate with
 * TR_EXTRACT().
 *
 * The last cdr of an improper list is *not* considered a part of the
 * list and will be treated just like the () of a proper list.  In
 * order to retrieve that last cdr, call TRL_GET_SUBLS() *after*
 * TR_ENDP() returns true.  Replacement of that portion must be done
 * with TRL_SET_SUBLS().
 *
 * No operation except TRL_GET_SUBLS(), TRL_SET_SUBLS(), TR_EXTRACT(),
 * and TR_ENDP() can be done on a translator for which TR_ENDP()
 * returns true.
 *
 * Everything prefixed with TRL_ is specific to list translators.
 * Likewise, TRV_ shows specificity to vector translators.  TR_
 * denotes a polymorphism.
 */

/**
 * Message IDs.  We have to bring this upfront because ISO C forbids
 * forward reference to enumerations.
 */
enum _tr_msg {
    /** Don't do anything. */
    TR_MSG_NOP,

    /** Put OBJ in place of the current element. */
    TR_MSG_REPLACE,

    /** Splice OBJ into the current cell. */
    TR_MSG_SPLICE,

    /**
     * Get the object at the current position.  If the input is an
     * improper list, the terminator is not returned in reply to this
     * message.  Use TRL_GET_SUBLS() to retrieve the terminator in
     * that case.
     */
    TR_MSG_GET_OBJ,

    /** Advance the iterator on the input. */
    TR_MSG_NEXT,

    /** Extract the product. */
    TR_MSG_EXTRACT,

    /** True if the end of the sequence has been reached. */
    TR_MSG_ENDP,

    /**
     * Splice OBJ and discard all cells at or after the current one
     * in the input.  Only implemented for list translators.
     */
    TRL_MSG_SET_SUBLS
};

typedef enum _tr_msg tr_msg;
typedef struct _list_translator list_translator;
typedef struct _vector_translator vector_translator;
typedef struct _sequence_translator sequence_translator;

struct _list_translator {
    ScmObj output;
    ScmObj cur;
    ScmObj src;
    ScmQueue q;
};

struct _vector_translator {
    ScmObj src;
    ScmObj diff;
    ScmQueue q;                 /* Points to diff. */
    int index;                  /* Current position. */
    int growth;
};

struct _sequence_translator {
    ScmObj (*trans)(sequence_translator *t, tr_msg msg, ScmObj obj);
    union {
        list_translator lst;
        vector_translator vec;
    } u;
};

/*
 * Operations on translators.  If a list- or vector-specific macro has
 * the same name (sans prefix) as a polymorphic one, the former tends
 * to be faster.
 */

/* List-specific macros. */
#define TRL_INIT(_t, _in)     ((_t).u.lst.output = SCM_INVALID,         \
                               SCM_QUEUE_POINT_TO((_t).u.lst.q,         \
                                                  (_t).u.lst.output),   \
                               (_t).u.lst.src = (_in),                  \
                               (_t).u.lst.cur = (_in),                  \
                               (_t).trans = listran)
#define TRL_GET_OBJ(_t)       (CAR((_t).u.lst.cur))
#define TRL_NEXT(_t)          ((_t).u.lst.cur = CDR((_t).u.lst.cur))
#define TRL_ENDP(_t)          (!CONSP((_t).u.lst.cur))
#define TRL_GET_SUBLS(_t)     ((_t).u.lst.cur)
#define TRL_SET_SUBLS(_t, _o) (TRL_CALL((_t), TRL_MSG_SET_SUBLS, (_o)))
#define TRL_EXTRACT(_t)       ((_t).u.lst.output)
#define TRL_CALL(_t, _m, _p)  (listran(&(_t), (_m), (_p)))

/* Vector-specific macros. */
#define TRV_INIT(_t, _in)  ((_t).u.vec.diff = SCM_NULL,                 \
                            SCM_QUEUE_POINT_TO((_t).u.vec.q,            \
                                               (_t).u.vec.diff),        \
                            (_t).u.vec.src = (_in),                     \
                            (_t).u.vec.index = 0,                       \
                            (_t).u.vec.growth = 0,                      \
                            (_t).trans = vectran)
#define TRV_GET_OBJ(_t)    (SCM_VECTOR_CREF((_t).u.vec.src, (_t).u.vec.index))
#define TRV_NEXT(_t)       (++(_t).u.vec.index)
#define TRV_ENDP(_t)       (SCM_VECTOR_LEN((_t).u.vec.src) <= (_t).u.vec.index)
#define TRV_EXTRACT(_t)    (TRV_CALL((_t), TR_MSG_EXTRACT, SCM_INVALID))
#define TRV_CALL(_t, _m, _p) (vectran(&(_t), (_m), (_p)))

/* Polymorphic macros. */
#define TR_CALL(_t, _msg, _p) ((*(_t).trans)(&(_t), (_msg), (_p)))
#define TR_GET_OBJ(_t)     (TR_CALL((_t), TR_MSG_GET_OBJ, SCM_INVALID))
#define TR_NEXT(_t)        ((void)TR_CALL((_t), TR_MSG_NEXT, SCM_INVALID))
#define TR_ENDP(_t)        ((int)TR_CALL((_t), TR_MSG_ENDP, SCM_INVALID))
#define TR_EXTRACT(_t)     (TR_CALL((_t), TR_MSG_EXTRACT, SCM_INVALID))


/**
 * Performs (relatively) complex operations on a list translator.
 *
 * @see list_translator, tr_msg
 */
static ScmObj
listran(sequence_translator *t, tr_msg msg, ScmObj obj)
{
    DECLARE_INTERNAL_FUNCTION("(list translator)");
    switch (msg) {
    default:
        break;

    case TR_MSG_ENDP:
        return (ScmObj)TRL_ENDP(*t);

    case TR_MSG_GET_OBJ:
        return TRL_GET_OBJ(*t);

    case TR_MSG_NEXT:
        TRL_NEXT(*t);
        break;

    case TR_MSG_REPLACE:
        obj = LIST_1(obj);
        /* Fall through. */
    case TRL_MSG_SET_SUBLS:
    case TR_MSG_SPLICE:

        /* Execute deferred copies. */
        while (!EQ(t->u.lst.src, t->u.lst.cur)) {
            SCM_QUEUE_ADD(t->u.lst.q, CAR(t->u.lst.src));
            t->u.lst.src = CDR(t->u.lst.src);
        }

        if (msg != TRL_MSG_SET_SUBLS) {
            SCM_QUEUE_APPEND(t->u.lst.q, obj);
#if SCM_STRICT_R5RS
            if (!NULLP(SCM_QUEUE_TERMINATOR(t->u.lst.q)))
                ERR_OBJ("bad splice list", obj);
#endif
            t->u.lst.src = obj = CDR(t->u.lst.cur);
        }
        SCM_QUEUE_SLOPPY_APPEND(t->u.lst.q, obj);
        break;

    case TR_MSG_EXTRACT:
        return t->u.lst.output;
    }
    return SCM_INVALID;
}

static ScmObj
vectran(sequence_translator *t, tr_msg msg, ScmObj obj)
{
    int splice_len;
    int change_index;

    switch (msg) {
    default:
        break;

    case TR_MSG_GET_OBJ:
        return TRV_GET_OBJ(*t);
    case TR_MSG_NEXT:
        TRV_NEXT(*t);
        break;
    case TR_MSG_ENDP:
        return (ScmObj)TRV_ENDP(*t);

    case TR_MSG_SPLICE:
        splice_len = scm_p_c_length(obj);
#if SCM_STRICT_R5RS
        if (splice_len < 0)
            ERR_OBJ("got bad splice list", obj);
#endif
        t->u.vec.growth += splice_len - 1;
        change_index = -t->u.vec.index - 1;
        goto record_change;

    case TR_MSG_REPLACE:
        change_index = t->u.vec.index;

      record_change:
        SCM_QUEUE_ADD(t->u.vec.q, CONS(scm_make_int(change_index), obj));
        break;

    case TR_MSG_EXTRACT:
        /* Create a new vector if modifications have been recorded. */
        if (!NULLP(t->u.vec.diff)) {
            ScmObj *copy_buf;
            ScmObj *src_buf;
            ScmObj tmp;
            ScmObj diff;
            int src_len, i, cpi;

            src_len = SCM_VECTOR_LEN(t->u.vec.src);
            src_buf = SCM_VECTOR_VEC(t->u.vec.src);
            copy_buf = malloc ((src_len + t->u.vec.growth) * sizeof (ScmObj));

            diff = t->u.vec.diff;
            change_index = SCM_INT_VALUE(CAAR(diff));

            for (i = cpi = 0; i < src_len; i++) {
                if (i == change_index) {
                    copy_buf[cpi++] = CDAR(diff);
                } else if (-i-1 == change_index) {
                    /* Splice. */
                    for (tmp = CDAR(diff); CONSP(tmp); tmp = CDR(tmp))
                        copy_buf[cpi++] = CAR(tmp);
                } else {
                    copy_buf[cpi++] = src_buf[i];
                    continue;
                }

                /* We replaced an element this round. */
                diff = CDR(diff);
                if (NULLP(diff))
                    /* Invalidate. */
                    change_index = src_len;
                else
                    change_index = SCM_INT_VALUE(CAAR(diff));
            }
            return scm_make_vector(copy_buf, src_len + t->u.vec.growth);
        }
        break;
    }
    return SCM_INVALID;
}

/*=======================================
  R5RS : 4.1 Primitive expression types
=======================================*/
/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.2 Literal expressions
===========================================================================*/
ScmObj
scm_s_quote(ScmObj datum, ScmObj env)
{
    DECLARE_FUNCTION("quote", syntax_fixed_1);
    return datum;
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.4 Procedures
===========================================================================*/
ScmObj
scm_s_lambda(ScmObj formals, ScmObj body, ScmObj env)
{
    DECLARE_FUNCTION("lambda", syntax_variadic_1);
    if (!CONSP(formals) && !NULLP(formals) && !SYMBOLP(formals))
        ERR_OBJ("bad formals", formals);
    if (!CONSP(body))
        ERR_OBJ("at least one expression required", body);

    return scm_make_closure(CONS(formals, body), env);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.5 Conditionals
===========================================================================*/
ScmObj
scm_s_if(ScmObj test, ScmObj conseq, ScmObj rest, ScmEvalState *eval_state)
{
    ScmObj env = eval_state->env;
    ScmObj alt;
    DECLARE_FUNCTION("if", syntax_variadic_tailrec_2);

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
ScmObj
scm_s_setd(ScmObj sym, ScmObj exp, ScmObj env)
{
    ScmObj evaled        = SCM_FALSE;
    ScmRef locally_bound;
    DECLARE_FUNCTION("set!", syntax_fixed_2);

    evaled = EVAL(exp, env);
    locally_bound = scm_lookup_environment(sym, env);
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
ScmObj
scm_s_cond_internal(ScmObj args, ScmObj case_key, ScmEvalState *eval_state)
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
            if (VALIDP(case_key)) {
                /* Don't pass the scm_p_memv to the NFALSEP macro as an
                 * argument (e.g. NFALSEP(scm_p_memv(key, test))), because
                 * there's an possibility that scm_p_memv is called multiple
                 * times after the macro expantion. */
                test = scm_p_memv(case_key, test);
                test = (NFALSEP(test)) ? case_key : SCM_FALSE;
            } else {
                test = EVAL(test, env);
            }
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
             * Handle the case like follows.
             *
             * (case 1
             *   ((1) . 2))
             */
            if (!CONSP(exps))
                ERR_OBJ("bad dot clause", clause);

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
                return scm_call(proc, LIST_1(test));
            }

            return scm_s_begin(exps, eval_state);
        }
    }

    /*
     * To distinguish unmatched status from SCM_UNDEF from a clause, pure
     * internal value SCM_INVALID is returned. Don't pass it to Scheme world.
     */
    return SCM_INVALID;
}

ScmObj
scm_s_cond(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj ret;
    DECLARE_FUNCTION("cond", syntax_variadic_tailrec_0);

    ret = scm_s_cond_internal(args, SCM_INVALID, eval_state);
    return (VALIDP(ret)) ? ret : SCM_UNDEF;
}

ScmObj
scm_s_case(ScmObj key, ScmObj clauses, ScmEvalState *eval_state)
{
    ScmObj ret;
    DECLARE_FUNCTION("case", syntax_variadic_tailrec_1);

    key = EVAL(key, eval_state->env);
    ret = scm_s_cond_internal(clauses, key, eval_state);
    return (VALIDP(ret)) ? ret : SCM_UNDEF;
}

ScmObj
scm_s_and(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env  = eval_state->env;
    ScmObj expr = SCM_INVALID;
    ScmObj val  = SCM_FALSE;
    DECLARE_FUNCTION("and", syntax_variadic_tailrec_0);

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

ScmObj
scm_s_or(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env  = eval_state->env;
    ScmObj expr = SCM_INVALID;
    ScmObj val  = SCM_INVALID;
    DECLARE_FUNCTION("or", syntax_variadic_tailrec_0);

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
ScmObj
scm_s_let(ScmObj args, ScmEvalState *eval_state)
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
    DECLARE_FUNCTION("let", syntax_variadic_tailrec_0);

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
        ERR("let: invalid form");
    bindings = POP_ARG(args);

    /* named let */
    if (SYMBOLP(bindings)) {
        named_let_sym = bindings;

        if (NULLP(args))
            ERR("let: invalid named let form");
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

    env = scm_extend_environment(vars, vals, env);
    eval_state->env = env;

    /* named let */
    if (SYMBOLP(named_let_sym)) {
        proc = scm_make_closure(CONS(vars, body), env);
        define_internal(named_let_sym, proc, env);
    }

    return scm_s_begin(body, eval_state);
}

ScmObj
scm_s_letstar(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj env     = eval_state->env;
    ScmObj var     = SCM_FALSE;
    ScmObj val     = SCM_FALSE;
    ScmObj binding = SCM_FALSE;
    DECLARE_FUNCTION("let*", syntax_variadic_tailrec_1);

    /*========================================================================
      (let* <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    if (!CONSP(bindings) && !NULLP(bindings))
        ERR("let*: syntax error");

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
        env = scm_extend_environment(LIST_1(var), LIST_1(val), env);
    }

    if (!NULLP(bindings))
        ERR_OBJ("invalid bindings form", bindings);

    eval_state->env = env;

    /* evaluate body */
    return scm_s_begin(body, eval_state);
}

ScmObj
scm_s_letrec(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj env      = eval_state->env;
    ScmObj frame    = SCM_FALSE;
    ScmObj vars     = SCM_NULL;
    ScmObj vals     = SCM_NULL;
    ScmObj binding  = SCM_FALSE;
    ScmObj var      = SCM_FALSE;
    ScmObj val      = SCM_FALSE;
    DECLARE_FUNCTION("letrec", syntax_variadic_tailrec_1);

    /*========================================================================
      (letrec <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    if (!CONSP(bindings) && !NULLP(bindings))
        ERR("letrec: syntax error");

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
    return scm_s_begin(body, eval_state);
}


/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.3 Sequencing
===========================================================================*/
ScmObj
scm_s_begin(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env  = eval_state->env;
    ScmObj expr = SCM_INVALID;
    DECLARE_FUNCTION("begin", syntax_variadic_tailrec_0);

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
ScmObj
scm_s_do(ScmObj bindings, ScmObj testframe, ScmObj commands, ScmEvalState *eval_state)
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
    DECLARE_FUNCTION("do", syntax_variadic_tailrec_2);

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
    env = scm_extend_environment(vars, vals, env);

    /* construct test */
    if (NULLP(testframe))
        ERR("invalid testframe");
    test       = CAR(testframe);
    expression = CDR(testframe);

    /* now execution phase! */
    while (FALSEP(EVAL(test, env))) {
        /* execute commands */
        EVAL(scm_s_begin(commands, eval_state), env);

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
        vals = scm_p_reverse(vals);

        /* set it */
        for (tmp_vars = vars;
             !NULLP(tmp_vars) && !NULLP(vals);
             tmp_vars = CDR(tmp_vars), vals = CDR(vals))
        {
            obj = scm_lookup_environment(CAR(tmp_vars), env);
            if (obj != SCM_INVALID_REF) {
                SET(obj, CAR(vals));
            } else {
                ERR("do: broken env");
            }
        }
    }

    eval_state->env = env;

    return NULLP(expression) ? EVAL(test, env) : scm_s_begin(expression, eval_state);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.5 Delayed evaluation
===========================================================================*/
ScmObj
scm_s_delay(ScmObj expr, ScmObj env)
{
    DECLARE_FUNCTION("delay", syntax_fixed_1);

    /* (lambda () exp) */
    return scm_make_closure(SCM_LIST_2(SCM_NULL, expr), env);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.6 Quasiquotation
===========================================================================*/

struct _qquote_result {
    ScmObj obj;
    tr_msg insn;
};

/**
 * Interpret a quasiquoted expression.
 *
 * @see qquote_vector()
 */
static qquote_result
qquote_internal(ScmObj input, ScmObj env, int nest)
{
    ScmObj obj;
    sequence_translator tr;
    qquote_result tmp_result;
    qquote_result my_result;
    DECLARE_INTERNAL_FUNCTION("quasiquote");

    if (VECTORP(input)) {
        TRV_INIT(tr, input);
    } else if (CONSP(input)) {
        TRL_INIT(tr, input);
        /* If INPUT has 2 or more elements, we process up to the
         * penultimate item and see if the tail has the form (<syn>
         * <datum>) where <syn> is unquote, unquote-splicing, or
         * quasiquote.
         */
        if (CONSP(CDR(input))) {
            for (; CONSP(CDDR(TRL_GET_SUBLS(tr))); TRL_NEXT(tr)) {
                obj = TRL_GET_OBJ(tr);
                tmp_result = qquote_internal(obj, env, nest);
                listran(&tr, tmp_result.insn, tmp_result.obj);
            }
            if (NULLP(CDDR(TRL_GET_SUBLS(tr)))) {
                ScmObj form;

                form = TRL_GET_SUBLS(tr);
                obj  = CAR(form);

                if (EQ(obj, SYM_QUASIQUOTE)) {
                    /* FORM == `x */
                    ++nest;
                } else if (EQ(obj, SYM_UNQUOTE)) {
                    /* FORM == ,x */
                    if (--nest == 0) {
                        TRL_SET_SUBLS(tr, EVAL(CADR(form), env));
                        my_result.obj  = TRL_EXTRACT(tr);
                        my_result.insn = TR_MSG_REPLACE;
                        return my_result;
                    }
                } else if (EQ(obj, SYM_UNQUOTE_SPLICING)) {
                    /* FORM == ,@x */
                    if (!EQ(form, input)) /* (a . ,@b) */
                        ERR_OBJ(",@ in wrong context", input);
                    if (--nest == 0) {
                        my_result.insn = TR_MSG_SPLICE;
                        my_result.obj  = EVAL(CADR(form), env);
                        return my_result;
                    }
                }
            }
        }
    } else {
        /* An atomic datum. */
        tmp_result.insn = TR_MSG_NOP;
        tmp_result.obj  = SCM_INVALID;
        return tmp_result;
    }

    /* Process all the other elements. */
    for (; !TR_ENDP(tr); TR_NEXT(tr)) {
        obj = TR_GET_OBJ(tr);
        tmp_result = qquote_internal(obj, env, nest);
        TR_CALL(tr, tmp_result.insn, tmp_result.obj);
    }

    /* Interpret the tail if an improper list. */
    if (CONSP(input) && !NULLP(TRL_GET_SUBLS(tr))) {
        tmp_result = qquote_internal(TRL_GET_SUBLS(tr), env, nest);
        if (tmp_result.insn != TR_MSG_NOP)
            TRL_SET_SUBLS(tr, tmp_result.obj);
    }

    my_result.obj = TR_EXTRACT(tr);
    my_result.insn = VALIDP(my_result.obj) ? TR_MSG_REPLACE : TR_MSG_NOP;
    return my_result;
}


ScmObj
scm_s_quasiquote(ScmObj datum, ScmObj env)
{
    qquote_result ret = qquote_internal(datum, env, 1);
    DECLARE_FUNCTION("quasiquote", syntax_fixed_1);

    switch (ret.insn) {
    case TR_MSG_NOP:
        return datum;
    case TR_MSG_SPLICE:
#if SCM_STRICT_R5RS
        ERR_OBJ("unquote-splicing in invalid context", datum);
#endif
        /* Otherwise fall through. */
    case TR_MSG_REPLACE:
        return ret.obj;
    default:
        ERR_OBJ("bug in quasiquote", datum);
    }
}

ScmObj
scm_s_unquote(ScmObj dummy, ScmObj env)
{
    DECLARE_FUNCTION("unquote", syntax_fixed_1);

    ERR("unquote outside quasiquote");
    return SCM_NULL;
}

ScmObj
scm_s_unquote_splicing(ScmObj dummy, ScmObj env)
{
    DECLARE_FUNCTION("unquote-splicing", syntax_fixed_1);

    ERR("unquote-splicing outside quasiquote");
    return SCM_NULL;
}


/*=======================================
  R5RS : 5.2 Definitions
=======================================*/
static void
define_internal(ScmObj var, ScmObj exp, ScmObj env)
{
    if (NULLP(env)) {
        /* given top-level environment */
        SCM_SYMBOL_SET_VCELL(var, EVAL(exp, env));
    } else {
        /* add val to the environment */
        env = scm_add_environment(var, EVAL(exp, env), env);
    }
}

ScmObj
scm_s_define(ScmObj var, ScmObj rest, ScmObj env)
{
    ScmObj procname = SCM_FALSE;
    ScmObj body     = SCM_FALSE;
    ScmObj formals  = SCM_FALSE;
    DECLARE_FUNCTION("define", syntax_variadic_1);

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
                        scm_make_closure(CONS(formals, body), env),
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
ScmObj
scm_p_scheme_report_environment(ScmObj version)
{
    DECLARE_FUNCTION("scheme-report-environment", procedure_fixed_1);

    /* sanity check */
    ASSERT_INTP(version);
    if (SCM_INT_VALUE(version) != 5)
        ERR_OBJ("version must be 5 but got", version);

#if SCM_STRICT_R5RS
    ERR("scheme-report-environment:" SCM_ERRMSG_NON_R5RS_ENV);
#else
    CDBG((SCM_DBG_COMPAT,
          "scheme-report-environment: warning:" SCM_ERRMSG_NON_R5RS_ENV));
#endif

    return SCM_INTERACTION_ENV;
}

ScmObj
scm_p_null_environment(ScmObj version)
{
    DECLARE_FUNCTION("null-environment", procedure_fixed_1);

    /* sanity check */
    ASSERT_INTP(version);
    if (SCM_INT_VALUE(version) != 5)
        ERR_OBJ("version must be 5 but got", version);

#if SCM_STRICT_R5RS
    ERR("null-environment:" SCM_ERRMSG_NON_R5RS_ENV);
#else
    CDBG((SCM_DBG_COMPAT,
          "null-environment: warning:" SCM_ERRMSG_NON_R5RS_ENV));
#endif

    return SCM_INTERACTION_ENV;
}

ScmObj
scm_p_interaction_environment(void)
{
    DECLARE_FUNCTION("interaction-environment", procedure_fixed_0);
    return SCM_INTERACTION_ENV;
}
