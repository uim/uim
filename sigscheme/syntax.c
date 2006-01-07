/*===========================================================================
 *  FileName : syntax.c
 *  About    : R5RS syntaxes
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

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static void define_internal(ScmObj var, ScmObj exp, ScmObj env);

/* Quasiquotation. */
typedef struct _qquote_result qquote_result;
static qquote_result qquote_internal(ScmObj input, ScmObj env, int nest);

/*=======================================
  Function Implementations
=======================================*/
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
#define TRV_GET_OBJ(_t)    (SCM_VECTOR_VEC((_t).u.vec.src)[(_t).u.vec.index])
#define TRV_NEXT(_t)       (++(_t).u.vec.index)
#define TRV_ENDP(_t)       (SCM_VECTOR_LEN((_t).u.vec.src) <= (_t).u.vec.index)
#define TRV_EXTRACT(_t)    (TRV_CALL((_t), TR_MSG_EXTRACT, SCM_INVALID))
#define TRV_CALL(_t, _m, _p) (vectran(&(_t), (_m), (_p)))

/* Polymorphic macros. */
#define TR_CALL(_t, _msg, _p) ((*(_t).trans)(&(_t), (_msg), (_p)))
#define TR_GET_OBJ(_t)     (TR_CALL((_t), TR_MSG_GET_OBJ, SCM_INVALID))
#define TR_NEXT(_t)        ((void)TR_CALL((_t), TR_MSG_NEXT, SCM_INVALID))
#define TR_ENDP(_t)        ((scm_bool)TR_CALL((_t), TR_MSG_ENDP, SCM_INVALID))
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
        splice_len = scm_length(obj);
#if SCM_STRICT_R5RS
        if (!SCM_LISTLEN_PROPERP(splice_len))
            ERR_OBJ("got bad splice list", obj);
#endif
        t->u.vec.growth += splice_len - 1;
        change_index = -t->u.vec.index - 1;
        goto record_change;

    case TR_MSG_REPLACE:
        change_index = t->u.vec.index;

      record_change:
        SCM_QUEUE_ADD(t->u.vec.q, CONS(MAKE_INT(change_index), obj));
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
            return MAKE_VECTOR(copy_buf, src_len + t->u.vec.growth);
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

#if SCM_STRICT_ARGCHECK
    if (SCM_LISTLEN_ERRORP(scm_validate_formals(formals)))
        ERR_OBJ("bad formals", formals);
#else
    /* Crashless no-validation:
     * Regard any non-list object as symbol. Since the lookup operation search
     * for a variable by EQ, this is safe although loosely allows
     * R5RS-incompatible code. */
#endif
    if (!CONSP(body))
        ERR_OBJ("at least one expression required", body);

    return MAKE_CLOSURE(CONS(formals, body), env);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.5 Conditionals
===========================================================================*/
ScmObj
scm_s_if(ScmObj test, ScmObj conseq, ScmObj rest, ScmEvalState *eval_state)
{
    ScmObj env, alt;
    DECLARE_FUNCTION("if", syntax_variadic_tailrec_2);

    env = eval_state->env;

    /*========================================================================
      (if <test> <consequent>)
      (if <test> <consequent> <alternate>)
    ========================================================================*/

    if (test = EVAL(test, env), NFALSEP(test)) {
#if SCM_STRICT_ARGCHECK
        SAFE_POP(rest);
        ASSERT_NO_MORE_ARG(rest);
#endif
        return conseq;
    } else {
        alt = (CONSP(rest)) ? CAR(rest) : SCM_UNDEF;
#if SCM_STRICT_ARGCHECK
        SAFE_POP(rest);
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
    ScmObj evaled;
    ScmRef locally_bound;
    DECLARE_FUNCTION("set!", syntax_fixed_2);

    ENSURE_SYMBOL(sym);

    evaled = EVAL(exp, env);
    locally_bound = scm_lookup_environment(sym, env);
    if (locally_bound == SCM_INVALID_REF) {
        /* Not found in the environment
           If symbol is not bound, error occurs */
        if (!SCM_SYMBOL_BOUNDP(sym))
            ERR_OBJ("unbound variable", sym);

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
    ScmObj env, clause, test, exps, proc;
    DECLARE_INTERNAL_FUNCTION("cond" /* , syntax_variadic_tailrec_0 */);

    env = eval_state->env;

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

    /* dirty hack to replace internal function name */
    if (VALIDP(case_key))
        SCM_MANGLE(name) = "case";

    if (NO_MORE_ARG(args))
        ERR("cond: syntax error: at least one clause required");

    /* looping in each clause */
    FOR_EACH (clause, args) {
        if (!CONSP(clause))
            ERR_OBJ("bad clause", clause);

        test = CAR(clause);
        exps = CDR(clause);

        if (EQ(test, SYM_ELSE)) {
            ASSERT_NO_MORE_ARG(args);
        } else {
            if (VALIDP(case_key)) {
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
                    ERR_OBJ("exp after => must be a procedure but got", proc);

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
    ScmObj expr, val;
    DECLARE_FUNCTION("and", syntax_variadic_tailrec_0);

    if (NO_MORE_ARG(args))
        return SCM_TRUE;

    FOR_EACH_WHILE (expr, args, CONSP(CDR(args))) {
        val = EVAL(expr, eval_state->env);
        if (FALSEP(val)) {
            ASSERT_PROPER_ARG_LIST(args);
            eval_state->ret_type = SCM_RETTYPE_AS_IS;
            return SCM_FALSE;
        }
    }
    expr = POP(args);
    ASSERT_NO_MORE_ARG(args);

    return expr;
}

ScmObj
scm_s_or(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj expr, val;
    DECLARE_FUNCTION("or", syntax_variadic_tailrec_0);

    if (NO_MORE_ARG(args))
        return SCM_FALSE;

    FOR_EACH (expr, args) {
        val = EVAL(expr, eval_state->env);
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
    ScmObj env, named_let_sym, proc, bindings, binding, body;
    ScmObj formals, var, actuals, val;
    ScmQueue varq, valq;
    DECLARE_FUNCTION("let", syntax_variadic_tailrec_0);

    env = eval_state->env;
    named_let_sym = SCM_FALSE;
    formals = SCM_NULL;
    actuals = SCM_NULL;

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

    if (!CONSP(args))
        ERR("let: invalid form");
    bindings = POP(args);

    /* named let */
    if (SYMBOLP(bindings)) {
        named_let_sym = bindings;

        if (!CONSP(args))
            ERR("let: invalid named let form");
        bindings = POP(args);
    }

    body = args;

    SCM_QUEUE_POINT_TO(varq, formals);
    SCM_QUEUE_POINT_TO(valq, actuals);
    FOR_EACH (binding, bindings) {
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

    env = scm_extend_environment(formals, actuals, env);
    eval_state->env = env;

    /* named let */
    if (SYMBOLP(named_let_sym)) {
        proc = MAKE_CLOSURE(CONS(formals, body), env);
        define_internal(named_let_sym, proc, env);
    }

    return scm_s_begin(body, eval_state);
}

ScmObj
scm_s_letstar(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj env, var, val, binding;
    DECLARE_FUNCTION("let*", syntax_variadic_tailrec_1);

    env = eval_state->env;

    /*========================================================================
      (let* <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/

    if (CONSP(bindings)) {
        FOR_EACH (binding, bindings) {
#if SCM_COMPAT_SIOD_BUGS
            /* temporary solution. the inefficiency is not a problem */
            if (LIST_1_P(binding))
                binding = LIST_2(CAR(binding), SCM_FALSE);
#endif

            if (!LIST_2_P(binding) || !SYMBOLP(var = CAR(binding)))
                goto err;
            val = EVAL(CADR(binding), env);

            /* extend env for each variable */
            env = scm_extend_environment(LIST_1(var), LIST_1(val), env);
        }
        if (!NULLP(bindings))
            goto err;
    } else if (NULLP(bindings)) {
        env = scm_extend_environment(SCM_NULL, SCM_NULL, env);
    } else {
        goto err;
    }

    eval_state->env = env;

    return scm_s_begin(body, eval_state);

 err:
    ERR_OBJ("invalid bindings form", bindings);
    /* NOTREACHED */
    return SCM_FALSE;
}

ScmObj
scm_s_letrec(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj binding, frame, formals, var, actuals, val;
    DECLARE_FUNCTION("letrec", syntax_variadic_tailrec_1);

    /*========================================================================
      (letrec <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/

    if (!LISTP(bindings))
        ERR("letrec: invalid bindings form");

    /* extend env by placeholder frame for subsequent lambda evaluations */
    /* FIXME: direct env object manipulation */
    frame = CONS(SCM_NULL, SCM_NULL);
    eval_state->env = CONS(frame, eval_state->env);

    formals = SCM_NULL;
    actuals = SCM_NULL;
    for (; CONSP(bindings); bindings = CDR(bindings)) {
        binding = CAR(bindings);
#if SCM_COMPAT_SIOD_BUGS
        /* temporary solution. the inefficiency is not a problem */
        if (LIST_1_P(binding))
            binding = LIST_2(CAR(binding), SCM_FALSE);
#endif

        if (!LIST_2_P(binding) || !SYMBOLP(var = CAR(binding)))
            ERR_OBJ("invalid binding form", binding);
        val = EVAL(CADR(binding), eval_state->env);

        /* construct formals and actuals list: any <init> must not refer a
           <variable> at this time */
        formals = CONS(var, formals);
        actuals = CONS(val, actuals);
    }

    if (!NULLP(bindings))
        ERR_OBJ("invalid bindings form", bindings);

    /* fill the placeholder frame */
    SET_CAR(frame, formals);
    SET_CDR(frame, actuals);

    return scm_s_begin(body, eval_state);
}


/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.3 Sequencing
===========================================================================*/
ScmObj
scm_s_begin(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj expr;
    DECLARE_FUNCTION("begin", syntax_variadic_tailrec_0);

    if (NO_MORE_ARG(args))
        return SCM_UNDEF;

    FOR_EACH_WHILE(expr, args, CONSP(CDR(args)))
        EVAL(expr, eval_state->env);

    expr = POP(args);

    ASSERT_NO_MORE_ARG(args);

    /* Return tail expression. */
    return expr;
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.4 Iteration
===========================================================================*/
/* FIXME:
 * - SEGV conditions by manual arg extraction
 * - side-effective arg in macros such as EVAL, NFALSEP
 * - expensive operations
 */
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
    ScmObj tmp_vars   = SCM_FALSE;
    ScmObj tmp;
    ScmRef obj;
    DECLARE_FUNCTION("do", syntax_variadic_tailrec_2);

    /* construct Environment and steps */
    FOR_EACH (binding, bindings) {
        if (NULLP(binding))
            ERR("invalid binding");

        var = MUST_POP_ARG(binding);
        ENSURE_SYMBOL(var);
        val = MUST_POP_ARG(binding);

        vars = CONS(var, vars);
        val  = EVAL(val, env);
        vals = CONS(val, vals);

        /* append <step> to steps */
        if (NO_MORE_ARG(binding))
            steps = CONS(var, steps);
        else
            steps = CONS(POP(binding), steps);

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
        FOR_EACH_PAIR (tmp, steps)
            vals = CONS(EVAL(CAR(tmp), env), vals);
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
    return MAKE_CLOSURE(SCM_LIST_2(SCM_NULL, expr), env);
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
                        /* FIXME: side-effective EVAL in another macro */
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
    qquote_result ret;
    DECLARE_FUNCTION("quasiquote", syntax_fixed_1);

    ret = qquote_internal(datum, env, 1);

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
    ScmObj val;

    if (NULLP(env)) {
        /* given top-level environment */
        val = EVAL(exp, env);
        SCM_SYMBOL_SET_VCELL(var, val);
    } else {
        /* add val to the environment */
        env = scm_add_environment(var, EVAL(exp, env), env);
    }
}

ScmObj
scm_s_define(ScmObj var, ScmObj rest, ScmObj env)
{
    ScmObj procname, body, formals, proc;
    DECLARE_FUNCTION("define", syntax_variadic_1);

    /*========================================================================
      (define <variable> <expression>)
    ========================================================================*/
    if (SYMBOLP(var)) {
        if (!LIST_1_P(rest))
            ERR_OBJ("exactly 1 arg required but got", rest);

        define_internal(var, CAR(rest), env);
    }

    /*========================================================================
      (define (<variable> . <formals>) <body>)

      => (define <variable>
             (lambda (<formals>) <body>))
    ========================================================================*/
    else if (CONSP(var)) {
        procname = CAR(var);
        formals  = CDR(var);
        body     = rest;

        ENSURE_SYMBOL(procname);
        proc = scm_s_lambda(formals, body, env);
        define_internal(procname, proc, env);
    } else {
        ERR_OBJ("syntax error", var);
    }

#if SCM_STRICT_R5RS
    return SCM_UNDEF;
#else
    return var;
#endif
}
