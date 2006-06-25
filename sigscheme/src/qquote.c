/*===========================================================================
 *  Filename : qquote.c
 *  About    : R5RS quasiquote
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

#include <config.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Definitions
=======================================*/
#define ERRMSG_BAD_SPLICE_LIST      "bad splice list"

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/

/*===========================================================================
  Utilities: Sequential Datum Translators
===========================================================================*/
/* Since the translator is only used for quasiquotation, and will not be used
 * for other purpose including macro
 * (http://d.hatena.ne.jp/jun0/20060403#1144019957), all codes have been made
 * qquote.c local. separate this again as generic utility if needed.
 *   -- YamaKen 2006-06-24 */

/*
 * These utilities are for copying a sequence with partial
 * modifications.  They're used for handling quasiquotation and macro
 * expansion.  Translator works as a copy-on-write iterator for a list
 * or vector.
 *
 * First, initialize the proper type of translator with either
 * TRL_INIT() or TRV_INIT(), supplying the datum to be duplicated.
 * Then, traverse over the `copy' by successively and alternately
 * calling TR_GET_ELM() and TR_NEXT().  If an item returned by
 * TR_GET_ELM() should be replaced, then call TR_EXECUTE() with the
 * message TR_MSG_REPLACE or TR_MSG_SPLICE (see their definition for
 * details).  When TR_ENDP() returns true, stop and obtain the
 * duplicate with TR_EXTRACT().  TR_CALL() is a low-level construct
 * that doesn't demultiplex the return value.  Usually you would want
 * TR_EXECUTE() instead.  The only exception is if you expect a
 * boolean to be returned (those that test true for TR_BOOL_MSG_P()).
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
    TR_MSG_GET_ELM,

    /** Advance the iterator on the input. */
    TR_MSG_NEXT,

    /** Extract the product. */
    TR_MSG_EXTRACT,

    /** True iff the end of the sequence has been reached. */
    TR_MSG_ENDP,

    /**
     * Splice OBJ and discard all cells at or after the current one
     * in the input.  Only implemented for list translators.
     */
    TRL_MSG_SET_SUBLS,

    TR_MSG_USR
#define TR_BOOL_MSG_P(m) ((m) == TR_MSG_ENDP)
};

typedef enum _tr_msg tr_msg;
typedef struct _tr_param tr_param;
typedef struct _list_translator list_translator;
typedef struct _vector_translator vector_translator;
typedef struct _sequence_translator sequence_translator;
typedef union _translator_ret translator_ret;

struct _tr_param {
    tr_msg msg;
    ScmObj obj;
};

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
    scm_int_t index;            /* Current position. */
    scm_int_t growth;
};

struct _sequence_translator {
    translator_ret (*trans)(sequence_translator *t, tr_msg msg, ScmObj obj);
    union {
        list_translator lst;
        vector_translator vec;
    } u;
};

union _translator_ret {
    ScmObj object;
    scm_bool boolean;
};

/*
 * Operations on translators.  If a list- or vector-specific macro has
 * the same name (sans prefix) as a polymorphic one, the former tends
 * to be faster.
 */

/* List-specific macros. */
#define TRL_INIT(_t, _in)     ((_t).u.lst.output = (_in),               \
                               SCM_QUEUE_POINT_TO((_t).u.lst.q,         \
                                                  (_t).u.lst.output),   \
                               (_t).u.lst.src = (_in),                  \
                               (_t).u.lst.cur = (_in),                  \
                               (_t).trans = scm_listran)
#define TRL_GET_ELM(_t)       (CAR((_t).u.lst.cur))
#define TRL_NEXT(_t)          ((_t).u.lst.cur = CDR((_t).u.lst.cur))
#define TRL_ENDP(_t)          (!CONSP((_t).u.lst.cur))
#define TRL_GET_SUBLS(_t)     ((_t).u.lst.cur)
#define TRL_SET_SUBLS(_t, _o) (TRL_CALL((_t), TRL_MSG_SET_SUBLS, (_o)))
#define TRL_EXTRACT(_t)       ((_t).u.lst.output)
#define TRL_CALL(_t, _m, _o)  (scm_listran(&(_t), (_m), (_o)))
#define TRL_EXECUTE(_t, _p)   (SCM_ASSERT(!TR_BOOL_MSG_P((_p).msg)),          \
                               scm_listran(&(_t), (_p).msg, (_p).obj).object)

#if SCM_USE_VECTOR
/* Vector-specific macros. */
#define TRV_INIT(_t, _in)  ((_t).u.vec.diff = SCM_NULL,                 \
                            SCM_QUEUE_POINT_TO((_t).u.vec.q,            \
                                               (_t).u.vec.diff),        \
                            (_t).u.vec.src = (_in),                     \
                            (_t).u.vec.index = 0,                       \
                            (_t).u.vec.growth = 0,                      \
                            (_t).trans = scm_vectran)
#define TRV_GET_ELM(_t)    (SCM_VECTOR_VEC((_t).u.vec.src)[(_t).u.vec.index])
#define TRV_NEXT(_t)       (++(_t).u.vec.index)
#define TRV_GET_INDEX(_t)  ((_t).u.vec.index)
#define TRV_GET_VEC(_t)    (SCM_VECTOR_VEC((_t).u.vec.src))
#define TRV_ENDP(_t)       (SCM_VECTOR_LEN((_t).u.vec.src) <= (_t).u.vec.index)
#define TRV_EXTRACT(_t)    (TRV_CALL((_t), TR_MSG_EXTRACT, SCM_INVALID).object)
#define TRV_EXECUTE(_t, _p)  (TRV_CALL((_t), (_p).msg, (_p).obj).object)
#define TRV_CALL(_t, _m, _o) (scm_vectran(&(_t), (_m), (_o)))
#endif /* SCM_USE_VECTOR */

/* Polymorphic macros. */
#define TR_CALL(_t, _msg, _o) ((*(_t).trans)(&(_t), (_msg), (_o)))
#define TR_EXECUTE(_t, _p) (TR_CALL((_t), (_p).msg, (_p).obj).object)
#define TR_GET_ELM(_t)     (TR_CALL((_t), TR_MSG_GET_ELM, SCM_INVALID).object)
#define TR_NEXT(_t)        ((void)TR_CALL((_t), TR_MSG_NEXT, SCM_INVALID))
#define TR_ENDP(_t)        (TR_CALL((_t), TR_MSG_ENDP, SCM_INVALID).boolean)
#define TR_EXTRACT(_t)     (TR_CALL((_t), TR_MSG_EXTRACT, SCM_INVALID).object)


/*=======================================
  Function Definitions
=======================================*/

static translator_ret scm_listran(sequence_translator *t, tr_msg msg,
                                  ScmObj obj);
#if SCM_USE_VECTOR
static translator_ret scm_vectran(sequence_translator *t, tr_msg msg,
                                  ScmObj obj);
#endif
static tr_param qquote_internal(ScmObj input, ScmObj env, scm_int_t nest);


#define RETURN_OBJECT(o)                        \
    do {                                        \
        translator_ret _ret;                    \
        _ret.object = (o);                      \
        return _ret;                            \
    } while (0)
#define RETURN_BOOLEAN(b)                       \
    do {                                        \
        translator_ret _ret;                    \
        _ret.boolean = (b);                     \
        return _ret;                            \
    } while (0)
/**
 * Performs (relatively) complex operations on a list translator.
 *
 * @see list_translator, tr_msg
 */
static translator_ret
scm_listran(sequence_translator *t, tr_msg msg, ScmObj obj)
{
    DECLARE_INTERNAL_FUNCTION("(list translator)");

    switch (msg) {
    case TR_MSG_NOP: /* for better performance */
        break;

    case TR_MSG_ENDP:
        RETURN_BOOLEAN(TRL_ENDP(*t));

    case TR_MSG_GET_ELM:
        RETURN_OBJECT(TRL_GET_ELM(*t));

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
#if SCM_STRICT_ARGCHECK
            if (!NULLP(SCM_QUEUE_TERMINATOR(t->u.lst.q)))
                ERR_OBJ(ERRMSG_BAD_SPLICE_LIST, obj);
#endif
            t->u.lst.src = obj = CDR(t->u.lst.cur);
        }
        SCM_QUEUE_SLOPPY_APPEND(t->u.lst.q, obj);
        break;

    case TR_MSG_EXTRACT:
        RETURN_OBJECT(TRL_EXTRACT(*t));

    default:
        SCM_ASSERT(scm_false);
    }
    RETURN_OBJECT(SCM_INVALID);
}

#if SCM_USE_VECTOR
#define REPLACED_INDEX(i) (i)
/* '- 1' allows zero as spliced index */
#define SPLICED_INDEX(i)  (-(i) - 1)

static translator_ret
scm_vectran(sequence_translator *t, tr_msg msg, ScmObj obj)
{
    ScmObj subst_rec, subst_index;
    scm_int_t splice_len;
    scm_int_t change_index;
    DECLARE_INTERNAL_FUNCTION("(vector translator)");

    switch (msg) {
    case TR_MSG_NOP: /* for better performance */
        break;

    case TR_MSG_GET_ELM:
        RETURN_OBJECT(TRV_GET_ELM(*t));

    case TR_MSG_NEXT:
        TRV_NEXT(*t);
        break;

    case TR_MSG_ENDP:
        RETURN_BOOLEAN(TRV_ENDP(*t));

    case TR_MSG_SPLICE:
        splice_len = scm_length(obj);
        /* obj MUST be a proper list regardless of strictness
         * configuration. Otherwise the encoded length feeds broken execution.
         *   -- YamaKen 2006-06-25 */
        if (!SCM_LISTLEN_PROPERP(splice_len))
            ERR_OBJ(ERRMSG_BAD_SPLICE_LIST, obj);
        t->u.vec.growth += splice_len - 1;
        change_index = SPLICED_INDEX(t->u.vec.index);
        goto record_change;

    case TR_MSG_REPLACE:
        change_index = REPLACED_INDEX(t->u.vec.index);

      record_change:
        subst_index = MAKE_INT(change_index);
        subst_rec = CONS(subst_index, obj);
        SCM_QUEUE_ADD(t->u.vec.q, subst_rec);
        break;

    case TR_MSG_EXTRACT:
        /* Create a new vector iff modifications have been recorded. */
        if (!NULLP(t->u.vec.diff)) {
            ScmObj *copy_buf, *src_buf;
            ScmObj diff, appendix, elm, ret;
            scm_int_t src_len, i, cpi;

            src_len = SCM_VECTOR_LEN(t->u.vec.src);
            src_buf = SCM_VECTOR_VEC(t->u.vec.src);
            copy_buf = scm_malloc((src_len + t->u.vec.growth)
                                  * sizeof(ScmObj));

            diff = t->u.vec.diff;
            change_index = SCM_INT_VALUE(CAAR(diff));
            for (i = cpi = 0; i < src_len; i++) {
                if (REPLACED_INDEX(i) == change_index) {
                    copy_buf[cpi++] = CDAR(diff);
                } else if (SPLICED_INDEX(i) == change_index) {
                    appendix = CDAR(diff);
                    FOR_EACH (elm, appendix)
                        copy_buf[cpi++] = elm;
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
            ret = MAKE_VECTOR(copy_buf, src_len + t->u.vec.growth);
            RETURN_OBJECT(ret);
        }
        RETURN_OBJECT(t->u.vec.src);

    default:
        SCM_ASSERT(scm_false);
    }
    RETURN_OBJECT(SCM_INVALID);
}

#undef REPLACED_INDEX
#undef SPLICED_INDEX
#endif /* SCM_USE_VECTOR */

#undef RETURN_OBJECT
#undef RETURN_BOOLEAN

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.6 Quasiquotation
===========================================================================*/

/**
 * Interpret a quasiquoted expression.
 */
static tr_param
qquote_internal(ScmObj input, ScmObj env, scm_int_t nest)
{
    ScmObj obj, form, args;
    sequence_translator tr;
    tr_param tmp_result;
    tr_param my_result;
    DECLARE_INTERNAL_FUNCTION("quasiquote");

    /*
     * syntax: quasiquote <qq template>
     * syntax: `<qq template>
     */

#if SCM_USE_VECTOR
    if (VECTORP(input)) {
        for (TRV_INIT(tr, input); !TRV_ENDP(tr); TRV_NEXT(tr)) {
            obj = TRV_GET_ELM(tr);
            tmp_result = qquote_internal(obj, env, nest);
            (void)TRV_EXECUTE(tr, tmp_result);
        }
    } else
#endif
    if (CONSP(input)) {
        /* This implementation adopt "minimum mercy" interpretation depending
         * on the R5RS specification cited below, to simplify the code.
         *
         * 4.2.6 Quasiquotation
         * Unpredictable behavior can result if any of the symbols quasiquote,
         * unquote, or unquote-splicing appear in positions within a <qq
         * template> otherwise than as described above. */
        for (TRL_INIT(tr, input); !TRL_ENDP(tr); TRL_NEXT(tr)) {
            ScmObj unwrapped;
            form = TRL_GET_SUBLS(tr);
            obj = CAR(form);
            unwrapped = SCM_UNWRAP_KEYWORD(obj);
            /*
             * R5RS: 7.1.4 Quasiquotations
             *
             * In <quasiquotation>s, a <list qq template D> can sometimes be
             * confused with either an <unquotation D> or a <splicing
             * unquotation D>. The interpretation as an <unquotation> or
             * <splicing unquotation D> takes precedence.
             */
            if (EQ(unwrapped, SYM_QUASIQUOTE)) {
                /* FORM == `x */
                if (args = CDR(form), !LIST_1_P(args))
                    ERR_OBJ("invalid quasiquote form", form);

                ++nest;
            } else if (EQ(unwrapped, SYM_UNQUOTE)) {
                /* FORM == ,x */
                if (args = CDR(form), !LIST_1_P(args))
                    ERR_OBJ("invalid unquote form", form);

                if (--nest == 0) {
                    obj = EVAL(CAR(args), env);
                    TRL_SET_SUBLS(tr, obj);
                    my_result.obj = TRL_EXTRACT(tr);
                    my_result.msg = TR_MSG_REPLACE;
                    return my_result;
                }
            } else if (EQ(unwrapped, SYM_UNQUOTE_SPLICING)) {
                /* FORM == ,@x */
                if (!EQ(form, input)) /* (a . ,@b) */
                    ERR_OBJ(",@ in invalid context", input);
                if (args = CDR(form), !LIST_1_P(args))
                    ERR_OBJ("invalid unquote-splicing form", form);

                if (--nest == 0) {
                    /* R5RS: 4.2.6 Quasiquotation
                     * If a comma appears followed immediately by an
                     * at-sign (@), then the following expression must
                     * evaluate to a list */
                    obj = EVAL(CAR(args), env);
                    /* Properness check of the list is performed on splice
                     * operation of (lis|vec)tran(). */
                    if (!LISTP(obj))
                        ERR(",@<x> must evaluate to a proper list");

                    my_result.obj = obj;
                    my_result.msg = TR_MSG_SPLICE;
                    return my_result;
                }
            }
            tmp_result = qquote_internal(obj, env, nest);
            (void)TRL_EXECUTE(tr, tmp_result);
        }
        /* Interpret the tail if an improper list. */
        if (!NULLP(TRL_GET_SUBLS(tr))) {
            tmp_result = qquote_internal(TRL_GET_SUBLS(tr), env, nest);
            SCM_ASSERT(tmp_result.msg != TR_MSG_SPLICE);
            if (tmp_result.msg == TR_MSG_REPLACE)
                TRL_SET_SUBLS(tr, tmp_result.obj);
        }
    } else {
        /* An atomic datum. */
#if SCM_USE_HYGIENIC_MACRO
        if (FARSYMBOLP(input)) {
            tmp_result.obj = SCM_UNWRAP_SYNTAX(input);
            tmp_result.msg = TR_MSG_REPLACE;
            return tmp_result;
        }
#endif
        tmp_result.obj = SCM_INVALID;
        tmp_result.msg = TR_MSG_NOP;
        return tmp_result;
    }

    my_result.obj = TR_EXTRACT(tr);
    my_result.msg = EQ(my_result.obj, input) ? TR_MSG_NOP : TR_MSG_REPLACE;
    return my_result;
}

SCM_EXPORT ScmObj
scm_s_quasiquote(ScmObj datum, ScmObj env)
{
    tr_param ret;
    DECLARE_FUNCTION("quasiquote", syntax_fixed_1);

    ret = qquote_internal(datum, env, 1);

    switch (ret.msg) {
    case TR_MSG_NOP:
        return datum;
    case TR_MSG_SPLICE:
        /* R5RS: 4.2.6 Quasiquotation
         * A comma at-sign should only appear within a list or vector <qq
         * template>. */
        ERR_OBJ(",@ in invalid context", datum);
        /* NOTREACHED */
    case TR_MSG_REPLACE:
        return ret.obj;
    default:
        SCM_ASSERT(scm_false);
        /* NOTREACHED */
        return SCM_FALSE;
    }
}

SCM_EXPORT ScmObj
scm_s_unquote(ScmObj dummy, ScmObj env)
{
    DECLARE_FUNCTION("unquote", syntax_fixed_1);

    ERR("unquote outside quasiquote");
    /* NOTREACHED */
    return SCM_FALSE;
}

SCM_EXPORT ScmObj
scm_s_unquote_splicing(ScmObj dummy, ScmObj env)
{
    DECLARE_FUNCTION("unquote-splicing", syntax_fixed_1);

    ERR("unquote-splicing outside quasiquote");
    /* NOTREACHED */
    return SCM_FALSE;
}
