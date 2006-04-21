/*===========================================================================
 *  FileName : macro.c
 *  About    : R5RS hygienic macros
 *
 *  Copyright (C) 2006 Jun Inoue <jun.lambda@gmail.com>
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

#include <stdlib.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"


/* ------------------------------
 *  Definitions
 * ------------------------------
 *
 * There doesn't seem to be a set of terminologies that is widely
 * agreed upon in the Scheme community regarding hygienic macros.
 * Here's a list of definitions and clarifications of the terms used
 * in SigScheme's macro facility.
 *
 * far symbol (farsym, wrapped identifier, wrapper):
 * An object that wraps an identifier with environment information.
 *
 * identifier (ident, variable):
 * A symbol or a far symbol.
 *
 * level:
 * A number associated with every subpattern, namely the number of
 * ellipses that affects the subpattern.  e.g. the pvar x in the
 * template ((x ...) ... y ...) is at level 2.
 *
 * <literals>:
 * The cadr of a valid syntax-rules form.  Note the <>.
 *
 * pattern variable (pvar, pv[prefix]):
 * An identifier that appears in a pattern which is not in <literals>.
 * Also, any identifier in a template that is eq? to a pattern
 * variable found in the corresponding pattern.  The latter may be
 * referred to as a pattern variable reference (pref).
 *
 * pattern variable marker (pvar marker, marker):
 * Objects found in a compiled pattern or template that mark where
 * pattern variables were.
 *
 * repeatable subpattern (reppat):
 * A subpattern *or subtemplate* designated as repeatable, i.e.
 * followed by an ellipsis.
 *
 * repeatable sublist (replist):
 * A repeatable subpattern that matches a list.
 *
 * repeatable subvector (repvector):
 * A repeatable subpattern that matches a vector.
 *
 * rule:
 * A pattern-template pair in a syntax-rules form.  Or, a pattern-body
 * pair in a case-lambda or match-case expression.
 *
 * sub:
 * A subform which is matched to a pattern variable, or a list of such
 * objects.  It derives, surprisingly perhaps, from `subform', only
 * contracted for disambiguation.  FIXME: Any better name?
 */

/* Tentative. */
#define SYM_ELLIPSIS scm_intern("...")
#define SYM_SYNTAX_RULES scm_intern("syntax-rules")

#define ELLIPSISP(o) EQ((o), SYM_ELLIPSIS)

#define MAKE_PVAR SCM_SUBPAT_MAKE_PVAR
#define PVAR_INDEX SCM_SUBPAT_PVAR_INDEX
#define PVARP SCM_SUBPAT_PVARP

#define MAKE_REPPAT SCM_SUBPAT_MAKE_REPPAT
#define REPPAT_PAT SCM_SUBPAT_REPPAT_PAT
#define REPPAT_PVCOUNT SCM_SUBPAT_REPPAT_PVCOUNT
#define REPPATP SCM_SUBPAT_REPPATP


#if SCM_DEBUG_MACRO
#define DBG_PRINT(args) (dbg_print args)
#define INIT_DBG() init_dbg()
#define DEFINE(name, init)  (SCM_SYMBOL_SET_VCELL(scm_intern((name)), (init)))
#include <stdarg.h>
enum dbg_flag {
    DBG_COMPILER     = 1 << 0,
    DBG_MATCHER      = 1 << 1,
    DBG_TRANSCRIPTOR = 1 << 2,
    DBG_FUNCALL      = 1 << 3,
    DBG_PVAR         = 1 << 4,
    DBG_RETURN       = 1 << 5,
    DBG_UNWRAP       = 1 << 6,
    DBG_EXPANDER     = 1 << 7
};

SCM_GLOBAL_VARS_BEGIN(static_macro);
#define static
static enum dbg_flag l_debug_mode;
#undef static
SCM_GLOBAL_VARS_END(static_macro);
#define l_debug_mode SCM_GLOBAL_VAR(static_macro, l_debug_mode)
SCM_DEFINE_STATIC_VARS(static_macro);

static void
dbg_print(enum dbg_flag mask, const char *fmt, ...)
{
    va_list va;

    if (mask & l_debug_mode) {
        va_start(va, fmt);
        scm_vformat(scm_err, SCM_FMT_INTERNAL, fmt, va);
        va_end(va);
    }
}

static ScmObj
scm_p_set_macro_debug_flagsx(ScmObj new_mode)
{
    SCM_ASSERT(INTP(new_mode));

    l_debug_mode = SCM_INT_VALUE(new_mode);
    return SCM_UNDEF;
}

static const struct scm_func_registration_info dbg_funcs[] = {
    { "set-macro-debug-flags!", scm_p_set_macro_debug_flagsx,
      SCM_PROCEDURE_FIXED_1 },
};

static void
init_dbg(void)
{
    SCM_GLOBAL_VARS_INIT(static_macro);
    l_debug_mode = 0;

    DEFINE("%debug-macro-compiler", MAKE_INT(DBG_COMPILER));
    DEFINE("%debug-macro-matcher", MAKE_INT(DBG_MATCHER));
    DEFINE("%debug-macro-transcriptor", MAKE_INT(DBG_TRANSCRIPTOR));
    DEFINE("%debug-macro-funcall", MAKE_INT(DBG_FUNCALL));
    DEFINE("%debug-macro-pvar", MAKE_INT(DBG_PVAR));
    DEFINE("%debug-macro-return", MAKE_INT(DBG_RETURN));
    DEFINE("%debug-macro-unwrap", MAKE_INT(DBG_UNWRAP));
    DEFINE("%debug-macro-expander", MAKE_INT(DBG_EXPANDER));
    scm_register_funcs(dbg_funcs);
}

#else  /* not SCM_DEBUG_MACRO */
#define DBG_PRINT(args) SCM_EMPTY_EXPR
#define INIT_DBG()      SCM_EMPTY_EXPR
#endif /* not SCM_DEBUG_MACRO */


typedef struct {
    ScmObj syms;
    ScmObj vals;
} var_map;

typedef struct {
    ScmObj pvmark;              /** Pvar marker in patterns. */
    var_map pvars;              /** Pvar -> level/marker. */
    scm_int_t pvlen;            /** Length of pvars.syms */
    ScmObj literals;
    struct {
        scm_bool pattern;       /* False if compiling template. */
    } mode;
} compilation_context;

static ScmObj compile(compilation_context *ctx, ScmObj form);
static ScmObj match(ScmObj pattern, ScmObj form, ScmPackedEnv def_penv, ScmPackedEnv use_penv, ScmObj env);
static ScmObj transcribe(ScmObj template, ScmObj sub, ScmPackedEnv def_penv, ScmObj use_env);

static ScmObj expand_hygienic_macro(ScmObj macro, ScmObj args, ScmObj env);

static scm_int_t list_find_index(ScmObj x, ScmObj ls);


SCM_EXPORT void
scm_init_macro(void)
{
    /* TODO: parameterize EVAL in scm_s_let(), scm_s_letrec() and
     * scm_s_define(), and call them with EVAL set to
     * eval_syntax_rules().  We might want to separate macros
     * namespace from that of objects since macros aren't first-class
     * objects. */
    scm_define_alias("let-syntax", "let");
    scm_define_alias("letrec-syntax", "letrec");
    scm_define_alias("define-syntax", "define");
    INIT_DBG();
}

SCM_EXPORT void SCM_NORETURN
scm_macro_bad_scope(ScmObj id)
{
    PLAIN_ERR("Identifier ~s found in wrong context.  "
              "Possibly, a macro was passed around like an object "
              "or a syntax didn't syntax-unwrap its arguments properly.",
              id);
}

static ScmObj
expand_hygienic_macro(ScmObj macro, ScmObj args, ScmObj env)
{
    ScmObj sub, rule, rules;
    ScmPackedEnv use_penv, def_penv;
    DECLARE_INTERNAL_FUNCTION("(expand_hygienic_macro)");

    rules = SCM_HMACRO_RULES(macro);
    def_penv = SCM_HMACRO_ENV(macro);
    use_penv = scm_pack_env(env);

    FOR_EACH (rule, rules) {
        sub = match(CAR(rule), args, def_penv, use_penv, env);
        if (VALIDP(sub))
            return transcribe(CDR(rule), sub, SCM_HMACRO_ENV(macro),
                              env);
    }
    ERR_OBJ("no matching pattern for", args);
    /* Not reached. */
}

SCM_EXPORT ScmObj
scm_expand_macro(ScmObj macro, ScmObj args, ScmEvalState *eval_state)
{
    ScmObj ret;
    DECLARE_INTERNAL_FUNCTION("scm_expand_macro");

    eval_state->ret_type = SCM_VALTYPE_NEED_EVAL;
#if SCM_STRICT_R5RS
    if (!SCM_LISTLEN_PROPERP(scm_length(args)))
        ERR_OBJ("bad argument list", args);
#endif
    ret = expand_hygienic_macro(macro, args, eval_state->env);
    DBG_PRINT((DBG_EXPANDER, "expanded to ~s\n", ret));
    return ret;
}

SCM_EXPORT ScmObj
scm_s_expand_macro(ScmObj macro, ScmObj args, ScmEvalState *eval_state)
{
    ScmObj ret;
    DECLARE_FUNCTION("expand-macro", syntax_variadic_tailrec_1);

    ret = scm_expand_macro(EVAL(macro, eval_state->env), args, eval_state);
    eval_state->ret_type = SCM_VALTYPE_AS_IS;
    return ret;
}

SCM_EXPORT ScmObj
scm_s_syntax_rules(ScmObj args, ScmObj env)
{
    ScmObj rule, compiled_rules;
    ScmQueue q;
    compilation_context ctx;
    DECLARE_FUNCTION("syntax-rules", syntax_variadic_0);

    if (NULLP(args) || NULLP(CDR(args)))
        ERR_OBJ("missing rules", args);

    ctx.literals = CAR(args);
    args = CDR(args);
#if SCM_STRICT_ARGCHECK
    /* Check literals. */
    {
        ScmObj pair;
        FOR_EACH_PAIR (pair, ctx.literals)
            ENSURE_IDENTIFIER(CAR(pair));
        if (!NULLP(pair))
            ERR_OBJ("bad <literals>", ctx.literals);
    }
#endif

    ctx.pvmark = MAKE_PVAR(SCM_NULL, 0);
    SCM_QUEUE_POINT_TO(q, compiled_rules);

    FOR_EACH (rule, args) {
        ScmObj pat, tmpl;
        /* ((_ . pattern) template) */
        if (!LIST_2_P(rule) || !CONSP(CAR(rule)))
            ERR_OBJ("malformed syntax rule", rule);
#if SCM_STRICT_ARGCHECK
        if (!IDENTIFIERP(CAAR(rule)))
            ERR_OBJ("pattern must start with an identifier", rule);
#endif
        ctx.pvars.syms   = SCM_NULL;
        ctx.pvars.vals   = SCM_NULL;
        ctx.pvlen        = 0;
        ctx.mode.pattern = scm_true;
        pat  = compile(&ctx, CDAR(rule));
        ctx.mode.pattern = scm_false;
        /* ctx.pvlen = 0; Not necessary because we just need its
         * change, not the absolute value. */
        tmpl = compile(&ctx, CADR(rule));
        SCM_QUEUE_ADD(q, CONS(pat, tmpl));
    }

    return MAKE_HYGIENIC_MACRO(compiled_rules, env);
}

SCM_EXPORT ScmObj
scm_s_match(ScmObj form, ScmObj clauses, ScmEvalState *state)
{
    ScmObj clause;
    compilation_context ctx;
    DECLARE_FUNCTION("match", syntax_variadic_tailrec_1);

    form             = EVAL(form, state->env);
    ctx.pvmark       = MAKE_PVAR(SCM_NULL, 0);
    ctx.literals     = SCM_NULL;
    ctx.mode.pattern = scm_true;
    /* (match <datum>
     *  (<pattern>)
     *  (<pattern>))
     */
    FOR_EACH (clause, clauses) {
        ScmObj pat, sub;
        if (!CONSP(clause))
            ERR_OBJ("malformed match clause", clause);
        ctx.pvars.syms = SCM_NULL;
        ctx.pvars.vals = SCM_NULL;
        ctx.pvlen      = 0;

        pat = compile(&ctx, SCM_UNWRAP_SYNTAX(CAR(clause)));

        /* match should never invoke scm_identifierequalp() since at
         * the moment no construct to literalize an identifier is
         * provided. */
        sub = match(pat, form,
                    0, 0, SCM_INVALID);
        if (VALIDP(sub)) {
            /* FIXME: directly manipulates environment. */
            state->env = CONS(CONS(ctx.pvars.syms, sub), state->env);

            /* FIXME: enforce proper tail recursion (but don't let
             * code that does no matching pay for it!). */
            return scm_s_begin(CDR(clause), state);
        }
    }
    return SCM_UNDEF;
}


/* ==============================
 * Pattern Compiler
 * ==============================*/


static ScmObj compile_rec(compilation_context *ctx, ScmObj form,
                          scm_int_t level);

static ScmObj
compile(compilation_context *ctx, ScmObj form)
{
    DECLARE_INTERNAL_FUNCTION("compile (pattern or template)");

    if (ELLIPSISP(form))
        ERR("misplaced ellipsis");
    return compile_rec(ctx, form, 0);
}

static ScmObj
compile_rec(compilation_context *ctx, ScmObj form, scm_int_t level)
{
    DECLARE_INTERNAL_FUNCTION("compile (pattern or template)");

    DBG_PRINT((DBG_COMPILER | DBG_FUNCALL, "compiling ~s [~S lv ~MD]\n",
               form, ctx->mode.pattern ? "pattern" : "template", level));
    /* TODO: maybe rewrite it using translators?  Not sure how much
     * advantage that would provide, though... */
    if (CONSP(form)) {
        ScmObj out, obj, rest;
        ScmQueue q;

        rest = out = form;
        SCM_QUEUE_POINT_TO(q, out);

      restart:
        if (ELLIPSISP(CAR(rest)))
            ERR_OBJ("misplaced ellipsis", form);
        FOR_EACH_BUTLAST (obj, rest) {
            if (ELLIPSISP(CAR(rest))) {
                ScmObj subpat;
                scm_int_t pvlen_before;

                pvlen_before = ctx->pvlen;
                rest = CDR(rest);
                subpat = compile_rec(ctx, obj, level + 1);
                subpat = MAKE_REPPAT(subpat, ctx->pvlen - pvlen_before);
                SCM_QUEUE_ADD(q, subpat);

                if (ctx->mode.pattern) {
                    if (!NULLP(rest))
                        ERR_OBJ("misplaced ellipsis", form);
                    return out;
                }
                if (ctx->pvlen == pvlen_before)
                    ERR_OBJ("constant repeatable subtemplate", form);
                if (CONSP(rest))
                    goto restart;
                return out;
            } else {
                SCM_QUEUE_ADD(q, compile_rec(ctx, obj, level));
            }
        }
        SCM_ASSERT(!ELLIPSISP(obj));
        SCM_QUEUE_ADD(q, compile_rec(ctx, obj, level));
        if (!NULLP(rest)) {
            if (ELLIPSISP(rest))
                ERR_OBJ("misplaced ellipsis", form);
            SCM_QUEUE_SLOPPY_APPEND(q, compile_rec(ctx, rest, level));
        }
        return out;
    } else if (VECTORP(form)) {
        scm_int_t i, j, ellipses, len;
        scm_bool ellipsis_ok, constantp;
        ScmObj *invec, *outvec;
        ScmObj out;

        len = SCM_VECTOR_LEN(form);
        if (!len) return form;
        invec = SCM_VECTOR_VEC(form);

        /* Count ellipses. */
        ellipsis_ok = scm_false;
        constantp   = scm_true;
        ellipses    = 0;
        for (i = 0; i < len; i++) {
            if (constantp && (CONSP(invec[i]) || IDENTIFIERP(invec[i])
                              || VECTORP(invec[i])))
                constantp = scm_false;
            if (ELLIPSISP(invec[i])) {
                if (!ellipsis_ok)
                    ERR_OBJ("misplaced ellipsis", form);
                ++ellipses;
                ellipsis_ok = scm_false;
            } else {
                ellipsis_ok = scm_true;
            }
        }
        if (constantp) return form;
        if (ctx->mode.pattern) {
            /* At most one ellipsis at the end. */
            if (1 < ellipses || (ellipses && !ELLIPSISP(invec[len - 1])))
                ERR_OBJ("misplaced ellipsis", form);
        }

        outvec = scm_malloc((len - ellipses) * sizeof(ScmObj));
        /* Fill with something recognized by GC. */
        for (i = 0; i < len - ellipses; i++)
            outvec[i] = SCM_NULL;
        out = MAKE_VECTOR(outvec, len - ellipses);

        /* i = input index, j = output index. */
        for (i = 1, j = 0; i < len; i++, j++) {
            SCM_ASSERT(j < len - ellipses);
            if (ELLIPSISP(invec[i])) {
                ScmObj subpat;
                scm_int_t pvlen_before;

                pvlen_before = ctx->pvlen;
                subpat = compile_rec(ctx, invec[i - 1], level + 1);
                if (!ctx->mode.pattern && ctx->pvlen == pvlen_before)
                    ERR_OBJ("constant repeatable subtemplate", form);
                outvec[j] = MAKE_REPPAT(subpat, ctx->pvlen - pvlen_before);
                if (++i == len)
                    return out;
            } else {
                outvec[j] = compile_rec(ctx, invec[i - 1], level);
            }
        }
        SCM_ASSERT(i == len);
        outvec[j] = compile_rec(ctx, invec[i - 1], level);
        return out;
    } else if (IDENTIFIERP(form)) {
        /* ctx->pvars.vals initially contains a list of levels.  Then
         * during template compilation, pvar markers are cached as
         * they are created by consing them to the vals list, like so:
         *
         *               syntax rule: ((_ (a ...) b c) '(a b))
         *                      syms: (c b a)
         * compile pattern  -> vals = (0 0 1)
         * compile template -> vals = (0
         *                             (#<pvar index=1> . 0)
         *                             (#<pvar index=2> . 1))
         */
        if (ctx->mode.pattern) {
            if (FALSEP(scm_p_memq(form, ctx->literals))) {
                if (!FALSEP(scm_p_memq(form, ctx->pvars.syms)))
                    ERR_OBJ("duplicate pattern variable", form);
                ctx->pvars.syms = CONS(form, ctx->pvars.syms);
                ctx->pvars.vals = CONS(MAKE_INT(level), ctx->pvars.vals);
                ++ctx->pvlen;
#if SCM_DEBUG_MACRO
                return MAKE_PVAR(form, 0);
#else
                return ctx->pvmark;
#endif
            }
            return form;
        } else {
            scm_int_t index;

            index = list_find_index(form, ctx->pvars.syms);
            if (index < 0) {
                /* Not found; FORM is a free variable. */
                return form;
            } else {
                ScmObj tail, ret;
                scm_int_t pvlevel;

                ++ctx->pvlen;
                tail = scm_list_tail(ctx->pvars.vals, index);
                SCM_ASSERT(CONSP(tail));
                if (CONSP(CAR(tail))) {
                    SCM_ASSERT(PVARP(CAAR(tail)) && INTP(CDAR(tail)));
                    pvlevel = SCM_INT_VALUE(CDAR(tail));
                    ret = CAAR(tail);
                } else {
                    SCM_ASSERT(INTP(CAR(tail)));
                    pvlevel = SCM_INT_VALUE(CAR(tail));
                    ret = MAKE_PVAR(form, index);
                    SET_CAR(tail, CONS(ret, CAR(tail)));
                }
                if (level != pvlevel) {
                    ERR_OBJ("pattern variable used at wrong level", form);
                }
                return ret;
            }
        }
    }
    return form;
}


/* ==============================
 * Pattern Matcher
 * ==============================*/

typedef struct {
    ScmPackedEnv def_penv;
    ScmPackedEnv use_penv;
    ScmObj use_env;
    ScmObj sub;                 /** Objects that matched a pvar.
                                 * Populated only if it's VALIDP(). */
} match_context;

static void merge_subs(ScmObj to, ScmObj from, ScmObj end);
static scm_bool match_rec(match_context *ctx, ScmObj pat, ScmObj form);
static scm_bool match_reppat(match_context *ctx, ScmObj arg, ScmObj form);

/**
 * Matches FORM against RULES.
 *
 * @param env Environment of the macro use.
 */
static ScmObj
match(ScmObj pattern, ScmObj form, ScmPackedEnv def_penv,
      ScmPackedEnv use_penv, ScmObj use_env)
{
    match_context ctx;
    DECLARE_INTERNAL_FUNCTION("(match)");

    DBG_PRINT((DBG_MATCHER | DBG_FUNCALL, "match: ~s =~~ ~s\n",
               form, pattern));

    ctx.def_penv = def_penv;
    ctx.use_penv = use_penv;
    ctx.use_env  = use_env;
    ctx.sub = SCM_INVALID;     /* Not investing storage yet. */

    if (match_rec(&ctx, pattern, form)) {
        /* Matched. */
        ctx.sub = SCM_NULL;
        match_rec(&ctx, pattern, form);
    }
    DBG_PRINT((DBG_MATCHER | DBG_RETURN, "match done, returning ~s\n",
               VALIDP(ctx.sub) ? ctx.sub : SCM_UNDEF));
    return ctx.sub;
}

#define MATCH_REC(c, p, f)                      \
    do {                                        \
        if (!match_rec((c), (p), (f)))          \
            return scm_false;                   \
    } while (0)

#define MISMATCH(reason, pat, form)                                          \
    do {                                                                     \
        DBG_PRINT((DBG_MATCHER, "~s !~~ ~s ~S\n", (form), (pat), (reason))); \
        return scm_false;                                                    \
    } while (0)

static scm_bool
match_rec(match_context *ctx, ScmObj pat, ScmObj form)
{
#if SCM_DEBUG_MACRO
    ScmObj pat_save = pat;
    ScmObj form_save = form;
#endif
    DECLARE_INTERNAL_FUNCTION("(<pattern>)");

    DBG_PRINT((DBG_MATCHER | DBG_FUNCALL, "match_rec: ~s =~~ ~s\n",
               form, pat));

    SCM_ASSERT(!(SUBPATP(pat) && REPPATP(pat)));

    FOR_EACH_PAIR (pat, pat) {
        ScmObj subpat = CAR(pat);
        if (SUBPATP(subpat) && REPPATP(subpat))
            return match_reppat(ctx, pat, form);
        if (!CONSP(form))
            MISMATCH("form too short", pat_save, form_save);
        MATCH_REC(ctx, subpat, CAR(form));
        form = CDR(form);
    }

    if (SUBPATP(pat)) {
        SCM_ASSERT(PVARP(pat));
        if (VALIDP(ctx->sub))
            ctx->sub = CONS(form, ctx->sub);
        return scm_true;
    }

    if (VECTORP(pat)) {
        scm_int_t plen, flen, len, i;
        ScmObj *pvec, *fvec;
        if (!VECTORP(form))
            return scm_false;
        plen = SCM_VECTOR_LEN(pat);
        flen = SCM_VECTOR_LEN(form);
        if (!plen)
            return !flen;
        pvec = SCM_VECTOR_VEC(pat);
        fvec = SCM_VECTOR_VEC(form);
        if (SUBPATP(pvec[plen - 1]) && REPPATP(pvec[plen - 1])) {
            len = plen - 1;
            if (SCM_VECTOR_LEN(form) < len)
                MISMATCH("form too short", pat_save, form_save);
        } else {
            if (plen != flen)
                MISMATCH("length mismatch", pat_save, form_save);
            len = plen;
        }

        for (i = 0; i < len; i++)
            MATCH_REC(ctx, pvec[i], fvec[i]);
        if (plen != len)
            return match_reppat(ctx, pat, form);
        return scm_true;
    } else if (IDENTIFIERP(pat)) {
        if (!IDENTIFIERP(form))
            MISMATCH("wrong atom", pat_save, form_save);
        if (!scm_identifierequalp(pat, ctx->def_penv, form,
                                  ctx->use_penv, ctx->use_env))
            MISMATCH("wrong name or binding", pat_save, form_save);
        return scm_true;
    }

    if (!FALSEP(scm_p_equalp(pat, form)))
        return scm_true;
    MISMATCH("wrong atom", pat_save, form_save);
    /* Not reached. */
}


/* FIXME: give arg a better name. */
static scm_bool
match_reppat(match_context *ctx, ScmObj arg, ScmObj form)
{
    ScmObj pat, sub_save, accum, reppat;
    scm_int_t i;

    DBG_PRINT((DBG_MATCHER | DBG_FUNCALL, "match_reppat: ~s =~~ ~s\n",
               form, arg));

    if (CONSP(arg)) {
        reppat = CAR(arg);
        pat = REPPAT_PAT(reppat);

        if (SUBPATP(pat)) {
            /* (pvar ...) */
            SCM_ASSERT(PVARP(pat));
            if (!SCM_LISTLEN_PROPERP(scm_length(form)))
                MISMATCH("repeatable subpattern matched against "
                         "improper list, vector, or atom",
                         pat, form);
            if (VALIDP(ctx->sub))
                ctx->sub = CONS(form, ctx->sub);
            return scm_true;
        }
    } else {
        SCM_ASSERT(VECTORP(arg));
        i = SCM_VECTOR_LEN(arg) - 1;
        SCM_ASSERT(i >= 0);
        SCM_ASSERT(i <= SCM_VECTOR_LEN(form));
        reppat = SCM_VECTOR_VEC(arg)[i];
        pat = REPPAT_PAT(reppat);
    }
    SCM_ASSERT(SUBPATP(reppat) && REPPATP(reppat));

    accum = sub_save = ctx->sub;
    /* Populate with empty subs. */
    if (VALIDP(ctx->sub)) {
        scm_int_t pvcount;
        for (pvcount = REPPAT_PVCOUNT(reppat); pvcount--;)
            accum = CONS(SCM_NULL, accum);
    }

    if (CONSP(arg)) {
        ScmObj subform;
        FOR_EACH (subform, form) {
            ctx->sub = sub_save;
            MATCH_REC(ctx, pat, subform);
            merge_subs(accum, ctx->sub, sub_save);
        }
        if (!NULLP(form))
            MISMATCH("repeatable subpattern matched against "
                     "improper list, vector, or atom",
                     pat, form);
    } else {                    /* VECTORP(arg) */
        scm_int_t len;
        ScmObj *vec;

        SCM_ASSERT(VECTORP(form));
        len = SCM_VECTOR_LEN(form);
        vec = SCM_VECTOR_VEC(form);
        /* i = SCM_VECTOR_LEN(pat) - 1; */
        for (; i < len; i++) {
            ctx->sub = sub_save;
            MATCH_REC(ctx, pat, vec[i]);
            merge_subs(accum, ctx->sub, sub_save);
        }
    }

    for (ctx->sub = accum; !EQ(accum, sub_save); accum = CDR(accum))
        SET_CAR(accum, scm_p_reversex(CAR(accum)));

    return scm_true;
}

/* ;; Push new bindings at the front.
 * (map!
 *  (cut cons <> <>)
 *  ctx->sub
 *  (take-until (cut eq? <> end) from))
 *
 * If you're hacking on this code, beware that the arguments can be
 * all SCM_INVALID.
 */
static void
merge_subs(ScmObj to, ScmObj from, ScmObj end)
{
    DBG_PRINT((DBG_MATCHER | DBG_FUNCALL, "merging ~s ++ ~s\n",
               VALIDP(to) ? to : SCM_UNDEF,
               VALIDP(from) ? from : SCM_UNDEF));
    while (!EQ(from, end)) {
        ScmObj next;
        next = CDR(from);
        SET_CDR(from, CAR(to));
        SET_CAR(to, from);
        to = CDR(to);
        from = next;
    }
}


/* ==============================
 * Template Transcription
 * ============================== */

#define DEFAULT_INDEX_BUF_SIZE 16

typedef struct {
    ScmObj fvars;              /* Alist; free variables -> wrapped ident. */
    scm_int_t index_buf[DEFAULT_INDEX_BUF_SIZE];
    scm_int_t *indices;
    scm_int_t index_buf_size;
    ScmPackedEnv def_penv;
    ScmObj def_env;
    ScmObj use_env;
} transcription_context;

typedef struct {
    enum {
        MSG_REPLACE,
        MSG_SPLICE,
        MSG_PVAR_EXHAUSTED
    } msg;
    union {
        ScmObj obj;
        scm_int_t exhausted_level;
    } u;
} transcribe_ret;

static transcribe_ret transcribe_rec(transcription_context *ctx,
                                     ScmObj template, ScmObj sub,
                                     scm_int_t level);
static transcribe_ret transcribe_reppat(transcription_context *ctx,
                                        ScmObj template, ScmObj sub,
                                        scm_int_t level);

static ScmObj
transcribe(ScmObj template, ScmObj sub, ScmPackedEnv def_penv, ScmObj use_env)
{
    transcribe_ret ret;
    transcription_context ctx;

    DBG_PRINT((DBG_TRANSCRIPTOR | DBG_FUNCALL, "transcribe\n"));
    ctx.fvars = SCM_NULL;
    ctx.indices = ctx.index_buf;
    ctx.index_buf_size = DEFAULT_INDEX_BUF_SIZE;
    ctx.def_penv = def_penv;
    ctx.def_env = scm_unpack_env(def_penv, use_env);
    ctx.use_env = use_env;
    ret = transcribe_rec(&ctx, template, sub, 0);
    SCM_ASSERT(ret.msg == MSG_REPLACE);
    if (ctx.indices != ctx.index_buf) {
        free(ctx.indices);
        DBG_PRINT((DBG_TRANSCRIPTOR, "freed dynamic buffer\n"));
    }
    DBG_PRINT((DBG_TRANSCRIPTOR | DBG_RETURN, "transcribe: returning ~s\n",
               ret.u.obj));
    return ret.u.obj;
}

static transcribe_ret
transcribe_rec(transcription_context *ctx, ScmObj template, ScmObj sub,
               scm_int_t level)
{
    transcribe_ret ret;

    DBG_PRINT((DBG_TRANSCRIPTOR | DBG_FUNCALL,
               "transcribe_rec [lv ~MD] ~s | ~s\n",
               level, template, sub));

#define RECURSE(_q, _obj)                                       \
        do {                                                    \
            transcribe_ret r;                                   \
            r = transcribe_rec(ctx, (_obj), sub, level);        \
            switch (r.msg) {                                    \
            case MSG_PVAR_EXHAUSTED:                            \
                return r;                                       \
            case MSG_REPLACE:                                   \
                SCM_QUEUE_ADD((_q), r.u.obj); break;            \
            case MSG_SPLICE:                                    \
                SCM_QUEUE_APPEND((_q), r.u.obj); break;         \
            default:                                            \
                SCM_ASSERT(scm_false);                          \
            }                                                   \
        } while (0)

#define RECURSE_ALWAYS_APPEND(_q, _obj)                         \
        do {                                                    \
            transcribe_ret r;                                   \
            r = transcribe_rec(ctx, (_obj), sub, level);        \
            if (r.msg == MSG_PVAR_EXHAUSTED)                    \
                return r;                                       \
            SCM_ASSERT(r.msg == MSG_REPLACE);                   \
            SCM_QUEUE_SLOPPY_APPEND((_q), r.u.obj);             \
        } while (0)

    if (CONSP(template)) {
        ScmObj tmp;
        ScmQueue q;
        ret.msg = MSG_REPLACE;
        ret.u.obj = SCM_NULL;
        SCM_QUEUE_POINT_TO(q, ret.u.obj);
        FOR_EACH (tmp, template)
            RECURSE(q, tmp);
        SCM_ASSERT(!(SUBPATP(template) && REPPATP(template)));
        if (!NULLP(template))
            RECURSE_ALWAYS_APPEND(q, template);
        return ret;
    } else if (VECTORP(template)) {
        scm_int_t i, len;
        ScmObj *vec;
        ScmQueue q;

        ret.msg = MSG_REPLACE;
        ret.u.obj = SCM_NULL;
        SCM_QUEUE_POINT_TO(q, ret.u.obj);
        vec = SCM_VECTOR_VEC(template);
        len = SCM_VECTOR_LEN(template);
        for (i = 0; i < len; i++)
            RECURSE(q, vec[i]);
        ret.u.obj = scm_p_list2vector(ret.u.obj);
        return ret;
    } else if (SUBPATP(template)) {
        if (PVARP(template)) {
            scm_int_t i;
            sub = scm_list_tail(sub, PVAR_INDEX(template));
            SCM_ASSERT(CONSP(sub));
            sub = CAR(sub);
            for (i = 0; i < level; i++) {
                DBG_PRINT((DBG_TRANSCRIPTOR, "ref (~MD) ; ~s\n",
                           ctx->indices[i], sub));
                sub = scm_list_tail(sub, ctx->indices[i]);
                SCM_ASSERT(VALIDP(sub));
                if (NULLP(sub)) {
                    ret.msg = MSG_PVAR_EXHAUSTED;
                    ret.u.exhausted_level = i;
                    return ret;
                }
                sub = CAR(sub);
            }
            ret.msg = MSG_REPLACE;
            ret.u.obj = sub;
            return ret;
        }
        /* REPPATP(template) */
        return transcribe_reppat(ctx, template, sub, level);
    } else if (IDENTIFIERP(template)) {
        ScmObj wrapped;

        wrapped = scm_p_assq(template, ctx->fvars);
        if (FALSEP(wrapped)) {
            wrapped = scm_wrap_identifier(template, ctx->def_penv,
                                          ctx->def_env);
            ctx->fvars = CONS(CONS(template, wrapped), ctx->fvars);
        } else {
            wrapped = CDR(wrapped);
        }
        ret.msg = MSG_REPLACE;
        ret.u.obj = wrapped;
        return ret;
    } else {
        ret.msg = MSG_REPLACE;
        ret.u.obj = template;
        return ret;
    }
    return ret;
#undef RECURSE
}

static transcribe_ret
transcribe_reppat(transcription_context *ctx, ScmObj template, ScmObj sub,
                  scm_int_t level)
{
    ScmObj form;
    transcribe_ret ret;
    ScmQueue q;

    SCM_ASSERT(SUBPATP(template) && REPPATP(template));
    SCM_ASSERT(level <= ctx->index_buf_size);

    DBG_PRINT((DBG_TRANSCRIPTOR | DBG_FUNCALL, "transcribe_reppat\n"));
    if (level == ctx->index_buf_size) {
        DBG_PRINT((DBG_TRANSCRIPTOR, "growing buffer from size ~MD\n",
                   ctx->index_buf_size));
        ctx->index_buf_size = level * 2;
        if (ctx->indices == ctx->index_buf) {
            SCM_ASSERT(level == DEFAULT_INDEX_BUF_SIZE);
            ctx->indices = scm_malloc(ctx->index_buf_size
                                      * sizeof(scm_int_t));
            memcpy(ctx->indices, ctx->index_buf,
                   level * sizeof(scm_int_t));
        } else {
            ctx->indices = scm_realloc(ctx->indices,
                                       ctx->index_buf_size
                                       * sizeof(scm_int_t));
        }
    }

    form = REPPAT_PAT(template);
    ret.u.obj = SCM_NULL;
    SCM_QUEUE_POINT_TO(q, ret.u.obj);
    ctx->indices[level] = 0;

    for (;;) {
        transcribe_ret subret;
        subret = transcribe_rec(ctx, form, sub, level + 1);
        if (subret.msg == MSG_PVAR_EXHAUSTED) {
            if (subret.u.exhausted_level == level)
                break;
            SCM_ASSERT(subret.u.exhausted_level < level);
            return subret;
        }
        SCM_ASSERT(subret.msg == MSG_REPLACE);
        SCM_QUEUE_ADD(q, subret.u.obj);
        ++ctx->indices[level];
    }
    ret.msg = MSG_SPLICE;
    return ret;
}

/* ==============================
 * Syntax unwrapping
 * ==============================*/

static ScmObj unwrap_farsymbol(ScmObj obj);
static void unwrap_dispatch(ScmObj obj);
static void unwrap_listx(ScmObj ls);
static void unwrap_vectorx(ScmObj obj);

/* Like FOR_EACH(), but leaves the argument at the last cons cell. */
#define UPTO_LAST_PAIR(ls) while (CONSP(CDR(ls)) && ((ls) = CDR(ls), 1))

static ScmObj
unwrap_farsymbol(ScmObj obj)
{
    SCM_ASSERT(FARSYMBOLP(obj));
    do
        obj = SCM_FARSYMBOL_SYM(obj);
    while (FARSYMBOLP(obj));
    return obj;
}

static void
unwrap_dispatch(ScmObj obj)
{
    if (CONSP(obj))
        unwrap_listx(obj);
    else if (VECTORP(obj))
        unwrap_vectorx(obj);
}

static void
unwrap_listx(ScmObj ls)
{
    do {
        if (FARSYMBOLP(CAR(ls)))
            SET_CAR(ls, unwrap_farsymbol(CAR(ls)));
        else
            unwrap_dispatch(CAR(ls));
    } UPTO_LAST_PAIR (ls);
    SET_CDR(ls, scm_unwrap_syntaxx(CDR(ls)));
}

static void
unwrap_vectorx(ScmObj obj)
{
    ScmObj *vec;
    scm_int_t i;

    i = SCM_VECTOR_LEN(obj);
    vec = SCM_VECTOR_VEC(obj);
    while (i--) {
        if (FARSYMBOLP(vec[i]))
            vec[i] = unwrap_farsymbol(vec[i]);
        else
            unwrap_dispatch(vec[i]);
    }
}

SCM_EXPORT ScmObj
scm_unwrap_syntaxx(ScmObj arg)
{
    DBG_PRINT((DBG_UNWRAP, "unwrap-syntax!: ~s\n", arg));
    if (FARSYMBOLP(arg))
        return unwrap_farsymbol(arg);
    unwrap_dispatch(arg);
    return arg;
}

SCM_EXPORT ScmObj
scm_unwrap_keyword(ScmObj obj)
{
    DBG_PRINT((DBG_UNWRAP, "unwrap-keyword: ~s\n", obj));
    return FARSYMBOLP(obj) ? unwrap_farsymbol(obj) : obj;
}

#if 0
/* Alternative implementation. */
SCM_EXPORT ScmObj
scm_unwrap_syntaxx(ScmObj arg)
{
    if (CONSP(arg)) {
        ScmObj ls = arg;
        do {
            SET_CAR(ls, scm_unwrap_syntaxx(CAR(ls)));
            tail = ls;
        } UPTO_LAST_PAIR(ls);
        SET_CDR(ls, scm_unwrap_syntaxx(CDR(ls)));
        return arg;
    }

    if (FARSYMBOLP(arg))
        return unwrap_farsymbol(arg);

    if (VECTORP(arg)) {
        scm_int_t i;
        ScmObj *vec;
        i = SCM_VECTOR_LEN(arg);
        vec = SCM_VECTOR_VEC(arg);
        while (i--)
            vec[i] = scm_unwrap_syntaxx(vec[i]);
        return arg;
    }
    return arg;
}
#endif /* 0 */
#undef UPTO_LAST_PAIR

/* ==============================
 * Auxiliary Utilities
 * ==============================*/

/* TODO: move to somewhere appropriate. */
SCM_EXPORT ScmObj
scm_p_reversex(ScmObj in)
{
    ScmObj out, next;
    DECLARE_FUNCTION("reverse!", procedure_fixed_1);

    out = SCM_NULL;
    while (CONSP(in)) {
        next = CDR(in);
        SET_CDR(in, out);
        out = in;
        in = next;
    }
    SCM_ENSURE_PROPER_LIST_TERMINATION(in, out);
    return out;
}

static scm_int_t
list_find_index(ScmObj x, ScmObj ls)
{
    ScmObj kar;
    scm_int_t index;

    index = 0;
    FOR_EACH (kar, ls) {
        if (EQ(x, kar))
            return index;
        ++index;
    }
    return -1;
}

