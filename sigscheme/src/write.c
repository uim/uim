/*===========================================================================
 *  Filename : write.c
 *  About    : Object writer
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

/* TODO: make format.c independent */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "my-stdint.h"
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/
enum OutputType {
    AS_WRITE,   /* string is enclosed by ", char is written using #\ notation */
    AS_DISPLAY, /* string and char is written as-is */
    UNKNOWN
};

#if SCM_USE_SRFI38
typedef size_t hashval_t;
typedef struct {
    ScmObj key;
    int datum;
} hash_entry;

typedef struct {
    size_t size;  /* capacity; MUST be a power of 2 */
    size_t used;  /* population */
    hash_entry *ents;
} hash_table;

typedef struct {
    hash_table seen; /* a table of seen objects */
    int next_index;  /* the next index to use for #N# */
} write_ss_context;
#endif /* SCM_USE_SRFI38 */

/*=======================================
  File Local Macro Declarations
=======================================*/
#if SCM_USE_SRFI38
#define INTERESTINGP(obj)                                                    \
    (CONSP(obj)                                                              \
     || (STRINGP(obj) && SCM_STRING_LEN(obj))                                \
     || CLOSUREP(obj)                                                        \
     || VECTORP(obj)                                                         \
     || VALUEPACKETP(obj)                                                    \
     || ERROBJP(obj))
#define OCCUPIED(ent)      (!EQ((ent)->key, SCM_INVALID))
#define HASH_EMPTY(table)  (!(table).used)
#define DEFINING_DATUM     (-1)
#define NONDEFINING_DATUM  0
#define GET_DEFINDEX(x)    ((unsigned)(x) >> 1)
#define HASH_INSERT    1 /* insert key if it's not registered yet */
#define HASH_FIND      0
#endif /* SCM_USE_SRFI38 */

/*=======================================
  Variable Declarations
=======================================*/
SCM_DEFINE_EXPORTED_VARS(write);

#if SCM_USE_SRFI38
SCM_GLOBAL_VARS_BEGIN(static_write);
#define static
/* misc info in priting shared structures */
static write_ss_context *l_write_ss_ctx;
#undef static
SCM_GLOBAL_VARS_END(static_write);
#define l_write_ss_ctx SCM_GLOBAL_VAR(static_write, l_write_ss_ctx)
SCM_DEFINE_STATIC_VARS(static_write);
#endif /* SCM_USE_SRFI38 */

/*=======================================
  File Local Function Declarations
=======================================*/
static void write_internal(ScmObj port, ScmObj obj, enum OutputType otype);
static void write_obj(ScmObj port, ScmObj obj, enum OutputType otype);
static void write_char(ScmObj port, ScmObj obj, enum OutputType otype);
static void write_string(ScmObj port, ScmObj obj, enum OutputType otype);
static void write_list(ScmObj port, ScmObj lst, enum OutputType otype);
static void write_vector(ScmObj port, ScmObj vec, enum OutputType otype);
static void write_port(ScmObj port, ScmObj obj, enum OutputType otype);
static void write_constant(ScmObj port, ScmObj obj, enum  OutputType otype);
static void write_errobj(ScmObj port, ScmObj obj, enum  OutputType otype);

#if SCM_USE_HYGIENIC_MACRO
static void write_farsymbol(ScmObj port, ScmObj obj, enum OutputType otype);
#endif

#if SCM_USE_SRFI38
static void hash_grow(hash_table *tab);
static hash_entry *hash_lookup(hash_table *tab, ScmObj key, int datum, int flag);
static void write_ss_scan(ScmObj obj, write_ss_context *ctx);
static int  get_shared_index(ScmObj obj);
static void write_ss_internal(ScmObj port, ScmObj obj, enum OutputType otype);
#endif /* SCM_USE_SRFI38 */

/*=======================================
   Function Implementations
=======================================*/
SCM_EXPORT void
scm_init_writer(void)
{
    SCM_GLOBAL_VARS_INIT(write);
#if SCM_USE_SRFI38
    SCM_GLOBAL_VARS_INIT(static_write);
#endif

    /* To allow re-initialization of the interpreter, this variable must be
     * re-initialized by assignment. Initialized .data section does not work
     * for such situation.  -- YamaKen 2006-03-31 */
    scm_write_ss_func = scm_write;
}

SCM_EXPORT void
scm_write(ScmObj port, ScmObj obj)
{
    write_internal(port, obj, AS_WRITE);
}

SCM_EXPORT void
scm_display(ScmObj port, ScmObj obj)
{
    write_internal(port, obj, AS_DISPLAY);
}

static void
write_internal(ScmObj port, ScmObj obj, enum OutputType otype)
{
    DECLARE_INTERNAL_FUNCTION("write");

    ENSURE_PORT(port);
    SCM_ENSURE_LIVE_PORT(port);
    if (!(SCM_PORT_FLAG(port) & SCM_PORTFLAG_OUTPUT))
        ERR("output port is required");

    write_obj(port, obj, otype);
    scm_port_flush(port);
}

static void
write_obj(ScmObj port, ScmObj obj, enum OutputType otype)
{
    ScmObj sym;

#if SCM_USE_SRFI38
    if (INTERESTINGP(obj)) {
        int index = get_shared_index(obj);
        if (index > 0) {
            /* defined datum */
            scm_format(port, SCM_FMT_RAW_C, "#~D#", index);
            return;
        }
        if (index < 0) {
            /* defining datum, with the new index negated */
            scm_format(port, SCM_FMT_RAW_C, "#~D=", -index);
            /* Print it; the next time it'll be defined. */
        }
    }
#endif
    switch (SCM_TYPE(obj)) {
    case ScmInt:
        scm_format(port, SCM_FMT_RAW_C, "~MD", SCM_INT_VALUE(obj));
        break;
    case ScmCons:
        if (ERROBJP(obj))
            write_errobj(port, obj, otype);
        else
            write_list(port, obj, otype);
        break;
    case ScmSymbol:
        scm_port_puts(port, SCM_SYMBOL_NAME(obj));
        break;
    case ScmChar:
        write_char(port, obj, otype);
        break;
    case ScmString:
        write_string(port, obj, otype);
        break;
    case ScmFunc:
        scm_port_puts(port, (SCM_SYNTAXP(obj)) ? "#<syntax " : "#<subr ");
        sym = scm_symbol_bound_to(obj);
        if (NFALSEP(sym))
            scm_display(port, sym);
        else
            scm_format(port, SCM_FMT_RAW_C, "~P", (void *)obj);
        scm_port_put_char(port, '>');
        break;
#if SCM_USE_HYGIENIC_MACRO
    case ScmMacro:
        scm_port_puts(port, "#<macro ");
        write_obj(port, SCM_HMACRO_RULES(obj), otype);
        scm_port_puts(port, ">");
        break;
    case ScmFarsymbol:
        write_farsymbol(port, obj, otype);
        break;
    case ScmSubpat:
        if (SCM_SUBPAT_PVARP(obj)) {
#if SCM_DEBUG_MACRO
            scm_port_puts(port, "#<pvar ");
            write_obj(port, SCM_SUBPAT_OBJ(obj), otype);
            scm_format(port, SCM_FMT_RAW_C, " ~MD>",
                       SCM_SUBPAT_PVAR_INDEX(obj));
#else  /* not SCM_DEBUG_MACRO */
            write_obj(port, SCM_SUBPAT_OBJ(obj), otype);
#endif /* not SCM_DEBUG_MACRO */
        } else {
            SCM_ASSERT(SCM_SUBPAT_REPPATP(obj));
            write_obj(port, SCM_SUBPAT_REPPAT_PAT(obj), otype);
#if SCM_DEBUG_MACRO
            scm_format(port, SCM_FMT_RAW_C, " ..[~MD]..",
                       SCM_SUBPAT_REPPAT_PVCOUNT(obj));
#else
            scm_port_puts(port, " ...");
#endif
        }
        break;
#endif /* SCM_USE_HYGIENIC_MACRO */
    case ScmClosure:
        scm_port_puts(port, "#<closure ");
        write_obj(port, SCM_CLOSURE_EXP(obj), otype);
        scm_port_put_char(port, '>');
        break;
    case ScmVector:
        write_vector(port, obj, otype);
        break;
    case ScmPort:
        write_port(port, obj, otype);
        break;
    case ScmContinuation:
        scm_format(port, SCM_FMT_RAW_C, "#<continuation ~P>", (void *)obj);
        break;
    case ScmValuePacket:
        scm_port_puts(port, "#<values ");
        if (NULLP(SCM_VALUEPACKET_VALUES(obj)))
            scm_port_puts(port, "()");
        else
            write_list(port, SCM_VALUEPACKET_VALUES(obj), otype);
#if SCM_USE_VALUECONS
#if SCM_USE_STORAGE_FATTY
        /* SCM_VALUEPACKET_VALUES() changes the type destructively */
        SCM_ENTYPE(obj, ScmValuePacket);
#else /* SCM_USE_STORAGE_FATTY */
#error "valuecons is not supported on this storage implementation"
#endif /* SCM_USE_STORAGE_FATTY */
#endif /* SCM_USE_VALUECONS */
        scm_port_put_char(port, '>');
        break;
    case ScmConstant:
        write_constant(port, obj, otype);
        break;
    case ScmCPointer:
        scm_format(port, SCM_FMT_RAW_C,
                   "#<c_pointer ~P>", SCM_C_POINTER_VALUE(obj));
        break;
    case ScmCFuncPointer:
        scm_format(port, SCM_FMT_RAW_C,
                   "#<c_func_pointer ~P>",
                   (void *)(uintptr_t)SCM_C_FUNCPOINTER_VALUE(obj));
        break;
    default:
        SCM_ASSERT(scm_false);
    }
}

static void
write_char(ScmObj port, ScmObj obj, enum OutputType otype)
{
    const ScmSpecialCharInfo *info;
    scm_ichar_t c;

    c = SCM_CHAR_VALUE(obj);
    switch (otype) {
    case AS_WRITE:
        scm_port_puts(port, "#\\");
        /* special chars */
        for (info = scm_special_char_table; info->esc_seq; info++) {
            if (c == info->code) {
                scm_port_puts(port, info->lex_rep);
                return;
            }
        }

        /* other control chars are printed in hexadecimal form */
        if (ICHAR_CONTROLP(c)) {
            scm_format(port, SCM_FMT_RAW_C, "x~02MX", (scm_int_t)c);
            return;
        }
        /* FALLTHROUGH */
    case AS_DISPLAY:
        scm_port_put_char(port, c);
        break;

    default:
        SCM_ASSERT(scm_false);
    }
}

static void
write_string(ScmObj port, ScmObj obj, enum OutputType otype)
{
#if SCM_USE_MULTIBYTE_CHAR
    ScmCharCodec *codec;
    ScmMultibyteString mbs;
    size_t len;
#else
    scm_int_t i, len;
#endif
    const ScmSpecialCharInfo *info;
    const char *str;
    scm_ichar_t c;
    DECLARE_INTERNAL_FUNCTION("write");

    str = SCM_STRING_STR(obj);

    switch (otype) {
    case AS_WRITE:
        scm_port_put_char(port, '\"'); /* opening doublequote */
#if SCM_USE_MULTIBYTE_CHAR
        if (scm_current_char_codec != scm_port_codec(port)) {
            /* Since the str does not have its encoding information, here
             * assumes that scm_current_char_codec is that. And then SigScheme
             * does not have an encoding conversion mechanism, puts it
             * as-is. */
            scm_port_puts(port, str);
        } else {
            len = strlen(str);
            codec = scm_port_codec(port);
            SCM_MBS_INIT2(mbs, str, len);
            while (SCM_MBS_GET_SIZE(mbs)) {
                c = SCM_CHARCODEC_READ_CHAR(codec, mbs);
#else /* SCM_USE_MULTIBYTE_CHAR */
            len = SCM_STRING_LEN(obj);
            for (i = 0; i < len; i++) {
                c = str[i];
#endif /* SCM_USE_MULTIBYTE_CHAR */
                for (info = scm_special_char_table; info->esc_seq; info++) {
                    if (c == info->code) {
                        scm_port_puts(port, info->esc_seq);
                        goto continue2;
                    }
                }
                scm_port_put_char(port, c);
            continue2:
                ;
            }
        }
        scm_port_put_char(port, '\"'); /* closing doublequote */
        break;

    case AS_DISPLAY:
        scm_port_puts(port, str);
        break;

    default:
        SCM_ASSERT(scm_false);
    }
}

static void
write_list(ScmObj port, ScmObj lst, enum OutputType otype)
{
    ScmObj car;
#if SCM_USE_SRFI38
    size_t necessary_close_parens;
    int index;
#endif
    DECLARE_INTERNAL_FUNCTION("write");

#if SCM_USE_SRFI38
    necessary_close_parens = 1;
  cheap_recursion:
#endif

    if (NULLP(lst)) {
        scm_port_puts(port, "()");
        return;
    }

    scm_port_put_char(port, '(');

    FOR_EACH (car, lst) {
        write_obj(port, car, otype);
        if (!CONSP(lst))
            break;
        scm_port_put_char(port, ' ');

#if SCM_USE_SRFI38
        /* See if the next pair is shared.  Note that the case
         * where the first pair is shared is handled in
         * write_obj(). */
        index = get_shared_index(lst);
        if (index > 0) {
            /* defined datum */
            scm_format(port, SCM_FMT_RAW_C, ". #~D#", index);
            goto close_parens_and_return;
        }
        if (index < 0) {
            /* defining datum, with the new index negated */
            scm_format(port, SCM_FMT_RAW_C, ". #~D=", -index);
            necessary_close_parens++;
            goto cheap_recursion;
        }
#endif
    }

    /* last item */
    if (!NULLP(lst)) {
        scm_port_puts(port, " . ");
        /* Callee takes care of shared data. */
        write_obj(port, lst, otype);
    }

#if SCM_USE_SRFI38
  close_parens_and_return:
    while (necessary_close_parens--)
#endif
        scm_port_put_char(port, ')');
}

static void
write_vector(ScmObj port, ScmObj vec, enum OutputType otype)
{
    ScmObj *v;
    scm_int_t len, i;

    scm_port_puts(port, "#(");

    v = SCM_VECTOR_VEC(vec);
    len = SCM_VECTOR_LEN(vec);
    for (i = 0; i < len; i++) {
        if (i)
            scm_port_put_char(port, ' ');
        write_obj(port, v[i], otype);
    }

    scm_port_put_char(port, ')');
}

static void
write_port(ScmObj port, ScmObj obj, enum OutputType otype)
{
    char *info;

    scm_port_puts(port, "#<");

    /* input or output */
    /* print "i", "o" or "io" if bidirectional port */
    if (SCM_PORT_FLAG(obj) & SCM_PORTFLAG_INPUT)
        scm_port_put_char(port, 'i');
    if (SCM_PORT_FLAG(obj) & SCM_PORTFLAG_OUTPUT)
        scm_port_put_char(port, 'o');

    scm_port_puts(port, "port");

    /* file or string */
    info = scm_port_inspect(obj);
    if (*info) {
        scm_port_put_char(port, ' ');
        scm_port_puts(port, info);
    }
    free(info);

    scm_port_put_char(port, '>');
}

static void
write_constant(ScmObj port, ScmObj obj, enum  OutputType otype)
{
    const char *str;

    if (EQ(obj, SCM_NULL))
        str = "()";
    else if (EQ(obj, SCM_TRUE))
        str = "#t";
    else if (EQ(obj, SCM_FALSE))
        str = "#f";
    else if (EQ(obj, SCM_EOF))
#if SCM_COMPAT_SIOD_BUGS
        str = "(eof)";
#else
        str = "#<eof>";
#endif
    else if (EQ(obj, SCM_UNBOUND))
        str = "#<unbound>";
    else if (EQ(obj, SCM_UNDEF))
        str = "#<undef>";
    else
        SCM_ASSERT(scm_false);

    scm_port_puts(port, str);
}

static void
write_errobj(ScmObj port, ScmObj obj, enum  OutputType otype)
{
    ScmObj err_obj_tag, reason, objs, trace_stack, elm;
    DECLARE_INTERNAL_FUNCTION("write");

    err_obj_tag = MUST_POP_ARG(obj);
    reason      = MUST_POP_ARG(obj);
    objs        = MUST_POP_ARG(obj);
    trace_stack = MUST_POP_ARG(obj);
    ASSERT_NO_MORE_ARG(obj);

    switch (otype) {
    case AS_WRITE:
        scm_port_puts(port, "#<error ");
        scm_write(port, reason);
        break;

    case AS_DISPLAY:
        scm_display(port, reason);
        if (CONSP(objs))
            scm_port_put_char(port, ':');
        break;

    default:
        SCM_ASSERT(scm_false);
    }

    FOR_EACH (elm, objs) {
        scm_port_put_char(port, ' ');
        scm_write(port, elm);
    }

    if (otype == AS_WRITE)
        scm_port_put_char(port, '>');
}

#if SCM_USE_HYGIENIC_MACRO
static void
write_farsymbol(ScmObj port, ScmObj obj, enum  OutputType otype)
{
    /* Assumes that ScmPackedEnv is an integer. */
    scm_port_puts(port, "#<farsym");
    for (; SCM_FARSYMBOLP(obj); obj = SCM_FARSYMBOL_SYM(obj))
        scm_format(port, SCM_FMT_RAW_C, " ~MD ", SCM_FARSYMBOL_ENV(obj));
    scm_display(port, obj); /* Name. */
    scm_port_puts(port, ">");
}
#endif /* SCM_USE_HYGIENIC_MACRO */

#if SCM_USE_SRFI38
static void
hash_grow(hash_table *tab)
{
    size_t old_size, new_size, i;
    hash_entry *old_ents;

    old_size = tab->size;
    new_size = old_size * 2;
    old_ents = tab->ents;

    tab->ents = scm_calloc(new_size, sizeof(hash_entry));
    tab->size = new_size;
    tab->used = 0;

    for (i = 0; i < old_size; i++)
        hash_lookup(tab, old_ents[i].key, old_ents[i].datum, HASH_INSERT);

    free(old_ents);
}

/**
 * @return A pointer to the entry, or NULL if not found.
 */
static hash_entry *
hash_lookup(hash_table *tab, ScmObj key, int datum, int flag)
{
    size_t i;
    hashval_t hashval;
    hash_entry *ent;

    /* If we have > 32 bits, we'll discard some of them.  The lower
     * bits are zeroed for alignment or used for tag bits, and in the
     * latter case, the tag can only take 3 values: pair, string, or
     * vector.  We'll drop these bits.  KEYs are expected to be
     * pointers into the heap, so their higher bis are probably
     * uniform.  I haven't confirmed either's validity, though. */
    hashval = (hashval_t)key;
    if (sizeof(hashval) > 4) {
        hashval /= sizeof(ScmCell);
        hashval &= 0xffffffff;
    }

    hashval *= 2654435761UL; /* golden ratio hash */

    /* We probe linearly, since a) speed isn't a primary concern for
     * SigScheme, and b) having a table of primes only for this
     * purpose is probably just a waste. */
    for (i = 0; i < tab->size; i++) {
        ent = &(tab->ents)[(hashval + i) & (tab->size - 1)];
        if (!OCCUPIED(ent)) {
            if (flag & HASH_INSERT) {
                ent->key = key;
                ent->datum = datum;
                tab->used++;

                /* used > size * 2/3 --> overpopulated */
                if (tab->used * 3 > tab->size * 2)
                    hash_grow(tab);
            }
            return NULL;
        }
        if (EQ(ent->key, key))
            return ent;
    }

    /* A linear probe should always find a slot. */
    SCM_ASSERT(scm_false);
}

/**
 * Find out what non-atomic objects a structure shares within itself.
 * @param obj The object in question, or a part of it.
 * @param ctx Where to put the scan results.
 */
static void
write_ss_scan(ScmObj obj, write_ss_context *ctx)
{
    scm_int_t i, len;
    hash_entry *ent;
    ScmObj err_obj_tag, reason, objs, trace_stack;
    DECLARE_INTERNAL_FUNCTION("write-with-shared-structure");

    if (ERROBJP(obj)) {
        err_obj_tag = MUST_POP_ARG(obj);
        reason      = MUST_POP_ARG(obj);
        objs        = MUST_POP_ARG(obj);
        trace_stack = MUST_POP_ARG(obj);
        ASSERT_NO_MORE_ARG(obj);

        write_ss_scan(reason, ctx);
        write_ss_scan(objs, ctx);
        return;
    }

    /* (for-each mark-as-seen-or-return-if-familiar obj) */
    for (; CONSP(obj); obj = CDR(obj)) {
        ent = hash_lookup(&ctx->seen, obj, NONDEFINING_DATUM, HASH_INSERT);
        if (ent) {
            ent->datum = DEFINING_DATUM;
            return;
        }
        write_ss_scan(CAR(obj), ctx);
    }

    if (INTERESTINGP(obj)) {
        ent = hash_lookup(&ctx->seen, obj, NONDEFINING_DATUM, HASH_INSERT);
        if (ent) {
            ent->datum = DEFINING_DATUM;
            return;
        }
        switch (SCM_TYPE(obj)) {
        case ScmClosure:
            /* We don't need to track env because it's not printed anyway. */
            write_ss_scan(SCM_CLOSURE_EXP(obj), ctx);
            break;

        case ScmValuePacket:
#if SCM_USE_VALUECONS
#if SCM_USE_STORAGE_FATTY
            if (!SCM_NULLVALUESP(obj)) {
                write_ss_scan(CDR(SCM_VALUEPACKET_VALUES(obj)), ctx);
                /* SCM_VALUEPACKET_VALUES() changes the type destructively */
                SCM_ENTYPE(obj, ScmValuePacket);
            }
#else /* SCM_USE_STORAGE_FATTY */
#error "valuecons is not supported on this storage implementation"
#endif /* SCM_USE_STORAGE_FATTY */
#else /* SCM_USE_VALUECONS */
            write_ss_scan(SCM_VALUEPACKET_VALUES(obj), ctx);
#endif /* SCM_USE_VALUECONS */
            break;

        case ScmVector:
            for (i = 0, len = SCM_VECTOR_LEN(obj); i < len; i++)
                write_ss_scan(SCM_VECTOR_VEC(obj)[i], ctx);
            break;

        default:
            break;
        }
    }
}

/**
 * @return The index for obj, if it's a defined datum.  If it's a
 *         defining datum, allocate an index for it and return the
 *         *additive inverse* of the index.  If obj is nondefining,
 *         return zero.
 */
static int
get_shared_index(ScmObj obj)
{
    hash_entry *ent;

    if (l_write_ss_ctx) {
        ent = hash_lookup(&l_write_ss_ctx->seen, obj, 0, HASH_FIND);

        if (ent) {
            if (ent->datum == DEFINING_DATUM) {
                ent->datum = l_write_ss_ctx->next_index++;
                return - (ent->datum);
            }
            return ent->datum;
        }
    }
    return 0;
}

static void
write_ss_internal(ScmObj port, ScmObj obj, enum OutputType otype)
{
    write_ss_context ctx = {{0}};
    unsigned int i;

    ctx.next_index = 1;
    ctx.seen.size = 1 << 8; /* arbitrary initial size */
    ctx.seen.ents = scm_calloc(ctx.seen.size, sizeof(hash_entry));
    for (i = 0; i < ctx.seen.size; i++) {
        ctx.seen.ents[i].key = SCM_INVALID;
    }

    write_ss_scan(obj, &ctx);

    /* If no structure is shared, we do a normal write. */
    if (!HASH_EMPTY(ctx.seen))
        l_write_ss_ctx = &ctx;

    write_internal(port, obj, otype);

    l_write_ss_ctx = NULL;
    free(ctx.seen.ents);
}

/* write with shared structure */
SCM_EXPORT void
scm_write_ss(ScmObj port, ScmObj obj)
{
    write_ss_internal(port, obj, AS_WRITE);
}

SCM_EXPORT void
scm_display_errobj_ss(ScmObj port, ScmObj errobj)
{
    write_ss_internal(port, errobj, AS_DISPLAY);
}
#endif /* SCM_USE_SRFI38 */

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.3 Output
===========================================================================*/
SCM_EXPORT ScmObj
scm_p_write(ScmObj obj, ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("write", procedure_variadic_1);

    port = scm_prepare_port(args, scm_out);
    scm_write(port, obj);
    return SCM_UNDEF;
}

SCM_EXPORT ScmObj
scm_p_display(ScmObj obj, ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("display", procedure_variadic_1);

    port = scm_prepare_port(args, scm_out);
    scm_display(port, obj);
    return SCM_UNDEF;
}
