/*===========================================================================
 *  FileName : debug.c
 *  About    : Functions for debugging
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
/*=======================================
  System Include
=======================================*/
#include <stdio.h>
#include <stdarg.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/
#if 1
/* FIXME: replace with C99-independent stdint.h */
typedef unsigned long uintptr_t;
#endif

enum OutputType {
    AS_WRITE,   /* string is enclosed by ", char is written using #\ notation. */
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
#define INTERESTINGP(obj)  \
    (CONSP(obj) \
     || (STRINGP(obj) && SCM_STRING_LEN(obj)) \
     || CLOSUREP(obj) \
     || VECTORP(obj) \
     || VALUEPACKETP(obj))
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
void (*Scm_writess_func)(ScmObj port, ScmObj obj) = &SigScm_WriteToPort;

static int debug_mask;
#if SCM_USE_SRFI38
static write_ss_context *write_ss_ctx; /* misc info in priting shared structures */
#endif

/*=======================================
  File Local Function Declarations
=======================================*/
static void print_ScmObj_internal(ScmObj port, ScmObj obj, enum OutputType otype);
static void print_char(ScmObj port, ScmObj obj, enum OutputType otype);
static void print_string(ScmObj port, ScmObj obj, enum OutputType otype);
static void print_list(ScmObj port, ScmObj lst, enum OutputType otype);
static void print_vector(ScmObj port, ScmObj vec, enum OutputType otype);
static void print_port(ScmObj port, ScmObj obj, enum OutputType otype);
static void print_constant(ScmObj port, ScmObj obj, enum  OutputType otype);
static void print_errobj(ScmObj port, ScmObj obj, enum  OutputType otype);

#if SCM_USE_SRFI38
static void hash_grow(hash_table *tab);
static hash_entry *hash_lookup(hash_table *tab, ScmObj key, int datum, int flag);
static void write_ss_scan(ScmObj obj, write_ss_context *ctx);
static int  get_shared_index(ScmObj obj);
#endif /* SCM_USE_SRFI38 */

/*=======================================
   Function Implementations
=======================================*/
int SigScm_DebugCategories(void)
{
    return debug_mask;
}

void SigScm_SetDebugCategories(int categories)
{
    debug_mask = categories;
}

int SigScm_PredefinedDebugCategories(void)
{
#if SCM_DEBUG
    return (SCM_DBG_DEVEL | SCM_DBG_COMPAT | SCM_DBG_OTHER
#if SCM_DEBUG_PARSER
            | SCM_DBG_PARSER
#endif
#if SCM_DEBUG_GC
            | SCM_DBG_GC
#endif
#if SCM_DEBUG_ENCODING
            | SCM_DBG_ENCODING
#endif
            );
#else /* SCM_DEBUG */
    return SCM_DBG_NONE;
#endif /* SCM_DEBUG */
}

void SigScm_CategorizedDebug(int category, const char *msg, ...)
{
    va_list va;

    va_start(va, msg);
    if (debug_mask & category) {
        SigScm_VErrorPrintf(msg, va);
        SigScm_ErrorNewline();
    }
    va_end(va);
}

void SigScm_Debug(const char *msg, ...)
{
    va_list va;

    va_start(va, msg);
    if (debug_mask & SCM_DBG_DEVEL) {
        SigScm_VErrorPrintf(msg, va);
        SigScm_ErrorNewline();
    }
    va_end(va);
}

void SigScm_Display(ScmObj obj)
{
    SigScm_DisplayToPort(scm_current_output_port, obj);
}

void SigScm_WriteToPort(ScmObj port, ScmObj obj)
{
    DECLARE_INTERNAL_FUNCTION("SigScm_WriteToPort");

    ASSERT_PORTP(port);
    SCM_ASSERT_LIVE_PORT(port);
    if (!(SCM_PORT_FLAG(port) & SCM_PORTFLAG_OUTPUT))
        ERR("output port is required");

    print_ScmObj_internal(port, obj, AS_WRITE);

#if SCM_VOLATILE_OUTPUT
    SCM_PORT_FLUSH(port);
#endif /* SCM_VOLATILE_OUTPUT */
}

void SigScm_DisplayToPort(ScmObj port, ScmObj obj)
{
    DECLARE_INTERNAL_FUNCTION("SigScm_DisplayToPort");

    ASSERT_PORTP(port);
    SCM_ASSERT_LIVE_PORT(port);
    if (!(SCM_PORT_FLAG(port) & SCM_PORTFLAG_OUTPUT))
        ERR("output port is required");

    print_ScmObj_internal(port, obj, AS_DISPLAY);

#if SCM_VOLATILE_OUTPUT
    SCM_PORT_FLUSH(port);
#endif /* SCM_VOLATILE_OUTPUT */
}

static void print_ScmObj_internal(ScmObj port, ScmObj obj, enum OutputType otype)
{
    ScmObj sym;

#if SCM_USE_SRFI38
    if (INTERESTINGP(obj)) {
        int index = get_shared_index(obj);
        if (index > 0) {
            /* defined datum */
            SigScm_PortPrintf(port, "#%d#", index);
            return;
        }
        if (index < 0) {
            /* defining datum, with the new index negated */
            SigScm_PortPrintf(port, "#%d=", -index);
            /* Print it; the next time it'll be defined. */
        }
    }
#endif
    switch (SCM_TYPE(obj)) {
    case ScmInt:
        SigScm_PortPrintf(port, "%d", SCM_INT_VALUE(obj));
        break;
    case ScmCons:
        if (ERROBJP(obj))
            print_errobj(port, obj, otype);
        else
            print_list(port, obj, otype);
        break;
    case ScmSymbol:
        SCM_PORT_PUTS(port, SCM_SYMBOL_NAME(obj));
        break;
    case ScmChar:
        print_char(port, obj, otype);
        break;
    case ScmString:
        print_string(port, obj, otype);
        break;
    case ScmFunc:
        SCM_PORT_PUTS(port, (SCM_SYNTAXP(obj)) ? "#<syntax " : "#<subr ");
        sym = Scm_SymbolBoundTo(obj);
        if (NFALSEP(sym))
            SigScm_DisplayToPort(port, sym);
        else
            SigScm_PortPrintf(port, "%p", (void *)obj);
        SCM_PORT_PUT_CHAR(port, '>');
        break;
    case ScmClosure:
        SCM_PORT_PUTS(port, "#<closure ");
        print_ScmObj_internal(port, SCM_CLOSURE_EXP(obj), otype);
        SCM_PORT_PUT_CHAR(port, '>');
        break;
    case ScmVector:
        print_vector(port, obj, otype);
        break;
    case ScmPort:
        print_port(port, obj, otype);
        break;
    case ScmContinuation:
        SCM_PORT_PUTS(port, "#<subr continuation>");
        break;
    case ScmValuePacket:
        SCM_PORT_PUTS(port, "#<values ");
        if (NULLP(SCM_VALUEPACKET_VALUES(obj)))
            SCM_PORT_PUTS(port, "()");
        else
            print_list(port, SCM_VALUEPACKET_VALUES(obj), otype);
#if SCM_USE_VALUECONS
        /* SCM_VALUEPACKET_VALUES() changes the type destructively */
        SCM_ENTYPE_VALUEPACKET(obj);
#endif
        SCM_PORT_PUT_CHAR(port, '>');
        break;
    case ScmConstant:
        print_constant(port, obj, otype);
        break;
    case ScmFreeCell:
        ERR("You cannot print ScmFreeCell, may be GC bug.");
        break;
    case ScmCPointer:
        SigScm_PortPrintf(port, "#<c_pointer %p>", SCM_C_POINTER_VALUE(obj));
        break;
    case ScmCFuncPointer:
        SigScm_PortPrintf(port, "#<c_func_pointer %p>",
                          (void *)(uintptr_t)SCM_C_FUNCPOINTER_VALUE(obj));
        break;
    }
}

static void print_char(ScmObj port, ScmObj obj, enum OutputType otype)
{
    const ScmSpecialCharInfo *info;
    int c;

    c = SCM_CHAR_VALUE(obj);
    switch (otype) {
    case AS_WRITE:
        SCM_PORT_PUTS(port, "#\\");
        /* special chars */
        for (info = Scm_special_char_table; info->esc_seq; info++) {
            if (c == info->code) {
                SCM_PORT_PUTS(port, info->lex_rep);
                return;
            }
        }

        /* other control chars are printed in hexadecimal form */ 
        if (isascii(c) && iscntrl(c)) {
            SigScm_PortPrintf(port, "x%02x", c);
            return;
        }
        /* FALLTHROUGH */
    case AS_DISPLAY:
        SCM_PORT_PUT_CHAR(port, c);
        break;

    default:
        ERR("print_char: unknown output type");
        break;
    }
}

/* FIXME: support multibyte char properly */
static void print_string(ScmObj port, ScmObj obj, enum OutputType otype)
{
    const ScmSpecialCharInfo *info;
    const char *str;
    int len, c, i;

    str = SCM_STRING_STR(obj);
    len = strlen(str);

    switch (otype) {
    case AS_WRITE:
        SCM_PORT_PUT_CHAR(port, '\"'); /* first doublequote */
        for (i = 0; i < len; i++) {
            c = str[i];
            for (info = Scm_special_char_table; info->esc_seq; info++) {
                if (c == info->code) {
                    SCM_PORT_PUTS(port, info->esc_seq);
                    break;
                }
            }
            if (!info->esc_seq)
                SCM_PORT_PUT_CHAR(port, str[i]);
        }
        SCM_PORT_PUT_CHAR(port, '\"'); /* last doublequote */
        break;

    case AS_DISPLAY:
        SCM_PORT_PUTS(port, str);
        break;

    default:
        ERR("print_string : unknown output type");
        break;
    }
}

static void print_list(ScmObj port, ScmObj lst, enum OutputType otype)
{
    ScmObj car;
#if SCM_USE_SRFI38
    int index, necessary_close_parens;

    necessary_close_parens = 1;
  cheap_recursion:
#endif

    if (NULLP(lst)) {
        SCM_PORT_PUTS(port, "()");
        return;
    }

    SCM_PORT_PUT_CHAR(port, '(');

    for (;;) {
        car = CAR(lst);
        print_ScmObj_internal(port, car, otype);
        lst = CDR(lst);
        if (!CONSP(lst))
            break;
        SCM_PORT_PUT_CHAR(port, ' ');

#if SCM_USE_SRFI38
        /* See if the next pair is shared.  Note that the case
         * where the first pair is shared is handled in
         * print_ScmObj_internal(). */
        index = get_shared_index(lst);
        if (index > 0) {
            /* defined datum */
            SigScm_PortPrintf(port, ". #%d#", index);
            goto close_parens_and_return;
        }
        if (index < 0) {
            /* defining datum, with the new index negated */
            SigScm_PortPrintf(port, ". #%d=", -index);
            necessary_close_parens++;
            goto cheap_recursion;
        }
#endif
    }

    /* last item */
    if (!NULLP(lst)) {
        SCM_PORT_PUTS(port, " . ");
        /* Callee takes care of shared data. */
        print_ScmObj_internal(port, lst, otype);
    }

#if SCM_USE_SRFI38
  close_parens_and_return:
    while (necessary_close_parens--)
#endif
        SCM_PORT_PUT_CHAR(port, ')');
}

static void print_vector(ScmObj port, ScmObj vec, enum OutputType otype)
{
    ScmObj *v;
    int len, i;

    SCM_PORT_PUTS(port, "#(");

    v = SCM_VECTOR_VEC(vec);
    len = SCM_VECTOR_LEN(vec);
    for (i = 0; i < len; i++) {
        if (i)
            SCM_PORT_PUT_CHAR(port, ' ');
        print_ScmObj_internal(port, v[i], otype);
    }

    SCM_PORT_PUT_CHAR(port, ')');
}

static void print_port(ScmObj port, ScmObj obj, enum OutputType otype)
{
    char *info;

    SCM_PORT_PUTS(port, "#<");

    /* input or output */
    /* print "i", "o" or "io" if bidirectional port */
    if (SCM_PORT_FLAG(obj) & SCM_PORTFLAG_INPUT)
        SCM_PORT_PUT_CHAR(port, 'i');
    if (SCM_PORT_FLAG(obj) & SCM_PORTFLAG_OUTPUT)
        SCM_PORT_PUT_CHAR(port, 'o');

    SCM_PORT_PUTS(port, "port");

    /* file or string */
    info = SCM_PORT_INSPECT(obj);
    if (*info) {
        SCM_PORT_PUT_CHAR(port, ' ');
        SCM_PORT_PUTS(port, info);
    }
    free(info);

    SCM_PORT_PUT_CHAR(port, '>');
}

static void print_constant(ScmObj port, ScmObj obj, enum  OutputType otype)
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

    SCM_PORT_PUTS(port, str);
}

static void print_errobj(ScmObj port, ScmObj obj, enum  OutputType otype)
{
    ScmObj err_obj_tag, reason, objs, trace_stack;
    DECLARE_INTERNAL_FUNCTION("print_errobj");

    err_obj_tag = MUST_POP_ARG(obj);
    reason      = MUST_POP_ARG(obj);
    objs        = MUST_POP_ARG(obj);
    trace_stack = MUST_POP_ARG(obj);
    ASSERT_NO_MORE_ARG(obj);

    switch (otype) {
    case AS_WRITE:
        SCM_PORT_PUTS(port, "#<error ");
        SigScm_WriteToPort(port, reason);
        break;

    case AS_DISPLAY:
        SigScm_DisplayToPort(port, reason);
        if (CONSP(objs))
            SCM_PORT_PUT_CHAR(port, ':');
        break;

    default:
        ERR("print_errobj: unknown output type");
        break;
    }

    for (; CONSP(objs); objs = CDR(objs)) {
        SCM_PORT_PUT_CHAR(port, ' ');
        SigScm_WriteToPort(port, CAR(objs));
    }

    if (otype == AS_WRITE)
        SCM_PORT_PUT_CHAR(port, '>');
}

#if SCM_USE_SRFI38
static void hash_grow(hash_table *tab)
{
    size_t old_size, new_size, i;
    hash_entry *old_ents;

    old_size = tab->size;
    new_size = old_size * 2;
    old_ents = tab->ents;

    tab->ents = Scm_calloc(new_size, sizeof(hash_entry));
    tab->size = new_size;
    tab->used = 0;

    for (i=0; i < old_size; i++)
        hash_lookup(tab, old_ents[i].key, old_ents[i].datum, HASH_INSERT);

    free (old_ents);
}

/**
 * @return A pointer to the entry, or NULL if not found.
 */
static hash_entry *hash_lookup(hash_table *tab, ScmObj key, int datum, int flag)
{
    size_t i;
    unsigned hashval;
    hash_entry *ent;

    /* If we have > 32 bits, we'll discard some of them.  The lower
     * bits are zeroed for alignment or used for tag bits, and in the
     * latter case, the tag can only take 3 values: pair, string, or
     * vector.  We'll drop these bits.  KEYs are expected to be
     * pointers into the heap, so their higher bis are probably
     * uniform.  I haven't confirmed either's validity, though. */
    hashval = (unsigned)key;
    if (sizeof(hashval) > 4) {
        hashval /= sizeof(ScmCell);
        hashval &= 0xffffffff;
    }

    hashval *= 2654435761UL; /* golden ratio hash */

    /* We probe linearly, since a) speed isn't a primary concern for
     * SigScheme, and b) having a table of primes only for this
     * purpose is probably just a waste. */
    for (i=0; i < tab->size; i++) {
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
    abort();
}

/**
 * Find out what non-atomic objects a structure shares within itself.
 * @param obj The object in question, or a part of it.
 * @param ctx Where to put the scan results.
 */
static void write_ss_scan(ScmObj obj, write_ss_context *ctx)
{
    int i;
    hash_entry *ent;

    /* (for-each mark-as-seen-or-return-if-familiar obj) */
    while (CONSP(obj)) {
        ent = hash_lookup(&ctx->seen, obj, NONDEFINING_DATUM, HASH_INSERT);
        if (ent) {
            ent->datum = DEFINING_DATUM;
            return;
        }
        write_ss_scan(CAR(obj), ctx);
        obj = CDR(obj);
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
            if (!SCM_NULLVALUESP(obj)) {
                write_ss_scan(CDR(SCM_VALUEPACKET_VALUES(obj)), ctx);
                /* SCM_VALUEPACKET_VALUES() changes the type destructively */
                SCM_ENTYPE_VALUEPACKET(obj);
            }
#else
            write_ss_scan(SCM_VALUEPACKET_VALUES(obj), ctx);
#endif
            break;

        case ScmVector:
            for (i=0; i < SCM_VECTOR_LEN(obj); i++)
                write_ss_scan(SCM_VECTOR_CREF(obj, i), ctx);
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
static int get_shared_index(ScmObj obj)
{
    hash_entry *ent;

    if (write_ss_ctx) {
        ent = hash_lookup(&write_ss_ctx->seen, obj, 0, HASH_FIND);

        if (ent) {
            if (ent->datum == DEFINING_DATUM) {
                ent->datum = write_ss_ctx->next_index++;
                return - (ent->datum);
            }
            return ent->datum;
        }
    }
    return 0;
}

void SigScm_WriteToPortWithSharedStructure(ScmObj port, ScmObj obj)
{
    write_ss_context ctx = {{0}};
    unsigned int i;

    ctx.next_index = 1;
    ctx.seen.size = 1 << 8; /* arbitrary initial size */
    ctx.seen.ents = Scm_calloc(ctx.seen.size, sizeof(hash_entry));
    for (i = 0; i < ctx.seen.size; i++) {
        ctx.seen.ents[i].key = SCM_INVALID;
    }

    write_ss_scan(obj, &ctx);

    /* If no structure is shared, we do a normal write. */
    if (!HASH_EMPTY(ctx.seen))
        write_ss_ctx = &ctx;

    SigScm_WriteToPort(port, obj);

    write_ss_ctx = NULL;
    free(ctx.seen.ents);
}
#endif /* SCM_USE_SRFI38 */
