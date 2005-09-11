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

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/
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
#define SCM_INVALID NULL
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
#if SCM_USE_SRFI38
static write_ss_context *write_ss_ctx; /* misc info in priting shared structures */
#endif

/*=======================================
  File Local Function Declarations
=======================================*/
static void print_ScmObj_internal(FILE *f, ScmObj obj, enum OutputType otype);
static void print_char(FILE *f, ScmObj obj, enum OutputType otype);
static void print_string(FILE *f, ScmObj obj, enum OutputType otype);
static void print_list(FILE *f, ScmObj list, enum OutputType otype);
static void print_vector(FILE *f, ScmObj vec, enum OutputType otype);
static void print_port(FILE *f, ScmObj port, enum OutputType otype);
static void print_etc(FILE *f, ScmObj obj, enum  OutputType otype);

#if SCM_USE_SRFI38
static void hash_grow(hash_table *tab);
static hash_entry *hash_lookup(hash_table *tab, ScmObj key, int datum, int flag);
static void write_ss_scan(ScmObj obj, write_ss_context *ctx);
static int  get_shared_index(ScmObj obj);
#endif /* SCM_USE_SRFI38 */

/*=======================================
   Function Implementations
=======================================*/
void SigScm_Display(ScmObj obj)
{
    print_ScmObj_internal(SCM_PORTINFO_FILE(scm_current_output_port), obj, AS_WRITE);
    fprintf(SCM_PORTINFO_FILE(scm_current_output_port), "\n");
}

void SigScm_WriteToPort(ScmObj port, ScmObj obj)
{
    FILE *f = NULL;

    if (SCM_PORTINFO_PORTTYPE(port) == PORT_FILE) {
        f = SCM_PORTINFO_FILE(port);
        print_ScmObj_internal(f, obj, AS_WRITE);
        return;
    }

    SigScm_Error("SigScm_WriteToPort : support write only for file port.");
}

void SigScm_DisplayToPort(ScmObj port, ScmObj obj)
{
    FILE *f = NULL;

    if (SCM_PORTINFO_PORTTYPE(port) == PORT_FILE) {
        f = SCM_PORTINFO_FILE(port);
        print_ScmObj_internal(f, obj, AS_DISPLAY);
        return;
    }

    SigScm_Error("SigScm_DisplayToPort : support display only for file port.");
}

static void print_ScmObj_internal(FILE *f, ScmObj obj, enum OutputType otype)
{
#if SCM_USE_SRFI38
    if (INTERESTINGP(obj)) {
        int index = get_shared_index(obj);
        if (index > 0) {
            /* defined datum */
            fprintf(f, "#%d#", index);
            return;
        }
        if (index < 0) {
            /* defining datum, with the new index negated */
            fprintf(f, "#%d=", -index);
            /* Print it; the next time it'll be defined. */
        }
    }
#endif
    switch (SCM_TYPE(obj)) {
    case ScmInt:
        fprintf(f, "%d", SCM_INT_VALUE(obj));
        break;
    case ScmCons:
        print_list(f, obj, otype);
        break;
    case ScmSymbol:
        fprintf(f, "%s", SCM_SYMBOL_NAME(obj));
        break;
    case ScmChar:
        print_char(f, obj, otype);
        break;
    case ScmString:
        print_string(f, obj, otype);
        break;
    case ScmFunc:
        fprintf(f, "#<subr>");
        break;
    case ScmClosure:
        fprintf(f, "#<closure:");
        print_ScmObj_internal(f, SCM_CLOSURE_EXP(obj), otype);
        fprintf(f, ">");
        break;
    case ScmVector:
        print_vector(f, obj, otype);
        break;
    case ScmPort:
        print_port(f, obj, otype);
        break;
    case ScmContinuation:
        fprintf(f, "#<subr continuation>");
        break;
    case ScmValuePacket:
        fputs("#<values ", f);
        print_list(f, SCM_VALUEPACKET_VALUES(obj), otype);
        putc('>', f);
        break;
    case ScmEtc:
        print_etc(f, obj, otype);
        break;
    case ScmFreeCell:
        SigScm_Error("You cannot print ScmFreeCell, may be GC bug.\n");
        break;
    case ScmCPointer:
        fprintf(f, "#<c_pointer %p>", SCM_C_POINTER_VALUE(obj));
        break;
    case ScmCFuncPointer:
        fprintf(f, "#<c_func_pointer %p>", (void*)SCM_C_FUNCPOINTER_VALUE(obj));
        break;
    }
}

static void print_char(FILE *f, ScmObj obj, enum OutputType otype)
{
    switch (otype) {
    case AS_WRITE:
        /*
         * in write, character objects are written using the #\ notation.
         */
        if (strcmp(SCM_CHAR_VALUE(obj), " ") == 0) {
            fprintf(f, "#\\space");
        } else if(strcmp(SCM_CHAR_VALUE(obj), "\n") == 0) {
            fprintf(f, "#\\newline");
        } else {
            fprintf(f, "#\\%s", SCM_CHAR_VALUE(obj));
        }
        break;
    case AS_DISPLAY:
        /*
         * in display, character objects appear in the reqpresentation as
         * if writen by write-char instead of by write.
         */
        fprintf(f, "%s", SCM_CHAR_VALUE(obj));
        break;
    default:
        SigScm_Error("print_char : unknown output type\n");
        break;
    }
}

static void print_string(FILE *f, ScmObj obj, enum OutputType otype)
{
    const char *str = SCM_STRING_STR(obj);
    int  size = strlen(str);
    int  i = 0;
    char c = 0;

    switch (otype) {
    case AS_WRITE:
        /*
         * in write, strings that appear in the written representation are
         * enclosed in doublequotes, and within those strings backslash and
         * doublequote characters are escaped by backslashes.
         */
        fprintf(f, "\""); /* first doublequote */
        for (i = 0; i < size; i++) {
            c = str[i];
            switch (c) {
            case '\"': fprintf(f, "\\\""); break;
            case '\n': fprintf(f, "\\n"); break;
            case '\r': fprintf(f, "\\r"); break;
            case '\f': fprintf(f, "\\f"); break;
            case '\t': fprintf(f, "\\t"); break;
            case '\\': fprintf(f, "\\\\"); break;
            default:
                fprintf(f, "%c", str[i]); break;
            }
        }
        fprintf(f, "\""); /* last doublequote */
        break;
    case AS_DISPLAY:
        fprintf(f, "%s", SCM_STRING_STR(obj));
        break;
    default:
        SigScm_Error("print_string : unknown output type\n");
        break;
    }
}

static void print_list(FILE *f, ScmObj list, enum OutputType otype)
{
    ScmObj car = SCM_NULL;
#if SCM_USE_SRFI38
    int index;
    int necessary_close_parens = 1;
  cheap_recursion:
#endif

    /* print left parenthesis */
    fprintf(f, "(");

    for (;;) {
        car = CAR(list);
        print_ScmObj_internal(f, car, otype);
        list = CDR(list);
        if (!CONSP(list))
            break;
        fputs(" ", f);

#if SCM_USE_SRFI38
        /* See if the next pair is shared.  Note that the case
         * where the first pair is shared is handled in
         * print_ScmObj_internal(). */
        index = get_shared_index(list);
        if (index > 0) {
            /* defined datum */
            fprintf(f, ". #%d#", index);
            goto close_parens_and_return;
        }
        if (index < 0) {
            /* defining datum, with the new index negated */
            fprintf(f, ". #%d=", -index);
            necessary_close_parens++;
            goto cheap_recursion;
        }
#endif
    }

    /* last item */
    if (!NULLP(list)) {
        fputs(" . ", f);
        /* Callee takes care of shared data. */
        print_ScmObj_internal(f, list, otype);
    }

#if SCM_USE_SRFI38
  close_parens_and_return:
    while (necessary_close_parens--)
#endif
        fputc(')', f);
}

static void print_vector(FILE *f, ScmObj vec, enum OutputType otype)
{
    ScmObj *v = SCM_VECTOR_VEC(vec);
    int c_len = SCM_VECTOR_LEN(vec);
    int i     = 0;

    /* print left parenthesis with '#' */
    fprintf(f, "#(");

    /* print each element */
    for (i = 0; i < c_len; i++) {
        print_ScmObj_internal(f, v[i], otype);

        if (i != c_len - 1)
            fprintf(f, " ");
    }

    fprintf(f, ")");
}

static void print_port(FILE *f, ScmObj port, enum OutputType otype)
{
    fprintf(f, "#<");

    /* input or output */
    if (SCM_PORT_PORTDIRECTION(port) == PORT_INPUT)
        fprintf(f, "i");
    else
        fprintf(f, "o");

    fprintf(f, "port ");

    /* file or string */
    if (SCM_PORTINFO_PORTTYPE(port) == PORT_FILE)
        fprintf(f, "file %s", SCM_PORTINFO_FILENAME(port));
    else if (SCM_PORTINFO_PORTTYPE(port) == PORT_STRING)
        fprintf(f, "string %s", SCM_PORTINFO_STR(port));

    fprintf(f, ">");
}

static void print_etc(FILE *f, ScmObj obj, enum  OutputType otype)
{
    if (EQ(obj, SCM_NULL))
        fprintf(f, "()");
    else if (EQ(obj, SCM_TRUE))
        fprintf(f, "#t");
    else if (EQ(obj, SCM_FALSE))
        fprintf(f, "#f");
    else if (EQ(obj, SCM_EOF))
        fprintf(f, "#<eof>");
    else if (EQ(obj, SCM_QUOTE))
        fprintf(f, "#<quote>");
    else if (EQ(obj, SCM_QUASIQUOTE))
        fprintf(f, "#<quasiquote>");
    else if (EQ(obj, SCM_UNQUOTE))
        fprintf(f, "#<unquote>");
    else if (EQ(obj, SCM_UNQUOTE_SPLICING))
        fprintf(f, "#<unquote_splicing>");
    else if (EQ(obj, SCM_UNBOUND))
        fprintf(f, "#<unbound>");
    else if (EQ(obj, SCM_UNDEF))
        fprintf(f, "#<undef>");
}

#if SCM_USE_SRFI38
static void hash_grow(hash_table *tab)
{
    size_t old_size = tab->size;
    size_t new_size = old_size * 2;
    size_t i;
    hash_entry *old_ents = tab->ents;

    tab->ents = calloc(new_size, sizeof(hash_entry));
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
        hashval /= sizeof(ScmObjInternal);
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
            write_ss_scan(SCM_VALUEPACKET_VALUES(obj), ctx);
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

        if (ent->datum == DEFINING_DATUM) {
            ent->datum = write_ss_ctx->next_index++;
            return - (ent->datum);
        }
        return ent->datum;
    }
    return 0;
}

void SigScm_WriteToPortWithSharedStructure(ScmObj port, ScmObj obj)
{
    write_ss_context ctx = {{0}};

    ctx.next_index = 1;
    ctx.seen.size = 1 << 8; /* arbitrary initial size */
    ctx.seen.ents = calloc(ctx.seen.size, sizeof(hash_entry));

    write_ss_scan(obj, &ctx);

    /* If no structure is shared, we do a normal write. */
    if (!HASH_EMPTY(ctx.seen))
        write_ss_ctx = &ctx;

    SigScm_WriteToPort(port, obj);

    write_ss_ctx = NULL;
    free(ctx.seen.ents);
}
#endif /* SCM_USE_SRFI38 */
