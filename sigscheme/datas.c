/*===========================================================================
 *  FileName : datas.c
 *  About    : GC(Garbage Collection) and Allocation
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
 * Description of the Garbage Collection
 *
 * Our GC uses Mark-and-Sweep algorithm. So, we have MARK phase and SWEEP phase.
 *
 * [1] Mark phase : gc_mark()
 *   - gc_mark_locations()
 *       marking the Scheme object which are stored in the registers.
 *
 *   - gc_mark_protected_var()
 *       marking the protected Scheme object which are being hold by C
 *       variables registered by SigScm_GC_Protect().
 *
 *   - gc_mark_locations()
 *       marking the Scheme object which are pushed to the stack, so we need to
 *       traverse the stack for marking the objects.
 *
 *   - gc_mark_symbol_hash()
 *       marking the Scheme object which is interned by calling Scm_Intern().
 *
 * [2] Sweep phase : gc_sweep()
 *   - scanning heaps and move non-marked object to the freelist.
 */

/*=======================================
  System Include
=======================================*/
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>

/*=======================================
  Portability Coordination
=======================================*/
#if 0
/* malloc.h is obsoleted by stdlib.h. At least FreeBSD generates an error. */
#include <malloc.h>
#endif

#if 0
#ifndef posix_memalign
/*
 * Cited from manpage of posix_memalign(3) of glibc:
 *
 * CONFORMING TO
 *     The  function  valloc()  appeared in 3.0 BSD. It is documented as being
 *     obsolete in BSD 4.3, and as legacy in SUSv2. It  no  longer  occurs  in
 *     SUSv3.   The  function memalign() appears in SunOS 4.1.3 but not in BSD
 *     4.4.  The function posix_memalign() comes from POSIX 1003.1d.
 */
#error "posix_memalign(3) is not available in this system"
#endif
#endif

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"
#include "encoding.h"

#if !SCM_OBJ_COMPACT
#if (SCM_CHARCELL_SIZE <= SCM_MB_MAX_LEN)
#error
#error "SCM_MB_MAX_LEN is exceeded design limit"
#endif
#endif /* !SCM_OBJ_COMPACT */

/*=======================================
  File Local Struct Declarations
=======================================*/
typedef ScmObj ScmObjHeap;

/* Represents C variable that is holding a ScmObj to be protected from GC */
typedef struct gc_protected_var_ gc_protected_var;
struct gc_protected_var_ {
    ScmObj *var;
    gc_protected_var *next_var;
};

/*=======================================
  File Local Macro Declarations
=======================================*/
/* specifies whether the storage abstraction layer can only handle nested
 * (stacked) continuation or R5RS-conformant full implementation. But current
 * implementation only supports '1'.
 */
#define SCM_NESTED_CONTINUATION_ONLY 1

#define INVALID_CONTINUATION_JMPENV  NULL

#define CONTINUATION_JMPENV          SCM_CONTINUATION_OPAQUE0
#define CONTINUATION_SET_JMPENV      SCM_CONTINUATION_SET_OPAQUE0
#define CONTINUATION_DYNEXT          SCM_CONTINUATION_OPAQUE1
#define CONTINUATION_SET_DYNEXT      SCM_CONTINUATION_SET_OPAQUE1

#define NAMEHASH_SIZE 1024

#define SCM_NEW_OBJ_INTERNAL(VALNAME)                                        \
    do {                                                                     \
        if (NULLP(scm_freelist))                                             \
            gc_mark_and_sweep();                                             \
        VALNAME = scm_freelist;                                              \
        scm_freelist = SCM_FREECELL_CDR(scm_freelist);                       \
    } while (/* CONSTCOND */ 0)

#define SCM_UNMARKER        0
#define SCM_INITIAL_MARKER  (SCM_UNMARKER + 1)
#if 1
#define SCM_IS_MARKED(a)    (SCM_MARK(a) == scm_cur_marker)
#define SCM_IS_UNMARKED(a)  (!SCM_IS_MARKED)
#define SCM_DO_MARK(a)      (SCM_MARK(a) = scm_cur_marker)
#define SCM_DO_UNMARK(a)    (SCM_MARK(a) = SCM_UNMARKER)
#define SCM_MARK_CORRUPT(a) ((unsigned)SCM_MARK(a) > (unsigned)scm_cur_marker)
#else
/* YamaKen's suggestion: remove if you don't favor them */
#define SCM_MARK_VALUE(a)     ((a)->gcmark)
#define SCM_MARKED(a)         (SCM_MARK_VALUE(a) == scm_cur_marker)
#define SCM_UNMARKED(a)       (!SCM_MARKED(a))
#define SCM_MARK(a)           (SCM_MARK_VALUE(a) = scm_cur_marker)
#define SCM_UNMARK(a)         (SCM_MARK_VALUE(a) = SCM_UNMARKER)
#define SCM_MARK_CORRUPTED(a) ((unsigned)SCM_MARK_VALUE(a) > (unsigned)scm_cur_marker)
#endif

/* special constant initialization */
#define SCM_CONSTANT_BIND_SUBSTANCE(obj, cell)                                \
    do {                                                                     \
        (obj) = &(cell);                                                     \
        SCM_ENTYPE((obj), ScmConstant);                                      \
    } while(/* CONSTCOND */ 0)

/*=======================================
  Variable Declarations
=======================================*/
static int           SCM_HEAP_SIZE = 10240;
static int           scm_heap_num  = 8;
static ScmObjHeap   *scm_heaps     = NULL;
static ScmObj        scm_freelist  = NULL;

static int           scm_cur_marker = SCM_INITIAL_MARKER;

static jmp_buf save_regs_buf;
ScmObj *scm_stack_start_pointer = NULL;
#if UIM_SCM_GCC4_READY_GC
/* See also the comment about these variables in sigscheme.h */
ScmObj *(*volatile scm_gc_protect_stack)(ScmObj *)
    = &SigScm_GC_ProtectStackInternal;
#endif /* UIM_SCM_GCC4_READY_GC */

/* multiple values */
#if SCM_USE_VALUECONS
ScmObj SigScm_null_values;
#endif

/* dynamic extent */
static ScmObj current_dynamic_extent = NULL;

/* temporary store for a object returned from a continuation */
static ScmObj continuation_thrown_obj = NULL;
static ScmObj continuation_stack = NULL;

static ScmObj *symbol_hash = NULL;
static gc_protected_var *protected_var_list = NULL;

ScmObj SigScm_null, SigScm_true, SigScm_false, SigScm_eof;
ScmObj SigScm_unbound, SigScm_undef;

static ScmCell SigScm_null_cell, SigScm_true_cell, SigScm_false_cell, SigScm_eof_cell;
static ScmCell SigScm_unbound_cell, SigScm_undef_cell;

/*=======================================
  File Local Function Declarations
=======================================*/
static void initialize_special_constants(void);
static void *malloc_aligned(size_t size);

static void allocate_heap(ScmObjHeap **heaps, int num_heap, int HEAP_SIZE, ScmObj *freelist);
static void add_heap(ScmObjHeap **heaps, int *num_heap, int HEAP_SIZE, ScmObj *freelist);
static void finalize_heap(void);

static void gc_preprocess(void);
static void gc_mark_and_sweep(void);

/* GC Mark Related Functions */
static void mark_obj(ScmObj obj);
static int  is_pointer_to_heap(ScmObj obj);

static void gc_mark_protected_var();
static void gc_mark_locations_n(ScmObj *start, int n);
static void gc_mark_locations(ScmObj *start, ScmObj *end);
static void gc_mark(void);

/* GC Sweep Related Functions */
static void sweep_obj(ScmObj obj);
static void gc_sweep(void);

/* dynamic extent */
static void initialize_dynamic_extent(void);
static void finalize_dynamic_extent(void);
static void wind_onto_dynamic_extent(ScmObj before, ScmObj after);
static void unwind_dynamic_extent(void);
static void enter_dynamic_extent(ScmObj dest);
static void exit_dynamic_extent(ScmObj dest);

/* continuation */
static void initialize_continuation_env(void);
static void finalize_continuation_env(void);
static void continuation_stack_push(ScmObj cont);
static ScmObj continuation_stack_pop(void);
static ScmObj continuation_stack_unwind(ScmObj dest_cont);

static void initialize_symbol_hash(void);
static void finalize_symbol_hash(void);
static int  symbol_name_hash(const char *name);

static void finalize_protected_var(void);

/*=======================================
  Function Implementations
=======================================*/
/*
 * To keep storage representation abstract, the special constants
 * initialization is encapsulated in this file. Upper layers must only use
 * abstract interfaces such as SCM_NULL and SCM_NULLP().
 */
static void initialize_special_constants(void)
{
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_null,    SigScm_null_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_true,    SigScm_true_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_false,   SigScm_false_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_eof,     SigScm_eof_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_unbound, SigScm_unbound_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_undef,   SigScm_undef_cell);
#if SCM_COMPAT_SIOD_BUGS
    SigScm_false = SigScm_null;
#endif
}

void SigScm_InitStorage(void)
{
    initialize_special_constants();
    allocate_heap(&scm_heaps, scm_heap_num, SCM_HEAP_SIZE, &scm_freelist);
#if 0 && SCM_COMPAT_SIOD_BUGS
    SigScm_GC_Protect(&SigScm_true);
    SigScm_true = Scm_NewInt(1);
#endif

#if SCM_USE_VALUECONS
    /*
     * To keep storage model abstract, the cell is allocated from a heap
     * instead of directly construct ScmCell
     */
    SigScm_null_values = CONS(SCM_NULL, SCM_NULL);
    SCM_ENTYPE_VALUEPACKET(SigScm_null_values);
    SigScm_GC_Protect(&SigScm_null_values);
#endif
    initialize_dynamic_extent();
    initialize_continuation_env();
    initialize_symbol_hash();
}

void SigScm_FinalizeStorage(void)
{
    finalize_continuation_env();
    finalize_dynamic_extent();
    finalize_heap();
    finalize_symbol_hash();
    finalize_protected_var();
}

static void *malloc_aligned(size_t size)
{
    void *p;
    /* 2005/08/08  Kazuki Ohta  <mover@hct.zaq.ne.jp>
     * commented out "posix_memalign"
     *
     * posix_memalign(&p, 16, size);
     */
    p = malloc(size);
    return p;
}


/*============================================================================
  Heap Allocator & Garbage Collector
============================================================================*/
static void allocate_heap(ScmObjHeap **heaps, int num_heap, int HEAP_SIZE, ScmObj *freelist)
{
    int i = 0;
    ScmObj heap, cell;

    CDBG((SCM_DBG_GC, "allocate_heap num:%d size:%d", num_heap, HEAP_SIZE));

    /* allocate heap */
    (*heaps) = (ScmObj*)malloc(sizeof(ScmObj) * num_heap);
    (*freelist) = SCM_NULL;

    /* fill with zero and construct free_list */
    for (i = 0; i < num_heap; i++) {
        /* Initialize Heap */
        heap = (ScmObj)malloc_aligned(sizeof(ScmCell) * HEAP_SIZE);
        (*heaps)[i] = heap;

        /* link in order */
        for (cell=heap; cell-heap < HEAP_SIZE; cell++) {
            SCM_ENTYPE_FREECELL(cell);
            SCM_DO_UNMARK(cell);
            SCM_FREECELL_SET_CDR(cell, cell+1);
        }

        SCM_FREECELL_SET_CDR(cell-1, (*freelist));
        /* and freelist is head of the heap */
        (*freelist) = (*heaps)[i];
    }
}

static void add_heap(ScmObjHeap **heaps, int *orig_num_heap, int HEAP_SIZE, ScmObj *freelist)
{
    int    num_heap = 0;
    ScmObj heap, cell;

    CDBG((SCM_DBG_GC, "add_heap current num of heaps:%d", *orig_num_heap));

    /* increment num_heap */
    (*orig_num_heap) += 1;
    num_heap = (*orig_num_heap);

    /* add heap */
    (*heaps) = (ScmObj*)realloc((*heaps), sizeof(ScmObj) * num_heap);

    /* allocate heap */
    heap = (ScmObj)malloc_aligned(sizeof(ScmCell) * HEAP_SIZE);
    (*heaps)[num_heap - 1] = heap;

    /* link in order */
    for (cell=heap; cell-heap < HEAP_SIZE; cell++) {
        SCM_ENTYPE_FREECELL(cell);
        SCM_DO_UNMARK(cell);
        SCM_FREECELL_SET_CDR(cell, cell+1);
    }

    SCM_FREECELL_SET_CDR(cell-1, *freelist);
    (*freelist) = (*heaps)[num_heap - 1];
}

static void finalize_heap(void)
{
    int i = 0;
    int j = 0;

    for (i = 0; i < scm_heap_num; i++) {
        for (j = 0; j < SCM_HEAP_SIZE; j++) {
            sweep_obj(&scm_heaps[i][j]);
        }
        free(scm_heaps[i]);
    }
    free(scm_heaps);
}

static void gc_preprocess(void)
{
    ++scm_cur_marker;           /* make everything unmarked */

    if (scm_cur_marker == SCM_UNMARKER) {
        /* We've been running long enough to do
         * (1 << (sizeof(int)*8)) - 1 GCs, yay! */
        int  i = 0;
        long j = 0;

        scm_cur_marker = SCM_INITIAL_MARKER;

        /* unmark everything */
        for (i = 0; i < scm_heap_num; i++) {
            for (j = 0; j < SCM_HEAP_SIZE; j++) {
                SCM_DO_UNMARK(&scm_heaps[i][j]);
            }
        }
    }
}

static void gc_mark_and_sweep(void)
{
    CDBG((SCM_DBG_GC, "[ gc start ]"));

    gc_preprocess();
    gc_mark();
    gc_sweep();

    /* we cannot sweep the object, so let's add new heap */
    if (NULLP(scm_freelist)) {
        CDBG((SCM_DBG_GC, "Cannot sweep the object, allocating new heap."));
        add_heap(&scm_heaps, &scm_heap_num, SCM_HEAP_SIZE, &scm_freelist);
    }
}

static void mark_obj(ScmObj obj)
{
    int i = 0;

mark_loop:
    /* no need to mark SCM_NULL */
    if (NULLP(obj))
        return;

    /* avoid cyclic marking */
    if (SCM_IS_MARKED(obj))
        return;

    /* mark this object */
    SCM_DO_MARK(obj);

    /* mark recursively */
    switch (SCM_TYPE(obj)) {
    case ScmCons:
        mark_obj(CAR(obj));
        obj = CDR(obj);
        goto mark_loop;
        break;

    case ScmSymbol:
        mark_obj(SCM_SYMBOL_VCELL(obj));
        break;

    case ScmClosure:
        mark_obj(SCM_CLOSURE_EXP(obj));
        obj = SCM_CLOSURE_ENV(obj);
        goto mark_loop;
        break;

    case ScmValuePacket:
#if SCM_USE_VALUECONS
        mark_obj(SCM_VALUECONS_CAR(obj));
        obj = SCM_VALUECONS_CDR(obj);
#else
        obj = SCM_VALUEPACKET_VALUES(obj);
#endif
        goto mark_loop;

    case ScmVector:
        for (i = 0; i < SCM_VECTOR_LEN(obj); i++) {
            mark_obj(SCM_VECTOR_VEC(obj)[i]);
        }
        break;

    default:
        break;
    }
}

void SigScm_GC_Protect(ScmObj *var)
{
    gc_protected_var *item;

    item = (gc_protected_var *)malloc(sizeof(gc_protected_var));
    item->var = var;

    item->next_var = protected_var_list;
    protected_var_list = item;
}

void SigScm_GC_Unprotect(ScmObj *var)
{
    gc_protected_var **item = &protected_var_list;
    gc_protected_var *next  = NULL;
    while (*item) {
        if ((*item)->var == var) {
            next = (*item)->next_var;
            free(*item);
            *item = next;
            break;
        }
    }
}

static void finalize_protected_var(void)
{
    gc_protected_var *item = protected_var_list;
    gc_protected_var *tmp  = NULL;
    while (item) {
        *item->var = NULL;
        tmp  = item;
        item = item->next_var;
        free(tmp);
    }
    protected_var_list = NULL;
}

static int is_pointer_to_heap(ScmObj obj)
{
    /* The core part of Conservative GC */
    int i = 0;
    ScmObj head = SCM_NULL;
    for (i = 0; i < scm_heap_num; i++) {
        if ((head = scm_heaps[i])
            && (head <= obj)
            && (obj  <  head + SCM_HEAP_SIZE)
            && ((((char*)obj - (char*)head) % sizeof(ScmCell)) == 0))
            return 1;
    }

    return 0;
}

static void gc_mark_protected_var(void)
{
    gc_protected_var *item = NULL;
    for (item = protected_var_list; item; item = item->next_var) {
        mark_obj(*item->var);
    }
}

static void gc_mark_locations_n(ScmObj *start, int n)
{
    int i = 0;
    ScmObj obj = SCM_NULL;

    /* mark stack */
    for (i = 0; i < n; i++) {
        obj = start[i];

        if (is_pointer_to_heap(obj)) {
            mark_obj(obj);
        }
    }

}

static void gc_mark_locations(ScmObj *start, ScmObj *end)
{
    int size = 0;
    ScmObj *tmp = NULL;

    /* swap end and start if (end < start) */
    if (end < start) {
        tmp = end;
        end = start;
        start = tmp;
    }

    /* get size */
    size = end - start;

    CDBG((SCM_DBG_GC, "gc_mark_locations() : size = %d", size));

    gc_mark_locations_n(start, size);
}

static void gc_mark_symbol_hash(void)
{
    int i = 0;
    for (i = 0; i < NAMEHASH_SIZE; i++) {
        mark_obj(symbol_hash[i]);
    }
}

static void gc_mark(void)
{
    ScmObj stack_end;
    void *save_regs_buf_end = (char *)save_regs_buf + sizeof(save_regs_buf);

    CDBG((SCM_DBG_GC, "gc_mark()"));

    setjmp(save_regs_buf);
    gc_mark_locations((ScmObj *)save_regs_buf, (ScmObj *)save_regs_buf_end);
    gc_mark_protected_var();
    gc_mark_locations(scm_stack_start_pointer, &stack_end);
    gc_mark_symbol_hash();
}

static void sweep_obj(ScmObj obj)
{
    /* if the type has the pointer to free, then free it! */
    switch (SCM_TYPE(obj)) {
    case ScmInt:
    case ScmCons:
    case ScmClosure:
        break;

    case ScmChar:
        if (SCM_CHAR_VALUE(obj))
            free(SCM_CHAR_VALUE(obj));
        break;

    case ScmString:
        if (SCM_STRING_STR(obj))
            free(SCM_STRING_STR(obj));
        break;

    case ScmVector:
        if (SCM_VECTOR_VEC(obj))
            free(SCM_VECTOR_VEC(obj));
        break;

    case ScmSymbol:
        if (SCM_SYMBOL_NAME(obj))
            free(SCM_SYMBOL_NAME(obj));
        break;

    case ScmPort:
        if (SCM_PORT_IMPL(obj))
            SCM_PORT_CLOSE_IMPL(obj);
        break;

    /* rarely swept objects */
    case ScmContinuation:
    case ScmFunc:
    case ScmConstant:
    case ScmFreeCell:
    default:
        break;
    }
}

static void gc_sweep(void)
{
    int i = 0;
    int j = 0;
    int corrected_obj_num = 0;

    ScmObj obj = SCM_NULL;
    ScmObj scm_new_freelist = SCM_NULL;
    /* iterate heaps */
    for (i = 0; i < scm_heap_num; i++) {
        corrected_obj_num = 0;

        /* iterate in heap */
        for (j = 0; j < SCM_HEAP_SIZE; j++) {
            obj = &scm_heaps[i][j];
            SCM_ASSERT(!SCM_MARK_CORRUPT(obj));
            if (!SCM_IS_MARKED(obj)) {
                sweep_obj(obj);

                SCM_ENTYPE_FREECELL(obj);
                SCM_FREECELL_SET_CAR(obj, SCM_NULL);
                SCM_FREECELL_SET_CDR(obj, scm_new_freelist);
                scm_new_freelist = obj;
                corrected_obj_num++;
            }
        }

        CDBG((SCM_DBG_GC, "heap[%d] swept = %d", i, corrected_obj_num));
    }
    scm_freelist = scm_new_freelist;
}

#if SCM_GCC4_READY_GC
ScmObj *SigScm_GC_ProtectStackInternal(ScmObj *designated_stack_start)
{
    /*
     * &stack_start will be relocated to start of the frame of subsequent
     * function call
     */
    ScmObj stack_start;

    if (!designated_stack_start)
        designated_stack_start = &stack_start;

    if (!scm_stack_start_pointer)
        scm_stack_start_pointer = designated_stack_start;

    /* may intentionally be an invalidated local address */
    return designated_stack_start;
}

#else /* SCM_GCC4_READY_GC */

void SigScm_GC_ProtectStack(ScmObj *stack_start)
{
    if (!scm_stack_start_pointer)
        scm_stack_start_pointer = stack_start;
}
#endif /* SCM_GCC4_READY_GC */

void SigScm_GC_UnprotectStack(ScmObj *stack_start)
{
    if (scm_stack_start_pointer == stack_start)
        scm_stack_start_pointer = NULL;
}

/*===========================================================================
  Object Allocators
===========================================================================*/
ScmObj Scm_NewCons(ScmObj a, ScmObj b)
{
    ScmObj obj = SCM_FALSE;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_CONS(obj);
    SET_CAR(obj, a);
    SET_CDR(obj, b);

    return obj;
}

ScmObj Scm_NewInt(int val)
{
    ScmObj obj = SCM_FALSE;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_INT(obj);
    SCM_INT_SET_VALUE(obj, val);

    return obj;
}

ScmObj Scm_NewSymbol(char *name, ScmObj v_cell)
{
    ScmObj obj = SCM_FALSE;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_SYMBOL(obj);
    SCM_SYMBOL_SET_NAME(obj, name);
    SCM_SYMBOL_SET_VCELL(obj, v_cell);

    return obj;
}

ScmObj Scm_NewChar(char *ch)
{
    ScmObj obj = SCM_FALSE;
    int len;

    len = Scm_mb_bare_c_strlen(ch);
    if (len > SCM_MB_MAX_LEN) {
        SigScm_Error("Scm_NewChar : invalid character ch = [%s], len = %d",
                     ch, len);
    }

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_CHAR(obj);
    SCM_CHAR_SET_VALUE(obj, ch);

    return obj;
}

ScmObj Scm_NewString(char *str)
{
    ScmObj obj = SCM_FALSE;

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_STRING(obj);
    SCM_STRING_SET_STR(obj, str);
    SCM_STRING_SET_LEN(obj, str ? Scm_mb_bare_c_strlen(str) : 0);

    return obj;
}

ScmObj Scm_NewStringCopying(const char *str)
{
    ScmObj obj = SCM_FALSE;

    if (!str) str = "";
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_STRING(obj);
    SCM_STRING_SET_STR(obj, strdup(str));
    SCM_STRING_SET_LEN(obj, Scm_mb_bare_c_strlen(str));

    return obj;
}

ScmObj Scm_NewStringWithLen(char *str, int len)
{
    ScmObj obj = SCM_FALSE;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_STRING(obj);
    SCM_STRING_SET_STR(obj, str);
    SCM_STRING_SET_LEN(obj, len);

    return obj;
}

ScmObj Scm_NewFunc(enum ScmFuncTypeCode type, ScmFuncType func)
{
    ScmObj obj = SCM_FALSE;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_FUNC(obj);
    SCM_FUNC_SET_TYPECODE(obj, type);
    SCM_FUNC_SET_CFUNC(obj, func);

    return obj;
}

ScmObj Scm_NewClosure(ScmObj exp, ScmObj env)
{
    ScmObj obj = SCM_FALSE;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_CLOSURE(obj);
    SCM_CLOSURE_SET_EXP(obj, exp);
    SCM_CLOSURE_SET_ENV(obj, env);

    return obj;
}

ScmObj Scm_NewVector(ScmObj *vec, int len)
{
    ScmObj obj = SCM_FALSE;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_VECTOR(obj);
    SCM_VECTOR_SET_VEC(obj, vec);
    SCM_VECTOR_SET_LEN(obj, len);

    return obj;
}

ScmObj Scm_NewPort(ScmCharPort *cport, enum ScmPortFlag flag)
{
    ScmObj obj = SCM_FALSE;

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_PORT(obj);

    if (flag & SCM_PORTFLAG_INPUT)
        flag |= SCM_PORTFLAG_LIVE_INPUT;
    if (flag & SCM_PORTFLAG_OUTPUT)
        flag |= SCM_PORTFLAG_LIVE_OUTPUT;
    SCM_PORT_SET_FLAG(obj, flag);

    SCM_PORT_SET_IMPL(obj, cport);

    return obj;
}

ScmObj Scm_NewContinuation(void)
{
    ScmObj obj = SCM_FALSE;

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_CONTINUATION(obj);
    CONTINUATION_SET_JMPENV(obj, INVALID_CONTINUATION_JMPENV);
    CONTINUATION_SET_DYNEXT(obj, current_dynamic_extent);

    return obj;
}

#if !SCM_USE_VALUECONS
ScmObj Scm_NewValuePacket(ScmObj values)
{
    ScmObj obj = SCM_FALSE;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_VALUEPACKET(obj);
    SCM_VALUEPACKET_SET_VALUES(obj, values);

    return obj;
}
#endif

#if SCM_USE_NONSTD_FEATURES
ScmObj Scm_NewCPointer(void *data)
{
    ScmObj obj = SCM_FALSE;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_C_POINTER(obj);
    SCM_C_POINTER_SET_VALUE(obj, data);

    return obj;
}

ScmObj Scm_NewCFuncPointer(ScmCFunc func)
{
    ScmObj obj = SCM_FALSE;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_C_FUNCPOINTER(obj);
    SCM_C_FUNCPOINTER_SET_VALUE(obj, func);

    return obj;
}
#endif /* SCM_USE_NONSTD_FEATURES */

/*============================================================================
  Dynamic Extent
============================================================================*/
#define MAKE_DYNEXT_FRAME(before, after) (CONS(before, after))
#define DYNEXT_FRAME_BEFORE CAR
#define DYNEXT_FRAME_AFTER  CDR

static void initialize_dynamic_extent(void)
{
    current_dynamic_extent = SCM_NULL;
    SigScm_GC_Protect(&current_dynamic_extent);
}

static void finalize_dynamic_extent(void)
{
}

static void wind_onto_dynamic_extent(ScmObj before, ScmObj after)
{
    current_dynamic_extent = CONS(MAKE_DYNEXT_FRAME(before, after),
                                  current_dynamic_extent);
}

static void unwind_dynamic_extent(void)
{
    if (NULLP(current_dynamic_extent))
        SigScm_Error("corrupted dynamic extent");

    current_dynamic_extent = CDR(current_dynamic_extent);
}

/* enter a dynamic extent of another continuation (dest) */
static void enter_dynamic_extent(ScmObj dest)
{
    ScmObj frame   = SCM_FALSE;
    ScmObj unwound = SCM_FALSE;
    ScmObj retpath = SCM_NULL;

    for (unwound = dest; !NULLP(unwound); unwound = CDR(unwound)) {
        if (EQ(unwound, current_dynamic_extent))
            break;
        frame = CAR(unwound);
        retpath = CONS(frame, retpath);
    }

    /* assumes that (SCM_NULL != NULL) */
    while (SCM_SHIFT_RAW(frame, retpath)) {
        Scm_call(DYNEXT_FRAME_BEFORE(frame), SCM_NULL);
    }
}

/* exit to a dynamic extent of another continuation (dest) */
static void exit_dynamic_extent(ScmObj dest)
{
    ScmObj frame = SCM_FALSE;

    for (;
         !NULLP(current_dynamic_extent);
         current_dynamic_extent = CDR(current_dynamic_extent))
    {
        if (EQ(current_dynamic_extent, dest))
            return;
        frame = CAR(current_dynamic_extent);
        Scm_call(DYNEXT_FRAME_AFTER(frame), SCM_NULL);
    }
}

ScmObj Scm_DynamicWind(ScmObj before, ScmObj thunk, ScmObj after)
{
    ScmObj ret   = SCM_FALSE;

    Scm_call(before, SCM_NULL);
    
    wind_onto_dynamic_extent(before, after);
    ret = Scm_call(thunk, SCM_NULL);
    unwind_dynamic_extent();

    Scm_call(after, SCM_NULL);

    return ret;
}

/*============================================================================
  Continuation
============================================================================*/
static void initialize_continuation_env(void)
{
    continuation_thrown_obj = SCM_FALSE;
    continuation_stack = SCM_NULL;
    SigScm_GC_Protect(&continuation_thrown_obj);
    SigScm_GC_Protect(&continuation_stack);
}

static void finalize_continuation_env(void)
{
}

static void continuation_stack_push(ScmObj cont)
{
    continuation_stack = CONS(cont, continuation_stack);
}

static ScmObj continuation_stack_pop(void)
{
    ScmObj recentmost = SCM_FALSE;

    if (!NULLP(continuation_stack)) {
        recentmost = CAR(continuation_stack);
        continuation_stack = CDR(continuation_stack);
    }

    return recentmost;
}

/* expire all descendant continuations and dest_cont */
static ScmObj continuation_stack_unwind(ScmObj dest_cont)
{
    ScmObj cont = SCM_FALSE;

    do {
        cont = continuation_stack_pop();
        if (FALSEP(cont))
            return SCM_FALSE;
        CONTINUATION_SET_JMPENV(cont, INVALID_CONTINUATION_JMPENV);
    } while (!EQ(dest_cont, cont));

    return dest_cont;
}

ScmObj Scm_CallWithCurrentContinuation(ScmObj proc, ScmEvalState *eval_state)
{
    jmp_buf env;
    ScmObj cont = SCM_FALSE;
    ScmObj ret  = SCM_FALSE;

    cont = Scm_NewContinuation();
    CONTINUATION_SET_JMPENV(cont, &env);
#if SCM_NESTED_CONTINUATION_ONLY
    continuation_stack_push(cont);
#endif

    if (setjmp(env)) {
        /* returned from longjmp */
        ret = continuation_thrown_obj;
        continuation_thrown_obj = SCM_FALSE;  /* make ret sweepable */

        enter_dynamic_extent(CONTINUATION_DYNEXT(cont));

        eval_state->ret_type = SCM_RETTYPE_AS_IS;
        return ret;
    } else {
#if SCM_NESTED_CONTINUATION_ONLY
        /* call proc with current continutation as (proc cont): This call must
         * not be Scm_tailcall(), to preserve current stack until longjmp()
         * called.
         */
        eval_state->ret_type = SCM_RETTYPE_AS_IS;
        ret = Scm_call(proc, LIST_1(cont));
#else
        /* ONLY FOR TESTING: This call is properly recursible, but all
         * continuations are broken and cannot be called, if the continuation
         * is implemented by longjmp().
         */
        ret = Scm_tailcall(proc, LIST_1(cont), eval_state);
#endif

#if SCM_NESTED_CONTINUATION_ONLY
        /* the continuation expires when this function returned */
        continuation_stack_unwind(cont);
#endif
        return ret;
    }
}

void Scm_CallContinuation(ScmObj cont, ScmObj ret)
{
    jmp_buf *env;

    env = CONTINUATION_JMPENV(cont);

    if (env != INVALID_CONTINUATION_JMPENV
#if SCM_NESTED_CONTINUATION_ONLY
        && CONTINUATIONP(continuation_stack_unwind(cont))
#endif
        )
    {
        exit_dynamic_extent(CONTINUATION_DYNEXT(cont));

        continuation_thrown_obj = ret;
        longjmp(*env, 1);
        /* NOTREACHED */
    } else {
        ERR("Scm_CallContinuation: called expired continuation");
    }
}

/*============================================================================
  Symbol table
============================================================================*/
/*
 * Symbol Name Hash Related Functions
 *
 * - Data Structure of Symbol Name Hash
 *
 *     - n = symbol_name_hash(name)
 *     - symbol_hash[n] = sym_list
 *     - sym_list = ( ScmObj(SYMBOL) ScmObj(SYMBOL) ... )
 *
 */
static void initialize_symbol_hash(void)
{
    int i = 0;
    symbol_hash = (ScmObj*)malloc(sizeof(ScmObj) * NAMEHASH_SIZE);
    for (i = 0; i < NAMEHASH_SIZE; i++) {
        symbol_hash[i] = SCM_NULL;
    }
}

static void finalize_symbol_hash(void)
{
    free(symbol_hash);
}

static int symbol_name_hash(const char *name)
{
    int hash = 0;
    int c;
    char *cname = (char *)name;
    while ((c = *cname++)) {
        hash = ((hash * 17) ^ c) % NAMEHASH_SIZE;
    }
    return hash;
}

ScmObj Scm_Intern(const char *name)
{
    int n = symbol_name_hash(name);
    ScmObj sym     = SCM_FALSE;
    ScmObj lst     = SCM_FALSE;
    ScmObj sym_lst = symbol_hash[n];

    /* Search Symbol by name */
    for (lst = sym_lst; !NULLP(lst); lst = CDR(lst)) {
        sym = CAR(lst);

        if (strcmp(SCM_SYMBOL_NAME(sym), name) == 0) {
            return sym;
        }
    }

    /* If not in the sym_lst, allocate new Symbol */
    sym = Scm_NewSymbol(strdup(name), SCM_UNBOUND);

    /* And Append it to the head of symbol_hash */
    sym_lst = CONS(sym, sym_lst);
    symbol_hash[n] = sym_lst;

    return sym;
}
