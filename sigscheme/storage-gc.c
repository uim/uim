/*===========================================================================
 *  FileName : storage-gc.c
 *  About    : Garbage Collection
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

#include <assert.h>

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

typedef ScmCell *ScmObjHeap;

/* Represents C variable that is holding a ScmObj to be protected from GC */
typedef struct gc_protected_var_ gc_protected_var;
struct gc_protected_var_ {
    ScmObj *var;
    gc_protected_var *next_var;
};

/*=======================================
  File Local Macro Declarations
=======================================*/
#define SCM_HEAP_SIZE 10240
#define NHEAP_INITIAL 1
#define NHEAP_MAX     8

#if !SCM_OBJ_COMPACT
#define SCM_UNMARKER          0
#define SCM_MARKER            (SCM_UNMARKER + 1)
#define SCM_IS_MARKED(a)      (SCM_MARK(a) == SCM_MARKER)
#define SCM_IS_UNMARKED(a)    (!SCM_IS_MARKED(a))
#define SCM_DO_MARK(a)        (SCM_MARK(a) = SCM_MARKER)
#define SCM_DO_UNMARK(a)      (SCM_MARK(a) = SCM_UNMARKER)
#endif /* !SCM_OBJ_COMPACT */

/*=======================================
  Variable Declarations
=======================================*/
static int           scm_heap_num;
static ScmObjHeap   *scm_heaps     = NULL;
static ScmObj        scm_freelist  = NULL;

static jmp_buf save_regs_buf;
static ScmObj *stack_start_pointer = NULL;
#if UIM_SCM_GCC4_READY_GC
/* See also the comment about these variables in sigscheme.h */
ScmObj *(*volatile scm_gc_protect_stack)(ScmObj *)
    = &SigScm_GC_ProtectStackInternal;
#endif /* UIM_SCM_GCC4_READY_GC */

static gc_protected_var *protected_var_list = NULL;

/* storage-symbol.c */
extern ScmObj *scm_symbol_hash;

/*=======================================
  File Local Function Declarations
=======================================*/
static void *malloc_aligned(size_t size);

static void initialize_heap(void);
static void add_heap(ScmObjHeap **heaps, int *num_heap, size_t heap_size, ScmObj *freelist);
static void finalize_heap(void);

static void gc_mark_and_sweep(void);

/* GC Mark Related Functions */
static void mark_obj(ScmObj obj);
static int  is_pointer_to_heap(ScmObj obj);

static void gc_mark_protected_var();
static void gc_mark_locations_n(ScmObj *start, int n);
static void gc_mark_locations(ScmObj *start, ScmObj *end);
static void gc_mark(void);

/* GC Sweep Related Functions */
static void free_cell(ScmCell *cell);
static void gc_sweep(void);

static void finalize_protected_var(void);

/*=======================================
  Function Implementations
=======================================*/
void SigScm_InitGC(void)
{
    initialize_heap();
}

void SigScm_FinalizeGC(void)
{
    finalize_heap();
    finalize_protected_var();
}

ScmObj SigScm_NewObjFromHeap(void)
{
    ScmObj ret = SCM_FALSE;

    if (NULLP(scm_freelist))
        gc_mark_and_sweep();

    ret = scm_freelist;
    scm_freelist = SCM_FREECELL_CDR(scm_freelist);

    return ret;
}

/*============================================================================
  ScmObj Protection
============================================================================*/
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
    gc_protected_var *item, **prev_next;

    prev_next = &protected_var_list;
    for (item = protected_var_list; item; item = item->next_var) {
        if (item->var == var) {
            *prev_next = item->next_var;
            free(item);
            break;
        }
        prev_next = &item->next_var;
    }
}

/*============================================================================
  C Stack Protection
============================================================================*/
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

    if (!stack_start_pointer)
        stack_start_pointer = designated_stack_start;

    /* may intentionally be an invalidated local address */
    return designated_stack_start;
}

#else /* SCM_GCC4_READY_GC */

void SigScm_GC_ProtectStack(ScmObj *stack_start)
{
    if (!stack_start_pointer)
        stack_start_pointer = stack_start;
}
#endif /* SCM_GCC4_READY_GC */

void SigScm_GC_UnprotectStack(ScmObj *stack_start)
{
    if (stack_start_pointer == stack_start)
        stack_start_pointer = NULL;
}

/*============================================================================
  Heap Allocator & Garbage Collector
============================================================================*/
/* FIXME: ensure safety in a portable way */
static void *malloc_aligned(size_t size)
{
    void *p;

#if HAVE_POSIX_MEMALIGN
    /*
     * Cited from manpage of posix_memalign(3) of glibc:
     *
     * CONFORMING TO
     *     The function valloc() appeared in 3.0 BSD. It is documented as being
     *     obsolete in BSD 4.3, and as legacy in SUSv2. It no longer occurs in
     *     SUSv3.  The function memalign() appears in SunOS 4.1.3 but not in
     *     BSD 4.4.  The function posix_memalign() comes from POSIX 1003.1d.
     */
    posix_memalign(&p, 16, size);
#else
    p = malloc(size);
    /* heaps must be aligned to sizeof(ScmCell) */
    assert(!((uintptr_t)p % sizeof(ScmCell)));
#endif

    return p;
}

static void initialize_heap(void)
{
    int i;

    scm_heap_num = 0;
    scm_heaps = NULL;
    scm_freelist = SCM_NULL;

    /* preallocate heaps */
    for (i = 0; i < NHEAP_INITIAL; i++)
        add_heap(&scm_heaps, &scm_heap_num, SCM_HEAP_SIZE, &scm_freelist);
}

static void add_heap(ScmObjHeap **heaps, int *orig_num_heap, size_t heap_size, ScmObj *freelist)
{
    int num_heap;
    ScmObjHeap heap;
    ScmCell *cell;

    CDBG((SCM_DBG_GC, "add_heap current num of heaps:%d", *orig_num_heap));

    if (NHEAP_MAX <= *orig_num_heap)
        ERR("heap exhausted"); /* FIXME: replace with fatal error handling */

    num_heap = *orig_num_heap + 1;
    *orig_num_heap = num_heap;
    *heaps = realloc(*heaps, sizeof(ScmObjHeap) * num_heap);

    heap = malloc_aligned(sizeof(ScmCell) * heap_size);
    (*heaps)[num_heap - 1] = heap;

    /* link in order */
    for (cell = heap; cell < &heap[heap_size]; cell++) {
        SCM_ENTYPE_FREECELL(cell);
        SCM_DO_UNMARK(cell);
        SCM_FREECELL_SET_CDR(cell, cell + 1);
    }

    SCM_FREECELL_SET_CDR(cell - 1, *freelist);
    *freelist = heap;
}

static void finalize_heap(void)
{
    int i;
    ScmCell *cell;
    ScmObjHeap heap;

    for (i = 0; i < scm_heap_num; i++) {
        heap = scm_heaps[i];
        for (cell = &heap[0]; cell < &heap[SCM_HEAP_SIZE]; cell++)
            free_cell(cell);
        free(heap);
    }
    free(scm_heaps);
}

static void gc_mark_and_sweep(void)
{
    CDBG((SCM_DBG_GC, "[ gc start ]"));

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
#if SCM_OBJ_COMPACT
    /* no need to mark immediates */
    if (!SCM_CANBE_MARKED(obj))
        return;
#else
    /* no need to mark constants */
    if (SCM_CONSTANTP(obj))
        return;
#endif
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

    case ScmSymbol:
        obj = SCM_SYMBOL_VCELL(obj);
        goto mark_loop;

    case ScmClosure:
        mark_obj(SCM_CLOSURE_EXP(obj));
        obj = SCM_CLOSURE_ENV(obj);
        goto mark_loop;

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

/* The core part of Conservative GC */

/* TODO: improve average performance by maintaining max(scm_heaps[all]) and
 * min(scm_heaps[all]) at add_heap().
 */
static int is_pointer_to_heap(ScmObj obj)
{
    ScmCell *heap, *ptr;
    int i;

#if SCM_OBJ_COMPACT
    if (!SCM_CANBE_MARKED(obj))
        return 0;
    /* The pointer on the stack is 'tagged' to represent its types.
     * So we need to ignore the tag to get its real pointer value. */
    ptr = (ScmCell *)SCM_STRIP_TAG_INFO(obj);
#else
    ptr = obj;
#endif
    /* heaps must be aligned to sizeof(ScmCell) */
    if ((uintptr_t)ptr % sizeof(ScmCell))
        return 0;

    for (i = 0; i < scm_heap_num; i++) {
        heap = scm_heaps[i];
        if (heap && heap <= ptr && ptr < &heap[SCM_HEAP_SIZE])
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

/* mark a contiguous region such as stack */
static void gc_mark_locations_n(ScmObj *start, int n)
{
    ScmObj *objp;
    for (objp = start; objp < &start[n]; objp++) {
        if (is_pointer_to_heap(*objp))
            mark_obj(*objp);
    }
}

static void gc_mark_locations(ScmObj *start, ScmObj *end)
{
    int size;
    ScmObj *tmp;

    /* swap end and start if (end < start) */
    if (end < start) {
        tmp = end;
        end = start;
        start = tmp;
    }

    size = end - start;

    CDBG((SCM_DBG_GC, "gc_mark_locations() : size = %d", size));

    gc_mark_locations_n(start, size);
}

static void gc_mark_symbol_hash(void)
{
    ScmObj *objp;

    /* not initialized yet */
    if (!scm_symbol_hash)
        return;

    for (objp = &scm_symbol_hash[0];
         objp < &scm_symbol_hash[NAMEHASH_SIZE];
         objp++)
    {
        mark_obj(*objp);
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
    gc_mark_locations(stack_start_pointer, &stack_end);
    gc_mark_symbol_hash();
}

static void free_cell(ScmCell *cell)
{
#if SCM_OBJ_COMPACT
    if (SCM_NEED_SWEEPP(cell)) {
        if (SCM_SWEEP_PHASE_SYMBOLP(cell)) {
            if (SCM_SYMBOL_NAME(cell))
                free(SCM_SYMBOL_NAME(cell));
        } else if (SCM_SWEEP_PHASE_STRINGP(cell)) {
            if (SCM_STRING_STR(cell))
                free(SCM_STRING_STR(cell));
        } else if (SCM_SWEEP_PHASE_VECTORP(cell)) {
            if (SCM_VECTOR_VEC(cell))
                free(SCM_VECTOR_VEC(cell));
        } else if (SCM_SWEEP_PHASE_PORTP(cell)) {
            if (SCM_PORT_IMPL(cell))
                SCM_PORT_CLOSE_IMPL(cell);
        } else if (SCM_SWEEP_PHASE_CONTINUATIONP(cell)) {
            /*
             * Since continuation object is not so many, destructing the object by
             * function call will not cost high. This function interface makes
             * continuation module substitution easy without preparing
             * module-specific header file which contains the module-specific
             * destruction macro.
             */
            Scm_DestructContinuation(cell);
        }
    }
#else /* SCM_OBJ_COMPACT */
    switch (SCM_TYPE(cell)) {
    case ScmCons:
    case ScmInt:
    case ScmChar:
    case ScmClosure:
        break;

    case ScmString:
        if (SCM_STRING_STR(cell))
            free(SCM_STRING_STR(cell));
        break;

    case ScmVector:
        if (SCM_VECTOR_VEC(cell))
            free(SCM_VECTOR_VEC(cell));
        break;

    case ScmSymbol:
        if (SCM_SYMBOL_NAME(cell))
            free(SCM_SYMBOL_NAME(cell));
        break;

    case ScmPort:
        if (SCM_PORT_IMPL(cell))
            SCM_PORT_CLOSE_IMPL(cell);
        break;

    /* rarely swept objects */
    case ScmContinuation:
        /*
         * Since continuation object is not so many, destructing the object by
         * function call will not cost high. This function interface makes
         * continuation module substitution easy without preparing
         * module-specific header file which contains the module-specific
         * destruction macro.
         */
        Scm_DestructContinuation(cell);
        break;

    case ScmFunc:
    case ScmConstant:
    case ScmFreeCell:
    default:
        break;
    }
#endif /* SCM_OBJ_COMPACT */
}

static void gc_sweep(void)
{
    int i, corrected_obj_num;
    ScmObjHeap heap;
    ScmCell *cell;
    ScmObj obj, new_freelist;

    new_freelist = scm_freelist; /* freelist remains on manual GC */

    /* iterate heaps */
    for (i = 0; i < scm_heap_num; i++) {
        corrected_obj_num = 0;
        heap = scm_heaps[i];

        /* iterate in heap */
        for (cell = &heap[0]; cell < &heap[SCM_HEAP_SIZE]; cell++) {
            /* FIXME: is this safe for SCM_OBJ_COMPACT? */
            obj = (ScmObj)cell;

            if (SCM_IS_MARKED(obj)) {
                SCM_DO_UNMARK(obj);
            } else {
                free_cell(cell);

                SCM_ENTYPE_FREECELL(obj);
                SCM_FREECELL_SET_CAR(obj, SCM_NULL);
                SCM_FREECELL_SET_CDR(obj, new_freelist);
                new_freelist = obj;
                corrected_obj_num++;
            }
        }

        CDBG((SCM_DBG_GC, "heap[%d] swept = %d", i, corrected_obj_num));
    }
    scm_freelist = new_freelist;
}
