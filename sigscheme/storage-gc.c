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
 * The GC performs conservative mark-and-sweep.
 *
 * [1] Mark phase : gc_mark()
 *   - gc_mark_locations()
 *       marks Scheme objects that held in the registers.
 *
 *   - gc_mark_locations()
 *       marks Scheme objects that located on the stack.
 *
 *   - gc_mark_protected_var()
 *       marks Scheme objects held in off-heap locations that registered by
 *       SigScm_GC_Protect().
 *
 *   - gc_mark_definite_locations_n()
 *       marks Scheme objects that held in the symbol table.
 *
 * [2] Sweep phase : gc_sweep()
 *   - collects unmarked objects on heaps into the freelist.
 */

/*=======================================
  System Include
=======================================*/
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>

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

/*=======================================
  File Local Macro Declarations
=======================================*/
#if (!SCM_OBJ_COMPACT && !SCM_USE_STORAGE_ABSTRACTION_LAYER)
#define SCM_UNMARKER          0
#define SCM_MARKER            (SCM_UNMARKER + 1)
#define SCM_IS_MARKED(a)      (SCM_MARK(a) == SCM_MARKER)
#define SCM_IS_UNMARKED(a)    (!SCM_IS_MARKED(a))
#define SCM_DO_MARK(a)        (SCM_MARK(a) = SCM_MARKER)
#define SCM_DO_UNMARK(a)      (SCM_MARK(a) = SCM_UNMARKER)
#endif

/*=======================================
  Variable Declarations
=======================================*/
static size_t heap_size, heap_alloc_threshold;
static int n_heaps, n_heaps_max;
static ScmObjHeap *heaps;
static ScmCell *heaps_lowest, *heaps_highest;
static ScmObj freelist;

static jmp_buf save_regs_buf;
static ScmObj *stack_start_pointer;
#if UIM_SCM_GCC4_READY_GC
/* See also the comment about these variables in sigscheme.h */
ScmObj *(*volatile scm_gc_protect_stack)(ScmObj *)
    = &SigScm_GC_ProtectStackInternal;
#endif /* UIM_SCM_GCC4_READY_GC */

static ScmObj **protected_vars;
static size_t protected_vars_size, n_empty_protected_vars;

/* storage-symbol.c */
extern ScmObj *scm_symbol_hash;

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj **locate_protected_var(ScmObj *var);

static void initialize_heap(size_t size, size_t alloc_threshold,
                            int n_max, int n_init);
static void add_heap(void);
static void finalize_heap(void);

static void gc_mark_and_sweep(void);

/* GC Mark Related Functions */
static void mark_obj(ScmObj obj);
static int  within_heapp(ScmObj obj);

static void gc_mark_protected_var();
static void gc_mark_locations_n(ScmObj *start, size_t n);
static void gc_mark_definite_locations_n(ScmObj *start, size_t n);
static void gc_mark_locations(ScmObj *start, ScmObj *end);
static void gc_mark(void);

/* GC Sweep Related Functions */
static void free_cell(ScmCell *cell);
static size_t gc_sweep(void);

static void finalize_protected_var(void);

/*=======================================
  Function Implementations
=======================================*/
void SigScm_InitGC(size_t heap_size, size_t heap_alloc_threshold,
                   int n_heaps_max, int n_heaps_init)
{
    stack_start_pointer = NULL;
    protected_vars = NULL;
    protected_vars_size = 0;
    n_empty_protected_vars = 0;
    initialize_heap(heap_size, heap_alloc_threshold, n_heaps_max, n_heaps_init);
}

void SigScm_FinalizeGC(void)
{
    finalize_heap();
    finalize_protected_var();
}

ScmObj SigScm_NewObjFromHeap(void)
{
    ScmObj ret = SCM_FALSE;

    if (NULLP(freelist))
        gc_mark_and_sweep();

    ret = freelist;
    freelist = SCM_FREECELL_NEXT(freelist);

    return ret;
}

/*============================================================================
  ScmObj Protection
============================================================================*/
/*
 * Registered veriable locations are held in vector instead of linked list to
 * maximize space and performance efficiency.
 */
static ScmObj **locate_protected_var(ScmObj *var)
{
    ScmObj **slot;

    if (protected_vars) {
        for (slot = protected_vars;
             slot < &protected_vars[protected_vars_size];
             slot++)
        {
            if (*slot == var)
                return slot;
        }
    }

    return NULL;
}

void SigScm_GC_Protect(ScmObj *var)
{
    ScmObj **slot;
    size_t new_size;

    if (n_empty_protected_vars) {
        slot = locate_protected_var(NULL);
        n_empty_protected_vars--;
    } else {
        new_size = sizeof(ScmObj *) * (protected_vars_size + 1);
        protected_vars = scm_realloc(protected_vars, new_size);
        slot = &protected_vars[protected_vars_size++];
    }
    *slot = var;
}

void SigScm_GC_Unprotect(ScmObj *var)
{
    ScmObj **slot;

    slot = locate_protected_var(var);
    if (slot) {
        *slot = NULL;
        n_empty_protected_vars++;
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
static void initialize_heap(size_t size, size_t alloc_threshold,
                            int n_max, int n_init)
{
    int i;

    heap_size = size;
    heap_alloc_threshold = alloc_threshold;
    n_heaps_max = n_max;
    n_heaps = 0;
    heaps = NULL;
    heaps_lowest = heaps_highest = NULL;
    freelist = SCM_NULL;

    /* preallocate heaps */
    for (i = 0; i < n_init; i++)
        add_heap();
}

static void add_heap(void)
{
    ScmObjHeap heap;
    ScmCell *cell;

    CDBG((SCM_DBG_GC, "add_heap current num of heaps:%d", n_heaps));

    if (n_heaps_max <= n_heaps)
        Scm_FatalError("heap exhausted");

    heaps = scm_realloc(heaps, sizeof(ScmObjHeap) * (n_heaps + 1));
    heap = scm_malloc_aligned(sizeof(ScmCell) * heap_size);
    heaps[n_heaps++] = heap;

    /* update the enclosure */
    if (heaps_highest < &heap[heap_size])
        heaps_highest = &heap[heap_size];
    if (&heap[0] < heaps_lowest)
        heaps_lowest = &heap[0];

    /* link in order */
    for (cell = &heap[0]; cell < &heap[heap_size - 1]; cell++)
        SCM_RECLAIM_CELL(cell, cell + 1);
    SCM_RECLAIM_CELL(cell, freelist);
    freelist = heap;
}

static void finalize_heap(void)
{
    int i;
    ScmCell *cell;
    ScmObjHeap heap;

    for (i = 0; i < n_heaps; i++) {
        heap = heaps[i];
        for (cell = &heap[0]; cell < &heap[heap_size]; cell++)
            free_cell(cell);
        free(heap);
    }
    free(heaps);
}

static void gc_mark_and_sweep(void)
{
    size_t n_collected;

    CDBG((SCM_DBG_GC, "[ gc start ]"));

    gc_mark();
    n_collected = gc_sweep();

    if (n_collected < heap_alloc_threshold) {
        CDBG((SCM_DBG_GC, "Cannot sweep the object, allocating new heap."));
        add_heap();
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
    free(protected_vars);
}

/* The core part of Conservative GC */

static int within_heapp(ScmObj obj)
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
    /*
     * Reject by rough conditions:
     * - heaps must be aligned to sizeof(ScmCell)
     * - ptr is pointing to outside the enclosure which contain all heaps
     */
    if (((uintptr_t)ptr % sizeof(ScmCell))
        || (ptr < heaps_lowest || heaps_highest <= ptr))
        return 0;

    for (i = 0; i < n_heaps; i++) {
        heap = heaps[i];
        if (heap && &heap[0] <= ptr && ptr < &heap[heap_size])
            return 1;
    }

    return 0;
}

static void gc_mark_protected_var(void)
{
    ScmObj **slot;

    if (protected_vars) {
        for (slot = protected_vars;
             slot < &protected_vars[protected_vars_size];
             slot++)
        {
            if (*slot)
                mark_obj(**slot);
        }
    }
}

/* mark a contiguous region such as stack */
static void gc_mark_locations_n(ScmObj *start, size_t n)
{
    ScmObj *objp;

    for (objp = start; objp < &start[n]; objp++) {
        if (within_heapp(*objp))
            mark_obj(*objp);
    }
}

static void gc_mark_definite_locations_n(ScmObj *start, size_t n)
{
    ScmObj *objp;

    for (objp = start; objp < &start[n]; objp++)
        mark_obj(*objp);
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

/*
 * To avoid incorrect stack_end placement, the jmp_buf is allocated to outside
 * of stack
 */
static void gc_mark(void)
{
    ScmObj stack_end;
    void *save_regs_buf_end = (char *)save_regs_buf + sizeof(save_regs_buf);

    setjmp(save_regs_buf);
    gc_mark_locations((ScmObj *)save_regs_buf, (ScmObj *)save_regs_buf_end);
    gc_mark_locations(stack_start_pointer, &stack_end);

    /* performed after above two because of cache pollution */
    gc_mark_protected_var();
    if (scm_symbol_hash)
        gc_mark_definite_locations_n(scm_symbol_hash, NAMEHASH_SIZE);
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

static size_t gc_sweep(void)
{
    int i;
    size_t sum_collected, n_collected;
    ScmObjHeap heap;
    ScmCell *cell;
    ScmObj obj, new_freelist;

    new_freelist = freelist; /* freelist remains on manual GC */

    sum_collected = 0;
    for (i = 0; i < n_heaps; i++) {
        n_collected = 0;
        heap = heaps[i];

        for (cell = &heap[0]; cell < &heap[heap_size]; cell++) {
            /* FIXME: is this safe for SCM_OBJ_COMPACT? */
            obj = (ScmObj)cell;

            if (SCM_IS_MARKED(obj)) {
                SCM_DO_UNMARK(obj);
            } else {
                free_cell(cell);
                SCM_RECLAIM_CELL(cell, new_freelist);
                new_freelist = obj;
                n_collected++;
            }
        }

        sum_collected += n_collected;
        CDBG((SCM_DBG_GC, "heap[%d] swept = %d", i, n_collected));
    }
    freelist = new_freelist;

    return sum_collected;
}
