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
#define NAMEHASH_SIZE 1024

#define SCM_NEW_OBJ_INTERNAL(VALNAME)                                   \
    if (EQ(scm_freelist, SCM_NULL))                                      \
        gc_mark_and_sweep();                                            \
    VALNAME = scm_freelist;                                             \
    scm_freelist = SCM_FREECELL_CDR(scm_freelist);                      \

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

static ScmObj *symbol_hash = NULL;
static gc_protected_var *protected_var_list = NULL;

#if SCM_COMPAT_SIOD
ScmObj scm_return_value    = NULL;
#endif

/*=======================================
  File Local Function Declarations
=======================================*/
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

static void initialize_symbol_hash(void);
static void finalize_symbol_hash(void);
static int  symbol_name_hash(const char *name);

static void finalize_protected_var(void);

/*=======================================
  Function Implementations
=======================================*/
void SigScm_InitStorage(void)
{
  allocate_heap(&scm_heaps, scm_heap_num, SCM_HEAP_SIZE, &scm_freelist);
  initialize_symbol_hash();
}

void SigScm_FinalizeStorage(void)
{
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


static void allocate_heap(ScmObjHeap **heaps, int num_heap, int HEAP_SIZE, ScmObj *freelist)
{
    int i = 0;
    ScmObj heap, cell;

#if DEBUG_GC
    printf("allocate_heap num:%d size:%d\n", num_heap, HEAP_SIZE);
#endif

    /* allocate heap */
    (*heaps) = (ScmObj*)malloc(sizeof(ScmObj) * num_heap);
    (*freelist) = SCM_NULL;

    /* fill with zero and construct free_list */
    for (i = 0; i < num_heap; i++) {
        /* Initialize Heap */
        heap = (ScmObj)malloc_aligned(sizeof(ScmObjInternal) * HEAP_SIZE);
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

#if DEBUG_GC
    printf("add_heap current num of heaps:%d\n", *orig_num_heap);
#endif

    /* increment num_heap */
    (*orig_num_heap) += 1;
    num_heap = (*orig_num_heap);

    /* add heap */
    (*heaps) = (ScmObj*)realloc((*heaps), sizeof(ScmObj) * num_heap);

    /* allocate heap */
    heap = (ScmObj)malloc_aligned(sizeof(ScmObjInternal) * HEAP_SIZE);
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
#if DEBUG_GC
    printf("[ gc start ]\n");
#endif

    gc_preprocess();
    gc_mark();
    gc_sweep();

    /* we cannot sweep the object, so let's add new heap */
    if (NULLP(scm_freelist)) {
#if DEBUG_GC
      printf("Cannot sweeped the object, allocating new heap.\n");
#endif
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
        obj = SCM_VALUEPACKET_VALUES(obj);
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

static void finalize_protected_var(void)
{
    gc_protected_var *item = protected_var_list;
    gc_protected_var *tmp  = NULL;
    while (item) {
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
            && ((((char*)obj - (char*)head) % sizeof(ScmObjInternal)) == 0))
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

#if DEBUG_GC
    printf("gc_mark_locations() : size = %d\n", size);
#endif

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

#if DEBUG_GC
    printf("gc_mark\n");
#endif

    setjmp(save_regs_buf);
    gc_mark_locations((ScmObj*)save_regs_buf,
                      (ScmObj*)(((char*)save_regs_buf) + sizeof(save_regs_buf)));

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
    case ScmFunc:
    case ScmClosure:
    case ScmFreeCell:
    case ScmEtc:
        break;

    case ScmChar:
        if (SCM_CHAR_VALUE(obj)) free(SCM_CHAR_VALUE(obj));
        break;

    case ScmString:
        if (SCM_STRING_STR(obj)) free(SCM_STRING_STR(obj));
        break;

    case ScmVector:
        if (SCM_VECTOR_VEC(obj)) free(SCM_VECTOR_VEC(obj));
        break;

    case ScmSymbol:
        if (SCM_SYMBOL_NAME(obj)) free(SCM_SYMBOL_NAME(obj));
        break;

    case ScmPort:
        /* handle each port type */
        switch (SCM_PORTINFO_PORTTYPE(obj)) {
        case PORT_FILE:
            if (SCM_PORTINFO_FILENAME(obj)) free(SCM_PORTINFO_FILENAME(obj));
            break;
        case PORT_STRING:
            if (SCM_PORTINFO_STR(obj)) free(SCM_PORTINFO_STR(obj));
            break;
        }
        /* free port info */
        if (SCM_PORT_PORTINFO(obj)) free(SCM_PORT_PORTINFO(obj));
        break;

    case ScmContinuation:
        /* free continuation info */
        if (SCM_CONTINUATION_CONTINFO(obj)) free(SCM_CONTINUATION_CONTINFO(obj));
        break;

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

#if DEBUG_GC
        printf("scm[%d] sweeped = %d\n", i, corrected_obj_num);
#endif
    }
    scm_freelist = scm_new_freelist;
}

void SigScm_GC_ProtectStack(ScmObj *stack_start)
{
    if (!scm_stack_start_pointer)
        scm_stack_start_pointer = stack_start;
}

void SigScm_GC_UnprotectStack(ScmObj *stack_start)
{
    if (scm_stack_start_pointer == stack_start)
        scm_stack_start_pointer = NULL;
}

/*===========================================================================
  Allocate Structure Functions
===========================================================================*/
ScmObj Scm_NewCons(ScmObj a, ScmObj b)
{
    ScmObj obj = SCM_NULL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_CONS(obj);
    SET_CAR(obj, a);
    SET_CDR(obj, b);

    return obj;
}

ScmObj Scm_NewInt(int val)
{
    ScmObj obj = SCM_NULL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_INT(obj);
    SCM_INT_SET_VALUE(obj, val);

    return obj;
}

ScmObj Scm_NewSymbol(char *name, ScmObj v_cell)
{
    ScmObj obj = SCM_NULL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_SYMBOL(obj);
    SCM_SYMBOL_SET_NAME(obj, name);
    SCM_SYMBOL_SET_VCELL(obj, v_cell);

    return obj;
}

ScmObj Scm_NewChar(char *ch)
{
    ScmObj obj = SCM_NULL;

    /* check length */
    if (SigScm_default_encoding_strlen(ch) != 1) {
        printf("ch = [%s], len = %d\n", ch, SigScm_default_encoding_strlen(ch));
        SigScm_Error("invalid character\n");
    }

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_CHAR(obj);
    SCM_CHAR_SET_VALUE(obj, ch);

    return obj;
}

ScmObj Scm_NewString(char *str)
{
    ScmObj obj = SCM_NULL;

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_STRING(obj);
    SCM_STRING_SET_STR(obj, str);
    SCM_STRING_SET_LEN(obj, SigScm_default_encoding_strlen(str));

    return obj;
}

ScmObj Scm_NewStringCopying(const char *str)
{
    ScmObj obj = SCM_NULL;

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_STRING(obj);
    SCM_STRING_STR(obj) = (char *)malloc(sizeof(char) * strlen(str) + 1);
    strcpy(SCM_STRING_STR(obj), str);
    SCM_STRING_SET_LEN(obj, SigScm_default_encoding_strlen(str));

    return obj;
}

ScmObj Scm_NewStringWithLen(char *str, int len)
{
    ScmObj obj = SCM_NULL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_STRING(obj);
    SCM_STRING_SET_STR(obj, str);
    SCM_STRING_SET_LEN(obj, len);

    return obj;
}

ScmObj Scm_NewFunc(enum ScmFuncTypeCode type, ScmFuncType func)
{
    ScmObj obj = SCM_NULL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_FUNC(obj);
    SCM_FUNC_SET_TYPECODE(obj, type);
    SCM_FUNC_SET_CFUNC(obj, func);

    return obj;
}

ScmObj Scm_NewClosure(ScmObj exp, ScmObj env)
{
    ScmObj obj = SCM_NULL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_CLOSURE(obj);
    SCM_CLOSURE_SET_EXP(obj, exp);
    SCM_CLOSURE_SET_ENV(obj, env);

    return obj;
}

ScmObj Scm_NewVector(ScmObj *vec, int len)
{
    ScmObj obj = SCM_NULL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_VECTOR(obj);
    SCM_VECTOR_SET_VEC(obj, vec);
    SCM_VECTOR_SET_LEN(obj, len);

    return obj;
}

ScmObj Scm_NewFilePort(FILE *file, const char *filename, enum ScmPortDirection pdirection)
{
    ScmObj obj = SCM_NULL;
    ScmPortInfo *pinfo = (ScmPortInfo *)malloc(sizeof(ScmPortInfo));

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_PORT(obj);
    SCM_PORT_SET_PORTDIRECTION(obj, pdirection);
    pinfo->port_type = PORT_FILE;
    pinfo->info.file_port.file = file;
    pinfo->info.file_port.filename = (char*)malloc(sizeof(char) * strlen(filename) + 1);
    strcpy(pinfo->info.file_port.filename, filename);
    pinfo->info.file_port.line = 0;
    pinfo->ungottenchar = 0;
    SCM_PORT_SET_PORTINFO(obj, pinfo);

    return obj;
}

ScmObj Scm_NewStringPort(const char *str)
{
    ScmObj obj = SCM_NULL;
    ScmPortInfo *pinfo = (ScmPortInfo *)malloc(sizeof(ScmPortInfo));

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_PORT(obj);
    SCM_PORT_SET_PORTDIRECTION(obj, PORT_INPUT);
    pinfo->port_type = PORT_STRING;
    pinfo->info.str_port.port_str = (char *)malloc(strlen(str) + 1);
    strcpy(pinfo->info.str_port.port_str, str);
    pinfo->info.str_port.str_current = pinfo->info.str_port.port_str;
    pinfo->ungottenchar = 0;
    SCM_PORT_SET_PORTINFO(obj, pinfo);

    return obj;
}

ScmObj Scm_NewContinuation(void)
{
    ScmObj obj = SCM_NULL;
    ScmContInfo *cinfo = NULL;

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_CONTINUATION(obj);
    cinfo = (ScmContInfo *)malloc(sizeof(ScmContInfo));
    SCM_CONTINUATION_SET_CONTINFO(obj, cinfo);

    return obj;
}

ScmObj Scm_NewValuePacket(ScmObj values)
{
    ScmObj packet = SCM_NULL;
    SCM_NEW_OBJ_INTERNAL(packet);

    SCM_ENTYPE_VALUEPACKET(packet);
    SCM_VALUEPACKET_SET_VALUES(packet, values);
    return packet;
}

#if SCM_USE_NONSTD_FEATURES
ScmObj Scm_NewCPointer(void *data)
{
    ScmObj obj = SCM_NULL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_C_POINTER(obj);
    SCM_C_POINTER_SET_VALUE(obj, data);

    return obj;
}

ScmObj Scm_NewCFuncPointer(ScmCFunc func)
{
    ScmObj obj = SCM_NULL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_ENTYPE_C_FUNCPOINTER(obj);
    SCM_C_FUNCPOINTER_SET_VALUE(obj, func);

    return obj;
}
#endif /* SCM_USE_NONSTD_FEATURES */

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
    ScmObj sym      = SCM_NULL;
    ScmObj list     = SCM_NULL;
    ScmObj sym_list = symbol_hash[n];
    char  *symname;

    /* Search Symbol by name */
    list = sym_list;
    for (; !NULLP(list); list = CDR(list)) {
        sym = CAR(list);

        if (strcmp(SCM_SYMBOL_NAME(sym), name) == 0) {
            return sym;
        }
    }

    /* If not in the sym_list, allocate new Symbol */
    symname  = (char*)malloc(strlen(name) + 1);
    strcpy(symname, name);
    sym = Scm_NewSymbol(symname, SCM_UNBOUND);

    /* And Append it to the head of symbol_hash */
    sym_list = CONS(sym, sym_list);
    symbol_hash[n] = sym_list;

    return sym;
}

ScmObj Scm_eval_c_string(const char *exp)
{
    ScmObj stack_start = NULL;
    ScmObj str_port    = SCM_NULL;
    ScmObj ret         = SCM_NULL;

    /* start protecting stack */
    SigScm_GC_ProtectStack(&stack_start);

    str_port = Scm_NewStringPort(exp);

    ret = SigScm_Read(str_port);
    ret = EVAL(ret, SCM_NULL);

#if SCM_COMPAT_SIOD
    scm_return_value = ret;
#endif

    /* now no need to protect stack */
    SigScm_GC_UnprotectStack(&stack_start);

    return ret;
}

#if SCM_COMPAT_SIOD
ScmObj Scm_return_value(void)
{
    return scm_return_value;
}
#endif
