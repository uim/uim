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
 *   - gc_mark_protected_obj()
 *       marking protected Scheme object which are protected by calling SigScm_gc_protect().
 *
 *   - gc_mark_stack()
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
#include <malloc.h>

#ifdef USE_BOEHM_GC
  #include <gc/gc.h>
#endif

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"

/*=======================================
  File Local Struct Declarations
=======================================*/
typedef ScmObj ScmObjHeap;

/* Represent protected from GC object */
typedef struct gc_protected_obj_ gc_protected_obj;
struct gc_protected_obj_ {
    ScmObj obj;
    gc_protected_obj *next_obj;
};

/*=======================================
  File Local Macro Declarations
=======================================*/
#define NAMEHASH_SIZE 1024

#ifdef USE_BOEHM_GC

#define SCM_NEW_OBJ_INTERNAL(VALNAME)                                   \
  VALNAME = GC_MALLOC(sizeof(ScmObjInternal));

#else

#define SCM_NEW_OBJ_INTERNAL(VALNAME)                                   \
    if (EQ(scm_freelist, SCM_NIL))					\
	gc_mark_and_sweep();						\
    VALNAME = scm_freelist;						\
    scm_freelist = SCM_FREECELL_CDR(scm_freelist);			\

#endif

/*=======================================
  Variable Declarations
=======================================*/
static int           SCM_HEAP_SIZE = 16384;
static int           scm_heap_num  = 64;
static ScmObjHeap   *scm_heaps     = NULL;
static ScmObj        scm_freelist  = NULL;

ScmObj *stack_start_pointer = NULL;


static ScmObj *symbol_hash = NULL;
static gc_protected_obj *protected_obj_list = NULL;

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

static void gc_mark_protected_obj();
static void gc_mark_stack(ScmObj *start, ScmObj *end);
static void gc_mark(void);

/* GC Sweep Related Functions */
static void sweep_obj(ScmObj obj);
static void gc_sweep(void);

static void initialize_symbol_hash(void);
static void finalize_symbol_hash(void);
static int  symbol_name_hash(const char *name);

/*=======================================
  Function Implementations
=======================================*/
void SigScm_InitStorage(void)
{
#ifdef USE_BOEHM_GC
  GC_enable_incremental();
#endif

  allocate_heap(&scm_heaps, scm_heap_num, SCM_HEAP_SIZE, &scm_freelist);
  initialize_symbol_hash();
}

void SigScm_FinalizeStorage(void)
{
#ifdef USE_BOEHM_GC
  /*    GC_invoke_finalizers(); */
#else 
    finalize_heap();
    finalize_symbol_hash();
#endif
}

static void *malloc_aligned(size_t size)
{

#ifdef USE_BOEHM_GC
  return  GC_MALLOC(size);

#else

    /* TODO : Need to reserch System Dependency! */
    void *p;
    posix_memalign(&p, 16, size);
    return p;

#endif

}


static void allocate_heap(ScmObjHeap **heaps, int num_heap, int HEAP_SIZE, ScmObj *freelist)
{
    int i = 0;
    int j = 0;
    ScmObj prev = NULL;
    ScmObj next = NULL;

#ifdef USE_BOEHM_GC
    return;
#endif

#if DEBUG_GC
    printf("allocate_heap num:%d size:%d\n", num_heap, HEAP_SIZE);
#endif

    /* allocate heap */
    (*heaps) = (ScmObj*)malloc(sizeof(ScmObj) * num_heap);
    (*freelist) = SCM_NIL;

    /* fill with zero and construct free_list */
    for (i = 0; i < num_heap; i++) {
        /* Initialize Heap */
        (*heaps)[i] = (ScmObj)malloc_aligned(sizeof(ScmObjInternal) * HEAP_SIZE);
        memset((*heaps)[i], 0, sizeof(ScmObjInternal) * HEAP_SIZE);

        /* link in order */
        prev = NULL;
        next = NULL;
        for (j = 0; j < HEAP_SIZE; j++) {
            next = &(*heaps)[i][j];
	    SCM_SETFREECELL(next);

	    /* prev's cdr is next */
	    if (prev)
		SCM_SETFREECELL_CDR(prev, next);

            /* the last cons' cdr is freelist */
            if (j == HEAP_SIZE - 1)
		SCM_SETFREECELL_CDR(next, (*freelist));

            prev = next;
        }

	/* and freelist is head of the heap */
	(*freelist) = (*heaps)[i];
    }
}

static void add_heap(ScmObjHeap **heaps, int *orig_num_heap, int HEAP_SIZE, ScmObj *freelist)
{
    int    i = 0;
    int    num_heap = 0;
    ScmObj prev     = NULL;
    ScmObj next     = NULL;

#if DEBUG_GC
    printf("add_heap current num of heaps:%d\n", *orig_num_heap);
#endif

    /* increment num_heap */
    (*orig_num_heap) += 1;
    num_heap = (*orig_num_heap);

    /* add heap */
    (*heaps) = (ScmObj*)realloc((*heaps), sizeof(ScmObj) * num_heap);

    /* allocate heap */
    (*heaps)[num_heap - 1] = (ScmObj)malloc_aligned(sizeof(ScmObjInternal) * HEAP_SIZE);
    memset((*heaps)[num_heap - 1], 0, sizeof(ScmObjInternal) * HEAP_SIZE);
    
    /* link in order */
    for (i = 0; i < HEAP_SIZE; i++) {
        next = &(*heaps)[num_heap - 1][i];
	SCM_SETFREECELL(next);

        if (prev)
	    SCM_SETFREECELL_CDR(prev, next);

        /* the last cons' cdr is freelist */
        if (i == HEAP_SIZE - 1)
	    SCM_SETFREECELL_CDR(next, (*freelist));

        prev = next;
    }

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
    /* Initialize Mark Table */
    int  i = 0;
    long j = 0;
    for (i = 0; i < scm_heap_num; i++) {
	for (j = 0; j < SCM_HEAP_SIZE; j++) {
	    SCM_DO_UNMARK(&scm_heaps[i][j]);
	}
    }
}

static void gc_mark_and_sweep(void)
{
#if DEBUG_GC
    printf("[ gc start ]\n");
#endif

#ifdef USE_BOEHM_GC

    return;
#endif

    gc_preprocess();
    gc_mark();
    gc_sweep();
    
    /* we cannot sweep the object, so let's add new heap */
    if (SCM_NULLP(scm_freelist)) {
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
    /* no need to mark SCM_NIL */
    if (SCM_NULLP(obj))
        return;

    /* avoid cyclic marking */
    if (SCM_IS_MARKED(obj))
        return;

    /* mark this object */
    SCM_DO_MARK(obj);

    /* mark recursively */
    switch (SCM_GETTYPE(obj)) {
        case ScmCons:
            mark_obj(SCM_CAR(obj));
	    obj = SCM_CDR(obj);
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
	case ScmVector:
	    for (i = 0; i < SCM_VECTOR_LEN(obj); i++) {
		mark_obj(SCM_VECTOR_VEC(obj)[i]);
	    }
	    break;
	default:
	    break;
    }
}

void SigScm_gc_protect(ScmObj obj)
{
    gc_protected_obj *item = (gc_protected_obj*)malloc(sizeof(gc_protected_obj));
    item->obj = obj;

    if (protected_obj_list) {
        item->next_obj = protected_obj_list;
        protected_obj_list = item;
    } else {
        protected_obj_list = item;
        protected_obj_list->next_obj = NULL; /* null terminated */
    }
}

static int is_pointer_to_heap(ScmObj obj)
{
    /* The core part of Conservative GC */
    int i;
    ScmObj head = SCM_NIL;
    for (i = 0; i < scm_heap_num; i++) {
	if ((head = scm_heaps[i])
	    && (head <= obj)
	    && (obj  <  head + SCM_HEAP_SIZE)
	    && ((((char*)obj - (char*)head) % sizeof(ScmObj)) == 0))
	    return 1;
    }

    return 0;
}

static void gc_mark_protected_obj(void)
{
    gc_protected_obj *item;
    for (item = protected_obj_list; item; item = item->next_obj) {
        mark_obj(item->obj);
    }
}

static void gc_mark_stack(ScmObj *start, ScmObj *end)
{
    int i    = 0;
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

    printf("gc_mark_stack() : size = %d\n", size);

    /* mark stack */
    for (i = 0; i < size; i++) {
        if (is_pointer_to_heap(start[i])) {
            mark_obj(start[i]);
        }
    }
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
    ScmObj obj;

#if DEBUG_GC
    printf("gc_mark\n");
#endif

    gc_mark_protected_obj();
    gc_mark_stack(stack_start_pointer, &obj);
    gc_mark_symbol_hash();
}

static void sweep_obj(ScmObj obj)
{
    /* if the type has the pointer to free, then free it! */
    switch (SCM_GETTYPE(obj)) {
        case ScmInt:
        case ScmCons:
        case ScmFunc:
        case ScmClosure:
        case ScmFreeCell:
        case ScmEtc:
	  break;
	case ScmChar:
	    if (SCM_CHAR_CH(obj)) {
		free(SCM_CHAR_CH(obj));
	    }
	    break;
        case ScmString:
	    if (SCM_STRING_STR(obj)){
		free(SCM_STRING_STR(obj));
	    }
            break;
	case ScmVector:
	    if (SCM_VECTOR_VEC(obj)) {
		free(SCM_VECTOR_VEC(obj));
	    }
	    break;
	case ScmSymbol:
	    if (SCM_SYMBOL_NAME(obj)) {
		free(SCM_SYMBOL_NAME(obj));
	    }
	    break;
	case ScmPort:
	    if (SCM_PORT_PORTINFO(obj)) {
		free(SCM_PORT_PORTINFO(obj));
	    }
	    break;
	case ScmContinuation:
	    if (SCM_CONTINUATION_CONTINFO(obj)) {
		free(SCM_CONTINUATION_CONTINFO(obj));
	    }
	default:
	    break;
    }
}

static void gc_sweep(void)
{
    int i = 0;
    int j = 0;
    int corrected_obj_num = 0;

    ScmObj obj = SCM_NIL;
    ScmObj scm_new_freelist = SCM_NIL;
    /* iterate heaps */
    for (i = 0; i < scm_heap_num; i++) {
	corrected_obj_num = 0;
	
	/* iterate in heap */
	for (j = 0; j < SCM_HEAP_SIZE; j++) {
	    obj = &scm_heaps[i][j];
	    if (!SCM_IS_MARKED(obj)) {
		sweep_obj(obj);

		SCM_SETFREECELL(obj);
		SCM_SETFREECELL_CAR(obj, SCM_NIL);
		SCM_SETFREECELL_CDR(obj, scm_new_freelist);
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

void SigScm_gc_protect_stack(ScmObj *stack_start)
{
    if (!stack_start_pointer)
	stack_start_pointer = stack_start;
}

void SigScm_gc_unprotect_stack(ScmObj *stack_start)
{
    if (stack_start_pointer == stack_start)
	stack_start_pointer = NULL;
}

/*===========================================================================
  Allocate Structure Functions
===========================================================================*/
ScmObj Scm_NewCons(ScmObj a, ScmObj b)
{
    ScmObj obj = SCM_NIL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETCONS(obj);
    SCM_SETCAR(obj, a);
    SCM_SETCDR(obj, b);

    return obj;
}

ScmObj Scm_NewInt(int val)
{
    ScmObj obj = SCM_NIL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETINT(obj);
    SCM_SETINT_VALUE(obj, val);

    return obj;
}

ScmObj Scm_NewSymbol(char *name, ScmObj v_cell)
{
    ScmObj obj = SCM_NIL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETSYMBOL(obj);
    SCM_SETSYMBOL_NAME(obj, name);
    SCM_SETSYMBOL_VCELL(obj, v_cell);

    return obj;
}

ScmObj Scm_NewChar(char *ch)
{
    ScmObj obj = SCM_NIL;

    /* check length */
    if (SigScm_default_encoding_strlen(ch) != 1) {
	printf("ch = [%s], len = %d\n", ch, SigScm_default_encoding_strlen(ch));
	SigScm_Error("invalid character\n");
    }

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETCHAR(obj);
    SCM_SETCHAR_CH(obj, ch);

    return obj;
}

ScmObj Scm_NewString(char *str)
{
    ScmObj obj = SCM_NIL;

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETSTRING(obj);
    SCM_SETSTRING_STR(obj, str);
    SCM_SETSTRING_LEN(obj, SigScm_default_encoding_strlen(str));

    return obj;
}

ScmObj Scm_NewStringCopying(char *str)
{
    ScmObj obj = SCM_NIL;

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETSTRING(obj);
    SCM_STRING_STR(obj) = (char *)malloc(sizeof(char) * strlen(str) + 1);
    strcpy(SCM_STRING_STR(obj), str);
    SCM_SETSTRING_LEN(obj, SigScm_default_encoding_strlen(str));

    return obj;
}

ScmObj Scm_NewString_With_StrLen(char *str, int len)
{
    ScmObj obj = SCM_NIL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETSTRING(obj);
    SCM_SETSTRING_STR(obj, str);
    SCM_SETSTRING_LEN(obj, len);

    return obj;
}

ScmObj Scm_NewFunc(enum ScmFuncArgNum num_arg, ScmFuncType func)
{
    ScmObj obj = SCM_NIL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETFUNC(obj);
    SCM_SETFUNC_NUMARG(obj, num_arg);
    SCM_SETFUNC_FUNC(obj, func);

    return obj;
}

ScmObj Scm_NewClosure(ScmObj exp, ScmObj env)
{
    ScmObj obj = SCM_NIL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETCLOSURE(obj);
    SCM_SETCLOSURE_EXP(obj, exp);
    SCM_SETCLOSURE_ENV(obj, env);

    return obj;
}

ScmObj Scm_NewVector(ScmObj *vec, int len)
{
    ScmObj obj = SCM_NIL;
    SCM_NEW_OBJ_INTERNAL(obj);
    
    SCM_SETVECTOR(obj);
    SCM_SETVECTOR_VEC(obj, vec);
    SCM_SETVECTOR_LEN(obj, len);

    return obj;
}

ScmObj Scm_NewPort(FILE *file, enum ScmPortDirection pdirection, enum ScmPortType ptype)
{
    ScmObj obj = SCM_NIL;
    ScmPortInfo *pinfo = (ScmPortInfo *)malloc(sizeof(ScmPortInfo));;

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETPORT(obj);
    SCM_SETPORT_PORTDIRECTION(obj, pdirection);
    switch (ptype) {
	case PORT_FILE:
	    {

		pinfo->file         = file;
		pinfo->line         = 0;
		pinfo->ungottenchar = 0;
	    }
	    break;
	case PORT_STRING:
	    {
		/* TODO : implemented this immediately! */
	    }
	    break;
	default:
	    SigScm_Error("Scm_NewPort : invalid port type\n");
	    break;
    }
    SCM_SETPORT_PORTINFO(obj, pinfo);

    return obj;
}

ScmObj Scm_NewContinuation(void)
{
    ScmObj obj = SCM_NIL;
    ScmContInfo *cinfo = NULL;

    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETCONTINUATION(obj);
    cinfo = (ScmContInfo *)malloc(sizeof(ScmContInfo));
    SCM_SETCONTINUATION_CONTINFO(obj, cinfo);

    return obj;
}

ScmObj Scm_NewCPointer(void *data)
{
    ScmObj obj = SCM_NIL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETC_POINTER(obj);
    SCM_SETC_POINTER_DATA(obj, data);

    return obj;
}

ScmObj Scm_NewCFuncPointer(C_FUNC func)
{
    ScmObj obj = SCM_NIL;
    SCM_NEW_OBJ_INTERNAL(obj);

    SCM_SETC_FUNCPOINTER(obj);
    SCM_SETC_FUNCPOINTER_FUNC(obj, func);

    return obj;
}

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
        symbol_hash[i] = SCM_NIL;
    
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
    ScmObj sym      = SCM_NIL;
    ScmObj list     = SCM_NIL;
    ScmObj sym_list = symbol_hash[n];
    char  *symname  = (char*)malloc(strlen(name) + 1);

    /* copy string */
    strcpy(symname, name);

    /* Search Symbol by name */
    list = sym_list;
    for (; !SCM_NULLP(list); list = SCM_CDR(list)) {
        sym = SCM_CAR(list);

        if (strcmp(SCM_SYMBOL_NAME(sym), name) == 0) {
            free(symname);
            return sym;
        }
    }

    /* If not in the sym_list, allocate new Symbol */
    sym = Scm_NewSymbol(symname, SCM_UNBOUND);

    /* And Append it to the head of symbol_hash */
    sym_list = Scm_NewCons(sym, sym_list);
    symbol_hash[n] = sym_list;

    return sym;
}

int Scm_GetInt(ScmObj num)
{
    if (EQ(ScmOp_numberp(num), SCM_FALSE))
	SigScm_ErrorObj("Scm_GetInt : number required but got ", num);

    return SCM_INT_VALUE(num);
}

char* Scm_GetString(ScmObj str)
{
    char *ret = NULL;
    switch (SCM_GETTYPE(str)) {
	case ScmString:
	    ret = SCM_STRING_STR(str);
	    break;
	case ScmSymbol:
	    ret = SCM_SYMBOL_NAME(str);
	    break;
	default:
	    SigScm_Error("Scm_GetString : cannot get string of not string nor symbol\n");
    }

    return ret;
}

void* Scm_GetCPointer(ScmObj c_ptr)
{
    if (!SCM_C_POINTERP(c_ptr))
	SigScm_ErrorObj("Scm_GetCPointer : c_ptr required but got ", c_ptr);

    return SCM_C_POINTER_DATA(c_ptr);
}

C_FUNC Scm_GetCFuncPointer(ScmObj c_funcptr)
{
    if (!SCM_C_FUNCPOINTERP(c_funcptr))
	SigScm_ErrorObj("Scm_GetCFuncPointer : c_funcptr required but got ", c_funcptr);

    return SCM_C_FUNCPOINTER_FUNC(c_funcptr);
}
