
/* Scheme In One Defun, but in C this time.

 *                   COPYRIGHT (c) 1988-1994 BY                             *
 *        PARADIGM ASSOCIATES INCORPORATED, CAMBRIDGE, MASSACHUSETTS.       *
 *        See the source file SLIB.C for more information.                  *

 $Id: siod.h,v 1.3 1999/09/23 23:42:37 yosh Exp $

 */
/* Notice for uim programmer:
 *  There are two types of execution context. Some of siod 
 *  functions can be called only from one of them.
 *  (1) Scheme context
 *       Current execution is under scheme stack.
 *       Executing functions called by scheme interpreter.
 *  (2) C context
 *       No scheme context on the stack
 * Yusuke.
 *
 * The nested Scheme evaluation feature (NESTED_REPL_C_STRING) will
 * remove this limitation. The feature will be enabled by default once
 * tested enough.  -- YamaKen 2004-12-31
 */
#ifndef __SIOD_H__
#define __SIOD_H__

#include "config.h"

#include <stdio.h>

#ifndef NESTED_REPL_C_STRING
#define NESTED_REPL_C_STRING 0
#endif

struct obj
  {
    short gc_mark;
    short type;
    union
      {
	struct
	  {
	    struct obj *car;
	    struct obj *cdr;
	  }
	cons;
	struct
	  {
	    int data;
	  }
	flonum;
	struct
	  {
	    char *pname;
	    struct obj *vcell;
	  }
	symbol;
	struct
	  {
	    char *name;
	    struct obj *(*f) (void);
	  }
	subr0;
	struct
	  {
	    char *name;
	    struct obj *(*f) (struct obj *);
	  }
	subr1;
	struct
	  {
	    char *name;
	    struct obj *(*f) (struct obj *, struct obj *);
	  }
	subr2;
	struct
	  {
	    char *name;
	    struct obj *(*f) (struct obj *, struct obj *, struct obj *);
	  }
	subr3;
	struct
	  {
	    char *name;
	    struct obj *(*f) (struct obj *, struct obj *, struct obj *,
			      struct obj *);
	  }
	subr4;
	struct
	  {
	    char *name;
	    struct obj *(*f) (struct obj *, struct obj *, struct obj *,
			      struct obj *, struct obj *);
	  }
	subr5;
	struct
	  {
	    char *name;
	    struct obj *(*f) (struct obj **, struct obj **);
	  }
	subrm;
	struct
	  {
	    char *name;
	    struct obj *(*f) (void *,...);
	  }
	subr;
	struct
	  {
	    struct obj *env;
	    struct obj *code;
	  }
	closure;
	struct
	  {
	    long dim;
	    char *data;
	  }
	string;
	struct
	  {
	    FILE *f;
	    char *name;
	  }
	c_file;
	struct
	  {
	    void *data;
	  }
	c_pointer;
      }
    storage_as;
#if DEBUG_SCM
    struct obj *dbg_info;	/* cons (fname . line) */
#endif
  };

#define CAR(x) ((*x).storage_as.cons.car)
#define CDR(x) ((*x).storage_as.cons.cdr)
#define PNAME(x) ((*x).storage_as.symbol.pname)
#define VCELL(x) ((*x).storage_as.symbol.vcell)
#define SUBR0(x) (*((*x).storage_as.subr0.f))
#define SUBR1(x) (*((*x).storage_as.subr1.f))
#define SUBR2(x) (*((*x).storage_as.subr2.f))
#define SUBR3(x) (*((*x).storage_as.subr3.f))
#define SUBR4(x) (*((*x).storage_as.subr4.f))
#define SUBR5(x) (*((*x).storage_as.subr5.f))
#define SUBRM(x) (*((*x).storage_as.subrm.f))
#define SUBRF(x) (*((*x).storage_as.subr.f))
#define INTNM(x) ((*x).storage_as.flonum.data)

#define NIL ((struct obj *) 0)
#define EQ(x,y) ((x) == (y))
#define NEQ(x,y) ((x) != (y))
#define NULLP(x) EQ(x,NIL)
#define NNULLP(x) NEQ(x,NIL)

#define TYPE(x) (((x) == NIL) ? 0 : ((*(x)).type))

#define TYPEP(x,y) (TYPE(x) == (y))
#define NTYPEP(x,y) (TYPE(x) != (y))

#define tc_nil    0
#define tc_cons   1
#define tc_intnum 2
#define tc_symbol 3
#define tc_subr_0 4		/* subr with no arg */
#define tc_subr_1 5
#define tc_subr_2 6
#define tc_subr_3 7
#define tc_lsubr  8		/* subr with arbitrarily many args */
#define tc_fsubr  9		/* form (evals args on its own) */
#define tc_msubr  10		/* form with tail call optimization */
#define tc_closure 11
#define tc_free_cell 12
#define tc_string       13
/*#define tc_double_array 14*/
/*#define tc_long_array   15*/
/*#define tc_lisp_array   16*/
#define tc_c_file       17
/*#define tc_byte_array   18*/
#define tc_subr_4 19
#define tc_subr_5 20
#define tc_subr_2n 21
#define tc_user_min 50
#define tc_c_pointer 50
#define tc_user_max 100

#define tc_table_dim 100

typedef struct obj *LISP;
typedef LISP (*SUBR_FUNC) (void);

#define CONSP(x)   TYPEP(x,tc_cons)
#define INTNUMP(x) TYPEP(x,tc_intnum)
#define SYMBOLP(x) TYPEP(x,tc_symbol)
#define STRINGP(x) TYPEP(x,tc_string)
#define POINTERP(x) TYPEP(x,tc_c_pointer)

#define NCONSP(x)   NTYPEP(x,tc_cons)
#define NINTNUMP(x) NTYPEP(x,tc_intnum)
#define NSYMBOLP(x) NTYPEP(x,tc_symbol)
#define NSTRINGP(x) NTYPEP(x,tc_string)

#define TKBUFFERN 5120

static void siod_init (int argc, char **argv, int warnflag, FILE *);
static void siod_quit (void);

static void set_repl_hooks (void (*puts_f) (char *),
			    LISP (*read_f) (void),
			    LISP (*eval_f) (LISP),
			    void (*print_f) (LISP));
static char *get_c_string (LISP x);
static char *get_c_string_dim (LISP x, long *);
static int get_c_int (LISP x);
static long nlength(LISP x);
static void *get_c_pointer (LISP x);

static LISP cons (LISP x, LISP y);
static LISP car (LISP x);
static LISP cdr (LISP x);
static LISP setcar (LISP cell, LISP value);
static LISP intcons (int x);
static LISP eql (LISP x, LISP y);
static LISP symcons (char *pname, LISP vcell);
static LISP symbol_boundp (LISP x, LISP env);
static LISP symbol_value (LISP x, LISP env);
static LISP symbol_to_string (LISP x, LISP env);
static LISP rintern (const char *name);
static LISP closure (LISP env, LISP code);
static LISP ptrcons (void *ptr);

static void init_subr (char *name, long type, SUBR_FUNC fcn);
static void init_subr_0 (char *name, LISP (*fcn) (void));
static void init_subr_1 (char *name, LISP (*fcn) (LISP));
static void init_subr_2 (char *name, LISP (*fcn) (LISP, LISP));
static void init_subr_2n (char *name, LISP (*fcn) (LISP, LISP));
static void init_subr_3 (char *name, LISP (*fcn) (LISP, LISP, LISP));
static void init_subr_4 (char *name, LISP (*fcn) (LISP, LISP, LISP, LISP));
static void init_subr_5 (char *name, LISP (*fcn) (LISP, LISP, LISP, LISP, LISP));
static void init_lsubr (char *name, LISP (*fcn) (LISP));
static void init_fsubr (char *name, LISP (*fcn) (LISP, LISP));
static void init_msubr (char *name, LISP (*fcn) (LISP *, LISP *));

static LISP delq (LISP elem, LISP l);
static void set_eval_hooks (long type, LISP (*fcn) (LISP, LISP *, LISP *));
static LISP leval (LISP x, LISP env);
static LISP symbolconc (LISP args);
static LISP lprin1f (LISP exp, FILE * f);
static LISP lread (LISP);
static LISP lreadtk (char *, long j);
static LISP lreadf (FILE * f);
static LISP require (LISP fname);
static LISP strcons (long length, const char *data);
static LISP equal (LISP, LISP);
static void set_fatal_exit_hook (void (*fcn) (void));
static LISP intern (LISP x);
static void gc_protect (LISP * location);
#if (NESTED_REPL_C_STRING)
static void siod_gc_protect_stack(LISP *stack_start);
static void siod_gc_unprotect_stack(LISP *stack_start);
#else
static int siod_repl_c_string_entered (void);
#endif
static long repl_c_string (const char *, long want_init, long want_print);
static LISP siod_return_value (void);
static LISP reverse (LISP);
static LISP nreverse (LISP);
static LISP cadr (LISP);
static LISP caar (LISP);
static LISP cdar (LISP);
static LISP cddr (LISP);
static LISP siod_true_value (void);
static LISP siod_false_value (void);
static LISP lapply (LISP fcn, LISP args);
static LISP listn (long n,...);
static char *must_malloc (unsigned long size);
static FILE *get_c_file (LISP p, FILE * deflt);
static char *last_c_errmsg (int);
static LISP llast_c_errmsg (int);
static void siod_c_provide(const char *);

static LISP funcall1 (LISP, LISP);
static LISP funcall2 (LISP, LISP, LISP);

static void siod_set_lib_path(const char *);

#endif /* __SIOD_H__ */
