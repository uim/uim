
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
#define tc_user_max 100

#define tc_table_dim 100

typedef struct obj *LISP;
typedef LISP (*SUBR_FUNC) (void);

#define CONSP(x)   TYPEP(x,tc_cons)
#define INTNUMP(x) TYPEP(x,tc_intnum)
#define SYMBOLP(x) TYPEP(x,tc_symbol)
#define STRINGP(x) TYPEP(x,tc_string)

#define NCONSP(x)   NTYPEP(x,tc_cons)
#define NINTNUMP(x) NTYPEP(x,tc_intnum)
#define NSYMBOLP(x) NTYPEP(x,tc_symbol)
#define NSTRINGP(x) NTYPEP(x,tc_string)

#define TKBUFFERN 5120

void siod_init (int argc, char **argv, int warnflag, FILE *);
void siod_quit (void);

void set_repl_hooks (void (*puts_f) (char *),
		     LISP (*read_f) (void),
		     LISP (*eval_f) (LISP),
		     void (*print_f) (LISP));
char *get_c_string (LISP x);
char *get_c_string_dim (LISP x, long *);
int get_c_int (LISP x);
long nlength(LISP x);

LISP cons (LISP x, LISP y);
LISP car (LISP x);
LISP cdr (LISP x);
LISP setcar (LISP cell, LISP value);
LISP intcons (int x);
LISP eql (LISP x, LISP y);
LISP symcons (char *pname, LISP vcell);
LISP symbol_boundp (LISP x, LISP env);
LISP symbol_value (LISP x, LISP env);
LISP symbol_to_string (LISP x, LISP env);
LISP rintern (const char *name);
LISP closure (LISP env, LISP code);

void init_subr (char *name, long type, SUBR_FUNC fcn);
void init_subr_0 (char *name, LISP (*fcn) (void));
void init_subr_1 (char *name, LISP (*fcn) (LISP));
void init_subr_2 (char *name, LISP (*fcn) (LISP, LISP));
void init_subr_2n (char *name, LISP (*fcn) (LISP, LISP));
void init_subr_3 (char *name, LISP (*fcn) (LISP, LISP, LISP));
void init_subr_4 (char *name, LISP (*fcn) (LISP, LISP, LISP, LISP));
void init_subr_5 (char *name, LISP (*fcn) (LISP, LISP, LISP, LISP, LISP));
void init_lsubr (char *name, LISP (*fcn) (LISP));
void init_fsubr (char *name, LISP (*fcn) (LISP, LISP));
void init_msubr (char *name, LISP (*fcn) (LISP *, LISP *));

LISP delq (LISP elem, LISP l);
void set_eval_hooks (long type, LISP (*fcn) (LISP, LISP *, LISP *));
LISP leval (LISP x, LISP env);
LISP symbolconc (LISP args);
LISP lprin1f (LISP exp, FILE * f);
LISP lread (LISP);
LISP lreadtk (char *, long j);
LISP lreadf (FILE * f);
LISP require (LISP fname);
LISP strcons (long length, const char *data);
LISP equal (LISP, LISP);
void set_fatal_exit_hook (void (*fcn) (void));
LISP intern (LISP x);
void gc_protect (LISP * location); /* exported as temporary solution for custom API */
#if (NESTED_REPL_C_STRING)
void siod_gc_protect_stack(LISP *stack_start);
void siod_gc_unprotect_stack(LISP *stack_start);
#endif
long repl_c_string (const char *, long want_init, long want_print);
LISP siod_return_value (void);
LISP reverse (LISP);
LISP nreverse (LISP);
LISP number2string (LISP, LISP, LISP, LISP);
LISP string2number (LISP, LISP);
LISP cadr (LISP);
LISP caar (LISP);
LISP cdar (LISP);
LISP cddr (LISP);
LISP caaar (LISP);
LISP caadr (LISP);
LISP cadar (LISP);
LISP caddr (LISP);
LISP cdaar (LISP);
LISP cdadr (LISP);
LISP cddar (LISP);
LISP cdddr (LISP);
LISP siod_true_value (void);
LISP siod_false_value (void);
LISP lapply (LISP fcn, LISP args);
LISP listn (long n,...);
char *must_malloc (unsigned long size);
FILE *get_c_file (LISP p, FILE * deflt);
char *last_c_errmsg (int);
LISP llast_c_errmsg (int);
void siod_c_provide(const char *);

LISP funcall1 (LISP, LISP);
LISP funcall2 (LISP, LISP, LISP);
LISP apply1 (LISP, LISP, LISP);

extern long siod_verbose_level;
void siod_set_lib_path(const char *);

#endif /* __SIOD_H__ */
