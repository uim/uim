/* Scheme In One Defun, but in C this time.

 *                      COPYRIGHT (c) 1988-1994 BY                          *
 *        PARADIGM ASSOCIATES INCORPORATED, CAMBRIDGE, MASSACHUSETTS.       *
 *                         ALL RIGHTS RESERVED                              *

 Permission to use, copy, modify, distribute and sell this software
 and its documentation for any purpose and without fee is hereby
 granted, provided that the above copyright notice appear in all copies
 and that both that copyright notice and this permission notice appear
 in supporting documentation, and that the name of Paradigm Associates
 Inc not be used in advertising or publicity pertaining to distribution
 of the software without specific, written prior permission.

 PARADIGM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 PARADIGM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 SOFTWARE.

 */

/*

   gjc@world.std.com

   Paradigm Associates Inc          Phone: 617-492-6079
   29 Putnam Ave, Suite 6
   Cambridge, MA 02138


   Release 1.0: 24-APR-88
   Release 1.1: 25-APR-88, added: macros, predicates, load. With additions by
   Barak.Pearlmutter@DOGHEN.BOLTZ.CS.CMU.EDU: Full flonum recognizer,
   cleaned up uses of NULL/0. Now distributed with siod.scm.
   Release 1.2: 28-APR-88, name changes as requested by JAR@AI.AI.MIT.EDU,
   plus some bug fixes.
   Release 1.3: 1-MAY-88, changed env to use frames instead of alist.
   define now works properly. vms specific function edit.
   Release 1.4 20-NOV-89. Minor Cleanup and remodularization.
   Now in 3 files, siod.h, slib.c, siod.c. Makes it easier to write your
   own main loops. Some short-int changes for lightspeed C included.
   Release 1.5 29-NOV-89. Added startup flag -g, select stop and copy
   or mark-and-sweep garbage collection, which assumes that the stack/register
   marking code is correct for your architecture.
   Release 2.0 1-DEC-89. Added repl_hooks, Catch, Throw. This is significantly
   different enough (from 1.3) now that I'm calling it a major release.
   Release 2.1 4-DEC-89. Small reader features, dot, backquote, comma.
   Release 2.2 5-DEC-89. gc,read,print,eval, hooks for user defined datatypes.
   Release 2.3 6-DEC-89. save_forms, obarray intern mechanism. comment char.
   Release 2.3a......... minor speed-ups. i/o interrupt considerations.
   Release 2.4 27-APR-90 gen_readr, for read-from-string.
   Release 2.5 18-SEP-90 arrays added to SIOD.C by popular demand. inums.
   Release 2.6 11-MAR-92 function prototypes, some remodularization.
   Release 2.7 20-MAR-92 hash tables, fasload. Stack check.
   Release 2.8  3-APR-92 Bug fixes, \n syntax in string reading.
   Release 2.9 28-AUG-92 gc sweep bug fix. fseek, ftell, etc. Change to
   envlookup to allow (a . rest) suggested by bowles@is.s.u-tokyo.ac.jp.
   Release 2.9a 10-AUG-93. Minor changes for Windows NT.
   Release 3.0  1-MAY-94. Release it, include changes/cleanup recommended by
   andreasg@nynexst.com for the OS2 C++ compiler. Compilation and running
   tested using DEC C, VAX C. WINDOWS NT. GNU C on SPARC. Storage
   management improvements, more string functions. SQL support.
   Release 3.1? -JUN-95 verbose flag, other integration improvements for htqs.c
   hpux by denson@sdd.hp.com, solaris by pgw9@columbia.edu.
   Release 3.2X MAR-96. dynamic linking, subr closures, other improvements.
 */
/*
  incoperated into libuim from gimp (Aug-02) Yusuke TABATA
  removed math functions (Oct-03) Yusuke TABATA
  removed vms,array,thinkc functions (Oct-03) Yusuke TABATA
  removed copygc (Nov-03) Yusuke TABATA
  removed many unneeded functionality (03-04) Yusuke TABATA
  removed sliba.c (Feb-04) Yusuke TABATA
  added second arg "LISP env" to undefine() (Jul-04-2004) YamaKen
  added 'case' special form (Sep-09-2004) Jun Inoue
  added 'else' symbol definition (Sep-21-2004) YamaKen
  fix broken feature? and provide (Sep-28-2004) YamaKen
  removed non-standard _"str" syntax for i18n (Sep-30-2004) YamaKen
  added NESTED_REPL_C_STRING feature (Dec-31-2004) YamaKen
  added heap_alloc_threshold and make configurable (Jan-07-2005) YamaKen
  added support for interactive debugging (Feb-09-2005) Jun Inoue
  renamed 'last' to 'last-pair' to conform to SRFI-1 (Apr-04-2005) YamaKen
  added inteql for "=" predicate (Jun-19-2005) YamaKen
 */

#include <config.h>

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <limits.h>
#include <sys/types.h>
#if HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include "siod.h"

/* struct */

struct catch_frame {
  LISP tag;
  LISP retval;
  jmp_buf cframe;
  struct catch_frame *next;
};


struct gen_readio {
  int (*getc_fcn) (void *);
  void (*ungetc_fcn) (int, void *);
  void *cb_argument;
};

struct gen_printio {
  int (*putc_fcn) (int, void *);
  int (*puts_fcn) (char *, void *);
  void *cb_argument;
};

struct user_type_hooks {
  LISP (*gc_mark) (LISP);
  void (*gc_free) (LISP);
  void (*prin1) (LISP, struct gen_printio *);
  LISP (*leval) (LISP, LISP *, LISP *);
  LISP (*equal) (LISP, LISP);
};

struct repl_hooks {
  void (*repl_puts) (char *);
  LISP (*repl_read) (void);
  LISP (*repl_eval) (LISP);
  void (*repl_print) (LISP);
};

struct gc_protected {
  LISP *location;
  long length;
  struct gc_protected *next;
};

struct func_frame {
  struct func_frame *prev;
  LISP obj;
};

/* forward declaration of static functions */
static void gc_for_newcell (void);

/* forward declaration of static functions previously declared in siod.h */
static void siod_init (int argc, char **argv, int warnflag, FILE *);
static void siod_quit (void);

#if 0
static void set_repl_hooks (void (*puts_f) (char *),
			    LISP (*read_f) (void),
			    LISP (*eval_f) (LISP),
			    void (*print_f) (LISP));
#endif
static char *get_c_string (LISP x);
static char *get_c_string_dim (LISP x, long *);
static int get_c_int (LISP x);
static long nlength(LISP x);
static void *get_c_pointer (LISP x);
static C_FUNC get_c_func_pointer (LISP x);

static LISP cons (LISP x, LISP y);
static LISP car (LISP x);
static LISP cdr (LISP x);
static LISP setcar (LISP cell, LISP value);
static LISP intcons (int x);
static LISP eql (LISP x, LISP y);
#if 0
static LISP inteql (LISP x, LISP y);
#endif
static LISP symcons (char *pname, LISP vcell);
static LISP symbol_boundp (LISP x, LISP env);
static LISP symbol_value (LISP x, LISP env);
static LISP symbol_to_string (LISP x, LISP env);
static LISP rintern (const char *name);
static LISP closure (LISP env, LISP code);
static LISP ptrcons (void *ptr);
static LISP funcptrcons (C_FUNC ptr);
static LISP assoc (LISP x, LISP alist);

static void init_subr (const char *name, long type, SUBR_FUNC fcn);
static void init_subr_0 (const char *name, LISP (*fcn) (void));
static void init_subr_1 (const char *name, LISP (*fcn) (LISP));
static void init_subr_2 (const char *name, LISP (*fcn) (LISP, LISP));
static void init_subr_2n (const char *name, LISP (*fcn) (LISP, LISP));
static void init_subr_3 (const char *name, LISP (*fcn) (LISP, LISP, LISP));
#if 0
static void init_subr_4 (const char *name, LISP (*fcn) (LISP, LISP, LISP, LISP));
static void init_subr_5 (const char *name, LISP (*fcn) (LISP, LISP, LISP, LISP, LISP));
#endif
static void init_lsubr (const char *name, LISP (*fcn) (LISP));
static void init_fsubr (const char *name, LISP (*fcn) (LISP, LISP));
static void init_msubr (const char *name, LISP (*fcn) (LISP *, LISP *));

static LISP delq (LISP elem, LISP l);
#if 0
static void set_eval_hooks (long type, LISP (*fcn) (LISP, LISP *, LISP *));
#endif
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
#if !(NESTED_REPL_C_STRING)
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
#if 0
static LISP siod_false_value (void);
#endif
static LISP lapply (LISP fcn, LISP args);
static LISP listn (long n,...);
static void *must_malloc (unsigned long size);
static FILE *get_c_file (LISP p, FILE * deflt);
#if 0
static char *last_c_errmsg (int);
#endif
static LISP llast_c_errmsg (int);
static void siod_c_provide(const char *);

static LISP funcall1 (LISP, LISP);
static LISP funcall2 (LISP, LISP, LISP);

static void siod_set_lib_path(const char *);

/* macros */

#define gc_protect siod_gc_protect

#define INTERRUPT_CHECK()

#define GETC_FCN(x) (*((*x).getc_fcn))((*x).cb_argument)
#define UNGETC_FCN(c,x) (*((*x).ungetc_fcn))(c,(*x).cb_argument)
#define PUTC_FCN(c,x) (*((*x).putc_fcn))(c,(*x).cb_argument)
#define PUTS_FCN(c,x) (*((*x).puts_fcn))(c,(*x).cb_argument)

#define STACK_LIMIT(_ptr,_amt) (((char *)_ptr) - (_amt))

#define STACK_CHECK(_ptr) \
  if (((char *) (_ptr)) < stack_limit_ptr) err_stack((char *) _ptr);

#define _NEWCELL(_into, _type)         \
{  if NULLP(freelist)                 \
      gc_for_newcell();               \
    _into = freelist;                 \
    freelist = CDR(freelist);         \
    ++gc_cells_allocated;             \
 (*_into).gc_mark = 0;                \
 (*_into).type = (short) _type;}

#if ! DEBUG_SCM
#define dbg_readini(f)
#define dbg_readend()
#define dbg_register_closure(x)
#define NEWCELL(_into, _type)	_NEWCELL (_into, _type)
#else
#define NEWCELL(_into, _type)		\
{  _NEWCELL (_into, _type);		\
   (*_into).dbg_info = NIL; }
#endif /* DEBUG_SCM */

/* exported global symbol */
static long siod_verbose_level;
static LISP sym_t;
/*  Added by Spencer Kimball for script-fu shit 6/3/97 */
static FILE *siod_output;
static const char *siod_lib;

#define MAX_ERROR 1024
static char siod_err_msg[MAX_ERROR];
static char *stack_limit_ptr;
static LISP sym_f;
static long nheaps;
static LISP *heaps;
static LISP heap, heap_end;
static long heap_size;
static long heap_alloc_threshold;
static long gc_status_flag;
static char *init_file;
static char *tkbuffer;
static long gc_cells_allocated;
static double gc_time_taken;
static LISP *stack_start_ptr;
static LISP freelist;
static jmp_buf errjmp;
static long errjmp_ok;
static LISP oblistvar;
static LISP eof_val;
static LISP sym_errobj;
static LISP sym_catchall;
static LISP sym_progn;
static LISP sym_lambda;
static LISP sym_else;
static LISP sym_quote;
static LISP sym_dot;
static LISP sym_after_gc;
static LISP sym_features;
static LISP unbound_marker;
static LISP *obarray;
static LISP repl_return_val;
#if (!NESTED_REPL_C_STRING)
static int repl_c_string_entered;
#endif
static long obarray_dim;
static struct catch_frame *catch_framep;
static void (*repl_puts) (char *);
static LISP (*repl_read) (void);
static LISP (*repl_eval) (LISP);
static void (*repl_print) (LISP);
static LISP *inums;
static long inums_dim;
static struct user_type_hooks *user_types;
static struct gc_protected *protected_registers;
static jmp_buf save_regs_gc_mark;
static double gc_rt;
static long gc_cells_swept;
static long gc_cells_collected;
static char *user_ch_readm;
static char *user_te_readm;
static LISP (*user_readm) (int, struct gen_readio *);
static LISP (*user_readt) (char *, long, int *);
static void (*fatal_exit_hook) (void);
static long stack_size;
static struct func_frame *func_trace;

#if DEBUG_SCM
static LISP dbg_pos = NIL;
static LISP dbg_mod = NIL;

static int dbg_getc (struct gen_readio * f);
static void dbg_ungetc (int c, struct gen_readio * f);
static void dbg_readini (char *file);
static void dbg_readend (void);
static void dbg_lineinc (void);
static void dbg_linedec (void);
static void init_dbg (void);
#endif /* DEBUG_SCM */

static LISP lreadparen (struct gen_readio * f);
static LISP lreadr (struct gen_readio *f);
static LISP my_err(char *message, LISP obj);
static LISP lprint (LISP exp, LISP);
/* static void gc_protect (LISP * location); */
static LISP provide (LISP name);

#define ENVLOOKUP_TRICK 1

static long inside_err = 0;

static char *
try_get_c_string (LISP x)
{
  if TYPEP
    (x, tc_symbol)
      return (PNAME (x));
  else if TYPEP
    (x, tc_string)
      return (x->storage_as.string.data);
  else
    return (NULL);
}

static LISP
envlookup (LISP var, LISP env)
{
  LISP frame, al, fl, tmp;
  for (frame = env; CONSP (frame); frame = CDR (frame))
    {
      tmp = CAR (frame);
      if NCONSP
	(tmp) my_err ("damaged frame", tmp);
      for (fl = CAR (tmp), al = CDR (tmp); CONSP (fl); fl = CDR (fl), al = CDR (al))
	{
	  if NCONSP
	    (al) my_err ("too few arguments", tmp);
	  if EQ
	    (CAR (fl), var) return (al);
	}
      /* suggested by a user. It works for reference (although conses)
         but doesn't allow for set! to work properly... */
#if (ENVLOOKUP_TRICK)
      if (SYMBOLP (fl) && EQ (fl, var))
	return (cons (al, NIL));
#endif
    }
  if NNULLP
    (frame) my_err ("damaged env", env);
  return (NIL);
}

static LISP
setvar (LISP var, LISP val, LISP env)
{
  LISP tmp;
  if NSYMBOLP
    (var) my_err ("wta(non-symbol) to setvar", var);
  tmp = envlookup (var, env);
  if NULLP
    (tmp) return (VCELL (var) = val);
  return (CAR (tmp) = val);
}

static void
show_backtrace(void)
{
  struct func_frame *fr;
  fprintf(siod_output, "*backtrace*\n");
  for (fr = func_trace; fr; fr = fr->prev) {
    fprintf(siod_output, ">>");
    lprin1f(fr->obj, siod_output);
#if DEBUG_SCM
    if NNULLP
      (fr->obj->dbg_info)
      {
	fprintf (siod_output,
		 " at %s:%d",
		 CAR (fr->obj->dbg_info)->storage_as.string.data,
		 INTNM (CDR (fr->obj->dbg_info)));
      }
#endif
    fprintf(siod_output, "\n");
  }
  fprintf(siod_output, "\n");
}

static LISP
my_err (char *message, LISP x)
{
  struct catch_frame *l;
  long was_inside = inside_err;
  LISP retval, nx;
  char *msg, *eobj;
  if ((!message) && CONSP (x) && TYPEP (CAR (x), tc_string))
    {
      msg = get_c_string (CAR (x));
      nx = CDR (x);
      retval = x;
    }
  else
    {
      msg = message;
      nx = x;
      retval = NIL;
    }
  if ((eobj = try_get_c_string (nx)) && !memchr (eobj, 0, 30))
    eobj = NULL;

  if NULLP
    (nx)
      sprintf (siod_err_msg, "ERROR: %s\n", msg);
  else if (eobj)
    sprintf (siod_err_msg, "ERROR: %s (errobj %s)\n", msg, eobj);
  else
    sprintf (siod_err_msg, "ERROR: %s (see errobj)\n", msg);

  if ((siod_verbose_level >= 1) && msg)
    {
      fprintf (siod_output, "%s\n", siod_err_msg);
      fflush (siod_output);
    }

  /*
   * Don't change the verbose level 2. This is used to suppress backtrace
   * when run by the testing framework. -- YamaKen 2005-11-05
   */
  if (siod_verbose_level >= 2)
    show_backtrace();

  if (errjmp_ok == 1)
    {
      /* prevent recording of bogus debug info */
      dbg_readend ();
      inside_err = 1;
      setvar (sym_errobj, nx, NIL);
      for (l = catch_framep; l; l = (*l).next)
	if (EQ ((*l).tag, sym_errobj) ||
	    EQ ((*l).tag, sym_catchall))
	  {
	    if (!msg)
	      msg = "quit";
	    (*l).retval = (NNULLP (retval) ? retval :
			   (was_inside) ? NIL :
			   cons (strcons (strlen (msg), msg), nx));
	    inside_err = 0;
	    longjmp ((*l).cframe, 2);
	  }
      inside_err = 0;
      longjmp (errjmp, (msg) ? 1 : 2);
    }
  if (siod_verbose_level >= 1)
    {
      fprintf (stderr, "FATAL ERROR DURING STARTUP OR CRITICAL CODE SECTION\n");
      fflush (stderr);
    }
  if (fatal_exit_hook)
    (*fatal_exit_hook) ();
  else
    exit (EXIT_FAILURE);
  return (NIL);
}

static void
init_slib_version (void)
{
  setvar (rintern ("*slib-version*"),
	  rintern ("$Id: interp_slib.c,v 1.12 2002/02/26 14:56:09 neo Exp $"),
	  NIL);
}

static struct user_type_hooks *
get_user_type_hooks (long type)
{
  long n;
  if (user_types == NULL)
    {
      n = sizeof (struct user_type_hooks) * tc_table_dim;
      user_types = (struct user_type_hooks *) must_malloc (n);
      memset (user_types, 0, n);
    }
  if ((type >= 0) && (type < tc_table_dim))
    return (&user_types[type]);
  else
    my_err ("type number out of range", NIL);
  return (NULL);
}

static int
get_c_int (LISP x)
{
  if NINTNUMP
    (x) my_err ("not a number", x);
  return ((long) INTNM (x));
}

static long
nlength (LISP obj)
{
  LISP l;
  long n;
  switch TYPE
    (obj)
    {
    case tc_string:
      return (strlen (obj->storage_as.string.data));
    case tc_nil:
      return (0);
    case tc_cons:
      for (l = obj, n = 0; CONSP (l); l = CDR (l), ++n)
	INTERRUPT_CHECK ();
      if NNULLP
	(l) my_err ("improper list to length", obj);
      return (n);
    default:
      my_err ("wta to length", obj);
      return (0);
    }
}

static LISP
get_eof_val (void)
{
  return (eof_val);
}

static double
myrealtime (void)
{
  time_t x;
  time (&x);
  return ((double) x);
}


static void
fput_st (FILE * f, char *st)
{
  if (siod_verbose_level >= 1)
    {
      /*      fprintf (stdout, "%s", st); */
      /*      fflush (siod_output); */
      fprintf (f, "%s", st);
      fflush (f);
    }
}

static void
put_st (char *st)
{
  fput_st (siod_output, st);
  fflush (siod_output);
}

static void
grepl_puts (char *st, void (*repl_puts) (char *))
{
  if (repl_puts == NULL)
    put_st (st);
  else
    (*repl_puts) (st);
}

static double
myruntime (void)
{
#if HAVE_SYS_TIMES_H
  double total;
  struct tms b;
  times (&b);
  total = b.tms_utime;
  total += b.tms_stime;
  return (total / 60.0);
#elif defined (G_OS_WIN32)
  FILETIME creation, exit, kernel, user;
  GetProcessTimes (GetCurrentProcess (), &creation, &exit, &kernel, &user);
  return (kernel.dwLowDateTime * 1e7 + user.dwLowDateTime * 1e7);
#endif
  return 0.0;
}

static long
repl (struct repl_hooks *h)
{
  LISP x;
  double rt, ct;
  while (1)
    {
      if (siod_verbose_level >= 2)
	grepl_puts ("> ", h->repl_puts);
      if (h->repl_read == NULL)
	x = lread (NIL);
      else
	x = (*h->repl_read) ();
      if EQ
	(x, eof_val) break;

      rt = myruntime ();
      ct = myrealtime ();

      gc_cells_allocated = 0;
      gc_time_taken = 0.0;
      if (h->repl_eval == NULL)
	repl_return_val = x = leval (x, NIL);
      else
	repl_return_val = x = (*h->repl_eval) (x);

      /*      sprintf (tkbuffer,
	       "Evaluation took %g seconds (%g in gc) %ld cons work, %g real.\n",
	       myruntime () - rt,
	       gc_time_taken,
	       gc_cells_allocated,
	       myrealtime () - ct);
      if (siod_verbose_level >= 3)
      grepl_puts (tkbuffer, h->repl_puts);
      if (h->repl_print == NULL)
	{
	  if (siod_verbose_level >= 2)
	    lprint (x, NIL);
	}
      else
      (*h->repl_print) (x);*/
    }
     
  return (0);
}

static LISP
setcdr (LISP cell, LISP value)
{
  if NCONSP
    (cell) my_err ("wta to setcdr", cell);
  return (CDR (cell) = value);
}

static LISP
newcell (long type)
{
  LISP z;
  NEWCELL (z, type);
  return (z);
}

static LISP
fopen_cg (FILE * (*fcn) (const char *, const char *), char *name, char *how)
{
  LISP sym;
  char errmsg[80];
  sym = newcell (tc_c_file);
  sym->storage_as.c_file.f = (FILE *) NULL;
  sym->storage_as.c_file.name = (char *) NULL;
  if (!(sym->storage_as.c_file.f = (*fcn) (name, how)))
    {
      snprintf(errmsg, 79, "could not open %s", name);
      my_err (errmsg, llast_c_errmsg (-1));
    }
  sym->storage_as.c_file.name = (char *) must_malloc (strlen (name) + 1);
  strcpy (sym->storage_as.c_file.name, name);
  return (sym);
}

static LISP
fopen_c (char *name, char *how)
{
  return (fopen_cg (fopen, name, how));
}

static void
file_gc_free (LISP ptr)
{
  if (ptr->storage_as.c_file.f)
    {
      fclose (ptr->storage_as.c_file.f);
      ptr->storage_as.c_file.f = (FILE *) NULL;
    }
  if (ptr->storage_as.c_file.name)
    {
      free (ptr->storage_as.c_file.name);
      ptr->storage_as.c_file.name = NULL;
    }
}

static LISP
fclose_l (LISP p)
{
  if NTYPEP
    (p, tc_c_file) my_err ("not a file", p);
  file_gc_free (p);
  return (NIL);
}

static LISP
lprin1 (LISP exp, LISP lf)
{
  FILE *f = get_c_file (lf, siod_output);
  lprin1f (exp, f);
  return (NIL);
}

static void
siod_set_lib_path(const char *path)
{
  siod_lib = path;
}

static LISP
vload (char *fname, long cflag, long rflag)
{
  LISP form, result, tail, lf, reader = NIL;
  FILE *f;
  int c, j;
  char buffer[512];
  char *fnbuf;
  char  *key = "parser:", *start, *end, *ftype = ".scm";
  if (rflag)
    {
      if ((fname[0] != '/'))
	{
	  fnbuf = alloca(strlen(siod_lib) + strlen(fname) + 2);
	  strcpy (fnbuf, siod_lib);
	  strcat (fnbuf, "/");
     	  strcat (fnbuf, fname);
	  if ((f = fopen (fnbuf, "r")))
	    {
	      fname = fnbuf;
	      fclose (f);
	    }
	}
    }
  if (siod_verbose_level >= 3)
    {
      put_st ("loading ");
      put_st (fname);
      put_st ("\n");
    }
  lf = fopen_c (fname, "r");
  f = lf->storage_as.c_file.f;
  dbg_readini (fname);
  result = NIL;
  tail = NIL;
  j = 0;
  buffer[0] = 0;
  c = getc (f);
  while ((c == '#') || (c == ';'))
    {
      while (((c = getc (f)) != EOF) && (c != '\n'))
	if ((j + 1) < (int)sizeof (buffer))
	  {
	    buffer[j] = c;
	    buffer[++j] = 0;
	  }
      if (c == '\n')
	{
	  c = getc (f);
#if DEBUG_SCM
	  dbg_lineinc ();
#endif
	}
    }
  if (c != EOF)
    ungetc (c, f);
  if ((start = strstr (buffer, key)))
    {
      for (end = &start[strlen (key)];
	   *end && isalnum ((int)*end);
	   ++end);
      j = end - start;
      memmove (buffer, start, j);
      buffer[strlen (key) - 1] = '_';
      buffer[j] = 0;
      strcat (buffer, ftype);
      require (strcons (-1, buffer));
      buffer[j] = 0;
      reader = rintern (buffer);
      reader = funcall1 (leval (reader, NIL), reader);
      if (siod_verbose_level >= 5)
	{
	  put_st ("parser:");
	  lprin1 (reader, NIL);
	  put_st ("\n");
	}
    }
  while (1)
    {
      form = NULLP (reader) ? lread (lf) : funcall1 (reader, lf);
      if EQ
	(form, eof_val) break;
      if (siod_verbose_level >= 5)
	lprint (form, NIL);
      if (cflag)
	{
	  form = cons (form, NIL);
	  if NULLP
	    (result)
	      result = tail = form;
	  else
	    tail = setcdr (tail, form);
	}
      else
	leval (form, NIL);
    }
  fclose_l (lf);
  dbg_readend ();
  if (siod_verbose_level >= 3)
    put_st ("done.\n");
  return (result);
}

static long
repl_driver (long want_init, struct repl_hooks *h)
{
  long ret;
  int k;
  struct repl_hooks hd;
  LISP stack_start;
#if (!NESTED_REPL_C_STRING)
  if (repl_c_string_entered)
    {
      my_err("nested repl_driver", NIL);
      ret = 0;
      goto fin;
    }
  repl_c_string_entered = 1;
  func_trace = NULL;
#endif
#if (NESTED_REPL_C_STRING)
  siod_gc_protect_stack(&stack_start);
#else
  stack_start_ptr = &stack_start;
  stack_limit_ptr = STACK_LIMIT (stack_start_ptr, stack_size);
#endif
  k = setjmp (errjmp);
  if (k == 2) {
    ret = (2);
    goto fin;
  }
#if (!NESTED_REPL_C_STRING)
  catch_framep = (struct catch_frame *) NULL;
#endif
  errjmp_ok = 1;
  if (want_init && init_file && (k == 0))
    vload (init_file, 0, 1);
  if (!h)
    {
      hd.repl_puts = repl_puts;
      hd.repl_read = repl_read;
      hd.repl_eval = repl_eval;
      hd.repl_print = repl_print;
      ret = (repl (&hd));
      goto fin;
    }
  else {
    ret = (repl (h));
    goto fin;
  }

 fin:
#if (NESTED_REPL_C_STRING)
  siod_gc_unprotect_stack(&stack_start);
#else
  repl_c_string_entered = 0;
#endif
  return ret;
}

static void
ignore_puts (char *st)
{
}

static void
noprompt_puts (char *st)
{
  if (strcmp (st, "> ") != 0)
    put_st (st);
}

static const char *repl_c_string_arg = NULL;
static long repl_c_string_flag = 0;

static int
rfs_getc (unsigned char **p)
{
  int i;
  i = **p;
  if (!i)
    return (EOF);
  *p = *p + 1;
  return (i);
}

static void
rfs_ungetc (unsigned char c, unsigned char **p)
{
  *p = *p - 1;
}

static int
flush_ws (struct gen_readio *f, char *eoferr)
{
  int c, commentp;
  commentp = 0;
  while (1)
    {
      c = GETC_FCN (f);
      if (c == EOF)
	{
	  if (eoferr)
	    my_err (eoferr, NIL);
	  else
	    return (c);
	}
      
      if (commentp)
	{
	  if (c == '\n')
	    commentp = 0;
	}
      else if (c == ';')
	commentp = 1;
      else if (!isspace (c))
	return (c);
    }
}

static LISP
strcons (long length, const char *data)
{
  LISP s;
  s = cons (NIL, NIL);
  s->type = tc_string;
  if (length == -1)
    length = strlen (data);
  s->storage_as.string.data = (char *) must_malloc (length + 1);
  s->storage_as.string.dim = length;
  if (data)
    memcpy (s->storage_as.string.data, data, length);
  s->storage_as.string.data[length] = 0;
  return (s);
}

static LISP
string_append (LISP args)
{
  long size;
  LISP l, s;
  char *data;
  size = 0;
  for (l = args; NNULLP (l); l = cdr (l))
    size += strlen (get_c_string (car (l)));
  s = strcons (size, NULL);
  data = s->storage_as.string.data;
  data[0] = 0;
  for (l = args; NNULLP (l); l = cdr (l))
    strcat (data, get_c_string (car (l)));
  return (s);
}

static void
err_stack (char *ptr)
     /* The user could be given an option to continue here */
{
  my_err ("the currently assigned stack limit has been exceeded", NIL);
}

static LISP
lreadstring (struct gen_readio * f)
{
  int j, c, n, ndigits;
  char *p;
  j = 0;
  p = tkbuffer;
  while (((c = GETC_FCN (f)) != '"') && (c != EOF))
    {
      if (c == '\\')
	{
	  c = GETC_FCN (f);
	  if (c == EOF)
	    my_err ("eof after \\", NIL);
	  switch (c)
	    {
	    case '\\':
	      c = '\\';
	      break;
	    case 'n':
	      c = '\n';
	      break;
	    case 't':
	      c = '\t';
	      break;
	    case 'r':
	      c = '\r';
	      break;
	    case 'f':
	      c = '\f';
	      break;
	    case 'a':
	      c = '\a';
	      break;
	    case 'b':
	      c = '\b';
	      break;
	    case 'v':
	      c = '\v';
	      break;
	    case 'd':
	      c = 0x04;
	      break;
	    case 'N':
	      c = 0;
	      break;
	    case 's':
	      c = ' ';
	      break;
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	      n = c - '0';
	      ndigits = 1;
	      while (ndigits < 3)
		{
		  c = GETC_FCN (f);
		  if (c == EOF)
		    my_err ("eof after \\0", NIL);
		  if (c >= '0' && c <= '7')
		    {
		      n = n * 8 + c - '0';
		      ndigits++;
		    }
		  else
		    {
		      UNGETC_FCN (c, f);
		      break;
		    }
		}
	      c = n;
	    }
	}
      if ((j + 1) >= TKBUFFERN)
	my_err ("read string overflow", NIL);
      ++j;
      *p++ = c;
    }
  *p = 0;
  return (strcons (j, tkbuffer));
}


static LISP
lreadsharp (struct gen_readio * f)
{
  LISP obj;
  int c;
  c = GETC_FCN (f);
  switch (c)
    {
    case '.':
      obj = lreadr (f);
      return (leval (obj, NIL));
    case 'f':
      return (NIL);
    case 't':
      return (intcons (1));
    default:
      return (my_err ("readsharp syntax not handled", NIL));
    }
}

static LISP
lreadr (struct gen_readio *f)
{
  int c, j;
  char *p, *buffer = tkbuffer;
#if DEBUG_SCM
  LISP dbg_start_pos, dbg_ret;
#define return(val)                            \
  do                                           \
    {                                          \
      dbg_ret = (val);                         \
      if NNULLP                                \
        (dbg_ret)                              \
	  dbg_ret->dbg_info = dbg_start_pos;   \
      return (dbg_ret);                        \
    }                                          \
  while (0)
#endif /* DEBUG_SCM */
  
  STACK_CHECK (&f);
  p = buffer;
  c = flush_ws (f, "end of file inside read");
#if DEBUG_SCM
  dbg_start_pos = car (dbg_pos);
#endif
  switch (c)
    {
    case '(':
      return (lreadparen (f));
    case ')':
      my_err ("unexpected close paren", NIL);
    case '\'':
      return (cons (sym_quote, cons (lreadr (f), NIL)));
    case '`':
      return (cons (rintern ("+internal-backquote"), lreadr (f)));
    case ',':
      c = GETC_FCN (f);
      switch (c)
	{
	case '@':
	  p = "+internal-comma-atsign";
	  break;
	case '.':
	  p = "+internal-comma-dot";
	  break;
	default:
	  p = "+internal-comma";
	  UNGETC_FCN (c, f);
	}
      return (cons (rintern (p), lreadr (f)));
      /* We use the form (_ "str") to represent gettext string to be
	 compatible with other lisp implementations, so the strange
	 _"str" syntax handled below are removed -- YamaKen 2004-09-30
      */
#if 0
    case '_':  /*  might be a string marked for translation using _(...)  */
      c = GETC_FCN (f);
      if (c == '"')
	return (lreadstring (f));
      else
	UNGETC_FCN (c, f);
      break;
#endif
    case '"':
      return (lreadstring (f));
    case '#':
      return (lreadsharp (f));
    default:
      if ((user_readm != NULL) && strchr (user_ch_readm, c))
	return ((*user_readm) (c, f));
    }
  *p++ = c;
  for (j = 1; j < TKBUFFERN; ++j)
    {
      c = GETC_FCN (f);
      if (c == EOF)
	return (lreadtk (buffer, j));
      if (isspace (c))
	return (lreadtk (buffer, j));
      if (strchr ("()'`,;\"", c) || strchr (user_te_readm, c))
	{
	  UNGETC_FCN (c, f);
	  return (lreadtk (buffer, j));
	}
      *p++ = c;
    }
  return (my_err ("token larger than TKBUFFERN", NIL));
#undef return
}

/* Iterative version */
static LISP
lreadparen (struct gen_readio * f)
{
  int c;
  LISP tmp, l = NIL;
  LISP last = l;

  while ((c = flush_ws(f, "end of file inside list")) != ')')
    {
#if DEBUG_SCM
      LISP dbg_start_pos;
      dbg_start_pos = car (dbg_pos);
#endif
      UNGETC_FCN (c,f);
      tmp = lreadr (f);
      if EQ
	(tmp, sym_dot)
	{
	  tmp = lreadr (f);
	  c = flush_ws (f, "end of file inside list");
	  if (c != ')')
	    my_err ("missing close paren", NIL);
	  if (l == NIL)
	    my_err("nor car for dotted pair", NIL);
	  CDR (last) = tmp;
	  break;
	}
      if (l == NIL)
	{
	  l = cons (tmp, NIL);
	  last = l;
	}
      else
	{
	  CDR (last) = cons (tmp, NIL);
	  last = cdr (last);
	}
#if DEBUG_SCM
      last->dbg_info = dbg_start_pos;
#endif
    }
  return l;
}

static LISP
readtl (struct gen_readio * f)
{
  int c;
#if DEBUG_SCM
  if NNULLP
    (dbg_pos)
    {
      struct gen_readio s;

      s.getc_fcn = (int (*)(void *)) dbg_getc;
      s.ungetc_fcn = (void (*)(int, void *)) dbg_ungetc;
      s.cb_argument = (void *) f;
      f = &s;
    }
#endif

  c = flush_ws (f, (char *) NULL);
  if (c == EOF)
    return (eof_val);
  UNGETC_FCN (c, f);
  return (lreadr (f));
}

#if DEBUG_SCM

static int
dbg_getc (struct gen_readio * f)
{
  int c;
  c = GETC_FCN (f);
  if (c == '\n')
    dbg_lineinc ();
  return c;
}

static void
dbg_ungetc (int c, struct gen_readio * f)
{
  UNGETC_FCN (c, f);
  if (c == '\n')
    dbg_linedec ();
}

static void
dbg_lineinc (void)
{
  LISP file, line;
  if CONSP
    (dbg_pos)
    {
      file = CAR (CAR (dbg_pos));
      line = CDR (CAR (dbg_pos));
      CAR (dbg_pos) = cons (file, intcons (INTNM (line) + 1));
      CAR (dbg_pos)->dbg_info = NIL;
    }
  /* else: we have given up debugging information */
}

static void
dbg_linedec (void)
{
  LISP file, line;
  if CONSP
    (dbg_pos)
    {
      file = CAR (CAR (dbg_pos));
      line = CDR (CAR (dbg_pos));
      CAR (dbg_pos) = cons (file, intcons (INTNM (line) - 1));
      CAR (dbg_pos)->dbg_info = NIL;
    }
  /* else: we have given up debugging information */
}

static void
dbg_readini (char *filename)
{
  LISP f, dbg_pos_save, dbg_closures;

  /* no debug info needed for now */
  dbg_pos_save = dbg_pos;
  dbg_pos = NIL;

  /* maintain a list of toplevel closures */
  f = strcons (-1, filename);
  dbg_closures = rintern ("dbg-closures");
  dbg_mod = assoc (f, VCELL (dbg_closures));
  if NNULLP
    (dbg_mod)
      CDR (dbg_mod) = NIL;
  else
    {
      dbg_mod = cons (cons (f, NIL), VCELL (dbg_closures));
      setvar (dbg_closures, dbg_mod, NIL);
      dbg_mod = CAR (dbg_mod);
    }

  dbg_pos = cons (cons (f, intcons (1)), dbg_pos_save);
}

static void
dbg_readend (void)
{
  dbg_pos = cdr (dbg_pos);
  if CONSP
    (dbg_pos)
    {
      dbg_mod = assoc (CAR (CAR (dbg_pos)), VCELL (rintern ("dbg-closures")));
      if NULLP
	(dbg_mod)
	  abort ();
    }
}

static void
dbg_register_closure (LISP x)
{
  /* maintain a list of toplevel closures */
  if CONSP
    (dbg_pos)
    {
      CDR (dbg_mod) = cons (x, CDR (dbg_mod));
      /* toplevel closures should know where their definitions begin */
      if NNULLP
	(cddr (x->storage_as.closure.code))
	  x->dbg_info = CDR (CDR (x->storage_as.closure.code))->dbg_info;
    }
}

static LISP
dbg_expand_file_name (LISP fl)
{
  char *fname, *fnbuf;
  FILE *f;
  size_t len;

  if NTYPEP
    (fl, tc_string)
      my_err ("wta to dbg_expand_file_name ()", fl);
  fname = fl->storage_as.string.data;
  if ((fname[0] != '/'))
    {
      len = strlen (siod_lib) + strlen (fname) + 2;
      fnbuf = (char *) must_malloc (len);
      strcpy (fnbuf, siod_lib);
      strcat (fnbuf, "/");
      strcat (fnbuf, fname);
      if ((f = fopen (fnbuf, "r")))
	{
	  fclose (f);
	  fl = cons (NIL, NIL);
	  fl->type = tc_string;
	  fl->storage_as.string.data = fnbuf;
	  fl->storage_as.string.dim = len-1;
	  return (fl);
	}
      free (fnbuf);
    }
  return (fl);
}

static LISP
dbg_get_info (LISP x)
{
  return NNULLP (x) ? x->dbg_info : NIL;
}

static LISP
dbg_get_line (LISP x)
{
  x = dbg_get_info (x);
#if DEBUG_SCM
  return NNULLP (x) ? CDR (x) : intcons (-1);
#endif
}

static LISP
dbg_get_file (LISP x)
{
  x = dbg_get_info (x);
  return NNULLP (x) ? CAR (x) : strcons (-1, "(No file info)");
}

static LISP
dbg_copy_info (LISP x, LISP y)
{
  return (x->dbg_info = dbg_get_info (y));
}

static void
init_dbg (void)
{
  dbg_pos = NIL;
  dbg_mod = NIL;
  gc_protect (&dbg_pos);
  gc_protect (&dbg_mod);
  init_subr_1 ("dbg-get-info", dbg_get_info);
  init_subr_1 ("dbg-get-line", dbg_get_line);
  init_subr_1 ("dbg-get-file", dbg_get_file);
  init_subr_2 ("dbg-copy-info!", dbg_copy_info);
  init_subr_1 ("dbg-expand-file-name", dbg_expand_file_name);
  setvar (rintern ("dbg-closures"), NIL, NIL);
  provide (rintern ("debug"));
}

#endif /* DEBUG_SCM */

static LISP
read_from_string (LISP x)
{
  char *p;
  struct gen_readio s;
  p = get_c_string (x);
  s.getc_fcn = (int (*)(void *)) rfs_getc;
  s.ungetc_fcn = (void (*)(int, void *)) rfs_ungetc;
  s.cb_argument = (char *) &p;
  return (readtl (&s));
}

static LISP
repl_c_string_read (void)
{
  LISP s;
  if (repl_c_string_arg == NULL)
    return (get_eof_val ());
  s = strcons (strlen (repl_c_string_arg), repl_c_string_arg);
  repl_c_string_arg = NULL;
  return (read_from_string (s));
}

static void
ignore_print (LISP x)
{
  repl_c_string_flag = 1;
}

static void
not_ignore_print (LISP x)
{
  repl_c_string_flag = 1;
  lprint (x, NIL);
}

static long
repl_c_string (const char *str,
	       long want_init, long want_print)
{
  struct repl_hooks h;
  long retval;
  if (want_print)
    h.repl_puts = noprompt_puts;
  else
    h.repl_puts = ignore_puts;
  h.repl_read = repl_c_string_read;
  h.repl_eval = NULL;
  if (want_print)
    h.repl_print = not_ignore_print;
  else
    h.repl_print = ignore_print;
  repl_c_string_arg = str;
  repl_c_string_flag = 0;
  retval = repl_driver (want_init, &h);
  if (retval != 0)
    return (retval);
  else if (repl_c_string_flag == 1)
    return (0);
  else
    return (2);
}

#if (!NESTED_REPL_C_STRING)
static int
siod_repl_c_string_entered (void)
{
  return repl_c_string_entered;
}
#endif

#if 0
static void
set_repl_hooks (void (*puts_f) (char *),
		LISP (*read_f) (void),
		LISP (*eval_f) (LISP),
		void (*print_f) (LISP))
{
  repl_puts = puts_f;
  repl_read = read_f;
  repl_eval = eval_f;
  repl_print = print_f;
}
#endif

static LISP
siod_return_value (void)
{
  return repl_return_val;
}

static void
gput_st (struct gen_printio *f, char *st)
{
  PUTS_FCN (st, f);
}

static int
fputs_fcn (char *st, void *cb)
{
  fput_st ((FILE *) cb, st);
  return (1);
}

static void
set_fatal_exit_hook (void (*fcn) (void))
{
  fatal_exit_hook = fcn;
}

static LISP
last_pair (LISP l)
{
  LISP v1, v2;
  v1 = l;
  v2 = CONSP (v1) ? CDR (v1) : my_err ("bad arg to last", l);
  while (CONSP (v2))
    {
      INTERRUPT_CHECK ();
      v1 = v2;
      v2 = CDR (v2);
    }
  return (v1);
}

static LISP
nconc (LISP a, LISP b)
{
  if NULLP
    (a)
      return (b);
  setcdr (last_pair (a), b);
  return (a);
}

#if (NESTED_REPL_C_STRING)
void
siod_gc_protect_stack(LISP *stack_start)
{
  if (!stack_start_ptr) {
    stack_start_ptr = stack_start;
    stack_limit_ptr = STACK_LIMIT (stack_start, stack_size);
  }
}

void
siod_gc_unprotect_stack(LISP *stack_start)
{
  if (stack_start_ptr == stack_start)
    stack_start_ptr = NULL;
}
#endif  /* NESTED_REPL_C_STRING */

static LISP
stack_limit (LISP amount, LISP silent)
{
  if NNULLP
    (amount)
    {
      stack_size = get_c_int (amount);
      stack_limit_ptr = STACK_LIMIT (stack_start_ptr, stack_size);
    }
  if NULLP
    (silent)
    {
      sprintf (tkbuffer, "Stack_size = %ld bytes, [%p,%p]\n",
	       stack_size, (void *)stack_start_ptr, stack_limit_ptr);
      put_st (tkbuffer);
      return (NIL);
    }
  else
    return (intcons (stack_size));
}

static char *
get_c_string (LISP x)
{
  if TYPEP
    (x, tc_symbol)
      return (PNAME (x));
  else if TYPEP
    (x, tc_string)
      return (x->storage_as.string.data);
  else
    my_err ("not a symbol or string", x);
  return (NULL);
}

static char *
get_c_string_dim (LISP x, long *len)
{
  switch (TYPE (x))
    {
    case tc_symbol:
      *len = strlen (PNAME (x));
      return (PNAME (x));
    case tc_string:
      *len = x->storage_as.string.dim;
      return (x->storage_as.string.data);
    default:
      my_err ("not a symbol or string", x);
      return (NULL);
    }
}

static LISP
lerr (LISP message, LISP x)
{
  if (CONSP (message) && TYPEP (CAR (message), tc_string))
    my_err (NULL, message);
  else
    my_err (get_c_string (message), x);
  return (NIL);
}

static void
gc_fatal_error (void)
{
  my_err ("ran out of storage", NIL);
}

static LISP
cons (LISP x, LISP y)
{
  LISP z;
  NEWCELL (z, tc_cons);
  CAR (z) = x;
  CDR (z) = y;
  return (z);
}

static LISP
consp (LISP x)
{
  if CONSP
    (x) return (sym_t);
  else
    return (NIL);
}

static LISP
car (LISP x)
{
  switch TYPE
    (x)
    {
    case tc_nil:
      return (NIL);
    case tc_cons:
      return (CAR (x));
    default:
      return (my_err ("wta to car", x));
    }
}

static LISP
cdr (LISP x)
{
  switch TYPE
    (x)
    {
    case tc_nil:
      return (NIL);
    case tc_cons:
      return (CDR (x));
    default:
      return (my_err ("wta to cdr", x));
    }
}

static LISP
setcar (LISP cell, LISP value)
{
  if NCONSP
    (cell) my_err ("wta to setcar", cell);
  return (CAR (cell) = value);
}

static LISP
intcons (int x)
{
  LISP z;
  long n;
  if ((inums_dim > 0) &&
      ((x - (n = (long) x)) == 0) &&
      (x >= 0) &&
      (n < inums_dim))
    return (inums[n]);
  NEWCELL (z, tc_intnum);
  INTNM (z) = x;
  return (z);
}

static LISP
numberp (LISP x)
{
  if INTNUMP
    (x) return (sym_t);
  else
    return (NIL);
}


static LISP
ash (LISP value, LISP n)
{
  long m, k;
  m = get_c_int (value);
  k = get_c_int (n);
  if (k > 0)
    m = m << k;
  else
    m = m >> (-k);
  return (intcons (m));
}

static LISP
plus (LISP x, LISP y)
{
  if NULLP
    (y)
      return (NULLP (x) ? intcons (0) : x);
  if NINTNUMP
    (x) my_err ("wta(1st) to plus", x);
  if NINTNUMP
    (y) my_err ("wta(2nd) to plus", y);
  return (intcons (INTNM (x) + INTNM (y)));
}

static LISP
ltimes (LISP x, LISP y)
{
  if NULLP
    (y)
      return (NULLP (x) ? intcons (1) : x);
  if NINTNUMP
    (x) my_err ("wta(1st) to times", x);
  if NINTNUMP
    (y) my_err ("wta(2nd) to times", y);
  return (intcons (INTNM (x) * INTNM (y)));
}

static LISP
difference (LISP x, LISP y)
{
  if NINTNUMP
    (x) my_err ("wta(1st) to difference", x);
  if NULLP
    (y)
      return (intcons (-INTNM (x)));
  else
    {
      if NINTNUMP
	(y) my_err ("wta(2nd) to difference", y);
      return (intcons (INTNM (x) - INTNM (y)));
    }
}

static LISP
Quotient (LISP x, LISP y)
{
  if NINTNUMP
    (x) my_err ("wta(1st) to quotient", x);
  if NULLP
    (y)
      return (intcons (1 / INTNM (x))); /* XXX wrong number of arguments actually */
  else
    {
      if NINTNUMP
	(y) my_err ("wta(2nd) to quotient", y);
      if (INTNM(y) == 0)
	return (my_err ("divided by 0 in quotient", y));
      else
        return (intcons (INTNM (x) / INTNM (y)));
    }
}

static LISP
Remainder (LISP x, LISP y)
{
  if NINTNUMP
    (x) my_err ("wta(1st) to remainder", x);
  if NULLP
    (y)
      return (intcons (1 % INTNM (x))); /* XXX wrong number of arguments actually */
  else
    {
      if NINTNUMP
	(y) my_err ("wta(2nd) to remainder", y);
      if (INTNM(y) == 0)
	return (my_err ("dividev by 0 in remainder", y));
      else
        return (intcons (INTNM (x) % INTNM (y)));
    }
}

static LISP
lllabs (LISP x)
{
  double v;
  if NINTNUMP
    (x) my_err ("wta to abs", x);
  v = INTNM (x);
  if (v < 0)
    return (intcons (-v));
  else
    return (x);
}

static LISP
greaterp (LISP x, LISP y)
{
  if NINTNUMP
    (x) my_err ("wta(1st) to greaterp", x);
  if NINTNUMP
    (y) my_err ("wta(2nd) to greaterp", y);
  if (INTNM (x) > INTNM (y))
    return (sym_t);
  return (NIL);
}

static LISP
lessp (LISP x, LISP y)
{
  if NINTNUMP
    (x) my_err ("wta(1st) to lessp", x);
  if NINTNUMP
    (y) my_err ("wta(2nd) to lessp", y);
  if (INTNM (x) < INTNM (y))
    return (sym_t);
  return (NIL);
}

static LISP
greaterEp (LISP x, LISP y)
{
  if NINTNUMP
    (x) my_err ("wta(1st) to greaterp", x);
  if NINTNUMP
    (y) my_err ("wta(2nd) to greaterp", y);
  if (INTNM (x) >= INTNM (y))
    return (sym_t);
  return (NIL);
}

static LISP
lessEp (LISP x, LISP y)
{
  if NINTNUMP
    (x) my_err ("wta(1st) to lessp", x);
  if NINTNUMP
    (y) my_err ("wta(2nd) to lessp", y);
  if (INTNM (x) <= INTNM (y))
    return (sym_t);
  return (NIL);
}

static LISP
lmax (LISP x, LISP y)
{
  if NULLP
    (y) return (x);
  if NINTNUMP
    (x) my_err ("wta(1st) to max", x);
  if NINTNUMP
    (y) my_err ("wta(2nd) to max", y);
  return ((INTNM (x) > INTNM (y)) ? x : y);
}

static LISP
lmin (LISP x, LISP y)
{
  if NULLP
    (y) return (x);
  if NINTNUMP
    (x) my_err ("wta(1st) to min", x);
  if NINTNUMP
    (y) my_err ("wta(2nd) to min", y);
  return ((INTNM (x) < INTNM (y)) ? x : y);
}

static LISP
assoc (LISP x, LISP alist)
{
  LISP l, tmp;
  for (l = alist; CONSP (l); l = CDR (l))
    {
      tmp = CAR (l);
      if (CONSP (tmp) && equal (CAR (tmp), x))
	return (tmp);
      INTERRUPT_CHECK ();
    }
  if EQ
    (l, NIL) return (NIL);
  return (my_err ("improper list to assoc", alist));
}

static LISP
equal (LISP a, LISP b)
{
  struct user_type_hooks *p;
  long atype;
  STACK_CHECK (&a);
loop:
  INTERRUPT_CHECK ();
  if EQ
    (a, b) return (sym_t);
  atype = TYPE (a);
  if (atype != TYPE (b))
    return (NIL);
  switch (atype)
    {
    case tc_cons:
      if NULLP
	(equal (car (a), car (b))) return (NIL);
      a = cdr (a);
      b = cdr (b);
      goto loop;
    case tc_intnum:
      return ((INTNM (a) == INTNM (b)) ? sym_t : NIL);
    case tc_symbol:
      return (NIL);
    default:
      p = get_user_type_hooks (atype);
      if (p->equal)
	return ((*p->equal) (a, b));
      else
	return (NIL);
    }
}

static LISP
eq (LISP x, LISP y)
{
  if EQ
    (x, y) return (sym_t);
  else
    return (NIL);
}

static LISP
eql (LISP x, LISP y)
{
  if EQ
    (x, y) return (sym_t);
  else if NINTNUMP
    (x) return (NIL);
  else if NINTNUMP
    (y) return (NIL);
  else if (INTNM (x) == INTNM (y))
    return (sym_t);
  return (NIL);
}

#if 0
static LISP
inteql (LISP x, LISP y)
{
  if NINTNUMP
    (x) my_err ("number required", x);
  else if NINTNUMP
    (y) my_err ("number required", y);
  else if EQ
    (x, y) return (sym_t);
  else if (INTNM (x) == INTNM (y))
    return (sym_t);
  return (NIL);
}
#endif


static LISP
append2 (LISP l1, LISP l2)
{
  long n;
  LISP result = NIL, p1, p2;
  n = nlength (l1) + nlength (l2);
  while (n > 0)
    {
      result = cons (NIL, result);
      --n;
    }
  for (p1 = result, p2 = l1; NNULLP (p2); p1 = cdr (p1), p2 = cdr (p2))
    setcar (p1, car (p2));
  for (p2 = l2; NNULLP (p2); p1 = cdr (p1), p2 = cdr (p2))
    setcar (p1, car (p2));
  return (result);
}

static LISP
append (LISP l)
{
  STACK_CHECK (&l);
  INTERRUPT_CHECK ();
  if NULLP
    (l)
      return (NIL);
  else if NULLP
    (cdr (l))
      return (car (l));
  else if NULLP
    (cddr (l))
      return (append2 (car (l), cadr (l)));
  else
    return (append2 (car (l), append (cdr (l))));
}

static LISP
symcons (char *pname, LISP vcell)
{
  LISP z;
  NEWCELL (z, tc_symbol);
  PNAME (z) = pname;
  VCELL (z) = vcell;
  return (z);
}

static LISP
symbolp (LISP x)
{
  if SYMBOLP
    (x) return (sym_t);
  else
    return (NIL);
}

static LISP
err_ubv (LISP v)
{
  return (my_err ("unbound variable", v));
}

static LISP
symbol_boundp (LISP x, LISP env)
{
  LISP tmp;
  if NSYMBOLP
    (x) my_err ("not a symbol", x);
  tmp = envlookup (x, env);
  if NNULLP
    (tmp) return (sym_t);
  if EQ
    (VCELL (x), unbound_marker) return (NIL);
  else
    return (sym_t);
}

static LISP
symbol_value (LISP x, LISP env)
{
  LISP tmp;
  if NSYMBOLP
    (x) my_err ("not a symbol", x);
  tmp = envlookup (x, env);
  if NNULLP
    (tmp) return (CAR (tmp));
  tmp = VCELL (x);
  if EQ
    (tmp, unbound_marker) err_ubv (x);
  return (tmp);
}

static LISP
symbol_to_string (LISP x, LISP env)
{
  LISP tmp;
  if NSYMBOLP
    (x) my_err ("not a symbol", x);
  tmp = envlookup (x, env);
  if NNULLP
    (tmp) return (CAR (tmp));
  tmp = strcons(-1, PNAME (x));
  return (tmp);
}


static void *
must_malloc (unsigned long size)
{
  void *tmp;
  tmp = (void *) malloc ((size) ? size : 1);
  if (tmp == (void *) NULL)
    my_err ("failed to allocate storage from system", NIL);
  return (tmp);
}

static int
name_hash(const char *name)
{
  int hash = 0;
  int c;
  char *cname = (char *)name;
  while ((c = *cname++)) {
    hash = ((hash * 17) ^ c) % obarray_dim;
  }
  return hash;
}

static LISP
gen_intern (const char *name)
{
  LISP l, sym, sl;
  char *cname;
  long hash = 0;
  if (obarray_dim > 1)
    {
      hash = name_hash(name);
      sl = obarray[hash];
    }
  else
    sl = oblistvar;
  for (l = sl; NNULLP (l); l = CDR (l))
    if (strcmp (name, PNAME (CAR (l))) == 0)
      {
	return (CAR (l));
      }

  cname = (char *) must_malloc (strlen (name) + 1);
  strcpy (cname, name);

  sym = symcons (cname, unbound_marker);
  if (obarray_dim > 1)
    obarray[hash] = cons (sym, sl);
  oblistvar = cons (sym, oblistvar);
  return (sym);
}

static void
unlink_from_sym_list(const char *name, LISP *lst)
{
  LISP cur, victim = NIL;
  if (!strcmp(name, PNAME(CAR(*lst)))) {
    victim = *lst;
    *lst = CDR(*lst);
  } else {
    for (cur = *lst; cur && CDR(cur); cur = CDR(cur)) {
      if (!strcmp(name, PNAME(CAR(CDR(cur))))) {
	victim = CDR(cur);
	CDR(cur) = CDR(CDR(cur));
	break;
      }
    }
  }
}

static void
do_undefine(const char *name)
{
  int hash;
  unlink_from_sym_list(name, &oblistvar);
  if (obarray_dim > 1) {
      hash = name_hash(name);
      unlink_from_sym_list(name, &obarray[hash]);
  }
}

static LISP
undefine(LISP name_list, LISP env)
{
  LISP name;
  for (; name_list; name_list = cdr(name_list)) {
    name = car(name_list);
    if SYMBOLP(name) {
      do_undefine(PNAME(name));
    }
  }
  return NIL;
}

static LISP
rintern (const char *name)
{
  return (gen_intern (name));
}

static LISP
intern (LISP name)
{
  return (rintern (get_c_string (name)));
}

static LISP
subrcons (long type, char *name, SUBR_FUNC f)
{
  LISP z;
  NEWCELL (z, type);
  (*z).storage_as.subr.name = name;
  (*z).storage_as.subr0.f = f;
  return (z);
}

static LISP
closure (LISP env, LISP code)
{
  LISP z;
  NEWCELL (z, tc_closure);
  (*z).storage_as.closure.env = env;
  (*z).storage_as.closure.code = code;
  dbg_register_closure (z);
  return (z);
}

static LISP
procedurep (LISP x)
{
  switch (TYPE (x))
    {
    case tc_subr_0:
    case tc_subr_1:
    case tc_subr_2:
    case tc_subr_3:
    case tc_lsubr:
    case tc_fsubr:
    case tc_msubr:
    case tc_closure:
    case tc_subr_4:
    case tc_subr_5:
    case tc_subr_2n:
      return (sym_t);
    default:
      return (NIL);
    }
}

static void
gc_protect_n (LISP * location, long n)
{
  struct gc_protected *reg;
  reg = (struct gc_protected *) must_malloc (sizeof (struct gc_protected));
  (*reg).location = location;
  (*reg).length = n;
  (*reg).next = protected_registers;
  protected_registers = reg;
}

void
siod_gc_protect (LISP * location)
{
  gc_protect_n (location, 1);
}

static void
gc_protect_sym (LISP * location, char *st)
{
  *location = rintern (st);
  gc_protect (location);
}

/* This function will resurrect when we implement module loading
static void
gc_unprotect (LISP * location)
{
  struct gc_protected *reg;
  struct gc_protected *prev_reg;

  prev_reg = NULL;
  reg = protected_registers;

  while (reg)
    {
      if (location == reg->location)
	{
	  if (prev_reg)
	    prev_reg->next = reg->next;
	  if (reg == protected_registers)
	    protected_registers = protected_registers->next;

	  free (reg);
	  break;
	}

      prev_reg = reg;
      reg = reg->next;
    }
}
*/

static LISP
string_gc_mark (LISP ptr)
{
  return (NIL);
}

static void
string_gc_free (LISP ptr)
{
  free (ptr->storage_as.string.data);
}

static void
string_prin1 (LISP ptr, struct gen_printio *f)
{
  int j;
  switch (ptr->type)
    {
    case tc_string:
      gput_st (f, "\"");
      if (strcspn (ptr->storage_as.string.data, "\"\\\n\r\t\f\a\b\v") ==
	  strlen (ptr->storage_as.string.data))
	gput_st (f, ptr->storage_as.string.data);
      else
	{
	  int n, c;
	  char cbuff[3];
	  n = strlen (ptr->storage_as.string.data);
	  for (j = 0; j < n; ++j)
	    switch (c = ptr->storage_as.string.data[j])
	      {
	      case '\\':
	      case '"':
		cbuff[0] = '\\';
		cbuff[1] = c;
		cbuff[2] = 0;
		gput_st (f, cbuff);
		break;
	      case '\n':
		gput_st (f, "\\n");
		break;
	      case '\r':
		gput_st (f, "\\r");
		break;
	      case '\t':
		gput_st (f, "\\t");
		break;
	      case '\f':
		gput_st (f, "\\f");
		break;
	      case '\a':
		gput_st (f, "\\a");
		break;
	      case '\b':
		gput_st (f, "\\b");
		break;
	      case '\v':
		gput_st (f, "\\v");
		break;
	      default:
		cbuff[0] = c;
		cbuff[1] = 0;
		gput_st (f, cbuff);
		break;
	      }
	}
      gput_st (f, "\"");
      break;
    }
}


static LISP
err_wta_str (LISP exp)
{
  return (my_err ("not a string", exp));
}

static LISP
string_equal (LISP a, LISP b)
{
  long len;
  
  if NTYPEP(a, tc_string)
    return (err_wta_str(a));

  if NTYPEP(b, tc_string)
    return (err_wta_str(b));

  len = a->storage_as.string.dim;
  if (len != b->storage_as.string.dim)
    return (NIL);
  if (memcmp (a->storage_as.string.data, b->storage_as.string.data, len) == 0)
    return (sym_t);
  else
    return (NIL);
}

static void
set_print_hooks (long type, void (*fcn) (LISP, struct gen_printio *))
{
  struct user_type_hooks *p;
  p = get_user_type_hooks (type);
  p->prin1 = fcn;
}

static void
set_gc_hooks (long type,
	      LISP (*mark) (LISP),
	      void (*free) (LISP))
{
  struct user_type_hooks *p;
  p = get_user_type_hooks (type);
  p->gc_mark = mark;
  p->gc_free = free;
}

static void
init_storage_a (void)
{
  struct user_type_hooks *p;
  set_gc_hooks (tc_string,
		string_gc_mark,
		string_gc_free);
  set_print_hooks (tc_string, string_prin1);
  p = get_user_type_hooks (tc_string);
  p->equal = string_equal;
}

static void
file_prin1 (LISP ptr, struct gen_printio *f)
{
  char *name;
  name = ptr->storage_as.c_file.name;
  gput_st (f, "#<FILE ");
  sprintf (tkbuffer, " %p", (void *)ptr->storage_as.c_file.f);
  gput_st (f, tkbuffer);
  if (name)
    {
      gput_st (f, " ");
      gput_st (f, name);
    }
  gput_st (f, ">");
}

static void *
get_c_pointer (LISP x)
{
  if NPOINTERP
    (x) my_err ("not a C pointer", x);
  return (x->storage_as.c_pointer.data);
}

static LISP
ptrcons (void *ptr)
{
  LISP x;
  NEWCELL (x, tc_c_pointer);
  (*x).storage_as.c_pointer.data = ptr;
  return (x);
}

static void
pointer_prin1 (LISP ptr, struct gen_printio *f)
{
  void *c_ptr;
  c_ptr = ptr->storage_as.c_pointer.data;
  gput_st (f, "#<PTR ");
  sprintf (tkbuffer, " %p", c_ptr);
  gput_st (f, tkbuffer);
  gput_st (f, ">");
}

static C_FUNC
get_c_func_pointer (LISP x)
{
  if NFPOINTERP
    (x) my_err ("not a C function pointer", x);
  return (x->storage_as.c_func_pointer.func);
}

static LISP
funcptrcons (C_FUNC ptr)
{
  LISP x;
  NEWCELL (x, tc_c_func_pointer);
  (*x).storage_as.c_func_pointer.func = ptr;
  return (x);
}

static void
func_pointer_prin1 (LISP ptr, struct gen_printio *f)
{
  void *c_ptr;
#if 0
  c_ptr = (void *)ptr->storage_as.c_func_pointer.func;
#else
  /*
    to suppress warning about function pointer to object pointer, we
    use a dirty trick.  -- YamaKen 2005-01-12
  */
  c_ptr = ptr->storage_as.c_pointer.data;
#endif
  gput_st (f, "#<FUNC_PTR ");
  sprintf (tkbuffer, " %p", c_ptr);
  gput_st (f, tkbuffer);
  gput_st (f, ">");
}


static void
init_storage_1 (void)
{
  LISP ptr;
  long j;
  tkbuffer = (char *) must_malloc (TKBUFFERN + 1);
  if ((nheaps < 1))
    my_err ("invalid number of heaps", NIL);
  heaps = (LISP *) must_malloc (sizeof (LISP) * nheaps);
  for (j = 0; j < nheaps; ++j)
    heaps[j] = NULL;
  heaps[0] = (LISP) must_malloc (sizeof (struct obj) * heap_size);
  heap = heaps[0];
  memset(heap, 0, sizeof (struct obj) * heap_size);
  heap_end = heap + heap_size;
  freelist = NIL;
  gc_protect (&oblistvar);
  if (obarray_dim > 1)
    {
      obarray = (LISP *) must_malloc (sizeof (LISP) * obarray_dim);
      for (j = 0; j < obarray_dim; ++j)
	obarray[j] = NIL;
      gc_protect_n (obarray, obarray_dim);
    }
  unbound_marker = cons (rintern ("**unbound-marker**"), NIL);
  gc_protect (&unbound_marker);
  eof_val = cons (rintern ("eof"), NIL);
  gc_protect (&eof_val);
  gc_protect_sym (&sym_t, "t");
  gc_protect_sym (&sym_f, "f");
  setvar (sym_t, sym_t, NIL);
  setvar (rintern ("let"), rintern ("let-internal-macro"), NIL);
  setvar (rintern ("let*"), rintern ("let*-macro"), NIL);
  setvar (rintern ("letrec"), rintern ("letrec-macro"), NIL);
  gc_protect_sym (&sym_errobj, "errobj");
  setvar (sym_errobj, NIL, NIL);
  gc_protect_sym (&sym_catchall, "all");
  gc_protect_sym (&sym_progn, "begin");
  gc_protect_sym (&sym_lambda, "lambda");
  gc_protect_sym (&sym_else, "else");
  setvar (sym_else, sym_t, NIL);
  gc_protect_sym (&sym_quote, "quote");
  gc_protect_sym (&sym_dot, ".");
  gc_protect_sym (&sym_after_gc, "*after-gc*");
  setvar (sym_after_gc, NIL, NIL);
  gc_protect_sym (&sym_features, "features");
  setvar (sym_features, NIL, NIL);
  if (inums_dim > 0)
    {
      inums = (LISP *) must_malloc (sizeof (LISP) * inums_dim);
      for (j = 0; j < inums_dim; ++j)
	{
	  NEWCELL (ptr, tc_intnum);
	  INTNM (ptr) = j;
	  inums[j] = ptr;
	}
      gc_protect_n (inums, inums_dim);
    }
}

static void
init_storage (void)
{
#if (!NESTED_REPL_C_STRING)
  LISP stack_start;
  if (stack_start_ptr == NULL)
    stack_start_ptr = &stack_start;
#endif
  init_storage_1 ();
  init_storage_a ();
  set_gc_hooks (tc_c_file, 0, file_gc_free);
  set_print_hooks (tc_c_file, file_prin1);
  set_print_hooks (tc_c_pointer, pointer_prin1);
  set_print_hooks (tc_c_func_pointer, func_pointer_prin1);
}

static void
init_subr (const char *name, long type, SUBR_FUNC fcn)
{
  setvar (rintern (name), subrcons (type, (char *)name, fcn), NIL);
}

static void
init_subr_0 (const char *name, LISP (*fcn) (void))
{
  init_subr (name, tc_subr_0, (SUBR_FUNC) fcn);
}

static void
init_subr_1 (const char *name, LISP (*fcn) (LISP))
{
  init_subr (name, tc_subr_1, (SUBR_FUNC) fcn);
}

static void
init_subr_2 (const char *name, LISP (*fcn) (LISP, LISP))
{
  init_subr (name, tc_subr_2, (SUBR_FUNC) fcn);
}

static void
init_subr_2n (const char *name, LISP (*fcn) (LISP, LISP))
{
  init_subr (name, tc_subr_2n, (SUBR_FUNC) fcn);
}

static void
init_subr_3 (const char *name, LISP (*fcn) (LISP, LISP, LISP))
{
  init_subr (name, tc_subr_3, (SUBR_FUNC) fcn);
}

#if 0
static void
init_subr_4 (const char *name, LISP (*fcn) (LISP, LISP, LISP, LISP))
{
  init_subr (name, tc_subr_4, (SUBR_FUNC) fcn);
}

static void
init_subr_5 (const char *name, LISP (*fcn) (LISP, LISP, LISP, LISP, LISP))
{
  init_subr (name, tc_subr_5, (SUBR_FUNC) fcn);
}
#endif

static void
init_lsubr (const char *name, LISP (*fcn) (LISP))
{
  init_subr (name, tc_lsubr, (SUBR_FUNC) fcn);
}

static void
init_fsubr (const char *name, LISP (*fcn) (LISP, LISP))
{
  init_subr (name, tc_fsubr, (SUBR_FUNC) fcn);
}

static void
init_msubr (const char *name, LISP (*fcn) (LISP *, LISP *))
{
  init_subr (name, tc_msubr, (SUBR_FUNC) fcn);
}

static LISP
assq (LISP x, LISP alist)
{
  LISP l, tmp;
  for (l = alist; CONSP (l); l = CDR (l))
    {
      tmp = CAR (l);
      if (CONSP (tmp) && EQ (CAR (tmp), x))
	return (tmp);
      INTERRUPT_CHECK ();
    }
  if EQ
    (l, NIL) return (NIL);
  return (my_err ("improper list to assq", alist));
}

static LISP
allocate_aheap (void)
{
  long j;
  LISP ptr, end, next;
  for (j = 0; j < nheaps; ++j)
    if (!heaps[j])
      {
	if (gc_status_flag && (siod_verbose_level >= 4))
	  fprintf (siod_output, "[allocating heap %ld]\n", j);
	heaps[j] = (LISP) must_malloc (sizeof (struct obj) * heap_size);
	ptr = heaps[j];
	end = heaps[j] + heap_size;
	while (1)
	  {
	    (*ptr).type = tc_free_cell;
	    next = ptr + 1;
	    if (next < end)
	      {
		CDR (ptr) = next;
		ptr = next;
	      }
	    else
	      {
		CDR (ptr) = freelist;
		break;
	      }
	  }
	freelist = heaps[j];
	return (sym_t);
      }
  return (NIL);
}

static long
looks_pointerp (LISP p)
{
  long j;
  LISP h;
  for (j = 0; j < nheaps; ++j)
    if ((h = heaps[j]) &&
	(p >= h) &&
	(p < (h + heap_size)) &&
	(((((char *) p) - ((char *) h)) % sizeof (struct obj)) == 0) &&
	NTYPEP (p, tc_free_cell))
        return (1);
  return (0);
}


static void
gc_mark (LISP ptr)
{
  struct user_type_hooks *p;
gc_mark_loop:
  if NULLP
    (ptr) return;
  if ((*ptr).gc_mark)
    return;
  (*ptr).gc_mark = 1;
#if DEBUG_SCM
  gc_mark ((*ptr).dbg_info);
#endif
  switch ((*ptr).type)
    {
    case tc_intnum:
      break;
    case tc_cons:
      gc_mark (CAR (ptr));
      ptr = CDR (ptr);
      goto gc_mark_loop;
    case tc_symbol:
      ptr = VCELL (ptr);
      goto gc_mark_loop;
    case tc_closure:
      gc_mark ((*ptr).storage_as.closure.code);
      ptr = (*ptr).storage_as.closure.env;
      goto gc_mark_loop;
    case tc_subr_0:
    case tc_subr_1:
    case tc_subr_2:
    case tc_subr_2n:
    case tc_subr_3:
    case tc_subr_4:
    case tc_subr_5:
    case tc_lsubr:
    case tc_fsubr:
    case tc_msubr:
      break;
    default:
      p = get_user_type_hooks (TYPE (ptr));
      if (p->gc_mark)
	ptr = (*p->gc_mark) (ptr);
    }
}

static void
mark_locations_array (LISP * x, long n)
{
  int j;
  LISP p;
  for (j = 0; j < n; ++j)
    {
      p = x[j];
      if (looks_pointerp (p))
	gc_mark (p);
    }
}

static void
mark_locations (LISP * start, LISP * end)
{
  LISP *tmp;
  long n;
  if (start > end)
    {
      tmp = start;
      start = end;
      end = tmp;
    }
  n = end - start;
  mark_locations_array (start, n);
}


static void
free_a_cell(LISP ptr)
{
  struct user_type_hooks *p;
  switch ((*ptr).type)
    {
    case tc_free_cell:
    case tc_cons:
    case tc_closure:
    case tc_intnum:
    case tc_subr_0:
    case tc_subr_1:
    case tc_subr_2:
    case tc_subr_2n:
    case tc_subr_3:
    case tc_subr_4:
    case tc_subr_5:
    case tc_lsubr:
    case tc_fsubr:
    case tc_msubr:
      break;
    case tc_symbol:
      free(PNAME(ptr));
      break;
    default:
      p = get_user_type_hooks (TYPE (ptr));
      if (p->gc_free)
	(*p->gc_free) (ptr);
    }
}

static void
gc_sweep (void)
{
  LISP ptr, end, nfreelist, org;
  long s, n, k;
  end = heap_end;
  s = n = 0;
  nfreelist = NIL;
  for (k = 0; k < nheaps; ++k)
    if (heaps[k])
      {
	org = heaps[k];
	end = org + heap_size;
	for (ptr = org; ptr < end; ++ptr)
	  if (((*ptr).gc_mark == 0))
	    {
	      free_a_cell(ptr);
	      ++n;
	      (*ptr).type = tc_free_cell;
	      CDR (ptr) = nfreelist;
	      nfreelist = ptr;
	    }
	  else {
	    (*ptr).gc_mark = 0;
	    ++s;
	  }
      }
  gc_cells_swept = s;
  gc_cells_collected = n;
  freelist = nfreelist;
}

static void
mark_protected_registers (void)
{
  struct gc_protected *reg;
  LISP *location;
  long j, n;
  for (reg = protected_registers; reg; reg = (*reg).next)
    {
      location = (*reg).location;
      n = (*reg).length;
      for (j = 0; j < n; ++j)
	gc_mark (location[j]);
    }
}

static void
gc_ms_stats_start (void)
{
  gc_rt = myruntime ();
  gc_cells_collected = 0;
  if (gc_status_flag && (siod_verbose_level >= 4))
    fprintf (siod_output, "[starting GC]\n");
}

static void
gc_ms_stats_end (void)
{
  long n, i;
  for (n = i = 0; i < nheaps; ++i)
    if (heaps[i])
      ++n;

  gc_rt = myruntime () - gc_rt;
  gc_time_taken = gc_time_taken + gc_rt;
  if (gc_status_flag && (siod_verbose_level >= 4))
    fprintf (siod_output, "[GC took %g cpu seconds, %ld / %ld cells collected in %ld / %ld heaps]\n",
	     gc_rt,
	     gc_cells_collected,
	     gc_cells_swept,
	     n,
	     nheaps);
}

static void
gc_mark_and_sweep (void)
{
  LISP stack_end;
  gc_ms_stats_start ();
  while (heap < heap_end)
    {
      heap->type = tc_free_cell;
      heap->gc_mark = 0;
      ++heap;
    }
  setjmp (save_regs_gc_mark);
  mark_locations ((LISP *) (uintptr_t)save_regs_gc_mark,
      (LISP *) (((uintptr_t) save_regs_gc_mark) + sizeof (save_regs_gc_mark)));
  mark_protected_registers ();
  mark_locations ((LISP *) stack_start_ptr,
		  (LISP *) & stack_end);
  gc_sweep ();
  gc_ms_stats_end ();
}

static void
gc_for_newcell (void)
{
  if (heap < heap_end)
    {
      freelist = heap;
      CDR (freelist) = NIL;
      ++heap;
      return;
    }
  if (errjmp_ok == 0)
    gc_fatal_error ();
  errjmp_ok = 0;
  gc_mark_and_sweep ();
  errjmp_ok = 1;
  if (gc_cells_collected == 0)
    {
      if NULLP
	(allocate_aheap ())
	  gc_fatal_error ();
    }
  else if ((gc_cells_collected >= heap_alloc_threshold) && NNULLP (sym_after_gc))
    leval (leval (sym_after_gc, NIL), NIL);
  else
    allocate_aheap ();
}

static LISP
user_gc (LISP args)
{
  long old_status_flag;
  errjmp_ok = 0;
  old_status_flag = gc_status_flag;
  if NNULLP (args)
    {
      if NULLP (car (args)) 
	  gc_status_flag = 0;
      else
	gc_status_flag = 1;
    }
  gc_mark_and_sweep ();
  gc_status_flag = old_status_flag;
  errjmp_ok = 1;
  return (NIL);
}

static long
nactive_heaps (void)
{
  long m;
  for (m = 0; (m < nheaps) && heaps[m]; ++m);
  return (m);
}

static long
freelist_length (void)
{
  long n;
  LISP l;
  for (n = 0, l = freelist; NNULLP (l); ++n)
    l = CDR (l);
  n += (heap_end - heap);
  return (n);
}

static LISP
gc_status (LISP args)
{
  long n, m;
  if NNULLP (args)
    {
      if NULLP (car (args)) 
	gc_status_flag = 0;
      else
	gc_status_flag = 1;
    }

  if (gc_status_flag)
    put_st ("garbage collection verbose\n");
  else
    put_st ("garbage collection silent\n");
  {
    m = nactive_heaps ();
    n = freelist_length ();
    sprintf (tkbuffer, "%ld/%ld heaps, %ld allocated %ld free\n",
	     m, nheaps, m * heap_size - n, n);
    put_st (tkbuffer);
  }

  return (NIL);
}

static LISP
gc_info (LISP arg)
{
  switch (get_c_int (arg))
    {
    case 0:
      return NIL;
    case 1:
      return (intcons (nactive_heaps ()));
    case 2:
      return (intcons (nheaps));
    case 3:
      return (intcons (heap_size));
    case 4:
      return (intcons (freelist_length ()));
    default:
      return (NIL);
    }
}

static LISP
leval_args (LISP l, LISP env)
{
  LISP result, v1, v2, tmp;
  if NULLP
    (l) return (NIL);
  if NCONSP
    (l) my_err ("bad syntax argument list", l);
  result = cons (leval (CAR (l), env), NIL);
  for (v1 = result, v2 = CDR (l);
       CONSP (v2);
       v1 = tmp, v2 = CDR (v2))
    {
      tmp = cons (leval (CAR (v2), env), NIL);
      CDR (v1) = tmp;
    }
  if NNULLP
    (v2) my_err ("bad syntax argument list", l);
  return (result);
}

static LISP
extend_env (LISP actuals, LISP formals, LISP env)
{
  if SYMBOLP
    (formals)
      return (cons (cons (cons (formals, NIL), cons (actuals, NIL)), env));
  return (cons (cons (formals, actuals), env));
}

#if 0
static void
set_eval_hooks (long type, LISP (*fcn) (LISP, LISP *, LISP *))
{
  struct user_type_hooks *p;
  p = get_user_type_hooks (type);
  p->leval = fcn;
}
#endif

static LISP
err_closure_code (LISP tmp)
{
  return (my_err ("closure code type not valid", tmp));
}

/* main evaluator */
static LISP
leval (LISP x, LISP env)
{
  LISP tmp, arg1;
  LISP rval;
  struct user_type_hooks *p;
  struct func_frame this_frame;
  STACK_CHECK (&x);
  this_frame.prev = func_trace;
  this_frame.obj = x;
  func_trace = &this_frame;
loop:
  INTERRUPT_CHECK ();
  switch TYPE
    (x)
    {
    case tc_symbol:
      tmp = envlookup (x, env);
      if NNULLP
	(tmp) {
	rval = (CAR (tmp));
	goto ret;
      }
      tmp = VCELL (x);
      if EQ
	(tmp, unbound_marker) err_ubv (x);
      rval = tmp;
      goto ret;

    case tc_cons:
      tmp = CAR (x);
      switch TYPE
	(tmp)
	{
	case tc_symbol:
	  tmp = envlookup (tmp, env);
	  if NNULLP
	    (tmp)
	    {
	      tmp = CAR (tmp);
	      break;
	    }
	  tmp = VCELL (CAR (x));
	  if EQ
	    (tmp, unbound_marker) err_ubv (CAR (x));
	  break;
	case tc_cons:
	  tmp = leval (tmp, env);
	  break;
	}
      switch TYPE
	(tmp)
	{
	case tc_subr_0:
	  rval = (SUBR0 (tmp) ());
	  goto ret;
	case tc_subr_1:
	  rval = (SUBR1 (tmp) (leval (car (CDR (x)), env)));
	  goto ret;
	case tc_subr_2:
	  x = CDR (x);
	  arg1 = leval (car (x), env);
	  x = NULLP (x) ? NIL : CDR (x);
	  rval = (SUBR2 (tmp) (arg1,
			       leval (car (x), env)));
	  goto ret;
	case tc_subr_2n:
	  x = CDR (x);
	  arg1 = leval (car (x), env);
	  x = NULLP (x) ? NIL : CDR (x);
	  arg1 = SUBR2 (tmp) (arg1,
			      leval (car (x), env));
	  for (x = cdr (x); CONSP (x); x = CDR (x))
	    arg1 = SUBR2 (tmp) (arg1, leval (CAR (x), env));
	  rval = (arg1);
	  goto ret;
	case tc_subr_3:
	  x = CDR (x);
	  arg1 = leval (car (x), env);
	  x = NULLP (x) ? NIL : CDR (x);
	  rval = (SUBR3 (tmp) (arg1,
			       leval (car (x), env),
			       leval (car (cdr (x)), env)));
	  goto ret;

	case tc_subr_4:
	  x = CDR (x);
	  arg1 = leval (car (x), env);
	  x = NULLP (x) ? NIL : CDR (x);
	  rval = (SUBR4 (tmp) (arg1,
			       leval (car (x), env),
			       leval (car (cdr (x)), env),
			       leval (car (cdr (cdr (x))), env)));
	  goto ret;

	case tc_subr_5:
	  x = CDR (x);
	  arg1 = leval (car (x), env);
	  x = NULLP (x) ? NIL : CDR (x);
	  rval = (SUBR5 (tmp) (arg1,
			      leval (car (x), env),
			      leval (car (cdr (x)), env),
			      leval (car (cdr (cdr (x))), env),
			      leval (car (cdr (cdr (cdr (x)))), env)));
	  goto ret;

	case tc_lsubr:
	  rval = (SUBR1 (tmp) (leval_args (CDR (x), env)));
	  goto ret;
	case tc_fsubr:
	  rval = (SUBR2 (tmp) (CDR (x), env));
	  goto ret;
	case tc_msubr:
	  if NULLP
	    (SUBRM (tmp) (&x, &env)) {
	    rval = x;
	    goto ret;
	  }
	  goto loop;
	case tc_closure:
	  switch TYPE
	    ((*tmp).storage_as.closure.code)
	    {
	    case tc_cons:
	      env = extend_env (leval_args (CDR (x), env),
				CAR ((*tmp).storage_as.closure.code),
				(*tmp).storage_as.closure.env);
	      x = CDR ((*tmp).storage_as.closure.code);
	      goto loop;
	    case tc_subr_1:
	      rval = (SUBR1 (tmp->storage_as.closure.code)
		      (tmp->storage_as.closure.env));
	      goto ret;
	    case tc_subr_2:
	      x = CDR (x);
	      arg1 = leval (car (x), env);
	      rval = (SUBR2 (tmp->storage_as.closure.code)
		      (tmp->storage_as.closure.env, arg1));
	      goto ret;
	    case tc_subr_3:
	      x = CDR (x);
	      arg1 = leval (car (x), env);
	      x = NULLP (x) ? NIL : CDR (x);
	      rval = (SUBR3 (tmp->storage_as.closure.code)
		      (tmp->storage_as.closure.env,
		       arg1,
		       leval (car (x), env)));
	      goto ret;
	    case tc_subr_4:
	      x = CDR (x);
	      arg1 = leval (car (x), env);
	      x = NULLP (x) ? NIL : CDR (x);
	      rval = (SUBR4 (tmp->storage_as.closure.code)
		      (tmp->storage_as.closure.env,
		       arg1,
		       leval (car (x), env),
		       leval (car (cdr (x)), env)));
	      goto ret;
	    case tc_subr_5:
	      x = CDR (x);
	      arg1 = leval (car (x), env);
	      x = NULLP (x) ? NIL : CDR (x);
	      rval = (SUBR5 (tmp->storage_as.closure.code)
		      (tmp->storage_as.closure.env,
		       arg1,
		       leval (car (x), env),
		       leval (car (cdr (x)), env),
		       leval (car (cdr (cdr (x))), env)));
	      goto ret;

	    case tc_lsubr:
	      rval = (SUBR1 (tmp->storage_as.closure.code)
		      (cons (tmp->storage_as.closure.env,
			     leval_args (CDR (x), env))));
	      goto ret;
	    default:
	      err_closure_code (tmp);
	    }
	  break;
	case tc_symbol:
	  x = cons (tmp, cons (cons (sym_quote, cons (x, NIL)), NIL));
	  x = leval (x, NIL);
	  goto loop;
	default:
	  p = get_user_type_hooks (TYPE (tmp));
	  if (p->leval)
	    {
	      if NULLP
		((*p->leval) (tmp, &x, &env)) {
		rval = x;
		goto ret;
	      } else
		goto loop;
	    }
	  my_err ("bad function", tmp);
	}
    default:
      rval = x;
      goto ret;
    }
 ret:
  func_trace = this_frame.prev;
  return rval;
}

static LISP
lapply (LISP fcn, LISP args)
{
  struct user_type_hooks *p;
  LISP acc;
  STACK_CHECK (&fcn);
  INTERRUPT_CHECK ();
  switch TYPE
    (fcn)
    {
    case tc_subr_0:
      return (SUBR0 (fcn) ());
    case tc_subr_1:
      return (SUBR1 (fcn) (car (args)));
    case tc_subr_2:
      return (SUBR2 (fcn) (car (args), car (cdr (args))));
    case tc_subr_2n:
      acc = SUBR2 (fcn) (car (args), car (cdr (args)));
      for (args = cdr (cdr (args)); CONSP (args); args = CDR (args))
	acc = SUBR2 (fcn) (acc, CAR (args));
      return (acc);
    case tc_subr_3:
      return (SUBR3 (fcn) (car (args), car (cdr (args)), car (cdr (cdr (args)))));
    case tc_subr_4:
      return (SUBR4 (fcn) (car (args), car (cdr (args)), car (cdr (cdr (args))),
			   car (cdr (cdr (cdr (args))))));
    case tc_subr_5:
      return (SUBR5 (fcn) (car (args), car (cdr (args)), car (cdr (cdr (args))),
			   car (cdr (cdr (cdr (args)))),
			   car (cdr (cdr (cdr (cdr (args)))))));
    case tc_lsubr:
      return (SUBR1 (fcn) (args));
    case tc_fsubr:
    case tc_msubr:
    case tc_symbol:
      my_err ("cannot be applied", fcn);
    case tc_closure:
      switch TYPE
	(fcn->storage_as.closure.code)
	{
	case tc_cons:
	  return (leval (cdr (fcn->storage_as.closure.code),
			 extend_env (args,
				     car (fcn->storage_as.closure.code),
				     fcn->storage_as.closure.env)));
	case tc_subr_1:
	  return (SUBR1 (fcn->storage_as.closure.code)
		  (fcn->storage_as.closure.env));
	case tc_subr_2:
	  return (SUBR2 (fcn->storage_as.closure.code)
		  (fcn->storage_as.closure.env,
		   car (args)));
	case tc_subr_3:
	  return (SUBR3 (fcn->storage_as.closure.code)
		  (fcn->storage_as.closure.env,
		   car (args), car (cdr (args))));
	case tc_subr_4:
	  return (SUBR4 (fcn->storage_as.closure.code)
		  (fcn->storage_as.closure.env,
		   car (args), car (cdr (args)), car (cdr (cdr (args)))));
	case tc_subr_5:
	  return (SUBR5 (fcn->storage_as.closure.code)
		  (fcn->storage_as.closure.env,
		   car (args), car (cdr (args)), car (cdr (cdr (args))),
		   car (cdr (cdr (cdr (args))))));
	case tc_lsubr:
	  return (SUBR1 (fcn->storage_as.closure.code)
		  (cons (fcn->storage_as.closure.env, args)));
	default:
	  err_closure_code (fcn);
	}
    default:
      p = get_user_type_hooks (TYPE (fcn));
      if (p->leval)
	return my_err ("have eval, dont know apply", fcn);
      else
	return my_err ("cannot be applied", fcn);
    }
}

static LISP
leval_setq (LISP args, LISP env)
{
  if (symbol_boundp( car(args), env) == sym_t) {
    return (setvar (car (args), leval (car (cdr (args)), env), env));
  } else {
    my_err ("unbound variable", car(args));
    return NIL;
  }
}

static LISP
syntax_define (LISP args)
{
  if SYMBOLP
    (car (args)) return (args);
  return (syntax_define (
			  cons (car (car (args)),
				cons (cons (sym_lambda,
					    cons (cdr (car (args)),
						  cdr (args))),
				      NIL))));
}

static LISP
leval_define (LISP args, LISP env)
{
  LISP tmp, var, val;
  tmp = syntax_define (args);
  var = car (tmp);
  if NSYMBOLP
    (var) my_err ("wta(non-symbol) to define", var);
  val = leval (car (cdr (tmp)), env);
  tmp = envlookup (var, env);
  if NNULLP
    (tmp) return (CAR (tmp) = val);
  if NULLP
    (env) return (VCELL (var) = val);
  tmp = car (env);
  setcar (tmp, cons (var, car (tmp)));
  setcdr (tmp, cons (val, cdr (tmp)));
  return (val);
}

static LISP
leval_if (LISP * pform, LISP * penv)
{
  LISP args, env;
  args = cdr (*pform);
  env = *penv;
  if NNULLP
    (leval (car (args), env))
      * pform = car (cdr (args));
  else
    *pform = car (cdr (cdr (args)));
  return (sym_t);
}

static LISP
arglchk (LISP x)
{
#if (!ENVLOOKUP_TRICK)
  LISP l;
  if SYMBOLP
    (x) return (x);
  for (l = x; CONSP (l); l = CDR (l));
  if NNULLP
    (l) my_err ("improper formal argument list", x);
#endif
  return (x);
}

static LISP
leval_lambda (LISP args, LISP env)
{
  LISP body;
#if ! DEBUG_SCM
  /* the debugger needs the body to be a list */
  if NULLP
    (cdr (cdr (args)))
      body = car (cdr (args));
  else
#endif
    body = cons (sym_progn, cdr (args));
  return (closure (env, cons (arglchk (car (args)), body)));
}

static LISP
leval_progn (LISP * pform, LISP * penv)
{
  LISP env, l, next;
  env = *penv;
  l = cdr (*pform);
  next = cdr (l);
  while (NNULLP (next))
    {
      leval (car (l), env);
      l = next;
      next = cdr (next);
    }
  *pform = car (l);
  return (sym_t);
}

static LISP
leval_or (LISP * pform, LISP * penv)
{
  LISP env, l, next, val;
  env = *penv;
  l = cdr (*pform);
  next = cdr (l);
  while (NNULLP (next))
    {
      val = leval (car (l), env);
      if NNULLP
	(val)
	{
	  *pform = val;
	  return (NIL);
	}
      l = next;
      next = cdr (next);
    }
  *pform = car (l);
  return (sym_t);
}

static LISP
leval_and (LISP * pform, LISP * penv)
{
  LISP env, l, next;
  env = *penv;
  l = cdr (*pform);
  if NULLP
    (l)
    {
      *pform = sym_t;
      return (NIL);
    }
  next = cdr (l);
  while (NNULLP (next))
    {
      if NULLP
	(leval (car (l), env))
	{
	  *pform = NIL;
	  return (NIL);
	}
      l = next;
      next = cdr (next);
    }
  *pform = car (l);
  return (sym_t);
}

static LISP
leval_catch_1 (LISP forms, LISP env)
{
  LISP l, val = NIL;
  for (l = forms; NNULLP (l); l = cdr (l))
    val = leval (car (l), env);
  catch_framep = catch_framep->next;
  return (val);
}

static LISP
leval_catch (LISP args, LISP env)
{
  struct catch_frame frame;
  struct func_frame *cur_func;
  int k;
  frame.tag = leval (car (args), env);
  frame.next = catch_framep;
  cur_func = func_trace;
  k = setjmp (frame.cframe);
  catch_framep = &frame;
  if (k == 2)
    {
      catch_framep = frame.next;
      func_trace = cur_func;
      return (frame.retval);
    }
  return (leval_catch_1 (cdr (args), env));
}

static LISP
lthrow (LISP tag, LISP value)
{
  struct catch_frame *l;
  for (l = catch_framep; l; l = (*l).next)
    if (EQ ((*l).tag, tag) ||
	EQ ((*l).tag, sym_catchall))
      {
	(*l).retval = value;
	longjmp ((*l).cframe, 2);
      }
  my_err ("no *catch found with this tag", tag);
  return (NIL);
}

static LISP
leval_let (LISP * pform, LISP * penv)
{
  LISP env, l;
  l = cdr (*pform);
  env = *penv;
  *penv = extend_env (leval_args (car (cdr (l)), env), car (l), env);
  *pform = car (cdr (cdr (l)));
  return (sym_t);
}

static LISP
letstar_macro (LISP form)
{
  LISP bindings = cadr (form);
  if (NNULLP (bindings) && NNULLP (cdr (bindings)))
    {
      setcdr (form, cons (cons (car (bindings), NIL),
			  cons (cons (rintern ("let*"),
				      cons (cdr (bindings),
					    cddr (form))),
				NIL)));
#if DEBUG_SCM
      /* (let (bind1) (let* (bind2+) body)) */
      CDR (form)->dbg_info = bindings->dbg_info;
      CAR (CDR (form))->dbg_info = bindings->dbg_info;
      CDR (CDR (form))->dbg_info = CDR (bindings)->dbg_info;
      CAR (CDR (CDR (form)))->dbg_info = CDR (bindings)->dbg_info;
      CDR (CAR (CDR (CDR (form))))->dbg_info = CDR (bindings)->dbg_info;
      CAR (CDR (CAR (CDR (CDR (form)))))->dbg_info = CDR (bindings)->dbg_info;
#endif
    }
  setcar (form, rintern ("let"));
  return (form);
}

static LISP
reverse (LISP l)
{
  LISP n, p;
  n = NIL;
  for (p = l; NNULLP (p); p = cdr (p))
    n = cons (car (p), n);
  return (n);
}

static LISP
split_to_name_and_value (LISP bindings)
{
  LISP fl, al, binding;
  fl = NIL;
  al = NIL;
  for (; NNULLP (bindings); bindings = cdr (bindings))
    {
      binding = car (bindings);
      if SYMBOLP
	(binding)
	{
	  fl = cons (binding, fl);
	  al = cons (NIL, al);
	}
      else
	{
	  fl = cons (car (binding), fl);
	  al = cons (cadr (binding), al);
	}
    }
  return (cons (fl, al));
}

static LISP
named_let_macro (LISP form)
{
  LISP name, fl, al, bindings, body;
#if DEBUG_SCM
  LISP orgbind = car (cddr (form));
#endif
  
  bindings = split_to_name_and_value (car (cddr (form)));
  fl = car (bindings);
  al = cdr (bindings);
  
  name = cadr (form);
  body = cdr (cddr (form));
  
  setcar (form,
          listn (3,
                 rintern ("letrec"),               
                 listn (1,
                        listn (2,
                               name,
                               cons (sym_lambda, cons (reverse (fl), body)))),
                 name));
  setcdr (form, reverse (al));
#if DEBUG_SCM
  /* (let name (orgbind) body) */
  /* ((letrec ((name (lambda vars body))) name) inits) */
  if NNULLP
    (orgbind)
    {
      al = CDR (form);
      fl = orgbind;
      for (; NNULLP (al); al = CDR (al), fl = CDR (fl))
	{
	  if NNULLP
	    (cdar (fl))
	      al->dbg_info = CDR (CAR (fl))->dbg_info;
	}
    }
  al = dbg_get_info (body);
  form->dbg_info = al;
  CDR (CAR (form))->dbg_info = al;
  CDR (CAR (CAR (CDR (CAR (form)))))->dbg_info = al;
  CDR (CDR (CAR (CDR (CAR (CAR (CDR (CAR (form))))))))->dbg_info = al;
#endif
  return (form);
}

static LISP
normal_let_macro (LISP form)
{
  LISP fl, al, bindings, body;
#if DEBUG_SCM
  LISP orgbind = cadr (form);
#endif

  bindings = split_to_name_and_value (cadr (form));
  fl = car (bindings);
  al = cdr (bindings);
  
  body = cddr (form);
#if ! DEBUG_SCM
  if NULLP
    (cdr (body)) body = car (body);
  else
#endif
    body = cons (sym_progn, body);
  setcdr (form, cons (reverse (fl), cons (reverse (al), cons (body, NIL))));
  setcar (form, rintern ("let-internal"));
#if DEBUG_SCM
  if NNULLP
    (orgbind)
    {
      CDR (CDR (form))->dbg_info = orgbind->dbg_info;
      al = CAR (CDR (CDR (form)));
      fl = orgbind;
      for (; NNULLP (al); al = CDR (al), fl = CDR (fl))
	{
	  if NNULLP
	    (cdar (fl))
	      al->dbg_info = CDR (CAR (fl))->dbg_info;
	}
    }
  CDR (CDR (CDR (form)))->dbg_info = dbg_get_info (CDR (body));
#endif
  return (form);
}

static LISP
let_macro (LISP form)
{
  if SYMBOLP
    (cadr (form))
      return (named_let_macro (form));
  else
    return (normal_let_macro (form));
}

static LISP
leval_quote (LISP args, LISP env)
{
  return (car (args));
}

static LISP
leval_tenv (LISP args, LISP env)
{
  return (env);
}

static LISP
leval_while (LISP args, LISP env)
{
  LISP l;
  while NNULLP
    (leval (car (args), env))
      for (l = cdr (args); NNULLP (l); l = cdr (l))
      leval (car (l), env);
  return (NIL);
}

static LISP
symbolconc (LISP args)
{
  long size;
  LISP l, s;
  size = 0;
  tkbuffer[0] = 0;
  for (l = args; NNULLP (l); l = cdr (l))
    {
      s = car (l);
      if NSYMBOLP
	(s) my_err ("wta(non-symbol) to symbolconc", s);
      size = size + strlen (PNAME (s));
      if (size > TKBUFFERN)
	my_err ("symbolconc buffer overflow", NIL);
      strcat (tkbuffer, PNAME (s));
    }
  return (rintern (tkbuffer));
}

static char *
subr_kind_str (long n)
{
  switch (n)
    {
    case tc_subr_0:
      return ("subr_0");
    case tc_subr_1:
      return ("subr_1");
    case tc_subr_2:
      return ("subr_2");
    case tc_subr_2n:
      return ("subr_2n");
    case tc_subr_3:
      return ("subr_3");
    case tc_subr_4:
      return ("subr_4");
    case tc_subr_5:
      return ("subr_5");
    case tc_lsubr:
      return ("lsubr");
    case tc_fsubr:
      return ("fsubr");
    case tc_msubr:
      return ("msubr");
    default:
      return ("???");
    }
}

static LISP
lprin1g (LISP exp, struct gen_printio * f)
{
  LISP tmp;
  long n;
  struct user_type_hooks *p;
  STACK_CHECK (&exp);
  INTERRUPT_CHECK ();
  switch TYPE
    (exp)
    {
    case tc_nil:
      gput_st (f, "()");
      break;
    case tc_cons:
      gput_st (f, "(");
      lprin1g (car (exp), f);
      for (tmp = cdr (exp); CONSP (tmp); tmp = cdr (tmp))
	{
	  gput_st (f, " ");
	  lprin1g (car (tmp), f);
	}
      if NNULLP
	(tmp)
	{
	  gput_st (f, " . ");
	  lprin1g (tmp, f);
	}
      gput_st (f, ")");
      break;
    case tc_intnum:
      n = (long) INTNM (exp);
      sprintf (tkbuffer, "%ld", n);
      gput_st (f, tkbuffer);
      break;
    case tc_symbol:
      gput_st (f, PNAME (exp));
      break;
    case tc_subr_0:
    case tc_subr_1:
    case tc_subr_2:
    case tc_subr_2n:
    case tc_subr_3:
    case tc_subr_4:
    case tc_subr_5:
    case tc_lsubr:
    case tc_fsubr:
    case tc_msubr:
      sprintf (tkbuffer, "#<%s ", subr_kind_str (TYPE (exp)));
      gput_st (f, tkbuffer);
      gput_st (f, (*exp).storage_as.subr.name);
      gput_st (f, ">");
      break;
    case tc_closure:
      gput_st (f, "#<CLOSURE ");
      if CONSP
	((*exp).storage_as.closure.code)
	{
	  lprin1g (car ((*exp).storage_as.closure.code), f);
	  gput_st (f, " ");
	  lprin1g (cdr ((*exp).storage_as.closure.code), f);
	}
      else
	lprin1g ((*exp).storage_as.closure.code, f);
      gput_st (f, ">");
      break;
    default:
      p = get_user_type_hooks (TYPE (exp));
      if (p->prin1)
	(*p->prin1) (exp, f);
      else
	{
	  sprintf (tkbuffer, "#<UNKNOWN %d %p>", TYPE (exp), (void *)exp);
	  gput_st (f, tkbuffer);
	}
    }
  return (NIL);
}

static int
pts_puts (char *from, void *cb)
{
  LISP into;
  size_t fromlen, intolen, intosize, fitsize;
  into = (LISP) cb;
  fromlen = strlen (from);
  intolen = strlen (into->storage_as.string.data);
  intosize = into->storage_as.string.dim - intolen;
  fitsize = (fromlen < intosize) ? fromlen : intosize;
  memcpy (&into->storage_as.string.data[intolen], from, fitsize);
  into->storage_as.string.data[intolen + fitsize] = 0;
  if (fitsize < fromlen)
    my_err ("print to string overflow", NIL);
  return (1);
}


static LISP
string_length (LISP string)
{
  if NTYPEP
    (string, tc_string) err_wta_str (string);
  return (intcons (strlen (string->storage_as.string.data)));
}


static LISP
string_dim (LISP string)
{
  if NTYPEP
    (string, tc_string) err_wta_str (string);
  return (intcons ((double) string->storage_as.string.dim));
}

static LISP
print_to_string (LISP exp, LISP str, LISP nostart)
{
  struct gen_printio s;
  if NTYPEP
    (str, tc_string) err_wta_str (str);
  s.putc_fcn = NULL;
  s.puts_fcn = pts_puts;
  s.cb_argument = str;
  if NULLP
    (nostart)
      str->storage_as.string.data[0] = 0;
  lprin1g (exp, &s);
  return (str);
}

static LISP
lprint (LISP exp, LISP lf)
{
  FILE *f = get_c_file (lf, siod_output);
  lprin1f (exp, f);
  if (siod_verbose_level > 0)
    fput_st (f, "\n");
  return (NIL);
}

static LISP
lprin1f (LISP exp, FILE * f)
{
  struct gen_printio s;
  s.putc_fcn = NULL;
  s.puts_fcn = fputs_fcn;
  s.cb_argument = f;
  lprin1g (exp, &s);
  return (NIL);
}

static LISP
lread (LISP f)
{
  return (lreadf (get_c_file (f, stdin)));
}

static int
f_getc (FILE * f)
{
  int c;
  c = getc (f);
  return (c);
}

static void
f_ungetc (int c, FILE * f)
{
  ungetc (c, f);
}

static LISP
lreadf (FILE * f)
{
  struct gen_readio s;
  s.getc_fcn = (int (*)(void *)) f_getc;
  s.ungetc_fcn = (void (*)(int, void *)) f_ungetc;
  s.cb_argument = (char *) f;
  return (readtl (&s));
}

static LISP
lreadtk (char *buffer, long j)
{
  int flag;
  LISP tmp;
  int adigit;
  char *p = buffer;
  p[j] = 0;
  if (user_readt != NULL)
    {
      tmp = (*user_readt) (p, j, &flag);
      if (flag)
	return (tmp);
    }
  if (*p == '-')
    p += 1;
  adigit = 0;
  while (isdigit ((int)*p))
    {
      p += 1;
      adigit = 1;
    }
  if (*p == '.')
    {
      p += 1;
      while (isdigit ((int)*p))
	{
	  p += 1;
	  adigit = 1;
	}
    }
  if (!adigit)
    goto a_symbol;
  if (*p == 'e')
    {
      p += 1;
      if (*p == '-' || *p == '+')
	p += 1;
      if (!isdigit ((int)*p))
	goto a_symbol;
      else
	p += 1;
      while (isdigit ((int)*p))
	p += 1;
    }
  if (*p)
    goto a_symbol;
  return (intcons (atof (buffer)));
a_symbol:
  return (rintern (buffer));
}

static LISP
copy_list (LISP x)
{
  if NULLP
    (x) return (NIL);
  STACK_CHECK (&x);
  return (cons (car (x), copy_list (cdr (x))));
}

static LISP
apropos (LISP matchl)
{
  LISP result = NIL, l, ml;
  char *pname;
  for (l = oblistvar; CONSP (l); l = CDR (l))
    {
      pname = get_c_string (CAR (l));
      ml = matchl;
      while (CONSP (ml) && strstr (pname, get_c_string (CAR (ml))))
	ml = CDR (ml);
      if NULLP
	(ml)
	  result = cons (CAR (l), result);
    }
  return (result);
}

static LISP
delq (LISP elem, LISP l)
{
  if NULLP
    (l) return (l);
  STACK_CHECK (&elem);
  if EQ
    (elem, car (l)) return (delq (elem, cdr (l)));
  setcdr (l, delq (elem, cdr (l)));
  return (l);
}


static LISP
memq (LISP x, LISP il)
{
  LISP l, tmp;
  for (l = il; CONSP (l); l = CDR (l))
    {
      tmp = CAR (l);
      if EQ
	(x, tmp) return (l);
      INTERRUPT_CHECK ();
    }
  if EQ
    (l, NIL) return (NIL);
  return (my_err ("improper list to memq", il));
}

static LISP
featurep (LISP name)
{
  return memq (name, leval (sym_features, NIL));
}

static LISP
provide (LISP name)
{
  if NSYMBOLP
    (name) {
    my_err ("wta(non-symbol) to provide", name);
    return NIL;
  }
  if (featurep(name) == NIL)
    setvar (sym_features, cons (name, leval (sym_features, NIL)), NIL);
  return (NIL);
}

static void
siod_c_provide(const char *name)
{
  provide(rintern(name));
}

static LISP
load (LISP fname, LISP cflag, LISP rflag)
{
  long len;
  char *s1, *s2;
  s1 = get_c_string_dim(fname, &len);
  s2 = alloca(len + 1);
  strlcpy(s2, s1, len + 1);
  return (vload (s2, NULLP (cflag) ? 0 : 1,
		 1));
}

static LISP
require (LISP fname)
{
  LISP sym;
  sym = intern (string_append (cons (rintern ("*"),
				     cons (fname,
				       cons (rintern ("-loaded*"), NIL)))));
  if (NULLP (symbol_boundp (sym, NIL)) ||
      NULLP (symbol_value (sym, NIL)))
    {
      load (fname, NIL, sym_t);
      setvar (sym, sym_t, NIL);
    }
  return (sym);
}

static LISP
quit (void)
{
  return (my_err (NULL, NIL));
}

static LISP
nullp (LISP x)
{
  if EQ
    (x, NIL) return (sym_t);
  else
    return (NIL);
}

static LISP
stringp (LISP x)
{
  return (TYPEP (x, tc_string) ? sym_t : NIL);
}

static FILE *
get_c_file (LISP p, FILE * deflt)
{
  if (NULLP (p) && deflt)
    return (deflt);
  if NTYPEP
    (p, tc_c_file) my_err ("not a file", p);
  if (!p->storage_as.c_file.f)
    my_err ("file is closed", p);
  return (p->storage_as.c_file.f);
}

static LISP
lgetc (LISP p)
{
  int i;
  i = f_getc (get_c_file (p, stdin));
  return ((i == EOF) ? NIL : intcons ((double) i));
}

static LISP
lungetc (LISP ii, LISP p)
{
  int i;
  if NNULLP
    (ii)
    {
      i = get_c_int (ii);
      f_ungetc (i, get_c_file (p, stdin));
    }
  return (NIL);
}

static LISP
lputc (LISP c, LISP p)
{
  int i;
  FILE *f;
  f = get_c_file (p, siod_output);
  if INTNUMP
    (c)
      i = (int) INTNM (c);
  else
    i = *get_c_string (c);
  putc (i, f);
  return (NIL);
}

static LISP
lputs (LISP str, LISP p)
{
  fput_st (get_c_file (p, siod_output), get_c_string (str));
  return (NIL);
}

static LISP
parse_number (LISP x)
{
  char *c;
  c = get_c_string (x);
  return (intcons (atof (c)));
}

static LISP
closure_code (LISP exp)
{
  return (exp->storage_as.closure.code);
}

static LISP
closure_env (LISP exp)
{
  return (exp->storage_as.closure.env);
}

static LISP
lwhile (LISP form, LISP env)
{
  LISP l;
  while (NNULLP (leval (car (form), env)))
    for (l = cdr (form); NNULLP (l); l = cdr (l))
      leval (car (l), env);
  return (NIL);
}

static LISP
nreverse (LISP x)
{
  LISP newp, oldp, nextp;
  newp = NIL;
  for (oldp = x; CONSP (oldp); oldp = nextp)
    {
      nextp = CDR (oldp);
      CDR (oldp) = newp;
      newp = oldp;
    }
  return (newp);
}

static LISP
siod_verbose (LISP arg)
{
  if NNULLP
    (arg)
      siod_verbose_level = get_c_int (car (arg));
  return (intcons (siod_verbose_level));
}

static LISP
siod_lib_path (void)
{
  return (strcons (-1, siod_lib));
}

static LISP
lruntime (void)
{
  return (cons (intcons (myruntime ()),
		cons (intcons (gc_time_taken), NIL)));
}

static LISP
lrealtime (void)
{
  return (intcons (myrealtime ()));
}

static LISP
caar (LISP x)
{
  return (car (car (x)));
}

static LISP
cadr (LISP x)
{
  return (car (cdr (x)));
}

static LISP
cdar (LISP x)
{
  return (cdr (car (x)));
}

static LISP
cddr (LISP x)
{
  return (cdr (cdr (x)));
}

static LISP
letrec_macro (LISP form)
{
  LISP letb, setb, l;
  for (letb = NIL, setb = cddr (form), l = cadr (form); NNULLP (l); l = cdr (l))
    {
      letb = cons (cons (caar (l), NIL), letb);
      setb = cons (listn (3, rintern ("set!"), caar (l), car(cdar (l))), setb);
#if DEBUG_SCM
      setb->dbg_info = dbg_get_info (cdar (l));
      CDR (CDR (CAR (setb)))->dbg_info = dbg_get_info (cdar (l));
#endif
    }
  setcdr (form, cons (letb, setb));
  setcar (form, rintern ("let"));
#if DEBUG_SCM
  CDR (form)->dbg_info = dbg_get_info (setb);
#endif
  return (form);
}

static LISP
lrand (LISP m)
{
  long res;
  res = rand ();
  if NULLP
    (m)
      return (intcons (res));
  else
    return (intcons (res % get_c_int (m)));
}

static LISP
lsrand (LISP s)
{
  srand (get_c_int (s));
  return (NIL);
}

static LISP
siod_true_value (void)
{
  return (sym_t);
}

#if 0
static LISP
siod_false_value (void)
{
  return (sym_f);
}

static char *
last_c_errmsg (int num)
{
  int xerrno = (num < 0) ? errno : num;
  static char serrmsg[100];
  const char *errmsg;
  errmsg = strerror (xerrno);
  if (!errmsg)
    {
      sprintf (serrmsg, "errno %d", xerrno);
      errmsg = (const char *) serrmsg;
    }
  return ((char *) errmsg);
}
#endif

static LISP
llast_c_errmsg (int num)
{
  int xerrno = (num < 0) ? errno : num;
  const char *errmsg = strerror (xerrno);
  if (!errmsg)
    return (intcons (xerrno));
  return (rintern ((char *) errmsg));
}

static LISP
lllast_c_errmsg (void)
{
  return (llast_c_errmsg (-1));
}

static LISP
parser_read (LISP ignore)
{
  return (leval (rintern ("read"), NIL));
}

static LISP
bitand (LISP a, LISP b)
{
  return (intcons (get_c_int (a) & get_c_int (b)));
}

static LISP
bitor (LISP a, LISP b)
{
  return (intcons (get_c_int (a) | get_c_int (b)));
}

static LISP
bitxor (LISP a, LISP b)
{
  return (intcons (get_c_int (a) ^ get_c_int (b)));
}

static LISP
bitnot (LISP a)
{
  return (intcons (~get_c_int (a)));
}


static LISP
mapcar1 (LISP fcn, LISP in)
{
  LISP res, ptr, l;
  if NULLP
    (in) return (NIL);
  res = ptr = cons (funcall1 (fcn, car (in)), NIL);
  for (l = cdr (in); CONSP (l); l = CDR (l))
    ptr = CDR (ptr) = cons (funcall1 (fcn, CAR (l)), CDR (ptr));
  return (res);
}

static LISP
mapcar2 (LISP fcn, LISP in1, LISP in2)
{
  LISP res, ptr, l1, l2;
  if (NULLP (in1) || NULLP (in2))
    return (NIL);
  res = ptr = cons (funcall2 (fcn, car (in1), car (in2)), NIL);
  for (l1 = cdr (in1), l2 = cdr (in2); CONSP (l1) && CONSP (l2); l1 = CDR (l1), l2 = CDR (l2))
    ptr = CDR (ptr) = cons (funcall2 (fcn, CAR (l1), CAR (l2)), CDR (ptr));
  return (res);
}

static LISP
mapcar3 (LISP fcn, LISP in1, LISP in2, LISP in3)
{
  LISP res, ptr, l1, l2, l3;
  if (NULLP (in1) || NULLP (in2) || NULLP (in3))
    return (NIL);
  res = ptr = cons (lapply (fcn, cons (car (in1), cons (car (in2), cons (car (in3), NIL)))), NIL);

  for (l1 = cdr (in1), l2 = cdr (in2), l3 = cdr(in3);
       CONSP (l1) && CONSP (l2) && CONSP(l3);
       l1 = CDR (l1), l2 = CDR (l2), l3 = CDR (l3))
    ptr = CDR (ptr) = cons (lapply (fcn, cons (CAR (l1), cons (CAR (l2), cons (CAR (l3), NIL)))), CDR (ptr));
  return (res);
}

static LISP
llength (LISP obj)
{
  return (intcons (nlength (obj)));
}

static LISP
mapcar (LISP l)
{
  LISP fcn = car (l);

  switch (get_c_int (llength (l)))
    {
    case 2:
      return (mapcar1 (fcn, car (cdr (l))));
    case 3:
      return (mapcar2 (fcn, car (cdr (l)), car (cdr (cdr (l)))));
    case 4:
      return (mapcar3 (fcn, car (cdr (l)), car (cdr (cdr (l))), car (cdr (cdr (cdr (l))))));
    default:
      return (my_err ("mapcar case not handled", l));
    }
}

static LISP
nth (LISP x, LISP li)
{
  LISP l;
  long j, n = get_c_int (x);
  for (j = 0, l = li; (j < n) && CONSP (l); ++j)
    l = CDR (l);
  if CONSP
    (l)
      return (CAR (l));
  else
    return (my_err ("bad arg to nth", x));
}

static LISP
list_ref (LISP list, LISP k)
{
  LISP l;
  long j, n = get_c_int (k);
  for (j = 0, l = list; (j < n) && CONSP (l); ++j)
    l = CDR (l);
  if CONSP
    (l)
      return (CAR (l));
  else
    return (my_err ("bad arg to list-ref", k));
}

#if 0
static uim_lisp
list_tail(uim_lisp lst, uim_lisp nth_)
{
  int nth = uim_scm_c_int(nth_);
  int i;
  for (i = 0; i < nth; i++) {
    if (uim_scm_nullp(lst)) {
      /* something bad happened */
      return uim_scm_f();
    }
    lst = uim_scm_cdr(lst);
  }
  return lst;
}
#endif

static LISP
llist (LISP l)
{
  return (l);
}


static LISP
lstrspn (LISP str1, LISP str2)
{
  return (intcons (strspn (get_c_string (str1), get_c_string (str2))));
}

static LISP
lstrcspn (LISP str1, LISP str2)
{
  return (intcons (strcspn (get_c_string (str1), get_c_string (str2))));
}


static LISP
ass (LISP x, LISP alist, LISP fcn)
{
  LISP l, tmp;
  for (l = alist; CONSP (l); l = CDR (l))
    {
      tmp = CAR (l);
      if (CONSP (tmp) && NNULLP (funcall2 (fcn, CAR (tmp), x)))
	return (tmp);
      INTERRUPT_CHECK ();
    }
  if EQ
    (l, NIL) return (NIL);
  return (my_err ("improper list to ass", alist));
}


static LISP
butlast (LISP l)
{
  INTERRUPT_CHECK ();
  STACK_CHECK (&l);
  if NULLP
    (l) my_err ("list is empty", l);
  if CONSP (l)
    {
      if NULLP (CDR (l))
	return (NIL);
      else
	return (cons (CAR (l), butlast (CDR (l))));
    }
  return (my_err ("not a list", l));
}


/* Note: relies on car(), cdr () and predicates for nil-check */
static LISP
leval_case (LISP * pform, LISP * penv)
{
  LISP args, env, clause, key, next, data;
  args = cdr (*pform);
  env = *penv;
  key = leval (car (args), env);
  args = cdr (args);
  next = cdr (args);
  while NNULLP
    (next)
    {
      clause = car (args);
      data = car (clause);
      while NNULLP
	(data)
	{
	  if (eql (key, car (data)))
	    goto progn;
	  data = cdr (data);
	}
      args = next;
      next = cdr (next);
    }
  /* last clause; might be `else' */
  clause = car (args);
  data = car (clause);
  if (eq (data, sym_else))
    goto progn;
  while NNULLP
    (data)
    {
      if (eql (key, car (data)))
	goto progn;
      data = cdr (data);
    }
  *pform = NIL;
  return (NIL);

 progn:
  clause = cdr (clause);
  next = cdr (clause);
  while NNULLP
    (next)
    {
      leval (car (clause), env);
      clause = next;
      next = cdr (next);
    }
  *pform = car (clause);
  return (sym_t);
}


static LISP
leval_cond (LISP * pform, LISP * penv)
{
  LISP args, env, clause, value, next;
  args = cdr (*pform);
  env = *penv;
  if NULLP
    (args)
    {
      *pform = NIL;
      return (NIL);
    }
  next = cdr (args);
  while NNULLP
    (next)
    {
      clause = car (args);
      value = leval (car (clause), env);
      if NNULLP
	(value)
	{
	  clause = cdr (clause);
	  if NULLP
	    (clause)
	    {
	      *pform = value;
	      return (NIL);
	    }
	  else
	    {
	      next = cdr (clause);
	      while (NNULLP (next))
		{
		  leval (car (clause), env);
		  clause = next;
		  next = cdr (next);
		}
	      *pform = car (clause);
	      return (sym_t);
	    }
	}
      args = next;
      next = cdr (next);
    }
  clause = car (args);
  next = cdr (clause);
  if NULLP
    (next)
    {
      *pform = car (clause);
      return (sym_t);
    }
  value = leval (car (clause), env);
  if NULLP
    (value)
    {
      *pform = NIL;
      return (NIL);
    }
  clause = next;
  next = cdr (next);
  while (NNULLP (next))
    {
      leval (car (clause), env);
      clause = next;
      next = cdr (next);
    }
  *pform = car (clause);
  return (sym_t);
}

static LISP
funcall1 (LISP fcn, LISP a1)
{
  switch TYPE
    (fcn)
    {
    case tc_subr_1:
      STACK_CHECK (&fcn);
      INTERRUPT_CHECK ();
      return (SUBR1 (fcn) (a1));
    case tc_closure:
      if TYPEP
	(fcn->storage_as.closure.code, tc_subr_2)
	{
	  STACK_CHECK (&fcn);
	  INTERRUPT_CHECK ();
	  return (SUBR2 (fcn->storage_as.closure.code)
		  (fcn->storage_as.closure.env, a1));
	}
    default:
      return (lapply (fcn, cons (a1, NIL)));
    }
}

static LISP
funcall2 (LISP fcn, LISP a1, LISP a2)
{
  switch TYPE
    (fcn)
    {
    case tc_subr_2:
    case tc_subr_2n:
      STACK_CHECK (&fcn);
      INTERRUPT_CHECK ();
      return (SUBR2 (fcn) (a1, a2));
    default:
      return (lapply (fcn, cons (a1, cons (a2, NIL))));
    }
}


static LISP
assv (LISP x, LISP alist)
{
  LISP l, tmp;
  for (l = alist; CONSP (l); l = CDR (l))
    {
      tmp = CAR (l);
      if (CONSP (tmp) && NNULLP (eql (CAR (tmp), x)))
	return (tmp);
      INTERRUPT_CHECK ();
    }
  if EQ
    (l, NIL) return (NIL);
  return (my_err ("improper list to assv", alist));
}

static LISP
lstrcmp (LISP s1, LISP s2)
{
  return (intcons (strcmp (get_c_string (s1), get_c_string (s2))));
}

static LISP
member (LISP x, LISP il)
{
  LISP l, tmp;
  for (l = il; CONSP (l); l = CDR (l))
    {
      tmp = CAR (l);
      if NNULLP
	(equal (x, tmp)) return (l);
      INTERRUPT_CHECK ();
    }
  if EQ
    (l, NIL) return (NIL);
  return (my_err ("improper list to member", il));
}

static LISP
memv (LISP x, LISP il)
{
  LISP l, tmp;
  for (l = il; CONSP (l); l = CDR (l))
    {
      tmp = CAR (l);
      if NNULLP
	(eql (x, tmp)) return (l);
      INTERRUPT_CHECK ();
    }
  if EQ
    (l, NIL) return (NIL);
  return (my_err ("improper list to memv", il));
}

static LISP
lsubset (LISP fcn, LISP l)
{
  LISP result = NIL, v;
  for (v = l; CONSP (v); v = CDR (v))
    if NNULLP
      (funcall1 (fcn, CAR (v)))
	result = cons (CAR (v), result);
  return (nreverse (result));
}

static LISP
listn (long n,...)
{
  LISP result, ptr;
  long j;
  va_list args;
  for (j = 0, result = NIL; j < n; ++j)
    result = cons (NIL, result);
  va_start (args, n);
  for (j = 0, ptr = result; j < n; ptr = cdr (ptr), ++j)
    setcar (ptr, va_arg (args, LISP));
  va_end (args);
  return (result);
}

static LISP
ltypeof (LISP obj)
{
  long x;
  x = TYPE (obj);
  switch (x)
    {
    case tc_nil:
      return (rintern ("tc_nil"));
    case tc_cons:
      return (rintern ("tc_cons"));
    case tc_intnum:
      return (rintern ("tc_intnum"));
    case tc_symbol:
      return (rintern ("tc_symbol"));
    case tc_subr_0:
      return (rintern ("tc_subr_0"));
    case tc_subr_1:
      return (rintern ("tc_subr_1"));
    case tc_subr_2:
      return (rintern ("tc_subr_2"));
    case tc_subr_2n:
      return (rintern ("tc_subr_2n"));
    case tc_subr_3:
      return (rintern ("tc_subr_3"));
    case tc_subr_4:
      return (rintern ("tc_subr_4"));
    case tc_subr_5:
      return (rintern ("tc_subr_5"));
    case tc_lsubr:
      return (rintern ("tc_lsubr"));
    case tc_fsubr:
      return (rintern ("tc_fsubr"));
    case tc_msubr:
      return (rintern ("tc_msubr"));
    case tc_closure:
      return (rintern ("tc_closure"));
    case tc_free_cell:
      return (rintern ("tc_free_cell"));
    case tc_string:
      return (rintern ("tc_string"));
    case tc_c_file:
      return (rintern ("tc_c_file"));
    case tc_c_pointer:
      return (rintern ("tc_c_pointer"));
    default:
      return (intcons (x));
    }
}

static LISP
string2integer (LISP str)
{
  char *s = get_c_string(str);
  int len = strlen(s);
  int i;
  int d = 1, num = 0;

  for (i=len-1; i>=0; i--) {
    int n = s[i];
    if (n < 48 || n > 57)
      return sym_f;

    num += d * (n - 48);
    d = d * 10;
  }
  return intcons(num);
}

static LISP
integer2string (LISP args)
{
  char buf[sizeof (long)*CHAR_BIT];
  char *p = buf + sizeof (buf);
  unsigned long n, r;
  LISP x, radix;
  x = car (args);
  radix = NNULLP (cdr (args)) ? CAR (CDR (args)) : intcons (10);
  if NINTNUMP
    (x)
      my_err ("wta to integer2string", x);
  if NINTNUMP
    (radix)
      my_err ("wta to integer2string", radix);
  r = INTNM (radix);
  if (r < 2 || 16 < r)
    my_err ("invalid radix to integer2string", radix);
  n = (r == 10) ? labs (INTNM (x)) : INTNM (x);
  do
    {
      if (n % r > 9)
	*--p = 'A' + n % r - 10;
      else
	*--p = '0' + n % r;
    }
  while (n /= r);
  if (r == 10 && INTNM (x) < 0)
    *--p = '-';
  return strcons (sizeof(buf)-(p-buf), p);
}

static void
init_subrs (void)
{

  init_subr_2 ("assoc", assoc);
  init_subr_2 ("append2", append2);
  init_lsubr ("append", append);
  init_subr_2 ("cons", cons);
  init_subr_1 ("car", car);
  init_subr_1 ("cdr", cdr);
  init_subr_2 ("set-car!", setcar);
  init_subr_2 ("set-cdr!", setcdr);
  init_subr_1 ("last-pair", last_pair);
  init_subr_2n ("+", plus);
  init_subr_2n ("-", difference);
  init_subr_2n ("*", ltimes);
  init_subr_2n ("/", Quotient);
  init_subr_2n ("remainder", Remainder);
  init_subr_2n ("min", lmin);
  init_subr_2n ("max", lmax);
  init_subr_1 ("abs", lllabs);
  init_subr_2 ("ash", ash);
  init_subr_2 (">", greaterp);
  init_subr_2 ("<", lessp);
  init_subr_2 (">=", greaterEp);
  init_subr_2 ("<=", lessEp);
  init_subr_2 ("equal?", equal);
  init_subr_2 ("eq?", eq);
  init_subr_2 ("eqv?", eql);
#if 0
  init_subr_2 ("=", inteql);  /* R5RS compatible */
#else
  init_subr_2 ("=", eql);     /* loosely accepts non-number objects */
#endif
  init_subr_2 ("assq", assq);
  init_msubr ("cond", leval_cond);
  init_msubr ("case", leval_case);
  init_subr_2 ("delq", delq);
  init_subr_1 ("read", lread);
  init_subr_1 ("parser_read", parser_read);
  setvar (rintern ("*parser_read.scm-loaded*"), sym_t, NIL);
  init_subr_0 ("eof-val", get_eof_val);
  init_subr_2 ("print", lprint);
  init_subr_2 ("prin1", lprin1);
  init_subr_3 ("print-to-string", print_to_string);
  init_subr_1 ("string-length", string_length);
  init_subr_1 ("string-dimension", string_dim);
  init_lsubr ("string-append", string_append);
  init_subr_1 ("string->integer", string2integer);
  init_lsubr ("integer->string", integer2string);
  init_subr_2 ("string=?", string_equal);
  init_subr_2 ("eval", leval);
  init_subr_2 ("apply", lapply);
  init_fsubr ("define", leval_define);
  init_fsubr ("lambda", leval_lambda);
  init_msubr ("if", leval_if);
  init_fsubr ("while", leval_while);
  init_msubr ("begin", leval_progn);
  init_fsubr ("set!", leval_setq);
  init_msubr ("or", leval_or);
  init_msubr ("and", leval_and);
  init_fsubr ("*catch", leval_catch);
  init_subr_2 ("*throw", lthrow);
  init_fsubr ("quote", leval_quote);
  init_lsubr ("apropos", apropos);
  init_lsubr ("verbose", siod_verbose);
  init_subr_0 ("load-path", siod_lib_path);
  init_subr_1 ("copy-list", copy_list);
  init_lsubr ("gc-status", gc_status);
  init_lsubr ("gc", user_gc);
  init_subr_3 ("load", load);
  init_subr_1 ("require", require);
  init_subr_1 ("pair?", consp);
  init_subr_1 ("symbol?", symbolp);
  init_subr_1 ("number?", numberp);
  init_subr_1 ("procedure?", procedurep);
  init_msubr ("let-internal", leval_let);
  init_subr_1 ("let-internal-macro", let_macro);
  init_subr_1 ("let*-macro", letstar_macro);
  init_subr_1 ("letrec-macro", letrec_macro);
  init_subr_2 ("symbol-bound?", symbol_boundp);
  init_subr_2 ("symbol-value", symbol_value);
  init_subr_3 ("set-symbol-value!", setvar);
  init_subr_2 ("symbol->string", symbol_to_string);
  init_fsubr ("the-environment", leval_tenv);
  init_subr_2 ("error", lerr);
  init_subr_0 ("quit", quit);
  init_subr_1 ("not", nullp);
  init_subr_1 ("null?", nullp);
  init_subr_2 ("env-lookup", envlookup);
  init_subr_1 ("reverse", reverse);
  init_lsubr ("symbolconc", symbolconc);
  init_subr_1 ("getc", lgetc);
  init_subr_2 ("ungetc", lungetc);
  init_subr_2 ("putc", lputc);
  init_subr_2 ("puts", lputs);
  init_subr_1 ("parse-number", parse_number);
  init_subr_2 ("%%stack-limit", stack_limit);
  init_subr_1 ("intern", intern);
  init_subr_2 ("%%closure", closure);
  init_subr_1 ("%%closure-code", closure_code);
  init_subr_1 ("%%closure-env", closure_env);
  init_fsubr ("while", lwhile);
  init_subr_1 ("nreverse", nreverse);
  init_subr_0 ("allocate-heap", allocate_aheap);
  init_subr_1 ("gc-info", gc_info);
  init_subr_0 ("runtime", lruntime);
  init_subr_0 ("realtime", lrealtime);
  init_subr_1 ("caar", caar);
  init_subr_1 ("cadr", cadr);
  init_subr_1 ("cdar", cdar);
  init_subr_1 ("cddr", cddr);
  init_subr_1 ("rand", lrand);
  init_subr_1 ("srand", lsrand);
  init_subr_0 ("last-c-error", lllast_c_errmsg);
  init_subr_2 ("bit-and", bitand);
  init_subr_2 ("bit-or", bitor);
  init_subr_2 ("bit-xor", bitxor);
  init_subr_1 ("bit-not", bitnot);
  init_subr_1 ("feature?", featurep);
  init_subr_1 ("provide", provide);
  init_subr_1 ("read-from-string", read_from_string);
  init_subr_1 ("length", llength);
  init_lsubr ("mapcar", mapcar);
  init_subr_3 ("mapcar2", mapcar2);
  init_subr_2 ("mapcar1", mapcar1);
  init_subr_2 ("memq", memq);
  init_subr_2 ("nconc", nconc);
  init_lsubr ("list", llist);
  init_subr_2 ("strspn", lstrspn);
  init_subr_2 ("strcspn", lstrcspn);
  init_subr_1 ("string?", stringp);
  init_subr_3 ("ass", ass);
  init_subr_2 ("nth", nth);
  init_subr_1 ("butlast", butlast);

  init_subr_2 ("list-ref", list_ref);
#if 0
  /*
   * list-tail is already existing in util.scm. To replace it with
   * this, implement equivalent error handling and validate with
   * test/test-util.scm. Please don't forget existence of util.scm.
   *   -- YamaKen 2005-07-03
   */
  init_subr_2 ("list-tail", list_tail);
#endif

  init_subr_2 ("assv", assv);
  init_subr_2 ("strcmp", lstrcmp);
  init_subr_2 ("subset", lsubset);
  init_subr_1 ("typeof", ltypeof);
  init_subr_2 ("memv", memv);
  init_subr_2 ("member", member);
  init_fsubr ("undefine", undefine);
  init_slib_version ();
}

static void
siod_quit (void)
{
  int i;
  struct gc_protected *reg, *tmp;
  /**/
  for (i = 0; i < nheaps; i++) {
    LISP ptr, end;
    if (!heaps[i]) {
      continue;
    }
    end = heaps[i] + heap_size;
    for (ptr = heaps[i]; ptr < end; ptr++) {
      free_a_cell(ptr);
    }
    free(heaps[i]);
  }
  free(heaps);
  /**/
  for (reg = protected_registers; reg;) {
    tmp = reg;
    reg = reg->next;
    free(tmp);
  }
  /**/
  free(tkbuffer);
  free(obarray);
  free(inums);
}

static void
siod_init (int argc, char **argv, int warnflag, FILE *fp)
{
  int k;
  char *ptr;
#if (NESTED_REPL_C_STRING)
  LISP stack_start;
#endif

  siod_output = fp;

  /* set global variables */
  siod_verbose_level = 4;
  sym_t = NIL;
  stack_limit_ptr = NULL;
  sym_f = NIL;
  nheaps = 2;
  heaps = NULL;
  heap = 0; heap_end = 0;
  heap_size = 5000;
  heap_alloc_threshold = 100;
  gc_status_flag = 1;
  init_file = (char *)NULL;
  tkbuffer = NULL;
  gc_cells_allocated = 0;
  gc_time_taken = 0;
  stack_start_ptr = NULL;
  freelist = NIL;
  errjmp_ok = 0;
  oblistvar = NIL;
  eof_val = NIL;
  sym_errobj = NIL;
  sym_catchall = NIL;
  sym_progn = NIL;
  sym_lambda = NIL;
  sym_else = NIL;
  sym_quote = NIL;
  sym_dot = NIL;
  sym_after_gc = NIL;
  sym_features = NIL;
  unbound_marker = NIL;
  obarray = NULL;
  repl_return_val = NIL;
#if (!NESTED_REPL_C_STRING)
  repl_c_string_entered = 0;
#endif
  obarray_dim = 100;
  catch_framep = (struct catch_frame *) NULL;
  repl_puts = NULL;
  repl_read = NULL;
  repl_eval = NULL;
  repl_print = NULL;
  inums = NULL;
  inums_dim = 256;
  user_types = NULL;
  protected_registers = NULL;
  gc_rt = 0;
  gc_cells_swept = 0;
  gc_cells_collected = 0;
  user_ch_readm = "";
  user_te_readm = "";
  user_readm = NULL;
  user_readt = NULL;
  fatal_exit_hook = NULL;
  stack_size = 50000;
  func_trace = NULL;

  /* parse arguments */
  for (k = 1; k < argc; ++k)
    {
      if (strlen (argv[k]) < 2)
	continue;
      if (argv[k][0] != '-')
	{
	  if (warnflag)
	    fprintf (stderr, "bad arg: %s\n", argv[k]);
	  continue;
	}
      switch (argv[k][1])
	{
	case 'l':
	  siod_lib = &argv[k][2];
	  break;
	case 'h':
	  heap_size = atol (&(argv[k][2]));
	  if ((ptr = strchr (&(argv[k][2]), ':')))
	    nheaps = atol (&ptr[1]);
	  break;
	case 't':
	  heap_alloc_threshold = atol (&(argv[k][2]));
	  break;
	case 'o':
	  obarray_dim = atol (&(argv[k][2]));
	  break;
	case 'i':
	  init_file = &(argv[k][2]);
	  break;
	case 'n':
	  inums_dim = atol (&(argv[k][2]));
	  break;
	case 's':
	  stack_size = atol (&(argv[k][2]));
	  break;
	case 'v':
	  siod_verbose_level = atol (&(argv[k][2]));
	  break;
	default:
	  if (warnflag)
	    fprintf (stderr, "bad arg: %s\n", argv[k]);
	}
    }
#if (NESTED_REPL_C_STRING)
  siod_gc_protect_stack(&stack_start);
#endif
  init_storage ();
  init_subrs ();
#if DEBUG_SCM
  init_dbg ();
#endif
#if (NESTED_REPL_C_STRING)
  siod_gc_unprotect_stack(&stack_start);
#endif
}
