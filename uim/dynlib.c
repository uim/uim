/*
  $Id:$

  dynlib.c: Bare dynamic library load/unload support for uim

  Copyright (c) 2004-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.
*/

#include <config.h>

#include <stdio.h>
#include <dlfcn.h>

#include "uim.h"
#include "uim-internal.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#if UIM_USE_NOTIFY_PLUGINS
#include "uim-notify.h"
#endif
#include "gettext.h"

#ifndef HAVE_DLFUNC
#define dlfunc(handle, symbol) \
  ((void (*)(void))(uintptr_t)dlsym((handle), (symbol)))
#endif

/*
 * SIOD's verbose-level compatible definition.
 * See sigscheme/operations-siod.c for further information.
 */
#define UIM_VLEVEL_DYNLIB 3

#ifdef DEBUG
#define DPRINTFN(n,x)  if ((n) <= verbose_level()) fprintf x;
#else
#define DPRINTFN(n,x)
#endif

static long verbose_level(void);

struct dynlib_unbind_args {
  uim_lisp lib_ptr;
  uim_lisp init_proc;
  uim_lisp quit_proc;
};
static uim_lisp dynlib_unbind(uim_lisp, uim_lisp, uim_lisp);
static void *dynlib_unbind_internal(struct dynlib_unbind_args *);
static uim_lisp dynlib_bind(uim_lisp);
static void *dynlib_bind_internal(uim_lisp);
static uim_lisp dynlib_unbind_all(uim_lisp);

static long 
verbose_level(void)
{
  uim_lisp vlevel;

  vlevel = uim_scm_callf("verbose", "");
  return C_INT(vlevel);
}

/* Called from uim_init */
void
uim_init_dynlib(void)
{
  uim_scm_init_proc1("%%dynlib-bind", dynlib_bind);
  uim_scm_init_proc3("%%dynlib-unbind", dynlib_unbind);
  uim_scm_init_proc1("%%dynlib-unbind-all", dynlib_unbind_all);
}

/* Called from uim_quit */
void
uim_quit_dynlib(void)
{
}

static uim_lisp
dynlib_unbind(uim_lisp lib_ptr,
	      uim_lisp init_proc,
	      uim_lisp quit_proc)
{
  struct dynlib_unbind_args args;
  args.lib_ptr = lib_ptr;
  args.init_proc = init_proc;
  args.quit_proc = quit_proc;

  return uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)dynlib_unbind_internal, (void *)&args);
}

static void *
dynlib_unbind_internal(struct dynlib_unbind_args *args)
{
  void *library;
  void (*dynlib_instance_quit)(void);

  library = C_PTR(args->lib_ptr);
  dynlib_instance_quit = C_FPTR(args->quit_proc);

  (*dynlib_instance_quit)();
  dlclose(library);

  return uim_scm_t();
}

static uim_lisp
dynlib_bind(uim_lisp name)
{
  return uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)dynlib_bind_internal, (void *)name);
}

static void *
dynlib_bind_internal(uim_lisp name)
{
  void *library;
  void (*dynlib_instance_init)(void);
  void (*dynlib_instance_quit)(void);

  DPRINTFN(UIM_VLEVEL_DYNLIB, (stderr, "Loading %s", REFER_C_STR(name)));
  library = dlopen(REFER_C_STR(name), RTLD_NOW);

  if (library == NULL) {
    uim_notify_fatal(_("dynlib: %s: Load failed."), dlerror());
    return uim_scm_f();
  }

  dynlib_instance_init
    = (void (*)(void))dlfunc(library, "uim_dynlib_instance_init");
  dynlib_instance_quit
    = (void (*)(void))dlfunc(library, "uim_dynlib_instance_quit");
  if (!dynlib_instance_init) {
    uim_notify_fatal(_("dynlib: %s: Initialization failed."), REFER_C_STR(name));
    return uim_scm_f();
  }
	
  DPRINTFN(UIM_VLEVEL_DYNLIB, (stderr, "Calling dynlib_instance_init() for %s.\n", REFER_C_STR(name)));
  (*dynlib_instance_init)();

  return LIST3(MAKE_PTR(library),
	       MAKE_FPTR(dynlib_instance_init),
	       MAKE_FPTR(dynlib_instance_quit));
}


static void *
dynlib_unbind_all_internal(uim_lisp plugin_alist_)
{
  /* call dlclose(3) collectively at the end in order to avoid GC problem */
  uim_lisp alist_ = plugin_alist_;

  while (!NULLP(alist_)) {
    uim_lisp plugin_, quit_proc_;
    void (*dynlib_instance_quit)(void);
    
    plugin_ = CAR(alist_);
    quit_proc_ = CAR(CDR(CDR(CDR(plugin_))));
    if (!FALSEP(quit_proc_)) {
      dynlib_instance_quit = C_FPTR(quit_proc_);
      (*dynlib_instance_quit)();
    }
    alist_ = CDR(alist_);
  }

  alist_ = plugin_alist_;
  while (!NULLP(alist_)) {
    uim_lisp plugin_, lib_;
    void *library;

    plugin_ = CAR(alist_);
    lib_ = CAR(CDR(plugin_));
    if (!FALSEP(lib_)) {
      library = C_PTR(lib_);
      dlclose(library);
    }
    alist_ = CDR(alist_);
  }

  return uim_scm_t();
}

static uim_lisp
dynlib_unbind_all(uim_lisp plugin_alist_)
{
  return uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)dynlib_unbind_all_internal, plugin_alist_);
}
