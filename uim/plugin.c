/*
  $Id:$

  plugin.c: Plugin support for uim.

  Copyright (c) 2004-2007 uim Project http://uim.freedesktop.org/

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
#include <string.h>
#include <dlfcn.h>
#include <dirent.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <pwd.h>

#include "uim-stdint.h"
#include "uim.h"
#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "uim-compat-scm.h"
#include "plugin.h"
#include "uim-internal.h"

#ifndef HAVE_DLFUNC
#define dlfunc dlsym
#endif

#define PLUGIN_PREFIX "libuim-"
#define PLUGIN_SUFFIX ".so"

/*
 * SIOD's verbose-level compatible definition.
 * See sigscheme/operations-siod.c for further information.
 */
#define UIM_VLEVEL_PLUGIN 3

#ifdef DEBUG
#define DPRINTFN(n,x)  if ((n) <= uim_scm_get_verbose_level()) fprintf x;
#else
#define DPRINTFN(n,x)
#endif

#if UIM_SCM_GCC4_READY_GC
static void *plugin_unload_internal(void *uim_lisp_name);
static void *uim_quit_plugin_internal(void *dummy);
#endif


static uim_lisp 
plugin_load(uim_lisp _name)
{
  const char *plugin_name;
  char *plugin_lib_filename = NULL, *plugin_scm_filename = NULL;
  uim_lisp lib_path = uim_scm_eval_c_string("uim-plugin-lib-load-path");
  uim_lisp scm_path = uim_scm_eval_c_string("uim-plugin-scm-load-path");
  uim_lisp path_car, path_cdr;
  void *library;
  void (*plugin_instance_init)(void);
  void (*plugin_instance_quit)(void);

  size_t len;

  plugin_name = uim_scm_refer_c_str(_name);
  
  if (plugin_name == NULL) {
    return uim_scm_f();
  }

  DPRINTFN(UIM_VLEVEL_PLUGIN, (stderr, "Searching libuim-%s.so.\n", plugin_name));

  for (path_cdr = lib_path;
       !uim_scm_nullp(path_cdr);
       path_cdr = uim_scm_cdr(path_cdr))
  {
    int fd;
    const char *path;
    path_car = uim_scm_car(path_cdr);
    path = uim_scm_refer_c_str(path_car);
    len = strlen(path) + 1 + strlen(PLUGIN_PREFIX) + strlen(plugin_name)+ strlen(PLUGIN_SUFFIX) + 1;
    plugin_lib_filename = malloc(sizeof(char) * len);
    snprintf(plugin_lib_filename, len, "%s/%s%s%s",
	     path, PLUGIN_PREFIX, plugin_name, PLUGIN_SUFFIX);
    fd = open(plugin_lib_filename, O_RDONLY);
    if (fd >= 0) {
      close(fd);
      DPRINTFN(UIM_VLEVEL_PLUGIN, (stderr, "Found %s.\n", plugin_lib_filename));
      break;
    }
    free(plugin_lib_filename);
    plugin_lib_filename = NULL;
  }

  DPRINTFN(UIM_VLEVEL_PLUGIN, (stderr, "Searching %s.scm.\n", plugin_name));
  for (path_cdr = scm_path;
       !uim_scm_nullp(path_cdr);
       path_cdr = uim_scm_cdr(path_cdr))
  {
    int fd;
    const char *path;
    path_car = uim_scm_car(path_cdr);
    path = uim_scm_refer_c_str(path_car);
    len = strlen(path) + 1 + strlen(plugin_name)+ strlen(".scm") + 1;
    plugin_scm_filename = malloc(sizeof(char) * len);
    snprintf(plugin_scm_filename, len, "%s/%s.scm", path, plugin_name);
    fd = open(plugin_scm_filename, O_RDONLY);
    if (fd >= 0) {
      close(fd);
      DPRINTFN(UIM_VLEVEL_PLUGIN, (stderr, "Found %s.\n", plugin_scm_filename));
      break;
    }
    free(plugin_scm_filename);
    plugin_scm_filename = NULL;
  }

  if (plugin_lib_filename == NULL) {
    free(plugin_scm_filename);
    return uim_scm_f();
  }

  DPRINTFN(UIM_VLEVEL_PLUGIN, (stderr, "Loading libuim-%s.so.\n", plugin_name));
  library = dlopen(plugin_lib_filename, RTLD_NOW);
  free(plugin_lib_filename);

  if (library == NULL) {
    fprintf(stderr, "load failed %s\n", dlerror());
    free(plugin_scm_filename);
    return uim_scm_f();
  }

  plugin_instance_init
    = (void (*)(void))(uintptr_t)dlfunc(library, "uim_plugin_instance_init");
  plugin_instance_quit
    = (void (*)(void))(uintptr_t)dlfunc(library, "uim_plugin_instance_quit");
  if (!plugin_instance_init) {
    fprintf(stderr, "%s plugin init failed\n", plugin_name);
    free(plugin_scm_filename);
    return uim_scm_f();
  }

  DPRINTFN(UIM_VLEVEL_PLUGIN, (stderr, "Calling plugin_instance_init() for %s.\n", plugin_name));
  (plugin_instance_init)();
  if (plugin_scm_filename) {
    uim_bool succeeded;

    succeeded = uim_scm_require_file(plugin_scm_filename);
    if (!succeeded) {
      fprintf(stderr, "%s plugin subsequent %s loading failed\n",
	      plugin_name, plugin_scm_filename);
      free(plugin_scm_filename);
      return uim_scm_f();
    }
  }

  {
    uim_lisp form;
    form = uim_scm_list5(uim_scm_make_symbol("plugin-list-append"),
		         _name,
		         uim_scm_make_ptr(library),
			 uim_scm_make_func_ptr(plugin_instance_init),
			 uim_scm_make_func_ptr(plugin_instance_quit));
    uim_scm_eval(form);
  }
  free(plugin_scm_filename);

  return uim_scm_t();
}

static uim_lisp
plugin_unload(uim_lisp _name)
#if UIM_SCM_GCC4_READY_GC
{
  return (uim_lisp)uim_scm_call_with_gc_ready_stack(plugin_unload_internal,
						    (void *)_name);
}

static void *
plugin_unload_internal(void *uim_lisp_name)
#endif
{
#if UIM_SCM_GCC4_READY_GC
  uim_lisp _name;
#else
  uim_lisp stack_start;
#endif
  void *library;
  void (*plugin_instance_quit)(void);

#if UIM_SCM_GCC4_READY_GC
  _name = (uim_lisp)uim_lisp_name;
#else
  uim_scm_gc_protect_stack(&stack_start);
#endif

  UIM_EVAL_FSTRING1(NULL, "(plugin-list-query-library \"%s\")",
		    uim_scm_refer_c_str(_name));
  if (UIM_SCM_FALSEP(uim_scm_return_value()))
    return uim_scm_f();
  library = uim_scm_c_ptr(uim_scm_return_value());

  UIM_EVAL_FSTRING1(NULL, "(plugin-list-query-instance-quit \"%s\")",
		    uim_scm_refer_c_str(_name));
  if (UIM_SCM_FALSEP(uim_scm_return_value()))
    return uim_scm_f();
  plugin_instance_quit = uim_scm_c_func_ptr(uim_scm_return_value());

  (plugin_instance_quit)();
  dlclose(library);

  UIM_EVAL_FSTRING1(NULL, "(plugin-list-delete \"%s\")",
		    uim_scm_refer_c_str(_name));

#if UIM_SCM_GCC4_READY_GC
  return (void *)uim_scm_t();
#else
  uim_scm_gc_unprotect_stack(&stack_start);

  return uim_scm_t();
#endif
}

/* Called from uim_init */
void
uim_init_plugin(void)
{
  uim_scm_init_subr_1("load-plugin", plugin_load);
  uim_scm_init_subr_1("unload-plugin", plugin_unload);

  return;
}

/* Called from uim_quit */
void
uim_quit_plugin(void)
#if UIM_SCM_GCC4_READY_GC
{
  uim_scm_call_with_gc_ready_stack(uim_quit_plugin_internal, NULL);
}

static void *
uim_quit_plugin_internal(void *dummy)
#endif
{
#if !UIM_SCM_GCC4_READY_GC
  uim_lisp stack_start;
#endif
  uim_lisp alist, rest, entry, name;

#if !UIM_SCM_GCC4_READY_GC
  uim_scm_gc_protect_stack(&stack_start);
#endif

  alist = uim_scm_eval_c_string("plugin-alist");
  for (rest = alist; !uim_scm_nullp(rest); rest = uim_scm_cdr(rest)) {
    entry = uim_scm_car(rest);
    name = uim_scm_car(entry);

    plugin_unload(name);
  }

#if UIM_SCM_GCC4_READY_GC
  return NULL;
#else
  uim_scm_gc_unprotect_stack(&stack_start);
#endif
}
