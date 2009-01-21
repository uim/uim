/*
  $Id:$

  plugin.c: Plugin support for uim.

  Copyright (c) 2004-2009 uim Project http://code.google.com/p/uim/

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

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "plugin.h"
#include "uim-internal.h"
#if UIM_USE_NOTIFY
#include "uim-notify.h"
#endif
#include "gettext.h"

#ifndef HAVE_DLFUNC
#define dlfunc(handle, symbol) \
  ((void (*)(void))(uintptr_t)dlsym((handle), (symbol)))
#endif

#define PLUGIN_PREFIX "libuim-"
#define PLUGIN_SUFFIX ".so"

/*
 * SIOD's verbose-level compatible definition.
 * See sigscheme/operations-siod.c for further information.
 */
#define UIM_VLEVEL_PLUGIN 3

#ifdef DEBUG
#define DPRINTFN(n,x)  if ((n) <= verbose_level()) fprintf x;
#else
#define DPRINTFN(n,x)
#endif

static long verbose_level(void);
static void *plugin_unload_internal(void *uim_lisp_name);
static void *uim_quit_plugin_internal(void *dummy);


static long 
verbose_level(void)
{
  uim_lisp vlevel;

  vlevel = uim_scm_callf("verbose", "");
  return C_INT(vlevel);
}

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

  plugin_name = REFER_C_STR(_name);
  
  if (plugin_name == NULL) {
    return uim_scm_f();
  }

  DPRINTFN(UIM_VLEVEL_PLUGIN, (stderr, "Searching libuim-%s.so.\n", plugin_name));

  for (path_cdr = lib_path;
       !NULLP(path_cdr);
       path_cdr = CDR(path_cdr))
  {
    int fd;
    const char *path;
    path_car = CAR(path_cdr);
    path = REFER_C_STR(path_car);
    len = strlen(path) + 1 + strlen(PLUGIN_PREFIX) + strlen(plugin_name)+ strlen(PLUGIN_SUFFIX) + 1;
    plugin_lib_filename = uim_malloc(sizeof(char) * len);
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
       !NULLP(path_cdr);
       path_cdr = CDR(path_cdr))
  {
    int fd;
    const char *path;
    path_car = CAR(path_cdr);
    path = REFER_C_STR(path_car);
    len = strlen(path) + 1 + strlen(plugin_name)+ strlen(".scm") + 1;
    plugin_scm_filename = uim_malloc(sizeof(char) * len);
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
    uim_notify_fatal(N_("%s plugin: Load failed."), dlerror());
    free(plugin_scm_filename);
    return uim_scm_f();
  }

  plugin_instance_init
    = (void (*)(void))dlfunc(library, "uim_plugin_instance_init");
  plugin_instance_quit
    = (void (*)(void))dlfunc(library, "uim_plugin_instance_quit");
  if (!plugin_instance_init) {
    uim_notify_fatal(N_("%s plugin: Init failed."), plugin_name);
    free(plugin_scm_filename);
    return uim_scm_f();
  }

  DPRINTFN(UIM_VLEVEL_PLUGIN, (stderr, "Calling plugin_instance_init() for %s.\n", plugin_name));
  (plugin_instance_init)();
  if (plugin_scm_filename) {
    uim_bool succeeded;

    succeeded = uim_scm_require_file(plugin_scm_filename);
    if (!succeeded) {
      /* FIXME: gettext here to expand %s in accordance with the
       * locale for the selected notification agent. See also the TODO
       * comment of uim-notify.h  -- YamaKen 2008-02-11 */
      uim_notify_fatal(N_("%s plugin: Subsequent %s load failed."),
		       plugin_name, plugin_scm_filename);
      free(plugin_scm_filename);
      return uim_scm_f();
    }
  }

  {
    uim_lisp form;
    form = LIST5(MAKE_SYM("plugin-list-append"),
		         _name,
		         MAKE_PTR(library),
			 MAKE_FPTR(plugin_instance_init),
			 MAKE_FPTR(plugin_instance_quit));
    uim_scm_eval(form);
  }
  free(plugin_scm_filename);

  return uim_scm_t();
}

static uim_lisp
plugin_unload(uim_lisp _name)
{
  return (uim_lisp)uim_scm_call_with_gc_ready_stack(plugin_unload_internal,
						    (void *)_name);
}

static void *
plugin_unload_internal(void *uim_lisp_name)
{
  uim_lisp _name;
  uim_lisp ret;
  void *library;
  void (*plugin_instance_quit)(void);

  _name = (uim_lisp)uim_lisp_name;

  ret = uim_scm_callf("plugin-list-query-library", "o", _name);
  if (FALSEP(ret))
    return uim_scm_f();
  library = C_PTR(ret);

  ret = uim_scm_callf("plugin-list-query-instance-quit", "o", _name);
  if (FALSEP(ret))
    return uim_scm_f();
  plugin_instance_quit = C_FPTR(ret);

  (plugin_instance_quit)();
  dlclose(library);

  uim_scm_callf("plugin-list-delete", "o", _name);

  return (void *)uim_scm_t();
}

/* Called from uim_init */
void
uim_init_plugin(void)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  uim_scm_init_proc1("load-plugin", plugin_load);
  uim_scm_init_proc1("unload-plugin", plugin_unload);

  UIM_CATCH_ERROR_END();
}

/* Called from uim_quit */
void
uim_quit_plugin(void)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  uim_scm_call_with_gc_ready_stack(uim_quit_plugin_internal, NULL);

  UIM_CATCH_ERROR_END();
}

static void *
uim_quit_plugin_internal(void *dummy)
{
  uim_lisp alist, rest, entry, name;

  alist = uim_scm_eval_c_string("plugin-alist");
  for (rest = alist; !NULLP(rest); rest = CDR(rest)) {
    entry = CAR(rest);
    name = CAR(entry);

    plugin_unload(name);
  }

  return NULL;
}
