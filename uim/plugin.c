/*
  $Id:$

  plugin.c: Plugin support for uim.

  Copyright (c) 2004,2005 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.
*/

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

#include "config.h"
#include "uim.h"
#include "uim-scm.h"
#ifndef UIM_SCM_NESTED_EVAL
#include "uim-compat-scm.h"
#endif
#include "plugin.h"
#include "context.h"

#ifndef HAVE_DLFUNC
#define dlfunc dlsym
#endif

#define PLUGIN_PREFIX "libuim-"
#define PLUGIN_SUFFIX ".so"

static uim_plugin_info_list *uim_plugin_list = NULL;
static void plugin_list_append(uim_plugin_info_list *entry);

static uim_lisp 
plugin_load(uim_lisp _name) {
  uim_plugin_info *info;
  uim_plugin_info_list *info_list_entry;
  char *plugin_name;
  char *plugin_lib_filename, *plugin_scm_filename;
#ifdef UIM_SCM_NESTED_EVAL
  uim_lisp lib_path = uim_scm_eval_c_string("uim-plugin-lib-load-path");
  uim_lisp scm_path = uim_scm_eval_c_string("uim-plugin-scm-load-path");
#else
  uim_lisp lib_path = uim_scm_symbol_value("uim-plugin-lib-load-path");
  uim_lisp scm_path = uim_scm_symbol_value("uim-plugin-scm-load-path");
#endif
  uim_lisp path_car, path_cdr;

  size_t len;
  
  plugin_name = uim_scm_c_str(_name);
  
  if(plugin_name == NULL) {
    return uim_scm_f();
  }

/*  fprintf(stderr, "uim-plugin-lib-load-path\n"); */
  for(path_car = uim_scm_car(lib_path), path_cdr = uim_scm_cdr(lib_path);
      path_car != uim_scm_f();
      path_car = uim_scm_car(path_cdr), path_cdr = uim_scm_cdr(path_cdr))
  {
    int fd;
    char *path = uim_scm_c_str(path_car);
    len = strlen(path) + 1 + strlen(PLUGIN_PREFIX) + strlen(plugin_name)+ strlen(PLUGIN_SUFFIX) + 1;
    plugin_lib_filename = malloc(sizeof(char) * len);
    snprintf(plugin_lib_filename, len, "%s/%s%s%s",
	     path, PLUGIN_PREFIX, plugin_name, PLUGIN_SUFFIX);
    fd = open(plugin_lib_filename, O_RDONLY);
    if(fd >= 0) {
      close(fd);
      break;
    }
    free(plugin_lib_filename);
    plugin_lib_filename = NULL;
  }
  
  for(path_car = uim_scm_car(scm_path), path_cdr = uim_scm_cdr(scm_path);
      path_car != uim_scm_f();
      path_car = uim_scm_car(path_cdr), path_cdr = uim_scm_cdr(path_cdr))
  {
    int fd;
    char *path = uim_scm_c_str(path_car);
    len = strlen(path) + 1 + strlen(plugin_name)+ strlen(".scm") + 1;
    plugin_scm_filename = malloc(sizeof(char) * len);
    snprintf(plugin_scm_filename, len, "%s/%s.scm", path, plugin_name);
    fd = open(plugin_scm_filename, O_RDONLY);
    if(fd >= 0) {
      close(fd);
      break;
    }
    free(plugin_scm_filename);
    plugin_scm_filename = NULL;
  }
  
  if(plugin_lib_filename == NULL || plugin_scm_filename == NULL) {
    return uim_scm_f();
  }
  
  info_list_entry = malloc(sizeof(uim_plugin_info_list));
  info = malloc(sizeof(uim_plugin_info));
  
  info->library = dlopen(plugin_lib_filename, RTLD_NOW);
  free(plugin_lib_filename);

  if(info->library == NULL) {
    fprintf(stderr, "load failed %s\n", dlerror());
    return uim_scm_f();
  }

  info->plugin_instance_init = (void (*)(void))dlfunc(info->library,
						      "uim_plugin_instance_init");
  info->plugin_instance_quit = (void (*)(void))dlfunc(info->library,
						      "uim_plugin_instance_quit");
  if(info->plugin_instance_init) {
    (info->plugin_instance_init)();
  } else {
    fprintf(stderr, "%s plugin init failed\n", plugin_name);
  }
  /*	plugin_list_append(uim_plugin_entry); */

  uim_scm_require_file(plugin_scm_filename);
  free(plugin_scm_filename);
  free(plugin_name);
  return uim_scm_t();
}

static void plugin_list_append(uim_plugin_info_list *entry)
{
  uim_plugin_info_list *cur;
  
  if(uim_plugin_list != NULL) {
    for(cur = uim_plugin_list; cur->next != NULL; cur = cur->next)
      ;
    cur->next = entry;
  } else {
    uim_plugin_list = entry;
  }
}

static uim_lisp
plugin_unload(uim_lisp _name)
{
  /* XXX: Dynamic unloading is not supported yet*/
  return uim_scm_f();
}

/* Called from uim_init */
void uim_init_plugin(void)
{
  /* This function is called before scheme files are loaded. Plugin's search 
   * path(both .so and .scm) should be got from plugin.scm.
   */
  char *plugin_lib_dir_env;
  char *plugin_scm_dir_env;
  struct passwd *pw;
  size_t len;

  uim_scm_init_subr_1("load-plugin", plugin_load);
  uim_scm_init_subr_1("unload-plugin", plugin_unload);

  return;
}

/* Called from uim_quit */
void uim_quit_plugin(void)
{
  uim_plugin_info_list *cur;
  for(cur = uim_plugin_list; cur != NULL; cur = cur->next)
    {
      (*((cur->plugin)->plugin_instance_quit))();
      dlclose((cur->plugin)->library);
    }
}

#if 0
int
uim_plugin_instance_query(struct _uim_plugin_info *info)
{
  /* XXX: not implemented */
  return 0;
}
#endif
