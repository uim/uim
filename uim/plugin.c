/*
  $Id:$

  plugin.c:

  Copyright (c) 2004 uim Project http://uim.freedesktop.org/

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
#include "plugin.h"
#include "context.h"

#ifndef HAVE_DLFUNC
#define dlfunc dlsym
#endif

#ifdef __APPLE__
  #define PLUGIN_SUFFIX ".dylib"
#else
  #define PLUGIN_SUFFIX ".so"
#endif /* __APPLE__ */

static uim_plugin_info_list *uim_plugin_list = NULL;
static void plugin_list_append(uim_plugin_info_list *entry);

static char **plugin_lib_path = NULL;
static char **plugin_scm_path = NULL;

static uim_lisp 
plugin_load(uim_lisp _name) {
  uim_plugin_info *info;
  uim_plugin_info_list *info_list_entry;
  char *tmp;
  char *module_filename;
  char *module_filename_fullpath;
  char *module_scm_filename;
  char **libpath, **scmpath;

  int i = 0;
  size_t len;
  
  tmp = uim_scm_c_str(_name);
  
  if(tmp == NULL) {
    return uim_scm_f();
  }

  len = strlen(tmp) + strlen(PLUGIN_SUFFIX) + 1;
  module_filename = malloc(sizeof(char) * len);
  snprintf(module_filename, len, "%s%s", tmp, PLUGIN_SUFFIX);

  libpath = plugin_lib_path;
  scmpath = plugin_scm_path;

  if(module_filename[0] != '/') {
    for(i = 2; i >= 0; i--, libpath--, scmpath--) {
      int fd;

      len = strlen(*libpath) + 1 + strlen(module_filename) + 1;
      module_filename_fullpath = malloc(sizeof(char) * len);
      snprintf(module_filename_fullpath, len, "%s/%s",
	       *libpath, module_filename);
      fd = open(module_filename_fullpath, O_RDONLY);
      if(fd >= 0) {
	close(fd);
	break;
      }

      free(module_filename_fullpath);
      module_filename_fullpath = NULL;
    }
  } else {
    module_filename_fullpath = strdup(module_filename);
  }

  if(module_filename_fullpath == NULL) {
    free(tmp);  
    return uim_scm_f();
  }

  dlopen(module_filename_fullpath, RTLD_GLOBAL|RTLD_NOW);
  
  info_list_entry = malloc(sizeof(uim_plugin_info_list));
  info = malloc(sizeof(uim_plugin_info));
  
  fprintf(stderr, "load %s\n",module_filename_fullpath);
  info->library = dlopen(module_filename_fullpath, RTLD_NOW);
  if(info->library == NULL) {
    fprintf(stderr, "load failed %s\n", dlerror());
    return uim_scm_f();
  }

  free(module_filename_fullpath);
  free(module_filename);

  info->plugin_init = (void (*)(void))dlfunc(info->library, "plugin_init");

  if(info->plugin_init) {
    fprintf(stderr, "plugin init\n");
    (info->plugin_init)();
  }
  /*	plugin_list_append(uim_plugin_entry); */

  len = strlen(*scmpath) + 1 + strlen(tmp) + strlen(".scm") + 1;
  module_scm_filename = malloc(sizeof(char) * len);
  snprintf(module_scm_filename, len, "%s/%s.scm", *scmpath, tmp);
  uim_scm_require_file(module_scm_filename); 

  free(module_scm_filename);
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
plugin_unload(uim_lisp _filename)
{
  /* XXX: Dynamic unloading is not supported yet*/
  return uim_scm_f();
}

/* Called from uim_init */
void uim_init_plugin(void)
{
  /* This function is called before scheme files are loaded. Plugin's search 
   * path(both .so(.dylib) and .scm) should be got from plugin.scm.
   */
  char *plugin_lib_dir_env;
  char *plugin_scm_dir_env;
  struct passwd *pw;
  size_t len;

  uim_scm_init_subr_1("load-plugin", plugin_load);
  uim_scm_init_subr_1("unload-plugin", plugin_unload);

  if(plugin_scm_path != NULL && plugin_scm_path != NULL) {
    return;
  }

  plugin_lib_dir_env = getenv("LIBUIM_PLUGIN_LIB_DIR");
  plugin_scm_dir_env = getenv("LIBUIM_SCM_FILES");

  pw = getpwuid(getuid());

  plugin_lib_path = malloc(sizeof(char *) * 3);
  plugin_scm_path = malloc(sizeof(char *) * 3);
  memset(plugin_lib_path, 0, sizeof(char *) * 3);
  memset(plugin_scm_path, 0, sizeof(char *) * 3);

  *plugin_lib_path++ = strdup(plugin_lib_dir_env);
  *plugin_lib_path++ = strdup(UIM_SYS_PLUGIN_LIB_DIR);
  *plugin_scm_path++ = strdup(plugin_scm_dir_env);
  *plugin_scm_path++ = strdup(UIM_SYS_PLUGIN_SCM_DIR);

  len = strlen(pw->pw_dir) + strlen("/.uim.d/plugin") + 1;
  *plugin_lib_path = malloc(sizeof(char) * len);
  *plugin_scm_path = malloc(sizeof(char) * len);
  snprintf(*plugin_lib_path, len, "%s/.uim.d/plugin", pw->pw_dir);
  snprintf(*plugin_scm_path, len, "%s/.uim.d/plugin", pw->pw_dir);

  return;
}

/* Called from uim_quit */
void uim_quit_plugin(void)
{
  uim_plugin_info_list *cur;
  for(cur = uim_plugin_list; cur != NULL; cur = cur->next)
    {
      
      (*((cur->plugin)->plugin_quit))();
      dlclose((cur->plugin)->library);
    }
}
