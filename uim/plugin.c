/*
  $Id$

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
#include <errno.h>

#include "config.h"
#include "uim.h"
#include "uim-scm.h"
#include "plugin.h"
#include "context.h"

static uim_plugin_info_list *uim_plugin_list = NULL;
static void plugin_list_append(uim_plugin_info_list *entry);

static uim_lisp 
plugin_load(uim_lisp _module_filename) {
	uim_plugin_info *info;
	uim_plugin_info_list *info_list_entry;
	char *module_filename;
	char *module_filename_fullpath;
	char *module_filename_suffix;
	size_t len;

	module_filename = uim_scm_c_str(_module_filename);

	if(module_filename == NULL) {
	  return uim_scm_f();
	}

	len = strlen(module_filename);
	module_filename_suffix = strrchr(module_filename, '.');
	if(len < 3 || module_filename_suffix == NULL) {
	  free(module_filename);
	  return uim_scm_f();
	}

	if( module_filename[0] != '/') {
	  /* FIXME: Need clean up! */
	  char *plugindir_env = getenv("LIBUIM_PLUGIN_LIB_DIR");
       	  if(plugindir_env != NULL) {
	    len = strlen(plugindir_env) + strlen(module_filename) + 2;
	    module_filename_fullpath = malloc(sizeof(char*) * len);
	    snprintf(module_filename_fullpath, len, "%s/%s",
		     plugindir_env, module_filename);
	  } else {
	    len = strlen(UIM_SYS_PLUGIN_LIB_DIR) + strlen(module_filename) + 2;
	    module_filename_fullpath = malloc(sizeof(char*) * len);
	    snprintf(module_filename_fullpath, len, "%s/%s",
		     UIM_SYS_PLUGIN_LIB_DIR, module_filename);
	  }
	} else {
	  module_filename_fullpath = strdup(module_filename);
	}

	dlopen(module_filename_fullpath, RTLD_GLOBAL|RTLD_NOW);

	info_list_entry = malloc(sizeof(uim_plugin_info_list));
	info = malloc(sizeof(uim_plugin_info));

	printf("load %s\n",module_filename_fullpath);
	info->library = dlopen(module_filename_fullpath,
			      RTLD_NOW);
	if(info->library == NULL) {
	  
	  printf("load failed %s\n", dlerror());
	  return uim_scm_f();
	}

	free(module_filename_fullpath);
	free(module_filename);

	info->plugin_init = dlsym(info->library, "plugin_init");
    
	if(info->plugin_init) {
	  printf("plugin init\n");
	  (info->plugin_init)();
	}
	/*	plugin_list_append(uim_plugin_entry); */

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
  uim_scm_init_subr_1("load-plugin", plugin_load);
  uim_scm_init_subr_1("unload-plugin", plugin_unload);
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
