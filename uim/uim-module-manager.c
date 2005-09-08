/*
  uim-module-manager.c: source file for uim-module-manager.

  Copyright (c) 2005 uim Project http://uim.freedesktop.org/

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

#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "config.h"

#include "uim.h"
#include "uim-scm.h"
#include "context.h"

#define MODULE_LIST_FILENAME UIM_DATADIR"/modules"
#define LOADER_SCM_FILENAME  UIM_DATADIR"/loader.scm"
#define INSTALLED_MODULES_SCM_FILENAME  UIM_DATADIR"/installed-modules.scm"

static char *path;

enum Action {
  Register,
  UnRegister,
  UnRegisterAll,
  None
};

char *action_command[] = {
  "register-modules",
  "unregister-modules",
  "unregister-all-modules",
  NULL
};

/* Utility function */
static char *
concat(const char *a, const char *b)
{
  int len;
  char *dest;
  if (!a || !b)
    return NULL;
  len = strlen(a) + strlen(b) + 1;
  dest = malloc(len);
  dest = strcpy(dest, a);
  strcat(dest, b);
  return dest;
}

static char *
append_module_names(char *modules, const char *new_module)
{
  if (!modules)
    return strdup(new_module);

  modules = realloc(modules, strlen(modules) + strlen(new_module) + 2);
  if (modules) {
    strcat(modules, " ");
    strcat(modules, new_module);
  }
  return modules;
}

static void
print_usage(void)
{
  printf("Usage:\n");
  printf("  uim-module-manager [options]\n\n");
  printf("Options:\n");
  printf("  --register <modules>   Register the modules\n");
  printf("  --unregister <modules> Unregister the modules\n");
  printf("  --path <path>          Target path where installed-modules.scm\n");
  printf("                         and loader.scm to be installed\n");
  printf("  --unregister-all       Unregister all modules\n\n");
  printf("Example:\n");
  printf("  uim-module-manager --register anthy skk\n");
  printf("  uim-module-manager --register prime --path /usr/local/share/uim\n");
  printf("  uim-module-manager --register personal-module --path ~/.uim.d/plugin\n\n");
  printf("Note:\n");
  printf("  Registeration and unregistration cannot be done simultaneously.\n\n");
}

static uim_lisp
read_module_list(void)
{
  FILE *fp;
  char buf[1024];
  uim_lisp module_list = uim_scm_null_list();

  if (path) {
    char *p = concat(path, "/modules");
    fp = fopen(p, "r");
    free(p);
  } else {
    fp = fopen(MODULE_LIST_FILENAME, "r");
  }

  if (!fp) {
    /* fprintf(stderr, "Warning: failed to read module list.\n"); */
    return uim_scm_f();
  }
  while (fgets(buf, sizeof(buf), fp) != NULL) {
    if (buf[0] == '#' || buf[0] == '\n') {
      continue; /* comment line or blank line */
    }
    else if (buf[strlen(buf) - 1] == '\n') {
      buf[strlen(buf) - 1] = '\0'; /* Clear \n. */
    }
    module_list = uim_scm_cons(uim_scm_make_symbol(buf), module_list);
  }
  fclose(fp);
  return module_list;
}

static uim_lisp
write_module_list(uim_lisp new_module, uim_lisp module_list)
{
  FILE *fp;

  if (path) {
    char *p = concat(path, "/modules");
    fp = fopen(p, "w");
    free(p);
  } else {
    fp = fopen(MODULE_LIST_FILENAME, "w");
  }

  if (!fp) {
    perror("Failed to write module list");
    return uim_scm_f();
  }

  fputs("# This is an automatically generated file. DO NOT EDIT.\n\n", fp);

  if (uim_scm_stringp(new_module) == UIM_TRUE) {
    fputs(uim_scm_refer_c_str(new_module), fp);
    fputs("\n", fp);
  }

  if (uim_scm_consp(module_list) == UIM_TRUE) {
    while (1) {
      uim_lisp module_name = uim_scm_car(module_list);
      fputs(uim_scm_refer_c_str(module_name), fp);
      fputs("\n",fp);
      module_list = uim_scm_cdr(module_list);
      if (module_list == uim_scm_null_list()) {
	break;
      }
    }
  }
  
  fclose(fp);
  return uim_scm_t();
}

static uim_lisp
write_loader_scm(uim_lisp str)
{
  FILE *fp;

  if (path) {
    char *p = concat(path, "/loader.scm");
    fp = fopen(p, "w");
    free(p);
  } else {
    fp = fopen(LOADER_SCM_FILENAME, "w");
  }

  if (!fp) {
    perror("Failed to open loader.scm");
    return uim_scm_f();
  }

  fputs(";; This is an automatically generated file. DO NOT EDIT.\n\n", fp);
  fputs(uim_scm_refer_c_str(str), fp);

  fclose(fp);
  return uim_scm_t();
}

static uim_lisp
write_installed_modules_scm(uim_lisp str)
{
  FILE *fp;

  if (path) {
    char *p = concat(path, "/installed-modules.scm");
    fp = fopen(p, "w");
    free(p);
  } else {
    fp = fopen(INSTALLED_MODULES_SCM_FILENAME, "w");
  }

  if (!fp) {
    perror("Failed to open installed-modules.scm");
    return uim_scm_f();
  }

  fputs(";; This is an automatically generated file. DO NOT EDIT.\n\n", fp);
  fputs(uim_scm_refer_c_str(str), fp);

  fclose(fp);
  return uim_scm_t();
}

int
main(int argc, char *argv[]) {
  int i;
  int action = None;
  char *module_names = NULL;

  if (argc <= 1) {
    print_usage();
    exit(EXIT_FAILURE);
  }
    
  /* FIXME: To generate loader.scm, we need this setenv for now.
     But it's a dirty hack, not appropriate. I guess we need entirely
     new module system. */
  setenv("LIBUIM_VANILLA", "1", 1);
  
  uim_init();
  uim_scm_set_verbose_level(1);

  for (i = 0; i < argc; i++) {
    if (strcmp(argv[i], "--register") == 0) {
      if (action != None) {
	action = None;
	break;
      }
      action = Register; i++;
      while (argv[i] && strncmp(argv[i], "--", 2)) {
	module_names = append_module_names(module_names, argv[i]);
	i++;
      }
      i--;
    } else if (strcmp(argv[i], "--unregister") == 0) {
      if (action != None) {
	action = None;
	break;
      }
      action = UnRegister; i++;
      while (argv[i] && strncmp(argv[i], "--", 2)) {
	module_names = append_module_names(module_names, argv[i]);
	i++;
      }
      i--;
    } else if (strcmp(argv[i], "--path") == 0) {
      if (argv[i + 1]) {
	path = argv[i + 1];
      }
    } else if (strcmp(argv[i], "--unregister-all") == 0) {
      if (action != None) {
	action = None;
	break;
      }
      action = UnRegisterAll;
    }
  }
  
  if (action == None) {
    print_usage();
    exit(EXIT_FAILURE);
  }

  uim_scm_init_subr_0("read-module-list", read_module_list);
  uim_scm_init_subr_2("write-module-list", write_module_list);
  uim_scm_init_subr_1("write-loader.scm", write_loader_scm);
  uim_scm_init_subr_1("write-installed-modules.scm", write_installed_modules_scm);

  if (!uim_scm_require_file("uim-module-manager.scm"))
    exit(1);

  /* for unregister-all */
  if (!module_names)
    module_names = "";

  UIM_EVAL_FSTRING2(NULL, "(%s \"%s\")",
		    action_command[action],
		    module_names);

  uim_quit();

  return 0;
}
