/*
  uim-module-manager.c: source file for uim-module-manager.

  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim

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
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-internal.h"

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

struct main_args {
  int argc;
  char **argv;
};
static void *main_internal(struct main_args *args);

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
  strlcpy(dest, a, len);
  strlcat(dest, b, len);
  return dest;
}

static uim_lisp
append_module_names(uim_lisp modules, const char *new_module)
{
  return uim_scm_callf("append", "oo", modules, LIST1(MAKE_STR(new_module)));
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
main(int argc, char *argv[])
{
  struct main_args args;

  if (argc <= 1) {
    print_usage();
    exit(EXIT_FAILURE);
  }
    
  /* FIXME: To generate loader.scm, we need this setenv for now.
     But it's a dirty hack, not appropriate. I guess we need entirely
     new module system. */
  setenv("LIBUIM_VANILLA", "1", 1);
  
  uim_init();

  args.argc = argc;
  args.argv = argv;
  uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)main_internal, &args);

  uim_quit();

  return EXIT_SUCCESS;
}

static void *
main_internal(struct main_args *args)
{
  int i, argc;
  int action = None;
  char **argv;
  uim_lisp module_names;

  argc = args->argc;
  argv = args->argv;
  module_names = uim_scm_null();
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
  
  if (action == None || (action != UnRegisterAll && !module_names)) {
    print_usage();
    exit(EXIT_FAILURE);
  }

  uim_scm_init_proc1("write-loader.scm", write_loader_scm);
  uim_scm_init_proc1("write-installed-modules.scm", write_installed_modules_scm);

  if (!uim_scm_require_file("uim-module-manager.scm"))
    uim_fatal_error("failed to require uim-module-manager.scm");

  if (path) {
    char *extra_file = concat(path, "/installed-modules.scm");
    struct stat st;
    if (stat(extra_file, &st) != -1)
      uim_scm_require_file(extra_file);
    free(extra_file);
  }

  return uim_scm_callf(action_command[action], "o", module_names);
}
