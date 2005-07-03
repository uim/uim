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
#include "uim-compat-scm.h"

#define MODULE_LIST_FILENAME UIM_DATADIR"/modules"
#define LOADER_SCM_FILENAME  UIM_DATADIR"/loader.scm"
#define INSTALLED_MODULES_SCM_FILENAME  UIM_DATADIR"/loader.scm"

static void
print_usage(void)
{
  printf("FIXME: write usage\n");
}

static uim_lisp
read_module_list(void)
{
  FILE *fp = fopen(MODULE_LIST_FILENAME, "r");
  char buf[1024];
  uim_lisp module_list = uim_scm_null_list();
  
  if(!fp) {
    perror("Failed to read module list");
    return uim_scm_f();
  }
  while (fgets (buf, sizeof(buf), fp) != NULL) {
    if(buf[0] == '#' || buf[0] == '\n') {
      continue; /* comment line or blank line */
    }
    else if(buf[strlen(buf)-1] == '\n') {
      buf[strlen(buf)-1] = '\0'; /* Clear \n. */
    }
    module_list = uim_scm_cons(uim_scm_intern_c_str(buf), module_list);
  }
  fclose(fp);
  return module_list;
}

static uim_lisp
write_module_list(uim_lisp new_module, uim_lisp module_list)
{
  FILE *fp = fopen(MODULE_LIST_FILENAME, "w");

  if(!fp) {
    perror("Failed to write module list");
    return uim_scm_f();
  }

  fputs("# This is an automatically generated file. DO NOT EDIT.\n\n", fp);

  if(uim_scm_stringp(new_module) == UIM_TRUE) {
    fputs(uim_scm_refer_c_str(new_module), fp);
    fputs("\n",fp);
  }

  if(uim_scm_consp(module_list) == UIM_TRUE) {
    
    while(1) {
      uim_lisp module_name = uim_scm_car(module_list);
      fputs(uim_scm_refer_c_str(module_name), fp);
      fputs("\n",fp);
      module_list = uim_scm_cdr(module_list);
      if(module_list == uim_scm_null_list()) {
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
  FILE *fp = fopen(LOADER_SCM_FILENAME, "w");

  if(!fp) {
    perror("Failed to open loader.scm");
    return uim_scm_f();
  }

  fputs("# This is an automatically generated file. DO NOT EDIT.\n\n", fp);

  fputs(uim_scm_refer_c_str(str), fp);
  fclose(fp);
  return uim_scm_t();
}

static uim_lisp
write_installed_modules_scm(uim_lisp str)
{
  FILE *fp = fopen(INSTALLED_MODULES_SCM_FILENAME, "w");

  if(!fp) {
    perror("Failed to open loader.scm");
    return uim_scm_f();
  }

  fputs("# This is an automatically generated file. DO NOT EDIT.\n\n", fp);

  fputs(uim_scm_refer_c_str(str), fp);
  fclose(fp);
  return uim_scm_t();
}

int
main(int argc, char *argv[]) {
  int registerp;
  uim_lisp form;

  if(argc <= 2) {
    print_usage();
    exit(EXIT_FAILURE);
  }

  if(strcmp(argv[1], "--register") == 0) {
    registerp = 1;
  } else if(strcmp(argv[1], "--unregister") == 0) {
    registerp = 2;
  } else {
    print_usage();
    exit(EXIT_FAILURE);
  }

  if(!argv[2]) {
    print_usage();
    exit(EXIT_FAILURE);
  }

  /* FIXME: To generate loader.scm, we need this setenv for now.
     But it's a dirty hack, not appropriate. I guess we need entirely new module system. */
  setenv("LIBUIM_VANILLA", "1", 1);

  uim_init();

  uim_scm_set_verbose_level(1);

  uim_scm_init_subr_0("read-module-list", read_module_list);
  uim_scm_init_subr_2("write-module-list", write_module_list);

  uim_scm_init_subr_1("write-loader.scm", write_loader_scm);
  uim_scm_init_subr_1("write-installed-modules.scm", write_installed_modules_scm);

  uim_scm_require_file("uim-module-manager.scm");

  if(registerp == 1) {
    form = uim_scm_list2(uim_scm_intern_c_str("register-module"),
			 uim_scm_qintern_c_str(argv[2]));
  }
  if(registerp == 2) {
    form = uim_scm_list2(uim_scm_intern_c_str("unregister-module"),
			 uim_scm_qintern_c_str(argv[2]));
  }

  uim_scm_eval(form);

  uim_quit();

  return 0;
}
