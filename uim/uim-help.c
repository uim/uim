/*
  uim-help.c: uim help launcher.

  Copyright (c) 2003-2009 uim Project http://code.google.com/p/uim/

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

#include "uim.h"
#include "uim-scm.h"
#include "uim-helper.h"
#include "uim-scm-abbrev.h"

char branch[BUFSIZ];

int uim_fd = -1;

static void
uim_help_disconnect_cb(void)
{
  uim_fd = -1;
}

static int
uim_help_get_current_branch(void)
{
  FILE *uim_fp = NULL;

  uim_fd = uim_helper_init_client_fd(uim_help_disconnect_cb);
  uim_helper_client_get_prop_list();

  if ((uim_fp = fdopen(uim_fd, "r")) == NULL) {
    perror("fdopen");
    uim_helper_close_client_fd(uim_fd);
    return 0;
  }

  /* TODO: trap signal */
  while (1) {
    const char prot_branch[] = "branch\t";
    char buf[BUFSIZ];

    fgets(buf, sizeof(buf), uim_fp);
    if (strncmp(buf, prot_branch, sizeof(prot_branch) - 1) == 0) {
      char *p = buf + sizeof(prot_branch) - 1;
      size_t len = strlen(p);

      if (len > 0)
	strlcpy(branch, p, len);
      p = strchr(branch, '\t');
      if (!p) {
	uim_helper_close_client_fd(uim_fd);
	return 0;
      }
      *p = '\0';
      break;
    }
  }
  uim_helper_close_client_fd(uim_fd);
  return 1;
}

int
main(void)
{
  uim_lisp branch_, exit_status_;
  int exit_status;

  /* TODO: be able to suppress ordinary initialization process */
  uim_init();

  if (!uim_help_get_current_branch())
    return 1;

  uim_scm_require_file("uim-help.scm");
  branch_ = uim_scm_null();
  uim_scm_gc_protect(&branch_);
  branch_ = MAKE_SYM(branch);

  exit_status_ = uim_scm_f();
  uim_scm_gc_protect(&exit_status_);

  exit_status_ = uim_scm_callf("uim-help", "o", branch_);
  exit_status  = uim_scm_c_int(exit_status_);

  uim_quit();

  return exit_status;
}
