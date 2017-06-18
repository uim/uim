/* prototype 
 * TOOOOOOOOOOOOOOOOOOOOOOOOOOO experimental
 */
/*

  Copyright (c) 2003-2013 uim Project https://github.com/uim/uim

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
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include "uim.h"
#include "uim-util.h"

static struct agent_context {
  uim_context uc;
  struct agent_context *next;
} default_context;

static void
commit_cb(void *ptr, const char *str)
{
  printf("%s commited\n", str);
}

static void
clear_cb(void *ptr)
{
}

static void
pushback_cb(void *ptr, int attr, const char *str)
{
  printf(" preedit %s\n", str);
}

static void
update_cb(void *ptr)
{
}

static void
init_agent()
{
  uim_context uc;
  int i, nr;
  if (uim_init() == -1) {
    printf("failed to init\n");
    exit(EXIT_FAILURE);
  }
  /**/
  uc =uim_create_context(&default_context,
			 "EUC-JP", NULL, NULL,
			 uim_iconv,
			 commit_cb);
  nr = uim_get_nr_im(uc);
  for (i = 0; i < nr; i++) {
    printf("%s\n", uim_get_im_name(uc, i));
  }
  uim_set_preedit_cb(uc, clear_cb, pushback_cb, update_cb);
  default_context.uc = uc;
  default_context.next = NULL;
}

int
main(int argc, char **argv)
{
  struct agent_context *ac = &default_context;
  uim_init();uim_quit();
  return 0;
  init_agent();
  /**/
  printf("Hello World.\n");
  while (1) {
    char buf[32];
    if (fgets(buf, 32, stdin) == NULL) {
      continue;
    }
    if (isalpha((unsigned char)buf[0])) {
      uim_press_key(ac->uc, buf[0], 0);
    } else {
      uim_press_key(ac->uc, UKey_Return, 0);
    }
  }
  return 0;
}
