/* 
  Copyright (c) 2003-2008 uim Project http://code.google.com/p/uim/

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

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-helper.h"
#include "dynlib.h"

#include "bsdlook.h"

static uim_lisp
uim_look_look(uim_lisp dict_, uim_lisp str_)
{
  const char *dict = REFER_C_STR(dict_);
  const char *str = REFER_C_STR(str_);
  uim_look_ctx *ctx;
  char buf[1024];
  char *dict_str;
  size_t len;
  uim_lisp ret_ = uim_scm_f();

  ctx = uim_look_init();
  if (!ctx)
    uim_fatal_error("uim_look_init() failed");

  if (!uim_look_open_dict(dict, ctx))
    return ret_;

  dict_str = uim_strdup(str);
  len = strlen(str);

  ret_ = uim_scm_null();
  if (uim_look(dict_str, ctx) != 0) {
    uim_look_set(ctx);
    while (uim_look_get(dict_str, buf, sizeof(buf), ctx) != 0) {
      /* don't use the word itself */
      if (strcasecmp(buf, dict_str) == 0)
	continue;
      if (len < strlen(buf))
	ret_ = CONS(MAKE_STR(buf + len), ret_);
    }
  }

  uim_look_finish(ctx);
  free(dict_str);

  return uim_scm_callf("reverse", "o", ret_);
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc2("look-lib-look", uim_look_look);
}

void
uim_plugin_instance_quit(void)
{
}
