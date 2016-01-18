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

#include <stdlib.h>
#include <string.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-helper.h"
#include "dynlib.h"

#include "bsdlook.h"

struct uim_look_look_internal_args {
  uim_look_ctx *ctx;
  char *dict_str;
  int words;
};

static uim_lisp
uim_look_look_internal(struct uim_look_look_internal_args *args)
{
  uim_lisp ret_ = uim_scm_null();
  int words = args->words;
  char buf[8192];
  size_t len = strlen(args->dict_str);

  while (uim_look_get(args->dict_str, buf, sizeof(buf), args->ctx) != 0) {
    /* don't use the word itself */
    if (strcasecmp(buf, args->dict_str) == 0)
      continue;
    if (len < strlen(buf))
      ret_ = CONS(MAKE_STR(buf + len), ret_);
    if (words != -1) {
      words--;
      if (words == 0)
	break;
    }
  }
  return ret_;
}

static uim_lisp
uim_look_look(uim_lisp isdict_, uim_lisp iscase_, uim_lisp words_, uim_lisp dict_, uim_lisp str_)
{
  const char *dict = REFER_C_STR(dict_);
  const char *str = REFER_C_STR(str_);
  uim_look_ctx *ctx;
  char *dict_str;
  uim_lisp ret_ = uim_scm_f();
  int words = -1;

  ctx = uim_look_init();

  uim_look_set_option_dictionary_order(C_BOOL(isdict_), ctx);
  uim_look_set_option_ignore_case(C_BOOL(iscase_), ctx);

  if (!ctx)
    uim_fatal_error("uim_look_init() failed");

  if (!uim_look_open_dict(dict, ctx))
    return ret_;

  dict_str = uim_strdup(str);

  if (INTP(words_))
    words = C_INT(words_);

  ret_ = uim_scm_null();
  if (uim_look(dict_str, ctx) != 0) {
    struct uim_look_look_internal_args args;

    uim_look_set(ctx);

    args.ctx = ctx;
    args.dict_str = dict_str;
    args.words = words;
    ret_ = (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_look_look_internal,
						      (void *)&args);
  }

  uim_look_finish(ctx);
  free(dict_str);

  return uim_scm_callf("reverse", "o", ret_);
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc5("look-lib-look", uim_look_look);
}

void
uim_plugin_instance_quit(void)
{
}
