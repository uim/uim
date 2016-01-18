/*

  Copyright (c) 2009-2013 uim Project https://github.com/uim/uim

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

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-helper.h"
#include "uim-notify.h"
#include "dynlib.h"

#include "uim-eb.h"

static uim_lisp
c_uim_eb_new(uim_lisp bookpath_)
{
  void *ret;

  if ((ret = uim_eb_new(REFER_C_STR(bookpath_))) == NULL)
    return uim_scm_f();
  return MAKE_PTR(ret);
}

static uim_lisp
c_uim_eb_search_text(uim_lisp ueb_, uim_lisp text_, uim_lisp encoding_)
{
  char *str;
  const char *enc;

  enc = NULLP(encoding_) ? "UTF-8" : REFER_C_STR(encoding_);

  if ((str = uim_eb_search_text(C_PTR(ueb_), REFER_C_STR(text_), enc)) == NULL)
    return MAKE_STR("");
  return MAKE_STR_DIRECTLY(str);
}

static uim_lisp
c_uim_eb_destroy(uim_lisp ueb_)
{
  uim_eb_destroy(C_PTR(ueb_));
  return uim_scm_t();
}

void
uim_plugin_instance_init(void)
{
  uim_eb_open();

  uim_scm_init_proc1("eb-new", c_uim_eb_new);
  uim_scm_init_proc3("eb-search-text", c_uim_eb_search_text);
  uim_scm_init_proc1("eb-destroy", c_uim_eb_destroy);
}

void
uim_plugin_instance_quit(void)
{
  uim_eb_close();
}

