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
#include <histedit.h>

#include "uim.h"
#include "uim-scm.h"
#include "dynlib.h"


static EditLine *el;
static History *hist;
static HistEvent hev;
static uim_lisp editline_readline(void);
static char *prompt(EditLine *e);

void
uim_plugin_instance_init(void)
{
  el = el_init("uim", stdin, stdout, stderr);
  el_set(el, EL_PROMPT, &prompt);
  el_set(el, EL_EDITOR, "emacs");

  hist = history_init();
  history(hist, &hev, H_SETSIZE, 100);
  el_set(el, EL_HIST, history, hist);
  el_source(el, NULL);

  uim_scm_init_proc0("editline-readline", editline_readline);

  uim_scm_callf("provide", "s", "editline");
}

void
uim_plugin_instance_quit(void)
{
  history_end(hist);
  el_end(el);
}

static uim_lisp
editline_readline(void)
{
  const char *line;
  int count = 0;

  line = el_gets(el, &count);

  if (count > 0 && line) {
    history(hist, &hev, H_ENTER, line);
    return uim_scm_make_str(line);
  } else {
    return uim_scm_eof();
  }
}

static char *
prompt(EditLine *e)
{
  uim_lisp p;

  p = uim_scm_callf("editline-prompt", "");

  /* libedit does not free the prompt str */
  return (char *)uim_scm_refer_c_str(p);
}
