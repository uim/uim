/*

  Copyright (c) 2003-2011 uim Project http://code.google.com/p/uim/

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

#include "uim-internal.h"

static FILE *spell_r = NULL, *spell_w = NULL;
static int spell_pid = 0;

static LISP
spellcheck_init(void)
{
  spell_pid = uim_ipc_open_command(spell_pid, &spell_r,
				   &spell_w, get_spell_command() );
  if (spell_pid == 0) {
    return NIL;
  }
  return siod_true_value();
}

static LISP
spellcheck_send_command(LISP str_)
{
  char *str = get_c_string( str_ );
  char *result;
  LISP ret;

  result = uim_ipc_send_command(&spell_pid, &spell_r, &spell_w, get_spell_command(), str);

  if (result == NULL)
    {
      return NIL;
    }

 ret = strcons( strlen(result), result );
 free(result);
 return ret;
}


void
uim_init_spellcheck(void)
{
 init_subr_0("spellcheck-lib-init", spellcheck_init);
 init_subr_1("spellcheck-lib-send-command", spellcheck_send_command);

}
