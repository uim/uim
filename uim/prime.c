/*

  Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.
*/

#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "context.h"
#include "plugin.h"

static FILE *primer = NULL, *primew = NULL;
static int prime_pid = 0;

static char *prime_command = "prime";

static uim_lisp
prime_send_command(uim_lisp str_)
{
  const char *str = uim_scm_refer_c_str(str_);
  char *result;
  uim_lisp ret;

  result = uim_ipc_send_command(&prime_pid, &primer, &primew, prime_command, str);

  if(result == NULL)
    {
      return uim_scm_f();
    }

 ret = uim_scm_make_str(result);
 free(result);
 return ret;

}

static uim_lisp
prime_lib_init(void)
{
  prime_pid = uim_ipc_open_command(prime_pid, &primer, &primew, prime_command );
  if(prime_pid == 0) {
    return uim_scm_f();
  }
  return uim_scm_t();
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_subr_0("prime-lib-init", prime_lib_init);
  uim_scm_init_subr_1("prime-lib-send-command", prime_send_command);
}

void
uim_plugin_instance_quit(void)
{
  if(primew) {
    uim_ipc_send_command(&prime_pid, &primer, &primew, prime_command, "close\n");
    fclose(primew);
    primew = NULL;
  }

  if(primer) {
    fclose(primer);
    primer = NULL;
  }
}
