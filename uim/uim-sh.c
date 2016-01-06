/*
  uim-sh.c: uim interactive shell for debugging, batch processing and
            serving as generic inferior process

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

#include "uim.h"
#include "uim-scm.h"


int
main(int argc, char *argv[])
{
  uim_lisp args, exit_status_;
  int exit_status;

  /* TODO: be able to suppress ordinary initialization process */
  uim_init();

  uim_scm_require_file("uim-sh.scm");

  args = uim_scm_null();
  exit_status_ = uim_scm_f();
  uim_scm_gc_protect(&args);
  uim_scm_gc_protect(&exit_status_);

  args = uim_scm_array2list((void **)argv, argc,
			    (uim_lisp (*)(void *))uim_scm_make_str);
  exit_status_ = uim_scm_callf("uim-sh", "o", args);
  exit_status  = uim_scm_c_int(exit_status_);

  uim_quit();

  return exit_status;
}
