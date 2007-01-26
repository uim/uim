/*
  uim-sh.c: uim interactive shell for debugging, batch processing and
            serving as generic inferior process

  Copyright (c) 2003-2007 uim Project http://uim.freedesktop.org/

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
#include "uim-compat-scm.h"
#include "uim-scm-abbrev.h"

#ifdef LIBEDIT
#include "editline.h"
#endif

#ifdef UIM_SH_USE_EXIT_HOOK
extern int uim_siod_fatal;
#endif

int
main(int argc, char *argv[])
{
  long verbose;
  uim_lisp args;

  uim_scm_set_output(stdout);
  
  /* TODO: be able to suppress ordinary initialization process */
  uim_init();

#ifdef LIBEDIT
  editline_init();
#endif

  verbose = uim_scm_get_verbose_level();
  uim_scm_set_verbose_level(1);
  uim_scm_require_file("uim-sh.scm");
#ifdef UIM_SH_USE_EXIT_HOOK
  /*
     is not working even if uim_siod_fatal is accessible. outermost
     *catch affects me?
  */
  if (uim_siod_fatal)
    return 1;
#endif

  /*
    verbose level must be greater than or equal to 1 to print anything
  */
  if (verbose < 1)
    verbose = 1;
  uim_scm_set_verbose_level(verbose);

  args = uim_scm_c_strs_into_list(argc, (const char *const *)argv);
  uim_scm_callf("uim-sh", "o", args);

#ifdef UIM_SH_USE_EXIT_HOOK
  /* is not working even if uim_siod_fatal is accessible. outermost
   * *catch affects me?
   */
  if (uim_siod_fatal)
    return 1;
#endif

#ifdef LIBEDIT
  editline_quit();
#endif

  uim_quit();

  return 0;
}
