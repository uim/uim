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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-util.h"
#include "dynlib.h"
#include "uim-notify.h"
#include "gettext.h"

#define MANA_COMMAND "mana"

static FILE *mana_r;
static FILE *mana_w;
static pid_t mana_pid;

static char *mana_ipc_send_command(pid_t *pid,
				   FILE **read_fp, FILE **write_fp,
				   const char *str);
static uim_lisp mana_init(void);
static uim_lisp mana_eval(uim_lisp buf_);
static uim_lisp eucjp_string_length(uim_lisp str_);

#ifdef DEBUG
static FILE *log;
#endif

static char *
mana_ipc_send_command(pid_t *pid,
		      FILE **read_fp, FILE **write_fp,
		      const char *str)
{
  char *tmp = uim_strdup("");
  char buf[8192];

  struct sigaction act, oact;

  act.sa_handler = SIG_IGN;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;

  sigaction(SIGPIPE, &act, &oact);

  fputs(str, *write_fp);

 again:
  if (fflush(*write_fp) != 0) {
    switch (errno) {
    case EINTR:
      goto again;
    case EPIPE:

      while (!feof(*read_fp)) {
        fgets(buf, sizeof(buf), *read_fp);
        if (buf != NULL) {
          if (strcmp(buf, "err") == 0)
            uim_notify_fatal(N_("uim-mana: Command 'mana' not found."));
          else
            uim_notify_fatal("uim-mana: %s", buf);
	}
      }

      *pid = 0;
      fclose(*read_fp);
      fclose(*write_fp);
      *read_fp = NULL;
      *write_fp = NULL;

      sigaction(SIGPIPE, &oact, NULL);
      free(tmp);

      return NULL;
    default:
      sigaction(SIGPIPE, &oact, NULL);
      free(tmp);
      return NULL;
    }
  }

  sigaction(SIGPIPE, &oact, NULL);

  if (feof(*read_fp)) {
    *pid = 0;
    fclose(*read_fp);
    fclose(*write_fp);
    *read_fp = NULL;
    *write_fp = NULL;
    free(tmp);
    return NULL;
  }

  while (fgets (buf, sizeof(buf), *read_fp) != NULL) {

    tmp = uim_realloc(tmp, strlen(tmp) + strlen(buf) + 1);
    strcat(tmp, buf);

    if (strchr( buf, '\n' )) {
      break;
    }
  }
 
  return tmp;

}

static uim_lisp
mana_init(void)
{
  char buf[100];
  int fd;
  int fl;

  if (mana_pid == 0)
    mana_pid = uim_ipc_open_command(0, &mana_r, &mana_w, MANA_COMMAND);

  if (mana_pid == 0)
    return uim_scm_f();

  fd = fileno(mana_r);
  fl = fcntl(fd, F_GETFL);
  fcntl(fd, F_SETFL, fl | O_NONBLOCK);
  fgets(buf, sizeof(buf), mana_r);
  fcntl(fd, F_SETFL, fl);

  if (feof(mana_r)) {
    mana_pid = 0;
    fclose(mana_r);
    fclose(mana_w);
    mana_r = mana_w = NULL;
    uim_notify_fatal(N_("uim-mana: Command 'mana' not found."));
    return uim_scm_f();
  }
  
  if (ferror(mana_r))
    clearerr(mana_r);

#ifdef DEBUG
  log = fopen("mana.log", "w");
#endif

  return uim_scm_t();
}

static uim_lisp
mana_eval(uim_lisp buf_)
{
  const char *buf = REFER_C_STR(buf_);
  char *ret_buf;
  char *eval_buf;
  uim_lisp ret;

  if (mana_pid == 0)
    return uim_scm_f();

  ret_buf = mana_ipc_send_command(&mana_pid, &mana_r, &mana_w, buf);

  if (ret_buf == NULL)
    return uim_scm_f();

#ifdef DEBUG
  fputs(buf, log);
  fputs(ret_buf, log);
  fflush(log);
#endif

  uim_asprintf(&eval_buf, "'%s", ret_buf);
  ret = uim_scm_eval_c_string(eval_buf);
  free(ret_buf);
  free(eval_buf);

  return ret;
}

static uim_lisp
eucjp_string_length(uim_lisp str_)
{
  const unsigned char *str = (const unsigned char *)REFER_C_STR(str_);
  int len = strlen((const char *)str);

  int ascii = 0;
  int mbyte = 0;

  int i;

  for (i = 0; i < len; i++) {
    if (str[i] < 0x80)
      ascii++;
    else
      mbyte++;
  }

  return MAKE_INT(ascii + (mbyte / 2));
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc0("mana-lib-init", mana_init);
  uim_scm_init_proc1("mana-lib-eval", mana_eval);
  uim_scm_init_proc1("mana-lib-eucjp-string-length", eucjp_string_length);
}

void
uim_plugin_instance_quit(void)
{
  if (mana_pid != 0) {
    mana_ipc_send_command(&mana_pid, &mana_r, &mana_w, "(quit)\n");
    mana_pid = 0;
  }
}
