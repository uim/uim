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
#if (!defined(DEBUG) && !defined(NDEBUG))
#define NDEBUG
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ERRNO_H
#include "errno.h"
#endif

#include "uim-fep.h"
#include "read.h"

static char *s_unget_buf = NULL;
static int s_buf_size = 0;


static int pselect_(int n, fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
            const struct timespec *timeout, const sigset_t *sigmask);

/*
 * select
 * ungetがあるときはselectを呼ばない. 
 */
int my_select(int n, fd_set *readfds, struct timeval *timeout)
{
  if (s_buf_size > 0) {
    FD_ZERO(readfds);
    FD_SET(g_win_in, readfds);
    return 1;
  }
  return select(n, readfds, NULL, NULL, timeout);
}

/*
 * pselect
 * ungetがあるときはpselectを呼ばない. 
 */
int my_pselect(int n, fd_set *readfds, const sigset_t *sigmask)
{
  if (s_buf_size > 0) {
    FD_ZERO(readfds);
    FD_SET(g_win_in, readfds);
    return 1;
  }
  return pselect_(n, readfds, NULL, NULL, NULL, sigmask);
}

/*
 * stdinをreadする
 * ungetがあるときはそれを返す
 */
ssize_t read_stdin(void *buf, int count)
{
  if (s_buf_size > 0) {
    if (s_buf_size > count) {
      memcpy(buf, s_unget_buf, count);
      s_buf_size -= count;
      memmove(s_unget_buf, s_unget_buf + count, s_buf_size);
      return count;
    } else {
      int rval = s_buf_size;
      memcpy(buf, s_unget_buf, s_buf_size);
      s_buf_size = 0;
      return rval;
    }
  }
  return read(g_win_in, buf, count);
}

/*
 * 読み取りすぎた文字列を戻す
 */
void unget_stdin(const char *str, int count)
{
  if (count <= 0) {
    return;
  }
  debug(("unget count = %d s_buf_size = %d\n", count, s_buf_size));
  s_unget_buf = uim_realloc(s_unget_buf, s_buf_size + count);
  memcpy(s_unget_buf + s_buf_size, str, count);
  s_buf_size += count;
}

static int pselect_(int n, fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
            const struct timespec *timeout, const sigset_t *sigmask)
{
  int ret;
  sigset_t orig_sigmask;
  sigset_t pending_signals;

  /* シグナルが保留されているか */
  sigpending(&pending_signals);
  if (
      sigismember(&pending_signals, SIGHUP)   ||
      sigismember(&pending_signals, SIGTERM)  ||
      sigismember(&pending_signals, SIGQUIT)  ||
      sigismember(&pending_signals, SIGINT)   ||
      sigismember(&pending_signals, SIGWINCH) ||
      sigismember(&pending_signals, SIGUSR1)  ||
      sigismember(&pending_signals, SIGUSR2)  ||
      sigismember(&pending_signals, SIGTSTP)  ||
      sigismember(&pending_signals, SIGCONT)
     ) {
    sigprocmask(SIG_SETMASK, sigmask, &orig_sigmask);
    sigprocmask(SIG_SETMASK, &orig_sigmask, NULL);
    errno = EINTR;
    return -1;
  }

  /* timeout は使わない */
  sigprocmask(SIG_SETMASK, sigmask, &orig_sigmask);
  ret = select(n, readfds, writefds, exceptfds, NULL);
  sigprocmask(SIG_SETMASK, &orig_sigmask, NULL);
  return ret;
}
