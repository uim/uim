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

#if HAVE_CONFIG_H
#include "config.h"
#endif
#ifndef DEBUG
#define NDEBUG
#endif
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "uim-fep.h"
#include "read.h"

char *unget_buf = NULL;
int buf_size = 0;

/*
 * select
 * ungetがあるときはselectを呼ばない. 
 */
int my_select(int n, fd_set *readfds, struct timeval *timeout)
{
  if (buf_size > 0) {
    FD_ZERO(readfds);
    FD_SET(STDIN_FILENO, readfds);
    return 1;
  }
  return select(n, readfds, NULL, NULL, timeout);
}

/*
 * stdinをreadする
 * ungetがあるときはそれを返す
 */
ssize_t read_stdin(void *buf, int count)
{
  if (buf_size > 0) {
    if (buf_size > count) {
      memcpy(buf, unget_buf, count);
      buf_size -= count;
      memmove(unget_buf, unget_buf + count, buf_size);
      return count;
    } else {
      int rval = buf_size;
      memcpy(buf, unget_buf, buf_size);
      buf_size = 0;
      return rval;
    }
  }
  return read(STDIN_FILENO, buf, count);
}

/*
 * 読み取りすぎた文字列を戻す
 */
void unget_stdin(const char *str, int count)
{
  if (count <= 0) {
    return;
  }
  debug(("unget count = %d buf_size = %d\n", count, buf_size));
  unget_buf = realloc(unget_buf, buf_size + count);
  memcpy(unget_buf + buf_size, str, count);
  buf_size += count;
}
