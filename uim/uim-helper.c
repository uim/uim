/*

  Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/

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

#include <unistd.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pwd.h>
#include <signal.h>
#include <errno.h>
#include "context.h"
#include "uim-helper.h"

enum RorW
  {
    READ,
    WRITE
  };

static int
uim_helper_fd(int fd, enum RorW rw)
{
    int rc;
    fd_set fds;
    struct timeval tv;

    /*   if(!fd || fd < 0)
	 return -1;*/

    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    tv.tv_sec = tv.tv_usec = 0;

    if (rw == READ)
      rc = select(fd + 1 , &fds, NULL, NULL, &tv);
    else    
      rc = select(fd + 1, NULL, &fds, NULL, &tv);

    if (rc < 0)
      return -1;
    
    return FD_ISSET(fd, &fds) ? 1 : 0;
}

void
uim_helper_send_message(int fd, const char *message)
{
  int res;
  int len, out_len;
  sig_t old_sigpipe;
  char *buf, *bufp;

  if (fd < 0)
    return;

  if (!message)
    return;

  /* readable and cannot read any character, means disconnected.
     so we should read here and proc such condition. */

  /*
    The assumption described above is not correct. uim_helper_fd()
    does only select(2), which only indicates whether system is busy
    or not. i.e. select(2) exists for non-blocking IO. Not indicates
    connection availability.

    What we have to do is:

    - Don't use uim_helper_fd() for testing connection availability

    - Determine connection error by EPIPE error of write(2) or some
      appropriate methods

    - Write all data even if uim_helper_fd() has returned 0
      (i.e. retry until written all data). (uim_helper_fd() == 0) only
      indicates system is busy.

    I think that we should remove uim_helper_fd() for
    writing. Blocking write is sufficient for uim.
    
    -- YamaKen 2005-02-07
  */

  len = strlen(message);
  buf = malloc(len + 2);
  snprintf(buf, len + 2,"%s\n", message);

  old_sigpipe = signal(SIGPIPE, SIG_IGN);

  out_len = len + 1;
  bufp = buf;
  while (out_len > 0) {
    if ((res = write(fd, bufp, out_len)) < 0) {
      if (errno == EAGAIN || errno == EINTR) {
	fd_set fds;
	struct timeval tv;
	int rc;

	FD_ZERO(&fds);
	FD_SET(fd, &fds);
	tv.tv_sec = 0;
	tv.tv_usec = 100000;
	rc = select(fd + 1, NULL, &fds, NULL, &tv);
	if (rc > 0 && FD_ISSET(fd, &fds)) {
	  continue;
	}
	fprintf(stderr, "uim_helper_send_message: write failed\n");
      }
      break;
    }

    bufp += res;
    out_len -= res;
  }
  free(buf);
  signal(SIGPIPE, old_sigpipe);
  return;
}

char *
uim_helper_get_pathname(void)
{
  char *path;
  char *login;
  struct passwd *pw = NULL;
 
  login = getenv("LOGNAME");
  
  if (!login) {
    pw = getpwuid(getuid());
    login = strdup(pw->pw_name);
  }

  path = (char *)malloc(strlen(login)+ 20);
  sprintf(path, "/tmp/uimhelper-%s",login);
  if (pw) {
    free(login);
  }
  return path;
}

int uim_helper_fd_readable(int fd)
{
  return uim_helper_fd(fd, READ);
}

int uim_helper_fd_writable(int fd)
{
  return uim_helper_fd(fd, WRITE);
}


int uim_helper_str_terminated(const char *str)
{
  if(!str)
    return 0;

  if(strlen(str) > 2&&
     str[strlen(str)-1] == '\n' &&
     str[strlen(str)-2] == '\n' )
    return 1;

  return 0;
}
