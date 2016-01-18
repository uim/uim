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
#include <config.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#include "uim.h"
#include "uim-helper.h"
#include "uim-internal.h"
#include "uim-util.h"


#define RECV_BUFFER_SIZE 1024

/*Common buffer for some functions's temporary buffer.
  Pay attention for use.*/
static char uim_recv_buf[RECV_BUFFER_SIZE];
static char *uim_read_buf;

static int uim_fd = -1;
static void (*uim_disconnect_cb)(void);


static char *
get_server_command(void)
{
  return UIM_LIBEXECDIR "/uim-helper-server";
}

int uim_helper_init_client_fd(void (*disconnect_cb)(void))
{
  struct sockaddr_un server;
  char path[MAXPATHLEN];
  FILE *serv_r = NULL, *serv_w = NULL;
  int fd = -1;
  
  uim_fd = -1;

  if (!uim_helper_get_pathname(path, sizeof(path)))
    goto error;

  memset(&server, 0, sizeof(server));
  server.sun_family = PF_UNIX;
  strlcpy(server.sun_path, path, sizeof(server.sun_path));

#ifdef SOCK_CLOEXEC
  /* linux-2.6.27+ variant that prevents racing on concurrent fork & exec in other thread */
  fd = socket(PF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
  if (fd == -1 && errno == EINVAL)
    /* fallback to plain SOCK_TYPE on older kernel */
#endif
  fd = socket(PF_UNIX, SOCK_STREAM, 0);
  if (fd < 0) {
    perror("fail to create socket");
    goto error;
  }
  fcntl(fd, F_SETFD, fcntl(fd, F_GETFD, 0) | FD_CLOEXEC);
  
#ifdef LOCAL_CREDS /* for NetBSD */
  /* Set the socket to receive credentials on the next message */
  {
    int on = 1;
    setsockopt(fd, 0, LOCAL_CREDS, &on, sizeof(on));
  }
#endif


  if (connect(fd, (struct sockaddr *)&server,sizeof(server)) < 0) {
    pid_t serv_pid = 0;
    char buf[128];
    
    serv_pid = uim_ipc_open_command(serv_pid, &serv_r, &serv_w,
				    get_server_command());

    if (serv_pid == 0)
      goto error;
    
    while (fgets (buf, sizeof(buf), serv_r ) != NULL ) {
      if (strcmp( buf, "\n" ) == 0)
	break;
    }
    
    if (connect(fd, (struct sockaddr *)&server,sizeof(server)) < 0)
      goto error;
  }

  if (uim_helper_check_connection_fd(fd))
    goto error;

  if (!uim_read_buf)
    uim_read_buf = uim_strdup("");
  uim_disconnect_cb = disconnect_cb;
  uim_fd = fd;

  return fd;

error:
  if (fd != -1)
    close(fd);

  if (serv_r)
    fclose(serv_r);
 
  if (serv_w)
    fclose(serv_w);

  return -1;
}

void
uim_helper_close_client_fd(int fd)
{
  if (fd != -1)
    close(fd);

  if (uim_disconnect_cb)
    uim_disconnect_cb();

  uim_fd = -1;
}

void
uim_helper_client_focus_in(uim_context uc)
{
  if (uc)
    uim_helper_send_message(uim_fd, "focus_in\n");
}

void
uim_helper_client_focus_out(uim_context uc)
{
  if (uc)
    uim_helper_send_message(uim_fd, "focus_out\n");
}

void
uim_helper_client_get_prop_list(void)
{
  uim_helper_send_message(uim_fd, "prop_list_get\n");
}

void
uim_helper_read_proc(int fd)
{
  int rc;

  while (uim_helper_fd_readable(fd) > 0) {
    rc = read(fd, uim_recv_buf, sizeof(uim_recv_buf));
    if (rc == 0 || (rc == -1 && errno != EAGAIN)) {
      uim_helper_close_client_fd(fd);
      return;
    } else if (rc > 0) {
      uim_read_buf = uim_helper_buffer_append(uim_read_buf, uim_recv_buf, rc);
    }
  }
}

char *
uim_helper_get_message(void)
{
  return uim_helper_buffer_get_message(uim_read_buf);
}
