
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

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include "uim.h"
#include "uim-helper.h"
#include "context.h"

static int uim_fd = -1;
static void (*uim_disconnect_cb)(void);

static void uim_helper_client_focus(uim_context uc, int flg);

#define BUFFER_SIZE (32 * 1024)
/*Common buffer for some functions's temporary buffer.
  Pay attention for use.*/
static char uim_help_buf[BUFFER_SIZE];
static int uim_read_buf_size;
static char *uim_read_buf;

static char *
get_server_command(void)
{
  return "uim-helper-server";
}


int uim_helper_init_client_fd(void (*disconnect_cb)(void))
{
  int fd;
  struct sockaddr_un server;
  char *path = uim_helper_get_pathname();
  int flag;
  
  uim_fd = -1;
  
  if (!path)
    return -1;

  bzero(&server, sizeof(server));
  server.sun_family = PF_UNIX;
  strcpy(server.sun_path, path);

  free(path);
  
  fd = socket(PF_UNIX, SOCK_STREAM, 0);
  if (fd < 0) {
    perror("fail to create socket");
    return -1;
  }
  
#ifdef LOCAL_CREDS /* for NetBSD */
  /* Set the socket to receive credentials on the next message */
  {
    int on = 1;
    setsockopt(fd, 0, LOCAL_CREDS, &on, sizeof(on));
  }
#endif


  if (connect(fd, (struct sockaddr *)&server,sizeof(server)) == -1) {
    int serv_pid = 0;
    FILE *serv_r = NULL, *serv_w = NULL;
    char buf[128];
    
    serv_pid = uim_ipc_open_command(serv_pid, &serv_r, &serv_w, get_server_command());
    
    if (serv_pid == 0) {
      return -1;
    }
    
    while (fgets (buf, sizeof(buf), serv_r ) != NULL ) {
      if (strcmp( buf, "\n" ) == 0)
	break;
    }
    
    if (connect(fd, (struct sockaddr *)&server,sizeof(server)) == -1) {
      return -1;
    }
  }

  if (uim_helper_check_connection_fd(fd)) {
    close(fd);
    return -1;
  }

  if ((flag = fcntl(fd, F_GETFL)) == -1) {
    close(fd);
    return -1;
  }

  flag |= O_NONBLOCK;
  if (fcntl(fd, F_SETFL, flag) == -1) {
    close(fd);
    return -1;
  }

  uim_read_buf = strdup("");
  uim_disconnect_cb = disconnect_cb;
  uim_fd = fd;
  return fd;
}

void
uim_helper_close_client_fd(int fd)
{
  if (fd != -1) {
    close(fd);
  }
  if (uim_disconnect_cb) {
    uim_disconnect_cb();
  }
  uim_fd = -1;
}


void
uim_helper_client_focus_in(uim_context uc)
{
  uim_helper_client_focus(uc, 0);
}

void
uim_helper_client_focus_out(uim_context uc)
{
  uim_helper_client_focus(uc, 1);
}

static void
uim_helper_client_focus(uim_context uc, int flg)
{
  if (uim_fd < 0)
    return;

  if (!uc)
    return;

  if (flg == 0)
    snprintf(uim_help_buf, BUFFER_SIZE, "focus_in\n");
  else
    snprintf(uim_help_buf, BUFFER_SIZE, "focus_out\n");

  uim_helper_send_message(uim_fd, uim_help_buf);
}



void
uim_helper_client_get_prop_list(void)
{
  snprintf(uim_help_buf, BUFFER_SIZE, "prop_list_get\n");
  uim_helper_send_message(uim_fd, uim_help_buf);
}

void
uim_helper_read_proc(int fd)
{
  char buf[BUFFER_SIZE];
  int rc;

  while (uim_helper_fd_readable(fd) > 0) {
    
    rc = read(fd, buf, sizeof(buf) - 1);
    buf[rc] = '\0';
    
    if (rc == 0) {
      if (uim_disconnect_cb) {
	uim_disconnect_cb();
      }
      uim_fd = -1;
      return;
    }
    uim_read_buf = (char *)realloc(uim_read_buf, strlen(uim_read_buf) + strlen(buf)+1);
    strcat(uim_read_buf, buf);
  }
  uim_read_buf_size = strlen(uim_read_buf);
  return;
}

static void
shift_read_buffer(int count)
{
  memmove(uim_read_buf, &uim_read_buf[count],
	  uim_read_buf_size - count);
  uim_read_buf_size -= count;
  uim_read_buf[uim_read_buf_size] = '\0';
}

char *
uim_helper_get_message(void)
{
  int i;
  char *buf;

  for (i = 0; i < uim_read_buf_size - 1; i++) {
    if (uim_read_buf[i] == '\n' &&
	uim_read_buf[i+1] == '\n') {
      buf = (char *)malloc(i+2);
      memcpy(buf, uim_read_buf, i+1);
      buf[i+1] = '\0';
      shift_read_buffer(i+2);
      return buf;
    }
  }
  return NULL;
}
