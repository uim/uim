/*

  Copyright (c) 2003-2006 uim Project http://uim.freedesktop.org/

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
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <pwd.h>
#include <sys/stat.h>
#include <unistd.h>

#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "plugin.h"
#include "uim-helper.h"

#define BUFFER_SIZE (4 * 1024)

static FILE *primer = NULL, *primew = NULL;
static int prime_pid = 0;

static char *prime_command = "prime";
static char *prime_ud_path;
static int prime_fd;
static uim_bool use_unix_domain_socket;

static int
prime_init_ud(char *path)
{
  int fd;
  struct sockaddr_un server;
    
  if (!path)
    return -1;

  bzero(&server, sizeof(server));
  server.sun_family = PF_UNIX;
  strcpy(server.sun_path, path);

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
    close(fd);
    /* fprintf(stderr, "connect failed\n"); */
    return -1;
  }
  
  if (uim_helper_check_connection_fd(fd)) {
    close(fd);
    return -1;
  }
  return fd;
}

static char *
prime_get_ud_path(void)
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
  sprintf(path, "/tmp/uim-prime-%s", login);
  if (pw) {
    free(login);
  }
  return path;
}

static char *
prime_read_msg_from_ud(int fd)
{
  char *read_buf = strdup("");
  char buf[BUFFER_SIZE];
  int rc;

  if (fd == -1)
    return NULL;

  while (uim_helper_fd_readable(fd) > 0) {
    
    rc = read(fd, buf, sizeof(buf)-1);
    buf[rc] = '\0';
    
    if (rc == 0) {
      fprintf(stderr, "disconnected\n");
      return NULL;
    }
    read_buf = (char *)realloc(read_buf, strlen(read_buf) + strlen(buf)+1);
    strcat(read_buf, buf);
  }
  return read_buf;
}

static void
prime_write_msg_to_ud(int fd, const char *message)
{
  if (strcmp(message, "") ==0) {
    return;
  }
  if (fd == -1)
    return;

  uim_helper_send_message(fd, message);
}


static uim_lisp
prime_send_command(uim_lisp str_)
{
  const char *str = uim_scm_refer_c_str(str_);
  char *result;
  uim_lisp ret;

  if (use_unix_domain_socket) {
    prime_write_msg_to_ud(prime_fd, str);
    result = prime_read_msg_from_ud(prime_fd);

    /*    if (!result) {
      prime_fd = prime_init_ud(prime_ud_path);
      }*/
  } else {
    int len = strlen(str);
    char *buf = malloc(len + 2);
    snprintf(buf, len + 2,"%s\n", str);
    result = uim_ipc_send_command(&prime_pid, &primer, &primew, prime_command, buf);
    free(buf);
  }

  if (!result) {
    return uim_scm_make_str("");
  }

 ret = uim_scm_make_str(result);
 free(result);
 return ret;

}

static uim_lisp
prime_lib_init(uim_lisp use_udp_)
{
  char *option;

  use_unix_domain_socket = uim_scm_c_bool(use_udp_);

  if (use_unix_domain_socket) {
    prime_ud_path = prime_get_ud_path();
    if (!prime_ud_path)
      return uim_scm_f();
      
    prime_fd = prime_init_ud(prime_ud_path);
    if (prime_fd == -1) {
      unlink(prime_ud_path);
      option = malloc(strlen("-u ") + strlen(prime_ud_path) + 1);
      sprintf(option, "-u %s", prime_ud_path);
      prime_pid = uim_ipc_open_command_with_option(prime_pid, &primer, &primew, prime_command, option);
      free(option);
      if (prime_pid == 0) {
	return uim_scm_f();
      } else {
	prime_fd = prime_init_ud(prime_ud_path);
	while (prime_fd == -1) {
	  prime_fd = prime_init_ud(prime_ud_path);
	}
      }
    }

    if (prime_fd == -1)
      return uim_scm_f();
    else
      return uim_scm_t();
  } else {
    if (prime_pid == 0) {
      prime_pid = uim_ipc_open_command( prime_pid, &primer, &primew, prime_command );
    }
    if (prime_pid == 0) {
      return uim_scm_f();
    }
    return uim_scm_t();
  }
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_subr_1("prime-lib-init", prime_lib_init);
  uim_scm_init_subr_1("prime-lib-send-command", prime_send_command);
}

void
uim_plugin_instance_quit(void)
{
  if (use_unix_domain_socket && prime_fd > 0) {
    prime_write_msg_to_ud(prime_fd, "close\n");
    prime_fd = -1;
  } else {
    if (primew) {
      uim_ipc_send_command(&prime_pid, &primer, &primew, prime_command, "close\n");
      fclose(primew);
      primew = NULL;
    }    
    if (primer) {
      fclose(primer);
      primer = NULL;
    }
  }
}
