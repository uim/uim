/*

  Copyright (c) 2003-2008 uim Project http://code.google.com/p/uim/

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
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <pwd.h>
#include <sys/stat.h>
#include <unistd.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "dynlib.h"
#include "uim-helper.h"
#include "uim-util.h"
#include "uim-posix.h"

#define BUFFER_SIZE (4 * 1024)

static FILE *primer, *primew;
static pid_t prime_pid = 0;

static char *prime_command = "prime";

static char prime_ud_path[MAXPATHLEN];
static int prime_fd = -1;
static uim_bool use_unix_domain_socket;

static int
prime_init_ud(char *path)
{
  int fd;
  struct sockaddr_un server;
    
  if (path[0] == '\0')
    return -1;

  memset(&server, 0, sizeof(server));
  server.sun_family = PF_UNIX;
  strlcpy(server.sun_path, path, sizeof(server.sun_path));

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

  if (connect(fd, (struct sockaddr *)&server, sizeof(server)) == -1) {
    close(fd);
    /* fprintf(stderr, "connect failed\n"); */
    return -1;
  }
  
  if (uim_helper_check_connection_fd(fd) < 0) {
    close(fd);
    return -1;
  }

  return fd;
}

static uim_bool
prime_get_ud_path(char *prime_path, int len)
{
  if (len <= 0)
    return UIM_FALSE;

  if (!uim_get_config_path(prime_path, len, !uim_helper_is_setugid()))
    goto path_error;

  if (strlcat(prime_path, "/socket", len) >= (size_t)len)
    goto path_error;

  if (!uim_check_dir(prime_path))
    goto path_error;

  if (strlcat(prime_path, "/uim-prime", len) >= (size_t)len)
    goto path_error;

  return UIM_TRUE;

 path_error:
  prime_path[0] = '\0';
  return UIM_FALSE;
}

static void
clear_prime_fd()
{
  close(prime_fd);
  prime_fd = -1;
  prime_ud_path[0] = '\0';
}

static char *
prime_read_msg_from_ud(int fd)
{
  char *read_buf;
  char buf[BUFFER_SIZE];
  int rc, len = 0;

  if (fd == -1)
    return NULL;

  read_buf  = uim_strdup("");

  for (;;) {
    if ((rc = read(fd, buf, sizeof(buf) - 1)) == -1 || rc == 0) {
      perror("disconnected");
      if (errno == EAGAIN || errno == EINTR)
	continue;

      free(read_buf);
      clear_prime_fd();
      return NULL;
    }
    buf[rc] = '\0';
    read_buf = uim_realloc(read_buf, strlen(read_buf) + strlen(buf) + 1);
    strcat(read_buf, buf);
    len += rc;

    if (len >= 2 && read_buf[len - 1] == '\n' && read_buf[len - 2] == '\n') {
      /* drop last "\n" */
      read_buf[len - 1] = '\0';
      break;
    }
  }

  return read_buf;
}

static void
prime_write_msg_to_ud(int fd, const char *message)
{
  if (!strcmp(message, ""))
    return;

  if (fd == -1)
    return;

  uim_helper_send_message(fd, message);
}

static uim_lisp
prime_send_command(uim_lisp str_)
{
  const char *str;
  char *result;
  uim_lisp ret;

  str = REFER_C_STR(str_);

  if (use_unix_domain_socket) {
    prime_write_msg_to_ud(prime_fd, str);
    result = prime_read_msg_from_ud(prime_fd);
    if (!result)
      return MAKE_STR("error\n\t\n");
  } else {
    char *buf;
    if (uim_asprintf(&buf, "%s\n", str) < 0 || buf == NULL)
      return MAKE_STR("");
    result = uim_ipc_send_command(&prime_pid, &primer, &primew, prime_command,
				  buf);
    free(buf);
    if (!result)
      return MAKE_STR("");
  }

  ret = MAKE_STR(result);
  free(result);

  return ret;
}

static uim_lisp
prime_lib_init(uim_lisp use_udp_)
{
  char *option;
  int timeout_count = 0;

  use_unix_domain_socket = C_BOOL(use_udp_);

  if (use_unix_domain_socket) {
    if (prime_fd != -1)
      return uim_scm_t();

    if (!prime_get_ud_path(prime_ud_path, sizeof(prime_ud_path)))
      return uim_scm_f();

    prime_fd = prime_init_ud(prime_ud_path);
    if (prime_fd == -1) {
      unlink(prime_ud_path);
      if (uim_asprintf(&option, "-u %s", prime_ud_path) < 0 || option == NULL)
	return uim_scm_f();
      prime_pid = uim_ipc_open_command_with_option(prime_pid, &primer, &primew,
						   prime_command, option);
      free(option);
      if (prime_pid == 0)
	return uim_scm_f();
      else {
	prime_fd = prime_init_ud(prime_ud_path);
	while (prime_fd == -1 && timeout_count < 100) {
	  usleep(100000);
	  prime_fd = prime_init_ud(prime_ud_path);
	  timeout_count++;
	}
      }
    }

    return (prime_fd == -1) ? uim_scm_f() : uim_scm_t();
  } else {
    if (prime_pid == 0)
      prime_pid = uim_ipc_open_command(prime_pid, &primer, &primew,
				       prime_command);

    return prime_pid ? uim_scm_t() : uim_scm_f();
  }
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc1("prime-lib-init", prime_lib_init);
  uim_scm_init_proc1("prime-lib-send-command", prime_send_command);
}

void
uim_plugin_instance_quit(void)
{
  if (use_unix_domain_socket && prime_fd != -1) {
    prime_write_msg_to_ud(prime_fd, "close\n");
    clear_prime_fd();
    use_unix_domain_socket = UIM_FALSE;
  } else {
    if (primew) {
      uim_ipc_send_command(&prime_pid, &primer, &primew, prime_command,
			   "close\n");
      fclose(primew);
      primew = NULL;
    }    
    if (primer) {
      fclose(primer);
      primer = NULL;
    }
  }
}
