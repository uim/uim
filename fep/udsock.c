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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
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
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include "udsock.h"

static int s_send_sockfd = -1;
static int s_recv_sockfd = -1;
static struct sockaddr_un s_servaddr;

static uim_bool
check_dir(const char *dir)
{
  struct stat st;

  if (dir == NULL)
    return UIM_FALSE;

  if (stat(dir, &st) < 0)
    return (mkdir(dir, 0700) < 0) ? UIM_FALSE : UIM_TRUE;
  else {
    mode_t mode = S_IFDIR | S_IRUSR | S_IWUSR | S_IXUSR;
    return ((st.st_mode & mode) == mode) ? UIM_TRUE : UIM_FALSE;
  }
}

char *
get_ud_path(void)
{
  char *path, *home = NULL;
  struct passwd *pw;

  pw = getpwuid(getuid());
  if (pw)
    home = pw->pw_dir;

  if (!home && !uim_helper_is_setugid())
    home = getenv("HOME");

  if (!home)
    return NULL;

  if (asprintf(&path, "%s/.uim.d", home) == -1)
    return NULL; /* XXX: fatal */

  if (!check_dir(path)) {
    free(path);
    return NULL;
  }
  free(path);

  if (asprintf(&path, "%s/.uim.d/fep", home) == -1)
    return NULL; /* XXX: fatal */

  if (!check_dir(path)) {
    free(path);
    return NULL;
  }

  return path;
}


const char *usersockname(const char *file)
{
  static char buf[UNIX_PATH_MAX];
  char filebuf[UNIX_PATH_MAX];
  char *sock_dir;

  if (file != NULL && file[0] == '/') {
    return file;
  }

  if (file == NULL) {
    strlcpy(filebuf, "backtick", UNIX_PATH_MAX);
  } else {
    strlcpy(filebuf, file, UNIX_PATH_MAX);
  }

  sock_dir = get_ud_path();
  if (!sock_dir) {
    sendline("uim-fep cannot create directory");
    exit(EXIT_FAILURE);
  }
  snprintf(buf, UNIX_PATH_MAX, "%s/%s", sock_dir, filebuf);
  free(sock_dir);

  return buf;
}

void init_sendsocket(const char *sock_path)
{
  const char *path;

  path = usersockname(sock_path);
  s_send_sockfd = socket(PF_UNIX, SOCK_DGRAM, 0);
  memset(&s_servaddr, 0, sizeof(s_servaddr));
  s_servaddr.sun_family = AF_UNIX;
  strlcpy(s_servaddr.sun_path, path, UNIX_PATH_MAX);
}

void sendline(const char *buf)
{
  sendto(s_send_sockfd, buf, strlen(buf), 0, (struct sockaddr *)&s_servaddr, sizeof(s_servaddr));
}

/*
 * socketのファイルディスクリプタを返す。
 * エラーの場合は-1を返し、errnoを設定する。
 */
void init_recvsocket(const char *sock_path)
{
  const char *path;

  path = usersockname(sock_path);
  unlink(path);
  s_recv_sockfd = socket(PF_UNIX, SOCK_DGRAM, 0);
  memset(&s_servaddr, 0, sizeof(s_servaddr));
  s_servaddr.sun_family = AF_UNIX;
  strlcpy(s_servaddr.sun_path, path, UNIX_PATH_MAX);
  if (bind(s_recv_sockfd, (struct sockaddr *)&s_servaddr, sizeof(s_servaddr)) < 0) {
    perror(path);
    exit(EXIT_FAILURE);
  }
  chmod(path, S_IRUSR|S_IWUSR);
}

void close_socket(void)
{
  if (s_send_sockfd != -1) {
    close(s_send_sockfd);
  }
  if (s_recv_sockfd != -1) {
    close(s_recv_sockfd);
  }
}

int recvline(char *buf, int n)
{
  return recvfrom(s_recv_sockfd, buf, n, 0, NULL, NULL);
}
