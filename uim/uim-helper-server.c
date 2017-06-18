/*
  uim-helper-server.c: This file will be renamed uim-helper-messagebus.c later.

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
#include <pwd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#include "uim.h"
#include "uim-internal.h"
#include "uim-helper.h"


struct client {
  int fd;
  char *rbuf;
  char *wbuf;
};

#define MAX_CLIENT 32
#define BUFFER_SIZE 1024

#ifndef SUN_LEN
#define SUN_LEN(su)							\
  (sizeof(*(su)) - sizeof((su)->sun_path) + strlen((su)->sun_path))
#endif

static fd_set s_fdset_read;
static fd_set s_fdset_write;
static int s_max_fd;
static int nr_client_slots;
static struct client *clients;
static char read_buf[BUFFER_SIZE];

static int
init_server_fd(char *path)
{
  int fd, flag;
  struct sockaddr_un myhost;
  struct passwd *pw;
  char *logname;

  fd = socket(PF_UNIX, SOCK_STREAM, 0);
  if (fd < 0) {
    perror("failed in socket()");
    return -1;
  }
  fchmod(fd, S_IRUSR | S_IWUSR);

  memset(&myhost, 0, sizeof(myhost));
  myhost.sun_family = PF_UNIX;
  strlcpy(myhost.sun_path, path, sizeof(myhost.sun_path));

  if (bind(fd, (struct sockaddr *)&myhost, SUN_LEN(&myhost)) < 0) {
    perror("failed in bind()");
    return -1;
  }

  logname = getenv("LOGNAME");
  if (logname) {
    pw = getpwnam(logname);
    if (pw) {
      fchown(fd, pw->pw_uid, -1);
    }
  }

  if ((flag = fcntl(fd, F_GETFL)) < 0) {
    close(fd);
    return -1;
  }

  flag |= O_NONBLOCK;
  if (fcntl(fd, F_SETFL, flag) < 0) {
    close(fd);
    return -1;
  }

  if (listen(fd, 5) < 0) {
    perror("failed in listen()");
    return -1;
  }

  FD_SET(fd, &s_fdset_read);
  s_max_fd = fd;

  return fd;
}

static struct client *
get_unused_client(void)
{
  int i;

  for (i = 0; i < nr_client_slots; i++) {
    if (clients[i].fd == -1)
      return &clients[i];
  }

  nr_client_slots++;
  clients = uim_realloc(clients, sizeof(struct client) * nr_client_slots);
  clients[nr_client_slots - 1].rbuf = uim_strdup("");
  clients[nr_client_slots - 1].wbuf = uim_strdup("");

  return &clients[nr_client_slots - 1];
}

static void
close_client(struct client *cl)
{
  close(cl->fd);
  if (cl->rbuf) {
    free(cl->rbuf);
    cl->rbuf = uim_strdup("");
  }
  if (cl->wbuf) {
    free(cl->wbuf);
    cl->wbuf = uim_strdup("");
  }
  cl->fd = -1;
}

static void
distribute_message(char *msg, struct client *cl)
{
  int i;
  size_t msg_len;

  msg_len = strlen(msg);

  for (i = 0; i < nr_client_slots; i++) {
    if (clients[i].fd != -1 && clients[i].fd != cl->fd) {
      clients[i].wbuf = uim_helper_buffer_append(clients[i].wbuf, msg, msg_len);
      FD_SET(clients[i].fd, &s_fdset_write);
    }
  }
}

static int
reflect_message_fragment(struct client *cl)
{
  ssize_t rc;
  char *msg;

  /* do read */
  rc = read(cl->fd, read_buf, sizeof(read_buf));
  if (rc == -1) {
    if (errno == EAGAIN || errno == EINTR)
      return 0;
    return -1;
  } else if (rc == 0)
    return -1;

  cl->rbuf = uim_helper_buffer_append(cl->rbuf, read_buf, rc);

  while ((msg = uim_helper_buffer_get_message(cl->rbuf))) {
    distribute_message(msg, cl);
    free(msg);
  }

  return 1;
}

static uim_bool
check_session_alive(void)
{
  /* If there's no connection, we can assume user logged out. */
  int i;

  for (i = 0; i < nr_client_slots; i++) {
    if (clients[i].fd != -1)
      return UIM_TRUE;
  }

  return UIM_FALSE; /* User already logged out */
}


static uim_bool
accept_new_connection(int server_fd)
{
  struct sockaddr_un clientsoc;
  socklen_t len;
  int new_fd, flag;
  struct client *cl;

  len = sizeof(clientsoc);
  new_fd = accept(server_fd, (struct sockaddr *)&clientsoc, &len);
  
  if (new_fd < 0) {
    perror("accpet failed");
    return UIM_FALSE;
  }

  if ((flag = fcntl(new_fd, F_GETFL)) < 0) {
    close(new_fd);
    return UIM_FALSE;
  }

  flag |= O_NONBLOCK;
  if (fcntl(new_fd, F_SETFL, flag) < 0) {
    close(new_fd);
    return UIM_FALSE;
  }

  cl = get_unused_client();
  if (!cl) {
    close(new_fd);
    return UIM_FALSE;
  }
  cl->fd = new_fd;
#ifdef LOCAL_CREDS	/* for NetBSD */
  {
    char buf[1] = { '\0' };
    write(cl->fd, buf, 1);
  }
#endif
  FD_SET(cl->fd, &s_fdset_read);
  if (cl->fd > s_max_fd)
    s_max_fd = cl->fd;

  return UIM_TRUE;
}

static void
write_message(struct client *cl)
{
  int ret, message_len, out_len;
  char *out;
  
  out = cl->wbuf;
  message_len = out_len = strlen(cl->wbuf);
  while (out_len > 0) {
    if ((ret = write(cl->fd, out, out_len)) < 0) {
      if (errno == EAGAIN) {
#if 0
	fprintf(stderr, "EAGAIN: fd = %d\n", cl->fd);
#endif
      } else {
	perror("uim-helper_server write(2) failed");
	if (errno == EPIPE) {
	  fprintf(stderr, "fd = %d\n", cl->fd);
	  FD_CLR(cl->fd, &s_fdset_read);
	  FD_CLR(cl->fd, &s_fdset_write);
	  if (cl->fd == s_max_fd)
	    s_max_fd--;
	  close_client(cl);
	}
      }
      break;
    } else {
      out += ret;
      out_len -= ret;
    }
  }
  if (out_len == 0) {
    free(cl->wbuf);
    cl->wbuf = uim_strdup("");
    FD_CLR(cl->fd, &s_fdset_write);    
  } else {
    uim_helper_buffer_shift(cl->wbuf, message_len - out_len);
  }
}


static void
read_message(struct client *cl)
{
  int result;

  result = reflect_message_fragment(cl);
  
  if (result < 0) {
    FD_CLR(cl->fd, &s_fdset_read);
    FD_CLR(cl->fd, &s_fdset_write);
    if (cl->fd == s_max_fd)
      s_max_fd--;
    close_client(cl);
  }
}

static void
uim_helper_server_process_connection(int server_fd)
{
  int i;
  fd_set readfds, writefds;

  while (1) {
    /* Copy readfds from s_fdset_read/s_fdset_write because select removes
       readble/writable fd from readfds/writefds */
    memcpy(&readfds, &s_fdset_read, sizeof(fd_set));
    memcpy(&writefds, &s_fdset_write, sizeof(fd_set));

    /* call select(), waiting until a file descriptor became readable */
    if (select(s_max_fd + 1, &readfds, &writefds, NULL, NULL) <= 0) {
      perror("uim-helper_server select(2) failed");
      sleep(3);
      continue;
    }

    /* for accept new connection */
    if (FD_ISSET(server_fd, &readfds)) {
      uim_bool accepted;
      accepted = accept_new_connection(server_fd);
      if (accepted == UIM_FALSE) {
	/* acception failed, go next loop without message processing. */
	continue;
      }
    } else {
      /* check data to write and from clients reached */
      for (i = 0; i < nr_client_slots; i++) {
	if (clients[i].fd != -1 && FD_ISSET(clients[i].fd, &writefds))
	  write_message(&clients[i]);

	if (clients[i].fd != -1 && FD_ISSET(clients[i].fd, &readfds))
	  read_message(&clients[i]);
      }
    }

    if (!check_session_alive())
      return;
  }
}


int
main(int argc, char **argv)
{
  char path[MAXPATHLEN];
  int server_fd;

  uim_init_error();

  if (!uim_helper_get_pathname(path, sizeof(path)))
    return 0;

  unlink(path);

  clients = NULL;
  nr_client_slots = 0;

  FD_ZERO(&s_fdset_read);
  FD_ZERO(&s_fdset_write);
  s_max_fd = 0;
  server_fd = init_server_fd(path);

  printf("waiting\n\n");
  fflush(stdout);

  fclose(stdin);
  fclose(stdout);

  if (server_fd < 0)
    return 0;

  /*  fprintf(stderr,"Waiting for connection at %s\n", path);*/

  signal(SIGPIPE, SIG_IGN);
  uim_helper_server_process_connection(server_fd);

  return 0;
}
