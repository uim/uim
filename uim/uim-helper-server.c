/*
  uim-helper-server.c: This file will be renamed uim-helper-messagebus.c later.

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

#include <sys/types.h>
#include <pwd.h>
#include <sys/socket.h>
#include <sys/un.h>
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
#include "uim.h"
#include "uim-helper.h"

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
static struct client {
  int fd;
  char *rbuf;
  char *write_queue;
} *clients;

/*
  prepare file descriptor.
*/
static int
init_serv_fd(char *path)
{
  int foo;
  int fd;
  int flag;
  struct sockaddr_un myhost;
  struct passwd *pw;
  char *logname;

  fd = socket(PF_UNIX, SOCK_STREAM, 0);
  if (fd < 0) {
    perror("failed in socket()");
    return -1;
  }

  bzero(&myhost, sizeof(myhost));
  myhost.sun_family = PF_UNIX;
  strcpy(myhost.sun_path, path);

  foo = bind(fd, (struct sockaddr *)&myhost, SUN_LEN(&myhost));
  if (foo < -1) {
    perror("failed in bind()");
    return -1;
  }

  chmod(path, S_IRUSR|S_IWUSR);

  logname = getenv("LOGNAME");
  if (logname) {
    pw = getpwnam(logname);
    if (pw)
      chown(path, pw->pw_uid, -1);
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

  foo = listen(fd, 5);
  if (foo == -1) {
    perror("failed in listen()");
    return -1;
  }
  /*  fprintf(stderr,"listen at %s\n",path);*/
  FD_SET(fd, &s_fdset_read);
  s_max_fd = fd;
  return fd;
}

static struct client *
get_unused_client()
{
  int i;
  for (i = 0; i < nr_client_slots; i++) {
    if (clients[i].fd == -1) {
      return &clients[i];
    }
  }
  nr_client_slots++;
  clients = realloc(clients, sizeof(struct client) * nr_client_slots);
  clients[nr_client_slots - 1].rbuf = strdup("");
  clients[nr_client_slots - 1].write_queue = strdup("");
  return &clients[nr_client_slots - 1];
}

static void
free_client(struct client *cl)
{
  if (cl->rbuf) {
    free(cl->rbuf);
    cl->rbuf = strdup("");
  }
  if (cl->write_queue) {
    free(cl->write_queue);
    cl->write_queue = strdup("");
  }
  cl->fd = -1;
}


static void
parse_content(char *content, struct client *cl)
{
  int i, content_len;

  content_len = strlen(content);

  for (i = 0; i < nr_client_slots; i++) {
    if (clients[i].fd == -1 || clients[i].fd == cl->fd) {
      continue;
    } else {
      clients[i].write_queue = (char *)realloc(clients[i].write_queue,
		      strlen(clients[i].write_queue) + content_len + 1);
      strcat(clients[i].write_queue, content);
      FD_SET(clients[i].fd, &s_fdset_write);
    }
  }
}

static void
shift_buffer(char *buf, int count)
{
  int len = strlen(buf);
  memmove(buf, &buf[count], len - count);
  buf[len - count] = '\0';
}

static char *
uim_helper_server_get_message(char *buf)
{
  int i;
  int len = strlen(buf);
  char *ret;

  for (i = 0; i < len - 1; i++) {
    if (buf[i] == '\n' && buf[i + 1] == '\n') {
      ret = (char *)malloc(i + 3);
      memcpy(ret, buf, i + 2);
      ret[i + 2] = '\0';
      shift_buffer(buf, i + 2);
      return ret;
    }
  }
  return NULL;
}

static int
proc_func(struct client *cl)
{
  int rc;
  char buf[BUFFER_SIZE];
  char *message;

  /* do read */
  rc = read(cl->fd, buf, BUFFER_SIZE - 1);
  if (rc <= 0) {
    if (rc < 0 && (errno == EAGAIN || errno == EINTR))
      return 0;
    return -1;
  }

  buf[rc] = '\0';

  cl->rbuf = (char *)realloc(cl->rbuf, strlen(cl->rbuf) + strlen(buf) + 1);
  strcat(cl->rbuf, buf);

  while ((message = uim_helper_server_get_message(cl->rbuf))) {
    /* process */
    parse_content(message, cl);
    free(message);
  }
  return 1;
}

static void
uim_helper_server_process_connection(int serv_fd)
{
  int i;
  fd_set readfds;
  fd_set writefds;

  while (1) {
    memcpy(&readfds, &s_fdset_read, sizeof(fd_set));
    memcpy(&writefds, &s_fdset_write, sizeof(fd_set));

    /* call select(), waiting until a file descriptor readable */
    if (select(s_max_fd + 1, &readfds, &writefds, NULL, NULL) <= 0) {
      perror("uim-helper_server select(2) failed");
      sleep(3);
      continue;
    }

    /* for accept new connection */
    if (FD_ISSET(serv_fd, &readfds)) {
      struct sockaddr_un clientsoc;
      socklen_t len = sizeof(clientsoc);
      int new_fd;
      int flag;
      struct client *cl;
      new_fd = accept(serv_fd, (struct sockaddr *)&clientsoc, &len);

      if (new_fd < 0) {
	perror("accpet failed");
	continue;
      }

      if ((flag = fcntl(new_fd, F_GETFL)) == -1) {
	close(new_fd);
	continue;
      }

      flag |= O_NONBLOCK;
      if (fcntl(new_fd, F_SETFL, flag) == -1) {
	close(new_fd);
	continue;
      }

      cl = get_unused_client();
      if (!cl) {
	close(new_fd);
	continue;
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
    } else {
      /* check data to write and from clients reached */
      for (i = 0; i < nr_client_slots; i++) {
	if (clients[i].fd != -1 && FD_ISSET(clients[i].fd, &writefds)) {
	  int ret, message_len, out_len;
	  char *out;

	  out = clients[i].write_queue;
	  message_len = out_len = strlen(clients[i].write_queue);
	  while (out_len > 0) {
	    if ((ret = write(clients[i].fd, out, out_len)) < 0) {
	      if (errno == EAGAIN) {
#if 0
		fprintf(stderr, "EAGAIN: fd = %d\n", clients[i].fd);
#endif
	      } else {
		perror("uim-helper_server write(2) failed");
		if (errno == EPIPE) {
		  fprintf(stderr, "fd = %d\n", clients[i].fd);
		  FD_CLR(clients[i].fd, &s_fdset_read);
		  FD_CLR(clients[i].fd, &s_fdset_write);
		  if (clients[i].fd == s_max_fd)
		    s_max_fd--;
		  close(clients[i].fd);
		  free_client(&clients[i]);
		}
	      }
	      break;
	    } else {
	      out += ret;
	      out_len -= ret;
	    }
	  }
	  if (out_len == 0) {
	    free(clients[i].write_queue);
	    clients[i].write_queue = strdup("");
	    FD_CLR(clients[i].fd, &s_fdset_write);
	  } else {
	    shift_buffer(clients[i].write_queue, message_len - out_len);
	  }
	}
	if (clients[i].fd != -1 && FD_ISSET(clients[i].fd, &readfds)) {
	  int result;
	  /* actual process */
	  result = proc_func(&clients[i]);

	  if (result < 0) {
	    FD_CLR(clients[i].fd, &s_fdset_read);
	    FD_CLR(clients[i].fd, &s_fdset_write);
	    if (clients[i].fd == s_max_fd)
	      s_max_fd--;
	    close(clients[i].fd);
	    free_client(&clients[i]);
	  }
	}
      }
    }
  }
}

int
main(int argc, char **argv)
{
  char *path = uim_helper_get_pathname();
  int serv_fd;
  unlink(path);

  clients = NULL;
  nr_client_slots = 0;

  FD_ZERO(&s_fdset_read);
  FD_ZERO(&s_fdset_write);
  s_max_fd = 0;
  serv_fd = init_serv_fd(path);

  printf("waiting\n\n");
  fflush(stdout);

  fclose(stdin);
  fclose(stdout);

  if (serv_fd < 0) {
    return 0;
  }
  /*  fprintf(stderr,"Waiting for connection at %s\n", path);*/

  free(path);

  signal(SIGPIPE, SIG_IGN);

  uim_helper_server_process_connection(serv_fd);

  return 0;
}


