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
#include "uim.h"
#include "uim-helper.h"

#define MAX_CLIENT 32
#define BUFFER_SIZE 1024

#ifndef SUN_LEN
#define SUN_LEN(su)							\
  (sizeof(*(su)) - sizeof((su)->sun_path) + strlen((su)->sun_path))
#endif

static int nr_client_slots;
static struct client {
  int fd;
  char *rbuf;
} *clients;

/*
  prepare file descriptor.
*/
static int
init_serv_fd(char *path)
{
  int foo;
  int fd;
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
  if(logname) {
    pw = getpwnam(logname);
    if(pw)
      chown(path, pw->pw_uid, -1);
  }

  foo = listen(fd, 5);
  if (foo == -1) {
    perror("failed in listen()");
    return -1;
  }
  /*  fprintf(stderr,"listen at %s\n",path);*/
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
  nr_client_slots ++;
  clients = realloc(clients, sizeof(struct client) * nr_client_slots);
  clients[nr_client_slots - 1].rbuf = strdup("");
  return &clients[nr_client_slots - 1];
}

static void
free_client(struct client *cl)
{
  if (cl->rbuf) {
    free(cl->rbuf);
    cl->rbuf = strdup("");
  }
  cl->fd = -1;
}


static void
parse_content(char *content, struct client *cl)
{
  int i;
  int ret, content_len, out_len;
  char *out;

  content_len = strlen(content);

  for (i = 0; i < nr_client_slots; i++) {
    if (clients[i].fd != -1 && clients[i].fd != cl->fd &&
		    (uim_helper_fd_writable(clients[i].fd) > 0)) {
      out = content;
      out_len = content_len;
      while (out_len > 0) {
	if ((ret = write(clients[i].fd, out, out_len)) < 0) {
	  if (errno == EAGAIN || errno == EINTR)
	    continue;

      	  if (errno == EPIPE) {
	    close(clients[i].fd);
	    free_client(&clients[i]);
	  }
	  break;
        }

	out += ret;
	out_len -= ret;
      }
    }
  }
}

static int
proc_func(struct client *cl)
{
  int rc;
  char buf[BUFFER_SIZE];

  /* do read */
  rc = read(cl->fd, buf, BUFFER_SIZE - 1);
  if (rc == 0) {
    close(cl->fd);
    return -1;
  }

  if (rc < 0) {
    return -1;
  }

  buf[rc] = '\0';

  cl->rbuf = (char *)realloc(cl->rbuf, strlen(cl->rbuf) + strlen(buf)+1);
  strcat(cl->rbuf, buf);

  if (uim_helper_str_terminated(cl->rbuf)) {
    /* process */
    parse_content(cl->rbuf, cl);
    free(cl->rbuf);
    cl->rbuf = strdup("");
  }

  return 1;
}

static void
uim_helper_server_process_connection(int serv_fd)
{
  int i;
  int fd_biggest = 0;
  fd_set readfds;

  fd_biggest = serv_fd;

  while (1) {
    /* setup readfds */
    FD_ZERO(&readfds);
    FD_SET(serv_fd, &readfds);  
    for (i = 0; i < nr_client_slots; i ++) {
      int fd = clients[i].fd;
      if (fd == -1) {
	continue;
      }
      FD_SET(fd, &readfds);
      if (fd > fd_biggest) {
	fd_biggest = fd;
      }
    }

    /* call select(), waiting until a file descriptor readable */
    if (select(fd_biggest+1, &readfds, NULL, NULL, NULL) < 0) {
      perror("select faild");
    }

    /* for accept new connection */
    if (FD_ISSET(serv_fd, &readfds)) {
      struct sockaddr_un clientsoc;
      socklen_t len = sizeof(clientsoc);
      int new_fd;
      struct client *cl;
      new_fd = accept(serv_fd, (struct sockaddr *)&clientsoc, &len);

      if(new_fd < 0) {
	perror("accpet failed");
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
    }    
    
    /* check data from clients reached */
    for (i = 0; i < nr_client_slots; i ++) {
      if (clients[i].fd != -1 &&
	  FD_ISSET(clients[i].fd, &readfds)) {
	int result;
	/* actual process */
	result = proc_func(&clients[i]);

	if (result < 0) {
	  close(clients[i].fd);
	  free_client(&clients[i]);
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


