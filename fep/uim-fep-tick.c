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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "udsock.h"
#define FALSE 0
#define TRUE 1
#define EOT 4
#define BUFSIZE 600


static void usage(void);
static void version(void);
static volatile sig_atomic_t terminate_requested;

static void
terminate_handler(int sig)
{
  terminate_requested = sig;
}

static void
init_signal_handlers(void)
{
  struct sigaction action;

  memset(&action, 0, sizeof(action));
  sigemptyset(&action.sa_mask);
  action.sa_handler = terminate_handler;
  sigaction(SIGHUP, &action, NULL);
  sigaction(SIGINT, &action, NULL);
  sigaction(SIGQUIT, &action, NULL);
  sigaction(SIGTERM, &action, NULL);
}

int main(int argc, char **argv)
{
  char buf[BUFSIZE + 1];
  char prev_buf[BUFSIZE + 1];
  const char *sock_path = NULL;
  int op;

  while ((op = getopt(argc, argv, "w:s:l:vh")) != -1) {
    switch (op) {
      case 's':
        sock_path = optarg;
        break;
      case 'v':
        version();
        return EXIT_SUCCESS;
      case 'h':
        usage();
        return EXIT_SUCCESS;
      case '?':
      default:
        usage();
        return EXIT_FAILURE;
    }
  }

  if (optind != argc) {
    usage();
    return EXIT_FAILURE;
  }

  init_sendsocket(sock_path);

  buf[0] = EOT;
  buf[1] = '\0';
  sendline(buf);
  close_socket();

  init_signal_handlers();

  init_recvsocket(sock_path);

  if (terminate_requested) {
    unlink_recvsocket(sock_path);
    return EXIT_FAILURE;
  }

  prev_buf[0] = '\0';

  while (TRUE) {
    int len = recvline(buf, BUFSIZE);
    if (len == 1 && buf[0] == EOT) {
      return EXIT_SUCCESS;
    }
    if (len < 0) {
      if (errno == EINTR && !terminate_requested) {
        continue;
      }
      unlink_recvsocket(sock_path);
      return EXIT_FAILURE;
    }
    buf[len] = '\0';
    if (strcmp(buf, prev_buf) != 0) {
      puts(buf);
      strlcpy(prev_buf, buf, sizeof(prev_buf));
      fflush(stdout);
    }
  }
  return EXIT_SUCCESS;
}

static void version(void)
{
  printf("uim-fep-tick %s\n", PACKAGE_VERSION);
}

static void usage(void)
{
  printf("usage: uim-fep-tick [-vh] [-s <socket file> [default=%s]]\n", usersockname(NULL));
  fflush(stdout);
}
