/*
  Copyright (c) 2006-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or
  without modification, are permitted provided that the
  following conditions are met:

  1. Redistributions of source code must retain the above
     copyright notice, this list of conditions and the
     following disclaimer.
  2. Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the
     following disclaimer in the documentation and/or other
     materials provided with the distribution.
  3. Neither the name of authors nor the names of its
     contributors may be used to endorse or promote products
     derived from this software without specific prior written
     permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <locale.h>

#include <sys/types.h>
#include <sys/select.h>

#include <uim/uim.h>
#include <uim/uim-helper.h>

#include "uim-el-helper-agent.h"

char* cmdbuf = NULL;
unsigned cmdbuf_len = 0;

int focused = 0;

static int
command_exists_in_cmdbuf()
{
  return (strchr(cmdbuf, '\n') != NULL);
}


static int
process_command()
{
  char *p;
  char *cmd;
  unsigned rest;

  debug_printf(DEBUG_NOTE, "process command\n");
  
  /* cmd always terminates with \n */

  p = strchr(cmdbuf, '\n');
  *p = '\0';

  /* send command to helper-server */
  cmd = helper_message_decode(cmdbuf);
  uim_helper_send_message(helper_fd, cmd);

  if (strcmp(cmd, "focus_in\n") == 0)
	focused = 1;

  free(cmd);

  rest = strlen(p + 1);

  if (rest > 0)
	memmove(cmdbuf, p + 1, rest);
  else
	cmdbuf[0] = '\0';

  return 1;
}


static void
process_message(char *msg)
{
  char *eol;

  debug_printf(DEBUG_NOTE, "process message from uim-helper-server\n");
  
  if (msg) {
	if ((eol = strchr(msg, '\n')) != NULL) {
	  *eol = '\0';
	} else {
	  free(msg);
	  return;
	}

	if (strcmp("focus_in", msg) == 0) {

	  if (focused)
		printf("focus_in\n");

	  focused = 0;

	} else if (strcmp("prop_activate", msg) == 0) { 

	  char *prop = eol + 1;

	  if ((eol = strchr(prop, '\n')) != NULL) {
		*eol = '\0';
		printf("prop_activate %s\n", prop);
	  }

	} else if (strcmp("prop_list_get", msg) == 0) { 

	  printf("prop_list_get\n");

	} else if (strncmp(msg, "im_change_", 10) == 0) {

	  char *imname = eol + 1;

	  if ((eol = strchr(imname, '\n')) != NULL) {
		*eol = '\0';
		printf("%s %s\n", msg, imname);
	  }
	  
	} else if (strcmp("im_list_get", msg) == 0) {

	  printf("im_list_get\n");

	} else if (strcmp("commit_string", msg) == 0) {

	  char *rest, *charset = "UTF-8";

	  debug_printf(DEBUG_NOTE, "commit string\n");

	  rest = eol + 1;

	  if ((eol = strchr(rest, '\n')) != NULL) {
		*eol = '\0';
		if (strncmp(rest, "charset=", 8) == 0) {
		  charset = rest + 8;
		  if ((eol = strchr(charset, ' ')) != NULL) {
			*eol = '\0';
			rest = eol + 1;
		  }
		}
		rest = helper_message_encode(rest);
		printf("commit_string %s %s\n", charset, rest);
		free(rest);
	  }

	} else if (strcmp("prop_update_custom", msg) == 0) {
	  
	  char *val, *custom = eol + 1;

	  if ((eol = strchr(custom, '\n')) != NULL) {
		*eol = '\0';
		val = eol + 1;

		if ((eol = strchr(val, '\n')) != NULL) {
		  *eol = '\0';
		  printf("prop_update_custom %s %s\n", custom, val);
		}
	  }

	} else if (strcmp("custom_reload_notify", msg) == 0) {

	  printf("custom_reload_notify\n");

	} else {

	  debug_printf(DEBUG_NOTE, "other message %s\n", msg);

	}

	fflush(NULL);
	free(msg);
  }
}


/**
 * @return 1 if success, 0 if error.
 */
static int
read_command()
{
  ssize_t len;
  char rbuf[DEFAULT_MESSAGE_SIZE];

  debug_printf(DEBUG_NOTE, "read command\n");

  do {
	len = read(STDIN_FILENO, rbuf, sizeof(rbuf) - 1);
	if (len == -1) {
	  debug_printf(DEBUG_NOTE, "stdin is corrupt: %s\n", strerror (errno));
	  return 0;
	}
	if (len == 0) {
	  debug_printf(DEBUG_NOTE, "unexpected EOF\n");
	  return 0;
	}

	rbuf[len] = '\0';

	if (strlen(cmdbuf) + len + 1 > cmdbuf_len) {
	  cmdbuf_len += DEFAULT_MESSAGE_SIZE;
	  cmdbuf = uim_realloc(cmdbuf, cmdbuf_len);
	  debug_printf(DEBUG_NOTE, "cmdbuf has extended\n");
	}

	strcat(cmdbuf, rbuf);

  } while (!command_exists_in_cmdbuf());

  return 1;
}


static void
wait_data_arrival(fd_set *rfds)
{
  int fdmax = STDIN_FILENO;

  if (helper_fd > 0) {
	FD_ZERO(rfds);
	FD_SET(helper_fd, rfds);
	if (helper_fd > fdmax) fdmax = helper_fd;
  }

  FD_SET(STDIN_FILENO, rfds);

  if (select(helper_fd + 1, rfds, NULL, NULL, NULL) < 0)
	debug_printf(DEBUG_ERROR, "select error\n");

}



void
cleanup(void)
{
  uim_quit();
}


int
main(int argc, char *argv[])
{
  int opt;

  while ((opt = getopt(argc, argv, "d")) != -1) {
	switch (opt) {
	case 'd':
	  debug_level ++;
	  break;
	}
  }

  if (debug_level == 0) fclose(stderr);

  if (uim_init() < 0) {
	debug_printf(DEBUG_ERROR, "uim_init failed\n");
	return -1;
  }

  atexit(cleanup);

  a_printf("OK\n");

  cmdbuf_len = DEFAULT_MESSAGE_SIZE;
  cmdbuf = uim_malloc(cmdbuf_len);
  cmdbuf[0] = '\0';

  while (1) {
	char *msg;
	fd_set rfds;

	check_helper_connection();

	wait_data_arrival(&rfds);

	debug_printf(DEBUG_NOTE, "data arrive\n");

	if (FD_ISSET(STDIN_FILENO, &rfds)) {
	  if (!read_command())
	    goto QUIT;
	}

	if (FD_ISSET(helper_fd, &rfds)) {
	  /* read message from helper */
	  uim_helper_read_proc(helper_fd);
	}

	while (command_exists_in_cmdbuf())
	  process_command();

	while ((msg = uim_helper_get_message())) {
	  process_message(msg);
	}
	fflush(NULL);
  }

 QUIT:
  uim_quit();
  return 0;
}

