/*

  uim-ipc.c: Utility functions for inter process communication.

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

#include <signal.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "uim-internal.h"
#include "uim-util.h"

/* This function is come from the GNU C Library manual */
static int
set_cloexec(int fd)
{
  int oldflags = fcntl (fd, F_GETFD, 0);
  /* If reading the flags failed, return error indication now.
  if (oldflags < 0)
  return oldflags;*/

  oldflags |= FD_CLOEXEC;
  /* Store modified flag word in the fdriptor. */
  return fcntl (fd, F_SETFD, oldflags);
}


static pid_t
open_pipe_rw(FILE **fr, FILE **fw)
{
  int fdr[2];
  int fdw[2];
  int pipe_fd[2];
  pid_t pid;
  int res;
  ssize_t nr;

  if (pipe(pipe_fd) < 0)
    return (pid_t)-1;

  if (fr && pipe(fdr) < 0)
    goto err0;
  if (fw && pipe(fdw) < 0)
    goto err1;

  pid = fork();
  if (pid < 0)
    goto err2;
  if (pid == 0) {
    /* child */
    pid = fork();
    if (pid) {
      /* child. report status of grand child for father anyway and _exit() */
      write(pipe_fd[1], &pid, sizeof(pid_t));
      _exit(0);
    } else {
      /* grand child */
      close(pipe_fd[0]);
      close(pipe_fd[1]);
      if (fr) {
	close(fdr[0]);
	dup2(fdr[1], 1);
      }
      if (fw) {
	close(fdw[1]);
	dup2(fdw[0], 0);
      }
    }
    return 0;
  }
  /* parent */
  waitpid(pid, &res, 0);
  if (fr) {
    close(fdr[1]);
    if (*fr == stdin)
      dup2(fdr[0], 0);
    else
      *fr = fdopen(fdr[0], "r");
  }
  if (fw) {
    close(fdw[0]);
    if (*fw == stdout)
      dup2(fdw[1], 1);
    else
      *fw = fdopen(fdw[1], "w");
  }

  if ((nr = read(pipe_fd[0], &pid, sizeof(pid_t))) == -1 || nr == 0)
    goto err0;

  close(pipe_fd[0]);
  close(pipe_fd[1]);

  if (pid != -1) {
    return pid;
  }
 err2:
  if (fw) {
    close(fdw[0]);
    close(fdw[1]);
  }
 err1:
  if (fr) {
    close(fdr[0]);
    close(fdr[1]);
  }
 err0:
  return (pid_t) -1;
}

pid_t
uim_ipc_open_command_with_option(pid_t old_pid,
				 FILE **read_fp, FILE **write_fp,
				 const char *command, const char *option)
{
  int result;
  pid_t new_pid;
  char **ap, *argv[10];
  char *str = NULL, *p;

  if (*read_fp != NULL) {
    fclose(*read_fp);
  }
  if (*write_fp != NULL) {
    fclose(*write_fp);
  }
  
  *read_fp = *write_fp = NULL;
  
  /* kill child process if exists */
  if (old_pid) {
    kill(old_pid, SIGKILL);
  }

  new_pid = open_pipe_rw(read_fp, write_fp);

  if (new_pid < 0)
    return 0;

  if (new_pid == 0) {
    /* child */

    int open_max;
    int i;
    
    open_max = sysconf (_SC_OPEN_MAX);
    for (i = 3; i < open_max; i++) {
      set_cloexec(i);      
    }

    if (!option) {
      argv[0] = (char *)command;
      argv[1] = NULL;
    } else {
      argv[0] = (char *)command;
      str = p = uim_strdup(option);
      for (ap = &argv[1]; (*ap = strsep(&p, " ")) != NULL;) {
	if (**ap != '\0')
	  if (++ap >= &argv[9])
	    break;
      }
      *ap = NULL;
    }
    if (uim_issetugid()) {
      int cmd_len = strlen(command) + 30;
      char *fullpath_command = uim_malloc(cmd_len);
      char *cmd_name = strrchr(command, '/');

      if (cmd_name && cmd_name + 1 != '\0')
	cmd_name++;
      else
	cmd_name = (char *)command;
      /*if (setuid(getuid())!=0) abort();*/ /* discarding privilege */
      
      snprintf(fullpath_command, cmd_len, "/usr/local/bin/%s", cmd_name);

      result = execv(fullpath_command, argv);

      if (result == -1) {
 	snprintf(fullpath_command, cmd_len, "/usr/bin/%s", cmd_name);
	result = execv(fullpath_command, argv);
      }
      if (result == -1) {
 	snprintf(fullpath_command, cmd_len, UIM_LIBEXECDIR "/%s", cmd_name);
	result = execv(fullpath_command, argv);
      }
      free(fullpath_command);
    } else {
      result = execvp(command, argv);
    }
    free(str);

    if (result == -1) {
      write(1,"err",strlen("err"));
    }
    _exit(127);
  }

  return new_pid;
}

pid_t
uim_ipc_open_command(pid_t old_pid,
		     FILE **read_fp, FILE **write_fp, const char *command)
{
  return uim_ipc_open_command_with_option(old_pid,
					  read_fp, write_fp, command, NULL);
}

char *
uim_ipc_send_command(pid_t *pid,
		     FILE **read_fp, FILE **write_fp,
		     const char *command, const char *str)
{
  char *tmp = uim_strdup("");
  char buf[8192];

  if (*read_fp == NULL || *write_fp == NULL) {
    *pid = uim_ipc_open_command(*pid, read_fp, write_fp, command);
  }
  if (*pid == 0) {
    free(tmp);
    return NULL;
  }

  fputs(str, *write_fp);

 again:
  if (fflush(*write_fp) != 0) {
    switch (errno) {
    case EINTR:
      goto again;
    default:
      goto err;
    }
  }

  if (feof(*read_fp)) {
    fclose(*read_fp);
    fclose(*write_fp);
    *read_fp = NULL;
    *write_fp = NULL;
    free(tmp);
    return NULL;
  }

  while (fgets (buf, sizeof(buf), *read_fp) != NULL) {
    if (strcmp( buf, "\n" ) == 0) {
      break;
    }

    tmp = uim_realloc(tmp, strlen(tmp) + strlen(buf) + 1);
    strcat(tmp, buf);
  }
 
  return tmp;

 err:
  free(tmp);
  *pid = uim_ipc_open_command(*pid, read_fp, write_fp, command);
  return NULL;
}
