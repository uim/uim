/*

  Copyright (c) 2009-2013 uim Project https://github.com/uim/uim

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

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "uim.h"
#include "uim-internal.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-posix.h"
#include "uim-notify.h"
#include "gettext.h"
#include "dynlib.h"

typedef struct {
  int flag;
  char *arg;
} opt_args;

static uim_lisp
make_arg_cons(const opt_args *arg)
{
  return CONS(MAKE_SYM(arg->arg), MAKE_INT(arg->flag));
}

static uim_lisp
make_arg_list(const opt_args *list)
{
  uim_lisp ret_;
  int i = 0;

  ret_ = uim_scm_null();
  while (list[i].arg != 0) {
    ret_ = CONS((uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)make_arg_cons,
							   (void *)&list[i]), ret_);
    i++;
  }
  return ret_;
}

static uim_lisp
c_current_process_id(void)
{
  return MAKE_INT(getpid());
}
static uim_lisp
c_parent_process_id(void)
{
  return MAKE_INT(getppid());
}
static uim_lisp
c_process_fork(void)
{
  return MAKE_INT(fork());
}
static uim_lisp
c__exit(uim_lisp status_)
{
  _exit(C_INT(status_));
  return uim_scm_t();
}

const static opt_args waitpid_options[] = {
#ifdef WCONTINUED
  { WCONTINUED, "$WCONTINUED" },
#endif
#ifdef WNOHANG
  { WNOHANG,    "$WNOHANG" },
#endif
#ifdef WUNTRACED
  { WUNTRACED,  "$WUNTRACED" },
#endif
  { 0, 0 }
};
static uim_lisp uim_lisp_process_waitpid_options;
static uim_lisp
c_process_waitpid_options(void)
{
  return uim_lisp_process_waitpid_options;
}
static uim_lisp
c_process_waitpid(uim_lisp pid_, uim_lisp options_)
{
  uim_lisp ret_ = uim_scm_null();
  int status;

  ret_ = MAKE_INT(waitpid(C_INT(pid_), &status, C_INT(options_)));
  if (WIFEXITED(status))
    return LIST5(ret_, uim_scm_t(), uim_scm_f(), uim_scm_f(), MAKE_INT(WEXITSTATUS(status)));
  else if (WIFSIGNALED(status))
    return LIST5(ret_, uim_scm_f(), uim_scm_t(), uim_scm_f(), MAKE_INT(WTERMSIG(status)));
#ifdef WIFSTOPPED
  else if (WIFSTOPPED(status))
    return LIST5(ret_, uim_scm_f(), uim_scm_f(), uim_scm_t(), MAKE_INT(WSTOPSIG(status)));
#endif

  return LIST5(ret_, uim_scm_f(), uim_scm_f(), uim_scm_f(), MAKE_INT(status));
}

static uim_lisp
c_daemon(uim_lisp nochdir_, uim_lisp noclose_)
{
  return MAKE_INT(daemon(C_BOOL(nochdir_), C_BOOL(noclose_)));
}

static uim_lisp
c_execve(uim_lisp file_, uim_lisp argv_, uim_lisp envp_)
{
  char **argv;
  char **envp;
  int i;
  int argv_len = uim_scm_length(argv_);
  int envp_len;
  uim_lisp ret_;

  if (argv_len < 1)
    return uim_scm_f();

  argv = uim_malloc(sizeof(char *) * (argv_len + 1));

  for (i = 0; i < argv_len; i++) {
    argv[i] = uim_strdup(REFER_C_STR(CAR(argv_)));
    argv_ = CDR(argv_);
  }
  argv[argv_len] = NULL;

  if (FALSEP(envp_) || NULLP(envp_)) {
    envp_len = 0;
    envp = NULL;
  } else {
    envp_len = uim_scm_length(envp_);
    envp = uim_malloc(sizeof(char *) * (envp_len + 1));

    for (i = 0; i < envp_len; i++) {
      uim_lisp env_ = CAR(envp_);

      uim_asprintf(&envp[i], "%s=%s", REFER_C_STR(CAR(env_)), REFER_C_STR(CDR(env_)));
      envp_ = CDR(envp_);
    }
    envp[envp_len] = NULL;
  }

  ret_ = MAKE_INT(execve(REFER_C_STR(file_), argv, envp));

  for (i = 0; i < argv_len; i++)
    free(argv[i]);
  free(argv);

  for (i = 0; i < envp_len; i++)
    free(envp[i]);
  free(envp);

  return ret_;
}

static uim_lisp
c_execvp(uim_lisp file_, uim_lisp argv_)
{
  char **argv;
  int i;
  int len = uim_scm_length(argv_);
  uim_lisp ret_;

  if (len < 1)
    return uim_scm_f();

  argv = uim_malloc(sizeof(char *) * (len + 1));

  for (i = 0; i < len; i++) {
    argv[i] = uim_strdup(REFER_C_STR(CAR(argv_)));
    argv_ = CDR(argv_);
  }
  argv[len] = NULL;

  ret_ = MAKE_INT(execvp(REFER_C_STR(file_), argv));

  for (i = 0; i < len; i++)
    free(argv[i]);
  free(argv);

  return ret_;
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc0("current-process-id", c_current_process_id);
  uim_scm_init_proc0("parent-process-id",  c_parent_process_id);
  uim_scm_init_proc0("process-fork", c_process_fork);
  uim_scm_init_proc1("_exit", c__exit);
  uim_scm_init_proc2("process-waitpid", c_process_waitpid);
  uim_scm_init_proc0("process-waitpid-options?", c_process_waitpid_options);
  uim_lisp_process_waitpid_options = make_arg_list(waitpid_options);
  uim_scm_gc_protect(&uim_lisp_process_waitpid_options);
  uim_scm_eval_c_string("(define process-waitpid-options-alist (process-waitpid-options?))");

  uim_scm_init_proc2("daemon", c_daemon);

  uim_scm_init_proc3("execve", c_execve);
  uim_scm_init_proc2("execvp", c_execvp);
}

void
uim_plugin_instance_quit(void)
{
  uim_scm_gc_unprotect(&uim_lisp_process_waitpid_options);
}
