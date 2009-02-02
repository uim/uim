/*

  Copyright (c) 2003-2009 uim Project http://code.google.com/p/uim/

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
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <pwd.h>
#include <errno.h>
#include <assert.h>
#include <fcntl.h>

#ifdef HAVE_POLL_H
#include <poll.h>
#elif defined(HAVE_SYS_POLL_H)
#include <sys/poll.h>
#else
#include "bsd-poll.h"
#endif

#include "uim.h"
#include "uim-internal.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-posix.h"
#include "uim-notify.h"
#include "gettext.h"

uim_bool
uim_get_user_name(char *name, int len, int uid)
{
  struct passwd *pw;

  if (len <= 0)
    return UIM_FALSE;
  pw = getpwuid(uid);
  if (!pw) {
    name[0] = '\0';
    return UIM_FALSE;
  }
  if (strlcpy(name, pw->pw_name, len) >= (size_t)len) {
    name[0] = '\0';
    endpwent();
    return UIM_FALSE;
  }
  endpwent();
  return UIM_TRUE;
}

static uim_lisp
user_name(void)
{
  char name[BUFSIZ];

  if (!uim_get_user_name(name, sizeof(name), getuid()))
    return uim_scm_f();

  return MAKE_STR(name);
}

uim_bool
uim_get_home_directory(char *home, int len, int uid)
{
  struct passwd *pw;

  if (len <= 0)
    return UIM_FALSE;
  pw = getpwuid(uid);
  if (!pw) {
    home[0] = '\0';
    return UIM_FALSE;
  }
  if (strlcpy(home, pw->pw_dir, len) >= (size_t)len) {
    home[0] = '\0';
    endpwent();
    return UIM_FALSE;
  }
  endpwent();
  return UIM_TRUE;
}

static uim_lisp
home_directory(uim_lisp user_)
{
  int uid;
  char home[MAXPATHLEN];

  if (INTP(user_)) {
    uid = C_INT(user_);
  } else if (STRP(user_)) {
    struct passwd *pw;

    pw = getpwnam(REFER_C_STR(user_));

    if (!pw)
      return uim_scm_f();

    uid = pw->pw_uid;
    endpwent();
  } else {
    return uim_scm_f();
  }

  if (!uim_get_home_directory(home, sizeof(home), uid)) {
    char *home_env = getenv("HOME");
    if (home_env)
      return MAKE_STR(home_env);
    return uim_scm_f();
  }

  return MAKE_STR(home);
}

uim_bool
uim_check_dir(const char *dir)
{
  struct stat st;

  if (stat(dir, &st) < 0)
    return (mkdir(dir, 0700) < 0) ? UIM_FALSE : UIM_TRUE;
  else {
    mode_t mode = S_IFDIR | S_IRUSR | S_IWUSR | S_IXUSR;
    return ((st.st_mode & mode) == mode) ? UIM_TRUE : UIM_FALSE;
  }
}

uim_bool
uim_get_config_path(char *path, int len, int is_getenv)
{
  char home[MAXPATHLEN];

  if (len <= 0)
    return UIM_FALSE;

  if (!uim_get_home_directory(home, sizeof(home), getuid()) && is_getenv) {
    char *home_env = getenv("HOME");

    if (!home_env)
      return UIM_FALSE;

    if (strlcpy(home, home_env, sizeof(home)) >= sizeof(home))
      return UIM_FALSE;
  }

  if (snprintf(path, len, "%s/.uim.d", home) == -1)
    return UIM_FALSE;

  if (!uim_check_dir(path)) {
    return UIM_FALSE;
  }

  return UIM_TRUE;
}

static uim_lisp
file_stat_mode(uim_lisp filename, mode_t mode)
{
  struct stat st;
  int err;

  err = stat(REFER_C_STR(filename), &st);
  if (err)
    return uim_scm_f();  /* intentionally returns #f instead of error */

  return MAKE_BOOL((st.st_mode & mode) == mode);
}

static uim_lisp
file_readablep(uim_lisp filename)
{
  return file_stat_mode(filename, S_IRUSR);
}

static uim_lisp
file_writablep(uim_lisp filename)
{
  return file_stat_mode(filename, S_IWUSR);
}

static uim_lisp
file_executablep(uim_lisp filename)
{
  return file_stat_mode(filename, S_IXUSR);
}

static uim_lisp
file_regularp(uim_lisp filename)
{
  return file_stat_mode(filename, S_IFREG);
}

static uim_lisp
file_directoryp(uim_lisp filename)
{
  return file_stat_mode(filename, S_IFDIR);
}

static uim_lisp
file_mtime(uim_lisp filename)
{
  struct stat st;
  int err;

  err = stat(REFER_C_STR(filename), &st);
  if (err)
    ERROR_OBJ("stat failed for file", filename);

  return MAKE_INT(st.st_mtime);
}

static uim_lisp
c_getenv(uim_lisp str)
{
  char *val;

  ENSURE_TYPE(str, str);

  val = getenv(REFER_C_STR(str));

  return (val) ? MAKE_STR(val) : uim_scm_f();
}

static uim_lisp
c_setenv(uim_lisp name, uim_lisp val, uim_lisp overwrite)
{
  int err;

  err = setenv(REFER_C_STR(name), REFER_C_STR(val), TRUEP(overwrite));

  return MAKE_BOOL(!err);
}

static uim_lisp
c_unsetenv(uim_lisp name)
{
  unsetenv(REFER_C_STR(name));

  return uim_scm_t();
}

static uim_lisp
setugidp(void)
{
  assert(uim_scm_gc_any_contextp());

  return MAKE_BOOL(uim_issetugid());
}

static uim_lisp
time_t_to_uim_lisp(time_t t)
{
  char t_str[64];

  snprintf(t_str, sizeof(t_str), "%.32g", (double)t);
  return MAKE_STR(t_str);
}

static uim_bool
uim_lisp_to_time_t(time_t *t, uim_lisp t_)
{
  const char *t_str = REFER_C_STR(t_);
  char *end;

  *t = (time_t)strtod(t_str, &end);
  return *end == '\0';
}

static uim_lisp
c_time(void)
{
  time_t now;

  if ((time(&now)) == (time_t) -1)
    return CONS(MAKE_SYM("error"), MAKE_STR(strerror(errno)));
  return time_t_to_uim_lisp(now);
}

static uim_lisp
c_difftime(uim_lisp time1_, uim_lisp time0_)
{
  time_t time1, time0;

  if (!uim_lisp_to_time_t(&time1, time1_))
    return uim_scm_f();
  if (!uim_lisp_to_time_t(&time0, time0_))
    return uim_scm_f();
  return time_t_to_uim_lisp(difftime(time1, time0));
}


static uim_lisp
c_sleep(uim_lisp seconds_)
{
  return MAKE_INT(sleep((unsigned int)C_INT(seconds_)));
}

typedef struct {
  int flag;
  char *arg;
} opt_args;

static uim_lisp
make_arg_list(const opt_args *list)
{
  uim_lisp ret_;
  int i = 0;

  ret_ = uim_scm_null();
  while (list[i].arg != 0) {
    ret_ = CONS(CONS(MAKE_SYM(list[i].arg), MAKE_INT(list[i].flag)), ret_);
    i++;
  }
  return ret_;
}

static uim_lisp
make_args_or(const opt_args *list, int flag)
{
  uim_lisp ret_;
  int i = 0;

  ret_ = uim_scm_null();
  while (list[i].arg != 0) {
    if (list[i].flag & flag)
      ret_ = CONS(MAKE_SYM(list[i].arg), ret_);
    i++;
  }
  return uim_scm_callf("reverse", "o", ret_);
}

const static opt_args open_flags[] = {
  { O_CREAT,    "$O_CREAT" },
  { O_EXCL,     "$O_EXCL" },
#ifdef O_EXLOCK
  { O_EXLOCK,   "$O_EXLOCK" },
#endif
  { O_NONBLOCK, "$O_NONBLOCK" },
  { O_RDONLY,   "$O_RDONLY" },
  { O_RDWR,     "$O_RDWR" },
#ifdef O_SHLOCK
  { O_SHLOCK,   "$O_SHLOCK" },
#endif
  { O_TRUNC,    "$O_TRUNC" },
  { 0, 0 }
};

const static opt_args open_mode[] = {
  { S_IRWXU, "$S_IRWXU" },
  { S_IRUSR, "$S_IRUSR" },
  { S_IWUSR, "$S_IWUSR" },
  { S_IXUSR, "$S_IXUSR" },

  { S_IRWXG, "$S_IRWXG" },
  { S_IRGRP, "$S_IRGRP" },
  { S_IWGRP, "$S_IWGRP" },
  { S_IXGRP, "$S_IXGRP" },

  { S_IRWXO, "$S_IRWXO" },
  { S_IROTH, "$S_IROTH" },
  { S_IWOTH, "$S_IWOTH" },
  { S_IXOTH, "$S_IXOTH" },
  { 0, 0 }
};


static uim_lisp uim_lisp_open_flags;
static uim_lisp
c_file_open_flags(void)
{
  return uim_lisp_open_flags;
}

static uim_lisp uim_lisp_open_mode;
static uim_lisp
c_file_open_mode(void)
{
  return uim_lisp_open_mode;
}

static uim_lisp
c_file_open(uim_lisp path_, uim_lisp flags_, uim_lisp mode_)
{
  int flags = 0;
  int mode = 0;

  while (!NULLP(flags_)) {
    flags |= C_INT(CAR(flags_));
    flags_ = CDR(flags_);
  }
  while (!NULLP(mode_)) {
    mode |= C_INT(CAR(mode_));
    mode_ = CDR(mode_);
  }
  return MAKE_INT(open(REFER_C_STR(path_), flags, mode));
}

static uim_lisp
c_file_close(uim_lisp fd_)
{
  return MAKE_INT(close(C_INT(fd_)));
}

static uim_lisp
c_file_read(uim_lisp d_, uim_lisp nbytes_)
{
  char *buf;
  uim_lisp ret_;
  int nbytes = C_INT(nbytes_);
  int i;
  int nr;
  char *p;

  buf = uim_malloc(nbytes);
  if ((nr = read(C_INT(d_), buf, nbytes)) == -1) {
    char err[BUFSIZ];

    snprintf(err, sizeof(err), "file-read: %s", strerror(errno));
    uim_notify_fatal(err);
    ERROR_OBJ(err, d_);
  }

  p = buf;
  ret_ = uim_scm_null();
  for (i = 0; i < nr; i++) {
    ret_ = CONS(MAKE_INT(*p & 0xff), ret_);
    p++;
  }
  free(buf);
  return uim_scm_callf("reverse", "o", ret_);
}

static uim_lisp
c_file_write(uim_lisp d_, uim_lisp buf_)
{
  int nbytes = uim_scm_length(buf_);
  uim_lisp ret_;
  char *buf;
  char *p;

  buf = p = uim_malloc(nbytes);
  while (!NULLP(buf_)) {
    *p = (char)C_INT(CAR(buf_));
    p++;
    buf_ = CDR(buf_);
  }
  ret_ = MAKE_INT((int)write(C_INT(d_), buf, nbytes));
  free(buf);
  return ret_;
}

const static opt_args poll_flags[] = {
  { POLLIN,     "$POLLIN" },
#ifdef POLLPRI
  { POLLPRI,    "$POLLPRI" },
#endif
  { POLLOUT,    "$POLLOUT" },
  { POLLERR,    "$POLLERR" },
#ifdef POLLHUP
  { POLLHUP,    "$POLLHUP"},
#endif
#ifdef POLLNVAL
  { POLLNVAL,   "$POLLNVAL"},
#endif
#ifdef POLLRDNORM
  { POLLRDNORM, "$POLLRDNORM"},
#endif
#ifdef POLLNORM
  { POLLNORM,   "$POLLNORM"},
#endif
#ifdef POLLWRNORM
  { POLLWRNORM, "$POLLWRNORM"},
#endif
#ifdef POLLRDBAND
  { POLLRDBAND, "$POLLRDBAND"},
#endif
#ifdef POLLWRBAND
  { POLLWRBAND, "$POLLWRBAND"},
#endif
};

static uim_lisp uim_lisp_poll_flags;
static uim_lisp
c_file_poll_flags(void)
{
  return uim_lisp_poll_flags;
}

static uim_lisp
c_file_poll(uim_lisp fds_, uim_lisp timeout_)
{
  struct pollfd *fds;
  int timeout = C_INT(timeout_);
  int nfds = uim_scm_length(fds_);
  uim_lisp fd_ = uim_scm_f();
  int i;
  int ret;
  uim_lisp ret_;

  fds = uim_calloc(nfds, sizeof(struct pollfd));

  for (i = 0; i < nfds; i++) {
    fd_ = CAR(fds_);
    fds[i].fd = C_INT(CAR(fd_));
    fds[i].events = C_INT(CDR(fd_));
    fds_ = CDR(fds_);
  }

  ret = poll(fds, nfds, timeout);
  if (ret == -1)
    return uim_scm_f();
  else if (ret == 0)
    return uim_scm_null();

  ret_ = uim_scm_null();
  for (i = 0; i < ret; i++)
    ret_ = CONS(CONS(MAKE_INT(fds[i].fd), make_args_or(poll_flags, fds[i].revents)), ret_);
  free(fds);
  return uim_scm_callf("reverse", "o", ret_);
}

static uim_lisp
c_file_ready(uim_lisp fd_)
{
  struct pollfd pfd;
  int ndfs;

  pfd.fd = C_INT(fd_);
  pfd.events = POLLIN;
  ndfs = poll(&pfd, 1, 0);

  if (ndfs < 0) {
    return uim_scm_f();
  } else if (ndfs == 0)
    return uim_scm_f();
  else
    return uim_scm_t();
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

void
uim_init_posix_subrs(void)
{
  uim_scm_init_proc0("user-name", user_name);
  uim_scm_init_proc1("home-directory", home_directory);

  uim_scm_init_proc1("file-readable?", file_readablep);
  uim_scm_init_proc1("file-writable?", file_writablep);
  uim_scm_init_proc1("file-executable?", file_executablep);
  uim_scm_init_proc1("file-regular?", file_regularp);
  uim_scm_init_proc1("file-directory?", file_directoryp);
  uim_scm_init_proc1("file-mtime", file_mtime);

  uim_scm_init_proc0("setugid?", setugidp);

  uim_scm_init_proc1("getenv", c_getenv);
  uim_scm_init_proc3("setenv", c_setenv);
  uim_scm_init_proc1("unsetenv", c_unsetenv);

  uim_scm_init_proc0("time", c_time);
  uim_scm_init_proc2("difftime", c_difftime);

  uim_scm_init_proc1("sleep", c_sleep);

  uim_scm_init_proc3("file-open", c_file_open);
  uim_scm_init_proc0("file-open-flags?", c_file_open_flags);
  uim_scm_init_proc0("file-open-mode?", c_file_open_mode);
  uim_scm_gc_protect(&uim_lisp_open_flags);
  uim_scm_gc_protect(&uim_lisp_open_mode);
  uim_lisp_open_flags = make_arg_list(open_flags);
  uim_lisp_open_mode = make_arg_list(open_mode);
  uim_scm_eval_c_string("(define open-flags-alist (file-open-flags?))");
  uim_scm_eval_c_string("(define open-mode-alist (file-open-mode?))");

  uim_scm_init_proc1("file-close", c_file_close);
  uim_scm_init_proc2("file-read", c_file_read);
  uim_scm_init_proc2("file-write", c_file_write);

  uim_scm_init_proc2("file-poll", c_file_poll);
  uim_scm_init_proc0("file-poll-flags?", c_file_poll_flags);
  uim_scm_gc_protect(&uim_lisp_poll_flags);
  uim_lisp_poll_flags = make_arg_list(poll_flags);
  uim_scm_eval_c_string("(define poll-flags-alist (file-poll-flags?))");

  uim_scm_init_proc1("file-ready?", c_file_ready);

  uim_scm_init_proc0("current-process-id", c_current_process_id);
  uim_scm_init_proc0("parent-process-id",  c_parent_process_id);
  uim_scm_init_proc0("process-fork", c_process_fork);
  uim_scm_init_proc1("_exit", c__exit);
}
