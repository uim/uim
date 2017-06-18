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
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>

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

const static opt_args open_flags[] = {
  { O_RDONLY,   "$O_RDONLY" },
  { O_WRONLY,   "$O_WRONLY" },
  { O_RDWR,     "$O_RDWR" },

#ifdef O_NONBLOCK
  { O_NONBLOCK, "$O_NONBLOCK" },
#endif
#ifdef O_APPEND
  { O_APPEND,   "$O_APPEND" },
#endif

#ifdef O_SHLOCK
  { O_SHLOCK,   "$O_SHLOCK" },
#endif
#ifdef O_EXLOCK
  { O_EXLOCK,   "$O_EXLOCK" },
#endif

#ifdef O_NOFOLLOW
  { O_NOFOLLOW, "$O_NOFOLLOW" },
#endif

#ifdef O_SYNC
  { O_SYNC,     "$O_SYNC" },
#endif

  { O_CREAT,    "$O_CREAT" },
  { O_TRUNC,    "$O_TRUNC" },
  { O_EXCL,     "$O_EXCL" },

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
  return MAKE_INT(open(REFER_C_STR(path_), C_INT(flags_), C_INT(mode_)));
}

static uim_lisp
c_file_close(uim_lisp fd_)
{
  return MAKE_INT(close(C_INT(fd_)));
}

struct c_file_read_args {
  const unsigned char *buf;
  int nr;
};

static uim_lisp
c_file_read_internal(struct c_file_read_args *args)
{
  int i;
  uim_lisp ret_ = uim_scm_null();
  const unsigned char *p = args->buf;

  ret_ = uim_scm_null();
  for (i = 0; i < args->nr; i++) {
    ret_ = CONS(MAKE_CHAR(*p), ret_);
    p++;
  }
  return ret_;
}

static uim_lisp
c_file_read(uim_lisp d_, uim_lisp nbytes_)
{
  unsigned char *buf;
  uim_lisp ret_;
  int nbytes = C_INT(nbytes_);
  int nr;
  struct c_file_read_args args;

  buf = uim_malloc(nbytes);
  if ((nr = read(C_INT(d_), buf, nbytes)) == 0)
    return uim_scm_eof();
  if (nr < 0)
    return uim_scm_f();

  args.buf = buf;
  args.nr = nr;
  ret_ = (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)c_file_read_internal,
						    (void *)&args);
  free(buf);
  return uim_scm_callf("reverse", "o", ret_);
}

static uim_lisp
c_file_write(uim_lisp d_, uim_lisp buf_)
{
  int nbytes = uim_scm_length(buf_);
  uim_lisp ret_;
  unsigned char *buf;
  unsigned char *p;

  buf = p = uim_malloc(nbytes);
  while (!NULLP(buf_)) {
    *p = C_CHAR(CAR(buf_));
    p++;
    buf_ = CDR(buf_);
  }
  ret_ = MAKE_INT((int)write(C_INT(d_), buf, nbytes));
  free(buf);
  return ret_;
}

const static opt_args position_whence[] = {
  { SEEK_SET, "$SEEK_SET" },
  { SEEK_CUR, "$SEEK_CUR" },
  { SEEK_END, "$SEEK_END" },
  { 0, 0 }
};

static uim_lisp uim_lisp_position_whence;
static uim_lisp
c_file_position_whence(void)
{
  return uim_lisp_position_whence;
}

static uim_lisp
c_file_position_set(uim_lisp fildes_, uim_lisp offset_, uim_lisp whence_)
{
  int ret = 0;

  ret = lseek(C_INT(fildes_), C_INT(offset_), C_INT(whence_));
  if (ret == -1) {
    uim_lisp err_ = LIST3(fildes_, offset_, whence_);
    ERROR_OBJ(strerror(errno), err_);
  }
  return MAKE_INT(ret);
}

static uim_lisp
c_duplicate2_fileno(uim_lisp oldd_, uim_lisp newd_)
{
  if (FALSEP(newd_))
    return MAKE_INT(dup(C_INT(oldd_)));
  return MAKE_INT(dup2(C_INT(oldd_), C_INT(newd_)));
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
  { 0, 0 }
};

static uim_lisp uim_lisp_poll_flags;
static uim_lisp
c_file_poll_flags(void)
{
  return uim_lisp_poll_flags;
}

struct c_file_poll_args {
  struct pollfd *fds;
  int nfds;
};

static uim_lisp
c_file_poll_internal(struct c_file_poll_args *args)
{
  int i;
  uim_lisp ret_ = uim_scm_null();
  struct pollfd *fds = args->fds;

  for (i = 0; i < args->nfds; i++)
    if (fds[i].revents != 0)
      ret_ = CONS(CONS(MAKE_INT(fds[i].fd), MAKE_INT(fds[i].revents)), ret_);
  return ret_;
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
  struct c_file_poll_args args;

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

  args.fds = fds;
  args.nfds = nfds;
  ret_ = (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)c_file_poll_internal,
						    (void *)&args);
  free(fds);
  return uim_scm_callf("reverse", "o", ret_);
}

static uim_lisp
c_create_pipe(void)
{
  int fildes[2];

  if (pipe(fildes) == -1)
    return uim_scm_f();
  return CONS(MAKE_INT(fildes[0]), MAKE_INT(fildes[1]));
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc3("file-open", c_file_open);
  uim_scm_init_proc0("file-open-flags?", c_file_open_flags);
  uim_scm_init_proc0("file-open-mode?", c_file_open_mode);
  uim_lisp_open_flags = make_arg_list(open_flags);
  uim_lisp_open_mode = make_arg_list(open_mode);
  uim_scm_gc_protect(&uim_lisp_open_flags);
  uim_scm_gc_protect(&uim_lisp_open_mode);

  uim_scm_init_proc1("file-close", c_file_close);
  uim_scm_init_proc2("file-read", c_file_read);
  uim_scm_init_proc2("file-write", c_file_write);
  uim_scm_init_proc3("file-position-set!", c_file_position_set);
  uim_scm_init_proc0("file-position-whence?", c_file_position_whence);
  uim_lisp_position_whence = make_arg_list(position_whence);
  uim_scm_gc_protect(&uim_lisp_position_whence);

  uim_scm_init_proc2("duplicate2-fileno", c_duplicate2_fileno);

  uim_scm_init_proc2("file-poll", c_file_poll);
  uim_scm_init_proc0("file-poll-flags?", c_file_poll_flags);
  uim_lisp_poll_flags = make_arg_list(poll_flags);
  uim_scm_gc_protect(&uim_lisp_poll_flags);

  uim_scm_init_proc0("create-pipe", c_create_pipe);
}

void
uim_plugin_instance_quit(void)
{
  uim_scm_gc_unprotect(&uim_lisp_open_flags);
  uim_scm_gc_unprotect(&uim_lisp_open_mode);
  uim_scm_gc_unprotect(&uim_lisp_position_whence);
  uim_scm_gc_unprotect(&uim_lisp_poll_flags);
}
