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
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>

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

static const char *
find_flags_equal(const opt_args *list, int flag, const char **errstrp)
{
  char *arg = NULL;
  int i = 0;
  const static char *err = N_("Invalid argument");

  *errstrp = NULL;
  while (1) {
    if (list[i].arg == 0) {
      *errstrp = err;
      return 0;
    }
    if (list[i].flag == flag) {
      arg = list[i].arg;
      break;
    }
    i++;
  }
  return arg;
}

static int
find_args_equal(const opt_args *list, char *arg, const char **errstrp)
{
  int flags = 0;
  int i = 0;
  const static char *err = N_("Invalid argument");

  *errstrp = NULL;
  while (1) {
    if (list[i].arg == 0) {
      *errstrp = err;
      return 0;
    }
    if (strcmp(list[i].arg, arg) == 0) {
      flags = list[i].flag;
      break;
    }
    i++;
  }
  return flags;
}

static uim_lisp
ref_args_or(const opt_args *list, int flags)
{
  int i = 0;
  uim_lisp ret_ = uim_scm_null();

  while (list[i].arg != 0) {
    if (list[i].flag & flags) {
      ret_ = CONS(MAKE_SYM(list[i].arg), ret_);
    }
    i++;
  }
  return uim_scm_callf("reverse", "o", ret_);
}

static uim_lisp
make_arg_list(const opt_args *list)
{
  uim_lisp ret_;
  int i = 0;

  ret_ = uim_scm_null();
  while (list[i].arg != 0) {
    ret_ = CONS(MAKE_SYM(list[i].arg), ret_);
    i++;
  }
  return ret_;
}

const static opt_args open_flags[] = {
  { O_CREAT,    "$O_CREAT" },
  { O_EXCL,     "$O_EXCL" },
  { O_EXLOCK,   "$O_EXLOCK" },
  { O_NONBLOCK, "$O_NONBLOCK" },
  { O_RDONLY,   "$O_RDONLY" },
  { O_RDWR,     "$O_RDWR" },
  { O_SHLOCK,   "$O_SHLOCK" },
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
  const char *errstr;

  while (!NULLP(flags_)) {
    char *f = C_SYM(CAR(flags_));

    flags |= find_args_equal(open_flags, f, &errstr);
    free(f);
    if (errstr) {
      uim_notify_fatal("file-open: %s", errstr);
      ERROR_OBJ(errstr, CAR(flags_));
    }
    flags_ = CDR(flags_);
  }
  while (!NULLP(mode_)) {
    char *m = C_SYM(CAR(mode_));

    mode |= find_args_equal(open_mode, m, &errstr);
    free(m);
    if (errstr) {
      uim_notify_fatal("file-open: %s", errstr);
      ERROR_OBJ(errstr, CAR(mode_));
    }
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
  uim_lisp ret1_, ret2_;
  int nbytes = C_INT(nbytes_);
  int i;
  char *p;

  buf = uim_malloc(C_INT(nbytes_));
  ret1_ = MAKE_INT((int)read(C_INT(d_), buf, nbytes));

  p = buf;
  ret2_ = uim_scm_null();
  for (i = 0; i < nbytes; i++) {
    ret2_ = CONS(MAKE_INT(*p), ret2_);
    p++;
  }
  free(buf);
  return CONS(ret1_, uim_scm_callf("reverse", "o", ret2_));
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

static uim_lisp
c_make_addrinfo()
{
  struct addrinfo *addrinfo = uim_malloc(sizeof(struct addrinfo));

  memset(addrinfo, 0, sizeof(struct addrinfo));
  return MAKE_PTR(addrinfo);
}

static uim_lisp
c_delete_addrinfo(uim_lisp addrinfo_)
{
  free(C_PTR(addrinfo_));
  return uim_scm_t();
}

const static opt_args ai_flags[] = {
  { AI_CANONNAME,   "$AI_CANONNAME" },
  { AI_NUMERICHOST, "$AI_NUMERICHOST" },
  { AI_PASSIVE,     "$AI_PASSIVE" },
  { 0, 0 }
};
static uim_lisp
c_addrinfo_set_ai_flags(uim_lisp addrinfo_, uim_lisp ai_flags_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);
  int flags = 0;
  const char *errstr;

  while (!NULLP(ai_flags_)) {
    char *f = C_SYM(CAR(ai_flags_));

    flags |= find_args_equal(ai_flags, f, &errstr);
    free(f);
    if (errstr) {
      uim_notify_fatal("addrinfo-set-ai-flags!: %s", errstr);
      ERROR_OBJ(errstr, CAR(ai_flags_));
    }
    ai_flags_ = CDR(ai_flags_);
  }

  addrinfo->ai_flags = flags;
  return uim_scm_t();
}
static uim_lisp
c_addrinfo_ref_ai_flags(uim_lisp addrinfo_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  return ref_args_or(ai_flags, addrinfo->ai_flags);
}

const static opt_args ai_family[] = {
  { PF_UNSPEC, "$PF_UNSPEC" },
  { PF_LOCAL,  "$PF_LOCAL" },
  { PF_INET,   "$PF_INET" },
  { PF_INET6,  "$PF_INET6" },
  { 0, 0 }
};

static uim_lisp
c_addrinfo_set_ai_family(uim_lisp addrinfo_, uim_lisp ai_family_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);
  char *f = C_SYM(ai_family_);
  int family;
  const char *errstr;

  family = find_args_equal(ai_family, f, &errstr);
  free(f);
  if (errstr) {
    uim_notify_fatal("addrinfo-set-ai-family!: %s", errstr);
    ERROR_OBJ(errstr, ai_family_);
  }
  addrinfo->ai_family = family;
  return uim_scm_t();
}
static uim_lisp
c_addrinfo_ref_ai_family(uim_lisp addrinfo_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);
  const char *family;
  const char *errstr;

  family = find_flags_equal(ai_family, addrinfo->ai_family, &errstr);
  if (errstr) {
    uim_notify_fatal("addrinfo-ai-family?: %s", errstr);
    ERROR_OBJ(errstr, addrinfo_);
  }
  return MAKE_SYM(family);
}

const static opt_args ai_socktype[] = {
  { SOCK_STREAM, "$SOCK_STREAM" },
  { SOCK_DGRAM,  "$SOCK_DGRAM" },
  { SOCK_RAW,    "$SOCK_RAW" },
  { 0, 0 }
};

static uim_lisp
c_addrinfo_set_ai_socktype(uim_lisp addrinfo_, uim_lisp ai_socktype_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);
  char *f = C_SYM(ai_socktype_);
  int socktype;
  const char *errstr;

  socktype = find_args_equal(ai_socktype, f, &errstr);
  free(f);
  if (errstr) {
    uim_notify_fatal("addrinfo-set-ai-socktype!: %s", errstr);
    ERROR_OBJ(errstr, ai_socktype_);
  }
  addrinfo->ai_socktype = socktype;
  return uim_scm_t();
}
static uim_lisp
c_addrinfo_ref_ai_socktype(uim_lisp addrinfo_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);
  const char *socktype;
  const char *errstr;

  socktype = find_flags_equal(ai_socktype, addrinfo->ai_socktype, &errstr);
  if (errstr) {
    uim_notify_fatal("addrinfo-ai-socktype?: %s", errstr);
    ERROR_OBJ(errstr, addrinfo_);
  }
  return MAKE_SYM(socktype);
}

const static opt_args ai_protocol[] = {
  { IPPROTO_UDP, "$IPPROTO_UDP" },
  { IPPROTO_TCP, "$IPPROTO_TCP" },
  { 0, 0 }
};

static uim_lisp
c_addrinfo_set_ai_protocol(uim_lisp addrinfo_, uim_lisp ai_protocol_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);
  char *f = C_SYM(ai_protocol_);
  int protocol;
  const char *errstr;

  protocol = find_args_equal(ai_protocol, f, &errstr);
  free(f);
  if (errstr) {
    uim_notify_fatal("addrinfo-set-ai-protocol!: %s", errstr);
    ERROR_OBJ(errstr, ai_protocol_);
  }
  addrinfo->ai_protocol = protocol;
  return uim_scm_t();
}
static uim_lisp
c_addrinfo_ref_ai_protocol(uim_lisp addrinfo_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);
  const char *protocol;
  const char *errstr;

  protocol = find_flags_equal(ai_protocol, addrinfo->ai_protocol, &errstr);
  if (errstr) {
    uim_notify_fatal("addrinfo-ai-protocol?: %s", errstr);
    ERROR_OBJ(errstr, addrinfo_);
  }
  return MAKE_SYM(protocol);
}

static uim_lisp
c_addrinfo_ref_ai_addrlen(uim_lisp addrinfo_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  return MAKE_INT(addrinfo->ai_addrlen);
}

static uim_lisp
c_addrinfo_ref_ai_addr(uim_lisp addrinfo_)
{
  struct addrinfo *addrinfo = C_PTR(addrinfo_);

  return MAKE_PTR(addrinfo->ai_addr);
}

static uim_lisp
c_getaddrinfo(uim_lisp hostname_, uim_lisp servname_, uim_lisp hint_)
{
  const char *hostname = REFER_C_STR(hostname_);
  char *servname = NULL;
  struct addrinfo *hints = C_PTR(hint_);
  struct addrinfo *res, *res0;
  uim_lisp ret_ = uim_scm_null();
  int error;

  if (INTP(servname_)) {
    uim_asprintf(&servname, "%d", C_INT(servname_));
  } else {
    servname = C_STR(servname_);
  }

  error = getaddrinfo(hostname, servname, hints, &res0);
  if (error) {
    const char *errstr = gai_strerror(error);

    free(servname);
    uim_notify_fatal("getaddrinfo: %s", gai_strerror(error));
    ERROR_OBJ(errstr, CONS(hostname_, servname_));
  }

  free(servname);
  for (res = res0; res; res = res->ai_next) {
    ret_ = CONS(MAKE_PTR(res) , ret_);
  }
  return uim_scm_callf("reverse", "o", ret_);
}


static uim_lisp
c_freeaddrinfo(uim_lisp addrinfo_)
{
  freeaddrinfo(C_PTR(addrinfo_));
  return uim_scm_t();
}

static uim_lisp
c_socket(uim_lisp domain_, uim_lisp type_, uim_lisp protocol_)
{
  int domain;
  int type;
  int protocol;
  char *sym;
  const char *errstr;

  sym = C_SYM(domain_);
  domain = find_args_equal(ai_family, sym, &errstr);
  free(sym);
  if (errstr) {
    uim_notify_fatal("socket: %s", errstr);
    ERROR_OBJ(errstr, domain_);
  }
  sym = C_SYM(type_);
  type = find_args_equal(ai_socktype, sym, &errstr);
  free(sym);
  if (errstr) {
    uim_notify_fatal("socket: %s", errstr);
    ERROR_OBJ(errstr, type_);
  }
  sym = C_SYM(protocol_);
  protocol = find_args_equal(ai_protocol, sym, &errstr);
  free(sym);
  if (errstr) {
    uim_notify_fatal("socket: %s", errstr);
    ERROR_OBJ(errstr, protocol_);
  }

  return MAKE_INT(socket(domain, type, protocol));
}

static uim_lisp
c_connect(uim_lisp s_, uim_lisp name_, uim_lisp namelen_)
{
  return MAKE_INT(connect(C_INT(s_), C_PTR(name_), C_INT(namelen_)));
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
  uim_scm_init_proc1("file-close", c_file_close);
  uim_scm_init_proc2("file-read", c_file_read);
  uim_scm_init_proc2("file-write", c_file_write);

  uim_scm_gc_protect(&uim_lisp_open_flags);
  uim_scm_gc_protect(&uim_lisp_open_mode);
  uim_lisp_open_flags = make_arg_list(open_flags);
  uim_lisp_open_mode = make_arg_list(open_mode);

  uim_scm_init_proc0("file-open-flags?", c_file_open_flags);
  uim_scm_init_proc0("file-open-mode?", c_file_open_mode);

  uim_scm_init_proc0("make-addrinfo", c_make_addrinfo);
  uim_scm_init_proc1("delete-addrinfo", c_delete_addrinfo);

  uim_scm_init_proc2("addrinfo-set-ai-flags!", c_addrinfo_set_ai_flags);
  uim_scm_init_proc1("addrinfo-ai-flags?", c_addrinfo_ref_ai_flags);
  uim_scm_init_proc2("addrinfo-set-ai-family!", c_addrinfo_set_ai_family);
  uim_scm_init_proc1("addrinfo-ai-family?", c_addrinfo_ref_ai_family);
  uim_scm_init_proc2("addrinfo-set-ai-socktype!", c_addrinfo_set_ai_socktype);
  uim_scm_init_proc1("addrinfo-ai-socktype?", c_addrinfo_ref_ai_socktype);
  uim_scm_init_proc2("addrinfo-set-ai-protocol!", c_addrinfo_set_ai_protocol);
  uim_scm_init_proc1("addrinfo-ai-protocol?", c_addrinfo_ref_ai_protocol);
  uim_scm_init_proc1("addrinfo-ai-addrlen?", c_addrinfo_ref_ai_addrlen);
  uim_scm_init_proc1("addrinfo-ai-addr?", c_addrinfo_ref_ai_addr);

  uim_scm_init_proc3("getaddrinfo", c_getaddrinfo);
  uim_scm_init_proc1("freeaddrinfo", c_freeaddrinfo);
  uim_scm_init_proc3("socket", c_socket);
  uim_scm_init_proc3("connect", c_connect);
}
