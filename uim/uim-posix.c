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

#include "uim.h"
#include "uim-internal.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-posix.h"
#include "uim-notify.h"
#include "gettext.h"
#include "dynlib.h"

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

static uim_bool
uim_check_dir_internal(const char *dir, int need_prepare)
{
  struct stat st;

  if (stat(dir, &st) < 0)
    if (need_prepare)	  
      return (mkdir(dir, 0700) < 0) ? UIM_FALSE : UIM_TRUE;
    else
      return UIM_FALSE;
  else {
    mode_t mode = S_IFDIR | S_IRUSR | S_IWUSR | S_IXUSR;
    return ((st.st_mode & mode) == mode) ? UIM_TRUE : UIM_FALSE;
  }
}

/* FIXME: use appropriate name for this API */
uim_bool
uim_check_dir(const char *dir)
{
  int need_prepare = UIM_TRUE;

  return uim_check_dir_internal(dir, need_prepare);
}

static uim_lisp
c_prepare_dir(uim_lisp dir_)
{
  if (!uim_check_dir(REFER_C_STR(dir_))) {
    return uim_scm_f();
  }
  return uim_scm_t();
}

static uim_bool
uim_get_config_path_internal(char *path, int len, int is_getenv, int need_prepare)
{
  char home[MAXPATHLEN];

  if (len <= 0)
    return UIM_FALSE;

  if (!uim_get_home_directory(home, sizeof(home), getuid()) && is_getenv) {
    char *home_env = getenv("HOME");

    if (!home_env) {
      path[0] = '\0';
      return UIM_FALSE;
    }

    if (strlcpy(home, home_env, sizeof(home)) >= sizeof(home)) {
      path[0] = '\0';
      return UIM_FALSE;
    }
  }

  if (snprintf(path, len, "%s/.uim.d", home) < 0) {
    path[0] = '\0';
    return UIM_FALSE;
  }

  if (!uim_check_dir_internal(path, need_prepare)) {
    return UIM_FALSE;
  }

  return UIM_TRUE;
}

/* FIXME: use appropriate name for this API */
uim_bool
uim_get_config_path(char *path, int len, int is_getenv)
{
  int need_prepare = UIM_TRUE;

  return uim_get_config_path_internal(path, len, is_getenv, need_prepare);
}

static uim_lisp
c_prepare_config_path(uim_lisp is_getenv_)
{
  char path[MAXPATHLEN];
  int need_prepare = UIM_TRUE;

  if (!uim_get_config_path_internal(path, sizeof(path), C_BOOL(is_getenv_), need_prepare))
    return uim_scm_f();
  return MAKE_STR(path);
}

static uim_lisp
c_get_config_path(uim_lisp is_getenv_)
{
  char path[MAXPATHLEN];
  int need_prepare = UIM_FALSE;

  /* No need to check the existence of path in this function */
  uim_get_config_path_internal(path, sizeof(path), C_BOOL(is_getenv_), need_prepare);

  return MAKE_STR(path);
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
c_unlink(uim_lisp path_)
{
  return MAKE_INT(unlink(REFER_C_STR(path_)));
}

static uim_lisp
c_mkdir(uim_lisp path_, uim_lisp mode_)
{
  return MAKE_INT(mkdir(REFER_C_STR(path_), C_INT(mode_)));
}

static uim_lisp
c_chdir(uim_lisp path_)
{
  return MAKE_INT(chdir(REFER_C_STR(path_)));
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
c_getuid(void)
{
  return MAKE_INT(getuid());
}

static uim_lisp
c_getgid(void)
{
  return MAKE_INT(getgid());
}

static uim_lisp
setugidp(void)
{
  assert(uim_scm_gc_any_contextp());

  return MAKE_BOOL(uim_issetugid());
}

static uim_lisp
c_setsid(void)
{
  return MAKE_INT(setsid());
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

static uim_lisp
c_error_string()
{
  return MAKE_STR(strerror(errno));
}

void
uim_init_posix_subrs(void)
{
  uim_scm_init_proc0("user-name", user_name);
  uim_scm_init_proc1("home-directory", home_directory);

  uim_scm_init_proc1("create/check-directory!", c_prepare_dir);
  uim_scm_init_proc1("get-config-path!", c_prepare_config_path);
  uim_scm_init_proc1("get-config-path", c_get_config_path);

  uim_scm_init_proc1("file-readable?", file_readablep);
  uim_scm_init_proc1("file-writable?", file_writablep);
  uim_scm_init_proc1("file-executable?", file_executablep);
  uim_scm_init_proc1("file-regular?", file_regularp);
  uim_scm_init_proc1("file-directory?", file_directoryp);
  uim_scm_init_proc1("file-mtime", file_mtime);

  uim_scm_init_proc1("unlink", c_unlink);
  uim_scm_init_proc2("mkdir", c_mkdir);
  uim_scm_init_proc1("chdir", c_chdir);

  uim_scm_init_proc0("getuid", c_getuid);
  uim_scm_init_proc0("getgid", c_getgid);
  uim_scm_init_proc0("setugid?", setugidp);

  uim_scm_init_proc0("setsid", c_setsid);

  uim_scm_init_proc1("getenv", c_getenv);
  uim_scm_init_proc3("setenv", c_setenv);
  uim_scm_init_proc1("unsetenv", c_unsetenv);

  uim_scm_init_proc0("time", c_time);
  uim_scm_init_proc2("difftime", c_difftime);

  uim_scm_init_proc1("sleep", c_sleep);

  uim_scm_init_proc0("posix-error-string", c_error_string);
}
