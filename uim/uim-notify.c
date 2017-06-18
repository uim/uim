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
#include <string.h>
#include <stdarg.h>
#include <dlfcn.h>
#include <dirent.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <pwd.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-internal.h"
#include "uim-notify.h"
#include "gettext.h"

#define NOTIFY_PLUGIN_PATH PKGLIBDIR "/notify"
#define NOTIFY_PLUGIN_PREFIX "libuimnotify-"
#define NOTIFY_PLUGIN_SUFFIX ".so"

#ifndef HAVE_DLFUNC
typedef void (*my_dlfunc_t)(void);
#define dlfunc(handle, symbol) \
  ((my_dlfunc_t)(uintptr_t)dlsym((handle), (symbol)))
#else
typedef dlfunc_t my_dlfunc_t;
#endif

struct uim_notify_agent {
  const uim_notify_desc *(*desc)(void);
  int (*init)(void);
  void (*quit)(void);
  int (*notify_info)(const char *);
  int (*notify_fatal)(const char *);
};

static my_dlfunc_t load_func(const char *path, const char *name);

/* FIXME: Move these decls to the 'stderr' agent section and make
 * invisible from other part of uim-notify.  -- YamaKen 2008-01-30 */
static void uim_notify_load_stderr(void);
static const uim_notify_desc *uim_notify_stderr_get_desc(void);


static struct uim_notify_agent agent_body;
static struct uim_notify_agent *agent = &agent_body;
static void *notify_dlhandle = NULL;


static my_dlfunc_t
load_func(const char *path, const char *name)
{
  my_dlfunc_t f;

  f = (my_dlfunc_t)dlfunc(notify_dlhandle, name);
  if (!f) {
    fprintf(stderr, "uim-notify: cannot found '%s()' in %s\n", name, path);
    dlclose(notify_dlhandle);
    uim_notify_load_stderr();
    return NULL;
  }
  return f;
}

uim_bool
uim_notify_load(const char *name)
{
  if (!agent->quit || !agent->desc) {
    fprintf(stderr, "uim-notify: notification agent module is not loaded\n");
    uim_notify_load_stderr();
    return UIM_FALSE;
  }

  if (getenv("UIM_DISABLE_NOTIFY") != NULL || strcmp(agent->desc()->name, name) == 0) {
    return UIM_TRUE;
  } else if (strcmp(name, "stderr") == 0) {
    agent->quit();
    if (notify_dlhandle)
      dlclose(notify_dlhandle);
    uim_notify_load_stderr();
  } else {
    char path[PATH_MAX];
    const char *str;

    agent->quit();
    if (notify_dlhandle)
      dlclose(notify_dlhandle);

    snprintf(path, sizeof(path), "%s/%s%s%s", NOTIFY_PLUGIN_PATH,
	     NOTIFY_PLUGIN_PREFIX, name, NOTIFY_PLUGIN_SUFFIX);

    notify_dlhandle = dlopen(path, RTLD_NOW);
    if ((str = dlerror())) {
      fprintf(stderr, "uim-notify: load failed %s(%s)\n", path, str);
      uim_notify_load_stderr();
      return UIM_FALSE;
    }
    agent->desc = (const uim_notify_desc *(*)(void))load_func(path, "uim_notify_plugin_get_desc");
    if (!agent->desc)
      return UIM_FALSE;

    agent->init = (int (*)(void))load_func(path, "uim_notify_plugin_init");
    if (!agent->init)
      return UIM_FALSE;

    agent->quit = (void (*)(void))load_func(path, "uim_notify_plugin_quit");
    if (!agent->quit)
      return UIM_FALSE;

    agent->notify_info = (int (*)(const char *))load_func(path, "uim_notify_plugin_info");
    if (!agent->notify_info)
      return UIM_FALSE;

    agent->notify_fatal = (int (*)(const char *))load_func(path, "uim_notify_plugin_fatal");
    if (!agent->notify_fatal)
      return UIM_FALSE;

    agent->init();
  }
  return UIM_TRUE;
}

const uim_notify_desc *
uim_notify_get_desc(void)
{
  return agent->desc();
}

uim_bool
uim_notify_init(void)
{
  /* Since a cyclic init/quit sequence leaves *agent uncleared,
   * explicit initialization is required. Such data initialization is
   * needed for some embedded platforms such as Qtopia.
   *   -- YamaKen 2008-01-30 */
  uim_notify_load_stderr();

  return agent->init();
}

void
uim_notify_quit(void)
{
  agent->quit();
}

uim_bool
uim_notify_info(const char *msg_fmt, ...)
{
  va_list ap;
  char msg[BUFSIZ];

  va_start(ap, msg_fmt);
  vsnprintf(msg, sizeof(msg), msg_fmt, ap);
  va_end(ap);

  return agent->notify_info(msg);
}

uim_bool
uim_notify_fatal(const char *msg_fmt, ...)
{
  va_list ap;
  char msg[BUFSIZ];

  va_start(ap, msg_fmt);
  vsnprintf(msg, sizeof(msg), msg_fmt, ap);
  va_end(ap);

  return agent->notify_fatal(msg);
}

/* Low stack-consumption version of uim_notify_fatal(). */
uim_bool
uim_notify_fatal_raw(const char *msg)
{
  return agent->notify_fatal(msg);
}

/*
 * Scheme interfaces
 */
static uim_lisp
notify_get_plugins_internal(void)
{
  uim_lisp ret_;
  DIR *dirp;
  struct dirent *dp;
  size_t plen, slen;
  const uim_notify_desc *desc;
  void *handle;
  uim_notify_desc *(*desc_func)(void);
  const char *str;

  plen = sizeof(NOTIFY_PLUGIN_PREFIX);
  slen = sizeof(NOTIFY_PLUGIN_SUFFIX);

  desc = uim_notify_stderr_get_desc();
  ret_ = CONS(LIST3(MAKE_SYM(desc->name),
		    MAKE_STR(desc->name),
		    MAKE_STR(desc->desc)),
	      uim_scm_null());

  if (getenv("UIM_DISABLE_NOTIFY") != NULL)
    return uim_scm_callf("reverse", "o", ret_);

  dirp = opendir(NOTIFY_PLUGIN_PATH);
  if (dirp) {
    while ((dp = readdir(dirp)) != NULL) {
      size_t len = strlen(dp->d_name);
      char path[PATH_MAX];
      if ((len < plen + slen - 1) ||
	  (PATH_MAX < (sizeof(NOTIFY_PLUGIN_PATH "/") + len)) ||
	  (strcmp(dp->d_name, NOTIFY_PLUGIN_PREFIX) <= 0) ||
	  (strcmp(dp->d_name + len + 1 - slen, NOTIFY_PLUGIN_SUFFIX) != 0))
	continue;

      snprintf(path, sizeof(path), "%s/%s", NOTIFY_PLUGIN_PATH, dp->d_name);
      handle = dlopen(path, RTLD_NOW);
      if ((str = dlerror()) != NULL) {
	fprintf(stderr, "load failed %s(%s)\n", path, str);
	continue;
      }
      desc_func = (uim_notify_desc *(*)(void))dlfunc(handle, "uim_notify_plugin_get_desc");
      if (!desc_func) {
	fprintf(stderr, "cannot found 'uim_notify_get_desc()' in %s\n", path);
	dlclose(handle);
	continue;
      }

      desc = desc_func();

      ret_ = CONS(LIST3(MAKE_SYM(desc->name),
			MAKE_STR(desc->name),
			MAKE_STR(desc->desc)),
		  ret_);

      dlclose(handle);
    }
    (void)closedir(dirp);
  }
  return uim_scm_callf("reverse", "o", ret_);
}

static uim_lisp
notify_get_plugins(void)
{
  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)notify_get_plugins_internal,
						    NULL);
}

static uim_lisp
notify_load(uim_lisp name_)
{
  const char *name = REFER_C_STR(name_);

  return MAKE_BOOL(uim_notify_load(name));
}

static uim_lisp
notify_info(uim_lisp msg_)
{
  const char *msg = REFER_C_STR(msg_);

  return MAKE_BOOL(uim_notify_info("%s", msg));
}

static uim_lisp
notify_fatal(uim_lisp msg_)
{
  const char *msg = REFER_C_STR(msg_);

  return MAKE_BOOL(uim_notify_fatal("%s", msg));
}

void
uim_init_notify_subrs(void)
{
  uim_scm_init_proc0("uim-notify-get-plugins", notify_get_plugins);
  uim_scm_init_proc1("uim-notify-load", notify_load);
  uim_scm_init_proc1("uim-notify-info", notify_info);
  uim_scm_init_proc1("uim-notify-fatal", notify_fatal);
}


/*
 * builtin 'stderr' notification agent
 */
static uim_bool uim_notify_stderr_init(void);
static void uim_notify_stderr_quit(void);
static uim_bool uim_notify_stderr_info(const char *);
static uim_bool uim_notify_stderr_fatal(const char *);

static const uim_notify_desc uim_notify_stderr_desc = {
  "stderr",
  "Standard Error output",
};

static void
uim_notify_load_stderr(void)
{
  agent->desc = uim_notify_stderr_get_desc;
  agent->init = uim_notify_stderr_init;
  agent->quit = uim_notify_stderr_quit;
  agent->notify_info = uim_notify_stderr_info;
  agent->notify_fatal = uim_notify_stderr_fatal;
  notify_dlhandle = NULL;
}

static const uim_notify_desc *
uim_notify_stderr_get_desc(void)
{
  return &uim_notify_stderr_desc;
}

static uim_bool
uim_notify_stderr_init(void)
{
  return UIM_TRUE;
}

static void
uim_notify_stderr_quit(void)
{
  return;
}

static uim_bool
uim_notify_stderr_info(const char *msg)
{
  fputs("libuim: [info] ", stderr);
  fputs(dgettext(GETTEXT_PACKAGE, msg), stderr);
  fputs("\n", stderr);

  return UIM_TRUE;
}

static uim_bool
uim_notify_stderr_fatal(const char *msg)
{
  /* To reduce stack consumption on hard situations such as memory
   * exhaustion, printf()s with indirect directives are intentionally
   * avoided here.  -- YamaKen 2008-02-11 */
  fputs("libuim: [fatal] ", stderr);
  fputs(dgettext(GETTEXT_PACKAGE, msg), stderr);
  fputs("\n", stderr);

  return UIM_TRUE;
}
