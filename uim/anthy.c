/*

  Copyright (c) 2003,2004,2005 uim Project http://uim.freedesktop.org/

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
/**/
#include <dlfcn.h>
#include <stdlib.h>
#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "uimint.h"
#include "plugin.h"

#define MAX_CONTEXT 256

#ifdef __APPLE__
  #define LIBANTHY_SO    "libanthy.dylib"
  #define LIBANTHYDIC_SO "libanthydic.dylib"
#else
  #define LIBANTHY_SO    "libanthy.so"
  #define LIBANTHYDIC_SO "libanthydic.so"

#endif /*__APPLE__*/

struct anthy_context;

struct anthy_conv_stat{
  int nr_segment;
};

struct anthy_segment_stat{
  int nr_candidate;
  int seg_len;
};

static struct anthy_api {
  void *diclib, *lib;

  int (*init)(void);
  int (*quit)(void);
  struct anthy_context *(*create_context)(void);
  void (*release_context)(struct anthy_context *);
  void (*set_string)(struct anthy_context *, char *);
  void (*get_stat)(struct anthy_context *, struct anthy_conv_stat *);
  void (*get_segment_stat)(struct anthy_context *, int,
			   struct anthy_segment_stat *);
  int (*get_segment)(struct anthy_context *, int, int, char *, int);
  void (*resize_segment)(struct anthy_context *, int, int);
  void (*commit_segment)(struct anthy_context *, int, int);
} api;

static struct context {
  struct anthy_context *ac;
} *context_slot;

static int
get_anthy_api(void)
{
  api.diclib = dlopen(LIBANTHYDIC_SO, RTLD_GLOBAL |RTLD_NOW);
  if (!api.diclib) {
	/*   fprintf (stdout, "%s\n", dlerror());*/
    return -1;
  }
  api.lib = dlopen(LIBANTHY_SO, RTLD_NOW);
  if (!api.lib) {
	/*   fprintf (stdout, "%s\n", dlerror()); */
    dlclose(api.diclib);
    return -1;
  }
  *(int  **)(&api.init) = dlsym(api.lib, "anthy_init");
  *(int  **)(&api.quit) = dlsym(api.lib, "anthy_quit");
  *(struct anthy_context **)(&api.create_context) = dlsym(api.lib, "anthy_create_context");
  *(void **)(&api.release_context) = dlsym(api.lib, "anthy_release_context");
  *(void **)(&api.set_string) = dlsym(api.lib, "anthy_set_string");
  *(void **)(&api.get_stat) = dlsym(api.lib, "anthy_get_stat");
  *(void **)(&api.get_segment_stat) = dlsym(api.lib, "anthy_get_segment_stat");
  *(int  **)(&api.get_segment) = dlsym(api.lib, "anthy_get_segment");
  *(void **)(&api.resize_segment) = dlsym(api.lib, "anthy_resize_segment");
  *(void **)(&api.commit_segment) = dlsym(api.lib, "anthy_commit_segment");
  if (api.init &&
      api.quit &&
      api.create_context &&
      api.release_context && api.set_string &&
      api.get_stat && api.get_segment_stat &&
      api.get_segment && api.resize_segment && api.commit_segment) {
    return 0;
  }
  return -1;
}

static struct anthy_context *
get_anthy_context(int id)
{
  if (id < 0 || id >= MAX_CONTEXT) {
    return NULL;
  }
  return context_slot[id].ac;
}

static uim_lisp
init_anthy_lib(void)
{
  int i;
  if (context_slot) {
    return uim_scm_t();
  }
  if (get_anthy_api() == -1) {
    return uim_scm_f();
  }
  if (api.init() == -1) {
    return uim_scm_f();
  }
  context_slot = malloc(sizeof(struct context) *
			MAX_CONTEXT);
  if (!context_slot) {
    return uim_scm_f();
  }
  for (i = 0; i < MAX_CONTEXT; i++) {
    context_slot[i].ac = NULL;
  }
  return uim_scm_t();
}

static uim_lisp
create_context(void)
{
  int i;
  if (!context_slot) {
    return uim_scm_f();
  }

  for (i = 0; i < MAX_CONTEXT; i++) {
    if (!context_slot[i].ac) {
      struct anthy_context *ac = api.create_context();
      if (!ac) {
	return uim_scm_f();
      }
      context_slot[i].ac = ac;
      return uim_scm_make_int(i);
    }
  }
  return uim_scm_f();
}


static uim_lisp
release_context(uim_lisp id_)
{
  int id = uim_scm_c_int(id_);
  if (context_slot[id].ac) {
    api.release_context(context_slot[id].ac);
    context_slot[id].ac = NULL;
  }
  return uim_scm_f();
}

static uim_lisp
set_string(uim_lisp id_, uim_lisp str_)
{
  int id = uim_scm_c_int(id_);
  char *str;
  struct anthy_context *ac = get_anthy_context(id);
  if (!ac) {
    return uim_scm_f();
  }
  str = uim_scm_c_str(str_);
  api.set_string(ac, str);
  free(str);
  return uim_scm_f();
}

static uim_lisp
get_nr_segments(uim_lisp id_)
{
  int id = uim_scm_c_int(id_);
  struct anthy_conv_stat acs;
  struct anthy_context *ac = get_anthy_context(id);
  if (!ac) {
    return uim_scm_f();
  }
  api.get_stat(ac, &acs);

  return uim_scm_make_int(acs.nr_segment);
}

static uim_lisp
get_nr_candidates(uim_lisp id_, uim_lisp nth_)
{
  int id, nth;
  struct anthy_context *ac;
  struct anthy_conv_stat cs;
  id = uim_scm_c_int(id_);
  nth = uim_scm_c_int(nth_);
  ac = get_anthy_context(id);
  if (!ac) {
    return uim_scm_f();
  }
  api.get_stat(ac, &cs);
  if (nth < cs.nr_segment) {
    struct anthy_segment_stat ss;
    api.get_segment_stat(ac, nth, &ss);
    return uim_scm_make_int(ss.nr_candidate);
  }
  return uim_scm_f();
}

static uim_lisp
get_nth_candidate(uim_lisp id_, uim_lisp seg_, uim_lisp nth_)
{
  int id = uim_scm_c_int(id_);
  int seg = uim_scm_c_int(seg_);
  int nth  = uim_scm_c_int(nth_);
  int buflen;
  char *buf;
  uim_lisp buf_;
  struct anthy_context *ac = get_anthy_context(id);
  if (!ac) {
    return uim_scm_f();
  }
  buflen = api.get_segment(ac, seg, nth, NULL, 0);
  if (buflen == -1) {
    return uim_scm_f();
  }
  buf = malloc(buflen+1);
  api.get_segment(ac, seg, nth, buf, buflen+1);
  buf_ = uim_scm_make_str(buf);
  free(buf);
  return buf_;
}

static uim_lisp
get_segment_length(uim_lisp id_, uim_lisp nth_)
{
  int id, nth;
  struct anthy_context *ac;
  struct anthy_conv_stat cs;
  id = uim_scm_c_int(id_);
  nth = uim_scm_c_int(nth_);
  ac = get_anthy_context(id);
  if (!ac) {
    return uim_scm_f();
  }
  api.get_stat(ac, &cs);
  if (nth < cs.nr_segment) {
    struct anthy_segment_stat ss;
    api.get_segment_stat(ac, nth, &ss);
    return uim_scm_make_int(ss.seg_len);
  }
  return uim_scm_f();
}

static uim_lisp
resize_segment(uim_lisp id_, uim_lisp seg_, uim_lisp cnt_)
{
  int id = uim_scm_c_int(id_);
  int seg = uim_scm_c_int(seg_);
  int cnt = uim_scm_c_int(cnt_);
  struct anthy_context *ac = get_anthy_context(id);
  api.resize_segment(ac, seg, cnt);
  return uim_scm_f();
}

static uim_lisp
commit_segment(uim_lisp id_, uim_lisp s_, uim_lisp nth_)
{
  int id = uim_scm_c_int(id_);
  int s = uim_scm_c_int(s_);
  int nth = uim_scm_c_int(nth_);
  struct anthy_context *ac = get_anthy_context(id);
  api.commit_segment(ac, s, nth);
  return uim_scm_f();
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_subr_0("anthy-lib-init", init_anthy_lib);
  uim_scm_init_subr_0("anthy-lib-alloc-context", create_context);
  uim_scm_init_subr_1("anthy-lib-free-context", release_context);
  uim_scm_init_subr_2("anthy-lib-set-string", set_string);
  uim_scm_init_subr_1("anthy-lib-get-nr-segments",get_nr_segments);
  uim_scm_init_subr_2("anthy-lib-get-nr-candidates", get_nr_candidates);
  uim_scm_init_subr_3("anthy-lib-get-nth-candidate", get_nth_candidate);
  uim_scm_init_subr_2("anthy-lib-get-segment-length", get_segment_length);
  uim_scm_init_subr_3("anthy-lib-resize-segment", resize_segment);
  uim_scm_init_subr_3("anthy-lib-commit-segment", commit_segment);
}

void
uim_plugin_instance_quit(void)
{
  int i;

  if (!context_slot) {
    return;
  }

  for (i = 0; i < MAX_CONTEXT; i++) {
    if (context_slot[i].ac) {
      api.release_context(context_slot[i].ac);
    }
  }

  if (api.quit) {
    api.quit();
  }
  if (api.diclib) {
    dlclose(api.diclib);
  }
  if (api.lib) {
    dlclose(api.lib);
  }
  if (context_slot) {
    free(context_slot);
    context_slot = NULL;
  }
}
