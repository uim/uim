/*

  Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
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
#include "context.h"

#define MAX_CONTEXT 256

#ifdef __APPLE__
  #define LIBANTHY_SO    "/sw/lib/libanthy.dylib"
  #define LIBANTHYDIC_SO "/sw/lib/libanthydic.dylib"
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

static LISP
init_anthy_lib(void)
{
  int i;
  if (context_slot) {
    return siod_true_value();
  }
  if (get_anthy_api() == -1) {
    return siod_false_value();
  }
  if (api.init() == -1) {
    return siod_false_value();
  }
  context_slot = malloc(sizeof(struct context) *
			MAX_CONTEXT);
  if(!context_slot) {
    return siod_false_value();
  }
  for (i = 0; i < MAX_CONTEXT; i++) {
    context_slot[i].ac = NULL;
  }
  return siod_true_value();
}

static LISP
create_context(void)
{
  int i;
  if (!context_slot) {
    return siod_false_value();
  }

  for (i = 0; i < MAX_CONTEXT; i++) {
    if (!context_slot[i].ac) {
      struct anthy_context *ac = api.create_context();
      if (!ac) {
	return siod_false_value();
      }
      context_slot[i].ac = ac;
      return intcons(i);
    }
  }
  return siod_false_value();
}


static LISP
release_context(LISP id_)
{
  int id = get_c_int(id_);
  if (context_slot[id].ac) {
    api.release_context(context_slot[id].ac);
    context_slot[id].ac = NULL;
  }
  return siod_false_value();
}

static LISP
set_string(LISP id_, LISP str_)
{
  int id = get_c_int(id_);
  char *str;
  struct anthy_context *ac = get_anthy_context(id);
  if (!ac) {
    return siod_false_value();
  }
  str = uim_get_c_string(str_);
  api.set_string(ac, str);
  free(str);
  return siod_false_value();
}

static LISP
get_nr_segments(LISP id_)
{
  int id = get_c_int(id_);
  struct anthy_conv_stat acs;
  struct anthy_context *ac = get_anthy_context(id);
  if (!ac) {
    return siod_false_value();
  }
  api.get_stat(ac, &acs);

  return intcons(acs.nr_segment);
}

static LISP
get_nr_candidates(LISP id_, LISP nth_)
{
  int id, nth;
  struct anthy_context *ac;
  struct anthy_conv_stat cs;
  id = get_c_int(id_);
  nth = get_c_int(nth_);
  ac = get_anthy_context(id);
  if (!ac) {
    return siod_false_value();
  }
  api.get_stat(ac, &cs);
  if (nth < cs.nr_segment) {
    struct anthy_segment_stat ss;
    api.get_segment_stat(ac, nth, &ss);
    return intcons(ss.nr_candidate);
  }
  return siod_false_value();
}

static LISP
get_nth_candidate(LISP id_, LISP seg_, LISP nth_)
{
  int id = get_c_int(id_);
  int seg = get_c_int(seg_);
  int nth  = get_c_int(nth_);
  int buflen;
  char *buf;
  LISP buf_;
  struct anthy_context *ac = get_anthy_context(id);
  if (!ac) {
    return siod_false_value();
  }
  buflen = api.get_segment(ac, seg, nth, NULL, 0);
  if (buflen == -1) {
    return siod_false_value();
  }
  buf = malloc(buflen+1);
  api.get_segment(ac, seg, nth, buf, buflen+1);
  buf_ = strcons(buflen, buf);
  free(buf);
  return buf_;
}

static LISP
resize_segment(LISP id_, LISP seg_, LISP cnt_)
{
  int id = get_c_int(id_);
  int seg = get_c_int(seg_);
  int cnt = get_c_int(cnt_);
  struct anthy_context *ac = get_anthy_context(id);
  api.resize_segment(ac, seg, cnt);
  return siod_false_value();
}

static LISP
commit_segment(LISP id_, LISP s_, LISP nth_)
{
  int id = get_c_int(id_);
  int s = get_c_int(s_);
  int nth = get_c_int(nth_);
  struct anthy_context *ac = get_anthy_context(id);
  api.commit_segment(ac, s, nth);
  return siod_false_value();
}

void
uim_init_anthy(void)
{
  init_subr_0("anthy-lib-init", init_anthy_lib);

  init_subr_0("anthy-lib-alloc-context", create_context);
  init_subr_1("anthy-lib-free-context", release_context);

  init_subr_2("anthy-lib-set-string", set_string);
  init_subr_1("anthy-lib-get-nr-segments",get_nr_segments);
  init_subr_2("anthy-lib-get-nr-candidates", get_nr_candidates);
  init_subr_3("anthy-lib-get-nth-candidate", get_nth_candidate);
  init_subr_3("anthy-lib-resize-segment", resize_segment);
  init_subr_3("anthy-lib-commit-segment", commit_segment);
}

void
uim_quit_anthy(void)
{
  int i;

  if (!context_slot) {
    return;
  }

  for (i = 0; i < MAX_CONTEXT; i++) {
    if(context_slot[i].ac) {
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
