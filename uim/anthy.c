/*

  Copyright (c) 2003-2007 uim Project http://uim.freedesktop.org/

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
#include <config.h>

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <anthy/anthy.h>

#include "uim-scm.h"
#include "plugin.h"

#define MAX_CONTEXT 256

/* handle anthy's version scheme like 7100b, 8158memm */
static char *anthy_version_major;
static char *anthy_version_minor;

static struct context {
  anthy_context_t ac;
} *context_slot;

static anthy_context_t 
get_anthy_context(int id)
{
  if (id < 0 || id >= MAX_CONTEXT) {
    return NULL;
  }
  return context_slot[id].ac;
}

static uim_lisp
anthy_version()
{
  return uim_scm_cons(uim_scm_make_str(anthy_version_major),
		      uim_scm_make_str(anthy_version_minor));
}

static void
get_anthy_version()
{
  const char *str;

  free(anthy_version_major);
  free(anthy_version_minor);

  str = anthy_get_version_string();

  if (!str || (!strcmp(str, "(unknown)"))) {
    anthy_version_major = strdup("-1");
    anthy_version_minor = strdup("");
  } else {
    int len, i;

    len = strlen(str);
    for (i = 0; str[i] != '\0'; i++) {
      if (isalpha(str[i]))
        break;
    }

    if (i != len) {
      anthy_version_major = malloc(i + 1);
      anthy_version_minor = malloc(len - i + 1);
      strlcpy(anthy_version_major, str, i + 1);
      strlcpy(anthy_version_minor, &str[i], len - i + 1);
    } else {
      anthy_version_major = strdup(str);
      anthy_version_minor = strdup("");
    }
  }
}

static uim_lisp
init_anthy_lib(void)
{
  int i;

  if (context_slot)
    return uim_scm_t();

  if (anthy_init() == -1)
    return uim_scm_f();

  get_anthy_version();

  context_slot = malloc(sizeof(struct context) * MAX_CONTEXT);
  if (!context_slot)
    return uim_scm_f();

  for (i = 0; i < MAX_CONTEXT; i++)
    context_slot[i].ac = NULL;

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
      anthy_context_t ac = anthy_create_context();
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
    anthy_release_context(context_slot[id].ac);
    context_slot[id].ac = NULL;
  }
  return uim_scm_f();
}

static uim_lisp
set_string(uim_lisp id_, uim_lisp str_)
{
  int id = uim_scm_c_int(id_);
  char *str;
  anthy_context_t ac = get_anthy_context(id);
  if (!ac) {
    return uim_scm_f();
  }
  str = uim_scm_c_str(str_);
  anthy_set_string(ac, str);
  free(str);
  return uim_scm_f();
}

static uim_lisp
get_nr_segments(uim_lisp id_)
{
  int id = uim_scm_c_int(id_);
  struct anthy_conv_stat acs;
  anthy_context_t ac = get_anthy_context(id);
  if (!ac) {
    return uim_scm_f();
  }
  anthy_get_stat(ac, &acs);

  return uim_scm_make_int(acs.nr_segment);
}

static uim_lisp
get_nr_candidates(uim_lisp id_, uim_lisp nth_)
{
  int id, nth;
  anthy_context_t ac;
  struct anthy_conv_stat cs;
  id = uim_scm_c_int(id_);
  nth = uim_scm_c_int(nth_);
  ac = get_anthy_context(id);
  if (!ac) {
    return uim_scm_f();
  }
  anthy_get_stat(ac, &cs);
  if (nth < cs.nr_segment) {
    struct anthy_segment_stat ss;
    anthy_get_segment_stat(ac, nth, &ss);
    return uim_scm_make_int(ss.nr_candidate);
  }
  return uim_scm_f();
}

static uim_lisp
get_nth_candidate(uim_lisp id_, uim_lisp seg_, uim_lisp nth_)
{
  int id, seg, nth, buflen;
  char *buf;
  uim_lisp buf_;
  anthy_context_t ac;
  
  id = uim_scm_c_int(id_);
  seg = uim_scm_c_int(seg_);
  nth  = uim_scm_c_int(nth_);
  ac = get_anthy_context(id);

  if (!ac)
    return uim_scm_f();

  buflen = anthy_get_segment(ac, seg, nth, NULL, 0);
  if (buflen == -1)
    return uim_scm_f();

  buf = malloc(buflen + 1);
  anthy_get_segment(ac, seg, nth, buf, buflen + 1);
  buf_ = uim_scm_make_str(buf);
  free(buf);

  return buf_;
}

static uim_lisp
get_unconv_candidate(uim_lisp id_, uim_lisp seg_)
{
  return get_nth_candidate(id_, seg_, uim_scm_make_int(NTH_UNCONVERTED_CANDIDATE));
}

static uim_lisp
get_segment_length(uim_lisp id_, uim_lisp nth_)
{
  int id, nth;
  anthy_context_t ac;
  struct anthy_conv_stat cs;
  id = uim_scm_c_int(id_);
  nth = uim_scm_c_int(nth_);
  ac = get_anthy_context(id);
  if (!ac) {
    return uim_scm_f();
  }
  anthy_get_stat(ac, &cs);
  if (nth < cs.nr_segment) {
    struct anthy_segment_stat ss;
    anthy_get_segment_stat(ac, nth, &ss);
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
  anthy_context_t ac = get_anthy_context(id);
  anthy_resize_segment(ac, seg, cnt);
  return uim_scm_f();
}

static uim_lisp
commit_segment(uim_lisp id_, uim_lisp s_, uim_lisp nth_)
{
  int id = uim_scm_c_int(id_);
  int s = uim_scm_c_int(s_);
  int nth = uim_scm_c_int(nth_);
  anthy_context_t ac = get_anthy_context(id);
  anthy_commit_segment(ac, s, nth);
  return uim_scm_f();
}

static uim_lisp
set_prediction_src_string(uim_lisp id_, uim_lisp str_)
{
#ifdef HAS_ANTHY_PREDICTION
  int id = uim_scm_c_int(id_);
  const char *str = uim_scm_refer_c_str(str_);
  anthy_context_t ac = get_anthy_context(id);
  if (!ac) {
    return uim_scm_f();
  }
  anthy_set_prediction_string(ac, str);
  return uim_scm_f();
#else
  return uim_scm_f();
#endif
}

static uim_lisp
get_nr_predictions(uim_lisp id_)
{
#ifdef HAS_ANTHY_PREDICTION
  int id = uim_scm_c_int(id_);
  anthy_context_t ac = get_anthy_context(id);
  struct anthy_prediction_stat ps;
  if (!ac) {
    return uim_scm_f();
  }
  anthy_get_prediction_stat(ac, &ps);
  return uim_scm_make_int(ps.nr_prediction);
#else
  return uim_scm_f();
#endif
}

static uim_lisp
get_nth_prediction(uim_lisp id_, uim_lisp nth_)
{
#ifdef HAS_ANTHY_PREDICTION
  int id  = uim_scm_c_int(id_);
  int nth = uim_scm_c_int(nth_); 
  int buflen;
  char *buf;
  uim_lisp buf_;
  anthy_context_t ac = get_anthy_context(id);
  if (!ac) {
    return uim_scm_f();
  }
  buflen = anthy_get_prediction(ac, nth, NULL, 0);
  if (buflen == -1) {
    return uim_scm_f();
  }
  buf = (char *)malloc(buflen + 1);
  anthy_get_prediction(ac, nth, buf, buflen + 1);
  buf_ = uim_scm_make_str(buf);
  free(buf);
  return buf_;
#else
  return uim_scm_f();
#endif
}

static uim_lisp
commit_nth_prediction(uim_lisp id_, uim_lisp nth_)
{
#ifdef HAS_ANTHY_COMMIT_PREDICTION
  int id  = uim_scm_c_int(id_);
  int nth = uim_scm_c_int(nth_); 
  anthy_context_t ac = get_anthy_context(id);

  if (!ac) {
    return uim_scm_f();
  }

  return anthy_commit_prediction(ac, nth) ? uim_scm_f() : uim_scm_t();
#else
  return uim_scm_f();
#endif
}

#ifndef ENABLE_ANTHY_STATIC
void
uim_plugin_instance_init(void)
#else
void
uim_anthy_plugin_instance_init(void)
#endif
{
  uim_scm_init_subr_0("anthy-lib-init", init_anthy_lib);
  uim_scm_init_subr_0("anthy-lib-alloc-context", create_context);
  uim_scm_init_subr_1("anthy-lib-free-context", release_context);
  uim_scm_init_subr_2("anthy-lib-set-string", set_string);
  uim_scm_init_subr_1("anthy-lib-get-nr-segments",get_nr_segments);
  uim_scm_init_subr_2("anthy-lib-get-nr-candidates", get_nr_candidates);
  uim_scm_init_subr_3("anthy-lib-get-nth-candidate", get_nth_candidate);
  uim_scm_init_subr_2("anthy-lib-get-unconv-candidate", get_unconv_candidate);
  uim_scm_init_subr_2("anthy-lib-get-segment-length", get_segment_length);
  uim_scm_init_subr_3("anthy-lib-resize-segment", resize_segment);
  uim_scm_init_subr_3("anthy-lib-commit-segment", commit_segment);
  uim_scm_init_subr_0("anthy-lib-get-anthy-version", anthy_version);
  uim_scm_init_subr_2("anthy-lib-set-prediction-src-string", set_prediction_src_string);
  uim_scm_init_subr_1("anthy-lib-get-nr-predictions", get_nr_predictions);
  uim_scm_init_subr_2("anthy-lib-get-nth-prediction", get_nth_prediction);
  uim_scm_init_subr_2("anthy-lib-commit-nth-prediction",
		      commit_nth_prediction);
}

#ifndef ENABLE_ANTHY_STATIC
void
uim_plugin_instance_quit(void)
#else
void
uim_anthy_plugin_instance_quit(void)
#endif
{
  int i;

  if (!context_slot) {
    return;
  }

  for (i = 0; i < MAX_CONTEXT; i++) {
    if (context_slot[i].ac) {
      anthy_release_context(context_slot[i].ac);
    }
  }

  anthy_quit();

  if (context_slot) {
    free(context_slot);
    context_slot = NULL;
  }
}
