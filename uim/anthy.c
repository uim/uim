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

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <anthy/anthy.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "dynlib.h"


#ifdef ENABLE_ANTHY_STATIC
void uim_anthy_plugin_instance_init(void);
void uim_anthy_plugin_instance_quit(void);
#endif

static uim_bool initialized;
static uim_lisp context_list;

static void
validate_segment_index(anthy_context_t ac, int i)
{
  int err;
  struct anthy_conv_stat cs;

  err = anthy_get_stat(ac, &cs);
  if (err)
    uim_fatal_error("anthy_get_stat() failed");
  if (!(0 <= i && i < cs.nr_segment))
    ERROR_OBJ("invalid segment index", MAKE_INT(i));
}

static anthy_context_t
get_anthy_context(uim_lisp ac_)
{
  anthy_context_t ac;

  ac = C_PTR(ac_);
  if (!ac)
    uim_fatal_error("NULL anthy_context_t");

  return ac;
}

static uim_lisp
anthy_version()
{
  return MAKE_STR(anthy_get_version_string());
}

static uim_lisp
init_anthy_lib(void)
{
  if (!initialized) {
    if (anthy_init() == -1)
      uim_fatal_error("anthy_init() failed");

    initialized = UIM_TRUE;
  }

  return uim_scm_t();
}

static uim_lisp
create_context(void)
{
  anthy_context_t ac;
  uim_lisp ac_;

  ac = anthy_create_context();
  if (!ac)
    uim_fatal_error("anthy_create_context() failed");

  ac_ = MAKE_PTR(ac);
  context_list = uim_scm_callf("cons", "oo", ac_, context_list);

  return ac_;
}


static uim_lisp
release_context(uim_lisp ac_)
{
  anthy_context_t ac;

  context_list = uim_scm_callf("delete!", "oo", ac_, context_list);

  ac = get_anthy_context(ac_);
  anthy_release_context(ac);
  uim_scm_nullify_c_ptr(ac_);

  return uim_scm_f();
}

static uim_lisp
set_string(uim_lisp ac_, uim_lisp str_)
{
  anthy_context_t ac;
  const char *str;

  ac = get_anthy_context(ac_);
  str = REFER_C_STR(str_);
  anthy_set_string(ac, str);

  return uim_scm_f();
}

static uim_lisp
get_nr_segments(uim_lisp ac_)
{
  anthy_context_t ac;
  struct anthy_conv_stat cs;
  int err;

  ac = get_anthy_context(ac_);
  err = anthy_get_stat(ac, &cs);
  if (err)
    uim_fatal_error("anthy_get_stat() failed");

  return MAKE_INT(cs.nr_segment);
}

static uim_lisp
get_nr_candidates(uim_lisp ac_, uim_lisp seg_)
{
  anthy_context_t ac;
  int seg, err;
  struct anthy_segment_stat ss;

  ac = get_anthy_context(ac_);
  seg = C_INT(seg_);

  validate_segment_index(ac, seg);

  err = anthy_get_segment_stat(ac, seg, &ss);
  if (err)
    uim_fatal_error("anthy_get_segment_stat() failed");

  return MAKE_INT(ss.nr_candidate);
}

static uim_lisp
get_nth_candidate(uim_lisp ac_, uim_lisp seg_, uim_lisp nth_)
{
  anthy_context_t ac;
  int seg, nth, buflen;
  char *buf;
  uim_lisp buf_;
  
  ac = get_anthy_context(ac_);
  seg = C_INT(seg_);
  nth  = C_INT(nth_);

  buflen = anthy_get_segment(ac, seg, nth, NULL, 0);
  if (buflen == -1)
    uim_fatal_error("anthy_get_segment() failed");

  buf = uim_malloc(buflen + 1);
  buflen = anthy_get_segment(ac, seg, nth, buf, buflen + 1);
  if (buflen == -1) {
    free(buf);
    uim_fatal_error("anthy_get_segment() failed");
  }
  buf_ = MAKE_STR_DIRECTLY(buf);

  return buf_;
}

static uim_lisp
get_unconv_candidate(uim_lisp ac_, uim_lisp seg_)
{
  uim_lisp nth_;

  nth_ = MAKE_INT(NTH_UNCONVERTED_CANDIDATE);
  return get_nth_candidate(ac_, seg_, nth_);
}

static uim_lisp
get_segment_length(uim_lisp ac_, uim_lisp seg_)
{
  anthy_context_t ac;
  int seg, err;
  struct anthy_segment_stat ss;

  ac = get_anthy_context(ac_);
  seg = C_INT(seg_);

  validate_segment_index(ac, seg);

  err = anthy_get_segment_stat(ac, seg, &ss);
  if (err)
    uim_fatal_error("anthy_get_segment_stat() failed");

  return MAKE_INT(ss.seg_len);
}

static uim_lisp
resize_segment(uim_lisp ac_, uim_lisp seg_, uim_lisp delta_)
{
  anthy_context_t ac;
  int seg, delta;

  ac = get_anthy_context(ac_);
  seg = C_INT(seg_);
  delta = C_INT(delta_);

  anthy_resize_segment(ac, seg, delta);
  return uim_scm_f();
}

static uim_lisp
commit_segment(uim_lisp ac_, uim_lisp seg_, uim_lisp nth_)
{
  anthy_context_t ac;
  int seg, nth;

  ac = get_anthy_context(ac_);
  seg = C_INT(seg_);
  nth = C_INT(nth_);

  anthy_commit_segment(ac, seg, nth);
  return uim_scm_f();
}

static uim_lisp
set_prediction_src_string(uim_lisp ac_, uim_lisp str_)
{
#ifdef HAS_ANTHY_PREDICTION
  anthy_context_t ac;
  const char *str;

  ac = get_anthy_context(ac_);
  str = REFER_C_STR(str_);

  anthy_set_prediction_string(ac, str);
#endif
  return uim_scm_f();
}

static uim_lisp
get_nr_predictions(uim_lisp ac_)
{
#ifdef HAS_ANTHY_PREDICTION
  anthy_context_t ac;
  struct anthy_prediction_stat ps;
  int err;

  ac = get_anthy_context(ac_);

  err = anthy_get_prediction_stat(ac, &ps);
  if (err)
    uim_fatal_error("anthy_get_prediction_stat() failed");
  return MAKE_INT(ps.nr_prediction);
#else
  return uim_scm_f();
#endif
}

static uim_lisp
get_nth_prediction(uim_lisp ac_, uim_lisp nth_)
{
#ifdef HAS_ANTHY_PREDICTION
  anthy_context_t ac;
  int nth, buflen;
  char *buf;
  uim_lisp buf_;

  ac = get_anthy_context(ac_);
  nth = C_INT(nth_); 

  buflen = anthy_get_prediction(ac, nth, NULL, 0);
  if (buflen == -1)
    uim_fatal_error("anthy_get_prediction() failed");

  buf = uim_malloc(buflen + 1);
  buflen = anthy_get_prediction(ac, nth, buf, buflen + 1);
  if (buflen == -1) {
    free(buf);
    uim_fatal_error("anthy_get_prediction() failed");
  }
  buf_ = MAKE_STR_DIRECTLY(buf);

  return buf_;
#else
  return uim_scm_f();
#endif
}

static uim_lisp
commit_nth_prediction(uim_lisp ac_, uim_lisp nth_)
{
#ifdef HAS_ANTHY_COMMIT_PREDICTION
  anthy_context_t ac;
  int nth, err;

  ac = get_anthy_context(ac_);
  nth = C_INT(nth_); 

  err = anthy_commit_prediction(ac, nth);

  return MAKE_BOOL(!err);
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
  context_list = uim_scm_null();
  uim_scm_gc_protect(&context_list);

  uim_scm_eval_c_string("(require-extension (srfi 1))"); /* for delete! */

  uim_scm_init_proc0("anthy-lib-init", init_anthy_lib);
  uim_scm_init_proc0("anthy-lib-alloc-context", create_context);
  uim_scm_init_proc1("anthy-lib-free-context", release_context);
  uim_scm_init_proc2("anthy-lib-set-string", set_string);
  uim_scm_init_proc1("anthy-lib-get-nr-segments",get_nr_segments);
  uim_scm_init_proc2("anthy-lib-get-nr-candidates", get_nr_candidates);
  uim_scm_init_proc3("anthy-lib-get-nth-candidate", get_nth_candidate);
  uim_scm_init_proc2("anthy-lib-get-unconv-candidate", get_unconv_candidate);
  uim_scm_init_proc2("anthy-lib-get-segment-length", get_segment_length);
  uim_scm_init_proc3("anthy-lib-resize-segment", resize_segment);
  uim_scm_init_proc3("anthy-lib-commit-segment", commit_segment);
  uim_scm_init_proc0("anthy-lib-get-anthy-version", anthy_version);
  uim_scm_init_proc2("anthy-lib-set-prediction-src-string", set_prediction_src_string);
  uim_scm_init_proc1("anthy-lib-get-nr-predictions", get_nr_predictions);
  uim_scm_init_proc2("anthy-lib-get-nth-prediction", get_nth_prediction);
  uim_scm_init_proc2("anthy-lib-commit-nth-prediction",
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
  if (initialized) {
    uim_scm_callf("for-each", "vo", "anthy-lib-free-context", context_list);
    context_list = uim_scm_null();
    uim_scm_gc_unprotect(&context_list);

    anthy_quit();
    initialized = UIM_FALSE;
  }
}
