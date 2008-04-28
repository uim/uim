/*
  $Id:$

  canna.c: Canna for uim.

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

#ifdef HAVE_CANNA_RK_H
#include <canna/RK.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-notify.h"
#include "gettext.h"
#include "dynlib.h"

#if 0
#define UIM_CANNA_DEBUG
#endif

#define BUFSIZE 1024

#define VALID_CANNA_CONTEXTP(cc)    ((cc) && (cc)->rk_context_id != -1)
#define VALID_SEGMENT_INDEXP(cc, i) (0 <= (i) && (i) < cc->segment_num)

enum canna_api_result {
  ERR = -1,
  OK  = 0
};

enum learning_mode {
  FORGET_SELECTED_CAND = 0,
  LEARN_SELECTED_CAND  = 1
};

struct canna_context {
#ifdef UIM_CANNA_DEBUG
  char *diclist;
#endif

  int rk_context_id;
  int rk_mode;

  int *max_cand_num_list;
  int segment_num;
};

static int rk_initialized = -1;
static char *cannaserver = NULL;
static uim_lisp context_list;


static struct canna_context *
get_canna_context(uim_lisp cc_)
{
  struct canna_context *cc;

  cc = C_PTR(cc_);
  if (!cc)
    uim_fatal_error("NULL canna_context");
  assert(cc->rk_context_id != -1);
#ifdef UIM_CANNA_DEBUG
  printf("rk_context_id: %d\n", cc->rk_context_id);
  printf("segment_num: %d\n", cc->segment_num);
#endif

  return cc;
}

static void
validate_segment_index(struct canna_context *cc, int i)
{
  assert(VALID_CANNA_CONTEXTP(cc));
  if (!VALID_SEGMENT_INDEXP(cc, i))
    ERROR_OBJ("invalid segment index", MAKE_INT(i));
}

static uim_lisp
init_canna_lib(uim_lisp str_)
{
  cannaserver = (TRUEP(str_)) ? C_STR(str_) : NULL;

  /* Immediate init & quit to test cannaserver connectivity? Real
   * initialization is exist at beginning of create_context(). I don't
   * know why this sequence is needed at here.  -- YamaKen 2007-07-21 */
  if (rk_initialized == -1) {
    if (RkInitialize(cannaserver) == ERR)
      uim_fatal_error("RkInitialize() failed");
    RkFinalize();
  }

  return uim_scm_t();
}

static uim_lisp
create_context(void)
{
  struct canna_context *cc;
  uim_lisp cc_;
  int dic_num;
  char *diclist, *buf;
  int buflen, i;

  if (rk_initialized == -1) {
    if (RkInitialize(cannaserver) == ERR)
      uim_fatal_error("RkInitialize() failed");
    rk_initialized = 1;
  }

  cc = uim_malloc(sizeof(*cc));
  cc->rk_context_id = RkCreateContext();
  if (cc->rk_context_id == ERR) {
    free(cc);
    uim_fatal_error("RkCreateContext() failed");
  }    

  cc->rk_mode = (RK_XFER << RK_XFERBITS) | RK_KFER;
  cc->max_cand_num_list = NULL;
  cc->segment_num = -1;

  diclist = uim_malloc(BUFSIZE);
  diclist[0] = '\0';

  dic_num = RkGetDicList(cc->rk_context_id, diclist, BUFSIZE);
  if (dic_num == ERR) {
    /* invalid context number */
    uim_fatal_error("RkGetDicList() failed");
  }

  /* buf[] = "dicname1\0dicname2\0dicname3\0...dicname_n\0\0" */
  buf = diclist;
  for (i = 0; i < dic_num; i++) {
    if (RkMountDic(cc->rk_context_id, buf, 0) == ERR) {
      /* FIXME: gettext here to expand %s in accordance with the
       * locale for the selected notification agent. See also the TODO
       * comment of uim-notify.h  -- YamaKen 2008-02-11 */
      uim_notify_fatal(N_("uim-canna: Failed to mount dictionary %s."), buf);
    }
    buflen = strlen(buf) + sizeof((char)'\0');
    buf += buflen;
  }
#ifdef UIM_CANNA_DEBUG
  cc->diclist = diclist;
#else
  free(diclist);
#endif

  cc_ = MAKE_PTR(cc);
  context_list = uim_scm_callf("cons", "oo", cc_, context_list);

  return cc_;
}

static uim_lisp
release_context(uim_lisp cc_)
{
  struct canna_context *cc;
  uim_bool err;

  context_list = uim_scm_callf("delete!", "oo", cc_, context_list);

  cc = get_canna_context(cc_);
  uim_scm_nullify_c_ptr(cc_);

  if (cc->rk_context_id != -1) {
    err = (RkCloseContext(cc->rk_context_id) == ERR);
  } else {
    err = UIM_TRUE;
  }
#ifdef UIM_CANNA_DEBUG
  free(cc->diclist);
#endif
  free(cc);

  if (err)
    uim_fatal_error("canna-lib-release-context failed");

  return uim_scm_f();
}

static void
_reset_conversion(struct canna_context *cc)
{
  assert(VALID_CANNA_CONTEXTP(cc));

  if (cc->segment_num >= 0) {
     cc->segment_num = -1;
     RkEndBun(cc->rk_context_id, FORGET_SELECTED_CAND);
  }
}

static void
_update_status(struct canna_context *cc)
{
  RkStat stat;
  int i;

  assert(VALID_CANNA_CONTEXTP(cc));

  free(cc->max_cand_num_list);
  cc->max_cand_num_list = uim_malloc(sizeof(int) * cc->segment_num);
  for (i = 0; i < cc->segment_num; i++) {
    RkGoTo(cc->rk_context_id, i);
    if (RkGetStat(cc->rk_context_id, &stat) == OK) {
      cc->max_cand_num_list[i] = stat.maxcand;
    } else {
      cc->max_cand_num_list[i] = -1;
      _reset_conversion(cc);
    }
  }
}

static uim_lisp
begin_conversion(uim_lisp cc_, uim_lisp str_)
{
  struct canna_context *cc;
  const char *str;
  int len, segment_num, mode;

  cc = get_canna_context(cc_);

  mode = cc->rk_mode;
  str = REFER_C_STR(str_);
  len = strlen(str);

  segment_num = RkBgnBun(cc->rk_context_id, (char *)str, len, mode);
  if (segment_num == ERR)
    uim_fatal_error("RkBgnBun() failed");

  cc->segment_num = segment_num;
  _update_status(cc);

  return MAKE_INT(cc->segment_num);
}

static uim_lisp
get_nth_candidate(uim_lisp cc_, uim_lisp seg_, uim_lisp nth_)
{
  struct canna_context *cc;
  int seg, nth, len;
  char buf[BUFSIZE];

  cc = get_canna_context(cc_);
  seg = C_INT(seg_);
  nth = C_INT(nth_);
  validate_segment_index(cc, seg);

  RkGoTo(cc->rk_context_id, seg);

  if (nth < 0 || nth > cc->max_cand_num_list[seg])
    nth = 0;
  RkXfer(cc->rk_context_id, nth);

  len = RkGetKanji(cc->rk_context_id, (unsigned char *)buf, BUFSIZE);
  if (len == ERR)
    uim_fatal_error("RkGetKanji() failed");
#ifdef UIM_CANNA_DEBUG
  printf("nth: %d, kanji: %s\n", nth, buf);
#endif
  return MAKE_STR(buf);
}

static uim_lisp
get_unconv_candidate(uim_lisp cc_, uim_lisp seg_)
{
  struct canna_context *cc;
  int seg, len;
  char buf[BUFSIZE];

  cc = get_canna_context(cc_);
  seg = C_INT(seg_);
  validate_segment_index(cc, seg);

  RkGoTo(cc->rk_context_id, seg);

  len = RkGetYomi(cc->rk_context_id, (unsigned char *)buf, BUFSIZE);
  if (len == ERR)
    uim_fatal_error("RkGetYomi() failed");
#ifdef UIM_CANNA_DEBUG
  fprintf(stderr, "yomi: %s\n", buf);
#endif
  return MAKE_STR(buf);
}

static uim_lisp
get_nr_segments(uim_lisp cc_)
{
  struct canna_context *cc;
  /* RkStat stat; */

  cc = get_canna_context(cc_);

  return MAKE_INT(cc->segment_num);
}

static uim_lisp
get_nr_candidates(uim_lisp cc_, uim_lisp seg_)
{
  struct canna_context *cc;
  int seg;

  cc = get_canna_context(cc_);
  seg = C_INT(seg_);
  validate_segment_index(cc, seg);

  if (cc->max_cand_num_list[seg] == -1)
    ERROR("invalid candidate index");

  return MAKE_INT(cc->max_cand_num_list[seg]);
}

static uim_lisp
resize_segment(uim_lisp cc_, uim_lisp seg_, uim_lisp delta_)
{
  struct canna_context *cc;
  int seg, delta, id;

  cc = get_canna_context(cc_);
  seg = C_INT(seg_);
  delta = C_INT(delta_);
  validate_segment_index(cc, seg);

  RkGoTo(cc->rk_context_id, seg);
  RkNfer(cc->rk_context_id);

  id = cc->rk_context_id;
  cc->segment_num = (delta > 0) ? RkEnlarge(id) : RkShorten(id);

  _update_status(cc);

  return uim_scm_t();
}

static uim_lisp
commit_segment(uim_lisp cc_, uim_lisp seg_, uim_lisp nth_)
{
  struct canna_context *cc;
#if 0
  int seg, nth;
#endif

  cc = get_canna_context(cc_);
#if 0
  seg = C_INT(seg_);
  nth = C_INT(nth_);
  validate_segment_index(cc, seg);
#endif

  RkEndBun(cc->rk_context_id, LEARN_SELECTED_CAND);
  cc->segment_num = -1;

  return uim_scm_t();
}

static uim_lisp
reset_conversion(uim_lisp cc_)
{
  struct canna_context *cc;

  cc = get_canna_context(cc_);

  _reset_conversion(cc);

  return uim_scm_t();
}

void
uim_plugin_instance_init(void)
{
  context_list = uim_scm_null();
  uim_scm_gc_protect(&context_list);

  uim_scm_eval_c_string("(require-extension (srfi 1))"); /* for delete! */

  uim_scm_init_proc1("canna-lib-init", init_canna_lib);
  uim_scm_init_proc0("canna-lib-alloc-context", create_context);
  uim_scm_init_proc1("canna-lib-release-context", release_context);
  uim_scm_init_proc3("canna-lib-get-nth-candidate", get_nth_candidate);
  uim_scm_init_proc2("canna-lib-get-unconv-candidate", get_unconv_candidate);
  uim_scm_init_proc1("canna-lib-get-nr-segments",get_nr_segments);
  uim_scm_init_proc2("canna-lib-get-nr-candidates", get_nr_candidates);
  uim_scm_init_proc3("canna-lib-resize-segment", resize_segment);
  uim_scm_init_proc2("canna-lib-begin-conversion", begin_conversion);
  uim_scm_init_proc3("canna-lib-commit-segment", commit_segment);
  uim_scm_init_proc1("canna-lib-reset-conversion", reset_conversion);
}

void
uim_plugin_instance_quit(void)
{
  uim_scm_callf("for-each", "vo", "canna-lib-release-context", context_list);
  context_list = uim_scm_null();
  uim_scm_gc_unprotect(&context_list);

  free(cannaserver);
  cannaserver = NULL;

  if (rk_initialized == 1) {
     RkFinalize();
     rk_initialized = -1;
  }
}
#endif /* HAVE_CANNA_RK_H */
