/*
  $Id:$

  canna.c: Canna for uim.

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

#include "config.h"
#ifdef HAVE_CANNA_RK_H
#include <canna/RK.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <errno.h>
#include <string.h>
#include "context.h"
#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "plugin.h"

#define MAX_CONTEXT 256
#define LIBCANNA_SO	"libcanna.so"

#define BUFSIZE 1024

struct canna_context {
  char diclist[BUFSIZE];

  int rk_context_id;
  int rk_mode;

  int current_cand_num;
  int max_current_cand_num;

  int segment_num;
  int current_segment_num;
};

static struct canna_context *context_array = NULL;

static int context_array_len;
static int rk_initialized = -1;
static char *cannaserver = NULL;

static struct canna_context *
get_canna_context(int id)
{
  int i;
  struct canna_context *context;

  context = context_array;

  if(id >= MAX_CONTEXT || id < 0)
    return NULL;
  for(i = 0; i < id; i++)
    context++;

/*  printf("rk_context_id: %d\n", context->rk_context_id);
    printf("segment_num: %d\n", context->segment_num); */
  return context;
}

static uim_lisp
init_canna_lib(uim_lisp str_)
{
  struct canna_context *context;
  int i;

  if(str_ != uim_scm_f())
    cannaserver = uim_scm_c_str(str_);
  else
    cannaserver = NULL;

  context_array = malloc(sizeof(struct canna_context) * MAX_CONTEXT);
  if(context_array == NULL)
    return uim_scm_f();

  context = context_array;

  for (i = 0; i < MAX_CONTEXT; i++) {
    context->rk_context_id = -1;
    context->rk_mode = (RK_XFER << RK_XFERBITS) | RK_KFER;

    context->current_cand_num = -1;
    context->max_current_cand_num = -1;

    context->segment_num = -1;
    context->current_segment_num = -1;

    context->diclist[0] = '\0';
    context++;
  }

  if(rk_initialized == -1) {
    if(RkInitialize(cannaserver) == -1) {
      fprintf(stderr, "%s\n", strerror(errno));
      return uim_scm_f();
    }
    RkFinalize();
  }

  return uim_scm_t();
}

static uim_lisp
create_context() {
  int i;
  char *buf;
  int buflen;
  struct canna_context *cc = context_array;

  if(rk_initialized == -1) {
    if(RkInitialize(cannaserver) == -1) {
      fprintf(stderr, "%s\n", strerror(errno));
      return uim_scm_f();
    }
    rk_initialized = 1;
  }

  for(i = 0; i < MAX_CONTEXT; i++) {
     if(cc->rk_context_id == -1) {
	int dic_num;
	cc->rk_context_id = RkCreateContext();
	dic_num = RkGetDicList(cc->rk_context_id,
				   cc->diclist, BUFSIZE);
	if(dic_num == 0) {
	    return uim_scm_f();
	} else if(dic_num == -1) {
	    /* invalid context number */
	    return uim_scm_f();
	} else {
	    int j;
	    /* buf[] = "dicname1\0dicname2\0dicname3\0...dicname_n\0\0" */
	    buf = cc->diclist;
	    for(j = 0; j < dic_num; j++) {
		RkMountDic(cc->rk_context_id, buf, 0);
		buflen = strlen(buf) + 1;
		buf += buflen;
	    }
	}
	return uim_scm_make_int(i);
     }
     cc++;
  }
  return uim_scm_f();
}

static uim_lisp
release_context(uim_lisp id_)
{
  int id = uim_scm_c_int(id_);
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
    return uim_scm_f();

  if(cc->rk_context_id == -1)
    return uim_scm_f();

  if(RkCloseContext(cc->rk_context_id) != -1) {
    cc->rk_context_id = -1;
    return uim_scm_t();
  } else {
    return uim_scm_f();
  }
}

static void
_reset_conversion(int id)
{
  struct canna_context *cc = get_canna_context(id);
  if(cc == NULL)
     return;

  if(cc->segment_num >= 0) {
     cc->segment_num = -1;
     RkEndBun(cc->rk_context_id, 0);
  }
}

static void
_update_status(int id)
{
  RkStat stat;
  struct canna_context *cc = get_canna_context(id);
  if(cc == NULL)
     return;

  if(cc->rk_context_id == -1)
     return;

  if(RkGetStat(cc->rk_context_id, &stat) == 0)
  {
    cc->current_segment_num = stat.bunnum;
    cc->current_cand_num = stat.candnum;
    cc->max_current_cand_num = stat.maxcand;
  } else {
    _reset_conversion(id);
  }
}

static void
_update_segment (const int id, const int segment_num)
{
  int i, tmp_segment_num;
  char buf[BUFSIZE];
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
    return;

  if(cc->rk_context_id == -1)
    return;

  tmp_segment_num = segment_num;
  if(segment_num >= cc->segment_num)
    tmp_segment_num = 0;

  for(i = 0; i <= tmp_segment_num; i++) {
    int len;
    RkGoTo(cc->rk_context_id, i);
    len = RkGetKanji(cc->rk_context_id, buf, BUFSIZE);
/*    printf("segment: %d, buf: %s\n", i, buf); */
  }

  RkGoTo(cc->rk_context_id, tmp_segment_num);
  _update_status(id);
}

static uim_lisp
begin_conversion(uim_lisp id_, uim_lisp str_)
{
  int id = uim_scm_c_int(id_);
  char *str;
  int len, segment_num, mode;
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
    return uim_scm_f();

  if(cc->rk_context_id == -1)
    return uim_scm_f();

  mode = cc->rk_mode;
  str = uim_scm_c_str(str_);
  len = strlen(str);

  segment_num = RkBgnBun(cc->rk_context_id, str, len, mode);

  if(segment_num == -1) {
     /* failed to conversion */
     if(str != NULL)
	free(str);
     return uim_scm_f();
  }

  cc->segment_num = segment_num;
  _update_segment(id, 0);

  if(str != NULL)
     free(str);

  return uim_scm_make_int(cc->segment_num);
}

static uim_lisp
get_nth_candidate(uim_lisp id_, uim_lisp seg_, uim_lisp nth_)
{
  int id = uim_scm_c_int(id_);
  int seg = uim_scm_c_int(seg_);
  int nth = uim_scm_c_int(nth_);
  struct canna_context *cc = get_canna_context(id);
  char buf[BUFSIZE];
  int len;

  if(cc == NULL)
    return uim_scm_f();

  _update_segment(id, seg);
  if(nth > cc->max_current_cand_num)
    nth = 0;

  RkXfer(cc->rk_context_id, nth);
  len = RkGetKanji(cc->rk_context_id, buf, BUFSIZE);

/*  printf("nth: %d, kanji: %s\n", nth, buf); */
  return uim_scm_make_str(buf);
}

static uim_lisp
get_nr_segments(uim_lisp id_)
{
  int id = uim_scm_c_int(id_);
/*  RkStat stat; */
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
     return uim_scm_f();

  if(cc->rk_context_id == -1)
     return uim_scm_f();

  return uim_scm_make_int(cc->segment_num);
}

static uim_lisp
get_nr_candidate(uim_lisp id_, uim_lisp nth_)
{
  int id = uim_scm_c_int(id_);
  int nth = uim_scm_c_int(nth_);
  RkStat stat;
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
     return uim_scm_f();

  if(cc->rk_context_id == -1)
     return uim_scm_f();

  RkGoTo(cc->rk_context_id, nth);

  if(RkGetStat(cc->rk_context_id, &stat) == 0)
    return uim_scm_make_int(stat.maxcand);
  else
    return uim_scm_f();
}

static uim_lisp
resize_segment(uim_lisp id_, uim_lisp s_, uim_lisp nth_)
{
  int id = uim_scm_c_int(id_);
  int s = uim_scm_c_int(s_);
  int nth = uim_scm_c_int(nth_);
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
     return uim_scm_f();

  if(cc->rk_context_id == -1)
     return uim_scm_f();

  RkGoTo(cc->rk_context_id, s);
  RkNfer(cc->rk_context_id);

  if(nth > 0)
      cc->segment_num = RkEnlarge(cc->rk_context_id);
  else
      cc->segment_num = RkShorten(cc->rk_context_id);

  _update_segment(id, cc->current_segment_num);

  return uim_scm_t();
}

static uim_lisp
commit_segment(uim_lisp id_, uim_lisp s_, uim_lisp nth_)
{
  int id = uim_scm_c_int(id_);
  int s = uim_scm_c_int(s_);
  int nth = uim_scm_c_int(nth_);
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
     return uim_scm_f();

  if(cc->rk_context_id == -1)
     return uim_scm_f();

  RkEndBun(cc->rk_context_id, 1);
  cc->segment_num = -1;

  return uim_scm_t();
}

static uim_lisp
reset_conversion(uim_lisp id_)
{
  int id = uim_scm_c_int(id_);

  _reset_conversion(id);

  return uim_scm_t();
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_subr_1("canna-lib-init", init_canna_lib);

  uim_scm_init_subr_0("canna-lib-alloc-context", create_context);
  uim_scm_init_subr_1("canna-lib-release-context", release_context);
  uim_scm_init_subr_3("canna-lib-get-nth-candidate", get_nth_candidate);
  uim_scm_init_subr_1("canna-lib-get-nr-segments",get_nr_segments);
  uim_scm_init_subr_2("canna-lib-get-nr-candidates", get_nr_candidate);
  uim_scm_init_subr_3("canna-lib-resize-segment", resize_segment);
  uim_scm_init_subr_2("canna-lib-begin-conversion", begin_conversion);
  uim_scm_init_subr_3("canna-lib-commit-segment", commit_segment);
  uim_scm_init_subr_1("canna-lib-reset-conversion", reset_conversion);
}

void
uim_plugin_instance_quit(void)
{
  if(cannaserver != NULL) {
     free(cannaserver);
     cannaserver = NULL;
  }

  if(RkFinalize && rk_initialized == 1) {
     RkFinalize();
     rk_initialized = -1;
  }

  if(context_array != NULL) {
     free(context_array);
     context_array = NULL;
  }
}
#endif /* HAVE_CANNA_RK_H */
