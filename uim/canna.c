/*
  $Id$

  canna.c: Canna for uim.

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
#include "siod.h"

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

static struct canna_api {
  void *lib;
  /*  struct rkfuncs Rk; */
  int (*RkInitialize)(char *);
  int (*RkCreateContext)(void);
  int (*RkCloseContext)(int);
  int (*RkBgnBun)(int, char *, int, int);
  int (*RkEndBun)(int, int);
  int (*RkFinalize)(void);
  int (*RkGetDicList)(int, char *, int);
  int (*RkMountDic)(int, char *, int);
  int (*RkNfer)(int);
  int (*RkXfer)(int, int);
  int (*RkEnlarge)(int);
  int (*RkShorten)(int);
  int (*RkGetStat)(int, RkStat *);
  int (*RkGoTo)(int, int);
  int (*RkGetKanji)(int, char *, int);
  int (*RkGetKanjiList)(int, char *, int);
  int (*RkSetServerName)(char *);
} api;

static int
get_canna_api(void)
{
  api.lib = dlopen(LIBCANNA_SO, RTLD_NOW);
  if(!api.lib)
    return -1;

  *(int **)(&api.RkInitialize) = dlsym(api.lib, "RkInitialize");
  *(int **)(&api.RkFinalize) = dlsym(api.lib, "RkFinalize");
  *(int **)(&api.RkGetDicList) = dlsym(api.lib, "RkGetDicList");
  *(int **)(&api.RkMountDic) = dlsym(api.lib, "RkMountDic");
  *(int **)(&api.RkCreateContext) = dlsym(api.lib, "RkCreateContext");
  *(int **)(&api.RkCloseContext) = dlsym(api.lib, "RkCloseContext");
  *(int **)(&api.RkBgnBun) = dlsym(api.lib, "RkBgnBun");
  *(int **)(&api.RkEndBun) = dlsym(api.lib, "RkEndBun");
  *(int **)(&api.RkGoTo) = dlsym(api.lib, "RkGoTo");
  *(int **)(&api.RkGetKanji) = dlsym(api.lib, "RkGetKanji");
  *(int **)(&api.RkGetKanjiList) = dlsym(api.lib, "RkGetKanjiList");
  *(int **)(&api.RkGetStat) = dlsym(api.lib, "RkGetStat");
  *(int **)(&api.RkXfer) = dlsym(api.lib, "RkXfer");
  *(int **)(&api.RkNfer) = dlsym(api.lib, "RkXfer");
  *(int **)(&api.RkEnlarge) = dlsym(api.lib, "RkEnlarge");
  *(int **)(&api.RkShorten) = dlsym(api.lib, "RkShorten");
  if(api.RkInitialize && api.RkFinalize && api.RkGetDicList &&
     api.RkMountDic && api.RkCreateContext && api.RkCloseContext &&
     api.RkBgnBun && api.RkEndBun && api.RkGoTo && api.RkGetKanji &&
     api.RkGetKanjiList && api.RkGetStat && api.RkXfer && api.RkNfer &&
     api.RkEnlarge && api.RkShorten)
  {
    /* ok! */
    return 0;
  }
  return -1;
}

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

static LISP
init_canna_lib(LISP str_)
{
  struct canna_context *context;
  int i;

  if (get_canna_api() == -1)
    return NIL;

  if(str_ != NIL)
    cannaserver = uim_scm_c_str(str_);
  else
    cannaserver = NULL;

  context_array = malloc(sizeof(struct canna_context) * MAX_CONTEXT);
  if(context_array == NULL)
    return NIL;

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

  return true_sym;
}

static LISP
create_context() {
  int i;
  char *buf;
  int buflen;
  struct canna_context *cc = context_array;

  if(rk_initialized == -1) {
    if(api.RkInitialize(cannaserver) == -1) {
      fprintf(stderr, "%s\n", strerror(errno));
      return NIL;
    }
    rk_initialized = 1;
  }

  for(i = 0; i < MAX_CONTEXT; i++) {
     if(cc->rk_context_id == -1) {
	int dic_num;
	cc->rk_context_id = api.RkCreateContext();
	dic_num = api.RkGetDicList(cc->rk_context_id,
				   cc->diclist, BUFSIZE);
	if(dic_num == 0) {
	    return NIL;
	} else if(dic_num == -1) {
	    /* invalid context number */
	    return NIL;
	} else {
	    int j;
	    /* buf[] = "dicname1\0dicname2\0dicname3\0...dicname_n\0\0" */
	    buf = cc->diclist;
	    for(j = 0; j < dic_num; j++) {
		api.RkMountDic(cc->rk_context_id, buf, 0);
		buflen = strlen(buf) + 1;
		buf += buflen;
	    }
	}
	return intcons(i);
     }
     cc++;
  }
  return NIL;
}

static LISP
release_context(LISP id_)
{
  int id = uim_scm_c_int(id_);
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
    return NIL;

  if(cc->rk_context_id == -1)
    return NIL;

  if(api.RkCloseContext(cc->rk_context_id) != -1) {
    cc->rk_context_id = -1;
    return true_sym;
  } else {
    return NIL;
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
     api.RkEndBun(cc->rk_context_id, 0);
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

  if(api.RkGetStat(cc->rk_context_id, &stat) == 0)
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
    api.RkGoTo(cc->rk_context_id, i);
    len = api.RkGetKanji(cc->rk_context_id, buf, BUFSIZE);
/*    printf("segment: %d, buf: %s\n", i, buf); */
  }

  api.RkGoTo(cc->rk_context_id, tmp_segment_num);
  _update_status(id);
}

static LISP
begin_conversion(LISP id_, LISP str_)
{
  int id = uim_scm_c_int(id_);
  char *str;
  int len, segment_num, mode;
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
    return NIL;

  if(cc->rk_context_id == -1)
    return NIL;

  mode = cc->rk_mode;
  str = uim_scm_c_str(str_);
  len = strlen(str);

  segment_num = api.RkBgnBun(cc->rk_context_id, str, len, mode);

  if(segment_num == -1) {
     /* failed to conversion */
     if(str != NULL)
	free(str);
     return NIL;
  }

  cc->segment_num = segment_num;
  _update_segment(id, 0);

  if(str != NULL)
     free(str);

  return intcons(cc->segment_num);
}

static LISP
get_nth_candidate(LISP id_, LISP seg_, LISP nth_)
{
  int id = uim_scm_c_int(id_);
  int seg = uim_scm_c_int(seg_);
  int nth = uim_scm_c_int(nth_);
  struct canna_context *cc = get_canna_context(id);
  char buf[BUFSIZE];
  int len;

  if(cc == NULL)
    return NIL;

  _update_segment(id, seg);
  if(nth > cc->max_current_cand_num)
    nth = 0;

  api.RkXfer(cc->rk_context_id, nth);
  len = api.RkGetKanji(cc->rk_context_id, buf, BUFSIZE);

/*  printf("nth: %d, kanji: %s\n", nth, buf); */
  return strcons(len, buf);
}

static LISP
get_nr_segments(LISP id_)
{
  int id = uim_scm_c_int(id_);
  RkStat stat;
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
     return NIL;

  if(cc->rk_context_id == -1)
     return NIL;

  return intcons(cc->segment_num);
}

static LISP
get_nr_candidate(LISP id_, LISP nth_)
{
  int id = uim_scm_c_int(id_);
  int nth = uim_scm_c_int(nth_);
  RkStat stat;
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
     return NIL;

  if(cc->rk_context_id == -1)
     return NIL;

  api.RkGoTo(cc->rk_context_id, nth);

  if(api.RkGetStat(cc->rk_context_id, &stat) == 0)
    return intcons(stat.maxcand);
  else
    return NIL;
}

static LISP
resize_segment(LISP id_, LISP s_, LISP nth_)
{
  int id = uim_scm_c_int(id_);
  int s = uim_scm_c_int(s_);
  int nth = uim_scm_c_int(nth_);
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
     return NIL;

  if(cc->rk_context_id == -1)
     return NIL;

  api.RkGoTo(cc->rk_context_id, s);
  api.RkNfer(cc->rk_context_id);

  if(nth > 0)
      cc->segment_num = api.RkEnlarge(cc->rk_context_id);
  else
      cc->segment_num = api.RkShorten(cc->rk_context_id);

  _update_segment(id, cc->current_segment_num);

  return true_sym;
}

static LISP
commit_segment(LISP id_, LISP s_, LISP nth_)
{
  int id = uim_scm_c_int(id_);
  int s = uim_scm_c_int(s_);
  int nth = uim_scm_c_int(nth_);
  struct canna_context *cc = get_canna_context(id);

  if(cc == NULL)
     return NIL;

  if(cc->rk_context_id == -1)
     return NIL;

  api.RkEndBun(cc->rk_context_id, 1);
  cc->segment_num = -1;
}

static LISP
reset_conversion(LISP id_)
{
  int id = uim_scm_c_int(id_);

  _reset_conversion(id);
}

void
uim_init_canna(void)
{
  init_subr_1("canna-lib-init", init_canna_lib);

  init_subr_0("canna-lib-alloc-context", create_context);
  init_subr_1("canna-lib-release-context", release_context);
  init_subr_3("canna-lib-get-nth-candidate", get_nth_candidate);
  init_subr_1("canna-lib-get-nr-segments",get_nr_segments);
  init_subr_2("canna-lib-get-nr-candidates", get_nr_candidate);
  init_subr_3("canna-lib-resize-segment", resize_segment);
  init_subr_2("canna-lib-begin-conversion", begin_conversion);
  init_subr_3("canna-lib-commit-segment", commit_segment);
  init_subr_1("canna-lib-reset-conversion", reset_conversion);
}

void
uim_quit_canna(void)
{
  if(cannaserver != NULL) {
     free(cannaserver);
     cannaserver = NULL;
  }

  if(api.RkFinalize && rk_initialized == 1) {
     api.RkFinalize();
     rk_initialized = -1;
  }

  if(api.lib) {
     dlclose(api.lib);
     memset(&api, 0, sizeof(struct canna_api));
  }

  if(context_array != NULL) {
     free(context_array);
     context_array = NULL;
  }
}
#endif /* HAVE_CANNA_RK_H */
