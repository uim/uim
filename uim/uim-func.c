/*

  Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/

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

#include <ctype.h>
#include <iconv.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include "context.h"
#include "uim-scm.h"
#include "uim-encoding.h"

#define MAX_LENGTH_OF_INT_AS_STR (((sizeof(int) == 4) ? sizeof("-2147483648") : sizeof("-9223372036854775808")) - sizeof((char)'\0'))

static const char **uim_get_encoding_alias(const char *encoding);
static iconv_t uim_iconv_open(const char *tocode, const char *fromcode);

char *uim_return_str;
char *uim_return_str_list[10]; /* XXX */
/* duplicate definition */
#define UIM_RETURN_STR_LIST_SIZE ((sizeof(uim_return_str_list) \
                                   / sizeof(uim_return_str_list[0])) - 2)
static struct uim_code_converter uim_iconv_tbl = {
  uim_iconv_is_convertible,
  uim_iconv_create,
  uim_iconv_code_conv,
  uim_iconv_release
};
struct uim_code_converter *uim_iconv = &uim_iconv_tbl;


/* push back a preedit segment to context */
static void
pushback_preedit_segment(uim_context uc, int atr, char *str)
{
  uc->psegs = realloc(uc->psegs,
		      sizeof(struct preedit_segment) * (uc->nr_psegs + 1));
  uc->psegs[uc->nr_psegs].attr = atr;
  uc->psegs[uc->nr_psegs].str = str;
  uc->nr_psegs ++;
}

/* notify all preedit segments to user */
void
uim_update_preedit_segments(uim_context uc)
{
  int i;
  if (uc->preedit_clear_cb) {
    uc->preedit_clear_cb(uc->ptr);
  }
  if (uc->preedit_pushback_cb) {
    for (i = 0; i < uc->nr_psegs; i++) {
      uc->preedit_pushback_cb(uc->ptr, uc->psegs[i].attr, uc->psegs[i].str);
    }
  }
  if (uc->preedit_update_cb) {
    uc->preedit_update_cb(uc->ptr);
  }
}

#ifdef UIM_CALLBACK_QUEUE
static void
uim_flush_cb(uim_context uc)
{
  struct cb *cb = uc->cb_q.first_cb;
  while (cb) {
    struct cb *tmp = cb;
    cb = cb->next;
    switch (tmp->type) {
    case COMMIT_CB:
      {
	char *s;
	s = uc->conv_if->convert(uc->conv, tmp->str);
 	if (uc->commit_cb) {
 	  uc->commit_cb(uc->ptr, s);
 	}
	free(s);
      }
      break;
    case CAND_ACTIVATE_CB:
      {
	if (uc->candidate_selector_activate_cb) {
	  uc->candidate_selector_activate_cb(uc->ptr, tmp->n1, tmp->n2);
	}
      }
      break;
    case CAND_SELECT_CB:
      {
	if (uc->candidate_selector_select_cb) {
	  uc->candidate_selector_select_cb(uc->ptr, tmp->n1);
	}
      }
      break;
    case CAND_SHIFT_PAGE_CB:
      {
	if (uc->candidate_selector_shift_page_cb) {
	  uc->candidate_selector_shift_page_cb(uc->ptr, tmp->n1);
	}
      }
      break;
    case CAND_DEACTIVATE_CB:
      {
	if (uc->candidate_selector_deactivate_cb) {
	  uc->candidate_selector_deactivate_cb(uc->ptr);
	}
      }
      break;
    case PREEDIT_CLEAR_CB:
      {
	uim_release_preedit_segments(uc);
      }
      break;
    case PREEDIT_PUSHBACK_CB:
      {
	char *s;
	s = uc->conv_if->convert(uc->conv, tmp->str);
	pushback_preedit_segment(uc, tmp->n1, s);
      }
      break;
    case PREEDIT_UPDATE_CB:
      {
	uim_update_preedit_segments(uc);
      }
      break;
    case MODE_UPDATE_CB:
      {
	if (uc->mode_update_cb) {
	  uc->mode_update_cb(uc->ptr, tmp->n1);
	}
      }
      break;
    case MODE_LIST_UPDATE_CB:
      {
	if (uc->mode_list_update_cb) {
	  uc->mode_list_update_cb(uc->ptr);
	}
      }
      break;
    case PROP_LABEL_UPDATE_CB:
      {
	if (uc->prop_label_update_cb) {
	  uc->prop_label_update_cb(uc->ptr, uc->proplabelstr);
	}
      }
      break;
    case PROP_LIST_UPDATE_CB:
      {
	if (uc->prop_list_update_cb) {
	  uc->prop_list_update_cb(uc->ptr, uc->propstr);
	}
      }
      break;
    case REQUEST_SURROUNDING_CB:
      {
	if (uc->request_surrounding_text_cb) {
	  uc->request_surrounding_text_cb(uc->ptr);
	}
      }
      break;
    case DELETE_SURROUNDING_CB:
      {
	if (uc->delete_surrounding_text_cb) {
	  uc->delete_surrounding_text_cb(uc->ptr, tmp->n1, tmp->n2);
	}
      }
      break;
    default:;
    }
    if (tmp->str) {
      free(tmp->str);
    }
    free(tmp);
  }
  uc->cb_q.first_cb = NULL;
  uc->cb_q.tail_cb = NULL;
}
#endif

/* release preedit segment in a context */
void
uim_release_preedit_segments(uim_context uc)
{
  int i;

  if(!uc)
    return;

  if (!uc->psegs) {
    uc->nr_psegs = 0;
    return ;
  }
  for (i = 0; i < uc->nr_psegs; i++) {
    free(uc->psegs[i].str);
  }
  free(uc->psegs);
  uc->psegs = NULL;
  uc->nr_psegs = 0;
}

static int check_encoding_equivalence(const char *tocode, const char *fromcode)
{
  const char **alias_tocode;
  const char **alias_fromcode;
  int i, j;
  int alias_tocode_alloced = 0;
  int alias_fromcode_alloced = 0;
  int found = 0;

  alias_tocode = uim_get_encoding_alias(tocode);
  alias_fromcode = uim_get_encoding_alias(fromcode);

  if (!alias_tocode) {
    alias_tocode = malloc(sizeof(char *) * 2);
    alias_tocode[0] = tocode;
    alias_tocode[1] = NULL;
    alias_tocode_alloced = 1;
  }
  if (!alias_fromcode) {
    alias_fromcode = malloc(sizeof(char *) * 2);
    alias_fromcode[0] = fromcode;
    alias_fromcode[1] = NULL;
    alias_fromcode_alloced = 1;
  }

  for (i = 0; alias_tocode[i]; i++) {
    for (j = 0; alias_fromcode[j]; j++) {
      if (!strcmp(alias_tocode[i], alias_fromcode[j])) {
        found = 1;
	break;
      }
    }
    if (found)
      break;
  }

  if (alias_tocode_alloced)
    free(alias_tocode);
  if (alias_fromcode_alloced)
    free(alias_fromcode);
  return found;
}

int
uim_iconv_is_convertible(const char *tocode, const char *fromcode)
{
  iconv_t ic;

  if (check_encoding_equivalence(tocode, fromcode))
    return 1;

  /* TODO cache the result */
  ic = uim_iconv_open(tocode, fromcode);
  if (ic == (iconv_t)-1) {
    return 0;
  }
  iconv_close(ic);
  return 1;
}

static const char**
uim_get_encoding_alias(const char *encoding) {
  int i, j;
  const char **alias;

  for (i = 0; (alias = uim_encoding_list[i]); i++) {
    for (j = 0; alias[j]; j++) {
      if (!strcmp(alias[j], encoding))
        return alias;
    }
  }
  return NULL;
}

static iconv_t
uim_iconv_open(const char *tocode, const char *fromcode) {
  iconv_t cd = (iconv_t)-1;
  int i, j;
  const char **alias_tocode, **alias_fromcode;
  int alias_tocode_alloced = 0;
  int alias_fromcode_alloced = 0;
  int opened = 0;

  alias_tocode = uim_get_encoding_alias(tocode);
  alias_fromcode = uim_get_encoding_alias(fromcode);

  if (!alias_tocode) {
    alias_tocode = malloc(sizeof(char *) * 2);
    alias_tocode[0] = tocode;
    alias_tocode[1] = NULL;
    alias_tocode_alloced = 1;
  }
  if (!alias_fromcode) {
    alias_fromcode = malloc(sizeof(char *) * 2);
    alias_fromcode[0] = fromcode;
    alias_fromcode[1] = NULL;
    alias_fromcode_alloced = 1;
  }

  for (i = 0; alias_tocode[i]; i++) {
    for (j = 0; alias_fromcode[j]; j++) {
      cd = iconv_open(alias_tocode[i], alias_fromcode[j]);
      if (cd != (iconv_t)-1) {
	opened = 1;
	break;
      }
    }
    if (opened)
      break;
  }

  if (alias_tocode_alloced)
    free(alias_tocode);
  if (alias_fromcode_alloced)
    free(alias_fromcode);
  return cd;
}

void *
uim_iconv_create(const char *tocode, const char *fromcode)
{
  iconv_t ic;

  if (check_encoding_equivalence(tocode, fromcode))
    return NULL;

  ic = uim_iconv_open(tocode, fromcode);
  if (ic == (iconv_t)-1) {
    /* since iconv_t is not explicit pointer, use 0 instead of NULL */
    ic = (iconv_t)0;
  }
  return (void *)ic;
}

char *
uim_iconv_code_conv(void *obj, const char *str)
{
  iconv_t ic;
  size_t len;
  size_t buflen;
  char *realbuf;
  char *outbuf;
  const char *inbuf;

  ic = (iconv_t)obj;
  if(!str)
    return NULL;

  if (!ic)
    return strdup(str);

  len = strlen(str);
  buflen = (len * 6)+3;
  realbuf = alloca(buflen);
  outbuf = realbuf;
  inbuf = str;
  bzero(realbuf, buflen);
  iconv(ic, (ICONV_CONST char **)&inbuf, &len, &outbuf, &buflen);
  return strdup(realbuf);
}

void
uim_iconv_release(void *obj)
{
  int err;
  err = iconv_close((iconv_t)obj);
}

#ifdef UIM_CALLBACK_QUEUE
void
uim_schedule_cb(uim_context uc, int type, char *str, int n1, int n2)
{
  struct cb *cb;

  if(!uc)
    return;

  cb = malloc(sizeof(struct cb));
  cb->type = type;
  cb->str = str;
  cb->n1 = n1;
  cb->n2 = n2;
  cb->next = NULL;

  if (!uc->cb_q.first_cb) {
    uc->cb_q.first_cb = cb;
  }
  if (uc->cb_q.tail_cb) {
    uc->cb_q.tail_cb->next = cb;
  }
  uc->cb_q.tail_cb = cb;
}
#endif

/** Calculate actual sexp string size from printf-style args.
 * This function calculates actual sexp string size from printf-style
 * args. Format string \a sexp_tmpl only accepts %d and %s.
 */
int
uim_sizeof_sexp_str(const char *sexp_tmpl, ...)
{
  va_list ap;
  int len, size;
  const char *sexp_tmpl_end, *escp = sexp_tmpl, *strarg;
  char fmtchr;

  va_start(ap, sexp_tmpl);
  len = strlen(sexp_tmpl);
  sexp_tmpl_end = sexp_tmpl + len - 1;
  while ((escp = strchr(escp, '%'))) {
    if (escp < sexp_tmpl_end) {
      escp += sizeof((char)'%');
      fmtchr = *escp++;
      switch (fmtchr) {
      case 'd':
	va_arg(ap, int);
	len += MAX_LENGTH_OF_INT_AS_STR;
	break;
      case 's':
	strarg = va_arg(ap, const char *);
	len += strlen(strarg);
	break;
      default:
	/* unexpected format string */
	size = -1;
	goto end;
      }
    } else {
      /* invalid format string */
      size = -1;
      goto end;
    }
  }
  size = len + sizeof((char)'\0');

 end:
  va_end(ap);

  return size;
}

void
uim_eval_string(uim_context uc, char *buf)
{
  /* Evaluate */
  uim_scm_eval_c_string(buf);

#ifdef UIM_CALLBACK_QUEUE
  /* Flush callback requests queued during scheme evaluation 
   * (To avoid C -> scheme -> C call, actual call is delayed to here)
   */
  if (!uc->cb_q.flushing) {
    uc->cb_q.flushing ++;
    uim_flush_cb(uc);
    uc->cb_q.flushing --;
  }
#endif
}

/* this is not a uim API, so did not name as uim_retrieve_context() */
static uim_context
retrieve_uim_context(uim_lisp id)
{
  uim_context uc;
  if (uim_scm_consp(id)) {  /* passed as Scheme-side input context */
    id = uim_scm_car(id);
  }
  uc = uim_find_context(uim_scm_c_int(id));
  return uc;
}

static uim_lisp
im_clear_preedit(uim_lisp id)
{
  uim_context uc = retrieve_uim_context(id);
#ifdef UIM_CALLBACK_QUEUE
  uim_schedule_cb(uc, PREEDIT_CLEAR_CB, NULL, 0, 0);
#else
  uim_release_preedit_segments(uc);
#endif
  return uim_scm_f();
}

static uim_lisp
im_pushback_preedit(uim_lisp id_, uim_lisp attr_, uim_lisp str_)
{
  uim_context uc = retrieve_uim_context(id_);
  const char *str = NULL;
  int attr = uim_scm_c_int(attr_);
  if (str_) {
    str = uim_scm_refer_c_str(str_);
  }
#ifdef UIM_CALLBACK_QUEUE
  uim_schedule_cb(uc, PREEDIT_PUSHBACK_CB, str, attr, 0);
#else
  {
    char *s;
    s = uc->conv_if->convert(uc->conv, str);
    pushback_preedit_segment(uc, attr, s);
  }
#endif
  return uim_scm_f();
}

static uim_lisp
im_update_preedit(uim_lisp id)
{
  uim_context uc = retrieve_uim_context(id);
#ifdef UIM_CALLBACK_QUEUE
  uim_schedule_cb(uc, PREEDIT_UPDATE_CB, NULL, 0, 0);
#else
  uim_update_preedit_segments(uc);
#endif
  return uim_scm_f();
}

/* ID, STR */
static uim_lisp
im_commit(uim_lisp id, uim_lisp str_)
{
  uim_context uc = retrieve_uim_context(id);
  const char *str = NULL;
  if (uim_scm_stringp(str_)) {
    str = uim_scm_refer_c_str(str_);
#ifdef UIM_CALLBACK_QUEUE
    uim_schedule_cb(uc, COMMIT_CB, str, 0, 0);
#else
    {
      char *s;
      s = uc->conv_if->convert(uc->conv, str);
      if (uc->commit_cb) {
	uc->commit_cb(uc->ptr, s);
      }
      free(s);
    }
#endif
  }
  return uim_scm_f();
}

static uim_lisp
im_commit_raw(uim_lisp id)
{
  uim_context uc = retrieve_uim_context(id);
  uc->commit_raw_flag = 1;
  return uim_scm_f();
}

static uim_lisp
im_get_raw_key_str(uim_lisp key_, uim_lisp key_state_)
{
  int key;
  int key_state = uim_scm_c_int(key_state_);
  char buf[2];
  
  if (uim_scm_integerp(key_)) {
    key = uim_scm_c_int(key_);
  } else {
    return uim_scm_f();
  }
  if ((key_state != 0 && key_state != UMod_Shift) ||
      key > 255) {
    return uim_scm_f();
  }
  
  buf[0] = key;
  buf[1] = 0;
  /* assuming shift modifier is bad habit. consider sticky shift and
     so on. I will modify this when I rewrite key handling of uim.
     -- 2004-07-06 YamaKen
  */
  if (key_state == UMod_Shift) {
    buf[0] = toupper(buf[0]);
  }
  return uim_scm_make_str(buf);
}

static uim_lisp
im_set_encoding(uim_lisp id, uim_lisp enc)
{
  const char *e = uim_scm_refer_c_str(enc);
  uim_context uc = retrieve_uim_context(id);

  if(!uc)
    return uim_scm_f();

  if (uc->conv) {
    uc->conv_if->release(uc->conv);
  }
  if (!strcmp(uc->encoding, e)) {
    uc->conv = 0;
    return uim_scm_f();
  }
  uc->conv = uc->conv_if->create(uc->encoding, e);
  return uim_scm_f();
}

static uim_lisp
im_clear_mode_list(uim_lisp id)
{
  int i;
  uim_context uc = retrieve_uim_context(id);

  if(!uc)
    return uim_scm_f();

  for (i = 0; i < uc->nr_modes; i++) {
    if (uc->modes[i]) {
      free(uc->modes[i]);
      uc->modes[i] = NULL;
    }
  }
  if (uc->modes) {
    free(uc->modes);
    uc->modes = NULL;
  }
  uc->nr_modes = 0;
  return uim_scm_f();
}

static uim_lisp
im_pushback_mode_list(uim_lisp id, uim_lisp str)
{
  const char *s;
  uim_context uc = retrieve_uim_context(id);

  if(!uc)
    return uim_scm_f();

  uc->modes = realloc(uc->modes,
		      sizeof(char *)*(uc->nr_modes+1));
  s = uim_scm_refer_c_str(str);
  uc->modes[uc->nr_modes] = uc->conv_if->convert(uc->conv, s);
  uc->nr_modes ++;
  return uim_scm_f();
}

static uim_lisp
im_update_mode_list(uim_lisp id)
{
  uim_context uc = retrieve_uim_context(id);

  if(!uc)
    return uim_scm_f();

#ifdef UIM_CALLBACK_QUEUE
  uim_schedule_cb(uc, MODE_LIST_UPDATE_CB, NULL, 0, 0);
#else
  if (uc->mode_list_update_cb) {
    uc->mode_list_update_cb(uc->ptr);
  }
#endif
  return uim_scm_f();
}

static uim_lisp
im_update_prop_list(uim_lisp id, uim_lisp prop_)
{
  uim_context uc = retrieve_uim_context(id);
  const char *prop = uim_scm_refer_c_str(prop_);

  if(uc) {
#ifdef UIM_CALLBACK_QUEUE
    uim_schedule_cb(uc, PROP_LIST_UPDATE_CB, NULL, 0, 0);
#endif
  } else {
    return uim_scm_f();
  }
  
  if(uc && uc->propstr)
    free(uc->propstr);
      
  uc->propstr = uc->conv_if->convert(uc->conv, prop);

#ifndef UIM_CALLBACK_QUEUE
    if (uc->prop_list_update_cb) {
      uc->prop_list_update_cb(uc->ptr, uc->propstr);
    }
#endif

  return uim_scm_f();
}


static uim_lisp
im_update_prop_label(uim_lisp id, uim_lisp prop_)
{
  uim_context uc = retrieve_uim_context(id);
  const char *prop = uim_scm_refer_c_str(prop_);
    
  if(uc) {
#ifdef UIM_CALLBACK_QUEUE
    uim_schedule_cb(uc, PROP_LABEL_UPDATE_CB, NULL, 0, 0);
#endif
  } else {
    return uim_scm_f();
  }

  if(uc && uc->proplabelstr)
    free(uc->proplabelstr);
  
  uc->proplabelstr = uc->conv_if->convert(uc->conv, prop);
  
#ifndef UIM_CALLBACK_QUEUE
    if (uc->prop_label_update_cb) {
      uc->prop_label_update_cb(uc->ptr, uc->proplabelstr);
    }
#endif

  return uim_scm_f();
}

static uim_lisp
im_update_mode(uim_lisp id, uim_lisp mode_)
{
  int mode = uim_scm_c_int(mode_);
  uim_context uc = retrieve_uim_context(id);

  if(!uc)
    return uim_scm_f();

  uc->mode = mode;
#ifdef UIM_CALLBACK_QUEUE
  uim_schedule_cb(uc, MODE_UPDATE_CB, NULL, mode, 0);
#else
  if (uc->mode_update_cb) {
    uc->mode_update_cb(uc->ptr, mode);
  }
#endif
  return uim_scm_f();
}

static char *
get_im_lang(const char *name)
{
  int i;
  for (i = 0; i < uim_nr_im; i++) {
    struct uim_im *im = &uim_im_array[i];
    if (!strcmp(im->name, name)) {
      return im->lang;
    }
  }
  return NULL;
}

static uim_lisp
im_register_im(uim_lisp name, uim_lisp lang, uim_lisp enc, uim_lisp s_desc)
{
  const char *im_name = uim_scm_refer_c_str(name);
  if (get_im_lang(im_name)) {
    /* avoid duplicate register */
    return uim_scm_f();
  }
  uim_im_array = realloc(uim_im_array,
			 sizeof(struct uim_im) *
			 (uim_nr_im + 1));
  uim_im_array[uim_nr_im].lang = uim_scm_c_str(lang);
  uim_im_array[uim_nr_im].name = uim_scm_c_str(name);
  uim_im_array[uim_nr_im].encoding = uim_scm_c_str(enc);
  uim_im_array[uim_nr_im].short_desc = uim_scm_c_str(s_desc);
  uim_nr_im ++;
  return uim_scm_t();
}

static uim_lisp
im_activate_candidate_selector(uim_lisp id_, uim_lisp nr_, uim_lisp display_limit_)
{
  uim_context uc = retrieve_uim_context(id_);
  int display_limit = uim_scm_c_int(display_limit_);
  int nr = uim_scm_c_int(nr_);
#ifdef UIM_CALLBACK_QUEUE
  uim_schedule_cb(uc, CAND_ACTIVATE_CB, NULL, nr, display_limit);
#else
  if (uc->candidate_selector_activate_cb) {
    uc->candidate_selector_activate_cb(uc->ptr, nr, display_limit);
  }
#endif
  return uim_scm_f();
}

static uim_lisp
im_select_candidate(uim_lisp id_, uim_lisp idx_)
{
  uim_context uc = retrieve_uim_context(id_);
  int idx = uim_scm_c_int(idx_);
#ifdef UIM_CALLBACK_QUEUE
  uim_schedule_cb(uc, CAND_SELECT_CB, NULL, idx, 0);
#else
  if (uc->candidate_selector_select_cb) {
    uc->candidate_selector_select_cb(uc->ptr, idx);
  }
#endif
  return uim_scm_f();
}


/* My naming sense seems bad... */
static uim_lisp
im_shift_page_candidate(uim_lisp id_, uim_lisp dir_)
{
  uim_context uc = retrieve_uim_context(id_);
  int dir;

  if FALSEP(dir_)
    dir = 0;
  else
    dir = 1;
    
#ifdef UIM_CALLBACK_QUEUE
  uim_schedule_cb(uc, CAND_SHIFT_PAGE_CB, NULL, dir, 0);
#else
  if (uc->candidate_selector_shift_page_cb) {
    uc->candidate_selector_shift_page_cb(uc->ptr, dir);
  }
#endif
  return uim_scm_f();
}

static uim_lisp
im_deactivate_candidate_selector(uim_lisp id_)
{
  uim_context uc = retrieve_uim_context(id_);
#ifdef UIM_CALLBACK_QUEUE
  uim_schedule_cb(uc, CAND_DEACTIVATE_CB, NULL, 0, 0);
#else
  if (uc->candidate_selector_deactivate_cb) {
    uc->candidate_selector_deactivate_cb(uc->ptr);
  }
#endif
  return uim_scm_f();
}

static uim_lisp
im_return_str(uim_lisp str_)
{
  if (uim_return_str) {
    free(uim_return_str);
    uim_return_str = NULL;
  }
  if (uim_scm_stringp(str_)) {
    uim_return_str = uim_scm_c_str(str_);
  }
  return uim_scm_f();
}

static uim_lisp
im_return_str_list(uim_lisp str_list_)
{
  /*XXX: This fixed length array is negligence */
  int i;

  if (uim_return_str_list) {
    for (i = 0; i < (int)UIM_RETURN_STR_LIST_SIZE; i++){
      if (uim_return_str_list[i]) {
	free(uim_return_str_list[i]);
	uim_return_str_list[i] = NULL;
      } else {
	break;
      }
    }
  }

  i = 0;

  while (!uim_scm_nullp(str_list_) && i < (int)UIM_RETURN_STR_LIST_SIZE) {
    uim_lisp str_ = uim_scm_car(str_list_);
    if (uim_scm_stringp(str_)) {
      uim_return_str_list[i] = uim_scm_c_str(str_);
    }

    i++;
    str_list_ = uim_scm_cdr(str_list_);
  }
  uim_return_str_list[i] = NULL;
  return uim_scm_f();
}

static uim_lisp
im_request_surrounding(uim_lisp id_)
{
  uim_context uc = retrieve_uim_context(id_);
  if (!uc->request_surrounding_text_cb) {
    return uim_scm_f();
  }
#ifdef UIM_CALLBACK_QUEUE
  uim_schedule_cb(uc, REQUEST_SURROUNDING_CB, NULL, 0, 0);
#else
  if (uc->request_surrounding_text_cb) {
    uc->request_surrounding_text_cb(uc->ptr);
  }
#endif
  return uim_scm_t();
}

static uim_lisp
im_delete_surrounding(uim_lisp id_, uim_lisp offset_, uim_lisp len_)
{
  uim_context uc = retrieve_uim_context(id_);
  int offset = uim_scm_c_int(offset_);
  int len = uim_scm_c_int(len_);
  if (!uc->delete_surrounding_text_cb) {
    return uim_scm_f();
  }
#ifdef UIM_CALLBACK_QUEUE
  uim_schedule_cb(uc, DELETE_SURROUNDING_CB, NULL, offset, len);
#else
  if (uc->delete_surrounding_text_cb) {
    uc->delete_surrounding_text_cb(uc->ptr, offset, len);
  }
#endif
  return uim_scm_t();
}

void
uim_init_im_subrs(void)
{
  /**/
  uim_scm_init_subr_1("im-return-str", im_return_str);
  uim_scm_init_subr_1("im-return-str-list", im_return_str_list);
  /**/
  uim_scm_init_subr_2("im-commit",       im_commit);
  uim_scm_init_subr_1("im-commit-raw",   im_commit_raw);
  uim_scm_init_subr_2("im-get-raw-key-str", im_get_raw_key_str);
  uim_scm_init_subr_2("im-set-encoding", im_set_encoding);
  /**/
  uim_scm_init_subr_4("im-register-im", im_register_im);
  /**/
  uim_scm_init_subr_1("im-clear-preedit",    im_clear_preedit);
  uim_scm_init_subr_3("im-pushback-preedit", im_pushback_preedit);
  uim_scm_init_subr_1("im-update-preedit",   im_update_preedit);
  /**/
  uim_scm_init_subr_1("im-clear-mode-list",    im_clear_mode_list);
  uim_scm_init_subr_2("im-pushback-mode-list", im_pushback_mode_list);
  uim_scm_init_subr_1("im-update-mode-list",   im_update_mode_list);
  /**/
  uim_scm_init_subr_2("im-update-prop-label", im_update_prop_label);
  uim_scm_init_subr_2("im-update-prop-list",  im_update_prop_list);
  /**/
  uim_scm_init_subr_2("im-update-mode", im_update_mode);
  /**/
  uim_scm_init_subr_3("im-activate-candidate-selector",  im_activate_candidate_selector);
  uim_scm_init_subr_2("im-select-candidate", im_select_candidate);
  uim_scm_init_subr_2("im-shift-page-candidate", im_shift_page_candidate);
  uim_scm_init_subr_1("im-deactivate-candidate-selector", im_deactivate_candidate_selector);
  /**/
  uim_scm_init_subr_1("im-request-surrounding", im_request_surrounding);
  uim_scm_init_subr_3("im-delete-surrounding", im_delete_surrounding);
}
