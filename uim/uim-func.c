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

#include <ctype.h>
#include <iconv.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include "context.h"
#include "siod.h"

#define MAX_LENGTH_OF_INT_AS_STR (((sizeof(int) == 4) ? sizeof("-2147483648") : sizeof("-9223372036854775808")) - sizeof((char)'\0'))

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

/* release preedit segment in a context */
void
uim_release_preedit_segments(uim_context uc)
{
  int i;
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

int
uim_iconv_is_convertible(const char *tocode, const char *fromcode)
{
  iconv_t ic;

  if (!strcmp("UTF-8", fromcode) || !strcmp(tocode, fromcode)) {
    return 1;
  }
  /* TODO cache the result */
  ic = iconv_open(tocode, fromcode);
  if (ic == (iconv_t)-1) {
    return 0;
  }
  iconv_close(ic);
  return 1;
}

void *
uim_iconv_create(const char *tocode, const char *fromcode)
{
  iconv_t ic;

  ic = iconv_open(tocode, fromcode);
  if (ic == (iconv_t)-1) {
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
  len = strlen(str);
  buflen = (len * 6)+3;
  realbuf = alloca(buflen);
  outbuf = realbuf;
  inbuf = str;
  if (!ic) {
    return strdup(str);
  }
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
  repl_c_string(buf, 0, 1);

  /* Flush callback requests queued during scheme evaluation 
   * (To avoid C -> scheme -> C call, actual call is delayed to here)
   */
  if (!uc->cb_q.flushing) {
    uc->cb_q.flushing ++;
    uim_flush_cb(uc);
    uc->cb_q.flushing --;
  }
}

/* this is not a uim API, so did not name as uim_retrieve_context() */
static uim_context
retrieve_uim_context(LISP id)
{
  uim_context uc;
  if CONSP(id) {  /* passed as Scheme-side input context */
    id = car(id);
  }
  uc = uim_find_context(get_c_int(id));
  return uc;
}

static LISP
im_clear_preedit(LISP id)
{
  uim_context uc = retrieve_uim_context(id);
  uim_schedule_cb(uc, PREEDIT_CLEAR_CB, NULL, 0, 0);
  return false_sym;
}

static LISP
im_pushback_preedit(LISP id_, LISP attr_, LISP str_)
{
  uim_context uc = retrieve_uim_context(id_);
  char *str = NULL;
  int attr = get_c_int(attr_);
  if (str_) {
    str = uim_get_c_string(str_);
  }
  uim_schedule_cb(uc, PREEDIT_PUSHBACK_CB, str, attr, 0);
  return false_sym;
}

static LISP
im_update_preedit(LISP id)
{
  uim_context uc = retrieve_uim_context(id);
  uim_schedule_cb(uc, PREEDIT_UPDATE_CB, NULL, 0, 0);
  return false_sym;
}

/* ID, STR */
static LISP
im_commit(LISP id, LISP str_)
{
  uim_context uc = retrieve_uim_context(id);
  char *str = NULL;
  if STRINGP(str_) {
    str = uim_get_c_string(str_);
    uim_schedule_cb(uc, COMMIT_CB, str, 0, 0);
  }
  return false_sym;
}

static LISP
im_commit_raw(LISP id)
{
  uim_context uc = retrieve_uim_context(id);
  uc->commit_raw_flag = 1;
  return false_sym;
}

static LISP
im_get_raw_key_str(LISP key_, LISP key_state_)
{
  int key;
  int key_state = get_c_int(key_state_);
  char buf[2];
  
  if (INTNUMP(key_)) {
    key = get_c_int(key_);
  } else {
    return false_sym;
  }
  if ((key_state != 0 && key_state != UMod_Shift) ||
      key > 255) {
    return false_sym;
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
  return strcons(1, buf);
}

static LISP
im_set_encoding(LISP id, LISP enc)
{
  char *e = uim_get_c_string(enc);
  uim_context uc = retrieve_uim_context(id);

  if(!uc)
    return false_sym;

  if (uc->conv) {
    uc->conv_if->release(uc->conv);
  }
  if (!strcmp(uc->encoding, e)) {
    free(e);
    uc->conv = 0;
    return false_sym;
  }
  uc->conv = uc->conv_if->create(uc->encoding, e);
  free(e);
  return false_sym;
}

static LISP
im_clear_mode_list(LISP id)
{
  int i;
  uim_context uc = retrieve_uim_context(id);

  if(!uc)
    return false_sym;

  for (i = 0; i < uc->nr_modes; i++) {
    if (uc->modes[i]) {
      free(uc->modes[i]);
    }
  }
  if (uc->modes) {
    free(uc->modes);
    uc->modes = NULL;
  }
  uc->nr_modes = 0;
  return false_sym;
}

static LISP
im_pushback_mode_list(LISP id, LISP str)
{
  char *s;
  uim_context uc = retrieve_uim_context(id);

  if(!uc)
    return false_sym;

  uc->modes = realloc(uc->modes,
		      sizeof(char *)*(uc->nr_modes+1));
  s = uim_get_c_string(str);
  uc->modes[uc->nr_modes] = uc->conv_if->convert(uc->conv, s);
  free(s);
  uc->nr_modes ++;
  return false_sym;
}

static LISP
im_update_mode_list(LISP id)
{
  uim_context uc = retrieve_uim_context(id);

  if(!uc)
    return false_sym;

  uim_schedule_cb(uc, MODE_LIST_UPDATE_CB, NULL, 0, 0);
  return false_sym;
}

static LISP
im_update_prop_list(LISP id, LISP prop_)
{
  uim_context uc = retrieve_uim_context(id);
  char *prop     = uim_get_c_string(prop_);

  if(uc)
    uim_schedule_cb(uc, PROP_LIST_UPDATE_CB, NULL, 0, 0);
  
  if(uc && uc->propstr)
    free(uc->propstr);
      
  uc->propstr = uc->conv_if->convert(uc->conv, prop);
  
  free(prop);

  return false_sym;
}


static LISP
im_update_prop_label(LISP id, LISP prop_)
{
  uim_context uc = retrieve_uim_context(id);
  char *prop     = uim_get_c_string(prop_);
    
  if(uc) {
    uim_schedule_cb(uc, PROP_LABEL_UPDATE_CB, NULL, 0, 0);
  } else {
    return false_sym;
  }

  if(uc && uc->proplabelstr)
    free(uc->proplabelstr);
  
  uc->proplabelstr = uc->conv_if->convert(uc->conv, prop);
  
  free(prop);

  return false_sym;
}

static LISP
im_update_mode(LISP id, LISP mode_)
{
  int mode = get_c_int(mode_);
  uim_context uc = retrieve_uim_context(id);

  if(!uc)
    return false_sym;

  uc->mode = mode;
  uim_schedule_cb(uc, MODE_UPDATE_CB, NULL, mode, 0);
  return false_sym;
}

static char *
get_im_lang(char *name)
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

static LISP
im_register_im(LISP name, LISP lang, LISP enc, LISP s_desc)
{
  char *im_name = get_c_string(name);
  char *lang_name = get_c_string(lang);
  char *encoding_name = get_c_string(enc);
  char *short_desc = get_c_string(s_desc);
  if (get_im_lang(im_name)) {
    /* avoid duplicate register */
    return false_sym;
  }
  uim_im_array = realloc(uim_im_array,
			 sizeof(struct uim_im) *
			 (uim_nr_im + 1));
  uim_im_array[uim_nr_im].lang = strdup(lang_name);
  uim_im_array[uim_nr_im].name = strdup(im_name);
  uim_im_array[uim_nr_im].encoding = strdup(encoding_name);
  uim_im_array[uim_nr_im].short_desc = strdup(short_desc);
  uim_nr_im ++;
  return true_sym;
}

static LISP
im_activate_candidate_selector(LISP id_, LISP nr_, LISP display_limit_)
{
  uim_context uc = retrieve_uim_context(id_);
  int display_limit = get_c_int(display_limit_);
  int nr = get_c_int(nr_);
  uim_schedule_cb(uc, CAND_ACTIVATE_CB, NULL, nr, display_limit);
  return false_sym;
}

static LISP
im_select_candidate(LISP id_, LISP idx_)
{
  uim_context uc = retrieve_uim_context(id_);
  int idx = get_c_int(idx_);
  uim_schedule_cb(uc, CAND_SELECT_CB, NULL, idx, 0);
  return false_sym;
}


/* My naming sense seems bad... */
static LISP
im_shift_page_candidate(LISP id_, LISP dir_)
{
  uim_context uc = retrieve_uim_context(id_);
  int dir;

  if FALSEP(dir_)
    dir = 0;
  else
    dir = 1;
    
  uim_schedule_cb(uc, CAND_SHIFT_PAGE_CB, NULL, dir, 0);
  return false_sym;
}

static LISP
im_deactivate_candidate_selector(LISP id_)
{
  uim_context uc = retrieve_uim_context(id_);
  uim_schedule_cb(uc, CAND_DEACTIVATE_CB, NULL, 0, 0);
  return false_sym;
}

static LISP
im_return_str(LISP str_)
{
  if (uim_return_str) {
    free(uim_return_str);
    uim_return_str = NULL;
  }
  if STRINGP(str_) {
    uim_return_str = uim_get_c_string(str_);
  }
  return false_sym;
}

static LISP
im_return_str_list(LISP str_list_)
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

  while (NNULLP(str_list_) && i < (int)UIM_RETURN_STR_LIST_SIZE) {
    LISP str_ = CAR(str_list_);
    if STRINGP(str_) {
      uim_return_str_list[i] = uim_get_c_string(str_);
    }

    i++;
    str_list_ = CDR(str_list_);
  }
  uim_return_str_list[i] = NULL;
  return false_sym;
}

static LISP
im_request_surrounding(LISP id_)
{
  uim_context uc = retrieve_uim_context(id_);
  if (!uc->request_surrounding_text_cb) {
    return NIL;
  }
  uim_schedule_cb(uc, REQUEST_SURROUNDING_CB, NULL, 0, 0);
  return true_sym;
}

static LISP
im_delete_surrounding(LISP id_, LISP offset_, LISP len_)
{
  uim_context uc = retrieve_uim_context(id_);
  int offset = get_c_int(offset_);
  int len = get_c_int(len_);
  if (!uc->delete_surrounding_text_cb) {
    return NIL;
  }
  uim_schedule_cb(uc, DELETE_SURROUNDING_CB, NULL, offset, len);
  return true_sym;
}

void
uim_init_im_subrs(void)
{
  /**/
  init_subr_1("im-return-str", im_return_str);
  init_subr_1("im-return-str-list", im_return_str_list);
  /**/
  init_subr_2("im-commit",       im_commit);
  init_subr_1("im-commit-raw",   im_commit_raw);
  init_subr_2("im-get-raw-key-str", im_get_raw_key_str);
  init_subr_2("im-set-encoding", im_set_encoding);
  /**/
  init_subr_4("im-register-im", im_register_im);
  /**/
  init_subr_1("im-clear-preedit",    im_clear_preedit);
  init_subr_3("im-pushback-preedit", im_pushback_preedit);
  init_subr_1("im-update-preedit",   im_update_preedit);
  /**/
  init_subr_1("im-clear-mode-list",    im_clear_mode_list);
  init_subr_2("im-pushback-mode-list", im_pushback_mode_list);
  init_subr_1("im-update-mode-list",   im_update_mode_list);
  /**/
  init_subr_2("im-update-prop-label", im_update_prop_label);
  init_subr_2("im-update-prop-list",  im_update_prop_list);
  /**/
  init_subr_2("im-update-mode", im_update_mode);
  /**/
  init_subr_3("im-activate-candidate-selector",  im_activate_candidate_selector);
  init_subr_2("im-select-candidate", im_select_candidate);
  init_subr_2("im-shift-page-candidate", im_shift_page_candidate);
  init_subr_1("im-deactivate-candidate-selector", im_deactivate_candidate_selector);
  /**/
  init_subr_1("im-request-surrounding", im_request_surrounding);
  init_subr_3("im-delete-surrounding", im_delete_surrounding);
}
