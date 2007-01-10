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

#include <config.h>

#include <ctype.h>
#include <iconv.h>
#include <string.h>
#include <stdlib.h>

#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#include "uim-internal.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-encoding.h"
#include "uim-util.h"
#include "uim-im-switcher.h"

static const char **uim_get_encoding_alias(const char *encoding);

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

/* release preedit segment in a context */
void
uim_release_preedit_segments(uim_context uc)
{
  int i;

  if (!uc)
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
  ic = (iconv_t)uim_iconv_open(tocode, fromcode);
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

void *
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
  return (void *)cd;
}

void *
uim_iconv_create(const char *tocode, const char *fromcode)
{
  iconv_t ic;

  if (check_encoding_equivalence(tocode, fromcode))
    return NULL;

  ic = (iconv_t)uim_iconv_open(tocode, fromcode);
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
  if (!str)
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

/* this is not a uim API, so did not name as uim_retrieve_context() */
static uim_context
retrieve_uim_context(uim_lisp c)
{
  uim_context uc;

  if (CONSP(c))  /* passed as Scheme-side input context */
    c = CAR(c);

  uc = uim_scm_c_ptr(c);
  return uc;
}

/* extract Scheme IM context from Scheme-object-wrapped uim_context */
static uim_lisp
im_retrieve_context(uim_lisp uc_)
{
  uim_context uc;

  uc = uim_scm_c_ptr(uc_);
  return uc->sc;
}

static uim_lisp
im_convertiblep(uim_lisp id, uim_lisp im_encoding_)
{
  uim_context uc;
  const char *im_encoding;

  uc = retrieve_uim_context(id);
  im_encoding = uim_scm_refer_c_str(im_encoding_);
  return MAKE_BOOL(uc->conv_if->is_convertible(uc->client_encoding,
                                               im_encoding));
}

static uim_lisp
im_clear_preedit(uim_lisp id)
{
  uim_context uc = retrieve_uim_context(id);
  uim_release_preedit_segments(uc);
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
  {
    char *s;
    s = uc->conv_if->convert(uc->outbound_conv, str);
    pushback_preedit_segment(uc, attr, s);
  }
  return uim_scm_f();
}

static uim_lisp
im_update_preedit(uim_lisp id)
{
  uim_context uc = retrieve_uim_context(id);
  uim_update_preedit_segments(uc);
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
    {
      char *s;
      s = uc->conv_if->convert(uc->outbound_conv, str);
      if (uc->commit_cb) {
	uc->commit_cb(uc->ptr, s);
      }
      free(s);
    }
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

  if (!uc)
    return uim_scm_f();

  if (uc->outbound_conv) {
    uc->conv_if->release(uc->outbound_conv);
  }
  if (uc->inbound_conv) {
    uc->conv_if->release(uc->inbound_conv);
  }
  if (!strcmp(uc->client_encoding, e)) {
    uc->outbound_conv = NULL;
    uc->inbound_conv = NULL;
    return uim_scm_f();
  }
  uc->outbound_conv = uc->conv_if->create(uc->client_encoding, e);
  uc->inbound_conv = uc->conv_if->create(e, uc->client_encoding);

  return uim_scm_f();
}

static uim_lisp
im_clear_mode_list(uim_lisp id)
{
  int i;
  uim_context uc = retrieve_uim_context(id);

  if (!uc)
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

  if (!uc)
    return uim_scm_f();

  uc->modes = realloc(uc->modes,
		      sizeof(char *)*(uc->nr_modes+1));
  s = uim_scm_refer_c_str(str);
  uc->modes[uc->nr_modes] = uc->conv_if->convert(uc->outbound_conv, s);
  uc->nr_modes ++;
  return uim_scm_f();
}

static uim_lisp
im_update_mode_list(uim_lisp id)
{
  uim_context uc = retrieve_uim_context(id);

  if (!uc)
    return uim_scm_f();

  if (uc->mode_list_update_cb) {
    uc->mode_list_update_cb(uc->ptr);
  }
  return uim_scm_f();
}

static uim_lisp
im_update_prop_list(uim_lisp id, uim_lisp prop_)
{
  uim_context uc = retrieve_uim_context(id);
  const char *prop = uim_scm_refer_c_str(prop_);

  if (!uc)
    return uim_scm_f();
  
  if (uc && uc->propstr)
    free(uc->propstr);
      
  uc->propstr = uc->conv_if->convert(uc->outbound_conv, prop);

  if (uc->prop_list_update_cb)
    uc->prop_list_update_cb(uc->ptr, uc->propstr);

  return uim_scm_f();
}

static uim_lisp
im_update_mode(uim_lisp id, uim_lisp mode_)
{
  int mode = uim_scm_c_int(mode_);
  uim_context uc = retrieve_uim_context(id);

  if (!uc)
    return uim_scm_f();

  uc->mode = mode;
  if (uc->mode_update_cb) {
    uc->mode_update_cb(uc->ptr, mode);
  }
  return uim_scm_f();
}

static uim_lisp
im_activate_candidate_selector(uim_lisp id_, uim_lisp nr_, uim_lisp display_limit_)
{
  uim_context uc = retrieve_uim_context(id_);
  int display_limit = uim_scm_c_int(display_limit_);
  int nr = uim_scm_c_int(nr_);
  if (uc->candidate_selector_activate_cb) {
    uc->candidate_selector_activate_cb(uc->ptr, nr, display_limit);
  }
  return uim_scm_f();
}

static uim_lisp
im_select_candidate(uim_lisp id_, uim_lisp idx_)
{
  uim_context uc = retrieve_uim_context(id_);
  int idx = uim_scm_c_int(idx_);
  if (uc->candidate_selector_select_cb) {
    uc->candidate_selector_select_cb(uc->ptr, idx);
  }
  return uim_scm_f();
}


/* My naming sense seems bad... */
static uim_lisp
im_shift_page_candidate(uim_lisp id_, uim_lisp dir_)
{
  uim_context uc = retrieve_uim_context(id_);
  int dir;

  if UIM_SCM_FALSEP(dir_)
    dir = 0;
  else
    dir = 1;
    
  if (uc->candidate_selector_shift_page_cb) {
    uc->candidate_selector_shift_page_cb(uc->ptr, dir);
  }
  return uim_scm_f();
}

static uim_lisp
im_deactivate_candidate_selector(uim_lisp id_)
{
  uim_context uc = retrieve_uim_context(id_);
  if (uc->candidate_selector_deactivate_cb) {
    uc->candidate_selector_deactivate_cb(uc->ptr);
  }
  return uim_scm_f();
}

static uim_lisp
im_acquire_text(uim_lisp id_, uim_lisp text_id_, uim_lisp origin_,
		uim_lisp former_len_, uim_lisp latter_len_)
{
  uim_context uc = retrieve_uim_context(id_);
  int err, former_len, latter_len;
  enum UTextArea text_id;
  enum UTextOrigin origin;
  char *former, *latter, *im_former, *im_latter;
  uim_bool is_former_null, is_latter_null;
  uim_lisp ret;

  if (!uc->acquire_text_cb)
    return uim_scm_f();

  former_len = uim_scm_c_int(former_len_);
  latter_len = uim_scm_c_int(latter_len_);

  text_id = uim_scm_c_int(text_id_);
  origin = uim_scm_c_int(origin_);

  err = uc->acquire_text_cb(uc->ptr, text_id, origin, former_len, latter_len,
			    &former, &latter);
  if (err)
    return uim_scm_f();

  /* FIXME: string->list is not applied here for each text part. This
   * interface should be revised when SigScheme has been introduced to
   * uim. Until then, perform character separation by each input methods if
   * needed.  -- YamaKen 2006-10-07 */
  im_former = uc->conv_if->convert(uc->inbound_conv, former);
  im_latter = uc->conv_if->convert(uc->inbound_conv, latter);

  is_former_null = (im_former && !im_former[0]);
  is_latter_null = (im_latter && !im_former[0]);
  if (is_former_null && is_latter_null)
    ret = uim_scm_call2(MAKE_SYM("ustr-new"),
                        uim_scm_null(), uim_scm_null());
  else if (is_former_null && !is_latter_null)
    ret = uim_scm_call2(MAKE_SYM("ustr-new"),
                        uim_scm_null(), LIST1(MAKE_STR(im_latter)));
  else if (!is_former_null && is_latter_null)
    ret = uim_scm_call2(MAKE_SYM("ustr-new"),
                        LIST1(MAKE_STR(im_former)), uim_scm_null());
  else
    ret = uim_scm_call2(MAKE_SYM("ustr-new"),
                        LIST1(MAKE_STR(im_former)),
                        LIST1(MAKE_STR(im_latter)));
  free(former);
  free(latter);
  free(im_former);
  free(im_latter);

  return ret;
}

static uim_lisp
im_delete_text(uim_lisp id_, uim_lisp text_id_, uim_lisp origin_,
	       uim_lisp former_len_, uim_lisp latter_len_)
{
  uim_context uc = retrieve_uim_context(id_);
  int err, former_len, latter_len;
  enum UTextArea text_id;
  enum UTextOrigin origin;

  former_len = uim_scm_c_int(former_len_);
  latter_len = uim_scm_c_int(latter_len_);
  text_id = uim_scm_c_int(text_id_);
  origin = uim_scm_c_int(origin_);

  if (!uc->delete_text_cb)
    return uim_scm_f();
  err = uc->delete_text_cb(uc->ptr, text_id, origin, former_len, latter_len);

  return uim_scm_make_bool(!err);
}

static uim_lisp
switch_im(uim_lisp id_, uim_lisp name_)
{
  uim_context uc;
  const char *name;

  uc = retrieve_uim_context(id_);
  name= uim_scm_refer_c_str(name_);

  uim_switch_im(uc, name);
  if (uc->configuration_changed_cb)
    uc->configuration_changed_cb(uc->ptr);

  return uim_scm_t();
}

static uim_lisp
switch_app_global_im(uim_lisp id_, uim_lisp name_)
{
  uim_context uc;
  const char *name;

  uc = retrieve_uim_context(id_);
  name = uim_scm_refer_c_str(name_);

  if (uc->switch_app_global_im_cb)
    uc->switch_app_global_im_cb(uc->ptr, name);

  return uim_scm_t();
}

static uim_lisp
switch_system_global_im(uim_lisp id_, uim_lisp name_)
{
  uim_context uc;
  const char *name;

  uc = retrieve_uim_context(id_);
  name = uim_scm_refer_c_str(name_);

  if (uc->switch_system_global_im_cb)
    uc->switch_system_global_im_cb(uc->ptr, name);

  return uim_scm_t();
}

void
uim_init_im_subrs(void)
{
  uim_scm_init_subr_1("im-retrieve-context", im_retrieve_context);
  uim_scm_init_subr_2("im-convertible?", im_convertiblep);
  /**/
  uim_scm_init_subr_2("im-commit",       im_commit);
  uim_scm_init_subr_1("im-commit-raw",   im_commit_raw);
  uim_scm_init_subr_2("im-get-raw-key-str", im_get_raw_key_str);
  uim_scm_init_subr_2("im-set-encoding", im_set_encoding);
  /**/
  uim_scm_init_subr_1("im-clear-preedit",    im_clear_preedit);
  uim_scm_init_subr_3("im-pushback-preedit", im_pushback_preedit);
  uim_scm_init_subr_1("im-update-preedit",   im_update_preedit);
  /**/
  uim_scm_init_subr_1("im-clear-mode-list",    im_clear_mode_list);
  uim_scm_init_subr_2("im-pushback-mode-list", im_pushback_mode_list);
  uim_scm_init_subr_1("im-update-mode-list",   im_update_mode_list);
  /**/
  uim_scm_init_subr_2("im-update-prop-list",  im_update_prop_list);
  /**/
  uim_scm_init_subr_2("im-update-mode", im_update_mode);
  /**/
  uim_scm_init_subr_3("im-activate-candidate-selector",  im_activate_candidate_selector);
  uim_scm_init_subr_2("im-select-candidate", im_select_candidate);
  uim_scm_init_subr_2("im-shift-page-candidate", im_shift_page_candidate);
  uim_scm_init_subr_1("im-deactivate-candidate-selector", im_deactivate_candidate_selector);
  /**/
  uim_scm_init_subr_5("im-acquire-text-internal", im_acquire_text);
  uim_scm_init_subr_5("im-delete-text-internal", im_delete_text);
  /**/
  uim_scm_init_subr_2("im-switch-im", switch_im);
  uim_scm_init_subr_2("im-switch-app-global-im", switch_app_global_im);
  uim_scm_init_subr_2("im-switch-system-global-im", switch_system_global_im);
}
