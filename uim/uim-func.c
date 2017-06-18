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

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "uim-internal.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-im-switcher.h"


#define TEXT_EMPTYP(txt) (!(txt) || !(txt)[0])


/* this is not a uim API, so did not name as uim_retrieve_context() */
static uim_context
retrieve_uim_context(uim_lisp c)
{
  uim_context uc;

  if (CONSP(c))  /* passed as Scheme-side input context */
    c = CAR(c);

  uc = C_PTR(c);
  assert(uc);

  return uc;
}

/* extract Scheme IM context from Scheme-object-wrapped uim_context */
static uim_lisp
im_retrieve_context(uim_lisp uc_)
{
  uim_context uc;

  uc = C_PTR(uc_);
  assert(uc);

  return uc->sc;
}

static uim_lisp
im_convertiblep(uim_lisp uc_, uim_lisp im_encoding_)
{
  uim_context uc;
  const char *im_encoding;
  uim_bool convertiblep;

  uc = retrieve_uim_context(uc_);
  im_encoding = REFER_C_STR(im_encoding_);
  convertiblep = uc->conv_if->is_convertible(uc->client_encoding, im_encoding);

  return MAKE_BOOL(convertiblep);
}

static uim_lisp
im_clear_preedit(uim_lisp uc_)
{
  uim_context uc;

  uc = retrieve_uim_context(uc_);
  if (uc->preedit_clear_cb)
    uc->preedit_clear_cb(uc->ptr);

  return uim_scm_f();
}

static uim_lisp
im_pushback_preedit(uim_lisp uc_, uim_lisp attr_, uim_lisp str_)
{
  uim_context uc;
  const char *str;
  char *converted_str;
  int attr;

  uc = retrieve_uim_context(uc_);
  attr = C_INT(attr_);
  str = REFER_C_STR(str_);

  converted_str = uc->conv_if->convert(uc->outbound_conv, str);
  if (uc->preedit_pushback_cb)
    uc->preedit_pushback_cb(uc->ptr, attr, converted_str);
  free(converted_str);

  return uim_scm_f();
}

static uim_lisp
im_update_preedit(uim_lisp uc_)
{
  uim_context uc;

  uc = retrieve_uim_context(uc_);
  if (uc->preedit_update_cb)
    uc->preedit_update_cb(uc->ptr);

  return uim_scm_f();
}

static uim_lisp
im_commit(uim_lisp uc_, uim_lisp str_)
{
  uim_context uc;
  const char *str;
  char *converted_str;

  uc = retrieve_uim_context(uc_);
  str = REFER_C_STR(str_);

  converted_str = uc->conv_if->convert(uc->outbound_conv, str);
  if (uc->commit_cb)
    uc->commit_cb(uc->ptr, converted_str);
  free(converted_str);

  return uim_scm_f();
}

static uim_lisp
im_set_encoding(uim_lisp uc_, uim_lisp enc_)
{
  uim_context uc;
  const char *enc;

  uc = retrieve_uim_context(uc_);
  enc = REFER_C_STR(enc_);

  uim_set_encoding(uc, enc);

  return uim_scm_f();
}

void
uim_set_encoding(uim_context uc, const char *enc)
{
  assert(uc);
  assert(enc);

  if (uc->outbound_conv)
    uc->conv_if->release(uc->outbound_conv);
  if (uc->inbound_conv)
    uc->conv_if->release(uc->inbound_conv);

  if (!strcmp(uc->client_encoding, enc)) {
    uc->outbound_conv = NULL;
    uc->inbound_conv  = NULL;
  } else {
    uc->outbound_conv = uc->conv_if->create(uc->client_encoding, enc);
    uc->inbound_conv  = uc->conv_if->create(enc, uc->client_encoding);
  }
}

static uim_lisp
im_clear_mode_list(uim_lisp uc_)
{
  uim_context uc;
  int i;

  uc = retrieve_uim_context(uc_);

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
im_pushback_mode_list(uim_lisp uc_, uim_lisp str_)
{
  uim_context uc;
  const char *str;

  uc = retrieve_uim_context(uc_);
  str = REFER_C_STR(str_);

  uc->modes = uim_realloc(uc->modes, sizeof(char *) * (uc->nr_modes + 1));
  uc->modes[uc->nr_modes] = uc->conv_if->convert(uc->outbound_conv, str);
  uc->nr_modes++;

  return uim_scm_f();
}

static uim_lisp
im_update_mode_list(uim_lisp uc_)
{
  uim_context uc;

  uc = retrieve_uim_context(uc_);

  if (uc->mode_list_update_cb)
    uc->mode_list_update_cb(uc->ptr);

  return uim_scm_f();
}

static uim_lisp
im_update_prop_list(uim_lisp uc_, uim_lisp prop_)
{
  uim_context uc;
  const char *prop;

  uc = retrieve_uim_context(uc_);
  prop = REFER_C_STR(prop_);
  
  free(uc->propstr);
  uc->propstr = uc->conv_if->convert(uc->outbound_conv, prop);

  if (uc->prop_list_update_cb)
    uc->prop_list_update_cb(uc->ptr, uc->propstr);

  return uim_scm_f();
}

static uim_lisp
im_update_mode(uim_lisp uc_, uim_lisp mode_)
{
  uim_context uc;
  int mode;

  uc = retrieve_uim_context(uc_);
  mode = C_INT(mode_);

  uc->mode = mode;
  if (uc->mode_update_cb)
    uc->mode_update_cb(uc->ptr, mode);

  return uim_scm_f();
}

static uim_lisp
im_activate_candidate_selector(uim_lisp uc_,
                               uim_lisp nr_, uim_lisp display_limit_)
{
  uim_context uc;
  int nr, display_limit;

  uc = retrieve_uim_context(uc_);
  nr = C_INT(nr_);
  display_limit = C_INT(display_limit_);

  if (uc->candidate_selector_activate_cb)
    uc->candidate_selector_activate_cb(uc->ptr, nr, display_limit);

  return uim_scm_f();
}

static uim_lisp
im_delay_activate_candidate_selector(uim_lisp uc_, uim_lisp delay_)
{
  uim_context uc;
  int delay;

  uc = retrieve_uim_context(uc_);
  delay = C_INT(delay_);

  if (uc->candidate_selector_delay_activate_cb)
    uc->candidate_selector_delay_activate_cb(uc->ptr, delay);

  return uim_scm_f();
}

static uim_lisp
im_select_candidate(uim_lisp uc_, uim_lisp idx_)
{
  uim_context uc;
  int idx;

  uc = retrieve_uim_context(uc_);
  idx = C_INT(idx_);

  if (uc->candidate_selector_select_cb)
    uc->candidate_selector_select_cb(uc->ptr, idx);

  return uim_scm_f();
}


/* My naming sense seems bad... */
static uim_lisp
im_shift_page_candidate(uim_lisp uc_, uim_lisp dir_)
{
  uim_context uc;
  int dir;

  uc = retrieve_uim_context(uc_);
  dir = (C_BOOL(dir_)) ? 1 : 0;
    
  if (uc->candidate_selector_shift_page_cb)
    uc->candidate_selector_shift_page_cb(uc->ptr, dir);

  return uim_scm_f();
}

static uim_lisp
im_deactivate_candidate_selector(uim_lisp uc_)
{
  uim_context uc;

  uc = retrieve_uim_context(uc_);

  if (uc->candidate_selector_deactivate_cb)
    uc->candidate_selector_deactivate_cb(uc->ptr);

  return uim_scm_f();
}

static uim_lisp
im_delay_activate_candidate_selector_supportedp(uim_lisp uc_)
{
  uim_context uc;

  uc = retrieve_uim_context(uc_);

  if (uc->candidate_selector_delay_activate_cb)
    return uim_scm_t();
  return uim_scm_f();
}

static uim_lisp
im_acquire_text(uim_lisp uc_, uim_lisp text_id_, uim_lisp origin_,
		uim_lisp former_len_, uim_lisp latter_len_)
{
  uim_context uc;
  int err, former_len, latter_len;
  enum UTextArea text_id;
  enum UTextOrigin origin;
  char *former, *latter, *cv_former, *cv_latter;
  uim_lisp former_, latter_;

  uc = retrieve_uim_context(uc_);

  if (!uc->acquire_text_cb)
    return uim_scm_f();

  text_id = C_INT(text_id_);
  origin = C_INT(origin_);
  former_len = C_INT(former_len_);
  latter_len = C_INT(latter_len_);

  err = uc->acquire_text_cb(uc->ptr, text_id, origin,
                            former_len, latter_len, &former, &latter);
  if (err)
    return uim_scm_f();

  /* FIXME: string->list is not applied here for each text part. This
   * interface should be revised when SigScheme has been introduced to
   * uim. Until then, perform character separation by each input methods if
   * needed.  -- YamaKen 2006-10-07 */
  cv_former = uc->conv_if->convert(uc->inbound_conv, former);
  cv_latter = uc->conv_if->convert(uc->inbound_conv, latter);
  free(former);
  free(latter);
  former_
    = (TEXT_EMPTYP(cv_former)) ? uim_scm_null() : LIST1(MAKE_STR_DIRECTLY(cv_former));
  latter_
    = (TEXT_EMPTYP(cv_latter)) ? uim_scm_null() : LIST1(MAKE_STR_DIRECTLY(cv_latter));

  return uim_scm_callf("ustr-new", "oo", former_, latter_);
}

static uim_lisp
im_delete_text(uim_lisp uc_, uim_lisp text_id_, uim_lisp origin_,
	       uim_lisp former_len_, uim_lisp latter_len_)
{
  uim_context uc;
  int err, former_len, latter_len;
  enum UTextArea text_id;
  enum UTextOrigin origin;

  uc = retrieve_uim_context(uc_);

  if (!uc->delete_text_cb)
    return uim_scm_f();

  text_id = C_INT(text_id_);
  origin = C_INT(origin_);
  former_len = C_INT(former_len_);
  latter_len = C_INT(latter_len_);

  err = uc->delete_text_cb(uc->ptr, text_id, origin, former_len, latter_len);

  return MAKE_BOOL(!err);
}

static uim_lisp
raise_configuration_change(uim_lisp uc_)
{
  uim_context uc;

  uc = retrieve_uim_context(uc_);

  if (uc->configuration_changed_cb)
    uc->configuration_changed_cb(uc->ptr);

  return uim_scm_t();
}

static uim_lisp
switch_app_global_im(uim_lisp uc_, uim_lisp name_)
{
  uim_context uc;
  const char *name;

  uc = retrieve_uim_context(uc_);
  name = REFER_C_STR(name_);

  if (uc->switch_app_global_im_cb)
    uc->switch_app_global_im_cb(uc->ptr, name);

  return uim_scm_t();
}

static uim_lisp
switch_system_global_im(uim_lisp uc_, uim_lisp name_)
{
  uim_context uc;
  const char *name;

  uc = retrieve_uim_context(uc_);
  name = REFER_C_STR(name_);

  if (uc->switch_system_global_im_cb)
    uc->switch_system_global_im_cb(uc->ptr, name);

  return uim_scm_t();
}

void
uim_init_im_subrs(void)
{
  uim_scm_init_proc1("im-retrieve-context", im_retrieve_context);
  uim_scm_init_proc2("im-set-encoding",     im_set_encoding);
  uim_scm_init_proc2("im-convertible?",     im_convertiblep);

  uim_scm_init_proc2("im-commit",           im_commit);
  uim_scm_init_proc1("im-clear-preedit",    im_clear_preedit);
  uim_scm_init_proc3("im-pushback-preedit", im_pushback_preedit);
  uim_scm_init_proc1("im-update-preedit",   im_update_preedit);

  uim_scm_init_proc3("im-activate-candidate-selector",
		     im_activate_candidate_selector);
  uim_scm_init_proc2("im-select-candidate", im_select_candidate);
  uim_scm_init_proc2("im-shift-page-candidate", im_shift_page_candidate);
  uim_scm_init_proc1("im-deactivate-candidate-selector",
		     im_deactivate_candidate_selector);

  uim_scm_init_proc2("im-delay-activate-candidate-selector",
		     im_delay_activate_candidate_selector);
  uim_scm_init_proc1("im-delay-activate-candidate-selector-supported?",
		     im_delay_activate_candidate_selector_supportedp);

  uim_scm_init_proc5("im-acquire-text-internal", im_acquire_text);
  uim_scm_init_proc5("im-delete-text-internal", im_delete_text);

  uim_scm_init_proc1("im-clear-mode-list",    im_clear_mode_list);
  uim_scm_init_proc2("im-pushback-mode-list", im_pushback_mode_list);
  uim_scm_init_proc1("im-update-mode-list",   im_update_mode_list);
  uim_scm_init_proc2("im-update-mode",        im_update_mode);

  uim_scm_init_proc2("im-update-prop-list", im_update_prop_list);

  uim_scm_init_proc1("im-raise-configuration-change",
		     raise_configuration_change);
  uim_scm_init_proc2("im-switch-app-global-im", switch_app_global_im);
  uim_scm_init_proc2("im-switch-system-global-im", switch_system_global_im);
}
