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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "uim.h"
#include "uim-internal.h"
#include "uim-util.h"
#include "uim-im-switcher.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"


#define OK 0

static uim_lisp get_nth_im(uim_context uc, int nth);

static uim_bool uim_initialized;


/****************************************************************
 * Core APIs                                                    *
 ****************************************************************/
int
uim_init(void)
{
  char *scm_files, *env;

  if (uim_initialized)
    return OK;

  env = getenv("LIBUIM_VERBOSE");
  uim_scm_init(env);

  uim_init_im_subrs();
  uim_init_intl_subrs();
  uim_init_util_subrs();
  uim_init_key_subrs();
  uim_init_rk_subrs();
  uim_init_plugin();
#ifdef ENABLE_ANTHY_STATIC
  uim_anthy_plugin_instance_init();
#endif

  if (uim_issetugid()) {
    scm_files = SCM_FILES;
  } else {
    scm_files = getenv("LIBUIM_SCM_FILES");
    scm_files = (scm_files) ? scm_files : SCM_FILES;
  }
  uim_scm_set_lib_path(scm_files);

  uim_scm_require_file("init.scm");

  uim_initialized = UIM_TRUE;

  return OK;
}

void
uim_quit(void)
{
  if (!uim_initialized)
    return;

  uim_quit_plugin();
#ifdef ENABLE_ANTHY_STATIC
  uim_anthy_plugin_instance_quit();
#endif
  uim_scm_quit();
  uim_initialized = UIM_FALSE;
}

uim_context
uim_create_context(void *ptr,
		   const char *enc,
		   const char *lang,
		   const char *engine,
		   struct uim_code_converter *conv,
		   void (*commit_cb)(void *ptr, const char *str))
{
  uim_context uc;
  uim_lisp lang_, engine_;

  if (!uim_initialized) {
    fprintf(stderr, "uim_create_context() before uim_init()\n");
    return NULL;
  }
  if (!uim_scm_is_alive())
    return NULL;

  uc = malloc(sizeof(*uc));
  if (!uc)
    return NULL;

  /* encoding handlings */
  if (!enc)
    enc = "UTF-8";
  uc->client_encoding = strdup(enc);
  uc->conv_if = (conv) ? conv : uim_iconv;
  uc->outbound_conv = NULL;
  uc->inbound_conv = NULL;

  /* variables */
  uc->is_enabled = UIM_TRUE;
  uc->nr_modes = 0;
  uc->modes = NULL;
  uc->mode = 0;
  uc->propstr = NULL;

  /* core callbacks */
  uc->commit_cb = commit_cb;
  uc->preedit_clear_cb = NULL;
  uc->preedit_pushback_cb = NULL;
  uc->preedit_update_cb = NULL;
  uc->candidate_selector_activate_cb = NULL;
  uc->candidate_selector_select_cb = NULL;
  uc->candidate_selector_shift_page_cb = NULL;
  uc->candidate_selector_deactivate_cb = NULL;
  uc->acquire_text_cb = NULL;
  uc->delete_text_cb = NULL;

  /* non-core callbacks */
  uc->mode_list_update_cb = NULL;
  uc->mode_update_cb = NULL;
  uc->prop_list_update_cb  = NULL;
  uc->configuration_changed_cb = NULL;
  uc->switch_app_global_im_cb = NULL;
  uc->switch_system_global_im_cb = NULL;

  /* foreign context objects */
  uc->ptr = ptr;

  /* FIXME: GC unsafe */
  lang_ = (lang) ? MAKE_SYM(lang) : uim_scm_f();
  engine_ = (engine) ? MAKE_SYM(engine) : uim_scm_f();
  uc->sc = uim_scm_f(); /* failsafe */
  uc->sc = uim_scm_callf("create-context", "poo", uc, lang_, engine_);
  uim_scm_gc_protect(&uc->sc);
  uim_scm_callf("setup-context", "o", uc->sc);

  return uc;
}

void
uim_release_context(uim_context uc)
{
  int i;

  assert(uc);

  uim_scm_callf("release-context", "p", uc);
  uim_scm_gc_unprotect(&uc->sc);
  if (uc->outbound_conv)
    uc->conv_if->release(uc->outbound_conv);
  if (uc->inbound_conv)
    uc->conv_if->release(uc->inbound_conv);
  for (i = 0; i < uc->nr_modes; i++) {
    free(uc->modes[i]);
    uc->modes[i] = NULL;
  }
  free(uc->propstr);
  free(uc->modes);
  free(uc->client_encoding);
  free(uc);
}

void
uim_reset_context(uim_context uc)
{
  assert(uc);

  uim_scm_callf("reset-handler", "p", uc);
}

void
uim_focus_in_context(uim_context uc)
{
  assert(uc);

  uim_scm_callf("focus-in-handler", "p", uc);
}

void
uim_focus_out_context(uim_context uc)
{
  assert(uc);

  uim_scm_callf("focus-out-handler", "p", uc);
}

void
uim_place_context(uim_context uc)
{
  assert(uc);

  uim_scm_callf("place-handler", "p", uc);
}

void
uim_displace_context(uim_context uc)
{
  assert(uc);

  uim_scm_callf("displace-handler", "p", uc);
}

void
uim_set_preedit_cb(uim_context uc,
		   void (*clear_cb)(void *ptr),
		   void (*pushback_cb)(void *ptr, int attr, const char *str),
		   void (*update_cb)(void *ptr))
{
  assert(uc);

  uc->preedit_clear_cb = clear_cb;
  uc->preedit_pushback_cb = pushback_cb;
  uc->preedit_update_cb = update_cb;
}

void
uim_set_candidate_selector_cb(uim_context uc,
                              void (*activate_cb)(void *ptr,
                                                  int nr, int display_limit),
                              void (*select_cb)(void *ptr, int index),
                              void (*shift_page_cb)(void *ptr, int direction),
                              void (*deactivate_cb)(void *ptr))
{
  assert(uc);

  uc->candidate_selector_activate_cb   = activate_cb;
  uc->candidate_selector_select_cb     = select_cb;
  uc->candidate_selector_deactivate_cb = deactivate_cb;
  uc->candidate_selector_shift_page_cb = shift_page_cb;
}

/* must be called from GC-protected stack context */
uim_candidate
uim_get_candidate(uim_context uc, int index, int accel_enumeration_hint)
{
  uim_candidate cand;
  uim_lisp triple;
  const char *str, *head, *ann;

  assert(uc);
  assert(index >= 0);
  assert(accel_enumeration_hint >= 0);

  triple = uim_scm_callf("get-candidate", "pii",
                         uc, index, accel_enumeration_hint);

  cand = malloc(sizeof(*cand));
  if (cand) {
    memset(cand, 0, sizeof(*cand));
    if (uim_scm_length(triple) == 3) {
      str  = uim_scm_refer_c_str(CAR(triple));
      head = uim_scm_refer_c_str(CAR(CDR(triple)));
      ann  = uim_scm_refer_c_str(CAR(CDR(CDR((triple)))));
      cand->str           = uc->conv_if->convert(uc->outbound_conv, str);
      cand->heading_label = uc->conv_if->convert(uc->outbound_conv, head);
      cand->annotation    = uc->conv_if->convert(uc->outbound_conv, ann);
    }
  }

  return cand;
}

const char *
uim_candidate_get_cand_str(uim_candidate cand)
{
  assert(cand);

  return cand->str;
}

const char *
uim_candidate_get_heading_label(uim_candidate cand)
{
  assert(cand);

  return cand->heading_label;
}

const char *
uim_candidate_get_annotation_str(uim_candidate cand)
{
  assert(cand);

  return cand->annotation;
}

void
uim_candidate_free(uim_candidate cand)
{
  assert(cand);

  free(cand->str);
  free(cand->heading_label);
  free(cand->annotation);
  free(cand);
}

int
uim_get_candidate_index(uim_context uc)
{
  assert(uc);

  return 0;
}

void
uim_set_candidate_index(uim_context uc, int nth)
{
  assert(uc);
  assert(nth >= 0);

  uim_scm_callf("set-candidate-index", "pi", uc, nth);
}

void
uim_set_text_acquisition_cb(uim_context uc,
			    int (*acquire_cb)(void *ptr,
					      enum UTextArea text_id,
					      enum UTextOrigin origin,
					      int former_len, int latter_len,
					      char **former, char **latter),
			    int (*delete_cb)(void *ptr, enum UTextArea text_id,
				    	     enum UTextOrigin origin,
					     int former_len, int latter_len))
{
  assert(uc);

  uc->acquire_text_cb = acquire_cb;
  uc->delete_text_cb = delete_cb;
}

uim_bool
uim_input_string(uim_context uc, const char *str)
{
  uim_lisp consumed;
  char *conv;

  assert(uc);
  assert(str);

  conv = uc->conv_if->convert(uc->inbound_conv, str);
  if (conv) {
    consumed = uim_scm_callf("input-string-handler", "ps", uc, conv);
    free(conv);

    return uim_scm_c_bool(consumed);
  }

  return UIM_FALSE;
}

/****************************************************************
 * Optional APIs                                                *
 ****************************************************************/
void
uim_set_configuration_changed_cb(uim_context uc,
				 void (*changed_cb)(void *ptr))
{
  assert(uc);

  uc->configuration_changed_cb = changed_cb;
}

void
uim_set_im_switch_request_cb(uim_context uc,
			     void (*sw_app_im_cb)(void *ptr, const char *name),
			     void (*sw_system_im_cb)(void *ptr,
                                                     const char *name))
{
  assert(uc);

  uc->switch_app_global_im_cb = sw_app_im_cb;
  uc->switch_system_global_im_cb = sw_system_im_cb;
}

void
uim_switch_im(uim_context uc, const char *engine)
{
  /* related to the commit log of r1400:

     We should not add the API uim_destroy_context(). We should move
     IM-switching feature into Scheme instead. It removes the context
     management code related to IM-switching duplicated in several IM
     bridges. Refer the implementation of GtkIMMulticontext as example
     of proxy-like IM-switcher. It does IM-switching without extending
     immodule API. We should follow its design to make our API simple.
     -- 2004-10-05 YamaKen
  */
  assert(uc);
  assert(engine);

  uim_scm_callf("uim-switch-im", "py", uc, engine);
}

const char *
uim_get_current_im_name(uim_context uc)
{
  uim_lisp im;

  assert(uc);

  im = uim_scm_callf("uim-context-im", "p", uc);
  if (UIM_SCM_FALSEP(im))
    return NULL;

  return uim_scm_refer_c_str(uim_scm_callf("im-name", "o", im));
}

const char *
uim_get_default_im_name(const char *localename)
{
  uim_lisp ret;

  assert(localename);

  ret = uim_scm_callf("uim-get-default-im-name", "s", localename);
  return uim_scm_refer_c_str(ret);
}

const char *
uim_get_im_name_for_locale(const char *localename)
{
  uim_lisp ret;

  assert(localename);

  ret = uim_scm_callf("uim-get-im-name-for-locale", "s", localename);
  return uim_scm_refer_c_str(ret);
}

/****************************************************************
 * Legacy 'mode' API                                            *
 ****************************************************************/
int
uim_get_nr_modes(uim_context uc)
{
  assert(uc);

  return uc->nr_modes;
}

const char *
uim_get_mode_name(uim_context uc, int nth)
{
  assert(uc);
  assert(nth >= 0);

  if (nth >= 0 && nth < uc->nr_modes) {
    return uc->modes[nth];
  }
  return NULL;
}

int
uim_get_current_mode(uim_context uc)
{
  assert(uc);

  return uc->mode;
}

void
uim_set_mode(uim_context uc, int mode)
{
  assert(uc);
  assert(mode >= 0);

  uc->mode = mode;
  uim_scm_callf("mode-handler", "pi", uc, mode);
}

void
uim_set_mode_cb(uim_context uc, void (*update_cb)(void *ptr, int mode))
{
  assert(uc);

  uc->mode_update_cb = update_cb;
}

void
uim_set_mode_list_update_cb(uim_context uc, void (*update_cb)(void *ptr))
{
  assert(uc);

  uc->mode_list_update_cb = update_cb;
}

/****************************************************************
 * Legacy 'property list' API                                   *
 ****************************************************************/
void
uim_set_prop_list_update_cb(uim_context uc,
			    void (*update_cb)(void *ptr, const char *str))
{
  assert(uc);

  uc->prop_list_update_cb = update_cb;
}

/* Obsolete */
void
uim_set_prop_label_update_cb(uim_context uc,
			     void (*update_cb)(void *ptr, const char *str))
{
  assert(uc);
}

void
uim_prop_list_update(uim_context uc)
{
  assert(uc);

  if (uc->propstr && uc->prop_list_update_cb)
    uc->prop_list_update_cb(uc->ptr, uc->propstr);
}

/* Obsolete */
void
uim_prop_label_update(uim_context uc)
{
  assert(uc);
}

void
uim_prop_activate(uim_context uc, const char *str)
{
  assert(uc);
  assert(str);
      
  uim_scm_callf("prop-activate-handler", "ps", uc, str);
}

/****************************************************************
 * Legacy 'custom' API                                          *
 ****************************************************************/
/* Tentative name. I followed above uim_prop_update_custom, but prop 
would not be proper to this function. */
/*
 * As I described in doc/HELPER-PROTOCOL, it had wrongly named by my
 * misunderstanding about what is the 'property' of uim. It should be
 * renamed along with corresponding procol names when an appropriate
 * time has come.  -- YamaKen 2005-09-12
 */
/** Update custom value from property message.
 * Update custom value from property message. All variable update is
 * validated by custom APIs rather than arbitrary sexp
 * evaluation. Custom symbol \a custom is quoted in sexp string to be
 * restricted to accept symbol literal only. This prevents arbitrary
 * sexp evaluation.
 */
void
uim_prop_update_custom(uim_context uc, const char *custom, const char *val)
{
  assert(uc);
  assert(custom);
  assert(val);

  uim_scm_callf("custom-set-handler", "pyo",
                uc, custom, uim_scm_eval_c_string(val));
}

uim_bool
uim_prop_reload_configs(void)
{
  /* FIXME: handle return value properly. */
  uim_scm_callf("custom-reload-user-configs", "");
  return UIM_TRUE;
}

/****************************************************************
 * Legacy nth-index based IM management APIs                    *
 ****************************************************************/
int
uim_get_nr_im(uim_context uc)
{
  uim_lisp n;

  assert(uc);

  n = uim_scm_callf("uim-n-convertible-ims", "p", uc);
  return uim_scm_c_int(n);
}

static uim_lisp
get_nth_im(uim_context uc, int nth)
{
  assert(uc);
  assert(nth >= 0);

  return uim_scm_callf("uim-nth-convertible-im", "pi", uc, nth);
}

const char *
uim_get_im_name(uim_context uc, int nth)
{
  uim_lisp im;

  im = get_nth_im(uc, nth);
  if (UIM_SCM_FALSEP(im))
    return NULL;

  return uim_scm_refer_c_str(uim_scm_callf("im-name", "o", im));
}

const char *
uim_get_im_language(uim_context uc, int nth)
{
  uim_lisp im;

  im = get_nth_im(uc, nth);
  if (UIM_SCM_FALSEP(im))
    return NULL;

  return uim_scm_refer_c_str(uim_scm_callf("im-lang", "o", im));
}

const char *
uim_get_im_encoding(uim_context uc, int nth)
{
  uim_lisp im;

  im = get_nth_im(uc, nth);
  if (UIM_SCM_FALSEP(im))
    return NULL;

  return uim_scm_refer_c_str(uim_scm_callf("im-encoding", "o", im));
}

const char *
uim_get_im_short_desc(uim_context uc, int nth)
{
  uim_lisp im, short_desc;

  im = get_nth_im(uc, nth);
  if (UIM_SCM_FALSEP(im))
    return NULL;

  short_desc = uim_scm_callf("im-short-desc", "o", im);
  return UIM_SCM_FALSEP(short_desc) ? "-" : uim_scm_refer_c_str(short_desc);
}
