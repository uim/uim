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

#include <pwd.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include "uim.h"
#include "uim-im-switcher.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-custom.h"
#include "uim-internal.h"
#include "uim-util.h"

static int uim_initialized;

void
uim_set_preedit_cb(uim_context uc,
		   void (*clear_cb)(void *ptr),
		   void (*pushback_cb)(void *ptr,
				       int attr,
				       const char *str),
		   void (*update_cb)(void *ptr))
{
  uc->preedit_clear_cb = clear_cb;
  uc->preedit_pushback_cb = pushback_cb;
  uc->preedit_update_cb = update_cb;
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
    return 0;
  }

  if (!conv) {
    conv = uim_iconv;
  }

  if (!uim_scm_is_alive() || !conv) {
    return NULL;
  }

  uc = malloc(sizeof(*uc));
  if (!uc) {
    return NULL;
  }

  uc->ptr = ptr;
  uc->is_enable = 1;
  uc->commit_cb = commit_cb;
  if (!enc) {
    enc = "UTF-8";
  }
  uc->client_encoding = strdup(enc);
  uc->conv_if = conv;
  uc->outbound_conv = NULL;
  uc->inbound_conv = NULL;
  /**/
  uc->nr_modes = 0;
  uc->modes = NULL;
  uc->mode = 0;
  /**/
  uc->propstr = NULL;
  /**/
  uc->preedit_clear_cb = NULL;
  uc->preedit_pushback_cb = NULL;
  uc->preedit_update_cb = NULL;
  /**/
  uc->mode_list_update_cb = NULL;
  /**/
  uc->mode_update_cb = NULL;
  /**/
  uc->prop_list_update_cb  = NULL;
  /**/
  uc->candidate_selector_activate_cb = NULL;
  uc->candidate_selector_select_cb = NULL;
  uc->candidate_selector_shift_page_cb = NULL;
  uc->candidate_selector_deactivate_cb = NULL;
  /**/
  uc->acquire_text_cb = NULL;
  uc->delete_text_cb = NULL;
  /**/
  uc->configuration_changed_cb = NULL;
  /**/
  uc->switch_app_global_im_cb = NULL;
  uc->switch_system_global_im_cb = NULL;
  /**/
  uc->nr_candidates = 0;
  uc->candidate_index = 0;

  lang_ = (lang) ? MAKE_SYM(lang) : uim_scm_f();
  engine_ = (engine) ? MAKE_SYM(engine) : uim_scm_f();
  uc->sc = NULL; /* failsafe */
  uc->sc = uim_scm_call3(MAKE_SYM("create-context"),
                         MAKE_PTR(uc), lang_, engine_);
  uim_scm_gc_protect(&uc->sc);

  return uc;
}

void
uim_reset_context(uim_context uc)
{
  uim_scm_call1(MAKE_SYM("reset-handler"), MAKE_PTR(uc));
}

void
uim_focus_in_context(uim_context uc)
{
  uim_scm_call1(MAKE_SYM("focus-in-handler"),MAKE_PTR(uc));
}

void
uim_focus_out_context(uim_context uc)
{
  uim_scm_call1(MAKE_SYM("focus-out-handler"), MAKE_PTR(uc));
}

void
uim_place_context(uim_context uc)
{
  uim_scm_call1(MAKE_SYM("place-handler"), MAKE_PTR(uc));
}

void
uim_displace_context(uim_context uc)
{
  uim_scm_call1(MAKE_SYM("displace-handler"), MAKE_PTR(uc));
}

void
uim_set_configuration_changed_cb(uim_context uc,
				 void (*changed_cb)(void *ptr))
{
  uc->configuration_changed_cb = changed_cb;
}

void
uim_set_im_switch_request_cb(uim_context uc,
			     void (*sw_app_im_cb)(void *ptr, const char *name),
			     void (*sw_system_im_cb)(void *ptr, const char *name))
{
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
  uim_reset_context(uc); /* FIXME: reset should be called here? */

  uim_scm_call1(MAKE_SYM("release-context"), MAKE_PTR(uc));
  uc->sc = uim_scm_call3(MAKE_SYM("create-context"),
                         MAKE_PTR(uc), uim_scm_f(), MAKE_SYM(engine));
}

void
uim_release_context(uim_context uc)
{
  int i;

  if (!uc)
    return;

  uim_scm_call1(MAKE_SYM("release-context"), MAKE_PTR(uc));
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

int
uim_get_nr_modes(uim_context uc)
{
  return uc->nr_modes;
}

const char *
uim_get_mode_name(uim_context uc, int nth)
{
  if (nth >= 0 && nth < uc->nr_modes) {
    return uc->modes[nth];
  }
  return NULL;
}

void
uim_set_mode_list_update_cb(uim_context uc,
			    void (*update_cb)(void *ptr))
{
  uc->mode_list_update_cb = update_cb;
}

void
uim_set_prop_list_update_cb(uim_context uc,
			    void (*update_cb)(void *ptr, const char *str))
{
  uc->prop_list_update_cb = update_cb;
}

/* Obsolete */
void
uim_set_prop_label_update_cb(uim_context uc,
			     void (*update_cb)(void *ptr, const char *str))
{
}

void
uim_prop_activate(uim_context uc, const char *str)
{
  if (!str)
    return;
      
  uim_scm_call2(MAKE_SYM("prop-activate-handler"),
                MAKE_PTR(uc), MAKE_STR(str));
}

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
  if (!custom || !val)
    return;

  uim_scm_call3(MAKE_SYM("custom-set-handler"),
                MAKE_PTR(uc),
                MAKE_SYM(custom),
                uim_scm_eval_c_string(val));
}


/* Tentative name. I followed above uim_prop_update_custom, but prop 
would not be proper to this function. */
/*
 * As I described in doc/HELPER-PROTOCOL, it had wrongly named by my
 * misunderstanding about what is the 'property' of uim. It should be
 * renamed along with corresponding procol names when an appropriate
 * time has come.  -- YamaKen 2005-09-12
 */
uim_bool
uim_prop_reload_configs(void)
{
  /* FIXME: handle return value properly. */
  uim_scm_call0(MAKE_SYM("custom-reload-user-configs"));
  return UIM_TRUE;
}

int
uim_get_current_mode(uim_context uc)
{
  return uc->mode;
}

void
uim_set_mode(uim_context uc, int mode)
{
  uc->mode = mode;
  uim_scm_call2(MAKE_SYM("mode-handler"), MAKE_PTR(uc), MAKE_INT(mode));
}

void
uim_set_mode_cb(uim_context uc, void (*update_cb)(void *ptr,
						  int mode))
{
  uc->mode_update_cb = update_cb;
}

void
uim_prop_list_update(uim_context uc)
{
  if (uc && uc->propstr && uc->prop_list_update_cb)
    uc->prop_list_update_cb(uc->ptr, uc->propstr);
}

/* Obsolete */
void
uim_prop_label_update(uim_context uc)
{
}

int
uim_get_nr_im(uim_context uc)
{
  uim_lisp n;

  if (!uc)
    return 0;

  n = uim_scm_call1(MAKE_SYM("uim-n-convertible-ims"), MAKE_PTR(uc));
  return uim_scm_c_int(n);
}

static uim_lisp
get_nth_im(uim_context uc, int nth)
{
  return uim_scm_call2(MAKE_SYM("uim-nth-convertible-im"),
                       MAKE_PTR(uc), MAKE_INT(nth));
}

const char *
uim_get_current_im_name(uim_context uc)
{
  uim_lisp im;

  if (!uc)
    return NULL;

  im = uim_scm_call1(MAKE_SYM("uim-context-im"), MAKE_PTR(uc));
  if (UIM_SCM_FALSEP(im))
    return NULL;

  return uim_scm_refer_c_str(uim_scm_call1(MAKE_SYM("im-name"), im));
}

const char *
uim_get_im_name(uim_context uc, int nth)
{
  uim_lisp im;

  im = get_nth_im(uc, nth);
  if (UIM_SCM_FALSEP(im))
    return NULL;

  return uim_scm_refer_c_str(uim_scm_call1(MAKE_SYM("im-name"), im));
}

const char *
uim_get_im_language(uim_context uc, int nth)
{
  uim_lisp im;

  im = get_nth_im(uc, nth);
  if (UIM_SCM_FALSEP(im))
    return NULL;

  return uim_scm_refer_c_str(uim_scm_call1(MAKE_SYM("im-lang"), im));
}

const char *
uim_get_im_short_desc(uim_context uc, int nth)
{
  uim_lisp im, short_desc;

  im = get_nth_im(uc, nth);
  if (UIM_SCM_FALSEP(im))
    return NULL;

  short_desc = uim_scm_call1(MAKE_SYM("im-short-desc"), im);
  return UIM_SCM_FALSEP(short_desc) ? "-" : uim_scm_refer_c_str(short_desc);
}

const char *
uim_get_im_encoding(uim_context uc, int nth)
{
  uim_lisp im;

  im = get_nth_im(uc, nth);
  if (UIM_SCM_FALSEP(im))
    return NULL;

  return uim_scm_refer_c_str(uim_scm_call1(MAKE_SYM("im-encoding"), im));
}

const char *
uim_get_default_im_name(const char *localename)
{
  const char *valid_default_im_name;
  uim_lisp ret;

  ret = uim_scm_call1(MAKE_SYM("uim-get-default-im-name"),
                      MAKE_STR(localename));
  valid_default_im_name = uim_scm_refer_c_str(ret);

  if (!valid_default_im_name)
    valid_default_im_name = "direct";  /* never happen */
  return valid_default_im_name;
}

const char *
uim_get_im_name_for_locale(const char *localename)
{
  const char *valid_im_name;
  uim_lisp ret;

  ret = uim_scm_call1(MAKE_SYM("uim-get-im-name-for-locale"),
                      MAKE_STR(localename));
  valid_im_name = uim_scm_refer_c_str(ret);

  if (!valid_im_name)
    valid_im_name = "direct";  /* never happen */
  return valid_im_name;
}

int uim_set_candidate_selector_cb(uim_context uc,
				  void (*activate_cb)(void *ptr, int nr, int display_limit),
				  void (*select_cb)(void *ptr, int index),
				  void (*shift_page_cb)(void *ptr, int direction),
				  void (*deactivate_cb)(void *ptr))
{
  uc->candidate_selector_activate_cb = activate_cb;
  uc->candidate_selector_select_cb = select_cb;
  uc->candidate_selector_deactivate_cb = deactivate_cb;
  uc->candidate_selector_shift_page_cb = shift_page_cb;

  return 0;
}

/* must be called from GC-protected stack context */
uim_candidate
uim_get_candidate(uim_context uc, int index, int accel_enumeration_hint)
{
  uim_candidate cand;
  uim_lisp triple;
  const char *str, *head, *ann;

  triple = uim_scm_call3(MAKE_SYM("get-candidate"),
                         MAKE_PTR(uc),
                         MAKE_INT(index),
                         MAKE_INT(accel_enumeration_hint));

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
  return cand->str;
}

const char *
uim_candidate_get_heading_label(uim_candidate cand)
{
  return cand->heading_label;
}

const char *
uim_candidate_get_annotation_str(uim_candidate cand)
{
  return cand->annotation;
}

void
uim_candidate_free(uim_candidate cand)
{
  free(cand->str);
  free(cand->heading_label);
  if (cand->annotation)
    free(cand->annotation);

  free(cand);
}

int uim_get_candidate_index(uim_context uc)
{
  return 0;
}

void
uim_set_candidate_index(uim_context uc, int nth)
{
  uim_scm_call2(MAKE_SYM("set-candidate-index"), MAKE_PTR(uc), MAKE_INT(nth));
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
  uc->acquire_text_cb = acquire_cb;
  uc->delete_text_cb = delete_cb;
}

uim_bool
uim_input_string(uim_context uc, const char *str)
{
  char *conv;

  conv = uc->conv_if->convert(uc->inbound_conv, str);
  if (conv) {
    if (conv)
      uim_scm_call2(MAKE_SYM("input-string-handler"),
                    MAKE_PTR(uc), MAKE_STR(conv));
    free(conv);

    /* FIXME: handle return value properly. */
    return UIM_TRUE;
  }

  return UIM_FALSE;
}

static void
uim_init_scm(void)
{
  char *scm_files = NULL;
  char *env = NULL;

  /*  if (!uim_issetugid()) {*/
    env = getenv("LIBUIM_VERBOSE");
    /*  }*/
  uim_scm_init(env);  /* init Scheme interpreter */

  uim_init_intl_subrs();
  uim_init_util_subrs();
  uim_init_plugin();
#ifdef ENABLE_ANTHY_STATIC
  uim_anthy_plugin_instance_init();
#endif
  uim_init_im_subrs();
  uim_init_key_subrs();
  
  if (!uim_issetugid()) {
    scm_files = getenv("LIBUIM_SCM_FILES");
  }
  uim_scm_set_lib_path((scm_files) ? scm_files : SCM_FILES);

  uim_scm_require_file("init.scm");
}

int
uim_init(void)
{
  if (uim_initialized)
    return 0;

  uim_init_scm();
  uim_initialized = 1;

  return 0;
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
  uim_initialized = 0;
}
