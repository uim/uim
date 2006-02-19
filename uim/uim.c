/*

  Copyright (c) 2003-2006 uim Project http://uim.freedesktop.org/

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

#include "config.h"
#include <pwd.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include "uim.h"
#include "uim-im-switcher.h"
#include "uim-scm.h"
#include "uim-custom.h"
#include "uim-internal.h"
#include "gettext.h"
#include "uim-util.h"

extern char *uim_return_str;
extern char *uim_return_str_list[10];
/* duplicate definition */
#define UIM_RETURN_STR_LIST_SIZE ((sizeof(uim_return_str_list) \
                                   / sizeof(uim_return_str_list[0])) - 2)

char *uim_last_client_encoding;

#define CONTEXT_ARRAY_SIZE 512
static uim_context context_array[CONTEXT_ARRAY_SIZE];
struct uim_im *uim_im_array;
int uim_nr_im;

/* Explicitly initialized by zero although it is ensured to be zero by
 * the C standard, because some implementation violates it? kzk, note
 * the name.  -- YamaKen 2006-02-20 */
static int uim_initialized = 0;

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

static void
get_context_id(uim_context uc)
{
  int i;
  for (i = 0; i < CONTEXT_ARRAY_SIZE; i++) {
    if (!context_array[i]) {
      context_array[i] = uc;
      uc->id = i;
      return;
    }
  }
  uc->id = -1;
}

static void
put_context_id(uim_context uc)
{
  context_array[uc->id] = NULL;
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
  get_context_id(uc);
  if (uc->id == -1)
    return NULL;

  uc->ptr = ptr;
  uc->is_enable = 1;
  uc->commit_cb = commit_cb;
  if (!enc) {
    enc = "UTF-8";
  }
  uc->short_desc = NULL;
  uc->encoding = strdup(enc);
  uc->conv_if = conv;
  uc->conv = NULL;
  /**/
  uc->nr_modes = 0;
  uc->modes = NULL;
  uc->mode = 0;
  /**/
  uc->propstr = NULL;
  uc->proplabelstr = NULL;
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
  uc->prop_label_update_cb = NULL;
  /**/
  uc->candidate_selector_activate_cb = NULL;
  uc->candidate_selector_select_cb = NULL;
  uc->candidate_selector_shift_page_cb = NULL;
  uc->candidate_selector_deactivate_cb = NULL;
  /**/
  uc->request_surrounding_text_cb = NULL;
  uc->delete_surrounding_text_cb = NULL;
  /**/
  uc->configuration_changed_cb = NULL;
  /**/
  uc->nr_candidates = 0;
  uc->candidate_index = 0;
  /**/
  uc->psegs = NULL;
  uc->nr_psegs = 0;

  if (!lang) {
    lang = "#f";
  }
  if (!engine) {
    engine = "#f";
    uc->current_im_name = NULL;
  } else {
    uc->current_im_name = strdup(engine);
  }
  if (uim_last_client_encoding) {
    free(uim_last_client_encoding);
    uim_last_client_encoding = NULL;
  }
  if (enc) {
    /* Save client encoding for bind_textdomain_codeset(). The value
       is used when UIM_EVAL_STRING() is called without
       uim_context. The specification assumes client always uses
       consistent encoding
    */
    uim_last_client_encoding = strdup(enc);
  }

  UIM_EVAL_FSTRING3(uc, "(create-context %d '%s '%s)", uc->id, lang, engine);

  return uc;
}

void
uim_reset_context(uim_context uc)
{
   UIM_EVAL_FSTRING1(uc, "(reset-handler %d)", uc->id);

   /* delete all preedit segments */
   uim_release_preedit_segments(uc);
}

void
uim_set_configuration_changed_cb(uim_context uc,
				 void (*changed_cb)(void *ptr))
{
  uc->configuration_changed_cb = changed_cb;
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
  int id = uc->id;
  uim_reset_context(uc); /* FIXME: reset should be called here? */

  UIM_EVAL_FSTRING1(uc, "(release-context %d)", uc->id);
  uim_release_preedit_segments(uc);
  uim_update_preedit_segments(uc);

  UIM_EVAL_FSTRING2(uc, "(create-context %d #f '%s)", id, engine);
  if (uc->current_im_name)
    free(uc->current_im_name);
  uc->current_im_name = strdup(engine);
#if 0
  /*
    I don't know when uc->short_desc is used, so this part is
    disabled. -- 2004-10-04 YamaKen
  */
  if (uc->short_desc)
    free(uc->short_desc);
  UIM_EVAL_FSTRING1(uc, "(uim-get-im-short-desc '%s)", im->name);
  uc->short_desc = uim_return_str;
  uim_return_str = NULL;  /* ownership has been transferred */
#endif
}

void
uim_release_context(uim_context uc)
{
  int i;

  if (!uc)
    return;

  UIM_EVAL_FSTRING1(uc, "(release-context %d)", uc->id);
  put_context_id(uc);
  if (uc->conv) {
    uc->conv_if->release(uc->conv);
  }
  uim_release_preedit_segments(uc);
  for (i = 0; i < uc->nr_modes; i++) {
    free(uc->modes[i]);
    uc->modes[i] = NULL;
  }
  free(uc->propstr);
  free(uc->proplabelstr);
  free(uc->modes);
  free(uc->short_desc);
  free(uc->encoding);
  free(uc->current_im_name);
  free(uc);
}

uim_context
uim_find_context(int id)
{
  uim_context uc;
  uc = context_array[id];
  return uc;
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


void
uim_set_prop_label_update_cb(uim_context uc,
			     void (*update_cb)(void *ptr, const char *str))
{
  uc->prop_label_update_cb = update_cb;
}


void
uim_prop_activate(uim_context uc, const char *str)
{
  if (!str)
    return;
      
  UIM_EVAL_FSTRING2(uc, "(prop-activate-handler %d \"%s\")",
		    uc->id, str);
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

  UIM_EVAL_FSTRING3(uc, "(custom-set-handler %d '%s %s)",
		    uc->id, custom, val);
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
  uim_scm_eval_c_string("(custom-reload-user-configs)");
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
  UIM_EVAL_FSTRING2(uc, "(mode-handler %d %d)", uc->id, mode);
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

void
uim_prop_label_update(uim_context uc)
{
  if (uc && uc->proplabelstr && uc->prop_label_update_cb)
    uc->prop_label_update_cb(uc->ptr, uc->proplabelstr);
}

int
uim_get_nr_im(uim_context uc)
{
  int i, nr = 0;

  if (!uc)
    return 0;

  for (i = 0; i < uim_nr_im; i++) {
    if (uc->conv_if->is_convertible(uc->encoding, uim_im_array[i].encoding)) {
      nr ++;
    }
  }
  return nr;
}

static struct uim_im *
get_nth_im(uim_context uc, int nth)
{
  int i,n=0;
  for (i = 0; i < uim_nr_im; i++) {
    if (uc->conv_if->is_convertible(uc->encoding, uim_im_array[i].encoding)) {
      if (n == nth) {
	return &uim_im_array[i];
      }
      n++;
    }
  }
  return NULL;
}

const char *
uim_get_current_im_name(uim_context uc)
{
  if (uc) {
    return uc->current_im_name;
  }
  return NULL;
}

const char *
uim_get_im_name(uim_context uc, int nth)
{
  struct uim_im *im = get_nth_im(uc, nth);

  if (im) {
    return im->name;
  }
  return NULL;
}

const char *
uim_get_im_language(uim_context uc, int nth)
{
  struct uim_im *im = get_nth_im(uc, nth);

  if (im) {
    return im->lang;
  }
  return NULL;
}

const char *
uim_get_im_short_desc(uim_context uc, int nth)
{
  struct uim_im *im = get_nth_im(uc, nth);

  if (im) {
    if (im->short_desc)
      free(im->short_desc);
    UIM_EVAL_FSTRING1(uc, "(uim-get-im-short-desc '%s)", im->name);
    im->short_desc = uim_return_str;
    uim_return_str = NULL;  /* ownership has been transferred */
    return im->short_desc;
  }
  return NULL;
}

const char *
uim_get_im_encoding(uim_context uc, int nth)
{
  struct uim_im *im = get_nth_im(uc, nth);

  if (im) {
    return im->encoding;
  }
  return NULL;
}

static const char *
uim_check_im_exist(const char *im_engine_name)
{
  int i;

  if (im_engine_name == NULL)
    return NULL;

  for (i = 0; i < uim_nr_im; i++) {
    struct uim_im *im = &uim_im_array[i];
    if (strcmp(im_engine_name, im->name) == 0) {
      return im->name;
    }
  }
  return NULL;
}

const char *
uim_get_default_im_name(const char *localename)
{
  const char *valid_default_im_name;
  UIM_EVAL_FSTRING1(NULL, "(uim-get-default-im-name \"%s\")", localename);
  valid_default_im_name = uim_check_im_exist(uim_return_str);

  if (!valid_default_im_name)
    valid_default_im_name = "direct";  /* never happen */
  return valid_default_im_name;
}

const char *
uim_get_im_name_for_locale(const char *localename)
{
  const char *valid_im_name;
  UIM_EVAL_FSTRING1(NULL, "(uim-get-im-name-for-locale \"%s\")", localename);
  valid_im_name = uim_check_im_exist(uim_return_str);

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

uim_candidate
uim_get_candidate(uim_context uc, int index, int accel_enumeration_hint)
{
  uim_candidate cand = malloc(sizeof(*cand));
  memset(cand, 0, sizeof(*cand));
  UIM_EVAL_FSTRING3(uc, "(get-candidate %d %d %d)",
		    uc->id, index, accel_enumeration_hint);

  if (uim_return_str_list[0] && uim_return_str_list[1]) {
    cand->str = uc->conv_if->convert(uc->conv, uim_return_str_list[0]);
    cand->heading_label = uc->conv_if->convert(uc->conv, uim_return_str_list[1]);    
  } else {
    cand->str = NULL;
    cand->heading_label = NULL;
  }

  if (uim_return_str_list[2]) {
    cand->annotation = uc->conv_if->convert(uc->conv, uim_return_str_list[2]);
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
  UIM_EVAL_FSTRING2(uc, "(set-candidate-index %d %d)", uc->id, nth);
  return ;
}

void
uim_set_surrounding_text_cb(uim_context uc,
			    void (*request_cb)(void *ptr),
			    int (*delete_cb)(void *ptr, int offset, int len))
{
  uc->request_surrounding_text_cb = request_cb;
  uc->delete_surrounding_text_cb = delete_cb;
}

void
uim_set_surrounding_text(uim_context uc, const char *text,
			 int cursor_pos, int len)
{
}

static void
uim_init_scm(void)
{
  int i;
  char *scm_files = NULL;
  char *env = NULL;

  /*  if (!uim_issetugid()) {*/
    env = getenv("LIBUIM_VERBOSE");
    /*  }*/
  uim_scm_init(env);  /* init Scheme interpreter */

#ifdef UIM_COMPAT_SCM
  uim_init_compat_scm_subrs();
#endif
  uim_init_intl_subrs();
  uim_init_util_subrs();
  uim_init_plugin();
  uim_init_im_subrs();
  uim_init_key_subrs();
  
  if (!uim_issetugid()) {
    scm_files = getenv("LIBUIM_SCM_FILES");
  }
  uim_scm_set_lib_path((scm_files) ? scm_files : SCM_FILES);

  uim_scm_require_file("init.scm");

  uim_return_str = NULL;
  for (i = 0; i < (int)UIM_RETURN_STR_LIST_SIZE; i++) {
    uim_return_str_list[i] = NULL;
  }

#if 0
  /*
    Current libuim implementation has the gettext encoding problem. It
    requires library-wide default encoding configurability rather than
    per context encoding.  -- YamaKen 2005-01-31
  */
#ifdef ENABLE_NLS
 {
   const char *client_enc;

   /* portable equivalent of nl_langinfo(CODESET) */
   UIM_EVAL_FSTRING1(NULL, "(locale-lang (locale-new \"%s\"))",
		     setlocale(LC_CTYPE, NULL));
   client_enc = uim_scm_refer_c_str(uim_scm_return_value());
   uim_last_client_encoding = strdup(client_enc);
 }
#endif
#endif
}

int
uim_init(void)
{
  if (uim_initialized)
    return 0;

  uim_last_client_encoding = NULL;
  uim_im_array = NULL;
  uim_nr_im = 0;
  uim_init_scm();
  uim_initialized = 1;

  return 0;
}

void
uim_quit(void)
{
  int i;

  if (!uim_initialized)
    return;

  /* release still active contexts */
  for (i = 0; i < CONTEXT_ARRAY_SIZE; i++) {
    if (context_array[i]) {
      uim_release_context(context_array[i]);
    }
  }
  /**/
  uim_quit_plugin();
  uim_scm_quit();
  free(uim_last_client_encoding);
  uim_last_client_encoding = NULL;
  uim_initialized = 0;
}
