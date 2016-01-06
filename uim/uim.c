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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#if !UIM_USE_NOTIFY_PLUGINS
#include <stdarg.h>
#endif

#include "uim.h"
#include "uim-internal.h"
#include "uim-util.h"
#include "uim-iconv.h"
#include "uim-posix.h"
#include "uim-im-switcher.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#if UIM_USE_NOTIFY_PLUGINS
#include "uim-notify.h"
#else
#include "gettext.h"
#endif


enum uim_result {
  FAILED = -1,
  OK = 0
};

static void fatal_error_hook(void);

static void *uim_init_internal(void *dummy);
struct uim_get_candidate_args {
  uim_context uc;
  int index;
  int enum_hint;
};
static void *uim_get_candidate_internal(struct uim_get_candidate_args *args);
struct uim_delay_activating_args {
  uim_context uc;
  int nr;
  int display_limit;
  int selected_index;
};
static void *uim_delay_activating_internal(struct uim_delay_activating_args *);
static uim_lisp get_nth_im(uim_context uc, int nth);
#ifdef ENABLE_ANTHY_STATIC
void uim_anthy_plugin_instance_init(void);
void uim_anthy_plugin_instance_quit(void);
#endif
#ifdef ENABLE_ANTHY_UTF8_STATIC
void uim_anthy_utf8_plugin_instance_init(void);
void uim_anthy_utf8_plugin_instance_quit(void);
#endif

static uim_bool uim_initialized;
static uim_lisp protected0, protected1;

unsigned int uim_init_count;

/****************************************************************
 * Core APIs                                                    *
 ****************************************************************/
static void
fatal_error_hook(void)
{
  /* actual error message is already printed by the Scheme interpreter */
  uim_fatal_error("an unhandled error raised from Scheme interpreter");
}

int
uim_init(void)
{
  int ret;
  char *sys_load_path;

  if (uim_initialized)
    return OK;

  uim_init_error();

  if (UIM_CATCH_ERROR_BEGIN())
    return FAILED;

  sys_load_path = (uim_issetugid()) ? NULL : getenv("LIBUIM_SYSTEM_SCM_FILES");
  uim_scm_init(sys_load_path);
  uim_scm_set_fatal_error_hook(fatal_error_hook);

  ret = (int)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_init_internal, NULL);

  UIM_CATCH_ERROR_END();

  return ret;
}

static void *
uim_init_internal(void *dummy)
{
  char *scm_files;

  protected0 = uim_scm_f();
  protected1 = uim_scm_f();
  uim_scm_gc_protect(&protected0);
  uim_scm_gc_protect(&protected1);

  /* To allow (cond-expand (uim ...)) in early initialization stages,
   * provision of the "uim" should be performed as early as possible. */
  uim_scm_callf("provide", "s", "uim");

  uim_init_im_subrs();
  uim_init_intl_subrs();
  uim_init_iconv_subrs();
  uim_init_posix_subrs();
  uim_init_util_subrs();
#if UIM_USE_NOTIFY_PLUGINS
  uim_notify_init();  /* init uim-notify facility */
#endif
  uim_init_notify_subrs();  /* init Scheme interface of uim-notify */
  uim_init_key_subrs();
  uim_init_rk_subrs();
  uim_init_dynlib();
#ifdef ENABLE_ANTHY_STATIC
  uim_anthy_plugin_instance_init();
#endif
#ifdef ENABLE_ANTHY_UTF8_STATIC
  uim_anthy_utf8_plugin_instance_init();
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

  return (void *)OK;
}

void
uim_quit(void)
{
  if (!uim_initialized)
    return;

  if (UIM_CATCH_ERROR_BEGIN()) {
    /* Leave uim_initialized uncleared to keep libuim disabled. */
    return;
  }

#ifdef ENABLE_ANTHY_STATIC
  uim_anthy_plugin_instance_quit();
#endif
#ifdef ENABLE_ANTHY_UTF8_STATIC
  uim_anthy_utf8_plugin_instance_quit();
#endif
#if UIM_USE_NOTIFY_PLUGINS
  uim_notify_quit();
#endif
  uim_scm_callf("annotation-unload", "");
  uim_scm_callf("dynlib-unload-all", "");
  uim_quit_dynlib();
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

  if (UIM_CATCH_ERROR_BEGIN())
    return NULL;

  assert(uim_scm_gc_any_contextp());

  uc = uim_malloc(sizeof(*uc));
  memset(uc, 0, sizeof(*uc));

  /* encoding handlings */
  if (!enc)
    enc = "UTF-8";
  uc->client_encoding = uim_strdup(enc);
  uc->conv_if = (conv) ? conv : uim_iconv;

  /* variables */
  uc->is_enabled = UIM_TRUE;

  /* core callbacks */
  uc->commit_cb = commit_cb;

  /* foreign context objects */
  uc->ptr = ptr;

  protected0 = lang_ = (lang) ? MAKE_SYM(lang) : uim_scm_f();
  protected1 = engine_ = (engine) ? MAKE_SYM(engine) : uim_scm_f();
  uc->sc = uim_scm_f(); /* failsafe */
  uc->sc = uim_scm_callf("create-context", "poo", uc, lang_, engine_);
  uim_scm_gc_protect(&uc->sc);
  uim_scm_callf("setup-context", "o", uc->sc);

  UIM_CATCH_ERROR_END();

  return uc;
}

void
uim_release_context(uim_context uc)
{
  int i;

  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
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
#ifdef DEBUG
  /* prevents operating on invalidated uim_context */
  memset(uc, 0, sizeof(*uc));
#endif
  free(uc);

  UIM_CATCH_ERROR_END();
}

void
uim_reset_context(uim_context uc)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  uim_scm_callf("reset-handler", "p", uc);

  UIM_CATCH_ERROR_END();
}

void
uim_focus_in_context(uim_context uc)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  uim_scm_callf("focus-in-handler", "p", uc);

  UIM_CATCH_ERROR_END();
}

void
uim_focus_out_context(uim_context uc)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  uim_scm_callf("focus-out-handler", "p", uc);

  UIM_CATCH_ERROR_END();
}

void
uim_place_context(uim_context uc)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  uim_scm_callf("place-handler", "p", uc);

  UIM_CATCH_ERROR_END();
}

void
uim_displace_context(uim_context uc)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  uim_scm_callf("displace-handler", "p", uc);

  UIM_CATCH_ERROR_END();
}

void
uim_set_preedit_cb(uim_context uc,
		   void (*clear_cb)(void *ptr),
		   void (*pushback_cb)(void *ptr, int attr, const char *str),
		   void (*update_cb)(void *ptr))
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  uc->preedit_clear_cb = clear_cb;
  uc->preedit_pushback_cb = pushback_cb;
  uc->preedit_update_cb = update_cb;

  UIM_CATCH_ERROR_END();
}

void
uim_set_candidate_selector_cb(uim_context uc,
                              void (*activate_cb)(void *ptr,
                                                  int nr, int display_limit),
                              void (*select_cb)(void *ptr, int index),
                              void (*shift_page_cb)(void *ptr, int direction),
                              void (*deactivate_cb)(void *ptr))
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  uc->candidate_selector_activate_cb   = activate_cb;
  uc->candidate_selector_select_cb     = select_cb;
  uc->candidate_selector_deactivate_cb = deactivate_cb;
  uc->candidate_selector_shift_page_cb = shift_page_cb;

  UIM_CATCH_ERROR_END();
}

uim_candidate
uim_get_candidate(uim_context uc, int index, int accel_enumeration_hint)
{
  struct uim_get_candidate_args args;
  uim_candidate cand;

  if (UIM_CATCH_ERROR_BEGIN())
    return NULL;

  assert(uim_scm_gc_any_contextp());
  assert(uc);
  assert(index >= 0);
  assert(accel_enumeration_hint >= 0);

  args.uc = uc;
  args.index = index;
  args.enum_hint = accel_enumeration_hint;

  cand = (uim_candidate)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_get_candidate_internal, &args);

  UIM_CATCH_ERROR_END();

  return cand;
}

static void *
uim_get_candidate_internal(struct uim_get_candidate_args *args)
{
  uim_context uc;
  uim_candidate cand;
  uim_lisp triple;
  const char *str, *head, *ann;

  uc = args->uc;
  triple = uim_scm_callf("get-candidate", "pii",
			 uc, args->index, args->enum_hint);
  ENSURE((uim_scm_length(triple) == 3), "invalid candidate triple");

  cand = uim_malloc(sizeof(*cand));
  memset(cand, 0, sizeof(*cand));

  str  = REFER_C_STR(CAR(triple));
  head = REFER_C_STR(CAR(CDR(triple)));
  ann  = REFER_C_STR(CAR(CDR(CDR((triple)))));
  cand->str           = uc->conv_if->convert(uc->outbound_conv, str);
  cand->heading_label = uc->conv_if->convert(uc->outbound_conv, head);
  cand->annotation    = uc->conv_if->convert(uc->outbound_conv, ann);

  return (void *)cand;
}

/* Accepts NULL candidates that produced by an error on uim_get_candidate(). */
const char *
uim_candidate_get_cand_str(uim_candidate cand)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return "";

  assert(uim_scm_gc_any_contextp());
  if (!cand)
    uim_fatal_error("null candidate");

  UIM_CATCH_ERROR_END();

  return cand->str;
}

const char *
uim_candidate_get_heading_label(uim_candidate cand)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return "";

  assert(uim_scm_gc_any_contextp());
  if (!cand)
    uim_fatal_error("null candidate");

  UIM_CATCH_ERROR_END();

  return cand->heading_label;
}

const char *
uim_candidate_get_annotation_str(uim_candidate cand)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return "";

  assert(uim_scm_gc_any_contextp());
  if (!cand)
    uim_fatal_error("null candidate");

  UIM_CATCH_ERROR_END();

  return cand->annotation;
}

void
uim_candidate_free(uim_candidate cand)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  if (!cand)
    uim_fatal_error("null candidate");

  free(cand->str);
  free(cand->heading_label);
  free(cand->annotation);
  free(cand);

  UIM_CATCH_ERROR_END();
}

int
uim_get_candidate_index(uim_context uc)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return 0;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  UIM_CATCH_ERROR_END();

  return 0;
}

void
uim_set_candidate_index(uim_context uc, int nth)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);
  assert(nth >= 0);

  uim_scm_callf("set-candidate-index", "pi", uc, nth);

  UIM_CATCH_ERROR_END();
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
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  uc->acquire_text_cb = acquire_cb;
  uc->delete_text_cb = delete_cb;

  UIM_CATCH_ERROR_END();
}

uim_bool
uim_input_string(uim_context uc, const char *str)
{
  uim_bool ret;
  uim_lisp consumed;
  char *conv;

  if (UIM_CATCH_ERROR_BEGIN())
    return UIM_FALSE;

  assert(uim_scm_gc_any_contextp());
  assert(uc);
  assert(str);

  conv = uc->conv_if->convert(uc->inbound_conv, str);
  if (conv) {
    protected0 =
      consumed = uim_scm_callf("input-string-handler", "ps", uc, conv);
    free(conv);

    ret = C_BOOL(consumed);
  } else {
    ret = UIM_FALSE;
  }

  UIM_CATCH_ERROR_END();

  return ret;
}

/****************************************************************
 * Optional APIs                                                *
 ****************************************************************/
void
uim_set_client_encoding(uim_context uc, const char *encoding)
{
  uim_lisp im_enc;

  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);
  assert(encoding);

  free(uc->client_encoding);
  uc->client_encoding = uim_strdup(encoding);

  protected0 = im_enc = uim_scm_callf("uim-context-encoding", "p", uc);
  uim_set_encoding(uc, REFER_C_STR(im_enc));

  UIM_CATCH_ERROR_END();
}

void
uim_set_configuration_changed_cb(uim_context uc,
				 void (*changed_cb)(void *ptr))
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  uc->configuration_changed_cb = changed_cb;

  UIM_CATCH_ERROR_END();
}

void
uim_set_im_switch_request_cb(uim_context uc,
			     void (*sw_app_im_cb)(void *ptr, const char *name),
			     void (*sw_system_im_cb)(void *ptr,
                                                     const char *name))
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  uc->switch_app_global_im_cb = sw_app_im_cb;
  uc->switch_system_global_im_cb = sw_system_im_cb;

  UIM_CATCH_ERROR_END();
}

void
uim_switch_im(uim_context uc, const char *engine)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  /* related to the commit log of r1400:

     We should not add the API uim_destroy_context(). We should move
     IM-switching feature into Scheme instead. It removes the context
     management code related to IM-switching duplicated in several IM
     bridges. Refer the implementation of GtkIMMulticontext as example
     of proxy-like IM-switcher. It does IM-switching without extending
     immodule API. We should follow its design to make our API simple.
     -- 2004-10-05 YamaKen
  */
  assert(uim_scm_gc_any_contextp());
  assert(uc);
  assert(engine);

  uim_scm_callf("uim-switch-im", "py", uc, engine);

  UIM_CATCH_ERROR_END();
}

const char *
uim_get_current_im_name(uim_context uc)
{
  uim_lisp im, ret;
  const char *name;

  if (UIM_CATCH_ERROR_BEGIN())
    return "direct";

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  protected0 = im = uim_scm_callf("uim-context-im", "p", uc);
  protected1 = ret = uim_scm_callf("im-name", "o", im);
  name = REFER_C_STR(ret);

  UIM_CATCH_ERROR_END();

  return name;
}

const char *
uim_get_default_im_name(const char *localename)
{
  uim_lisp ret;
  const char *name;

  if (UIM_CATCH_ERROR_BEGIN())
    return "direct";

  assert(uim_scm_gc_any_contextp());
  assert(localename);

  protected0 = ret = uim_scm_callf("uim-get-default-im-name", "s", localename);
  name = REFER_C_STR(ret);

  UIM_CATCH_ERROR_END();

  return name;
}

const char *
uim_get_im_name_for_locale(const char *localename)
{
  uim_lisp ret;
  const char *name;

  if (UIM_CATCH_ERROR_BEGIN())
    return "direct";

  assert(uim_scm_gc_any_contextp());
  assert(localename);

  protected0 =
    ret = uim_scm_callf("uim-get-im-name-for-locale", "s", localename);
  name = REFER_C_STR(ret);

  UIM_CATCH_ERROR_END();

  return name;
}

#if !UIM_USE_NOTIFY_PLUGINS
uim_bool
uim_notify_info(const char *msg_fmt, ...)
{
  va_list args;
  int ret;

  va_start(args, msg_fmt);
  fputs("libuim: [info] ", stderr);
  ret = vfprintf(stderr, msg_fmt, args);
  fputs("\n", stderr);
  va_end(args);

  return (ret >= 0);
}

uim_bool
uim_notify_fatal(const char *msg_fmt, ...)
{
  va_list args;
  int ret;

  va_start(args, msg_fmt);
  fputs("libuim: [fatal] ", stderr);
  ret = vfprintf(stderr, msg_fmt, args);
  fputs("\n", stderr);
  va_end(args);

  return (ret >= 0);
}

static uim_lisp
notify_get_plugins(void)
{
   return CONS(LIST3(MAKE_SYM("builtin"), MAKE_STR(N_("builtin")), MAKE_STR(N_("libuim builtin"))), uim_scm_null());
}

static uim_lisp
notify_info(uim_lisp msg_)
{
  const char *msg = REFER_C_STR(msg_);

  return MAKE_BOOL(uim_notify_info("%s", dgettext(GETTEXT_PACKAGE, msg)));
}

static uim_lisp
notify_fatal(uim_lisp msg_)
{
  const char *msg = REFER_C_STR(msg_);

  return MAKE_BOOL(uim_notify_fatal("%s", dgettext(GETTEXT_PACKAGE, msg)));
}

void
uim_init_notify_subrs(void)
{
  uim_scm_init_proc0("uim-notify-get-plugins", notify_get_plugins);
  uim_scm_init_proc1("uim-notify-info", notify_info);
  uim_scm_init_proc1("uim-notify-fatal", notify_fatal);
}
#endif  /* !UIM_USE_NOTIFY_PLUGINS */

void
uim_set_delay_candidate_selector_cb(uim_context uc,
                                    void (*delay_activate_cb)(void *ptr,
                                                              int delay))
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  uc->candidate_selector_delay_activate_cb = delay_activate_cb;

  UIM_CATCH_ERROR_END();
}

void
uim_delay_activating(uim_context uc, int *nr, int *display_limit, int *selected_index)
{
  struct uim_delay_activating_args args;

  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  args.uc = uc;
  args.nr = *nr;
  args.display_limit = *display_limit;
  args.selected_index = *selected_index;

  uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_delay_activating_internal, &args);
  *nr = args.nr;
  *display_limit = args.display_limit;
  *selected_index = args.selected_index;

  UIM_CATCH_ERROR_END();
}

static void *
uim_delay_activating_internal(struct uim_delay_activating_args *args)
{
  uim_context uc;
  uim_lisp triple;

  uc = args->uc;
  triple = uim_scm_callf("delay-activating-handler", "p", uc);
  if (LISTP(triple) && uim_scm_length(triple) == 3) {
    args->nr = C_INT(CAR(triple));
    args->display_limit = C_INT(CAR(CDR(triple)));
    args->selected_index = C_INT(CAR(CDR(CDR(triple))));
  }
  return NULL;
}

/****************************************************************
 * Legacy 'mode' API                                            *
 ****************************************************************/
int
uim_get_nr_modes(uim_context uc)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return 0;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  UIM_CATCH_ERROR_END();

  return uc->nr_modes;
}

const char *
uim_get_mode_name(uim_context uc, int nth)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return NULL;

  assert(uim_scm_gc_any_contextp());
  assert(uc);
  assert(nth >= 0);
  assert(nth < uc->nr_modes);

  UIM_CATCH_ERROR_END();

  return uc->modes[nth];
}

int
uim_get_current_mode(uim_context uc)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return 0;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  UIM_CATCH_ERROR_END();

  return uc->mode;
}

void
uim_set_mode(uim_context uc, int mode)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);
  assert(mode >= 0);

  uc->mode = mode;
  uim_scm_callf("mode-handler", "pi", uc, mode);

  UIM_CATCH_ERROR_END();
}

void
uim_set_mode_cb(uim_context uc, void (*update_cb)(void *ptr, int mode))
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  UIM_CATCH_ERROR_END();

  uc->mode_update_cb = update_cb;
}

void
uim_set_mode_list_update_cb(uim_context uc, void (*update_cb)(void *ptr))
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  UIM_CATCH_ERROR_END();

  uc->mode_list_update_cb = update_cb;
}

/****************************************************************
 * Legacy 'property list' API                                   *
 ****************************************************************/
void
uim_set_prop_list_update_cb(uim_context uc,
			    void (*update_cb)(void *ptr, const char *str))
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  uc->prop_list_update_cb = update_cb;

  UIM_CATCH_ERROR_END();
}

/* Obsolete */
void
uim_set_prop_label_update_cb(uim_context uc,
			     void (*update_cb)(void *ptr, const char *str))
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  UIM_CATCH_ERROR_END();
}

void
uim_prop_list_update(uim_context uc)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  if (uc->propstr && uc->prop_list_update_cb)
    uc->prop_list_update_cb(uc->ptr, uc->propstr);

  UIM_CATCH_ERROR_END();
}

/* Obsolete */
void
uim_prop_label_update(uim_context uc)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  UIM_CATCH_ERROR_END();
}

void
uim_prop_activate(uim_context uc, const char *str)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);
  assert(str);
      
  uim_scm_callf("prop-activate-handler", "ps", uc, str);

  UIM_CATCH_ERROR_END();
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
 * Update custom value from property message. The implementation
 * avoids arbitrary sexp evaluation for both custom symbol \a custom
 * and custom value \a val.
 */
void
uim_prop_update_custom(uim_context uc, const char *custom, const char *val)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  assert(uim_scm_gc_any_contextp());
  assert(uc);
  assert(custom);
  assert(val);

  uim_scm_callf("custom-set-handler", "pys", uc, custom, val);

  UIM_CATCH_ERROR_END();
}

uim_bool
uim_prop_reload_configs(void)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return UIM_FALSE;

  assert(uim_scm_gc_any_contextp());

  /* FIXME: handle return value properly. */
  uim_scm_callf("custom-reload-user-configs", "");

  UIM_CATCH_ERROR_END();

  return UIM_TRUE;
}

/****************************************************************
 * Legacy nth-index based IM management APIs                    *
 ****************************************************************/
int
uim_get_nr_im(uim_context uc)
{
  uim_lisp n_;
  int n;

  if (UIM_CATCH_ERROR_BEGIN())
    return 0;

  assert(uim_scm_gc_any_contextp());
  assert(uc);

  protected0 = n_ = uim_scm_callf("uim-n-convertible-ims", "p", uc);
  n = C_INT(n_);

  UIM_CATCH_ERROR_END();

  return n;
}

static uim_lisp
get_nth_im(uim_context uc, int nth)
{
  assert(uim_scm_gc_any_contextp());
  assert(uc);
  assert(nth >= 0);

  return uim_scm_callf("uim-nth-convertible-im", "pi", uc, nth);
}

const char *
uim_get_im_name(uim_context uc, int nth)
{
  uim_lisp im, str_;
  const char *str;

  if (UIM_CATCH_ERROR_BEGIN())
    return NULL;

  protected0 = im = get_nth_im(uc, nth);
  protected1 = str_ = uim_scm_callf("im-name", "o", im);
  str = REFER_C_STR(str_);

  UIM_CATCH_ERROR_END();

  return str;
}

const char *
uim_get_im_language(uim_context uc, int nth)
{
  uim_lisp im, str_;
  const char *str;

  if (UIM_CATCH_ERROR_BEGIN())
    return NULL;

  protected0 = im = get_nth_im(uc, nth);
  protected1 = str_ = uim_scm_callf("im-lang", "o", im);
  str = REFER_C_STR(str_);

  UIM_CATCH_ERROR_END();

  return str;
}

const char *
uim_get_im_encoding(uim_context uc, int nth)
{
  uim_lisp im, str_;
  const char *str;

  if (UIM_CATCH_ERROR_BEGIN())
    return NULL;

  protected0 = im = get_nth_im(uc, nth);
  protected1 = str_ = uim_scm_callf("im-encoding", "o", im);
  str = REFER_C_STR(str_);

  UIM_CATCH_ERROR_END();

  return str;
}

const char *
uim_get_im_short_desc(uim_context uc, int nth)
{
  uim_lisp im, short_desc;
  const char *str;

  if (UIM_CATCH_ERROR_BEGIN())
    return NULL;

  protected0 = im = get_nth_im(uc, nth);
  protected1 = short_desc = uim_scm_callf("im-short-desc", "o", im);
  str = (FALSEP(short_desc)) ? "-" : REFER_C_STR(short_desc);

  UIM_CATCH_ERROR_END();

  return str;
}
