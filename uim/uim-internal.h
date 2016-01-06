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

#ifndef UIM_INTERNAL_H
#define UIM_INTERNAL_H

#include <config.h>

#if UIM_USE_ERROR_GUARD
#include <setjmp.h>
#endif
#if HAVE_ISSETUGID
#include <unistd.h>
#endif

#include "uim.h"
#include "uim-scm.h"

#ifdef __cplusplus
extern "C" {
#endif


#if UIM_USE_ERROR_GUARD
#if HAVE_SIGSETJMP
#define JMP_BUF           sigjmp_buf
#define SETJMP(env)       sigsetjmp((env), 1)
#define LONGJMP(env, val) siglongjmp((env), (val))
#else
#define JMP_BUF           jmp_buf
#define SETJMP(env)       setjmp(env)
#define LONGJMP(env, val) longjmp((env), (val))
#endif
#endif /* UIM_USE_ERROR_GUARD */


struct uim_candidate_ {
  char *str;         /* candidate */
  char *heading_label;
  char *annotation;
  /* uim_pos part_of_speech; */
  /* int freq; */
  /* int freshness; */
  /* int formality; */
  /* char *src_dict; */
};

struct uim_context_ {
  uim_lisp sc;  /* Scheme-side context */
  void *ptr;    /* 1st callback argument */

  /* encoding handlings */
  char *client_encoding;
  struct uim_code_converter *conv_if;
  void *outbound_conv;
  void *inbound_conv;

  /* whether key input to IM is enabled */
  uim_bool is_enabled;

  /* legacy 'mode' API*/
  int mode;
  int nr_modes;
  char **modes;
  /* legacy 'property' API */
  char *propstr;

  /* commit */
  void (*commit_cb)(void *ptr, const char *str);
  /* preedit */
  void (*preedit_clear_cb)(void *ptr);
  void (*preedit_pushback_cb)(void *ptr, int attr, const char *str);
  void (*preedit_update_cb)(void *ptr);
  /* candidate selector */
  void (*candidate_selector_activate_cb)(void *ptr, int nr, int index);
  void (*candidate_selector_select_cb)(void *ptr, int index);
  void (*candidate_selector_shift_page_cb)(void *ptr, int direction);
  void (*candidate_selector_deactivate_cb)(void *ptr);
  void (*candidate_selector_delay_activate_cb)(void *ptr, int delay);
  /* text acquisition */
  int (*acquire_text_cb)(void *ptr,
                         enum UTextArea text_id, enum UTextOrigin origin,
                         int former_len, int latter_len,
                         char **former, char **latter);
  int (*delete_text_cb)(void *ptr,
                        enum UTextArea text_id, enum UTextOrigin origin,
                        int former_len, int latter_len);

  /* mode */
  void (*mode_list_update_cb)(void *ptr);
  void (*mode_update_cb)(void *ptr, int);
  /* property */
  void (*prop_list_update_cb)(void *ptr, const char *str);

  /* configuration changed */
  void (*configuration_changed_cb)(void *ptr);
  /* IM switching */
  void (*switch_app_global_im_cb)(void *ptr, const char *name);
  void (*switch_system_global_im_cb)(void *ptr, const char *name);
};

void uim_init_error(void);
#if UIM_USE_ERROR_GUARD
/* internal functions: don't call directly */
uim_bool uim_caught_fatal_error(void);
uim_bool uim_catch_error_begin_pre(void);
uim_bool uim_catch_error_begin_post(void);
void     uim_catch_error_end(void);

/* can be nested */
#define UIM_CATCH_ERROR_BEGIN()						\
  (uim_caught_fatal_error()						\
   || (uim_catch_error_begin_pre()					\
       && SETJMP(uim_catch_block_env)					\
       && uim_catch_error_begin_post()))
#define UIM_CATCH_ERROR_END() uim_catch_error_end()
#else /* not UIM_USE_ERROR_GUARD */
/* if !UIM_USE_ERROR_GUARD, uim immediately exit(3)s on any error. */
#define UIM_CATCH_ERROR_BEGIN() UIM_FALSE
#define UIM_CATCH_ERROR_END()   ((void)0)
#endif /* not UIM_USE_ERROR_GUARD */
/* throw recoverable error */
void    uim_throw_error(const char *msg);

void uim_init_dynlib(void);
void uim_quit_dynlib(void);

void uim_init_im_subrs(void);
void uim_init_key_subrs(void);
void uim_init_util_subrs(void);
void uim_init_notify_subrs(void);

void uim_init_rk_subrs(void);
void uim_init_intl_subrs(void);

#if UIM_USE_NOTIFY_PLUGINS
uim_bool uim_notify_fatal_raw(const char *msg);
#else
uim_bool uim_notify_info(const char *msg_fmt, ...);
uim_bool uim_notify_fatal(const char *msg_fmt, ...);
#endif

void uim_set_encoding(uim_context uc, const char *enc);
#if HAVE_ISSETUGID
#define uim_issetugid() issetugid()
#else
uim_bool uim_issetugid(void);
#endif


#if UIM_USE_ERROR_GUARD
/* don't touch directly */
extern JMP_BUF uim_catch_block_env;
#endif

#ifdef __cplusplus
}
#endif
#endif
