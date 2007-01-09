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

#ifndef _uim_internal_h_included_
#define _uim_internal_h_included_

#include <config.h>

#include <stdio.h>

#include "uim.h"
#include "uim-scm.h"

#ifdef __cplusplus
extern "C" {
#endif

struct uim_im {
  char *name;
  char *lang;
  char *encoding;
  char *short_desc;
};

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

struct preedit_segment {
  int attr;
  char *str;
};

struct uim_context_ {
  /* cookier pointer */
  void *ptr;
  /* internal id */
  int id;
  /**/
  int is_enable;

  struct uim_code_converter *conv_if;
  void *outbound_conv;
  void *inbound_conv;
  char *current_im_name;
  char *short_desc;
  char *encoding;
  /**/
  int commit_raw_flag;
  /**/
  int nr_modes;
  char **modes;
  /**/
  int mode;
  /**/
  char *propstr;
  /**/
  int candidate_index;
  int nr_candidates;
  /**/
  void (*commit_cb)(void *ptr, const char *str);
  /* preedit */
  void (*preedit_clear_cb)(void *ptr);
  void (*preedit_pushback_cb)(void *ptr, int attr, const char *str);
  void (*preedit_update_cb)(void *ptr);
  /* mode list */
  void (*mode_list_update_cb)(void *ptr);
  /* mode */
  void (*mode_update_cb)(void *ptr, int);
  /* property list */
  void (*prop_list_update_cb)(void *ptr, const char *str);
  /* candidate window */
  void (*candidate_selector_activate_cb)(void *ptr, int nr, int index);
  void (*candidate_selector_select_cb)(void *ptr, int index);
  void (*candidate_selector_shift_page_cb)(void *ptr, int direction);
  void (*candidate_selector_deactivate_cb)(void *ptr);
  /* text acquisition */
  int (*acquire_text_cb)(void *ptr, enum UTextArea text_id, enum UTextOrigin origin, int former_len, int latter_len, char **former, char **latter);
  int (*delete_text_cb)(void *ptr, enum UTextArea text_id, enum UTextOrigin origin, int former_len, int latter_len);
  /* configuration changed */
  void (*configuration_changed_cb)(void *ptr);
  /* switch IM */
  void (*switch_app_global_im_cb)(void *ptr, const char *name);
  void (*switch_system_global_im_cb)(void *ptr, const char *name);
  /* preedit segments array */
  struct preedit_segment *psegs;
  int nr_psegs;
};


/**/
uim_context
uim_find_context(int id);
void uim_scm_init(const char *verbose_level);
void uim_scm_quit(void);

#ifdef UIM_COMPAT_SCM
void uim_init_compat_scm_subrs(void);
#endif
void uim_init_key_subrs(void);
void uim_init_util_subrs(void);
void uim_init_im_subrs(void);
void uim_init_intl_subrs(void);

/**/
void uim_init_plugin(void);
void uim_quit_plugin(void);

int uim_iconv_is_convertible(const char *tocode, const char *fromcode);
void *uim_iconv_create(const char *tocode, const char *fromcode);
char *uim_iconv_code_conv(void *obj, const char *str);
void uim_iconv_release(void *obj);

void uim_release_preedit_segments(uim_context uc);
void uim_update_preedit_segments(uim_context uc);

uim_bool uim_issetugid(void);

extern struct uim_im *uim_im_array;
extern int uim_nr_im;

#ifdef __cplusplus
}
#endif
#endif
