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

#ifndef _context_h_included_
#define _context_h_included_

#include <stdio.h>
#include "config.h"
#include "gettext.h"
#include "uim.h"
#include "uim-scm.h"
#include "siod.h"

struct uim_im {
  char *name;
  char *lang;
  char *encoding;
  char *short_desc;
};

struct cb_q {
  struct cb *first_cb;
  struct cb *tail_cb;
  int flushing;
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
  void *conv;
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
  char *proplabelstr;
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
  /* property label */
  void (*prop_label_update_cb)(void *ptr, const char *str);
  /* candidate window */
  void (*candidate_selector_activate_cb)(void *ptr, int nr, int index);
  void (*candidate_selector_select_cb)(void *ptr, int index);
  void (*candidate_selector_shift_page_cb)(void *ptr, int direction);
  void (*candidate_selector_deactivate_cb)(void *ptr);
  /* surrounding text */
  void (*request_surrounding_text_cb)(void *ptr);
  int (*delete_surrounding_text_cb)(void *ptr, int offset, int len);
  /* callback queue */
  struct cb_q cb_q;
  /* preedit segments array */
  struct preedit_segment *psegs;
  int nr_psegs;
};

enum {
  CAND_ACTIVATE_CB,
  CAND_SELECT_CB,
  CAND_DEACTIVATE_CB,
  COMMIT_CB,
  PREEDIT_CLEAR_CB,
  PREEDIT_PUSHBACK_CB,
  PREEDIT_UPDATE_CB,
  MODE_UPDATE_CB,
  MODE_LIST_UPDATE_CB,
  PROP_LABEL_UPDATE_CB,
  PROP_LIST_UPDATE_CB,
  CAND_SHIFT_PAGE_CB,
  REQUEST_SURROUNDING_CB,
  DELETE_SURROUNDING_CB
};

struct cb {
  int type;
  char *str;
  int n1, n2;
  struct cb *next;
};


#ifdef ENABLE_NLS
#define UIM_PREPARE_SAVING_TEXTDOMAIN_CODESET() \
    const char *orig_encoding, *client_encoding;
#define UIM_SWITCH_TEXTDOMAIN_CODESET(uc) \
  orig_encoding = bind_textdomain_codeset(GETTEXT_PACKAGE, NULL); \
  client_encoding = (uc) ? ((struct uim_context_ *)uc)->encoding : uim_last_client_encoding; \
  bind_textdomain_codeset(GETTEXT_PACKAGE, client_encoding);
#define UIM_RESTORE_TEXTDOMAIN_CODESET() \
  bind_textdomain_codeset(GETTEXT_PACKAGE, orig_encoding);
#else  /* ENABLE_NLS */
#define UIM_PREPARE_SAVING_TEXTDOMAIN_CODESET()
#define UIM_SWITCH_TEXTDOMAIN_CODESET(uc)
#define UIM_RESTORE_TEXTDOMAIN_CODESET()
#endif  /* ENABLE_NLS */

/* we cannot use the variadic macro (i.e. __VA_ARGS__) because we
   should also support C89 compilers
*/
#define UIM_EVAL_STRING_INTERNAL(uc, sexp_str) \
      if (uc) \
        uim_eval_string(uc, sexp_str); \
      else \
        repl_c_string(sexp_str, 0, 1); \

#define UIM_EVAL_STRING(uc, sexp_str) \
  { \
    UIM_PREPARE_SAVING_TEXTDOMAIN_CODESET(); \
    UIM_SWITCH_TEXTDOMAIN_CODESET(uc); \
    UIM_EVAL_STRING_INTERNAL(uc, sexp_str); \
    UIM_RESTORE_TEXTDOMAIN_CODESET(); \
  }

#define UIM_EVAL_FSTRING1(uc, sexp_tmpl, arg1) \
  { \
    int form_size; \
    char *buf; \
    UIM_PREPARE_SAVING_TEXTDOMAIN_CODESET(); \
    UIM_SWITCH_TEXTDOMAIN_CODESET(uc); \
    form_size = uim_sizeof_sexp_str(sexp_tmpl, arg1); \
    if (form_size != -1) { \
      buf = malloc(form_size); \
      snprintf(buf, form_size, sexp_tmpl, arg1); \
      UIM_EVAL_STRING_INTERNAL(uc, buf); \
      free(buf); \
    } \
    UIM_RESTORE_TEXTDOMAIN_CODESET(); \
  }

#define UIM_EVAL_FSTRING2(uc, sexp_tmpl, arg1, arg2) \
  { \
    int form_size; \
    char *buf; \
    UIM_PREPARE_SAVING_TEXTDOMAIN_CODESET(); \
    UIM_SWITCH_TEXTDOMAIN_CODESET(uc); \
    form_size = uim_sizeof_sexp_str(sexp_tmpl, arg1, arg2); \
    if (form_size != -1) { \
      buf = malloc(form_size); \
      snprintf(buf, form_size, sexp_tmpl, arg1, arg2); \
      UIM_EVAL_STRING_INTERNAL(uc, buf); \
      free(buf); \
    } \
    UIM_RESTORE_TEXTDOMAIN_CODESET(); \
  }

#define UIM_EVAL_FSTRING3(uc, sexp_tmpl, arg1, arg2, arg3) \
  { \
    int form_size; \
    char *buf; \
    UIM_PREPARE_SAVING_TEXTDOMAIN_CODESET(); \
    UIM_SWITCH_TEXTDOMAIN_CODESET(uc); \
    form_size = uim_sizeof_sexp_str(sexp_tmpl, arg1, arg2, arg3); \
    if (form_size != -1) { \
      buf = malloc(form_size); \
      snprintf(buf, form_size, sexp_tmpl, arg1, arg2, arg3); \
      UIM_EVAL_STRING_INTERNAL(uc, buf); \
      free(buf); \
    } \
    UIM_RESTORE_TEXTDOMAIN_CODESET(); \
  }

#define UIM_EVAL_FSTRING4(uc, sexp_tmpl, arg1, arg2, arg3, arg4) \
  { \
    int form_size; \
    char *buf; \
    UIM_PREPARE_SAVING_TEXTDOMAIN_CODESET(); \
    UIM_SWITCH_TEXTDOMAIN_CODESET(uc); \
    form_size = uim_sizeof_sexp_str(sexp_tmpl, arg1, arg2, arg3, arg4); \
    if (form_size != -1) { \
      buf = malloc(form_size); \
      snprintf(buf, form_size, sexp_tmpl, arg1, arg2, arg3, arg4); \
      UIM_EVAL_STRING_INTERNAL(uc, buf); \
      free(buf); \
    } \
    UIM_RESTORE_TEXTDOMAIN_CODESET(); \
  }

#define UIM_EVAL_FSTRING5(uc, sexp_tmpl, arg1, arg2, arg3, arg4, arg5) \
  { \
    int form_size; \
    char *buf; \
    UIM_PREPARE_SAVING_TEXTDOMAIN_CODESET(); \
    UIM_SWITCH_TEXTDOMAIN_CODESET(uc); \
    form_size = uim_sizeof_sexp_str(sexp_tmpl, arg1, arg2, arg3, arg4, arg5); \
    if (form_size != -1) { \
      buf = malloc(form_size); \
      snprintf(buf, form_size, sexp_tmpl, arg1, arg2, arg3, arg4, arg5); \
      UIM_EVAL_STRING_INTERNAL(uc, buf); \
      free(buf); \
    } \
    UIM_RESTORE_TEXTDOMAIN_CODESET(); \
  }

/**/
uim_context
uim_find_context(int id);
void uim_init_key_subrs(void);
void uim_init_util_subrs(void);
void uim_init_table_subrs(void);
void uim_init_im_subrs(void);

/**/
void uim_init_skk_dic(void);
void uim_quit_skk_dic(void);
void uim_init_anthy(void);
void uim_quit_anthy(void);
void uim_init_prime(void);
void uim_quit_prime(void);
void uim_init_m17nlib(void);
void uim_quit_m17nlib(void);

/**/
char *uim_get_c_string(LISP str);
int uim_key_sym_to_int(LISP sym);

int uim_iconv_is_convertible(const char *tocode, const char *fromcode);
void *uim_iconv_create(const char *tocode, const char *fromcode);
char *uim_iconv_code_conv(void *obj, const char *str);
void uim_iconv_release(void *obj);

int uim_sizeof_sexp_str(const char *tmpl, ...);
void uim_eval_string(uim_context, char *str);
void uim_schedule_cb(uim_context, int type, char *str, int n1, int n2);
void uim_release_preedit_segments(uim_context uc);
void uim_update_preedit_segments(uim_context uc);

extern struct uim_im *uim_im_array;
extern int uim_nr_im;
extern char *uim_last_client_encoding;

#endif
