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

/*
   This is an obsolete API only exist for backward compatibility. New
   codes should not use this.

                                                2004-12-21 YamaKen
*/

/*                         !!CAUTION!!

   This API is designed for input method plugin and internal uim
   implementation. Other developers should not use this API since the
   API easily causes fatal crash involving GC if you does not pay
   attention enough. Be careful.

                                                2004-12-21 YamaKen
*/


/* API and ABI are unstable */
#ifndef _uim_compat_scm_h_included_
#define _uim_compat_scm_h_included_

#include "uim.h"
#include "uim-scm.h"

#ifdef __cplusplus
extern "C" {
#endif

char *uim_get_c_string(uim_lisp str);

long
uim_scm_repl_c_string(char *str, long want_init, long want_print);

uim_lisp
uim_scm_int_from_c_int(int integer);

uim_lisp
uim_scm_str_from_c_str(const char *str);

uim_lisp
uim_scm_c_strs_into_list(int n_strs, const char *const *strs);
uim_lisp
uim_scm_intern_c_str(const char *str);
uim_lisp
uim_scm_qintern_c_str(const char *str);

uim_lisp
uim_scm_nth(uim_lisp n, uim_lisp lst);

uim_lisp
uim_scm_nreverse(uim_lisp cell);

void
uim_scm_provide(const char *feature);

/*
  C representation of list: These interfaces are not yet stable and
  may be altered in near future. Be careful to use.
    -- YamaKen 2004-01-06
*/
typedef void *(*uim_scm_c_list_conv_func)(uim_lisp elem);
typedef void (*uim_scm_c_list_free_func)(void *elem);

void **uim_scm_c_list(const char *list_repl, const char *mapper_proc,
		      uim_scm_c_list_conv_func conv_func);
char *uim_scm_c_str_failsafe(uim_lisp str);
char **uim_scm_c_str_list(const char *list_repl, const char *mapper_proc);
void uim_scm_c_list_free(void **list, uim_scm_c_list_free_func free_func);

/* function table for dynamic loading */
struct uim_api_tbl {
  int       (*uim_init)(void);
  void      (*uim_quit)(void);

  FILE     *(*uim_scm_get_output)(void);
  void      (*uim_scm_set_output)(FILE *fp);
  long      (*uim_scm_get_verbose_level)(void);
  void      (*uim_scm_set_verbose_level)(long new_value);
  void      (*uim_scm_load_file)(const char *fn);

  int       (*uim_scm_c_int)(uim_lisp integer);
  int       (*uim_scm_symbol_value_int)(const char *symbol_str);
  uim_lisp  (*uim_scm_int_from_c_int)(int integer);
  char     *(*uim_scm_c_str)(uim_lisp str);
  char     *(*uim_scm_symbol_value_str)(const char *symbol_str);
  uim_lisp  (*uim_scm_str_from_c_str)(const char *str);
  uim_lisp  (*uim_scm_c_strs_into_list)(int n_strs, const char *const *strs);
  uim_lisp  (*uim_scm_symbol_value)(const char *symbol_str);
  uim_lisp  (*uim_scm_intern_c_str)(const char *str);
  uim_lisp  (*uim_scm_qintern_c_str)(const char *str);
  void      (*uim_scm_gc_protect)(uim_lisp *location);
  long      (*uim_scm_repl_c_string)(char *str,
				     long want_init, long want_print);

  uim_lisp  (*uim_scm_t)(void);
  uim_lisp  (*uim_scm_f)(void);
  uim_lisp  (*uim_scm_null_list)(void);
  int       (*uim_scm_nullp)(uim_lisp obj);
  int       (*uim_scm_eq)(uim_lisp a, uim_lisp b);
  int       (*uim_scm_string_equal)(uim_lisp a, uim_lisp b);
  uim_lisp  (*uim_scm_eval)(uim_lisp obj);
  uim_lisp  (*uim_scm_quote)(uim_lisp obj);
  uim_lisp  (*uim_scm_car)(uim_lisp cell);
  uim_lisp  (*uim_scm_cdr)(uim_lisp cell);
  uim_lisp  (*uim_scm_cadr)(uim_lisp cell);
  uim_lisp  (*uim_scm_caar)(uim_lisp cell);
  uim_lisp  (*uim_scm_cdar)(uim_lisp cell);
  uim_lisp  (*uim_scm_cddr)(uim_lisp cell);
  uim_lisp  (*uim_scm_cons)(uim_lisp car, uim_lisp cdr);
  uim_lisp  (*uim_scm_nth)(uim_lisp n, uim_lisp lst);
  uim_lisp  (*uim_scm_list1)(uim_lisp elm1);
  uim_lisp  (*uim_scm_list2)(uim_lisp elm1, uim_lisp elm2);
  uim_lisp  (*uim_scm_list3)(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3);
  uim_lisp  (*uim_scm_list4)(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3,
			     uim_lisp elm4);
  uim_lisp  (*uim_scm_list5)(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3,
			     uim_lisp elm4, uim_lisp elm5);

  uim_lisp  (*uim_scm_reverse)(uim_lisp cell);
  uim_lisp  (*uim_scm_nreverse)(uim_lisp cell);

  uim_lisp  (*uim_custom_value)(uim_lisp custom_sym);
  int       (*uim_custom_value_as_bool)(uim_lisp custom_sym);
  int       (*uim_custom_value_as_int)(uim_lisp custom_sym);
  char     *(*uim_custom_value_as_str)(uim_lisp custom_sym);
  char     *(*uim_custom_value_as_path)(uim_lisp custom_sym);
  uim_lisp  (*uim_custom_value_as_symbol)(uim_lisp custom_sym);
  void      (*uim_custom_set)(uim_lisp custom_sym, uim_lisp custom_val);
  char     *(*uim_custom_symbol_label)(uim_lisp custom_sym, uim_lisp val_sym);
  char     *(*uim_custom_symbol_desc)(uim_lisp custom_sym, uim_lisp val_sym);
  uim_lisp  (*uim_custom_label)(uim_lisp custom_sym);
  uim_lisp  (*uim_custom_desc)(uim_lisp custom_sym);
  uim_lisp  (*uim_custom_type)(uim_lisp custom_sym);
  uim_lisp  (*uim_custom_default_value)(uim_lisp custom_sym);
  int       (*uim_custom_ctype)(uim_lisp custom_sym);
  uim_lisp  (*uim_custom_range)(uim_lisp custom_sym);
  char     *(*uim_custom_group_label)(uim_lisp group_sym);
  char     *(*uim_custom_group_desc)(uim_lisp group_sym);
  uim_lisp  (*uim_custom_group_subgroups)(uim_lisp group_sym);
  uim_lisp  (*uim_custom_list_groups)(void);
  uim_lisp  (*uim_custom_list_primary_groups)(void);
  uim_lisp  (*uim_custom_collect_by_group)(uim_lisp group_sym);
  char     *(*uim_custom_value_as_string)(uim_lisp sym);
  char     *(*uim_custom_definition_as_string)(uim_lisp sym);
};

#ifdef __cplusplus
}
#endif
#endif
