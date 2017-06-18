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


#ifndef UIM_CUSTOM_H
#define UIM_CUSTOM_H

#include "uim.h"

#ifdef __cplusplus
extern "C" {
#endif

enum UCustomType {
  UCustom_Bool,
  UCustom_Int,
  UCustom_Str,
  UCustom_Pathname,
  UCustom_Choice,
  UCustom_OrderedList,
  UCustom_Key,
  UCustom_Table
};

enum UCustomPathnameType {
  UCustomPathnameType_RegularFile,
  UCustomPathnameType_Directory
};

enum UCustomKeyType {
  UCustomKey_Regular,   /* "<Control>j" */
  UCustomKey_Reference  /* "generic-cancel-key" */
};

enum UCustomKeyEditorType {
  UCustomKeyEditor_Basic,    /* only "<Control>j" style should be available */
  UCustomKeyEditor_Advanced  /* "generic-cancel-key" style should be specifiable */
};

union uim_custom_value {
  int as_bool;
  int as_int;
  char *as_str;
  struct uim_custom_pathname *as_pathname;
  struct uim_custom_choice *as_choice;
  struct uim_custom_choice **as_olist;
  struct uim_custom_key **as_key;
  char ***as_table;
};

struct uim_custom_pathname {
  char *str;
  int type;   /* UCustomPathnameType */
};

struct uim_custom_choice {
  char *symbol;
  char *label;
  char *desc;
};

struct uim_custom_key {
  int type;         /* UCustomKeyType */
  int editor_type;  /* UCustomKeyEditorType */
  char *literal;
  char *label;
  char *desc;
};

union uim_custom_range {
  struct {
    int min, max;
  } as_int;

  struct {
    char *regex;
  } as_str;

  struct {
    struct uim_custom_choice **valid_items;
  } as_choice;

  struct {
    struct uim_custom_choice **valid_items;  /* contains all possible items */
  } as_olist;

  struct {
    struct uim_custom_choice **valid_items;
  } as_table_header;
};

struct uim_custom {
  int type;  /* UCustomType */
  int is_active;
  char *symbol;
  char *label;
  char *desc;
  union uim_custom_value *value;
  union uim_custom_value *default_value;
  union uim_custom_range *range;
};

struct uim_custom_group {
  char *symbol;
  char *label;
  char *desc;
};


uim_bool uim_custom_enable(void);

/* load & save */
uim_bool uim_custom_load(void);
uim_bool uim_custom_save(void);
uim_bool uim_custom_save_custom(const char *custom_sym);
uim_bool uim_custom_broadcast(void);
uim_bool uim_custom_broadcast_reload_request(void);

/* custom variable */
struct uim_custom *uim_custom_get(const char *custom_sym);
uim_bool uim_custom_set(const struct uim_custom *custom);
void uim_custom_free(struct uim_custom *custom);

/* callback function */
uim_bool uim_custom_cb_add(const char *custom_sym, void *ptr,
			   void (*update_cb)(void *ptr, const char *custom_sym));
uim_bool uim_custom_cb_remove(const char *custom_sym);
uim_bool uim_custom_group_cb_add(const char *group_sym, void *ptr,
				 void (*update_cb)(void *ptr, const char *group_sym));
uim_bool uim_custom_group_cb_remove(const char *group_sym);
uim_bool uim_custom_global_cb_add(void *ptr,
				  void (*group_list_update_cb)(void *ptr));
uim_bool uim_custom_global_cb_remove(void);

/* literalization */
char *uim_custom_value_as_literal(const char *custom_sym);
char *uim_custom_definition_as_literal(const char *custom_sym);

/* custom group */
struct uim_custom_group *uim_custom_group_get(const char *group_sym);
void uim_custom_group_free(struct uim_custom_group *custom_group);

/* custom symbol list */
char **uim_custom_collect_by_group(const char *group_sym);
/* char **uim_custom_collect_by_groups(const char *const *group_syms); */

/* group symbol list */
char **uim_custom_groups(void);
char **uim_custom_primary_groups(void);
char **uim_custom_group_subgroups(const char *group_sym);

void uim_custom_symbol_list_free(char **symbol_list);

/* custom choice (for ordered list) */
struct uim_custom_choice *uim_custom_choice_new(char *symbol,
						char *label,
						char *desc);
void uim_custom_choice_list_free(struct uim_custom_choice **list);

/* custom key */
struct uim_custom_key *uim_custom_key_new(int type,
					  int editor_type,
					  char *literal,
					  char *label,
					  char *desc);
void uim_custom_key_list_free(struct uim_custom_key **list);

#ifdef __cplusplus
}
#endif
#endif  /* UIM_CUSTOM_H */
