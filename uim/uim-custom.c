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

/*
  This implementation is not yet working and lacking error
  handlings. Restruction of uim-scm API is required to work.
    -- YamaKen 2004-12-17
*/

/*
  Don't insert NULL checks for free(3). free(3) accepts NULL as proper
  argument that causes no action.  -- YamaKen 2004-12-17
*/

#define UIM_NO_COMPAT_CUSTOM

#include <stdlib.h>
#include <string.h>
#include "config.h"
#include "context.h"
#include "uim-scm.h"
#include "uim-custom.h"


static int uim_custom_type_eq(const char *custom_sym, const char *custom_type);
static int uim_custom_type(const char *custom_sym);
static int uim_custom_is_active(const char *custom_sym);
static char *uim_custom_get_str(const char *custom_sym, const char *proc);
static char *uim_custom_label(const char *custom_sym);
static char *uim_custom_desc(const char *custom_sym);
static struct uim_custom_choice *uim_custom_choice_get(const char *custom_sym, const char *choice_sym);
static void uim_custom_choice_free(struct uim_custom_choice *custom_choice);
static struct uim_custom_choice **uim_custom_choice_item_list(const char *custom_sym);
void uim_custom_choice_list_free(struct uim_custom_choice **list);
static union uim_custom_value *uim_custom_value_internal(const char *custom_sym, const char *getter_proc);

static union uim_custom_value *uim_custom_value(const char *custom_sym);
static union uim_custom_value *uim_custom_default_value(const char *custom_sym);
static void uim_custom_value_free(int custom_type, union uim_custom_value *custom_value);
static uim_lisp uim_custom_range_elem(const char *custom_sym, const char *accessor_proc);
static union uim_custom_range *uim_custom_range_get(const char *custom_sym);
static void uim_custom_range_free(int custom_type, union uim_custom_range *custom_range);

static const char str_list_arg[] = "uim-custom-c-str-list-arg";


#if 1  /* should be reorganized into uim-scm.[hc] */
#define TRUEP(x) EQ(x, true_sym)
#define FALSEP(x) EQ(x, false_sym)
 
#define NTRUEP(x) NEQ(x, true_sym)
#define NFALSEP(x) NEQ(x, false_sym)

typedef void *(*uim_scm_c_list_conv_func)(uim_lisp elem);
typedef void (*uim_scm_c_list_free_func)(void *elem);

uim_lisp uim_scm_return_value(void);
char *uim_scm_c_symbol(uim_lisp symbol);
void **uim_scm_c_list(const char *list_repl, const char *mapper_proc,
		      uim_scm_c_list_conv_func conv_func);
char *uim_scm_c_str_failsafe(uim_lisp str);
char **uim_scm_c_str_list(const char *list_repl, const char *mapper_proc);
void uim_scm_c_list_free(void **list, uim_scm_c_list_free_func free_func);

static uim_lisp true_sym;
static uim_lisp false_sym;
static uim_lisp return_val;


uim_lisp
uim_scm_return_value(void)
{
  return (uim_lisp)siod_return_value();
}

char *
uim_scm_c_symbol(uim_lisp symbol)
{
  /* siod dependent */
  return uim_scm_c_str(symbol);
}

/*
  - list_repl must always returns same list for each evaluation
  - returns NULL terminated array. NULL will not appeared except terminator
  - non-string element such as #f is converted to ""
 */
void **
uim_scm_c_list(const char *list_repl, const char *mapper_proc,
	       uim_scm_c_list_conv_func conv_func)
{
  int list_len, i;
  void **result;

  UIM_EVAL_FSTRING1(NULL, "(length %s)", list_repl);
  return_val = uim_scm_return_value();
  list_len = uim_scm_c_int(return_val);

  result = (void **)malloc(sizeof(void *) * (list_len + 1));
  result[list_len] = NULL;
  for (i = 0; i < list_len; i++) {
    UIM_EVAL_FSTRING3(NULL, "(%s (nth %d %s))", mapper_proc, i, list_repl);
    return_val = uim_scm_return_value();
    result[i] = (*conv_func)(return_val);
  }

  return result;
}

char *
uim_scm_c_str_failsafe(uim_lisp str)
{
  char *c_str;
  c_str = uim_scm_c_str(str);
  return (c_str) ? c_str : strdup("");
}

char **
uim_scm_c_str_list(const char *list_repl, const char *mapper_proc)
{
  void **list;
  
  list = uim_scm_c_list(list_repl, mapper_proc,
			(uim_scm_c_list_conv_func)uim_scm_c_str_failsafe);

  return (char **)list;
}

void
uim_scm_c_list_free(void **list, uim_scm_c_list_free_func free_func)
{
  void *elem;
  void **p;

  for (p = list; elem = *p; p++) {
    free_func(elem);
  }
  free(list);
}

#endif  /* should be reorganized into uim-scm.[hc] and others */


static int
uim_custom_type_eq(const char *custom_sym, const char *custom_type)
{
  UIM_EVAL_FSTRING2(NULL, "(eq? (custom-type '%s) '%s)",
		    custom_sym, custom_type);

  return NFALSEP(uim_scm_return_value());
}

static int
uim_custom_type(const char *custom_sym) {
  if (uim_custom_type_eq(custom_sym, "boolean")) {
    return UCustom_Bool;
  } else if (uim_custom_type_eq(custom_sym, "integer")) {
    return UCustom_Int;
  } else if (uim_custom_type_eq(custom_sym, "string")) {
    return UCustom_Str;
  } else if (uim_custom_type_eq(custom_sym, "pathname")) {
    return UCustom_Pathname;
  } else if (uim_custom_type_eq(custom_sym, "symbol")) {
    return UCustom_Choice;
  } else if (uim_custom_type_eq(custom_sym, "key")) {
    return UCustom_Key;
  } else {
    return UCustom_Bool;
  }
}

static int
uim_custom_is_active(const char *custom_sym)
{
  UIM_EVAL_FSTRING1(NULL, "(custom-active? '%s)", custom_sym);
  return_val = uim_scm_return_value();

  return NFALSEP(return_val);
}

static char *
uim_custom_get_str(const char *custom_sym, const char *proc)
{
  UIM_EVAL_FSTRING2(NULL, "(%s '%s)", proc, custom_sym);
  return_val = uim_scm_return_value();

  /*
    The arg must be assigned to return_val to be proteced from GC
    while evaluation
  */
  return uim_scm_c_str(return_val);
}

static char *
uim_custom_label(const char *custom_sym)
{
  return uim_custom_get_str(custom_sym, "custom-label");
}

static char *
uim_custom_desc(const char *custom_sym)
{
  return uim_custom_get_str(custom_sym, "custom-desc");
}

static struct uim_custom_choice *
uim_custom_choice_get(const char *custom_sym, const char *choice_sym)
{
  struct uim_custom_choice *c_choice;

  c_choice = (struct uim_custom_choice *)malloc(sizeof(struct uim_custom_choice));
  c_choice->symbol = strdup(choice_sym);

  UIM_EVAL_FSTRING2(NULL, "(custom-symbol-label '%s '%s)",
		    custom_sym, choice_sym);
  return_val = uim_scm_return_value();
  c_choice->label = uim_scm_c_str(return_val);

  UIM_EVAL_FSTRING2(NULL, "(custom-symbol-desc '%s '%s)",
		    custom_sym, choice_sym);
  return_val = uim_scm_return_value();
  c_choice->desc = uim_scm_c_str(return_val);

  return c_choice;
}

static void
uim_custom_choice_free(struct uim_custom_choice *custom_choice)
{
  free(custom_choice->symbol);
  free(custom_choice->label);
  free(custom_choice->desc);
  free(custom_choice);
}

static struct uim_custom_choice **
uim_custom_choice_item_list(const char *custom_sym)
{
  char *choice_sym, **choice_sym_list, **p;
  struct uim_custom_choice *custom_choice, **custom_choice_list;

  UIM_EVAL_FSTRING2(NULL, "(define %s (custom-range '%s))",
		    str_list_arg, custom_sym);
  choice_sym_list =
    (char **)uim_scm_c_list(str_list_arg, "custom-symbol-rec-sym",
			    (uim_scm_c_list_conv_func)uim_scm_c_symbol);

  for (p = choice_sym_list; choice_sym = *p; p++) {
    custom_choice = uim_custom_choice_get(custom_sym, choice_sym);
    *p = (char *)custom_choice;  /* intentionally overwrite */
  }
  /* reuse the list structure */
  custom_choice_list = (struct uim_custom_choice **)choice_sym_list;

  return custom_choice_list;
}

void
uim_custom_choice_list_free(struct uim_custom_choice **list)
{
  uim_scm_c_list_free((void **)list,
		      (uim_scm_c_list_free_func)uim_custom_choice_free);
}

static union uim_custom_value *
uim_custom_value_internal(const char *custom_sym, const char *getter_proc)
{
  int type;
  union uim_custom_value *value;
  char *custom_value_symbol;

  value = (union uim_custom_value *)malloc(sizeof(union uim_custom_value));

  UIM_EVAL_FSTRING2(NULL, "(%s '%s)", getter_proc, custom_sym);
  return_val = uim_scm_return_value();

  type = uim_custom_type(custom_sym);
  switch (type) {
  case UCustom_Bool:
    value->as_bool = NFALSEP(return_val);
    break;
  case UCustom_Int:
    value->as_int = uim_scm_c_int(return_val);
    break;
  case UCustom_Str:
    value->as_str = uim_scm_c_str(return_val);
    break;
  case UCustom_Pathname:
    value->as_pathname = uim_scm_c_str(return_val);
    break;
  case UCustom_Choice:
    custom_value_symbol = uim_scm_c_symbol(return_val);
    value->as_choice = uim_custom_choice_get(custom_sym, custom_value_symbol);
    free(custom_value_symbol);
    break;
#if 0
  case UCustom_Key:
    value->as_key = uim_scm_c_str(return_val);
    break;
#endif
  default:
    value = NULL;
  }

  return value;
}

static union uim_custom_value *
uim_custom_value(const char *custom_sym)
{
  return uim_custom_value_internal(custom_sym, "custom-value");
}

static union uim_custom_value *
uim_custom_default_value(const char *custom_sym)
{
  return uim_custom_value_internal(custom_sym, "custom-default-value");
}

static void
uim_custom_value_free(int custom_type, union uim_custom_value *custom_value)
{
  switch (custom_type) {
  case UCustom_Str:
    free(custom_value->as_str);
    break;
  case UCustom_Pathname:
    free(custom_value->as_pathname);
    break;
  case UCustom_Choice:
    uim_custom_choice_free(custom_value->as_choice);
    break;
#if 0
  case UCustom_Key:
    free(custom_value->as_key);
    break;
#endif
  }
  free(custom_value);
}

/*
  The arg must be assigned to return_val by caller to be proteced
  from GC while subsequent evaluation
*/
static uim_lisp
uim_custom_range_elem(const char *custom_sym, const char *accessor_proc)
{
  UIM_EVAL_FSTRING2(NULL, "(%s (custom-range '%s))",
		    accessor_proc, custom_sym);
  
  return uim_scm_return_value();
}

static union uim_custom_range *
uim_custom_range_get(const char *custom_sym)
{
  int type;
  union uim_custom_range *range;

  range = (union uim_custom_range *)malloc(sizeof(union uim_custom_range));
  type = uim_custom_type(custom_sym);
  switch (type) {
  case UCustom_Int:
    return_val = uim_custom_range_elem(custom_sym, "car");
    range->as_int.min = uim_scm_c_int(return_val);
    return_val = uim_custom_range_elem(custom_sym, "cadr");
    range->as_int.max = uim_scm_c_int(return_val);
    break;
  case UCustom_Str:
    return_val = uim_custom_range_elem(custom_sym, "car");
    range->as_str.regex = uim_scm_c_str(return_val);
    break;
  case UCustom_Choice:
    range->as_choice.valid_items = uim_custom_choice_item_list(custom_sym);
    break;
  }

  return range;
}

static void
uim_custom_range_free(int custom_type, union uim_custom_range *custom_range)
{
  switch (custom_type) {
  case UCustom_Str:
    free(custom_range->as_str.regex);
    break;
  case UCustom_Choice:
    uim_custom_choice_list_free(custom_range->as_choice.valid_items);
    break;
#if 0
  case UCustom_Key:
    free(custom_value->as_key);
    break;
#endif
  }
  free(custom_range);
}

uim_bool
uim_custom_init(void)
{
#if 1  /* should be reorganized into uim-scm.c */
  true_sym  = (uim_lisp)siod_true_value();
#if 0
  false_sym = (uim_lisp)siod_false_value();
#else
  /* false_sym has to be NIL until bug #617 and #642 are fixed
   * -- YamaKen
   */
  false_sym = uim_scm_f();
#endif
#endif

  uim_scm_load_file("custom.scm");
  uim_scm_gc_protect(return_val);

  return UIM_TRUE;
}

uim_bool
uim_custom_quit(void)
{
  /* TODO */
  return UIM_TRUE;
}

uim_bool
uim_custom_save(void)
{
  /* TODO */
  return UIM_TRUE;
}

uim_bool
uim_custom_broadcast(void)
{
  /* TODO */
  return UIM_TRUE;
}

struct uim_custom *
uim_custom_get(const char *custom_sym)
{
  struct uim_custom *custom;

  custom = (struct uim_custom *)malloc(sizeof(struct uim_custom));
  custom->type = uim_custom_type(custom_sym);
  custom->is_active = uim_custom_is_active(custom_sym);
  custom->symbol = strdup(custom_sym);
  custom->label = uim_custom_label(custom_sym);
  custom->desc = uim_custom_desc(custom_sym);
  custom->value = uim_custom_value(custom_sym);
  custom->default_value = uim_custom_default_value(custom_sym);
  custom->range = uim_custom_range_get(custom_sym);

  return custom;
}

uim_bool
uim_custom_set(const struct uim_custom *custom)
{
  switch (custom->type) {
  case UCustom_Bool:
    UIM_EVAL_FSTRING2(NULL, "(custom-set! '%s #%s)",
		      custom->symbol, (custom->value->as_bool) ? "t" : "f");
    break;
  case UCustom_Int:
    UIM_EVAL_FSTRING2(NULL, "(custom-set! '%s %d)",
		      custom->symbol, custom->value->as_int);
    break;
  case UCustom_Str:
    UIM_EVAL_FSTRING2(NULL, "(custom-set! '%s %s)",
		      custom->symbol, custom->value->as_str);
    break;
  case UCustom_Pathname:
    UIM_EVAL_FSTRING2(NULL, "(custom-set! '%s %s)",
		      custom->symbol, custom->value->as_pathname);
    break;
  case UCustom_Choice:
    UIM_EVAL_FSTRING2(NULL, "(custom-set! '%s '%s)",
		      custom->symbol, custom->value->as_choice->symbol);
    break;
#if 0
  case UCustom_Key:
    UIM_EVAL_FSTRING2(NULL, "(custom-set! '%s %s)",
		      custom->symbol, custom->value->as_key);
    break;
#endif
  default:
    return UIM_FALSE;
  }
  return NFALSEP(uim_scm_return_value());
}

void
uim_custom_free(struct uim_custom *custom)
{
  free(custom->symbol);
  free(custom->label);
  free(custom->desc);
  uim_custom_value_free(custom->type, custom->value);
  uim_custom_value_free(custom->type, custom->default_value);
  uim_custom_range_free(custom->type, custom->range);
  free(custom);
}

char *
uim_custom_value_as_literal(const char *custom_sym)
{
  return uim_custom_get_str(custom_sym, "custom-canonical-value-as-string");
}

char *
uim_custom_definition_as_literal(const char *custom_sym)
{
  return uim_custom_get_str(custom_sym, "custom-as-string");
}

struct uim_custom_group *
uim_custom_group_get(const char *group_sym)
{
  struct uim_custom_group *custom_group;

  custom_group = (struct uim_custom_group *)malloc(sizeof(struct uim_custom_group));
  custom_group->symbol = strdup(group_sym);
  custom_group->label = uim_custom_get_str(group_sym, "custom-group-label");
  custom_group->desc = uim_custom_get_str(group_sym, "custom-group-desc");

  return custom_group;
}

void
uim_custom_group_free(struct uim_custom_group *custom_group)
{
  free(custom_group->symbol);
  free(custom_group->label);
  free(custom_group->desc);
  free(custom_group);
}

/* NULL as group_sym means 'any group' */
char **
uim_custom_collect_by_group(const char *group_sym)
{
  char **custom_list;

  UIM_EVAL_FSTRING2(NULL, "(define %s (custom-collect-by-group '%s))",
		    str_list_arg, (group_sym) ? group_sym : "#f");
  custom_list = uim_scm_c_str_list(str_list_arg, "symbol->string");

  return custom_list;
}

char **
uim_custom_groups(void)
{
  char **group_list;

  UIM_EVAL_FSTRING1(NULL, "(define %s (custom-list-groups))", str_list_arg);
  group_list = uim_scm_c_str_list(str_list_arg, "symbol->string");

  return group_list;
}

char **
uim_custom_primary_groups(void)
{
  char **group_list;

  UIM_EVAL_FSTRING1(NULL, "(define %s (custom-list-primary-groups))",
		    str_list_arg);
  group_list = uim_scm_c_str_list(str_list_arg, "symbol->string");

  return group_list;
}

char **
uim_custom_group_subgroups(const char *group_sym)
{
  char **group_list;

  UIM_EVAL_FSTRING2(NULL, "(define %s (custom-group-subgroups '%s))",
		    str_list_arg, group_sym);
  group_list = uim_scm_c_str_list(str_list_arg, "symbol->string");

  return group_list;
}

void
uim_custom_symbol_list_free(char **symbol_list)
{
  uim_scm_c_list_free((void **)symbol_list, (uim_scm_c_list_free_func)free);
}

char *
uim_custom_set_cb(void (*update_cb)(const char *custom_sym))
{
  /* TODO */
  return NULL;
}
