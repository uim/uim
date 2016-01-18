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

/*
  static functions that have uim_custom_ prefix could be exported as API
  function if needed.  -- YamaKen 2004-12-30
*/

/*
  Don't insert NULL checks for free(3). free(3) accepts NULL as proper
  argument that causes no action.  -- YamaKen 2004-12-17
*/

#include <config.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "gettext.h"

#include "uim-scm.h"
#include "uim-custom.h"
#include "uim-internal.h"
#include "uim-helper.h"

#if 0
/*
 * The UIM_CUSTOM_EXPERIMENTAL_MTIME_SENSING is disabled since:
 *
 * - file_content_is_same() has a bug which may return invalid result
 *   when the file size is greater than 4095 bytes
 *
 * - The codes aim to save custom-groups that some changes are
 *   applied, but it should not be achieved by such violent method
 *   (comparing entire content of saved files). Observing updated
 *   group in uim-custom client program is recommended way
 *
 * - It breaks original behavior. See the comment of
 *   custom-reload-user-configs in custom-rt.scm
 *
 *  -- YamaKen 2005-09-12
 */
#define UIM_CUSTOM_EXPERIMENTAL_MTIME_SENSING
#endif

#define MAX_LENGTH_OF_INT_AS_STR (((sizeof(int) == 4) ? sizeof("-2147483648") : sizeof("-9223372036854775808")) - sizeof((char)'\0'))

#define UGETTEXT(str) (dgettext(GETTEXT_PACKAGE, (str)))

/* we cannot use the variadic macro (i.e. __VA_ARGS__) because we
   should also support C89 compilers
*/
#define UIM_EVAL_STRING_INTERNAL(uc, sexp_str) \
  (uim_scm_last_val = uim_scm_eval_c_string(sexp_str))

#define UIM_EVAL_STRING(uc, sexp_str) \
  { \
    UIM_EVAL_STRING_INTERNAL(uc, sexp_str); \
  }

#define UIM_EVAL_FSTRING1(uc, sexp_tmpl, arg1) \
  { \
    int form_size; \
    char *buf; \
    form_size = uim_sizeof_sexp_str(sexp_tmpl, arg1); \
    if (form_size != -1) { \
      uim_asprintf(&buf, sexp_tmpl, arg1); \
      UIM_EVAL_STRING_INTERNAL(uc, buf); \
      free(buf); \
    } \
  }

#define UIM_EVAL_FSTRING2(uc, sexp_tmpl, arg1, arg2) \
  { \
    int form_size; \
    char *buf; \
    form_size = uim_sizeof_sexp_str(sexp_tmpl, arg1, arg2); \
    if (form_size != -1) { \
      uim_asprintf(&buf, sexp_tmpl, arg1, arg2); \
      UIM_EVAL_STRING_INTERNAL(uc, buf); \
      free(buf); \
    } \
  }

#define UIM_EVAL_FSTRING3(uc, sexp_tmpl, arg1, arg2, arg3) \
  { \
    int form_size; \
    char *buf; \
    form_size = uim_sizeof_sexp_str(sexp_tmpl, arg1, arg2, arg3); \
    if (form_size != -1) { \
      uim_asprintf(&buf, sexp_tmpl, arg1, arg2, arg3); \
      UIM_EVAL_STRING_INTERNAL(uc, buf); \
      free(buf); \
    } \
  }

#define UIM_EVAL_FSTRING4(uc, sexp_tmpl, arg1, arg2, arg3, arg4) \
  { \
    int form_size; \
    char *buf; \
    form_size = uim_sizeof_sexp_str(sexp_tmpl, arg1, arg2, arg3, arg4); \
    if (form_size != -1) { \
      uim_asprintf(&buf, sexp_tmpl, arg1, arg2, arg3, arg4); \
      UIM_EVAL_STRING_INTERNAL(uc, buf); \
      free(buf); \
    } \
  }

#define UIM_EVAL_FSTRING5(uc, sexp_tmpl, arg1, arg2, arg3, arg4, arg5) \
  { \
    int form_size; \
    char *buf; \
    form_size = uim_sizeof_sexp_str(sexp_tmpl, arg1, arg2, arg3, arg4, arg5); \
    if (form_size != -1) { \
      uim_asprintf(&buf, sexp_tmpl, arg1, arg2, arg3, arg4, arg5); \
      UIM_EVAL_STRING_INTERNAL(uc, buf); \
      free(buf); \
    } \
  }

typedef void (*uim_custom_cb_update_cb_t)(void *ptr, const char *custom_sym);
typedef void (*uim_custom_global_cb_update_cb_t)(void *ptr);

typedef void *(*uim_scm_c_list_conv_func)(uim_lisp elem);
typedef void (*uim_scm_c_list_free_func)(void *elem);

/* exported for internal use */
uim_bool uim_custom_init(void);
uim_bool uim_custom_quit(void);

/* uim_scm_return_value() can only be used to retrieve result of
 * UIM_EVAL_FSTRINGn() or UIM_EVAL_STRING(). */
static uim_lisp uim_scm_return_value(void);
static int uim_sizeof_sexp_str(const char *tmpl, ...);

static void **uim_scm_c_list(const char *list_repl, const char *mapper_proc,
                             uim_scm_c_list_conv_func conv_func);
static char *uim_scm_c_str_failsafe(uim_lisp str);
static char **uim_scm_c_str_list(const char *list_repl,
                                 const char *mapper_proc);
static void uim_scm_c_list_free(void **list,
                                uim_scm_c_list_free_func free_func);

static char *literalize_string(const char *str);
static char *literalize_string_internal(const char *str);

static char *c_list_to_str(const void *const *list, char *(*mapper)(const void *elem), const char *sep);

static int uim_custom_type_eq(const char *custom_sym, const char *custom_type);
static int uim_custom_type(const char *custom_sym);
static int uim_custom_is_active(const char *custom_sym);
static const char *uim_custom_get_str(const char *custom_sym,
                                      const char *proc);
static char *uim_custom_label(const char *custom_sym);
static char *uim_custom_desc(const char *custom_sym);

static struct uim_custom_pathname *uim_custom_pathname_get(const char *custom_sym, const char *getter_proc);
static struct uim_custom_pathname *uim_custom_pathname_new(char *str, int type);
static void uim_custom_pathname_free(struct uim_custom_pathname *custom_pathname);
static struct uim_custom_choice *uim_custom_choice_get(const char *custom_sym, const char *choice_sym);
static char *extract_choice_symbol(const struct uim_custom_choice *custom_choice);
static char *choice_list_to_str(const struct uim_custom_choice *const *list, const char *sep);
static void uim_custom_choice_free(struct uim_custom_choice *custom_choice);
static struct uim_custom_choice **extract_choice_list(const char *list_repl, const char *custom_sym);
static struct uim_custom_choice **uim_custom_choice_item_list(const char *custom_sym);

static struct uim_custom_choice **uim_custom_olist_get(const char *custom_sym, const char *getter_proc);
static struct uim_custom_choice **uim_custom_olist_item_list(const char *custom_sym);

static struct uim_custom_key **uim_custom_key_get(const char *custom_sym, const char *getter_proc);
static void uim_custom_key_free(struct uim_custom_key *custom_key);
static char *extract_key_literal(const struct uim_custom_key *custom_key);
static char *key_list_to_str(const struct uim_custom_key *const *list, const char *sep);

static char ***uim_custom_table_get(const char *custom_sym, const char *getter_proc);
static char *literalized_strdup(const char *str);
static char *row_list_to_str(const char *const *list);
static char *table_to_str(const char** const *list, const char *sep);
static struct uim_custom_choice **uim_custom_table_header_item_list(const char *custom_sym);


static union uim_custom_value *uim_custom_value_internal(const char *custom_sym, const char *getter_proc);
static union uim_custom_value *uim_custom_value(const char *custom_sym);
static union uim_custom_value *uim_custom_default_value(const char *custom_sym);
static void uim_custom_value_free(int custom_type, union uim_custom_value *custom_value);
static uim_lisp uim_custom_range_elem(const char *custom_sym, const char *accessor_proc);
static union uim_custom_range *uim_custom_range_get(const char *custom_sym);
static void uim_custom_range_free(int custom_type, union uim_custom_range *custom_range);
static uim_lisp uim_custom_cb_update_cb_gate(uim_lisp cb, uim_lisp ptr, uim_lisp custom_sym);
static uim_lisp uim_custom_global_cb_update_cb_gate(uim_lisp cb, uim_lisp ptr);
static uim_bool custom_cb_remove(const char *key_sym, const char *hook);
static uim_bool custom_cb_add(const char *hook, const char *validator,
			      const char *custom_sym, void *ptr,
			      const char *gate_func, void (*cb)(void));
struct custom_cb_add_args {
  const char *hook;
  const char *validator;
  const char *custom_sym;
  void *ptr;
  const char *gate_func;
  void (*cb)(void);
};
static void *custom_cb_add_internal(struct custom_cb_add_args *args);

static void helper_disconnect_cb(void);
static char *uim_conf_path(const char *subpath);
static char *custom_file_path(const char *group, pid_t pid);
static uim_bool prepare_dir(const char *dir);
static uim_bool uim_conf_prepare_dir(const char *subdir);
static uim_bool for_each_primary_groups(uim_bool (*func)(const char *));
static uim_bool uim_custom_load_group(const char *group);
static uim_bool uim_custom_save_group(const char *group);
static const char *uim_custom_get_primary_group_by_custom(const char *custom_sym);

static const char str_list_arg[] = "uim-custom-c-str-list-arg";
static const char custom_subdir[] = "customs";
static const char custom_msg_tmpl[] = "prop_update_custom\n%s\n%s\n";
static int helper_fd = -1;
static uim_lisp return_val;
static uim_lisp uim_scm_last_val;


static uim_lisp
uim_scm_return_value(void)
{
  return uim_scm_last_val;
}

/** Calculate actual sexp string size from printf-style args.
 * This function calculates actual sexp string size from printf-style
 * args. Format string \a sexp_tmpl only accepts %d and %s.
 */
int
uim_sizeof_sexp_str(const char *sexp_tmpl, ...)
{
  va_list ap;
  int len, size;
  int tmp;
  const char *sexp_tmpl_end, *escp = sexp_tmpl, *strarg;
  char fmtchr;

  va_start(ap, sexp_tmpl);
  len = strlen(sexp_tmpl);
  sexp_tmpl_end = sexp_tmpl + len - 1;
  while ((escp = strchr(escp, '%'))) {
    if (escp < sexp_tmpl_end) {
      escp += sizeof((char)'%');
      fmtchr = *escp++;
      switch (fmtchr) {
      case 'd':
	tmp = va_arg(ap, int);
	len += MAX_LENGTH_OF_INT_AS_STR;
	break;
      case 's':
	strarg = va_arg(ap, const char *);
	len += strlen(strarg);
	break;
      default:
	/* unexpected format string */
	size = -1;
	goto end;
      }
    } else {
      /* invalid format string */
      size = -1;
      goto end;
    }
  }
  size = len + sizeof((char)'\0');

 end:
  va_end(ap);

  return size;
}

/*
  - list_repl must always returns same list for each evaluation
  - returns NULL terminated array. NULL will not appeared except terminator
  - non-string element such as #f is converted to ""
 */
static void **
uim_scm_c_list(const char *list_repl, const char *mapper_proc,
	       uim_scm_c_list_conv_func conv_func)
{
  int list_len, i;
  void **result;

  UIM_EVAL_FSTRING1(NULL, "(length %s)", list_repl);
  list_len = uim_scm_c_int(uim_scm_return_value());

  result = (void **)malloc(sizeof(void *) * (list_len + 1));
  if (!result)
    return NULL;

  result[list_len] = NULL;
  for (i = 0; i < list_len; i++) {
    UIM_EVAL_FSTRING3(NULL, "(%s (nth %d %s))", mapper_proc, i, list_repl);
    result[i] = (*conv_func)(uim_scm_return_value());
  }

  return result;
}

static char *
uim_scm_c_str_failsafe(uim_lisp str)
{
  return (uim_scm_truep(str)) ? uim_scm_c_str(str) : strdup("");
}

static char **
uim_scm_c_str_list(const char *list_repl, const char *mapper_proc)
{
  void **list;
  
  list = uim_scm_c_list(list_repl, mapper_proc,
			(uim_scm_c_list_conv_func)uim_scm_c_str_failsafe);

  return (char **)list;
}

static void
uim_scm_c_list_free(void **list, uim_scm_c_list_free_func free_func)
{
  void *elem;
  void **p;

  if (!list)
    return;

  for (p = list; *p; p++) {
    elem = *p;
    free_func(elem);
  }
  free(list);
}

static char *
literalize_string(const char *str)
{
  return uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)literalize_string_internal, (void *)str);
}

static char *
literalize_string_internal(const char *str)
{
  uim_lisp form;
  char *escaped;

  form = uim_scm_list2(uim_scm_make_symbol("string-escape"),
		       uim_scm_make_str(str));
  escaped = uim_scm_c_str(uim_scm_eval(form));

  return escaped;
}

static char *
c_list_to_str(const void *const *list, char *(*mapper)(const void *elem), const char *sep)
{
  size_t buf_size;
  char *buf, *bufp, *str;
  const void *const *elem;

  buf_size = sizeof(char);
  for (elem = list; *elem; elem++) {
    if (elem != list)
      buf_size += strlen(sep);
    str = (*mapper)(*elem);
    buf_size += strlen(str);
    free(str);
  }
  buf = (char *)malloc(buf_size);
  buf[0] = '\0';

  for (bufp = buf, elem = list; *elem; elem++) {
    if (elem != list) {
      strlcat(buf, sep, buf_size);
      bufp += strlen(sep);
    }
    str = (*mapper)(*elem);
    strlcat(buf, str, buf_size);
    bufp += strlen(str);
    free(str);
  }

  return buf;
}

static int
uim_custom_type_eq(const char *custom_sym, const char *custom_type)
{
  UIM_EVAL_FSTRING2(NULL, "(eq? (custom-type '%s) '%s)",
		    custom_sym, custom_type);

  return uim_scm_c_bool(uim_scm_return_value());
}

static int
uim_custom_type(const char *custom_sym)
{
  if (uim_custom_type_eq(custom_sym, "boolean")) {
    return UCustom_Bool;
  } else if (uim_custom_type_eq(custom_sym, "integer")) {
    return UCustom_Int;
  } else if (uim_custom_type_eq(custom_sym, "string")) {
    return UCustom_Str;
  } else if (uim_custom_type_eq(custom_sym, "pathname")) {
    return UCustom_Pathname;
  } else if (uim_custom_type_eq(custom_sym, "choice")) {
    return UCustom_Choice;
  } else if (uim_custom_type_eq(custom_sym, "ordered-list")) {
    return UCustom_OrderedList;
  } else if (uim_custom_type_eq(custom_sym, "key")) {
    return UCustom_Key;
  } else if (uim_custom_type_eq(custom_sym, "table")) {
    return UCustom_Table;
  } else {
    return UCustom_Bool;
  }
}

static int
uim_custom_is_active(const char *custom_sym)
{
  UIM_EVAL_FSTRING1(NULL, "(custom-active? '%s)", custom_sym);
  return_val = uim_scm_return_value();

  return uim_scm_c_bool(return_val);
}

static const char *
uim_custom_get_str(const char *custom_sym, const char *proc)
{
  UIM_EVAL_FSTRING2(NULL, "(%s '%s)", proc, custom_sym);
  return_val = uim_scm_return_value();

  return uim_scm_refer_c_str(return_val);
}

static char *
uim_custom_label(const char *custom_sym)
{
  const char *str;

  str = uim_custom_get_str(custom_sym, "custom-label");
  return strdup(UGETTEXT(str));
}

static char *
uim_custom_desc(const char *custom_sym)
{
  const char *str;

  str = uim_custom_get_str(custom_sym, "custom-desc");
  return strdup(UGETTEXT(str));
}

/* pathname */
static struct uim_custom_pathname *
uim_custom_pathname_get(const char *custom_sym, const char *getter_proc)
{
  struct uim_custom_pathname *custom_pathname;
  char *str, *type_sym;
  int type;

  UIM_EVAL_FSTRING2(NULL, "(%s '%s)", getter_proc, custom_sym);
  return_val = uim_scm_return_value();
  str = uim_scm_c_str(return_val);

  UIM_EVAL_FSTRING1(NULL, "(custom-pathname-type '%s)", custom_sym);
  return_val = uim_scm_return_value();
  type_sym = uim_scm_c_symbol(return_val);
  if (strcmp(type_sym, "directory") == 0)
    type = UCustomPathnameType_Directory;
  else
    type = UCustomPathnameType_RegularFile;
  free(type_sym);

  custom_pathname = uim_custom_pathname_new(str, type);
  if (!custom_pathname)
    return NULL;

  return custom_pathname;
}

static struct uim_custom_pathname *
uim_custom_pathname_new(char *str, int type)
{
  struct uim_custom_pathname *custom_pathname;

  custom_pathname = malloc(sizeof(struct uim_custom_pathname));
  if (!custom_pathname)
    return NULL;

  custom_pathname->str = str;
  custom_pathname->type = type;

  return custom_pathname;
}

static void
uim_custom_pathname_free(struct uim_custom_pathname *custom_pathname)
{
  if (!custom_pathname)
    return;

  free(custom_pathname->str);
}

/* choice */
static struct uim_custom_choice *
uim_custom_choice_get(const char *custom_sym, const char *choice_sym)
{
  struct uim_custom_choice *c_choice;

  c_choice = uim_custom_choice_new(NULL, NULL, NULL);
  if (!c_choice)
    return NULL;

  c_choice->symbol = strdup(choice_sym);

  UIM_EVAL_FSTRING2(NULL, "(custom-choice-label '%s '%s)",
		    custom_sym, choice_sym);
  return_val = uim_scm_return_value();
  c_choice->label = strdup(UGETTEXT(uim_scm_refer_c_str(return_val)));

  UIM_EVAL_FSTRING2(NULL, "(custom-choice-desc '%s '%s)",
		    custom_sym, choice_sym);
  return_val = uim_scm_return_value();
  c_choice->desc = strdup(UGETTEXT(uim_scm_refer_c_str(return_val)));

  return c_choice;
}

/**
 * TODO
 */
struct uim_custom_choice *
uim_custom_choice_new(char *symbol, char *label, char *desc)
{
  struct uim_custom_choice *custom_choice;

  custom_choice = (struct uim_custom_choice *)malloc(sizeof(struct uim_custom_choice));
  if (!custom_choice)
    return NULL;

  custom_choice->symbol = symbol;
  custom_choice->label = label;
  custom_choice->desc = desc;

  return custom_choice;
}

static void
uim_custom_choice_free(struct uim_custom_choice *custom_choice)
{
  if (!custom_choice)
    return;

  free(custom_choice->symbol);
  free(custom_choice->label);
  free(custom_choice->desc);
  free(custom_choice);
}

static struct uim_custom_choice **
extract_choice_list(const char *list_repl, const char *custom_sym)
{
  char *choice_sym, **choice_sym_list, **p;
  struct uim_custom_choice *custom_choice, **custom_choice_list;

  choice_sym_list =
    (char **)uim_scm_c_list(list_repl, "symbol->string",
			    (uim_scm_c_list_conv_func)uim_scm_c_str);
  if (!choice_sym_list)
    return NULL;

  for (p = choice_sym_list; *p; p++) {
    choice_sym = *p;
    custom_choice = uim_custom_choice_get(custom_sym, choice_sym);
    free(choice_sym); /* free the old contents */
    *p = (char *)custom_choice;  /* intentionally overwrite */
  }

  /* reuse the list structure */
  custom_choice_list = (struct uim_custom_choice **)choice_sym_list;

  return custom_choice_list;
}

static struct uim_custom_choice **
uim_custom_choice_item_list(const char *custom_sym)
{
  UIM_EVAL_FSTRING2(NULL, "(define %s (custom-range '%s))",
		    str_list_arg, custom_sym);
  return extract_choice_list(str_list_arg, custom_sym);
}

static char *
extract_choice_symbol(const struct uim_custom_choice *custom_choice)
{
  return strdup(custom_choice->symbol);
}

static char *
choice_list_to_str(const struct uim_custom_choice *const *list, const char *sep)
{
  return c_list_to_str((const void *const *)list,
		       (char *(*)(const void *))extract_choice_symbol, sep);
}

/**
 * TODO
 */
void
uim_custom_choice_list_free(struct uim_custom_choice **list)
{
  uim_scm_c_list_free((void **)list,
		      (uim_scm_c_list_free_func)uim_custom_choice_free);
}

/* ordered list */
static struct uim_custom_choice **
uim_custom_olist_get(const char *custom_sym, const char *getter_proc)
{
  UIM_EVAL_FSTRING3(NULL, "(define %s (%s '%s))",
		    str_list_arg, getter_proc, custom_sym);
  return extract_choice_list(str_list_arg, custom_sym);
}

static struct uim_custom_choice **
uim_custom_olist_item_list(const char *custom_sym)
{
  return uim_custom_choice_item_list(custom_sym);
}

/* key */
static struct uim_custom_key **
uim_custom_key_get(const char *custom_sym, const char *getter_proc)
{
  char **key_literal_list, **key_label_list, **key_desc_list;
  int *key_type_list, editor_type, list_len, i;
  struct uim_custom_key *custom_key, **custom_key_list;

  UIM_EVAL_FSTRING3(NULL, "(define %s ((if uim-custom-expand-key? custom-expand-key-references (lambda (l) l)) (%s '%s)))",
		    str_list_arg, getter_proc, custom_sym);
  key_literal_list =
    (char **)uim_scm_c_list(str_list_arg,
			    "(lambda (key) (if (symbol? key) (symbol->string key) key))",
			    (uim_scm_c_list_conv_func)uim_scm_c_str);
  key_type_list =
    (int *)uim_scm_c_list(str_list_arg,
			  "(lambda (key) (if (symbol? key) 1 0))",
			  (uim_scm_c_list_conv_func)uim_scm_c_int);
  key_label_list =
    (char **)uim_scm_c_list(str_list_arg,
			    "(lambda (key) (if (symbol? key) (custom-label key) #f))",
			    (uim_scm_c_list_conv_func)uim_scm_c_str_failsafe);
  key_desc_list =
    (char **)uim_scm_c_list(str_list_arg,
			    "(lambda (key) (if (symbol? key) (custom-desc key) #f))",
			    (uim_scm_c_list_conv_func)uim_scm_c_str_failsafe);
  if (!key_type_list || !key_literal_list || !key_label_list || !key_desc_list)
  {
    free(key_type_list);
    uim_custom_symbol_list_free(key_literal_list);
    uim_custom_symbol_list_free(key_label_list);
    uim_custom_symbol_list_free(key_desc_list);
    return NULL;
  }

  UIM_EVAL_FSTRING1(NULL, "(custom-key-advanced-editor? '%s)", custom_sym);
  return_val = uim_scm_return_value();
  editor_type = uim_scm_c_bool(return_val) ? UCustomKeyEditor_Advanced : UCustomKeyEditor_Basic;

  UIM_EVAL_FSTRING1(NULL, "(length %s)", str_list_arg);
  return_val = uim_scm_return_value();
  list_len = uim_scm_c_int(return_val);

  for (i = 0; i < list_len; i++) {
    char *literal, *label, *desc;
    int type;
    type = (key_type_list[i] == 1) ? UCustomKey_Reference : UCustomKey_Regular;
    literal = key_literal_list[i];
    label = key_label_list[i];
    desc = key_desc_list[i];
    custom_key = uim_custom_key_new(type, editor_type, literal, label, desc);
    key_literal_list[i] = (char *)custom_key;  /* intentionally overwrite */
  }
  /* reuse the list structure */
  custom_key_list = (struct uim_custom_key **)key_literal_list;

  /* ownership of elements had been transferred to custom_key_list */
  free(key_type_list);
  free(key_label_list);
  free(key_desc_list);

  return custom_key_list;
}

/**
 * TODO
 */
struct uim_custom_key *
uim_custom_key_new(int type, int editor_type,
		   char *literal, char *label, char *desc)
{
  struct uim_custom_key *custom_key;

  custom_key = (struct uim_custom_key *)malloc(sizeof(struct uim_custom_key));
  if (!custom_key)
    return NULL;

  custom_key->type = type;
  custom_key->editor_type = editor_type;
  custom_key->literal = literal;
  custom_key->label = label;
  custom_key->desc = desc;

  return custom_key;
}

static void
uim_custom_key_free(struct uim_custom_key *custom_key)
{
  if (!custom_key)
    return;

  free(custom_key->literal);
  free(custom_key->label);
  free(custom_key->desc);
  free(custom_key);
}

static char *
extract_key_literal(const struct uim_custom_key *custom_key)
{
  char *literal;

  switch (custom_key->type) {
  case UCustomKey_Regular:
    literal = literalize_string(custom_key->literal);
    break;
  case UCustomKey_Reference:
    literal = strdup(custom_key->literal);
    break;
  default:
    literal = strdup("\"\"");
  }

  return literal;
}

static char *
key_list_to_str(const struct uim_custom_key *const *list, const char *sep)
{
  return c_list_to_str((const void *const *)list,
		       (char *(*)(const void *))extract_key_literal, sep);
}

/**
 * TODO
 */
void
uim_custom_key_list_free(struct uim_custom_key **list)
{
  uim_scm_c_list_free((void **)list,
		      (uim_scm_c_list_free_func)uim_custom_key_free);
}

/* table */
static char ***
uim_custom_table_get(const char *custom_sym, const char *getter_proc)
{
  char ***custom_table;
  int row_count;
  int row;

  UIM_EVAL_FSTRING1(NULL, "(length %s)", custom_sym);
  row_count = uim_scm_c_int(uim_scm_return_value());

  custom_table = (char ***)malloc(sizeof(char **) * (row_count + 1));
  if (!custom_table)
    return NULL;

  custom_table[row_count] = NULL;
  for (row = 0; row < row_count; row++) {
    int column_count;
    int column;
    UIM_EVAL_FSTRING2(NULL, "(length (nth %d %s))", row, custom_sym);
    column_count = uim_scm_c_int(uim_scm_return_value());

    custom_table[row] = (char **)malloc(sizeof(char *) * (column_count + 1));
    if (!custom_table[row])
      return NULL;
    custom_table[row][column_count] = NULL;
    for (column = 0; column < column_count; column++) {
      char *str;
      UIM_EVAL_FSTRING3(NULL, "(nth %d (nth %d %s))", column, row, custom_sym);
      str = uim_scm_c_str(uim_scm_return_value());
      if (!str)
        return NULL;
      custom_table[row][column] = malloc(sizeof(char) * (strlen(str) + 1));
      if (!custom_table[row][column])
        return NULL;
      custom_table[row][column] = str;
    }
  }

  return custom_table;
}

static char *
literalized_strdup(const char *str)
{
  return strdup(literalize_string(str));
}

static char *
row_list_to_str(const char *const *list)
{
  return c_list_to_str((const void *const *)list,
                       (char *(*)(const void *))literalized_strdup, " ");
}

static char *
table_to_str(const char** const *list, const char *sep)
{
  return c_list_to_str((const void *const *)list,
		       (char *(*)(const void *))row_list_to_str, sep);
}

static struct uim_custom_choice **
uim_custom_table_header_item_list(const char *custom_sym)
{
  return uim_custom_choice_item_list(custom_sym);
}

static union uim_custom_value *
uim_custom_value_internal(const char *custom_sym, const char *getter_proc)
{
  int type;
  union uim_custom_value *value;
  char *custom_value_symbol;

  if (!custom_sym || !getter_proc)
    return NULL;

  value = (union uim_custom_value *)malloc(sizeof(union uim_custom_value));
  if (!value)
    return NULL;

  type = uim_custom_type(custom_sym);
  UIM_EVAL_FSTRING2(NULL, "(%s '%s)", getter_proc, custom_sym);
  return_val = uim_scm_return_value();
  switch (type) {
  case UCustom_Bool:
    value->as_bool = uim_scm_c_bool(return_val);
    break;
  case UCustom_Int:
    value->as_int = uim_scm_c_int(return_val);
    break;
  case UCustom_Str:
    value->as_str = uim_scm_c_str(return_val);
    break;
  case UCustom_Pathname:
    value->as_pathname = uim_custom_pathname_get(custom_sym, getter_proc);
    break;
  case UCustom_Choice:
    custom_value_symbol = uim_scm_c_symbol(return_val);
    value->as_choice = uim_custom_choice_get(custom_sym, custom_value_symbol);
    free(custom_value_symbol);
    break;
  case UCustom_OrderedList:
    value->as_olist = uim_custom_olist_get(custom_sym, getter_proc);
    break;
  case UCustom_Key:
    value->as_key = uim_custom_key_get(custom_sym, getter_proc);
    break;
  case UCustom_Table:
    value->as_table = uim_custom_table_get(custom_sym, getter_proc);
    break;
  default:
    free(value);
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
  if (!custom_value)
    return;

  switch (custom_type) {
  case UCustom_Str:
    free(custom_value->as_str);
    break;
  case UCustom_Pathname:
    uim_custom_pathname_free(custom_value->as_pathname);
    break;
  case UCustom_Choice:
    uim_custom_choice_free(custom_value->as_choice);
    break;
  case UCustom_OrderedList:
    uim_custom_choice_list_free(custom_value->as_olist);
    break;
  case UCustom_Key:
    uim_custom_key_list_free(custom_value->as_key);
    break;
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
  if (!range)
    return NULL;

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
  case UCustom_OrderedList:
    range->as_olist.valid_items = uim_custom_olist_item_list(custom_sym);
    break;
  case UCustom_Table:
    range->as_table_header.valid_items
        = uim_custom_table_header_item_list(custom_sym);
    break;
  }

  return range;
}

static void
uim_custom_range_free(int custom_type, union uim_custom_range *custom_range)
{
  if (!custom_range)
    return;

  switch (custom_type) {
  case UCustom_Str:
    free(custom_range->as_str.regex);
    break;
  case UCustom_Choice:
    uim_custom_choice_list_free(custom_range->as_choice.valid_items);
    break;
  case UCustom_OrderedList:
    uim_custom_choice_list_free(custom_range->as_olist.valid_items);
    break;
  }
  free(custom_range);
}

static void
helper_disconnect_cb(void)
{
  helper_fd = -1;
}

/**
 * Enables use of custom API. This function must be called before
 * uim_custom_*() functions are called. uim_init() must be called before this
 * function.
 *
 * @see uim_init()
 * @retval UIM_TRUE succeeded
 * @retval UIM_FALSE failed
 */
uim_bool
uim_custom_enable(void)
{
  UIM_EVAL_STRING(NULL, "(require-dynlib \"custom-enabler\")");
  return uim_scm_c_bool(uim_scm_return_value());
}

/*
 * This function is exported as internal use. Intentionally disdocumented.
 *
 * Initializes custom API. This function must be called before uim_custom_*()
 * functions are called. uim_init() must be called before this function.
 *
 * @see uim_init()
 * @retval UIM_TRUE succeeded
 * @retval UIM_FALSE failed
 */
uim_bool
uim_custom_init(void)
{
  const char *client_codeset;

  return_val = uim_scm_f();
  uim_scm_last_val = uim_scm_f();
  uim_scm_gc_protect(&return_val);
  uim_scm_gc_protect(&uim_scm_last_val);

  uim_scm_init_proc3("custom-update-cb-gate", uim_custom_cb_update_cb_gate);
  uim_scm_init_proc2("custom-global-update-cb-gate",
		     uim_custom_global_cb_update_cb_gate);

  uim_scm_require_file("custom.scm");

  /* temporary solution to control key definition expantion */
  UIM_EVAL_STRING(NULL, "(define uim-custom-expand-key? #t)");

  /* Assumes that bind_textdomain_codeset() is already called in client
   * program. */
  client_codeset = bind_textdomain_codeset(textdomain(NULL), NULL);
  bind_textdomain_codeset(GETTEXT_PACKAGE, client_codeset);

  return UIM_TRUE;
}

/*
 * This function is exported as internal use. Intentionally disdocumented.
 *
 * Finalizes custom API. This function must be called before uim_quit().
 *
 * @see uim_quit()
 * @retval UIM_TRUE succeeded
 * @retval UIM_FALSE failed
 */
uim_bool
uim_custom_quit(void)
{
  uim_custom_cb_remove(NULL);
  uim_custom_group_cb_remove(NULL);
  uim_custom_global_cb_remove();

  return UIM_TRUE;
}

static char *
uim_conf_path(const char *subpath)
{
  char *dir;

  UIM_EVAL_STRING(NULL, "(string-append (or (home-directory (user-name)) \"\") \"/.uim.d\")");
  dir = uim_scm_c_str(uim_scm_return_value());
  if (subpath) {
    UIM_EVAL_FSTRING2(NULL, "\"%s/%s\"", dir, subpath);
    free(dir);
    dir = uim_scm_c_str(uim_scm_return_value());
  }

  return dir;
}

static char *
custom_file_path(const char *group, pid_t pid)
{
  char *custom_dir, *file_path;

  custom_dir = uim_conf_path(custom_subdir);
  if (pid) {
    UIM_EVAL_FSTRING3(NULL, "\"%s/.custom-%s.scm.%d\"", custom_dir, group, (int)pid);
  } else {
    UIM_EVAL_FSTRING2(NULL, "\"%s/custom-%s.scm\"", custom_dir, group);
  }
  file_path = uim_scm_c_str(uim_scm_return_value());
  free(custom_dir);

  return file_path;
}

static uim_bool
prepare_dir(const char *dir)
{
  struct stat st;

  if (stat(dir, &st) < 0) {
    return (mkdir(dir, 0700) < 0) ? UIM_FALSE : UIM_TRUE;
  } else {
    mode_t mode = S_IFDIR | S_IRUSR | S_IWUSR | S_IXUSR;

    return ((st.st_mode & mode) == mode) ? UIM_TRUE : UIM_FALSE;
  }
}

static uim_bool
uim_conf_prepare_dir(const char *subdir)
{
  uim_bool succeeded;
  char *dir;

  dir = uim_conf_path(NULL);
  succeeded = prepare_dir(dir);
  free(dir);
  if (!succeeded)
    return UIM_FALSE;

  if (subdir) {
    dir = uim_conf_path(subdir);
    succeeded = prepare_dir(dir);
    free(dir);
    if (!succeeded)
      return UIM_FALSE;
  }

  return UIM_TRUE;
}

static uim_bool
for_each_primary_groups(uim_bool (*func)(const char *))
{
  uim_bool succeeded = UIM_TRUE;
  char **primary_groups, **grp;

  primary_groups = uim_custom_primary_groups();
  for (grp = primary_groups; *grp; grp++) {
    succeeded = (*func)(*grp) && succeeded;
  }
  uim_custom_symbol_list_free(primary_groups);

  return succeeded;
}

static uim_bool
uim_custom_load_group(const char *group)
{
  char *file_path;
  uim_bool succeeded;

  file_path = custom_file_path(group, 0);
  succeeded = uim_scm_load_file(file_path);
  free(file_path);

  return succeeded;
}

/**
 * Loads per-user custom variable configurations. This function loads per-user
 * custom variable values from ~/.uim.d/customs/custom-*.scm previously saved
 * by uim_custom_save().
 *
 * @see uim_custom_save()
 * @retval UIM_TRUE succeeded
 * @retval UIM_FALSE failed
 */
uim_bool
uim_custom_load(void)
{
  if(uim_helper_is_setugid() ==UIM_FALSE) {
    return for_each_primary_groups(uim_custom_load_group);
  } else {
    return UIM_FALSE;
  }
}

#ifdef UIM_CUSTOM_EXPERIMENTAL_MTIME_SENSING
static uim_bool
file_content_is_same(const char *a_path, const char *b_path)
{
  uim_bool ret;
  FILE *a, *b;
  char a_buf[4096], b_buf[4096];

  a = fopen(a_path, "r");
  b = fopen(b_path, "r");

  while(1) {
    char *a_eof, *b_eof;

    if (!a || !b) {
      ret = UIM_FALSE;
      break;
    }

    a_eof = fgets(a_buf, sizeof(a_buf), a);
    b_eof = fgets(b_buf, sizeof(b_buf), b);

    if (!a_eof && !b_eof) {
      ret = UIM_TRUE;
      break;
    }

    if ((!a_eof && b_eof) || (a_eof && !b_eof)) {
      ret = UIM_FALSE;
      break;
    }

    if (strcmp(a_buf, b_buf) != 0) {
      ret = UIM_FALSE;
      break;
    }
  }

  if (a)
    fclose(a);
  if (b)
    fclose(b);
  return ret;
}
#endif

static uim_bool
uim_custom_save_group(const char *group)
{
  uim_bool succeeded = UIM_FALSE;
  char **custom_syms, **sym;
  char *def_literal;
  pid_t pid;
  char *tmp_file_path, *file_path;
  FILE *file;

  if (!uim_conf_prepare_dir(custom_subdir))
    return UIM_FALSE;

  /*
    to avoid write conflict and broken by accident, we write customs
    to temporary file first
  */
  pid = getpid();
  tmp_file_path = custom_file_path(group, pid);
  file = fopen(tmp_file_path, "w");
  if (!file)
    goto error;

  custom_syms = uim_custom_collect_by_group(group);
  if (!custom_syms) {
    fclose(file);
    goto error;
  }

  for (sym = custom_syms; *sym; sym++) {
    def_literal = uim_custom_definition_as_literal(*sym);
    if (def_literal) {
      fputs(def_literal, file);
      fprintf(file, "\n");
      free(def_literal);
    }
  }
  uim_custom_symbol_list_free(custom_syms);

  if (fclose(file) < 0)
    goto error;

  /* rename prepared temporary file to proper name */
  file_path = custom_file_path(group, 0);
#ifdef UIM_CUSTOM_EXPERIMENTAL_MTIME_SENSING
  /*
   * Avoiding a file saving at here by such method is a layer
   * violation. Observing updated group is recommended way.
   *   -- YamaKen 2005-08-09
   */
  if (file_content_is_same(tmp_file_path, file_path)) {
    succeeded = UIM_TRUE;
    remove(tmp_file_path);
  } else {
    succeeded = (rename(tmp_file_path, file_path) == 0);
  }
#else
  succeeded = (rename(tmp_file_path, file_path) == 0);
#endif
  free(file_path);

 error:
  free(tmp_file_path);

  return succeeded;
}

/**
 * Saves per-user custom variable configurations. This function saves current
 * custom variable values into ~/.uim.d/customs/custom-*.scm. The directory
 * will be made if not exist. The saved values will be implicitly loaded at
 * uim_init() or can explicitly be loaded by uim_custom_load().
 *
 * @see uim_init()
 * @see uim_custom_load()
 * @retval UIM_TRUE succeeded
 * @retval UIM_FALSE failed
 */
uim_bool
uim_custom_save(void)
{
  if(uim_helper_is_setugid() == UIM_FALSE) {
    return for_each_primary_groups(uim_custom_save_group);
  } else {
    return UIM_FALSE;
  }
}

/**
 * Saves a per-user custom variable configuration. This function saves a
 * primary custom group values which contains the specified custom variable as
 * ~/.uim.d/customs/custom-primary-group-file-containing-the-variable.scm. The
 * directory will be made if not exist. The saved values will be implicitly
 * loaded at uim_init() or can explicitly be loaded by uim_custom_load().
 *
 * @see uim_init()
 * @see uim_custom_load()
 * @retval UIM_TRUE succeeded
 * @retval UIM_FALSE failed
 */
uim_bool
uim_custom_save_custom(const char *custom_sym)
{
  if(uim_helper_is_setugid() == UIM_FALSE) {
    const char *group_sym = uim_custom_get_primary_group_by_custom(custom_sym);
    return uim_custom_save_group(group_sym);
  } else {
    return UIM_FALSE;
  }
}

/**
 * Broadcasts custom variable configurations to other uim-enabled application
 * processes via uim-helper-server. This function broadcasts current custom
 * variable values to other uim-enabled application processes via
 * uim-helper-server. The received processes updates custom variables
 * dynamically. This enables dynamic re-configuration of input methods.
 *
 * @retval UIM_TRUE succeeded
 * @retval UIM_FALSE failed
 */
uim_bool
uim_custom_broadcast(void)
{
  char **custom_syms, **sym;
  char *value, *msg;

  if (helper_fd < 0) {
    helper_fd = uim_helper_init_client_fd(helper_disconnect_cb);
  }

  custom_syms = uim_custom_collect_by_group(NULL);
  for (sym = custom_syms; *sym; sym++) {
    value = uim_custom_value_as_literal(*sym);
    if (value) {
#if 1
      uim_asprintf(&msg, custom_msg_tmpl, *sym, value);
#else
      /* old behavior: useless since other memory exhaustions disable uim */
      if (asprintf(&msg, custom_msg_tmpl, *sym, value) < 0 || !msg) {
	free(msg);
	free(value);
	uim_custom_symbol_list_free(custom_syms);
	return UIM_FALSE;
      }
#endif
      uim_helper_send_message(helper_fd, msg);
      free(msg);
      free(value);
    }
  }
  uim_custom_symbol_list_free(custom_syms);

  if (helper_fd != -1) {
    uim_helper_close_client_fd(helper_fd);
  }

  return UIM_TRUE;
}

/**
 * Broadcasts a request to reload custom files to other uim-enabled
 * application processes via uim-helper-server.
 *
 * @retval UIM_TRUE succeeded
 * @retval UIM_FALSE failed
 */
uim_bool
uim_custom_broadcast_reload_request(void)
{
  if (helper_fd < 0) {
    helper_fd = uim_helper_init_client_fd(helper_disconnect_cb);
  }

  uim_helper_send_message(helper_fd, "custom_reload_notify\n");

  if (helper_fd != -1) {
    uim_helper_close_client_fd(helper_fd);
  }

  return UIM_TRUE;
}

/**
 * Returns attributes and current value of a custom variable. Returned value
 * must be freed by uim_custom_free().
 *
 * @see uim_custom_free()
 * @return custom variable attributes and current value
 * @param custom_sym custom variable name
 */
struct uim_custom *
uim_custom_get(const char *custom_sym)
{
  struct uim_custom *custom;

  if (!custom_sym)
    return UIM_FALSE;

  custom = (struct uim_custom *)malloc(sizeof(struct uim_custom));
  if (!custom)
    return UIM_FALSE;

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

/**
 * Updates value of a custom variable. This function tries that an update of
 * the custom variable specified by symbol and value of contained in @a
 * custom. Update failes when passed value is invalid for the custom
 * variable. Previous value is kept in real custom variable when the
 * failure. @a custom should be created by uim_custom_get() and then user of
 * uim-custom API can modify value of the @custom before passing to this
 * function.
 *
 * If you want to set null list as value for ordered-list or key,
 * allocate an array contains 1 NULL element and set it into
 * custom->value->as_foo.
 *
 * @see uim_custom_get()
 * @param custom custom variable symbol and value
 * @retval UIM_TRUE succeeded
 * @retval UIM_FALSE failed
 */
uim_bool
uim_custom_set(const struct uim_custom *custom)
{
  char *literal;

  if (!custom)
    return UIM_FALSE;

  switch (custom->type) {
  case UCustom_Bool:
    UIM_EVAL_FSTRING2(NULL, "(custom-set-value! '%s #%s)",
		      custom->symbol, (custom->value->as_bool) ? "t" : "f");
    break;
  case UCustom_Int:
    UIM_EVAL_FSTRING2(NULL, "(custom-set-value! '%s %d)",
		      custom->symbol, custom->value->as_int);
    break;
  case UCustom_Str:
    literal = literalize_string(custom->value->as_str);
    UIM_EVAL_FSTRING2(NULL, "(custom-set-value! '%s %s)",
		      custom->symbol, literal);
    free(literal);
    break;
  case UCustom_Pathname:
    literal = literalize_string(custom->value->as_pathname->str);
    UIM_EVAL_FSTRING2(NULL, "(custom-set-value! '%s %s)",
		      custom->symbol, literal);
    free(literal);
    break;
  case UCustom_Choice:
    UIM_EVAL_FSTRING2(NULL, "(custom-set-value! '%s '%s)",
		      custom->symbol, custom->value->as_choice->symbol);
    break;
  case UCustom_OrderedList:
    {
      char *val;
      val = choice_list_to_str((const struct uim_custom_choice *const *)custom->value->as_olist, " ");
      UIM_EVAL_FSTRING2(NULL, "(custom-set-value! '%s '(%s))", custom->symbol, val);
      free(val);
    }
    break;
  case UCustom_Key:
    {
      char *val;
      val = key_list_to_str((const struct uim_custom_key *const *)custom->value->as_key, " ");
      UIM_EVAL_FSTRING2(NULL, "(custom-set-value! '%s (map gui-key-str->key-str '(%s)))", custom->symbol, val);
      free(val);
    }
    break;
  case UCustom_Table:
    {
      char *val
        = table_to_str((const char ** const *)custom->value->as_table, ") (");
      UIM_EVAL_FSTRING2(NULL, "(custom-set-value! '%s '((%s)))",
        custom->symbol, val);
      free(val);
    }
    break;
  default:
    return UIM_FALSE;
  }
  return uim_scm_c_bool(uim_scm_return_value());
}

/**
 * Frees pre-allocated C representation of a custom variable. All C
 * representation of a custom variable allocated by uim_custom_get() must be
 * freed by this function.
 *
 * @see uim_custom_get()
 * @param custom C representation of a custom variable
 */
void
uim_custom_free(struct uim_custom *custom)
{
  if (!custom)
    return;

  free(custom->symbol);
  free(custom->label);
  free(custom->desc);
  uim_custom_value_free(custom->type, custom->value);
  uim_custom_value_free(custom->type, custom->default_value);
  uim_custom_range_free(custom->type, custom->range);
  free(custom);
}

/**
 * Returns Scheme literal of a custom variable value. Returned string must be
 * free() by caller.
 *
 * @return the literal
 * @param custom_sym custom variable name
 */
char *
uim_custom_value_as_literal(const char *custom_sym)
{
  return strdup(uim_custom_get_str(custom_sym, "custom-value-as-literal"));
}

/**
 * Returns Scheme literal of a custom variable definition. Returned string
 * must be free() by caller.
 *
 * @return the literal
 * @param custom_sym custom variable name
 */
char *
uim_custom_definition_as_literal(const char *custom_sym)
{
  return strdup(uim_custom_get_str(custom_sym, "custom-definition-as-literal"));
}

/**
 * Returns attributes of a custom group.and current value of a custom
 * variable. Returned value must be freed by uim_custom_group_free().
 *
 * @see uim_custom_group_free()
 * @return attributes of custom group
 * @param group_sym custom group name
 */
struct uim_custom_group *
uim_custom_group_get(const char *group_sym)
{
  struct uim_custom_group *custom_group;
  const char *label, *desc;

  custom_group = (struct uim_custom_group *)malloc(sizeof(struct uim_custom_group));
  if (!custom_group)
    return NULL;

  label = uim_custom_get_str(group_sym, "custom-group-label");
  desc = uim_custom_get_str(group_sym, "custom-group-desc");

  custom_group->symbol = strdup(group_sym);
  custom_group->label = strdup(UGETTEXT(label));
  custom_group->desc = strdup(UGETTEXT(desc));

  return custom_group;
}

/**
 * Frees C representation of a custom group.
 *
 * @see uim_custom_group_get()
 * @param custom_group C representation of custom group
 */
void
uim_custom_group_free(struct uim_custom_group *custom_group)
{
  if (!custom_group)
    return;

  free(custom_group->symbol);
  free(custom_group->label);
  free(custom_group->desc);
  free(custom_group);
}

/**
 * Returns custom variable symbols that belongs to @a group_sym. The symbols
 * consist of NULL-terminated array of C string and must be freed by
 * uim_custom_symbol_list_free().
 *
 * @see uim_custom_symbol_list_free()
 * @return custom variable symbols
 * @param group_sym custom group name. NULL means 'any group'
 */
char **
uim_custom_collect_by_group(const char *group_sym)
{
  char **custom_list;

  UIM_EVAL_FSTRING2(NULL, "(define %s (custom-collect-by-group '%s))",
		    str_list_arg, (group_sym) ? group_sym : "#f");
  custom_list = uim_scm_c_str_list(str_list_arg, "symbol->string");

  return custom_list;
}

/**
 * Returns all existing custom group symbols. The symbols consist of
 * NULL-terminated array of C string and must be freed by
 * uim_custom_symbol_list_free().
 *
 * @see uim_custom_symbol_list_free()
 * @return custom variable symbols
 */
char **
uim_custom_groups(void)
{
  char **group_list;

  UIM_EVAL_FSTRING1(NULL, "(define %s (custom-list-groups))", str_list_arg);
  group_list = uim_scm_c_str_list(str_list_arg, "symbol->string");

  return group_list;
}

/**
 * Returns all existing primary custom group symbols. Subgroups are not
 * returned. The symbols consist of NULL-terminated array of C string and must
 * be freed by uim_custom_symbol_list_free().
 *
 * @see uim_custom_symbol_list_free()
 * @return custom variable symbols
 */
char **
uim_custom_primary_groups(void)
{
  char **group_list;

  UIM_EVAL_FSTRING1(NULL, "(define %s (custom-list-primary-groups))",
		    str_list_arg);
  group_list = uim_scm_c_str_list(str_list_arg, "symbol->string");

  return group_list;
}

/**
 * Returns subgroup symbols of @a group_sym. The symbols consist of
 * NULL-terminated array of C string and must be freed by
 * uim_custom_symbol_list_free().
 *
 * @see uim_custom_symbol_list_free()
 * @return custom subgroup symbols
 * @param group_sym custom group name
 */
char **
uim_custom_group_subgroups(const char *group_sym)
{
  char **group_list;

  UIM_EVAL_FSTRING2(NULL, "(define %s (custom-group-subgroups '%s))",
		    str_list_arg, group_sym);
  group_list = uim_scm_c_str_list(str_list_arg, "symbol->string");

  return group_list;
}

/**
 * Frees a symbol list allocated by uim_custom_*() functions. The term 'list'
 * does not mean linked list. Actually NULL-terminated array.
 *
 * @see uim_custom_collect_by_group()
 * @see uim_custom_groups()
 * @see uim_custom_primary_groups()
 * @see uim_custom_group_subgroups()
 * @param symbol_list pre-allocated symbol list
 */
void
uim_custom_symbol_list_free(char **symbol_list)
{
  uim_scm_c_list_free((void **)symbol_list, (uim_scm_c_list_free_func)free);
}

static const char *
uim_custom_get_primary_group_by_custom(const char *custom_sym)
{
  uim_lisp groups;
  groups = uim_scm_callf("custom-groups", "y", custom_sym);

  return uim_scm_refer_c_str(uim_scm_car(groups));
}

static uim_lisp
uim_custom_cb_update_cb_gate(uim_lisp cb, uim_lisp ptr, uim_lisp custom_sym)
{
  uim_custom_cb_update_cb_t update_cb;
  void *c_ptr;
  char *c_custom_sym;

  update_cb = (uim_custom_cb_update_cb_t)uim_scm_c_func_ptr(cb);
  c_ptr = uim_scm_c_ptr(ptr);
  c_custom_sym = uim_scm_c_symbol(custom_sym);
  (*update_cb)(c_ptr, c_custom_sym);
  free(c_custom_sym);

  return uim_scm_f();
}

static uim_lisp
uim_custom_global_cb_update_cb_gate(uim_lisp cb, uim_lisp ptr)
{
  uim_custom_global_cb_update_cb_t update_cb;
  void *c_ptr;

  update_cb = (uim_custom_global_cb_update_cb_t)uim_scm_c_func_ptr(cb);
  c_ptr = uim_scm_c_ptr(ptr);
  (*update_cb)(c_ptr);

  return uim_scm_f();
}

static uim_bool
custom_cb_add(const char *hook, const char *validator,
	      const char *custom_sym, void *ptr,
	      const char *gate_func, void (*cb)(void))
{
  struct custom_cb_add_args args;

  args.hook = hook;
  args.validator = validator;
  args.custom_sym = custom_sym;
  args.ptr = ptr;
  args.gate_func = gate_func;
  args.cb = cb;
  return (uim_bool)(uintptr_t)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)custom_cb_add_internal, &args);
}

static void *
custom_cb_add_internal(struct custom_cb_add_args *args)
{
  uim_bool succeeded;
  const char *hook;
  const char *validator;
  const char *custom_sym;
  void *ptr;
  const char *gate_func;
  void (*cb)(void);
  uim_lisp form;

  hook = args->hook;
  validator = args->validator;
  custom_sym = args->custom_sym;
  ptr = args->ptr;
  gate_func = args->gate_func;
  cb = args->cb;

  form = uim_scm_list5(uim_scm_make_symbol(validator),
		       uim_scm_quote(uim_scm_make_symbol(custom_sym)),
		       uim_scm_make_ptr(ptr),
		       uim_scm_make_symbol(gate_func),
		       uim_scm_make_func_ptr(cb));
  form = uim_scm_cons(uim_scm_quote(uim_scm_make_symbol(hook)), form);
  form = uim_scm_cons(uim_scm_make_symbol("custom-register-cb"), form);
  succeeded = uim_scm_c_bool(uim_scm_eval(form));

  return (void *)(uintptr_t)succeeded;
}

static uim_bool
custom_cb_remove(const char *key_sym, const char *hook)
{
  uim_bool removed;

  UIM_EVAL_FSTRING2(NULL, "(custom-remove-hook '%s '%s)",
		    (key_sym) ? key_sym : "#f", hook);
  removed = uim_scm_c_bool(uim_scm_return_value());

  return removed;
}

/**
 * Set a callback function in a custom variable. The @a update_cb is called
 * back when the custom variable specified by @a custom_sym is
 * updated. Multiple callbacks for one custom variable is allowed.
 *
 * @retval UIM_TRUE succeeded
 * @retval UIM_FALSE failed
 * @param custom_sym custom variable name
 * @param ptr an opaque value passed back to client at callback
 * @param update_cb function pointer called back when the custom variable is
 *        updated
 */
uim_bool
uim_custom_cb_add(const char *custom_sym, void *ptr,
		  void (*update_cb)(void *ptr, const char *custom_sym))
{
  return custom_cb_add("custom-update-hooks", "custom-rec",
		       custom_sym, ptr,
		       "custom-update-cb-gate", (void (*)(void))update_cb);
}

/**
 * Remove the callback functions in a custom variable. All functions set for @a
 * custom_sym will be removed.
 *
 * @retval UIM_TRUE some functions are removed
 * @retval UIM_FALSE no functions are removed
 * @param custom_sym custom variable name. NULL instructs 'all callbacks'
 */
uim_bool
uim_custom_cb_remove(const char *custom_sym)
{
  return custom_cb_remove(custom_sym, "custom-update-hooks");
}

/**
 * Set a callback function in a custom group. The @a update_cb is
 * called back when the custom group specified by @a group_sym is
 * updated (i.e. new custom variable has been defined in the group or
 * a custom variable is removed from the group). Multiple callbacks
 * for one custom variable is allowed.
 *
 * @retval UIM_TRUE succeeded
 * @retval UIM_FALSE failed
 * @param group_sym custom group name
 * @param ptr an opaque value passed back to client at callback
 * @param update_cb function pointer called back when the custom group is
 *        updated
 */
uim_bool
uim_custom_group_cb_add(const char *group_sym, void *ptr,
			void (*update_cb)(void *ptr, const char *group_sym))
{
  return custom_cb_add("custom-group-update-hooks", "custom-group-rec",
		       group_sym, ptr,
		       "custom-update-cb-gate", (void (*)(void))update_cb);
}

/**
 * Remove the callback functions in a custom group. All functions set for @a
 * group_sym will be removed.
 *
 * @retval UIM_TRUE some functions are removed
 * @retval UIM_FALSE no functions are removed
 * @param group_sym custom group name. NULL instructs 'all callbacks'
 */
uim_bool
uim_custom_group_cb_remove(const char *group_sym)
{
  return custom_cb_remove(group_sym, "custom-group-update-hooks");
}

/**
 * Set a callback function for global events. The @a
 * group_list_update_cb is called back when group list is updated
 * (i.e. new custom group has been defined or a custom group is
 * undefined). Multiple callbacks is allowed.
 *
 * @retval UIM_TRUE succeeded
 * @retval UIM_FALSE failed
 * @param ptr an opaque value passed back to client at callback
 * @param group_list_update_cb function pointer called back when
 *        custom group list is updated
 */
uim_bool
uim_custom_global_cb_add(void *ptr, void (*group_list_update_cb)(void *ptr))
{
  return custom_cb_add("custom-group-update-hooks", "(lambda (dummy) #t)",
		       "global", ptr,
		       "custom-global-update-cb-gate",
		       (void (*)(void))group_list_update_cb);
}

/**
 * Remove all callback functions for global events.
 *
 * @retval UIM_TRUE some functions are removed
 * @retval UIM_FALSE no functions are removed
 */
uim_bool uim_custom_global_cb_remove(void)
{
  return custom_cb_remove("global", "custom-group-list-update-hooks");
}
