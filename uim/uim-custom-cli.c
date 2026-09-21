/*
  Copyright (c) 2026 uim Project https://github.com/uim/uim

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "uim.h"
#include "uim-custom.h"
#include "uim-helper.h"

static void
usage(FILE *stream)
{
  fprintf(stream,
          "Usage: uim-custom-cli CUSTOM_NAME CUSTOM_VALUE\n"
          "       uim-custom-cli CUSTOM_NAME VALUE [VALUE ...]\n"
          "       uim-custom-cli [-s|--save] CUSTOM_NAME CUSTOM_VALUE\n"
          "       uim-custom-cli [-s|--save] CUSTOM_NAME VALUE [VALUE ...]\n"
          "       uim-custom-cli [-l|--list]\n"
          "       uim-custom-cli [-h|--help]\n"
          "\n"
          "CUSTOM_NAME is a custom variable name.\n"
          "CUSTOM_VALUE is a valid Scheme expression.\n"
          "Multiple values are supported for ordered-list customs.\n"
          "--save also saves the value for future uim processes.\n");
}

static const char *
custom_type_name(int type)
{
  switch (type) {
  case UCustom_Bool:
    return "boolean";
  case UCustom_Int:
    return "integer";
  case UCustom_Str:
    return "string";
  case UCustom_Pathname:
    return "pathname";
  case UCustom_Choice:
    return "choice";
  case UCustom_OrderedList:
    return "ordered-list";
  case UCustom_Key:
    return "key";
  case UCustom_Table:
    return "table";
  default:
    return "unknown";
  }
}

struct custom_category_subgroup {
  struct uim_custom_group *group;
  char **custom_symbols;
};

struct custom_category {
  struct uim_custom_group *group;
  char **custom_symbols;
  struct custom_category_subgroup *subgroups;
  size_t nr_subgroups;
};

struct custom_categories {
  struct custom_category *items;
  size_t nr_items;
};

static void
custom_categories_free(struct custom_categories *categories)
{
  size_t i;
  size_t j;

  if (!categories)
    return;

  for (i = 0; i < categories->nr_items; i++) {
    struct custom_category *category = &categories->items[i];

    uim_custom_group_free(category->group);
    uim_custom_symbol_list_free(category->custom_symbols);
    for (j = 0; j < category->nr_subgroups; j++) {
      uim_custom_group_free(category->subgroups[j].group);
      uim_custom_symbol_list_free(category->subgroups[j].custom_symbols);
    }
    free(category->subgroups);
  }
  free(categories->items);
  categories->items = NULL;
  categories->nr_items = 0;
}

static int
collect_custom_categories(struct custom_categories *categories)
{
  char **primary_groups = NULL;
  size_t i;

  if (!categories)
    return 0;

  categories->items = NULL;
  categories->nr_items = 0;

  primary_groups = uim_custom_primary_groups();
  if (!primary_groups) {
    fprintf(stderr, "uim-custom-cli: cannot collect primary groups\n");
    return 0;
  }

  for (i = 0; primary_groups[i]; i++)
    ;
  categories->nr_items = i;
  if (categories->nr_items > 0) {
    categories->items = calloc(categories->nr_items,
                               sizeof(*categories->items));
    if (!categories->items) {
      fprintf(stderr, "uim-custom-cli: cannot allocate categories\n");
      uim_custom_symbol_list_free(primary_groups);
      categories->nr_items = 0;
      return 0;
    }
  }

  for (i = 0; i < categories->nr_items; i++) {
    struct custom_category *category = &categories->items[i];
    char **subgroup_symbols;
    size_t nr_subgroups;
    size_t j;

    category->group = uim_custom_group_get(primary_groups[i]);
    category->custom_symbols = uim_custom_collect_by_group(primary_groups[i]);
    subgroup_symbols = uim_custom_group_subgroups(primary_groups[i]);
    if (!category->group || !category->custom_symbols || !subgroup_symbols) {
      fprintf(stderr, "uim-custom-cli: cannot collect category: %s\n",
              primary_groups[i]);
      uim_custom_symbol_list_free(subgroup_symbols);
      uim_custom_symbol_list_free(primary_groups);
      custom_categories_free(categories);
      return 0;
    }

    for (j = 0; subgroup_symbols[j]; j++)
      ;
    nr_subgroups = j;
    if (nr_subgroups > 0) {
      category->subgroups = calloc(nr_subgroups, sizeof(*category->subgroups));
      if (!category->subgroups) {
        fprintf(stderr, "uim-custom-cli: cannot allocate subgroups: %s\n",
                primary_groups[i]);
        uim_custom_symbol_list_free(subgroup_symbols);
        uim_custom_symbol_list_free(primary_groups);
        custom_categories_free(categories);
        return 0;
      }
    }
    category->nr_subgroups = nr_subgroups;

    for (j = 0; j < category->nr_subgroups; j++) {
      category->subgroups[j].group = uim_custom_group_get(subgroup_symbols[j]);
      category->subgroups[j].custom_symbols =
        uim_custom_collect_by_group(subgroup_symbols[j]);
      if (!category->subgroups[j].group ||
          !category->subgroups[j].custom_symbols) {
        fprintf(stderr, "uim-custom-cli: cannot collect subgroup: %s\n",
                subgroup_symbols[j]);
        uim_custom_symbol_list_free(subgroup_symbols);
        uim_custom_symbol_list_free(primary_groups);
        custom_categories_free(categories);
        return 0;
      }
    }
    uim_custom_symbol_list_free(subgroup_symbols);
  }

  uim_custom_symbol_list_free(primary_groups);
  return 1;
}

static int
custom_category_subgroup_contains(
    const struct custom_category_subgroup *subgroup,
    const char *custom_sym)
{
  char **symbol;

  for (symbol = subgroup->custom_symbols; symbol && *symbol; symbol++) {
    if (strcmp(*symbol, custom_sym) == 0)
      return 1;
  }
  return 0;
}

static const struct custom_category *
find_custom_category(const struct custom_categories *categories,
                     const char *custom_sym)
{
  size_t i;
  char **symbol;

  for (i = 0; i < categories->nr_items; i++) {
    for (symbol = categories->items[i].custom_symbols;
         symbol && *symbol;
         symbol++) {
      if (strcmp(*symbol, custom_sym) == 0)
        return &categories->items[i];
    }
  }
  return NULL;
}

static void
print_custom_category(const struct custom_category *category,
                      const char *custom_sym)
{
  size_t i;
  int printed_subgroup = 0;

  printf("primary-group: %s", category->group->symbol);
  if (category->group->label && category->group->label[0] != '\0')
    printf(" (%s)", category->group->label);
  printf("\n");

  for (i = 0; i < category->nr_subgroups; i++) {
    const struct custom_category_subgroup *subgroup =
      &category->subgroups[i];

    /* "main" is implicit when no explicit subgroup was specified. */
    if (strcmp(subgroup->group->symbol, "main") == 0 ||
        !custom_category_subgroup_contains(subgroup, custom_sym))
      continue;

    if (!printed_subgroup)
      printf("subgroups:     ");
    else
      printf("               ");
    printf("%s", subgroup->group->symbol);
    if (subgroup->group->label && subgroup->group->label[0] != '\0')
      printf(" (%s)", subgroup->group->label);
    printf("\n");
    printed_subgroup = 1;
  }
  if (!printed_subgroup)
    printf("subgroups:     (none)\n");
}

static void
print_choices(const struct uim_custom_choice *const *choices,
              const char *description)
{
  const struct uim_custom_choice *const *choice;

  printf("%s", description);
  for (choice = choices; choice && *choice; choice++) {
    printf(" %s", (*choice)->symbol);
    if ((*choice)->label && (*choice)->label[0] != '\0')
      printf(" (%s)", (*choice)->label);
  }
  printf("\n");
}

static int
print_custom(const char *custom_sym,
             const struct custom_category *category)
{
  struct uim_custom *custom;
  char *value;

  custom = uim_custom_get(custom_sym);
  if (!custom) {
    fprintf(stderr, "uim-custom-cli: cannot get custom: %s\n", custom_sym);
    return 0;
  }

  value = uim_custom_value_as_literal(custom_sym);
  if (!value) {
    fprintf(stderr, "uim-custom-cli: cannot get custom value: %s\n",
            custom_sym);
    uim_custom_free(custom);
    return 0;
  }

  printf("symbol:        %s\n",
         custom->symbol);
  print_custom_category(category, custom_sym);

  printf("type:          %s\n"
         "active?:       %s\n"
         "value:         %s\n"
         "label:         %s\n"
         "description:   %s\n",
         custom_type_name(custom->type),
         custom->is_active ? "true" : "false",
         value,
         custom->label,
         custom->desc);

  switch (custom->type) {
  case UCustom_Int:
    printf("range:         %d - %d\n",
           custom->range->as_int.min,
           custom->range->as_int.max);
    break;
  case UCustom_Str:
    printf("regex:         %s\n", custom->range->as_str.regex);
    break;
  case UCustom_Pathname:
    printf("path type:     %s\n",
           custom->value->as_pathname->type == UCustomPathnameType_Directory
           ? "directory" : "regular-file");
    break;
  case UCustom_Choice:
    print_choices((const struct uim_custom_choice *const *)
                  custom->range->as_choice.valid_items,
                  "candidates:   ");
    break;
  case UCustom_OrderedList:
    print_choices((const struct uim_custom_choice *const *)
                  custom->range->as_olist.valid_items,
                  "items:        ");
    break;
  case UCustom_Table:
    print_choices((const struct uim_custom_choice *const *)
                  custom->range->as_table_header.valid_items,
                  "columns:      ");
    break;
  default:
    break;
  }

  free(value);
  uim_custom_free(custom);
  return 1;
}

static int
list_customs(void)
{
  struct custom_categories categories;
  char **custom_syms;
  char **custom_sym;
  int succeeded;

  if (uim_init() < 0) {
    fprintf(stderr, "uim-custom-cli: uim_init() failed\n");
    return 0;
  }

  if (!uim_custom_enable()) {
    fprintf(stderr, "uim-custom-cli: uim_custom_enable() failed\n");
    uim_quit();
    return 0;
  }

  if (!collect_custom_categories(&categories)) {
    uim_quit();
    return 0;
  }

  custom_syms = uim_custom_collect_by_group(NULL);
  if (!custom_syms) {
    fprintf(stderr, "uim-custom-cli: cannot collect custom variables\n");
    custom_categories_free(&categories);
    uim_quit();
    return 0;
  }

  /* Report an error if a custom does not belong to a category. */
  for (custom_sym = custom_syms; *custom_sym; custom_sym++) {
    if (!find_custom_category(&categories, *custom_sym)) {
      fprintf(stderr, "uim-custom-cli: custom has no primary group: %s\n",
              *custom_sym);
      uim_custom_symbol_list_free(custom_syms);
      custom_categories_free(&categories);
      uim_quit();
      return 0;
    }
  }

  succeeded = 1;
  for (custom_sym = custom_syms; *custom_sym; custom_sym++) {
    const struct custom_category *category;

    category = find_custom_category(&categories, *custom_sym);
    succeeded = print_custom(*custom_sym, category) && succeeded;
    if (custom_sym[1])
      printf("\n");
  }

  uim_custom_symbol_list_free(custom_syms);
  custom_categories_free(&categories);
  uim_quit();
  return succeeded;
}

static int
contains_line_break(const char *value)
{
  return strchr(value, '\n') != NULL || strchr(value, '\r') != NULL;
}

static int
normalize_string_value(const char *value, char **normalized_value)
{
  size_t i;
  size_t len;
  size_t escaped_len;
  char *p;

  len = strlen(value);
  if (value[0] == '"') {
    if (len < 2 || value[len - 1] != '"') {
      /* Reject strings that do not end with a double quote. */
      return 0;
    }
    for (i = 1; i < len - 1; i++) {
      if (value[i] == '\\') {
        if (++i >= len - 1) {
          /* Reject strings with a trailing backslash. */
          return 0;
        }
      } else if (value[i] == '"') {
        /* Reject strings with an unescaped double quote. */
        return 0;
      }
    }
    *normalized_value = strdup(value);
    return *normalized_value != NULL;
  }

  /* Wrap bare values in double quotes. */
  escaped_len = len + 2;
  for (i = 0; i < len; i++) {
    if (value[i] == '\\' || value[i] == '"')
      escaped_len++;
  }

  *normalized_value = (char *)malloc(escaped_len);
  if (!*normalized_value)
    return 0;

  p = *normalized_value;
  *p++ = '"';
  for (i = 0; i < len; i++) {
    /* Escape backslashes and double quotes. */
    if (value[i] == '\\' || value[i] == '"')
      *p++ = '\\';
    *p++ = value[i];
  }
  *p++ = '"';
  *p = '\0';
  return 1;
}

static int
make_ordered_list_value(int nr_values, char **values, char **list_value)
{
  size_t i;
  size_t length = 3;
  char *p;

  for (i = 0; i < (size_t)nr_values; i++)
    length += strlen(values[i]) + (i ? 1 : 0);

  *list_value = (char *)malloc(length);
  if (!*list_value)
    return 0;

  /* Convert one or more values into a list. */
  p = *list_value;
  *p++ = '(';
  for (i = 0; i < (size_t)nr_values; i++) {
    size_t value_length;

    if (i)
      *p++ = ' ';
    value_length = strlen(values[i]);
    memcpy(p, values[i], value_length);
    p += value_length;
  }
  *p++ = ')';
  *p = '\0';
  return 1;
}

static int
normalize_ordered_list_value(
    const char *custom_sym,
    const char *value,
    const struct uim_custom_choice *const *items,
    char **normalized_value)
{
  const char *list;
  const struct uim_custom_choice *const *item;
  char *contents;
  char *token;
  char *owned_list = NULL;
  char *single_value;
  char *single_values[1];
  size_t contents_len;
  size_t i;

  list = value;
  if (list[0] == '(' || (list[0] == '\'' && list[1] == '(')) {
    if (*list == '\'')
      list++;
  } else {
    /* Convert a single value into a list containing one item. */
    single_value = (char *)list;
    if (*single_value == '\'')
      single_value++;
    if (*single_value == '\0') {
      fprintf(stderr, "uim-custom-cli: expected an ordered-list item for %s\n", custom_sym);
      return 0;
    }
    single_values[0] = single_value;
    if (!make_ordered_list_value(1, single_values, &owned_list)) {
      fprintf(stderr, "uim-custom-cli: cannot allocate custom value\n");
      return 0;
    }
    list = owned_list;
  }

  contents_len = strlen(list);
  if (contents_len < 2 || list[0] != '(' || list[contents_len - 1] != ')') {
    fprintf(stderr, "uim-custom-cli: expected a Scheme list for %s\n", custom_sym);
    free(owned_list);
    return 0;
  }

  for (i = 1; i < contents_len - 1; i++) {
    if (list[i] == '(' || list[i] == ')') {
      fprintf(stderr, "uim-custom-cli: expected a flat Scheme list for %s\n", custom_sym);
      free(owned_list);
      return 0;
    }
  }

  contents_len -= 2;
  contents = (char *)malloc(contents_len + 1);
  if (!contents) {
    fprintf(stderr, "uim-custom-cli: cannot allocate custom value\n");
    free(owned_list);
    return 0;
  }
  memcpy(contents, list + 1, contents_len);
  contents[contents_len] = '\0';

  /* Validate each item in the list against the available items. */
  token = strtok(contents, " \t");
  while (token) {
    for (item = items; item && *item; item++) {
      if (strcmp((*item)->symbol, token) == 0)
        break;
    }
    if (!item || !*item) {
      fprintf(stderr, "uim-custom-cli: invalid item for ordered-list custom %s: %s\n", custom_sym, token);
      print_choices(items, "available items:");
      free(contents);
      free(owned_list);
      return 0;
    }
    token = strtok(NULL, " \t");
  }

  free(contents);
  *normalized_value = strdup(list);
  free(owned_list);
  return *normalized_value != NULL;
}

static int
normalize_custom_value(const char *custom_sym, const char *value,
                       int multiple_values, char **normalized_value)
{
  struct uim_custom *custom;
  char **custom_syms;
  char **sym;
  const char *choice_sym;
  const struct uim_custom_choice *const *choice;
  char *end;
  long integer_value;

  custom_syms = uim_custom_collect_by_group(NULL);
  if (!custom_syms) {
    fprintf(stderr, "uim-custom-cli: cannot collect custom variables\n");
    return 0;
  }

  custom = NULL;
  for (sym = custom_syms; *sym; sym++) {
    if (strcmp(*sym, custom_sym) == 0) {
      custom = uim_custom_get(custom_sym);
      break;
    }
  }
  uim_custom_symbol_list_free(custom_syms);

  if (!custom) {
    fprintf(stderr, "uim-custom-cli: unknown custom: %s\n", custom_sym);
    return 0;
  }

  if (multiple_values && custom->type != UCustom_OrderedList) {
    fprintf(stderr,
            "uim-custom-cli: multiple values are supported only for "
            "ordered-list customs: %s\n",
            custom_sym);
    uim_custom_free(custom);
    return 0;
  }

  switch (custom->type) {
  case UCustom_Bool:
    if (strcasecmp(value, "t") == 0 ||
        strcasecmp(value, "true") == 0 ||
        strcasecmp(value, "#t") == 0)
      *normalized_value = strdup("#t");
    else if (strcasecmp(value, "f") == 0 ||
             strcasecmp(value, "false") == 0 ||
             strcasecmp(value, "#f") == 0)
      *normalized_value = strdup("#f");
    else {
      fprintf(stderr, "uim-custom-cli: invalid boolean value for %s: %s\n",
              custom_sym, value);
      uim_custom_free(custom);
      return 0;
    }
    break;
  case UCustom_Int:
    errno = 0;
    integer_value = strtol(value, &end, 10);
    if (errno == ERANGE || *value == '\0' || *end != '\0' ||
        !custom->range ||
        integer_value < custom->range->as_int.min ||
        integer_value > custom->range->as_int.max) {
      fprintf(stderr, "uim-custom-cli: invalid integer value for %s: %s\n",
              custom_sym, value);
      uim_custom_free(custom);
      return 0;
    }
    *normalized_value = strdup(value);
    break;
  case UCustom_Str:
  case UCustom_Pathname:
    if (!normalize_string_value(value, normalized_value)) {
      fprintf(stderr, "uim-custom-cli: invalid Scheme string for %s\n",
              custom_sym);
      uim_custom_free(custom);
      return 0;
    }
    break;
  case UCustom_Choice:
    /* Validate the symbol without its optional quote. */
    choice_sym = value;
    if (*choice_sym == '\'')
      choice_sym++;
    if (*choice_sym == '\0' || *choice_sym == '(' ||
        *choice_sym == '"' || *choice_sym == '#') {
      fprintf(stderr, "uim-custom-cli: invalid choice value for %s: %s\n",
              custom_sym, value);
      uim_custom_free(custom);
      return 0;
    }
    if (!custom->range) {
      fprintf(stderr, "uim-custom-cli: custom has no choice range: %s\n",
              custom_sym);
      uim_custom_free(custom);
      return 0;
    }
    /* Check whether the value is one of the valid choices. */
    for (choice = (const struct uim_custom_choice *const *)
                       custom->range->as_choice.valid_items;
         choice && *choice; choice++) {
      if (strcmp((*choice)->symbol, choice_sym) == 0)
        break;
    }
    if (!choice || !*choice) {
      fprintf(stderr, "uim-custom-cli: invalid choice for custom %s: %s\n",
              custom_sym, choice_sym);
      print_choices((const struct uim_custom_choice *const *)
                    custom->range->as_choice.valid_items,
                    "available choices:");
      uim_custom_free(custom);
      return 0;
    }
    if (value[0] == '\'')
      *normalized_value = strdup(value);
    else
      uim_asprintf(normalized_value, "'%s", value);
    break;
  case UCustom_OrderedList:
    if (!custom->range) {
      fprintf(stderr, "uim-custom-cli: custom has no ordered-list items: %s\n",
              custom_sym);
      uim_custom_free(custom);
      return 0;
    }
    if (!normalize_ordered_list_value(
            custom_sym, value,
            (const struct uim_custom_choice *const *)
            custom->range->as_olist.valid_items,
            normalized_value)) {
      uim_custom_free(custom);
      return 0;
    }
    break;
  case UCustom_Key:
  case UCustom_Table:
    if (value[0] != '(' &&
        !(value[0] == '\'' && value[1] == '(')) {
      fprintf(stderr, "uim-custom-cli: expected a Scheme list for %s\n",
              custom_sym);
      uim_custom_free(custom);
      return 0;
    }
    *normalized_value = strdup(value);
    break;
  default:
    fprintf(stderr, "uim-custom-cli: unsupported custom type for %s\n",
            custom_sym);
    uim_custom_free(custom);
    return 0;
  }

  if (!*normalized_value) {
    fprintf(stderr, "uim-custom-cli: cannot allocate custom value\n");
    uim_custom_free(custom);
    return 0;
  }

  uim_custom_free(custom);
  return 1;
}

int
main(int argc, char **argv)
{
  char *message;
  char *value;
  char *owned_value = NULL;
  char *normalized_value = NULL;
  int multiple_values;
  int save = 0;
  int argument_offset = 1;
  int i;
  int fd;

  if (argc == 2 &&
      (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0)) {
    usage(stdout);
    return EXIT_SUCCESS;
  }

  if (argc == 2 &&
      (strcmp(argv[1], "--list") == 0 || strcmp(argv[1], "-l") == 0))
    return list_customs() ? EXIT_SUCCESS : EXIT_FAILURE;

  if (argc > 1 &&
      (strcmp(argv[1], "--save") == 0 || strcmp(argv[1], "-s") == 0)) {
    save = 1;
    argument_offset++;
  }

  if (argc - argument_offset < 2) {
    usage(stderr);
    return EXIT_FAILURE;
  }

  for (i = argument_offset + 1; i < argc; i++) {
    if (contains_line_break(argv[i])) {
      fprintf(stderr,
              "uim-custom-cli: custom value must not contain a line break\n");
      return EXIT_FAILURE;
    }
  }

  multiple_values = argc > argument_offset + 2;
  if (multiple_values) {
    /* Extra arguments are the CLI shorthand for an ordered-list value. */
    if (!make_ordered_list_value(argc - argument_offset - 1,
                                 &argv[argument_offset + 1],
                                 &owned_value)) {
      fprintf(stderr, "uim-custom-cli: cannot allocate custom value\n");
      return EXIT_FAILURE;
    }
    value = owned_value;
  } else {
    value = argv[argument_offset + 1];
  }

  if (uim_init() < 0) {
    fprintf(stderr, "uim-custom-cli: uim_init() failed\n");
    free(owned_value);
    return EXIT_FAILURE;
  }
  if (!uim_custom_enable()) {
    fprintf(stderr, "uim-custom-cli: uim_custom_enable() failed\n");
    free(owned_value);
    uim_quit();
    return EXIT_FAILURE;
  }

  if (!normalize_custom_value(argv[argument_offset], value, multiple_values,
                              &normalized_value)) {
    free(owned_value);
    uim_quit();
    return EXIT_FAILURE;
  }
  free(owned_value);

  if (!uim_custom_set_value_as_literal(argv[argument_offset],
                                       normalized_value)) {
    fprintf(stderr, "uim-custom-cli: cannot set custom value: %s\n",
            argv[argument_offset]);
    free(normalized_value);
    uim_quit();
    return EXIT_FAILURE;
  }

  if (save) {
    if (!uim_custom_save_custom(argv[argument_offset])) {
      fprintf(stderr, "uim-custom-cli: cannot save custom value: %s\n",
              argv[argument_offset]);
      free(normalized_value);
      uim_quit();
      return EXIT_FAILURE;
    }
  }

  uim_quit();

  fd = uim_helper_init_client_fd(NULL);
  if (fd < 0) {
    fprintf(stderr, "uim-custom-cli: cannot connect to uim-helper-server\n");
    free(normalized_value);
    return EXIT_FAILURE;
  }

  uim_asprintf(&message, "prop_update_custom\n%s\n%s\n",
               argv[argument_offset], normalized_value);
  uim_helper_send_message(fd, message);
  free(message);
  free(normalized_value);
  uim_helper_close_client_fd(fd);

  return EXIT_SUCCESS;
}
