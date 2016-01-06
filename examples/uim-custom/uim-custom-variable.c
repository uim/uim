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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "uim/uim.h"
#include "uim/uim-custom.h"

static char *
choice_items_to_str(const struct uim_custom_choice **items, const char *sep)
{
  size_t buf_size;
  char *buf, *bufp;
  const struct uim_custom_choice **item;

  buf_size = sizeof('\0');
  for (item = items; *item; item++) {
    if (item != items)
      buf_size += strlen(sep);
    buf_size += strlen((*item)->symbol);
  }
  buf = (char *)malloc(buf_size);

  for (bufp = buf, item = items; *item; item++) {
    if (item != items) {
      strcpy(bufp, sep);
      bufp += strlen(sep);
    }
    strcpy(bufp, (*item)->symbol);
    bufp += strlen((*item)->symbol);
  }

  return buf;
}

static void
inspect_custom(const struct uim_custom *custom)
{
  char buf_val[64], buf_default_val[64], buf_range[128];
  const char *s_type, *s_value, *s_default_value;
  char *s_range;

  s_range = "";
  switch (custom->type) {
  case UCustom_Bool:
    s_type = "boolean";
    s_value = custom->value->as_bool ? "#t" : "#f";
    s_default_value = custom->default_value->as_bool ? "#t" : "#f";
    break;
  case UCustom_Int:
    s_type = "integer";
    sprintf(buf_val, "%d", custom->value->as_int);
    s_value = buf_val;
    sprintf(buf_default_val, "%d", custom->default_value->as_int);
    s_default_value = buf_default_val;
    sprintf(buf_range, "%d - %d",
	    custom->range->as_int.min,
	    custom->range->as_int.max);
    s_range = buf_range;
    break;
  case UCustom_Str:
    s_type = "string";
    s_value = custom->value->as_str;
    s_default_value = custom->default_value->as_str;
    s_range = custom->range->as_str.regex;
    break;
  case UCustom_Pathname:
    s_type = "pathname";
    s_value = custom->value->as_pathname->str;
    s_default_value = custom->default_value->as_pathname->str;
    break;
  case UCustom_Choice:
    s_type = "choice";
    sprintf(buf_val, "%s (%s)",
	    custom->value->as_choice->symbol,
	    custom->value->as_choice->label);
    s_value = buf_val;
    sprintf(buf_default_val, "%s (%s)",
	    custom->default_value->as_choice->symbol,
	    custom->default_value->as_choice->label);
    s_default_value = buf_default_val;
    s_range = choice_items_to_str((const struct uim_custom_choice **)custom->range->as_choice.valid_items, " ");
    break;
  case UCustom_Key:
    s_type = "key";
    s_value = "";
    s_default_value = "";
    break;
  default:
    s_type = "";
    s_value = "";
    s_default_value = "";
  }

  printf("symbol:        %s\n", custom->symbol);
  printf("type:          %s\n", s_type);
  printf("active?:       %s\n", custom->is_active ? "true" : "false");
  printf("value:         %s\n", s_value);
  printf("default value: %s\n", s_default_value);
  printf("range:         %s\n", s_range);
  printf("label:         %s\n", custom->label);
  printf("description:   %s\n", custom->desc);

  if (custom->type == UCustom_Choice)
    free(s_range);
}

int
main(int argc, char *argv[])
{
  if (uim_init() < 0) {
    fprintf(stderr, "uim_init() failed.\n");
    return -1;
  }

  if (uim_custom_enable()) {
    uim_bool succeeded;
    struct uim_custom *custom;

    custom = uim_custom_get("anthy-candidate-op-count");
    if (custom) {
      inspect_custom(custom);

      printf("\ntrying that modify the custom value to 100\n");
      custom->value->as_int = 100;  /* out of range */
      succeeded = uim_custom_set(custom);
      printf("succeeded = %s\n", succeeded ? "true" : "false");

      printf("\ncurrent status of struct uim_custom *custom\n");
      inspect_custom(custom);  /* shows 100 as value */
      uim_custom_free(custom);

      printf("\ncurrent status of real custom value\n");
      custom = uim_custom_get("anthy-candidate-op-count");
      inspect_custom(custom);  /* shows real value */

      printf("\ntrying that modify the custom value to 5\n");
      custom->value->as_int = 5;  /* valid */
      succeeded = uim_custom_set(custom);
      printf("succeeded = %s\n\n", succeeded ? "true" : "false");
      inspect_custom(custom);

      uim_custom_free(custom);
    }

    uim_custom_save();  /* save updated custom value into ~/.uim.d/customs/ */
  } else {
    fprintf(stderr, "uim_custom_enable() failed.\n");
    uim_quit();
    return -1;
  }

  uim_quit();

  return 0;
}
