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

#include "config.h"

#include <stdlib.h>
#include <string.h>
#include "uim-compat-scm.h"
#include "uim-compat-custom.h"


uim_lisp
uim_custom_value(uim_lisp custom_sym) {
  uim_lisp form;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-value"),
		       uim_scm_quote(custom_sym));

  return uim_scm_eval(form);
}

int
uim_custom_value_as_bool(uim_lisp custom_sym) {
  uim_lisp val;
  int result = 0;

  if (uim_scm_eq(uim_custom_type(custom_sym),
		 uim_scm_intern_c_str("boolean")))
  {
    val = uim_custom_value(custom_sym);
    result = uim_scm_eq(val, uim_scm_f()) ? 0 : 1;
  }

  return result;
}

int
uim_custom_value_as_int(uim_lisp custom_sym) {
  uim_lisp val;
  int result = 0;

  if (uim_scm_eq(uim_custom_type(custom_sym),
		 uim_scm_intern_c_str("integer")))
  {
    val = uim_custom_value(custom_sym);
    result = uim_scm_c_int(val);
  }

  return result;
}

char *
uim_custom_value_as_str(uim_lisp custom_sym) {
  uim_lisp val;
  char *result = NULL;

  if (uim_scm_eq(uim_custom_type(custom_sym),
		 uim_scm_intern_c_str("string")))
  {
    val = uim_custom_value(custom_sym);
    result = uim_scm_c_str(val);
  }

  return result;
}

char *
uim_custom_value_as_path(uim_lisp custom_sym) {
  uim_lisp val;
  char *result = NULL;

  if (uim_scm_eq(uim_custom_type(custom_sym),
		 uim_scm_intern_c_str("pathname")))
  {
    val = uim_custom_value(custom_sym);
    result = uim_scm_c_str(val);
  }

  return result;
}

uim_lisp
uim_custom_value_as_symbol(uim_lisp custom_sym) {
  uim_lisp val;

  if (uim_scm_eq(uim_custom_type(custom_sym),
		 uim_scm_intern_c_str("symbol")))
  {
    val = uim_custom_value(custom_sym);
  } else {
    val = uim_scm_f();
  }

  return val;
}

void
uim_custom_set(uim_lisp custom_sym, uim_lisp custom_val) {
  uim_lisp form;

  form = uim_scm_list3(uim_scm_intern_c_str("custom-set!"),
		       uim_scm_quote(custom_sym),
		       custom_val);
  uim_scm_eval(form);
}

char *
uim_custom_symbol_label(uim_lisp custom_sym, uim_lisp val_sym) {
  uim_lisp form, label;

  form = uim_scm_list3(uim_scm_intern_c_str("custom-symbol-label"),
		       uim_scm_quote(custom_sym),
		       uim_scm_quote(val_sym));
  label = uim_scm_eval(form);

  return uim_scm_c_str(label);
}

char *
uim_custom_symbol_desc(uim_lisp custom_sym, uim_lisp val_sym) {
  uim_lisp form, desc;

  form = uim_scm_list3(uim_scm_intern_c_str("custom-symbol-desc"),
		       uim_scm_quote(custom_sym),
		       uim_scm_quote(val_sym));
  desc = uim_scm_eval(form);

  return uim_scm_c_str(desc);
}

uim_lisp
uim_custom_label(uim_lisp custom_sym) {
  uim_lisp form;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-label"),
		       uim_scm_quote(custom_sym));

  return uim_scm_eval(form);
}

uim_lisp
uim_custom_desc(uim_lisp custom_sym) {
  uim_lisp form;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-desc"),
		       uim_scm_quote(custom_sym));

  return uim_scm_eval(form);
}

uim_lisp
uim_custom_type(uim_lisp custom_sym) {
  uim_lisp form;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-type"),
		       uim_scm_quote(custom_sym));

  return uim_scm_eval(form);
}

uim_lisp
uim_custom_default_value(uim_lisp custom_sym) {
  uim_lisp form;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-default-value"),
		       uim_scm_quote(custom_sym));

  return uim_scm_eval(form);
}

int
uim_custom_ctype(uim_lisp custom_sym) {
  uim_lisp type;
  int result;

  type = uim_custom_type(custom_sym);
  if (uim_scm_eq(type,
		 uim_scm_intern_c_str("boolean")))
  {
    result = UCustom_Bool;
  } else if (uim_scm_eq(type,
			uim_scm_intern_c_str("integer")))
  {
    result = UCustom_Int;
  } else if (uim_scm_eq(type,
			uim_scm_intern_c_str("string")))
  {
    result = UCustom_Str;
  } else if (uim_scm_eq(type,
			uim_scm_intern_c_str("pathname")))
  {
    result = UCustom_Path;
  } else if (uim_scm_eq(type,
			uim_scm_intern_c_str("symbol")))
  {
    result = UCustom_Symbol;
  } else if (uim_scm_eq(type,
			uim_scm_intern_c_str("key")))
  {
    result = UCustom_Key;
  } else {
    result = UCustom_Bool;
  }

  return result;
}

uim_lisp
uim_custom_range(uim_lisp custom_sym) {
  uim_lisp form;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-range"),
		       uim_scm_quote(custom_sym));

  return uim_scm_eval(form);
}

char *
uim_custom_group_label(uim_lisp group_sym) {
  uim_lisp form, label;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-group-label"),
		       uim_scm_quote(group_sym));
  label = uim_scm_eval(form);

  return uim_scm_c_str(label);
}

char *
uim_custom_group_desc(uim_lisp group_sym) {
  uim_lisp form, desc;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-group-desc"),
		       uim_scm_quote(group_sym));
  desc = uim_scm_eval(form);

  return uim_scm_c_str(desc);
}

uim_lisp
uim_custom_group_subgroups(uim_lisp group_sym) {
  uim_lisp form, subgrps;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-group-subgroups"),
		       uim_scm_quote(group_sym));
  subgrps = uim_scm_eval(form);

  return subgrps;
}

uim_lisp
uim_custom_list_groups(void) {
  uim_lisp form, groups;

  form = uim_scm_list1(uim_scm_intern_c_str("custom-list-groups"));
  groups = uim_scm_eval(form);

  return groups;
}

uim_lisp
uim_custom_list_primary_groups(void) {
  uim_lisp form, groups;

  form = uim_scm_list1(uim_scm_intern_c_str("custom-list-primary-groups"));
  groups = uim_scm_eval(form);

  return groups;
}

uim_lisp
uim_custom_collect_by_group(uim_lisp group_sym) {
  uim_lisp form, customs;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-collect-by-group"),
		       uim_scm_quote(group_sym));
  customs = uim_scm_eval(form);

  return customs;
}

char *
uim_custom_value_as_string(uim_lisp sym) {
  uim_lisp form, value;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-canonical-value-as-string"),
		       uim_scm_quote(sym));
  value = uim_scm_eval(form);

  return uim_scm_c_str(value);
}

char *
uim_custom_definition_as_string(uim_lisp sym) {
  uim_lisp form, definition;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-as-string"),
		       uim_scm_quote(sym));
  definition = uim_scm_eval(form);

  return uim_scm_c_str(definition);
}
