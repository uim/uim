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
   This is an obsolete API only exist for backward compatibility. New
   codes should not use this. Use uim-custom.h instead.

                                                2004-12-21 YamaKen
*/


/* API and ABI are unstable */
#ifndef _uim_compat_custom_h_included_
#define _uim_compat_custom_h_included_

#include "uim-compat-scm.h"

#ifdef __cplusplus
extern "C" {
#endif


enum UCustomType {
  UCustom_Bool,
  UCustom_Int,
  UCustom_Str,
  UCustom_Path,
  UCustom_Symbol,
  UCustom_Key
};

uim_lisp
uim_custom_value(uim_lisp custom_sym);
int
uim_custom_value_as_bool(uim_lisp custom_sym);
int
uim_custom_value_as_int(uim_lisp custom_sym);
char *
uim_custom_value_as_str(uim_lisp custom_sym);
char *
uim_custom_value_as_path(uim_lisp custom_sym);
uim_lisp
uim_custom_value_as_symbol(uim_lisp custom_sym);
/* char **uim_custom_value_as_key(uim_lisp custom_sym); */
void
uim_custom_set(uim_lisp custom_sym, uim_lisp custom_val);
char *
uim_custom_symbol_label(uim_lisp custom_sym, uim_lisp val_sym);
char *
uim_custom_symbol_desc(uim_lisp custom_sym, uim_lisp val_sym);
uim_lisp
uim_custom_label(uim_lisp custom_sym);
uim_lisp
uim_custom_desc(uim_lisp custom_sym);
uim_lisp
uim_custom_type(uim_lisp custom_sym);
uim_lisp
uim_custom_default_value(uim_lisp custom_sym);
int
uim_custom_ctype(uim_lisp custom_sym);
uim_lisp
uim_custom_range(uim_lisp custom_sym);
char *
uim_custom_group_label(uim_lisp group_sym);
char *
uim_custom_group_desc(uim_lisp group_sym);
uim_lisp
uim_custom_group_subgroups(uim_lisp group_sym);
uim_lisp
uim_custom_list_groups(void);
uim_lisp
uim_custom_list_primary_groups(void);
uim_lisp
uim_custom_collect_by_group(uim_lisp group_sym);
char *
uim_custom_value_as_string(uim_lisp sym);
char *
uim_custom_definition_as_string(uim_lisp sym);

#ifdef __cplusplus
}
#endif
#endif
