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
#include <uim/uim.h>
#include <uim/uim-custom.h>

static uim_bool
dump_custom(const char *custom_sym)
{
  struct uim_custom *custom;
  char *def_literal;

  custom = uim_custom_get(custom_sym);
  if (custom) {
    /* human readable variable name and description */
    printf("\n;; %s\n;; %s\n", custom->label, custom->desc);

    def_literal = uim_custom_definition_as_literal(custom_sym);
    if (def_literal) {
      printf("%s\n", def_literal);
      free(def_literal);
    }
    uim_custom_free(custom);
  }

  return UIM_TRUE;
}

static uim_bool
dump_group(const char *group_sym)
{
  struct uim_custom_group *group;
  char **custom_syms, **custom_sym;

  group = uim_custom_group_get(group_sym);
  if (!group)
    return UIM_FALSE;

  /* print group header */
  printf(";;;\n;;; %s\n;;;\n", group->label);

  custom_syms = uim_custom_collect_by_group(group_sym);
  if (custom_syms) {
    for (custom_sym = custom_syms; *custom_sym; custom_sym++) {
      dump_custom(*custom_sym);
    }
    uim_custom_symbol_list_free(custom_syms);
  }

  printf("\n\n");

  return UIM_TRUE;
}

int
main(int argc, char *argv[])
{
  if (uim_init() < 0) {
    fprintf(stderr, "uim_init() failed.\n");
    return -1;
  }

  if (uim_custom_enable()) {
    char **primary_groups, **grp;

    primary_groups = uim_custom_primary_groups();
    for (grp = primary_groups; *grp; grp++) {
      dump_group(*grp);
    }
    uim_custom_symbol_list_free(primary_groups);
  } else {
    fprintf(stderr, "uim_custom_enable() failed.\n");
    uim_quit();
    return -1;
  }

  uim_quit();

  return 0;
}
