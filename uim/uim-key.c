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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "context.h"

/* valid-key-symbols in key.scm has also to be updated if you add new
 * key definition to key_tab
 */
static struct {
  int key;
  char *str;
} key_tab[] = {
  {UKey_Backspace, "backspace"},
  {UKey_Delete, "delete"},
  {UKey_Escape, "escape"},
  {UKey_Return, "return"},
  {UKey_Tab, "tab"},
  {UKey_Left, "left"},
  {UKey_Up, "up"},
  {UKey_Right, "right"},
  {UKey_Down, "down"},
  {UKey_Prior, "prior"},
  {UKey_Next, "next"},
  {UKey_Home, "home"},
  {UKey_End, "end"},
  {UKey_Zenkaku_Hankaku, "zenkaku-hankaku"},
  {UKey_Multi_key, "Multi_key"},
  {UKey_Mode_switch, "Mode_switch"},
  {UKey_Henkan_Mode, "Henkan_Mode"},
  {UKey_Muhenkan, "Muhenkan"},
  {UKey_F1, "F1"},
  {UKey_F2, "F2"},
  {UKey_F3, "F3"},
  {UKey_F4, "F4"},
  {UKey_F5, "F5"},
  {UKey_F6, "F6"},
  {UKey_F7, "F7"},
  {UKey_F8, "F8"},
  {UKey_F9, "F9"},
  {UKey_F10, "F10"},
  {UKey_F11, "F11"},
  {UKey_F12, "F12"},
  {UKey_F13, "F13"},
  {UKey_F14, "F14"},
  {UKey_F15, "F15"},
  {UKey_F16, "F16"},
  {UKey_F17, "F17"},
  {UKey_F18, "F18"},
  {UKey_F19, "F19"},
  {UKey_F20, "F20"},
  {UKey_F21, "F21"},
  {UKey_F22, "F22"},
  {UKey_F23, "F23"},
  {UKey_F24, "F24"},
  {UKey_F25, "F25"},
  {UKey_F26, "F26"},
  {UKey_F27, "F27"},
  {UKey_F28, "F28"},
  {UKey_F29, "F29"},
  {UKey_F30, "F30"},
  {UKey_F31, "F31"},
  {UKey_F32, "F32"},
  {UKey_F33, "F33"},
  {UKey_F34, "F34"},
  {UKey_F35, "F35"},
  {UKey_Private1, "Private1"},
  {UKey_Private2, "Private2"},
  {UKey_Private3, "Private3"},
  {UKey_Private4, "Private4"},
  {UKey_Private5, "Private5"},
  {UKey_Private6, "Private6"},
  {UKey_Private7, "Private7"},
  {UKey_Private8, "Private8"},
  {UKey_Private9, "Private9"},
  {UKey_Private10, "Private10"},
  {UKey_Private11, "Private11"},
  {UKey_Private12, "Private12"},
  {UKey_Private13, "Private13"},
  {UKey_Private14, "Private14"},
  {UKey_Private15, "Private15"},
  {UKey_Private16, "Private16"},
  {UKey_Private17, "Private17"},
  {UKey_Private18, "Private18"},
  {UKey_Private19, "Private19"},
  {UKey_Private20, "Private20"},
  {UKey_Private21, "Private21"},
  {UKey_Private22, "Private22"},
  {UKey_Private23, "Private23"},
  {UKey_Private24, "Private24"},
  {UKey_Private25, "Private25"},
  {UKey_Private26, "Private26"},
  {UKey_Private27, "Private27"},
  {UKey_Private28, "Private28"},
  {UKey_Private29, "Private29"},
  {UKey_Private30, "Private30"},
  {UKey_Shift_key, "Shift_key"},
  {UKey_Alt_key, "Alt_key"},
  {UKey_Control_key, "Control_key"},
  {UKey_Meta_key, "Meta_key"},
  {UKey_Super_key, "Super_key"},
  {UKey_Hyper_key, "Hyper_key"},
  /*  {UKey_Other, "other"},*/
  {0,0}
};

#if 0
int uim_key_sym_to_int(uim_lisp sym);

int
uim_key_sym_to_int(uim_lisp sym_)
{
  char *sym = uim_scm_refer_c_str(sym_);
  int res = 0;
  int i;
  for (i = 0; key_tab[i].key; i++) {
    if (!strcmp(key_tab[i].str, sym)) {
      res = key_tab[i].key;
    }
  }
  return res;
}
#endif

static char *
get_sym(int key)
{
  int i;
  char *res = NULL;
  for (i = 0; key_tab[i].key; i++) {
    if (key_tab[i].key == key) {
      res = key_tab[i].str;
    }
  }
  return res;
}

static int
keycode_to_sym(int key, char *buf)
{
  char *s = get_sym(key);
  if (!s) {
    if (key > 128) {
      return -1;
    }
    snprintf(buf, 19, "%d", key);
  } else {
    snprintf(buf, 19, "'%s", s);
  }
  return 0;
}

static void
handle_key(uim_context uc, char *p, int key, int state)
{
  char keybuf[20];
  int rv;

  rv = keycode_to_sym(key, keybuf);
  if (rv == -1) {
    uc->commit_raw_flag = 1;
    return;
  }
  UIM_EVAL_FSTRING4(uc, "(key-%s-handler %d %s %d)", p, uc->id, keybuf, state);
}

static int
emergency_key_p(int key, int state)
{
  if ((state == UMod_Shift) &&
      (key == UKey_Backspace)) {
    return 1;
  }
  return 0;
}

int
uim_press_key(uim_context uc, int key, int state)
{
  if (!uc) {
    return 1;
  }
  if (getenv("LIBUIM_ENABLE_EMERGENCY_KEY") && emergency_key_p(key, state)) {
    uc->is_enable = uc->is_enable ? 0 : 1;
    return 0;
  }
  if (!uc->is_enable) {
    return 1;
  }
  uc->commit_raw_flag = 0;
  handle_key(uc, "press", key, state);
  return uc->commit_raw_flag;
}

int
uim_release_key(uim_context uc, int key, int state)
{
  if (!uc) {
    return 1;
  }
  if (!uc->is_enable) {
    return 1;
  }
  uc->commit_raw_flag = 0;
  handle_key(uc, "release", key, state);
  return uc->commit_raw_flag;
}

static uim_lisp
define_key(uim_lisp args, uim_lisp env)
{
  uim_lisp form, predicate_sym, sources;

  predicate_sym = uim_scm_car(args);
  sources = uim_scm_nullp(args) ? uim_scm_null_list() : uim_scm_cadr(args);
  form = uim_scm_list3(uim_scm_make_symbol("define-key-internal"),
		       uim_scm_quote(predicate_sym),
		       sources);

  return uim_scm_eval(form);
}

void
uim_init_key_subrs(void)
{
  uim_scm_init_fsubr("define-key", (uim_lisp (*)(uim_lisp, uim_lisp))define_key);
}
