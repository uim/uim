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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifndef DEBUG
#define NDEBUG
#endif

#include <uim/uim.h>

#ifdef HAVE_CURSES_H
#include <curses.h>
#endif
#ifdef HAVE_TERM_H
#include <term.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#include "key.h"
#include "uim-fep.h"

#define _KEY_UP    "\033[A"
#define _KEY_DOWN  "\033[B"
#define _KEY_RIGHT "\033[C"
#define _KEY_LEFT  "\033[D"

static int strcmp_prefix(const char *str, const char *prefix);

int tty2key(char key)
{
  /* c-a から c-z */
  if (key >= 1 && key <= 26) {
    return key + ('a' - 1);
  }
  /* c-\ c-] c-^ c-_ */
  if (key >= 28 && key <= 31) {
    return key + ('A' - 1);
  }
  switch (key) {
  /* c-space */
  case 0:
    return ' ';
  /* c-? */
  case 0x7f:
    return UKey_Delete;
  /* c-[ */
  case ESCAPE_CODE:
    return UKey_Escape;
  }
  return key;
}

int tty2key_state(char key)
{
  int key_state = 0;
  if (key >= 'A' && key <= 'Z') {
    key_state += UMod_Shift;
  }
  if (key >= 0 && key <= 31 && key != ESCAPE_CODE) {
    key_state +=  UMod_Control;
  }
  return key_state;
}

/*
 * strに対応するキーコードとエスケープシーケンスの長さを返す
 * 見つからなかったらUKey_Escapeと1を返す
 */
int *escape_sequence2key(const char *str)
{
  static int rval[2];
  int len;
  if ((len = strcmp_prefix(str, _KEY_UP)) > 0) {
    rval[0] = UKey_Up;
  } else if ((len = strcmp_prefix(str, _KEY_DOWN)) > 0) {
    rval[0] = UKey_Down;
  } else if ((len = strcmp_prefix(str, _KEY_RIGHT)) > 0) {
    rval[0] = UKey_Right;
  } else if ((len = strcmp_prefix(str, _KEY_LEFT)) > 0) {
    rval[0] = UKey_Left;
  } else if (key_backspace != NULL && (len = strcmp_prefix(str, key_backspace)) > 0) {
    rval[0] = UKey_Backspace;
  } else if (key_dc != NULL && (len = strcmp_prefix(str, key_dc)) > 0) {
    rval[0] = UKey_Delete;
  } else if (key_left != NULL && (len = strcmp_prefix(str, key_left)) > 0) {
    rval[0] = UKey_Left;
  } else if (key_up != NULL && (len = strcmp_prefix(str, key_up)) > 0) {
    rval[0] = UKey_Up;
  } else if (key_right != NULL && (len = strcmp_prefix(str, key_right)) > 0) {
    rval[0] = UKey_Right;
  } else if (key_down != NULL && (len = strcmp_prefix(str, key_down)) > 0) {
    rval[0] = UKey_Down;
  } else if (key_ppage != NULL && (len = strcmp_prefix(str, key_ppage)) > 0) {
    rval[0] = UKey_Prior;
  } else if (key_npage != NULL && (len = strcmp_prefix(str, key_npage)) > 0) {
    rval[0] = UKey_Next;
  } else if (key_home != NULL && (len = strcmp_prefix(str, key_home)) > 0) {
    rval[0] = UKey_Home;
  } else if (key_end != NULL && (len = strcmp_prefix(str, key_end)) > 0) {
    rval[0] = UKey_End;
  } else if (key_f1 != NULL && (len = strcmp_prefix(str, key_f1)) > 0) {
    rval[0] = UKey_F1;
  } else if (key_f2 != NULL && (len = strcmp_prefix(str, key_f2)) > 0) {
    rval[0] = UKey_F2;
  } else if (key_f3 != NULL && (len = strcmp_prefix(str, key_f3)) > 0) {
    rval[0] = UKey_F3;
  } else if (key_f4 != NULL && (len = strcmp_prefix(str, key_f4)) > 0) {
    rval[0] = UKey_F4;
  } else if (key_f5 != NULL && (len = strcmp_prefix(str, key_f5)) > 0) {
    rval[0] = UKey_F5;
  } else if (key_f6 != NULL && (len = strcmp_prefix(str, key_f6)) > 0) {
    rval[0] = UKey_F6;
  } else if (key_f7 != NULL && (len = strcmp_prefix(str, key_f7)) > 0) {
    rval[0] = UKey_F7;
  } else if (key_f8 != NULL && (len = strcmp_prefix(str, key_f8)) > 0) {
    rval[0] = UKey_F8;
  } else if (key_f9 != NULL && (len = strcmp_prefix(str, key_f9)) > 0) {
    rval[0] = UKey_F9;
  } else if (key_f10 != NULL && (len = strcmp_prefix(str, key_f10)) > 0) {
    rval[0] = UKey_F10;
  } else if (key_f11 != NULL && (len = strcmp_prefix(str, key_f11)) > 0) {
    rval[0] = UKey_F11;
  } else if (key_f12 != NULL && (len = strcmp_prefix(str, key_f12)) > 0) {
    rval[0] = UKey_F12;
  } else {
    rval[0] = UKey_Escape;
    len = 1;
  }
  rval[1] = len;
  return rval;
}

/*
 * prefixがstrの語頭のときstrlen(prefix)を返す
 * それ以外は0を返す
 */
static int strcmp_prefix(const char *str, const char *prefix)
{
  int i;
  assert(str != NULL && prefix != NULL);
  for (i = 0; str[i] != '\0' && prefix[i] != '\0'; i++) {
    if (str[i] != prefix[i]) {
      break;
    }
  }
  if (prefix[i] == '\0') {
    return i;
  }
  return 0;
}
