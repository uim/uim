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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#if (!defined(DEBUG) && !defined(NDEBUG))
#define NDEBUG
#endif

#include <uim/uim.h>

#ifdef HAVE_CURSES_H
#include <curses.h>
#endif
#ifdef HAVE_TERM_H
#include <term.h>
#elif HAVE_NCURSES_TERM_H
#include <ncurses/term.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
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

static int strcmp_prefix(const char *str, int str_len, const char *prefix);

int tty2key(char key)
{
  key &= 0x7f;
  switch (key) {
  /* c-space */
  case 0:
    return ' ';
  case '\t':
    return UKey_Tab;
  case '\r':
    return UKey_Return;
  /* c-[ */
  case ESCAPE_CODE:
    return UKey_Escape;
  /* c-? */
  case 0x7f:
    return UKey_Delete;
  }
  /* c-a から c-z */
  if (key >= 1 && key <= 26) {
    return key + ('a' - 1);
  }
  /* c-\ c-] c-^ c-_ */
  if (key >= 28 && key <= 31) {
    return key + ('A' - 1);
  }
  return key;
}

int tty2key_state(char key)
{
  int key_state = (key & 0x80) ? UMod_Meta : 0;
  key &= 0x7f;
  if (key == '\t' ||
      key == '\r' ||
      key == ESCAPE_CODE ||
      key == 0x7f) {
    return key_state;
  }
  if (key >= 'A' && key <= 'Z') {
    key_state += UMod_Shift;
  }
  if (key <= 31) {
    key_state +=  UMod_Control;
  }
  return key_state;
}

/*
 * strに対応するキーコードとエスケープシーケンスの長さを返す
 * 見つからなかったらUKey_Escapeと、途中まで一致しているエスケープシー
 * ケンスがある場合はTRUEない場合はFALSEを返す
 */
int *escape_sequence2key(const char *str, int str_len)
{
  static int rval[2];
  int len;
  int not_enough = 0;
  if        (                         (not_enough += len = strcmp_prefix(str, str_len, _KEY_UP      )), len > 0) { rval[0] = UKey_Up;
  } else if (                         (not_enough += len = strcmp_prefix(str, str_len, _KEY_DOWN    )), len > 0) { rval[0] = UKey_Down;
  } else if (                         (not_enough += len = strcmp_prefix(str, str_len, _KEY_RIGHT   )), len > 0) { rval[0] = UKey_Right;
  } else if (                         (not_enough += len = strcmp_prefix(str, str_len, _KEY_LEFT    )), len > 0) { rval[0] = UKey_Left;
  } else if (key_backspace != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_backspace)), len > 0)) { rval[0] = UKey_Backspace;
  } else if (key_dc        != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_dc       )), len > 0)) { rval[0] = UKey_Delete;    
  } else if (key_left      != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_left     )), len > 0)) { rval[0] = UKey_Left;
  } else if (key_up        != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_up       )), len > 0)) { rval[0] = UKey_Up;
  } else if (key_right     != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_right    )), len > 0)) { rval[0] = UKey_Right;
  } else if (key_down      != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_down     )), len > 0)) { rval[0] = UKey_Down;
  } else if (key_ppage     != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_ppage    )), len > 0)) { rval[0] = UKey_Prior;
  } else if (key_npage     != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_npage    )), len > 0)) { rval[0] = UKey_Next;
  } else if (key_home      != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_home     )), len > 0)) { rval[0] = UKey_Home;
  } else if (key_end       != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_end      )), len > 0)) { rval[0] = UKey_End;
  } else if (key_ic        != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_ic       )), len > 0)) { rval[0] = UKey_Insert;    
  } else if (key_f1        != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_f1       )), len > 0)) { rval[0] = UKey_F1;
  } else if (key_f2        != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_f2       )), len > 0)) { rval[0] = UKey_F2;
  } else if (key_f3        != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_f3       )), len > 0)) { rval[0] = UKey_F3;
  } else if (key_f4        != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_f4       )), len > 0)) { rval[0] = UKey_F4;
  } else if (key_f5        != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_f5       )), len > 0)) { rval[0] = UKey_F5;
  } else if (key_f6        != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_f6       )), len > 0)) { rval[0] = UKey_F6;
  } else if (key_f7        != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_f7       )), len > 0)) { rval[0] = UKey_F7;
  } else if (key_f8        != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_f8       )), len > 0)) { rval[0] = UKey_F8;
  } else if (key_f9        != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_f9       )), len > 0)) { rval[0] = UKey_F9;
  } else if (key_f10       != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_f10      )), len > 0)) { rval[0] = UKey_F10;
  } else if (key_f11       != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_f11      )), len > 0)) { rval[0] = UKey_F11;
  } else if (key_f12       != NULL && ((not_enough += len = strcmp_prefix(str, str_len, key_f12      )), len > 0)) { rval[0] = UKey_F12;
  } else {
    rval[0] = UKey_Other;
    len = not_enough < 0 ? TRUE : FALSE;
  }
  rval[1] = len;
  return rval;
}

/*
 * prefixがstrの語頭のときstrlen(prefix)を返す
 * strがprefixの語頭のとき-strlen(str)を返す
 * それ以外は0を返す
 */
static int strcmp_prefix(const char *str, int str_len, const char *prefix)
{
  int i;
  assert(str != NULL && prefix != NULL);
  for (i = 0; i < str_len && prefix[i] != '\0'; i++) {
    if (str[i] != prefix[i]) {
      break;
    }
  }
  if (prefix[i] == '\0') {
    return i;
  }
  if (i == str_len) {
    return -i;
  }
  return 0;
}


void print_key(int key, int key_state)
{
  if (key == 'q' && key_state == 0) {
    done(EXIT_SUCCESS);
  }
  printf("\"");
  if (key_state & UMod_Alt) {
    printf("<Alt>");
  }
  if (key_state & UMod_Meta) {
    printf("<Meta>");
  }
  if (key_state & UMod_Control) {
    printf("<Control>");
  }

  if (key == '"' || key == '\\') {
    printf("\\%c", key);
  } else if (key >= ' ' && key <= 127) {
    printf("%c", key);
  } else {
    switch (key) {
    case UKey_Escape:
      printf("escape");
      break;
    case UKey_Tab:
      printf("tab");
      break;
    case UKey_Backspace:
      printf("backspace");
      break;
    case UKey_Delete:
      printf("delete");
      break;
    case UKey_Return:
      printf("return");
      break;
    case UKey_Left:
      printf("left");
      break;
    case UKey_Up:
      printf("up");
      break;
    case UKey_Right:
      printf("right");
      break;
    case UKey_Down:
      printf("down");
      break;
    case UKey_Prior:
      printf("prior");
      break;
    case UKey_Next:
      printf("next");
      break;
    case UKey_Home:
      printf("home");
      break;
    case UKey_End:
      printf("end");
      break;
    case UKey_Insert:
      printf("insert");
      break;
    case UKey_Multi_key:
      printf("Multi_key");
      break;
    case UKey_Mode_switch:
      printf("Mode_switch");
      break;
    case UKey_Kanji:
      printf("Kanji");
      break;
    case UKey_Muhenkan:
      printf("Muhenkan");
      break;
    case UKey_Henkan_Mode:
      printf("Henkan_Mode");
      break;
    case UKey_Romaji:
      printf("romaji");
      break;
    case UKey_Hiragana:
      printf("hiragana");
      break;
    case UKey_Katakana:
      printf("katakana");
      break;
    case UKey_Hiragana_Katakana:
      printf("hiragana-katakana");
      break;
    case UKey_Zenkaku:
      printf("zenkaku");
      break;
    case UKey_Hankaku:
      printf("hankaku");
      break;
    case UKey_Zenkaku_Hankaku:
      printf("zenkaku-hankaku");
      break;
    case UKey_Touroku:
      printf("touroku");
      break;
    case UKey_Massyo:
      printf("massyo");
      break;
    case UKey_Kana_Lock:
      printf("kana-lock");
      break;
    case UKey_Kana_Shift:
      printf("kana-shift");
      break;
    case UKey_Eisu_Shift:
      printf("eisu-shift");
      break;
    case UKey_Eisu_toggle:
      printf("eisu-toggle");
      break;
    case UKey_F1:
      printf("F1");
      break;
    case UKey_F2:
      printf("F2");
      break;
    case UKey_F3:
      printf("F3");
      break;
    case UKey_F4:
      printf("F4");
      break;
    case UKey_F5:
      printf("F5");
      break;
    case UKey_F6:
      printf("F6");
      break;
    case UKey_F7:
      printf("F7");
      break;
    case UKey_F8:
      printf("F8");
      break;
    case UKey_F9:
      printf("F9");
      break;
    case UKey_F10:
      printf("F10");
      break;
    case UKey_F11:
      printf("F11");
      break;
    case UKey_F12:
      printf("F12");
      break;
    case UKey_F13:
      printf("F13");
      break;
    case UKey_F14:
      printf("F14");
      break;
    case UKey_F15:
      printf("F15");
      break;
    case UKey_F16:
      printf("F16");
      break;
    case UKey_F17:
      printf("F17");
      break;
    case UKey_F18:
      printf("F18");
      break;
    case UKey_F19:
      printf("F19");
      break;
    case UKey_F20:
      printf("F20");
      break;
    case UKey_F21:
      printf("F21");
      break;
    case UKey_F22:
      printf("F22");
      break;
    case UKey_F23:
      printf("F23");
      break;
    case UKey_F24:
      printf("F24");
      break;
    case UKey_F25:
      printf("F25");
      break;
    case UKey_F26:
      printf("F26");
      break;
    case UKey_F27:
      printf("F27");
      break;
    case UKey_F28:
      printf("F28");
      break;
    case UKey_F29:
      printf("F29");
      break;
    case UKey_F30:
      printf("F30");
      break;
    case UKey_F31:
      printf("F31");
      break;
    case UKey_F32:
      printf("F32");
      break;
    case UKey_F33:
      printf("F33");
      break;
    case UKey_F34:
      printf("F34");
      break;
    case UKey_F35:
      printf("F35");
      break;
    case UKey_Private1:
      printf("Private1");
      break;
    case UKey_Private2:
      printf("Private2");
      break;
    case UKey_Private3:
      printf("Private3");
      break;
    case UKey_Private4:
      printf("Private4");
      break;
    case UKey_Private5:
      printf("Private5");
      break;
    case UKey_Private6:
      printf("Private6");
      break;
    case UKey_Private7:
      printf("Private7");
      break;
    case UKey_Private8:
      printf("Private8");
      break;
    case UKey_Private9:
      printf("Private9");
      break;
    case UKey_Private10:
      printf("Private10");
      break;
    case UKey_Private11:
      printf("Private11");
      break;
    case UKey_Private12:
      printf("Private12");
      break;
    case UKey_Private13:
      printf("Private13");
      break;
    case UKey_Private14:
      printf("Private14");
      break;
    case UKey_Private15:
      printf("Private15");
      break;
    case UKey_Private16:
      printf("Private16");
      break;
    case UKey_Private17:
      printf("Private17");
      break;
    case UKey_Private18:
      printf("Private18");
      break;
    case UKey_Private19:
      printf("Private19");
      break;
    case UKey_Private20:
      printf("Private20");
      break;
    case UKey_Private21:
      printf("Private21");
      break;
    case UKey_Private22:
      printf("Private22");
      break;
    case UKey_Private23:
      printf("Private23");
      break;
    case UKey_Private24:
      printf("Private24");
      break;
    case UKey_Private25:
      printf("Private25");
      break;
    case UKey_Private26:
      printf("Private26");
      break;
    case UKey_Private27:
      printf("Private27");
      break;
    case UKey_Private28:
      printf("Private28");
      break;
    case UKey_Private29:
      printf("Private29");
      break;
    case UKey_Private30:
      printf("Private30");
      break;
    case UKey_Shift_key:
      printf("Shift_key");
      break;
    case UKey_Control_key:
      printf("Control_key");
      break;
    case UKey_Alt_key:
      printf("Alt_key");
      break;
    case UKey_Meta_key:
      printf("Meta_key");
      break;
    case UKey_Super_key:
      printf("Super_key");
      break;
    case UKey_Hyper_key:
      printf("Hyper_key");
      break;
    }
  }
  printf("\"\r\n");
}

