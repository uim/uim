/*

  Copyright (c) 2007-2013 uim Project https://github.com/uim/uim

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
 * A hack to distinguish Japanese kana_RO key from yen sign key (both
 * keys normally generates backslash on ASCII input). See [uim-en 11]
 * and the follow messages for the discussion.
 *
 * This hack assumes that the xmodmap for the Japanese kana keyboard
 * is defined as follows:
 * 
 * yen sign key: keycode X = backslash bar
 * kana_RO key:  keycode Y = backslash underscore
 *
 * And also assumes that Japanese kana_RO key is the only one key that
 * has 'backslash underscore' modmap.
 */

#include <config.h>

#include <X11/Xlib.h>
#include <X11/keysym.h>

#include "uim.h"
#include "uim-x-util.h"

enum KeySymIndex {
  UNMODIFIED_KEYSYM_INDEX = 0,
  SHIFTED_KEYSYM_INDEX = 1
};

static uim_bool is_japanese_keyboard;
static KeyCode kana_RO_keycode, yen_sign_keycode;


int
uim_x_kana_input_hack_translate_key(int ukey, KeyCode hardware_keycode)
{
  if (ukey == '\\'
      && is_japanese_keyboard
      && hardware_keycode == yen_sign_keycode
      && hardware_keycode != kana_RO_keycode)
  {
    ukey = UKey_Yen;
  }

  return ukey;
}

int
uim_x_kana_input_hack_filter_event(uim_context uc, XEvent *event)
{
  unsigned int keycode;
  int translated_key;
  KeySym keysym;

  if (event->type != KeyPress && event->type != KeyRelease)
    return UIM_FALSE;

  /* Only unmodified keys are translated. */
  if (!event->xkey.state) {
    keycode = event->xkey.keycode;
    keysym = XLookupKeysym(&event->xkey, UNMODIFIED_KEYSYM_INDEX);
    translated_key = uim_x_kana_input_hack_translate_key(keysym, keycode);

    if (translated_key == UKey_Yen) {
      int not_filtered;

      if (event->type == KeyPress)
	not_filtered = uim_press_key(uc, translated_key, 0);
      else
	not_filtered = uim_release_key(uc, translated_key, 0);

      if (!not_filtered)
	return UIM_TRUE;
    }
  }
  return UIM_FALSE;
}

void
uim_x_kana_input_hack_init(Display *display)
{
  int min_keycode, max_keycode, keysyms_per_keycode, keycode_count, i;
  KeySym *map, *syms, unmodified, shifted;

  /* To allow refreshing keyboard encoding configuration by call this
   * function again, the global variables are explicitly initialized
   * with zero here. */
  is_japanese_keyboard = UIM_FALSE;
  kana_RO_keycode = 0;

  XDisplayKeycodes(display, &min_keycode, &max_keycode);
  keycode_count = max_keycode - min_keycode + 1;
  map = XGetKeyboardMapping(display,
			    min_keycode, keycode_count, &keysyms_per_keycode);

  if (keysyms_per_keycode >= SHIFTED_KEYSYM_INDEX + 1) {
    for (i = 0, syms = map;
	 i < keycode_count;
	 i++, syms += keysyms_per_keycode)
    {
      unmodified = syms[UNMODIFIED_KEYSYM_INDEX];
      shifted = syms[SHIFTED_KEYSYM_INDEX];

      /* Assumes that Japanese kana_RO key is the only one key that
       * has 'backslash underscore' modmap. */
      if (unmodified == XK_backslash) {
	if (shifted == XK_underscore) {
	  is_japanese_keyboard = UIM_TRUE;
	  kana_RO_keycode = min_keycode + i;
	} else if (shifted == XK_bar) {
	  yen_sign_keycode = min_keycode + i;
	}
      }
    }
  }

  XFree(map);
}
