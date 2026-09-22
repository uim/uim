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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <xkbcommon/xkbcommon-keysyms.h>

#include "uim-wayland.h"

/*
 * xkbcommon keysyms have the same values as X11 keysyms, so this is
 * the same table as convert_key_event() in gtk4/immodule.
 */
static int
keysym_to_ukey(xkb_keysym_t sym)
{
  if (sym < 256)
    return (int)sym;
  if (sym >= XKB_KEY_F1 && sym <= XKB_KEY_F35)
    return sym - XKB_KEY_F1 + UKey_F1;
  if (sym >= XKB_KEY_KP_0 && sym <= XKB_KEY_KP_9)
    return sym - XKB_KEY_KP_0 + UKey_0;
  if (sym >= XKB_KEY_dead_grave && sym <= XKB_KEY_dead_horn)
    return sym - XKB_KEY_dead_grave + UKey_Dead_Grave;
  if (sym >= XKB_KEY_Kanji && sym <= XKB_KEY_Eisu_toggle)
    return sym - XKB_KEY_Kanji + UKey_Kanji;
  if (sym >= XKB_KEY_Hangul && sym <= XKB_KEY_Hangul_Special)
    return sym - XKB_KEY_Hangul + UKey_Hangul;
  if (sym >= XKB_KEY_kana_fullstop && sym <= XKB_KEY_semivoicedsound)
    return sym - XKB_KEY_kana_fullstop + UKey_Kana_Fullstop;

  switch (sym) {
  case XKB_KEY_BackSpace:
    return UKey_Backspace;
  case XKB_KEY_Delete:
    return UKey_Delete;
  case XKB_KEY_Insert:
    return UKey_Insert;
  case XKB_KEY_Escape:
    return UKey_Escape;
  case XKB_KEY_Tab:
  case XKB_KEY_ISO_Left_Tab:
    return UKey_Tab;
  case XKB_KEY_Return:
    return UKey_Return;
  case XKB_KEY_Left:
    return UKey_Left;
  case XKB_KEY_Up:
    return UKey_Up;
  case XKB_KEY_Right:
    return UKey_Right;
  case XKB_KEY_Down:
    return UKey_Down;
  case XKB_KEY_Prior:
    return UKey_Prior;
  case XKB_KEY_Next:
    return UKey_Next;
  case XKB_KEY_Home:
    return UKey_Home;
  case XKB_KEY_End:
    return UKey_End;
  case XKB_KEY_Multi_key:
    return UKey_Multi_key;
  case XKB_KEY_Codeinput:
    return UKey_Codeinput;
  case XKB_KEY_SingleCandidate:
    return UKey_SingleCandidate;
  case XKB_KEY_MultipleCandidate:
    return UKey_MultipleCandidate;
  case XKB_KEY_PreviousCandidate:
    return UKey_PreviousCandidate;
  case XKB_KEY_Mode_switch:
    return UKey_Mode_switch;
  case XKB_KEY_Shift_L:
  case XKB_KEY_Shift_R:
    return UKey_Shift_key;
  case XKB_KEY_Control_L:
  case XKB_KEY_Control_R:
    return UKey_Control_key;
  case XKB_KEY_Alt_L:
  case XKB_KEY_Alt_R:
    return UKey_Alt_key;
  case XKB_KEY_Meta_L:
  case XKB_KEY_Meta_R:
    return UKey_Meta_key;
  case XKB_KEY_Super_L:
  case XKB_KEY_Super_R:
    return UKey_Super_key;
  case XKB_KEY_Hyper_L:
  case XKB_KEY_Hyper_R:
    return UKey_Hyper_key;
  case XKB_KEY_Caps_Lock:
    return UKey_Caps_Lock;
  case XKB_KEY_Num_Lock:
    return UKey_Num_Lock;
  case XKB_KEY_Scroll_Lock:
    return UKey_Scroll_Lock;
  default:
    return UKey_Other;
  }
}

static bool
mod_active(struct xkb_state *state, const char *name)
{
  return xkb_state_mod_name_is_active(state, name,
                                      XKB_STATE_MODS_EFFECTIVE) > 0;
}

void
uim_wayland_convert_key(xkb_keysym_t sym,
                        struct xkb_state *state,
                        int *ukey,
                        int *umod)
{
  *ukey = keysym_to_ukey(sym);

  *umod = 0;
  if (!state)
    return;
  if (mod_active(state, XKB_MOD_NAME_SHIFT))
    *umod |= UMod_Shift;
  if (mod_active(state, XKB_MOD_NAME_CTRL))
    *umod |= UMod_Control;
  if (mod_active(state, XKB_MOD_NAME_ALT))
    *umod |= UMod_Alt;
  if (mod_active(state, XKB_MOD_NAME_LOGO))
    *umod |= UMod_Super;
  if (mod_active(state, "Hyper"))
    *umod |= UMod_Hyper;
}
