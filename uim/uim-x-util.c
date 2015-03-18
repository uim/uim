/*

  Copyright (c) 2007-2013 uim Project http://code.google.com/p/uim/

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

#include <X11/Xlib.h>
#include <X11/keysym.h>

#include "uim-x-util.h"

int
uim_x_keysym2ukey(KeySym xkeysym)
{
    int ukey = UKey_Other;

    if (xkeysym < 128 && xkeysym >= 32)
	ukey = (int)(xkeysym);
    else if (xkeysym >= XK_F1 && xkeysym <= XK_F35)
	ukey = (int)(xkeysym - XK_F1 + UKey_F1);
    // GTK+ and Qt don't support dead_stroke yet
    else if (xkeysym >= XK_dead_grave && xkeysym <= XK_dead_horn)
	ukey = (int)(xkeysym - XK_dead_grave + UKey_Dead_Grave);
    else if (xkeysym >= XK_Kanji && xkeysym <= XK_Eisu_toggle)
	ukey = (int)(xkeysym - XK_Kanji + UKey_Kanji);
    else if (xkeysym >= XK_Hangul && xkeysym <= XK_Hangul_Special)
	ukey = (int)(xkeysym - XK_Hangul + UKey_Hangul);
    else if (xkeysym >= XK_kana_fullstop && xkeysym <= XK_semivoicedsound)
	ukey = (int)(xkeysym - XK_kana_fullstop + UKey_Kana_Fullstop);
    else {
	switch (xkeysym) {
	case XK_yen: ukey = UKey_Yen; break;
	case XK_BackSpace: ukey = UKey_Backspace; break;
	case XK_Delete: ukey = UKey_Delete; break;
	case XK_Insert: ukey = UKey_Insert; break;
	case XK_Escape: ukey = UKey_Escape; break;
	case XK_Tab:
	case XK_ISO_Left_Tab: ukey = UKey_Tab; break;
	case XK_Return: ukey = UKey_Return; break;
	case XK_Left: ukey = UKey_Left; break;
	case XK_Up: ukey = UKey_Up; break;
	case XK_Right: ukey = UKey_Right; break;
	case XK_Down: ukey = UKey_Down; break;
	case XK_Prior: ukey = UKey_Prior; break;
	case XK_Next: ukey = UKey_Next; break;
	case XK_Home: ukey = UKey_Home; break;
	case XK_End: ukey = UKey_End; break;
	case XK_Multi_key: ukey = UKey_Multi_key; break;
	case XK_Codeinput: ukey = UKey_Codeinput; break;
	case XK_SingleCandidate: ukey = UKey_SingleCandidate; break;
	case XK_MultipleCandidate: ukey = UKey_MultipleCandidate; break;
	case XK_PreviousCandidate: ukey = UKey_PreviousCandidate; break;
	case XK_Mode_switch: ukey = UKey_Mode_switch; break;
	case XK_Shift_L: ukey = UKey_Shift_key; break;
	case XK_Shift_R: ukey = UKey_Shift_key; break;
	case XK_Control_L: ukey = UKey_Control_key; break;
	case XK_Control_R: ukey = UKey_Control_key; break;
	case XK_Alt_L: ukey = UKey_Alt_key; break;
	case XK_Alt_R: ukey = UKey_Alt_key; break;
	case XK_Meta_L: ukey = UKey_Meta_key; break;
	case XK_Meta_R: ukey = UKey_Meta_key; break;
	case XK_Super_L: ukey = UKey_Super_key; break;
	case XK_Super_R: ukey = UKey_Super_key; break;
	case XK_Hyper_L: ukey = UKey_Hyper_key; break;
	case XK_Hyper_R: ukey = UKey_Hyper_key; break;
	case XK_Caps_Lock: ukey = UKey_Caps_Lock; break;
	case XK_Num_Lock: ukey = UKey_Num_Lock; break;
	case XK_Scroll_Lock: ukey = UKey_Scroll_Lock; break;
	default:
	    ukey = UKey_Other;
	}
    }

    return ukey;
}
