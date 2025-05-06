/*

  Copyright (c) 2006-2013 uim Project https://github.com/uim/uim

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

#include <config.h>

/*
 * copied from uim-key.c
 */

struct key_entry {
  int key;
  const char *str;
};

static struct key_entry key_tab[] = {
  {UKey_Yen, "yen"},
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
  {UKey_Insert, "insert"},
  {UKey_Multi_key, "Multi_key"},
  {UKey_Codeinput, "codeinput"},
  {UKey_SingleCandidate, "single-candidate"},
  {UKey_MultipleCandidate, "multiple-candidate"},
  {UKey_PreviousCandidate, "previous-candidate"},
  {UKey_Mode_switch, "Mode_switch"},
  {UKey_Kanji, "Kanji"},
  {UKey_Muhenkan, "Muhenkan"},
  {UKey_Henkan_Mode, "Henkan_Mode"},
  {UKey_Romaji, "romaji"},
  {UKey_Hiragana, "hiragana"},
  {UKey_Katakana, "katakana"},
  {UKey_Hiragana_Katakana, "hiragana-katakana"},
  {UKey_Zenkaku, "zenkaku"},
  {UKey_Hankaku, "hankaku"},
  {UKey_Zenkaku_Hankaku, "zenkaku-hankaku"},
  {UKey_Touroku, "touroku"},
  {UKey_Massyo, "massyo"},
  {UKey_Kana_Lock, "kana-lock"},
  {UKey_Kana_Shift, "kana-shift"},
  {UKey_Eisu_Shift, "eisu-shift"},
  {UKey_Eisu_toggle, "eisu-toggle"},

  {UKey_Hangul, "hangul"},
  {UKey_Hangul_Start, "hangul-start"},
  {UKey_Hangul_End, "hangul-end"},
  {UKey_Hangul_Hanja, "hangul-hanja"},
  {UKey_Hangul_Jamo, "hangul-jamo"},
  {UKey_Hangul_Romaja, "hangul-romaja"},
  {UKey_Hangul_Codeinput, "hangul-codeinput"},
  {UKey_Hangul_Jeonja, "hangul-jeonja"},
  {UKey_Hangul_Banja, "hangul-banja"},
  {UKey_Hangul_PreHanja, "hangul-prehanja"},
  {UKey_Hangul_PostHanja, "hangul-posthanja"},
  {UKey_Hangul_SingleCandidate, "hangul-single-candidate"},
  {UKey_Hangul_MultipleCandidate, "hangul-multiple-candidate"},
  {UKey_Hangul_PreviousCandidate, "hangul-previous-candidate"},
  {UKey_Hangul_Special, "hangul-special"},

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

  {UKey_Dead_Grave, "dead-grave"},
  {UKey_Dead_Acute, "dead-acute"},
  {UKey_Dead_Circumflex, "dead-circumflex"},
  {UKey_Dead_Tilde, "dead-tilde"},
  {UKey_Dead_Macron, "dead-macron"},
  {UKey_Dead_Breve, "dead-breve"},
  {UKey_Dead_Abovedot, "dead-abovedot"},
  {UKey_Dead_Diaeresis, "dead-diaeresis"},
  {UKey_Dead_Abovering, "dead-abovering"},
  {UKey_Dead_Doubleacute, "dead-doubleacute"},
  {UKey_Dead_Caron, "dead-caron"},
  {UKey_Dead_Cedilla, "dead-cedilla"},
  {UKey_Dead_Ogonek, "dead-ogonek"},
  {UKey_Dead_Iota, "dead-iota"},
  {UKey_Dead_VoicedSound, "dead-voiced-sound"},
  {UKey_Dead_SemivoicedSound, "dead-semivoiced-sound"},
  {UKey_Dead_Belowdot, "dead-belowdot"},
  {UKey_Dead_Hook, "dead-hook"},
  {UKey_Dead_Horn, "dead-horn"},

  {UKey_Kana_Fullstop, "kana-fullstop"},
  {UKey_Kana_OpeningBracket, "kana-opening-bracket"},
  {UKey_Kana_ClosingBracket, "kana-closing-bracket"},
  {UKey_Kana_Comma, "kana-comma"},
  {UKey_Kana_Conjunctive, "kana-conjunctive"},
  {UKey_Kana_WO, "kana-WO"},
  {UKey_Kana_a, "kana-a"},
  {UKey_Kana_i, "kana-i"},
  {UKey_Kana_u, "kana-u"},
  {UKey_Kana_e, "kana-e"},
  {UKey_Kana_o, "kana-o"},
  {UKey_Kana_ya, "kana-ya"},
  {UKey_Kana_yu, "kana-yu"},
  {UKey_Kana_yo, "kana-yo"},
  {UKey_Kana_tsu, "kana-tsu"},
  {UKey_Kana_ProlongedSound, "kana-prolonged-sound"},
  {UKey_Kana_A, "kana-A"},
  {UKey_Kana_I, "kana-I"},
  {UKey_Kana_U, "kana-U"},
  {UKey_Kana_E, "kana-E"},
  {UKey_Kana_O, "kana-O"},
  {UKey_Kana_KA, "kana-KA"},
  {UKey_Kana_KI, "kana-KI"},
  {UKey_Kana_KU, "kana-KU"},
  {UKey_Kana_KE, "kana-KE"},
  {UKey_Kana_KO, "kana-KO"},
  {UKey_Kana_SA, "kana-SA"},
  {UKey_Kana_SHI, "kana-SHI"},
  {UKey_Kana_SU, "kana-SU"},
  {UKey_Kana_SE, "kana-SE"},
  {UKey_Kana_SO, "kana-SO"},
  {UKey_Kana_TA, "kana-TA"},
  {UKey_Kana_CHI, "kana-CHI"},
  {UKey_Kana_TSU, "kana-TSU"},
  {UKey_Kana_TE, "kana-TE"},
  {UKey_Kana_TO, "kana-TO"},
  {UKey_Kana_NA, "kana-NA"},
  {UKey_Kana_NI, "kana-NI"},
  {UKey_Kana_NU, "kana-NU"},
  {UKey_Kana_NE, "kana-NE"},
  {UKey_Kana_NO, "kana-NO"},
  {UKey_Kana_HA, "kana-HA"},
  {UKey_Kana_HI, "kana-HI"},
  {UKey_Kana_FU, "kana-FU"},
  {UKey_Kana_HE, "kana-HE"},
  {UKey_Kana_HO, "kana-HO"},
  {UKey_Kana_MA, "kana-MA"},
  {UKey_Kana_MI, "kana-MI"},
  {UKey_Kana_MU, "kana-MU"},
  {UKey_Kana_ME, "kana-ME"},
  {UKey_Kana_MO, "kana-MO"},
  {UKey_Kana_YA, "kana-YA"},
  {UKey_Kana_YU, "kana-YU"},
  {UKey_Kana_YO, "kana-YO"},
  {UKey_Kana_RA, "kana-RA"},
  {UKey_Kana_RI, "kana-RI"},
  {UKey_Kana_RU, "kana-RU"},
  {UKey_Kana_RE, "kana-RE"},
  {UKey_Kana_RO, "kana-RO"},
  {UKey_Kana_WA, "kana-WA"},
  {UKey_Kana_N, "kana-N"},
  {UKey_Kana_VoicedSound, "kana-voiced-sound"},
  {UKey_Kana_SemivoicedSound, "kana-semivoiced-sound"},
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

  {UKey_Caps_Lock, "caps-lock"},
  {UKey_Num_Lock, "num-lock"},
  {UKey_Scroll_Lock, "scroll-lock"},
  /*  {UKey_Other, "other"},*/
  {0, 0}
};

static const char *
uim_pref_get_keysym(int ukey)
{
  int i;
  const char *sym = NULL;

  for (i = 0; key_tab[i].key; i++) {
    if (key_tab[i].key == ukey) {
      sym = key_tab[i].str;
      break;
    }
  }
  return sym;
}
