/*
  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or
  without modification, are permitted provided that the
  following conditions are met:

  1. Redistributions of source code must retain the above
     copyright notice, this list of conditions and the
     following disclaimer.
  2. Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the
     following disclaimer in the documentation and/or other
     materials provided with the distribution.
  3. Neither the name of authors nor the names of its
     contributors may be used to endorse or promote products
     derived from this software without specific prior written
     permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "key.h"

/*
  convert abbreviated key description to generic Emacs-style key description
  (for XEmacs)
*/
void
convert_keyname_a2e(char *keyname, const char *name, size_t keyname_len)
{
  if (strcmp(name, "BS") == 0)
	strlcpy(keyname, "backspace", keyname_len);
  else if (strcmp(name, "TAB") == 0)
	strlcpy(keyname, "tab", keyname_len);
  else if (strcmp(name, "RET") == 0)
	strlcpy(keyname, "return", keyname_len);
  else if (strcmp(name, "ESC") == 0)
	strlcpy(keyname, "escape", keyname_len);
  else if (strcmp(name, "DEL") == 0)
	strlcpy(keyname, "delete", keyname_len);
  else if (strcmp(name, "DEL") == 0)
	strlcpy(keyname, "delete", keyname_len);
  else if (strcmp(name, "SPC") == 0)
	strlcpy(keyname, "space", keyname_len);
  else
	strlcpy(keyname, name, keyname_len);
}


/*
  convert generic Emacs-style key description to Uim-style key description
*/
enum UKey
convert_keyname_e2u(const char *keyname)
{
  if (strcmp("space", keyname) == 0)
	return ' ';  /* "space" has no Uim-style key description */
  else if (strcmp("backspace", keyname) == 0)
	return UKey_Backspace;
  else if (strcmp("tab", keyname) == 0)
	return UKey_Tab;
  else if (strcmp("return", keyname) == 0)
	return UKey_Return;
  else if (strcmp("escape", keyname) == 0)
	return UKey_Escape;
  else if (strcmp("prior", keyname) == 0)
	return UKey_Prior;
  else if (strcmp("next", keyname) == 0)
	return UKey_Next;
  else if (strcmp("end", keyname) == 0)
	return UKey_End;
  else if (strcmp("home", keyname) == 0)
	return UKey_Home;
  else if (strcmp("left", keyname) == 0)
	return UKey_Left;
  else if (strcmp("up", keyname) == 0)
	return UKey_Up;
  else if (strcmp("right", keyname) == 0)
	return UKey_Right;
  else if (strcmp("down", keyname) == 0)
	return UKey_Down;
  else if (strcmp("insert", keyname) == 0)
	return UKey_Insert;
  else if (strcmp("delete", keyname) == 0)
	return UKey_Delete;
  else if (strcmp("kanji", keyname) == 0)
	return UKey_Kanji;
  else if (strcmp("muhenkan", keyname) == 0)
	return UKey_Muhenkan;
  else if (strcmp("henkan", keyname) == 0)
	return UKey_Henkan_Mode;
  else if (strcmp("romaji", keyname) == 0)
	return UKey_Romaji;
  else if (strcmp("hiragana", keyname) == 0)
	return UKey_Hiragana;
  else if (strcmp("katakana", keyname) == 0)
	return UKey_Katakana;
  else if (strcmp("hiragana-katakana", keyname) == 0)
	return UKey_Hiragana_Katakana;
  else if (strcmp("zenkaku", keyname) == 0)
	return UKey_Zenkaku;
  else if (strcmp("hankaku", keyname) == 0)
	return UKey_Hankaku;
  else if (strcmp("zenkaku-hankaku", keyname) == 0)
	return UKey_Zenkaku_Hankaku;
  else if (strcmp("touroku", keyname) == 0)
	return UKey_Touroku;
  else if (strcmp("massyo", keyname) == 0)
	return UKey_Massyo;
  else if (strcmp("kana-lock", keyname) == 0)
	return UKey_Kana_Lock;
  else if (strcmp("kana-shift", keyname) == 0)
	return UKey_Kana_Shift;
  else if (strcmp("eisu-shift", keyname) == 0)
	return UKey_Eisu_Shift;
  else if (strcmp("eisu-toggle", keyname) == 0)
	return UKey_Eisu_toggle;
  else if (strcmp("Caps_Lock", keyname) == 0
           || strcmp("caps-lock", keyname) == 0)
    return UKey_Caps_Lock;
  else if (strcmp("Num_Lock", keyname) == 0
           || strcmp("num-lock", keyname) == 0)
    return UKey_Num_Lock;
  else if (strcmp("Scroll_Lock", keyname) == 0
           || strcmp("scroll-lock", keyname) == 0)
    return UKey_Scroll_Lock;
  else if (strcmp("yen", keyname) == 0)
    return UKey_Yen;
  else if (strcmp("Multi_key", keyname) == 0
           || strcmp("multi-key", keyname) == 0)
    return UKey_Multi_key;
  else if (strcmp("Codeinput", keyname) == 0
           || strcmp("codeinput", keyname) == 0)
    return UKey_Codeinput;
  else if (strcmp("SingleCandidate", keyname) == 0
           || strcmp("singlecandidate", keyname) == 0)
    return UKey_SingleCandidate;
  else if (strcmp("MultipleCandidate", keyname) == 0
           || strcmp("multiplecandidate", keyname) == 0)
    return UKey_MultipleCandidate;
  else if (strcmp("PreviousCandidate", keyname) == 0
           || strcmp("previouscandidate", keyname) == 0)
    return UKey_PreviousCandidate;
  else if (strcmp("Mode_switch", keyname) == 0
           || strcmp("mode-switch", keyname) == 0)
	return UKey_Mode_switch;
  else if (strncmp("f", keyname, strlen("f")) == 0) {
	keyname += strlen("f");
	if (strcmp("1", keyname) == 0)
	  return UKey_F1;
	else if (strcmp("2", keyname) == 0)
	  return UKey_F2;
	else if (strcmp("3", keyname) == 0)
	  return UKey_F3;
	else if (strcmp("4", keyname) == 0)
	  return UKey_F4;
	else if (strcmp("5", keyname) == 0)
	  return UKey_F5;
	else if (strcmp("6", keyname) == 0)
	  return UKey_F6;
	else if (strcmp("7", keyname) == 0)
	  return UKey_F7;
	else if (strcmp("8", keyname) == 0)
	  return UKey_F8;
	else if (strcmp("9", keyname) == 0)
	  return UKey_F9;
	else if (strcmp("10", keyname) == 0)
	  return UKey_F10;
	else if (strcmp("11", keyname) == 0)
	  return UKey_F11;
	else if (strcmp("12", keyname) == 0)
	  return UKey_F12;
	else if (strcmp("13", keyname) == 0)
	  return UKey_F13;
	else if (strcmp("14", keyname) == 0)
	  return UKey_F14;
	else if (strcmp("15", keyname) == 0)
	  return UKey_F15;
	else if (strcmp("16", keyname) == 0)
	  return UKey_F16;
	else if (strcmp("17", keyname) == 0)
	  return UKey_F17;
	else if (strcmp("18", keyname) == 0)
	  return UKey_F18;
	else if (strcmp("19", keyname) == 0)
	  return UKey_F19;
	else if (strcmp("20", keyname) == 0)
	  return UKey_F20;
	else if (strcmp("21", keyname) == 0)
	  return UKey_F21;
	else if (strcmp("22", keyname) == 0)
	  return UKey_F22;
	else if (strcmp("23", keyname) == 0)
	  return UKey_F23;
	else if (strcmp("24", keyname) == 0)
	  return UKey_F24;
	else if (strcmp("25", keyname) == 0)
	  return UKey_F25;
	else if (strcmp("26", keyname) == 0)
	  return UKey_F26;
	else if (strcmp("27", keyname) == 0)
	  return UKey_F27;
	else if (strcmp("28", keyname) == 0)
	  return UKey_F28;
	else if (strcmp("29", keyname) == 0)
	  return UKey_F29;
	else if (strcmp("30", keyname) == 0)
	  return UKey_F30;
	else if (strcmp("31", keyname) == 0)
	  return UKey_F31;
	else if (strcmp("32", keyname) == 0)
	  return UKey_F32;
	else if (strcmp("33", keyname) == 0)
	  return UKey_F33;
	else if (strcmp("34", keyname) == 0)
	  return UKey_F34;
	else if (strcmp("35", keyname) == 0)
	  return UKey_F35;
  } else if (strcmp("Hangul", keyname) == 0)
	  return UKey_Hangul;
  else if (strncmp("Hangul_", keyname, strlen("Hangul_")) == 0) {
	keyname += strlen("Hangul_");
	if (strcmp("Start", keyname) == 0)
	  return UKey_Hangul_Start;
	else if (strcmp("End", keyname) == 0)
	  return UKey_Hangul_End;
	else if (strcmp("Hanja", keyname) == 0)
	  return UKey_Hangul_Hanja;
	else if (strcmp("Jamo", keyname) == 0)
	  return UKey_Hangul_Jamo;
	else if (strcmp("Romaja", keyname) == 0)
	  return UKey_Hangul_Romaja;
	else if (strcmp("Codeinput", keyname) == 0)
	  return UKey_Hangul_Codeinput;
	else if (strcmp("Jeonja", keyname) == 0)
	  return UKey_Hangul_Jeonja;
	else if (strcmp("Banja", keyname) == 0)
	  return UKey_Hangul_Banja;
	else if (strcmp("PreHanja", keyname) == 0)
	  return UKey_Hangul_PreHanja;
	else if (strcmp("PostHanja", keyname) == 0)
	  return UKey_Hangul_PostHanja;
	else if (strcmp("SingleCandidate", keyname) == 0)
	  return UKey_Hangul_SingleCandidate;
	else if (strcmp("MultipleCandidate", keyname) == 0)
	  return UKey_Hangul_MultipleCandidate;
	else if (strcmp("PreviousCandidate", keyname) == 0)
	  return UKey_Hangul_PreviousCandidate;
	else if (strcmp("Special", keyname) == 0)
	  return UKey_Hangul_Special;
  } else if (strncmp("hangul-", keyname, strlen("hangul-")) == 0) {
    keyname += strlen("hangul-");
	if (strcmp("start", keyname) == 0)
	  return UKey_Hangul_Start;
	else if (strcmp("end", keyname) == 0)
	  return UKey_Hangul_End;
	else if (strcmp("hanja", keyname) == 0)
	  return UKey_Hangul_Hanja;
	else if (strcmp("jamo", keyname) == 0)
	  return UKey_Hangul_Jamo;
	else if (strcmp("romaja", keyname) == 0)
	  return UKey_Hangul_Romaja;
	else if (strcmp("codeinput", keyname) == 0)
	  return UKey_Hangul_Codeinput;
	else if (strcmp("jeonja", keyname) == 0)
	  return UKey_Hangul_Jeonja;
	else if (strcmp("banja", keyname) == 0)
	  return UKey_Hangul_Banja;
	else if (strcmp("prehanja", keyname) == 0)
	  return UKey_Hangul_PreHanja;
	else if (strcmp("posthanja", keyname) == 0)
	  return UKey_Hangul_PostHanja;
	else if (strcmp("singlecandidate", keyname) == 0)
	  return UKey_Hangul_SingleCandidate;
	else if (strcmp("multiplecandidate", keyname) == 0)
	  return UKey_Hangul_MultipleCandidate;
	else if (strcmp("previouscandidate", keyname) == 0)
	  return UKey_Hangul_PreviousCandidate;
	else if (strcmp("special", keyname) == 0)
	  return UKey_Hangul_Special;
  } else if (strncmp("dead-", keyname, strlen("dead-")) == 0) {
	keyname += strlen("dead-");
	if (strcmp("grave", keyname) == 0)
	  return UKey_Dead_Grave;
	else if (strcmp("acute", keyname) == 0)
	  return UKey_Dead_Acute;
	else if (strcmp("circumflex", keyname) == 0)
	  return UKey_Dead_Circumflex;
	else if (strcmp("tilde", keyname) == 0)
	  return UKey_Dead_Tilde;
	else if (strcmp("macron", keyname) == 0)
	  return UKey_Dead_Macron;
	else if (strcmp("breve", keyname) == 0)
	  return UKey_Dead_Breve;
	else if (strcmp("abovedot", keyname) == 0)
	  return UKey_Dead_Abovedot;
	else if (strcmp("diaeresis", keyname) == 0)
	  return UKey_Dead_Diaeresis;
	else if (strcmp("abovering", keyname) == 0)
	  return UKey_Dead_Abovering;
	else if (strcmp("doubleacute", keyname) == 0)
	  return UKey_Dead_Doubleacute;
	else if (strcmp("caron", keyname) == 0)
	  return UKey_Dead_Caron;
	else if (strcmp("cedilla", keyname) == 0)
	  return UKey_Dead_Cedilla;
	else if (strcmp("ogonek", keyname) == 0)
	  return UKey_Dead_Ogonek;
	else if (strcmp("iota", keyname) == 0)
	  return UKey_Dead_Iota;
	else if (strcmp("voicedsound", keyname) == 0)
	  return UKey_Dead_VoicedSound;
	else if (strcmp("semivoicedsound", keyname) == 0)
	  return UKey_Dead_SemivoicedSound;
	else if (strcmp("belowdot", keyname) == 0)
	  return UKey_Dead_Belowdot;
	else if (strcmp("hook", keyname) == 0)
	  return UKey_Dead_Hook;
	else if (strcmp("horn", keyname) == 0)
	  return UKey_Dead_Horn;
  } else if (strncmp("kana_", keyname, strlen("kana_")) == 0) {
	keyname += strlen("kana_");
    if (strcmp("fullstop", keyname) == 0)
	  return UKey_Kana_Fullstop;
	else if (strcmp("openingbracket", keyname) == 0)
	  return UKey_Kana_OpeningBracket;
	else if (strcmp("closingbracket", keyname) == 0)
	  return UKey_Kana_ClosingBracket;
	else if (strcmp("comma", keyname) == 0)
	  return UKey_Kana_Comma;
	else if (strcmp("conjunctive", keyname) == 0)
	  return UKey_Kana_Conjunctive;
	else if (strcmp("WO", keyname) == 0)
	  return UKey_Kana_WO;
	else if (strcmp("a", keyname) == 0)
	  return UKey_Kana_a;
	else if (strcmp("i", keyname) == 0)
	  return UKey_Kana_i;
	else if (strcmp("u", keyname) == 0)
	  return UKey_Kana_u;
	else if (strcmp("e", keyname) == 0)
	  return UKey_Kana_e;
	else if (strcmp("o", keyname) == 0)
	  return UKey_Kana_o;
	else if (strcmp("ya", keyname) == 0)
	  return UKey_Kana_ya;
	else if (strcmp("yu", keyname) == 0)
	  return UKey_Kana_yu;
	else if (strcmp("yo", keyname) == 0)
	  return UKey_Kana_yo;
	else if (strcmp("tsu", keyname) == 0)
	  return UKey_Kana_tsu;
	else if (strcmp("A", keyname) == 0)
	  return UKey_Kana_A;
	else if (strcmp("I", keyname) == 0)
	  return UKey_Kana_I;
	else if (strcmp("U", keyname) == 0)
	  return UKey_Kana_U;
	else if (strcmp("E", keyname) == 0)
	  return UKey_Kana_E;
	else if (strcmp("O", keyname) == 0)
	  return UKey_Kana_O;
	else if (strcmp("KA", keyname) == 0)
	  return UKey_Kana_KA;
	else if (strcmp("KI", keyname) == 0)
	  return UKey_Kana_KI;
	else if (strcmp("KU", keyname) == 0)
	  return UKey_Kana_KU;
	else if (strcmp("KE", keyname) == 0)
	  return UKey_Kana_KE;
	else if (strcmp("KO", keyname) == 0)
	  return UKey_Kana_KO;
	else if (strcmp("SA", keyname) == 0)
	  return UKey_Kana_SA;
	else if (strcmp("SHI", keyname) == 0)
	  return UKey_Kana_SHI;
	else if (strcmp("SU", keyname) == 0)
	  return UKey_Kana_SU;
	else if (strcmp("SE", keyname) == 0)
	  return UKey_Kana_SE;
	else if (strcmp("SO", keyname) == 0)
	  return UKey_Kana_SO;
	else if (strcmp("TA", keyname) == 0)
	  return UKey_Kana_TA;
	else if (strcmp("CHI", keyname) == 0)
	  return UKey_Kana_CHI;
	else if (strcmp("TSU", keyname) == 0)
	  return UKey_Kana_TSU;
	else if (strcmp("TE", keyname) == 0)
	  return UKey_Kana_TE;
	else if (strcmp("TO", keyname) == 0)
	  return UKey_Kana_TO;
	else if (strcmp("NA", keyname) == 0)
	  return UKey_Kana_NA;
	else if (strcmp("NI", keyname) == 0)
	  return UKey_Kana_NI;
	else if (strcmp("NU", keyname) == 0)
	  return UKey_Kana_NU;
	else if (strcmp("NE", keyname) == 0)
	  return UKey_Kana_NE;
	else if (strcmp("NO", keyname) == 0)
	  return UKey_Kana_NO;
	else if (strcmp("HA", keyname) == 0)
	  return UKey_Kana_HA;
	else if (strcmp("HI", keyname) == 0)
	  return UKey_Kana_HI;
	else if (strcmp("FU", keyname) == 0)
	  return UKey_Kana_FU;
	else if (strcmp("HE", keyname) == 0)
	  return UKey_Kana_HE;
	else if (strcmp("HO", keyname) == 0)
	  return UKey_Kana_HO;
	else if (strcmp("MA", keyname) == 0)
	  return UKey_Kana_MA;
	else if (strcmp("MI", keyname) == 0)
	  return UKey_Kana_MI;
	else if (strcmp("MU", keyname) == 0)
	  return UKey_Kana_MU;
	else if (strcmp("ME", keyname) == 0)
	  return UKey_Kana_ME;
	else if (strcmp("MO", keyname) == 0)
	  return UKey_Kana_MO;
	else if (strcmp("YA", keyname) == 0)
	  return UKey_Kana_YA;
	else if (strcmp("YU", keyname) == 0)
	  return UKey_Kana_YU;
	else if (strcmp("YO", keyname) == 0)
	  return UKey_Kana_YO;
	else if (strcmp("RA", keyname) == 0)
	  return UKey_Kana_RA;
	else if (strcmp("RI", keyname) == 0)
	  return UKey_Kana_RI;
	else if (strcmp("RU", keyname) == 0)
	  return UKey_Kana_RU;
	else if (strcmp("RE", keyname) == 0)
	  return UKey_Kana_RE;
	else if (strcmp("RO", keyname) == 0)
	  return UKey_Kana_RO;
	else if (strcmp("WA", keyname) == 0)
	  return UKey_Kana_WA;
	else if (strcmp("N", keyname) == 0)
	  return UKey_Kana_N;
  } else if (strcmp("prolongedsound", keyname) == 0)
	  return UKey_Kana_ProlongedSound;
  else if (strcmp("voicedsound", keyname) == 0)
	return UKey_Kana_VoicedSound;
  else if (strcmp("semivoicedsound", keyname) == 0)
	return UKey_Kana_SemivoicedSound;
  else
	return UKey_Other;

  return UKey_Other;
}
