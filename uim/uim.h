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

/* API and ABI is unstable */
#ifndef UIM_H
#define UIM_H

#include <stdlib.h>

#include "version.h"

#ifdef __cplusplus
extern "C" {
#endif

#define UIM_VERSION_REQUIRE(major, minor, patchlevel)			     \
  ((major) < UIM_VERSION_MAJOR						     \
   || ((major) == UIM_VERSION_MAJOR && (minor) < UIM_VERSION_MINOR)	     \
   || ((major) == UIM_VERSION_MAJOR && (minor) == UIM_VERSION_MINOR	     \
       && (patchlevel) <= UIM_VERSION_PATCHLEVEL))

#ifndef UIM_BOOL_DEFINED
/*
 * A boolean type for uim to explicitly indicate intention about values
 *
 *                           *** IMPORTANT ***
 *
 * Do not test a value with (val == UIM_TRUE). The UIM_TRUE is only A TYPICAL
 * VALUE FOR TRUE. Use (val) or (val != UIM_FALSE) instead.
 *
 */
typedef int uim_bool;

#define UIM_FALSE 0
#define UIM_TRUE 1

#define UIM_BOOL_DEFINED 1
#endif /* UIM_BOOL_DEFINED */


typedef struct uim_context_ *uim_context;

typedef struct uim_candidate_ *uim_candidate;

enum UKey {
  UKey_0 = 48,
  UKey_1 = 49,
  UKey_2 = 50,
  UKey_3 = 51,
  UKey_4 = 52,
  UKey_5 = 53,
  UKey_6 = 54,
  UKey_7 = 55,
  UKey_8 = 56,
  UKey_9 = 57,

  /* non-ASCII Latin-1 characters */
  UKey_nobreakspace   = 0x00a0,
  UKey_exclamdown     = 0x00a1,
  UKey_cent           = 0x00a2,
  UKey_sterling       = 0x00a3,
  UKey_currency       = 0x00a4,
  UKey_Yen            = 0x00a5,	/* == 165 */
  UKey_brokenbar      = 0x00a6,
  UKey_section        = 0x00a7,
  UKey_diaeresis      = 0x00a8,
  UKey_copyright      = 0x00a9,
  UKey_ordfeminine    = 0x00aa,
  UKey_guillemotleft  = 0x00ab,
  UKey_notsign        = 0x00ac,
  UKey_hyphen         = 0x00ad,
  UKey_registered     = 0x00ae,
  UKey_macron         = 0x00af,
  UKey_degree         = 0x00b0,
  UKey_plusminus      = 0x00b1,
  UKey_twosuperior    = 0x00b2,
  UKey_threesuperior  = 0x00b3,
  UKey_acute          = 0x00b4,
  UKey_mu             = 0x00b5,
  UKey_paragraph      = 0x00b6,
  UKey_periodcentered = 0x00b7,
  UKey_cedilla        = 0x00b8,
  UKey_onesuperior    = 0x00b9,
  UKey_masculine      = 0x00ba,
  UKey_guillemotright = 0x00bb,
  UKey_onequarter     = 0x00bc,
  UKey_onehalf        = 0x00bd,
  UKey_threequarters  = 0x00be,
  UKey_questiondown   = 0x00bf,
  UKey_Agrave         = 0x00c0,
  UKey_Aacute         = 0x00c1,
  UKey_Acircumflex    = 0x00c2,
  UKey_Atilde         = 0x00c3,
  UKey_Adiaeresis     = 0x00c4,
  UKey_Aring          = 0x00c5,
  UKey_AE             = 0x00c6,
  UKey_Ccedilla       = 0x00c7,
  UKey_Egrave         = 0x00c8,
  UKey_Eacute         = 0x00c9,
  UKey_Ecircumflex    = 0x00ca,
  UKey_Ediaeresis     = 0x00cb,
  UKey_Igrave         = 0x00cc,
  UKey_Iacute         = 0x00cd,
  UKey_Icircumflex    = 0x00ce,
  UKey_Idiaeresis     = 0x00cf,
  UKey_ETH            = 0x00d0,
  UKey_Eth            = 0x00d0,
  UKey_Ntilde         = 0x00d1,
  UKey_Ograve         = 0x00d2,
  UKey_Oacute         = 0x00d3,
  UKey_Ocircumflex    = 0x00d4,
  UKey_Otilde         = 0x00d5,
  UKey_Odiaeresis     = 0x00d6,
  UKey_multiply       = 0x00d7,
  UKey_Oslash         = 0x00d8,
  UKey_Ooblique       = 0x00d8,
  UKey_Ugrave         = 0x00d9,
  UKey_Uacute         = 0x00da,
  UKey_Ucircumflex    = 0x00db,
  UKey_Udiaeresis     = 0x00dc,
  UKey_Yacute         = 0x00dd,
  UKey_THORN          = 0x00de,
  UKey_Thorn          = 0x00de,
  UKey_ssharp         = 0x00df,
  UKey_agrave         = 0x00e0,
  UKey_aacute         = 0x00e1,
  UKey_acircumflex    = 0x00e2,
  UKey_atilde         = 0x00e3,
  UKey_adiaeresis     = 0x00e4,
  UKey_aring          = 0x00e5,
  UKey_ae             = 0x00e6,
  UKey_ccedilla       = 0x00e7,
  UKey_egrave         = 0x00e8,
  UKey_eacute         = 0x00e9,
  UKey_ecircumflex    = 0x00ea,
  UKey_ediaeresis     = 0x00eb,
  UKey_igrave         = 0x00ec,
  UKey_iacute         = 0x00ed,
  UKey_icircumflex    = 0x00ee,
  UKey_idiaeresis     = 0x00ef,
  UKey_eth            = 0x00f0,
  UKey_ntilde         = 0x00f1,
  UKey_ograve         = 0x00f2,
  UKey_oacute         = 0x00f3,
  UKey_ocircumflex    = 0x00f4,
  UKey_otilde         = 0x00f5,
  UKey_odiaeresis     = 0x00f6,
  UKey_division       = 0x00f7,
  UKey_oslash         = 0x00f8,
  UKey_ooblique       = 0x00f8,
  UKey_ugrave         = 0x00f9,
  UKey_uacute         = 0x00fa,
  UKey_ucircumflex    = 0x00fb,
  UKey_udiaeresis     = 0x00fc,
  UKey_yacute         = 0x00fd,
  UKey_thorn          = 0x00fe,
  UKey_ydiaeresis     = 0x00ff,

  UKey_Escape = 256,
  UKey_Tab,
  UKey_Backspace,
  UKey_Delete,
  UKey_Insert,
  UKey_Return,
  UKey_Left,
  UKey_Up ,
  UKey_Right ,
  UKey_Down ,
  UKey_Prior , /* page up */
  UKey_Next , /* page down */
  UKey_Home,
  UKey_End,

  UKey_Multi_key, /* multi-key character compose */
  UKey_Codeinput,
  UKey_SingleCandidate,
  UKey_MultipleCandidate,
  UKey_PreviousCandidate,
  UKey_Mode_switch, /* charcter set switch */

  /* Japanese keyboard */
  UKey_Kanji, /* kanji, kanji convert */
  UKey_Muhenkan, /* cancel conversion */
  UKey_Henkan_Mode, /* start/stop conversion */
  UKey_Henkan = UKey_Henkan_Mode, /* alias for Henkan_Mode */
  UKey_Romaji,
  UKey_Hiragana,
  UKey_Katakana,
  UKey_Hiragana_Katakana, /* hiragana/katakana toggle */
  UKey_Zenkaku,
  UKey_Hankaku,
  UKey_Zenkaku_Hankaku, /* zenkaku/hankaku toggle */
  UKey_Touroku,
  UKey_Massyo,
  UKey_Kana_Lock,
  UKey_Kana_Shift,
  UKey_Eisu_Shift, /* alphanumeric shift */
  UKey_Eisu_toggle, /* alphanumeric toggle */

  /* Korean keyboard */
  UKey_Hangul,
  UKey_Hangul_Start,
  UKey_Hangul_End,
  UKey_Hangul_Hanja,
  UKey_Hangul_Jamo,
  UKey_Hangul_Romaja,
  UKey_Hangul_Codeinput,
  UKey_Hangul_Jeonja,
  UKey_Hangul_Banja,
  UKey_Hangul_PreHanja,
  UKey_Hangul_PostHanja,
  UKey_Hangul_SingleCandidate,
  UKey_Hangul_MultipleCandidate,
  UKey_Hangul_PreviousCandidate,
  UKey_Hangul_Special,

  /* function keys */
  UKey_F1,
  UKey_F2,
  UKey_F3,
  UKey_F4,
  UKey_F5,
  UKey_F6,
  UKey_F7,
  UKey_F8,
  UKey_F9,
  UKey_F10,
  UKey_F11,
  UKey_F12,
  UKey_F13,
  UKey_F14,
  UKey_F15,
  UKey_F16,
  UKey_F17,
  UKey_F18,
  UKey_F19,
  UKey_F20,
  UKey_F21,
  UKey_F22,
  UKey_F23,
  UKey_F24,
  UKey_F25,
  UKey_F26,
  UKey_F27,
  UKey_F28,
  UKey_F29,
  UKey_F30,
  UKey_F31,
  UKey_F32,
  UKey_F33,
  UKey_F34,
  UKey_F35, /* X, Gtk and Qt supports up to F35 */

  /* dead keys */
  UKey_Dead_Grave,
  UKey_Dead_Acute,
  UKey_Dead_Circumflex,
  UKey_Dead_Tilde,
  UKey_Dead_Macron,
  UKey_Dead_Breve,
  UKey_Dead_Abovedot,
  UKey_Dead_Diaeresis,
  UKey_Dead_Abovering,
  UKey_Dead_Doubleacute,
  UKey_Dead_Caron,
  UKey_Dead_Cedilla,
  UKey_Dead_Ogonek,
  UKey_Dead_Iota,
  UKey_Dead_VoicedSound,
  UKey_Dead_SemivoicedSound,
  UKey_Dead_Belowdot,
  UKey_Dead_Hook,
  UKey_Dead_Horn,

  /* Japanese Kana keys */
  UKey_Kana_Fullstop,
  UKey_Kana_OpeningBracket,
  UKey_Kana_ClosingBracket,
  UKey_Kana_Comma,
  UKey_Kana_Conjunctive,
  UKey_Kana_WO,
  UKey_Kana_a,
  UKey_Kana_i,
  UKey_Kana_u,
  UKey_Kana_e,
  UKey_Kana_o,
  UKey_Kana_ya,
  UKey_Kana_yu,
  UKey_Kana_yo,
  UKey_Kana_tsu,
  UKey_Kana_ProlongedSound,
  UKey_Kana_A,
  UKey_Kana_I,
  UKey_Kana_U,
  UKey_Kana_E,
  UKey_Kana_O,
  UKey_Kana_KA,
  UKey_Kana_KI,
  UKey_Kana_KU,
  UKey_Kana_KE,
  UKey_Kana_KO,
  UKey_Kana_SA,
  UKey_Kana_SHI,
  UKey_Kana_SU,
  UKey_Kana_SE,
  UKey_Kana_SO,
  UKey_Kana_TA,
  UKey_Kana_CHI,
  UKey_Kana_TSU,
  UKey_Kana_TE,
  UKey_Kana_TO,
  UKey_Kana_NA,
  UKey_Kana_NI,
  UKey_Kana_NU,
  UKey_Kana_NE,
  UKey_Kana_NO,
  UKey_Kana_HA,
  UKey_Kana_HI,
  UKey_Kana_FU,
  UKey_Kana_HE,
  UKey_Kana_HO,
  UKey_Kana_MA,
  UKey_Kana_MI,
  UKey_Kana_MU,
  UKey_Kana_ME,
  UKey_Kana_MO,
  UKey_Kana_YA,
  UKey_Kana_YU,
  UKey_Kana_YO,
  UKey_Kana_RA,
  UKey_Kana_RI,
  UKey_Kana_RU,
  UKey_Kana_RE,
  UKey_Kana_RO,
  UKey_Kana_WA,
  UKey_Kana_N,
  UKey_Kana_VoicedSound,
  UKey_Kana_SemivoicedSound,

  /* non-standard platform specific keys (e.g. Zaurus PDA) */
  UKey_Private1,
  UKey_Private2,
  UKey_Private3,
  UKey_Private4,
  UKey_Private5,
  UKey_Private6,
  UKey_Private7,
  UKey_Private8,
  UKey_Private9,
  UKey_Private10,
  UKey_Private11,
  UKey_Private12,
  UKey_Private13,
  UKey_Private14,
  UKey_Private15,
  UKey_Private16,
  UKey_Private17,
  UKey_Private18,
  UKey_Private19,
  UKey_Private20,
  UKey_Private21,
  UKey_Private22,
  UKey_Private23,
  UKey_Private24,
  UKey_Private25,
  UKey_Private26,
  UKey_Private27,
  UKey_Private28,
  UKey_Private29,
  UKey_Private30,

  /* modifier keys */
  UKey_Shift = 0x8000,
  UKey_Control,
  UKey_Alt,
  UKey_Meta,
  UKey_Super,
  UKey_Hyper,

  /* lock modifier keys: unstable */
  UKey_Caps_Lock = 0x9000,
  UKey_Num_Lock,
  UKey_Scroll_Lock,

#if 1
  /* Deprecated. Please replace with UKey_Shift and so on. */
  UKey_Shift_key = UKey_Shift,
  UKey_Control_key = UKey_Control,
  UKey_Alt_key = UKey_Alt,
  UKey_Meta_key = UKey_Meta,
  UKey_Super_key = UKey_Super,
  UKey_Hyper_key = UKey_Hyper,
#endif

  UKey_Other = 0x10000
};
  
enum UKeyModifier {
  UMod_Shift = 1,
  UMod_Control = 2,
  UMod_Alt = 4,
  UMod_Meta = 8,
  UMod_Pseudo0 = 16,
  UMod_Pseudo1 = 32,
  UMod_Super = 64,
  UMod_Hyper = 128
};
  
enum UPreeditAttr {
  UPreeditAttr_None = 0,
  UPreeditAttr_UnderLine = 1,
  UPreeditAttr_Reverse = 2,
  UPreeditAttr_Cursor = 4,
  UPreeditAttr_Separator = 8
};

/* Cursor of clipboard text is always positioned at end. */
enum UTextArea {
  UTextArea_Unspecified = 0,
  UTextArea_Primary     = 1,  /* primary text area which IM commits to */
  UTextArea_Selection   = 2,  /* user-selected region of primary text area */
  UTextArea_Clipboard   = 4   /* clipboard text */
};

enum UTextOrigin {
  UTextOrigin_Unspecified = 0,
  UTextOrigin_Cursor      = 1,  /* current position of the cursor */
  UTextOrigin_Beginning   = 2,  /* beginning of the text */
  UTextOrigin_End         = 3   /* end of the text */
};

/*
 * Text extent specifiers
 *
 * All bridges that support the text acquisition API must implement the
 * handlings for these 'required' text extent specifiers.
 *
 *   required:
 *     - zero and positive numbers
 *     - UTextExtent_Full
 *     - UTextExtent_Line
 *
 * Zero and positive numbers are interpreted as string length (counted in
 * characters, not bytes).
 *
 *
 * Following language-specific extent specifiers are recommended to be
 * implemented although experimental. Input methods that use these specifiers
 * should separate the features based on the specifiers as "experimental
 * features" and off by default.  And do not assume correct result is always
 * returned. These specifiers may be re-categorized as 'required' when we have
 * been well-experimented and it is considered as appropriate.
 *
 *   recommended:
 *     - UTextExtent_Paragraph
 *     - UTextExtent_Sentence
 *     - UTextExtent_Word
 *
 *
 * These specifiers are experimental and reserved for future use.
 *
 *   experimental:
 *     - UTextExtent_CharFrags
 *     - UTextExtent_DispRect
 *     - UTextExtent_DispLine
 *
 * UTextExtent_CharFrags stands for "character fragments" such as Thai
 * combining marks, Hangul jamo, Japanese voiced consonant marks etc. It is
 * supposed to be used for the "surrounding text" acquisition. Bridges should
 * supply only such combinable characters if this specifier is passed.
 */
enum UTextExtent {
  UTextExtent_Unspecified = -1,  /* invalid */

  /* logical extents */
  UTextExtent_Full      = -2,   /* beginning or end of the whole text */
  UTextExtent_Paragraph = -3,   /* the paragraph which the origin is included */
  UTextExtent_Sentence  = -5,   /* the sentence which the origin is included */
  UTextExtent_Word      = -9,   /* the word which the origin is included */
  UTextExtent_CharFrags = -17,  /* character fragments around the origin */

  /* physical extents */
  UTextExtent_DispRect  = -33,  /* the text region displayed in the widget */
  UTextExtent_DispLine  = -65,  /* displayed line (eol: linebreak) */
  UTextExtent_Line      = -129  /* real line      (eol: newline char) */
};

/* abstracting platform-dependent character code conversion method */
struct uim_code_converter {
  int  (*is_convertible)(const char *tocode, const char *fromcode);
  void *(*create)(const char *tocode, const char *fromcode);
  char *(*convert)(void *obj, const char *str);
  void (*release)(void *obj);
};

/**
 * Initialize and allocate resources to start to input.  This function
 * must be called before any other uim functions are called.  The
 * second calling of this function makes no sense. This function
 * performs setlocale(3). Be careful if your code also performs it.
 *
 * @return 0 on success, otherwise -1
 */
int
uim_init(void);

/**
 * Finalize uim library by freeing all resources allocated by uim.
 */
void
uim_quit(void);

/**
 * Create new input context.
 * param lang and engine is used to specify appropriate input method tied to returning input context.
 * Currently selected input method is used if you specify both as NULL.
 *
 * @param ptr cookie value which is passed as an argument of uim's callback functions.
 * @param enc iconv-acceptable name of client encoding. Say "UTF-8" to use most of input methods.
 * @param lang name language you want to input
 * @param engine name of conversion engine you want to use
 * @param conv character code converter. Say "uim_iconv" or place your own platform-specific, preferable implementation. See struct uim_code_converter.
 * @param commit_cb callback function which is called when there comes somestring to commit. 1st argument of this callback function is "ptr" and 2nd argument the string to commit.
 *
 * @return uim_context which newly created.
 */ 
uim_context
uim_create_context(void *ptr,
		   const char *enc,
		   const char *lang,
		   const char *engine,
		   struct uim_code_converter *conv,
		   void (*commit_cb)(void *ptr, const char *str));

/**
 * Release input context which is created by uim_create_context.
 *
 * @param uc input cotext to be released.
 * @see uim_create_context
 */
void
uim_release_context(uim_context uc);

/**
 * Reset input context to neutral state.
 *
 * This handler MUST NOT commit a string and/or update the preedit. If a
 * preedit string is existing on a GUI toolkit-level reset, the bridge is
 * responsible to clear it. Internal state that considered as 'global' is
 * permitted to be kept.
 *
 * @param uc input context to be reset
 */
void
uim_reset_context(uim_context uc);

/**
 * Notify input context that the textarea is being focused in.
 *
 * The input context is permitted to commit a string and/or update the
 * preedit.
 *
 * @param uc input context
 */
void
uim_focus_in_context(uim_context uc);

/**
 * Notify input context that the textarea is being focused out.
 *
 * The input context is permitted to commit a string and/or update the
 * preedit.
 *
 * @param uc input context
 */
void
uim_focus_out_context(uim_context uc);

/**
 * Notify input context that the input point has been relocated.
 *
 * This notifies an input context that the input point (textarea and/or cursor
 * position) has been relocated. The input context is permitted to commit a
 * string and/or update the preedit.
 *
 * @param uc input context
 */
void
uim_place_context(uim_context uc);

/**
 * Notify input context that the input at the position has been discontinued.
 *
 * This notifies an input context that input at current input point (textarea
 * and/or cursor position) has been discontinued. The input context is
 * permitted to commit a string, but must not update/clear the
 * preedit. Bridge-level preedit must be cleared by bridge itself. uim-level
 * preedit is permitted to be preserved for subsequent 'place' handler call,
 * or else silently cleared.
 *
 * @param uc input context
 */
void
uim_displace_context(uim_context uc);

/**
 * Set callback functions to be called when the preedit string changes.
 * Preedit string is passed to applications by sequential calls of pushback_cb, between the calls of clear_cb and update_cb.
 * Each callback's 1st argument "ptr" corresponds to the 1st argument of uim_create_context.
 *
 * @param uc input context
 * @param clear_cb called when preedit string should be cleared.
 * @param pushback_cb called when additional preedit string comes. 2nd argument is the attribute of preedit string and 3rd argument is additonal preedit string.
 * @param update_cb called when the changes of preedit string should be updated graphically.
 *
 * @see uim_create_context
 */
void
uim_set_preedit_cb(uim_context uc,
		   void (*clear_cb)(void *ptr),
		   void (*pushback_cb)(void *ptr,
				       int attr,
				       const char *str),
		   /* page change cb .. etc will be here */
		   void (*update_cb)(void *ptr));

/* dealing pressing key */
/**
 * Send key press event to uim context
 *
 * @param uc input context which event goes to
 * @param key keycode and value is 32 to 126.
 * @param state keystate
 *
 * @return 0 if IM not handle the event, otherwise the event is handled by IM so please stop key event handling.
 */
int
uim_press_key(uim_context uc, int key, int state);
/**
 * Send key press release to uim context
 *
 * @param uc input context which event goes to
 * @param key keycode and valued is 32 to 126.
 * @param state keystate
 *
 * @return 0 if IM not handle the event, otherwise the event is handled by IM so please stop key event handling.
 */
int
uim_release_key(uim_context uc, int key, int state);

/**
 * Change client encoding of an input context.
 *
 * @param uc input context
 * @param encoding client encoding name
 *
 * @see uim_create_context
 */
void uim_set_client_encoding(uim_context uc, const char *encoding);


/* im list */
/**
 * Get the number of input methods with same encoding and language of the passed context.
 *
 * @param uc input context
 *
 * @return number of input methods
 */
int uim_get_nr_im(uim_context uc);

/**
 * Get the name of nth input method.
 *
 * @warning you must not free the result.
 *
 * @param uc input context
 * @param nth index of input method.
 *
 * @return name of nth input method. only valid until next uim API call.
 */
const char *uim_get_im_name(uim_context uc, int nth);

/**
 * Get the language of nth input method.
 *
 * @warning you must not free the result.
 *
 * @param uc input context
 * @param nth index of input method. only valid until next uim API call.
 */
const char *uim_get_im_language(uim_context uc, int nth);

/**
 * Get the short description of nth input method.
 *
 * @warning you must not free the result.
 *
 * @param uc input context
 * @param nth index of input method. only valid until next uim API call.
 */
const char *uim_get_im_short_desc(uim_context uc, int nth);

/**
 * Get the encoding of nth input method.
 *
 * @warning you must not free the result
 *
 * @param uc input context
 * @param nth index of input method
 *
 * @return nth input method's encoding. only valid until next uim API call.
 */
const char *uim_get_im_encoding(uim_context uc, int nth);


/**
 * Get the default input method engine name.
 *
 * @warning you must not free the result
 *
 * @param localename locale name
 *
 * @return input method name. only valid until next uim API call.
 */
const char *uim_get_default_im_name(const char *localename);

/**
 * Get the most preferable input method engine name for the localename.
 *
 * @warning you must not free the result
 *
 * @param localename locale name
 *
 * @return input method name. only valid until next uim API call.
 */
const char *uim_get_im_name_for_locale(const char *localename);

/* candidate */
/**
 * Set callback functions to be called when the candidate-selection occurs.
 * Each callback's 1st argument "ptr" corresponds to the 1st argument of uim_create_context.
 *
 * @param uc input context
 * @param activate_cb called when candidate window should be activated.
 * @param select_cb called when a candidate is selected and its index is 2nd argument. 
 * @param shift_page_cb 
 * @param deactivate_cb called when candidate window should be deactivated.
 *
 * @see uim_create_context
 */
void uim_set_candidate_selector_cb(uim_context uc,
                                   void (*activate_cb)(void *ptr,
                                                       int nr,
                                                       int display_limit),
                                   void (*select_cb)(void *ptr, int index),
                                   void (*shift_page_cb)(void *ptr,
                                                         int direction),
                                   void (*deactivate_cb)(void *ptr));

/**
 * Set callback function to support delay showing candidate-selection.
 *
 * @param uc input context
 * @param delay_activate_cb called when candidate window should be activated with delay.
 *
 * @see uim_create_context
 */
void uim_set_delay_candidate_selector_cb(uim_context uc,
                                         void (*delay_activate_cb)(void *ptr,
                                                                   int delay));

/**
 * Notify that the candidate selector is being activated after delay.
 *
 * The input context must update number of candidates,
 * display limit and selected index.
 *
 * @param uc input context
 * @param nr [out] total number of candidates
 * @param display_limit [out] number of candidates to show on one page
 * @param selected_index [out] index of selected candidate
 */
void uim_delay_activating(uim_context uc, int *nr, int *display_limit, int *selected_index);

/**
 * Get candidate data.
 *
 * @param uc input context
 * @param index index of the candidate you want to get
 * @param accel_enumeration_hint index of the first candidate displayed in
 * the candidate selector
 *
 * @warning You must free the result by uim_candidate_free
 *
 * @see uim_candidate_free
 *
 * @return data of candidate
 */
uim_candidate uim_get_candidate(uim_context uc, int index, int accel_enumeration_hint);
/**
 * Free the result of uim_get_candidate.
 *
 * @param cand the data you want to free
 */
void uim_candidate_free(uim_candidate cand);

int   uim_get_candidate_index(uim_context uc);
/**
 * Select the candidate by specifying $index
 *
 * @param uc input context
 * @param index index of the candidate you want to select
 */
void  uim_set_candidate_index(uim_context uc, int index);

/**
 * Get the string of candidate.
 *
 * @warning You must not free the result. All datas are freed by calling uim_candidate_free.
 *
 * @param cand the data you got by calling uim_get_candidate
 *
 * @see uim_get_candidate
 *
 * @return string of candidate data
 */
const char *uim_candidate_get_cand_str(uim_candidate cand);
/**
 * Get the string of candidate's heading label.
 *
 * @warning You must not free the result. All datas are freed by calling uim_candidate_free.
 *
 * @param cand the data you got by uim_get_candidate
 *
 * @see uim_get_candidate
 *
 * @return string of candidate data's heading label
 */
const char *uim_candidate_get_heading_label(uim_candidate cand);

/**
 * Get the string of candidate's annotation.
 *
 * @warning You must not free the result. All datas are freed by calling uim_candidate_free.
 * @warning If no data is available, return string is "" (empty string).
 *
 * @param cand the data you got by uim_get_candidate
 *
 * @see uim_get_candidate
 *
 * @return string of candidate's annotation str
 */
const char *uim_candidate_get_annotation_str(uim_candidate cand);

/*property*/
/**
 * Set callback function to be called when property list is updated.
 *
 * @param uc input context
 * @param update_cb called when property list is updated.
 *        1st argument "ptr" corresponds to the 1st argument of uim_create_context.
 *        2nd argument is the message to be sent to the helper server with "prop_list_update" command and charset info.
 */
void
uim_set_prop_list_update_cb(uim_context uc,
			    void (*update_cb)(void *ptr, const char *str));
/**
 * Force to input context to update property list.
 *
 * @param uc input context
 */
void
uim_prop_list_update(uim_context uc);

/**
 * Obsolete. Only existing for Backward compatibility and should not
 * be called.
 */
void
uim_set_prop_label_update_cb(uim_context uc,
			     void (*update_cb)(void *ptr, const char *str));
/**
 * Obsolete. Only existing for Backward compatibility and should not
 * be called.
 */
void
uim_prop_label_update(uim_context uc);
void
uim_prop_activate(uim_context uc, const char *str);
void
uim_prop_update_custom(uim_context uc, const char *custom, const char *val);
uim_bool
uim_prop_reload_configs(void);

/* mode is obsoleted by property, so please use property API instead of mode API */
int
uim_get_current_mode(uim_context uc);
void
uim_set_mode(uim_context uc, int nth);
void
uim_set_mode_cb(uim_context uc, void (*update_cb)(void *ptr,
						  int mode));
/* mode list */
int
uim_get_nr_modes(uim_context uc);
/* you must not free the result. and only valid until next uim API call */
const char *
uim_get_mode_name(uim_context uc, int nth);
void
uim_set_mode_list_update_cb(uim_context uc,
			    void (*update_cb)(void *ptr));

/* text acquisition */
/*
 * Consideration about text update interface
 *
 * In under-development composer framework, a single commit event of a
 * composer instance can commit a text, update the preedit, and delete
 * surrounding texts atomically to reduce text flicker. But because
 * introducing this interface to current uim breaks backward compatibility
 * completely, adding separated surrounding text deletion interface is better
 * solution at now.  -- YamaKen 2006-10-07
 *
 * https://github.com/uim/uim/blob/composer/scm/event.scm
 *
 * (define-event 'commit
 *   upward-event-rec-spec
 *   '((utext           ())   ;; can include cursor position info
 *     (preedit-updated #t)   ;; can also update preedit as atomic event
 *     (former-del-len  0)    ;; for surrounding text operation
 *     (latter-del-len  0)))  ;; for surrounding text operation
 */
/**
 * Set callback functions for text acquisition and modification.
 *
 * All "former_len" and "latter_len" can be specified by zero, positive
 * numbers or enum UTextExtent. The text length is counted in singlebyte or
 * multibyte characters (not counted in bytes). Bridges may return a string
 * shorter than requested if the text is actually shorter than the requested
 * length, or the target textarea does not have the text acquisition
 * ability. Otherwise exact length string must be returned.
 *
 * Both @a acquire_cb and @a delete_cb returns zero if succeeded, otherwise
 * returns a negative integer if the bridge does not support the specified
 * text operation. But even if zero is returned, actual length of acquired
 * strings cannot be assumed (i.e. may be shorter than requested).
 *
 * @param uc input context
 * @param acquire_cb called back when the input context want to acquire a
 *        bridge-side text.
 *        1st argument "ptr" passes back the 1st argument of
 *                     uim_create_context.
 *        2nd argument "text_id" specifies a textarea having target text.
 *        3rd argument "origin" specifies the origin which former_len and
 *                     latter_len refers.
 *        4th argument "former_len" specifies length of the text preceding the
 *                     text origin to be acquired.
 *        5th argument "latter_len" specifies length of the text following the
 *                     text origin to be acquired.
 *        6th argument "former" passes a pointer reference to receive the
 *                     former part of the acquired text. The returned pointer
 *                     may be NULL and object ownership is transferred to
 *                     libuim.
 *        7th argument "latter" passes a pointer reference to receive the
 *                     latter part of the acquired text. The returned pointer
 *                     may be NULL and object ownership is transferred to
 *                     libuim.
 * @param delete_cb called back when the input context want to delete a
 *        bridge-side text.
 *        1st argument "ptr" passes back the 1st argument of
 *                     uim_create_context.
 *        2nd argument "text_id" specifies a textarea which is going to be
 *                     operated on.
 *        3rd argument "origin" specifies the origin which former_len and
 *                     latter_len refers.
 *        4th argument "former_len" specifies length of the text preceding the
 *                     text origin to be deleted.
 *        5th argument "latter_len" specifies length of the text following the
 *                     text origin to be deleted.
 */
void
uim_set_text_acquisition_cb(uim_context uc,
			    int (*acquire_cb)(void *ptr,
					      enum UTextArea text_id,
					      enum UTextOrigin origin,
					      int former_len, int latter_len,
					      char **former, char **latter),
			    int (*delete_cb)(void *ptr,
					     enum UTextArea text_id,
					     enum UTextOrigin origin,
					     int former_len, int latter_len));

/**
 * Input arbitrary string into input context.
 *
 * @param uc the input context tied with the text area.
 * @param str the string to be input into.
 *
 * @return true if @a str is accepted (consumed) by the input context.
 */
uim_bool
uim_input_string(uim_context uc, const char *str);

/*
 * Set callback function to be called when configuration of input
 * context is changed.
 *
 * @param uc input context
 * @param changed_cb called when configuration of the input context is changed.
 *        1st argument "ptr" corresponds to the 1st argument of uim_create_context.
 */
void
uim_set_configuration_changed_cb(uim_context uc,
				 void (*changed_cb)(void *ptr));


/* For plugins implementation. Bridges should not use these functions. */
void uim_fatal_error(const char *msg);  /* Disables uim */
void *uim_malloc(size_t size);
void *uim_realloc(void *p, size_t size);
void *uim_calloc(size_t nmemb, size_t size);
char *uim_strdup(const char *s);
int uim_asprintf(char **, const char *, ...);


#ifdef __cplusplus
}
#endif

#endif /* UIM_H */
