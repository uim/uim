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

/* API and ABI is unstable */
#ifndef _uim_h_included_
#define _uim_h_included_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

/*
 * A boolean type for uim to explicitly indicate intention about values.  A
 * true value is represented as (val != UIM_FALSE). i.e. Don't test a value
 * with (val == UIM_TRUE).
 */
typedef int uim_bool;

#define UIM_FALSE 0
#define UIM_TRUE 1


typedef struct uim_context_ *uim_context;

typedef struct uim_candidate_ *uim_candidate;

/* This will be filled later, so ABI is unstable */
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
  UKey_Escape = 256,
  UKey_Tab,
  UKey_Backspace,
  UKey_Delete,
  UKey_Return,
  UKey_Left,
  UKey_Up ,
  UKey_Right ,
  UKey_Down ,
  UKey_Prior , /* page up */
  UKey_Next , /* page down */
  UKey_Home,
  UKey_End,
  UKey_Zenkaku_Hankaku, /* zenkaku/hankaku toggle */
  UKey_Multi_key, /* multi-key character compose */
  UKey_Mode_switch, /* charcter set switch */
  UKey_Henkan_Mode, /* start/stop conversion */
  UKey_Muhenkan, /* cancel conversion */
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
  /* this part is especially unstable */
  /* modifier keys: "_key" suffix should be removed in the future */
  UKey_Shift_key,
  UKey_Control_key,
  UKey_Alt_key,
  UKey_Meta_key,
  UKey_Super_key,
  UKey_Hyper_key,
  UKey_Other = 1000
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

#ifndef UIM_NO_BACKWARD_COMPATIBLE_API
  /* will be deprecated */
  ,
  UKey_Shift = UMod_Shift,
  UKey_Control = UMod_Control,
  UKey_Alt = UMod_Alt,
  UKey_Meta = UMod_Meta,
  UKey_Pseudo0 = UMod_Pseudo0,
  UKey_Pseudo1 = UMod_Pseudo1
#endif
};
  
enum UPreeditAttr {
  UPreeditAttr_None = 0,
  UPreeditAttr_UnderLine = 1,
  UPreeditAttr_Reverse = 2,
  UPreeditAttr_Cursor = 4,
  UPreeditAttr_Separator = 8

#ifndef UIM_NO_BACKWARD_COMPATIBLE_API
  /* will be deprecated */
  ,
  UPeAttr_None = UPreeditAttr_None,
  UPeAttr_UnderLine = UPreeditAttr_UnderLine,
  UPeAttr_Reverse = UPreeditAttr_Reverse,
  UPeAttr_Cursor = UPreeditAttr_Cursor,
  UPeAttr_Separator = UPreeditAttr_Separator
#endif
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
 * @param enc name of client encoding, say "UTF-8"
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
 * Reset input context.
 * Pending string might be committed and preedit string might be erased. (Not yet implemented so)
 *
 * @param uc input context to reset
 */
void
uim_reset_context(uim_context uc);

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
 * @return name of nth input method
 */
const char *uim_get_im_name(uim_context uc, int nth);

/**
 * Get the language of nth input method.
 *
 * @warning you must not free the result.
 *
 * @param uc input context
 * @param nth index of input method
 */
const char *uim_get_im_language(uim_context uc, int nth);

/**
 * Get the short description of nth input method.
 *
 * @warning you must not free the result.
 *
 * @param uc input context
 * @param nth index of input method
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
 * @return nth input method's encoding
 */
const char *uim_get_im_encoding(uim_context uc, int nth);


/**
 * Get the default input method engine name.
 *
 * @warning you must not free the result
 *
 * @param localename locale name
 *
 * @return input method name.
 */
const char *uim_get_default_im_name(const char *localename);

/**
 * Get the most preferable input method engine name for the localename.
 *
 * @warning you must not free the result
 *
 * @param localename locale name
 *
 * @return input method name.
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
int uim_set_candidate_selector_cb(uim_context uc,
				  void (*activate_cb)(void *ptr, int nr, int display_limit),
				  void (*select_cb)(void *ptr, int index),
				  void (*shift_page_cb)(void *ptr, int direction),
				  void (*deactivate_cb)(void *ptr));

/**
 * Get candidate data.
 *
 * @param uc input context
 * @param index index of the candidate you want to get
 * @param accel_enumeration_hint
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
 * Set callback function to be called when property list is updated.
 *
 * @param uc input context
 * @param update_cb called when property list is updated.
 *        1st argument "ptr" corresponds to the 1st argument of uim_create_context.
 *        2nd argument is the message to be sent to the helper server with "prop_label_update" command and charset info.
 */
void
uim_set_prop_label_update_cb(uim_context uc,
			     void (*update_cb)(void *ptr, const char *str));
/**
 * Update property label
 *
 * @param uc input context
 */
void
uim_prop_label_update(uim_context uc);
void
uim_prop_activate(uim_context uc, const char *str);
void
uim_prop_update_custom(uim_context uc, const char *custom, const char *val);

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
/* you must not free the result */
const char *
uim_get_mode_name(uim_context uc, int nth);
void
uim_set_mode_list_update_cb(uim_context uc,
			    void (*update_cb)(void *ptr));

/* surrounding text (experimental, ask yusuke)*/
/**
 * Set callback functions to be called when input methods want to get or
 * delete surrounding text.
 * @param uc input context
 * @param request_cb called when input methods want to get surrounding text. user can call uim_set_surrounding text to reply this request.
 * @param delete_cb called when input methods want to delete surrounding text. 'offset characters in the left of cursor and 'len - 'offset characters will be deleted. User should return 0 when it succeed.
 */
void
uim_set_surrounding_text_cb(uim_context uc,
			    void (*request_cb)(void *ptr),
			    int (*delete_cb)(void *ptr, int offset, int len));
/**
 * Set surrounding text as a reply to request callback.
 * @param uc input context
 * @param text text around cursor
 * @param cursor_pos position of cursor in text. This should be number of actual characters at the left of cursor, not bytes.
 * @param len number of characters in text
 */
void
uim_set_surrounding_text(uim_context uc, const char *text,
			 int cursor_pos, int len);


/* Utility functions */
int
uim_ipc_open_command(int old_pid, FILE **read_handler, FILE **write_handler, const char *command);
int
uim_ipc_open_command_with_option(int old_pid, FILE **read_handler, FILE **write_handler, const char *command, const char *option);
char *
uim_ipc_send_command(int *pid, FILE **read_handler,
		     FILE **write_handler, const char *command, const char *str);

/* will be deprecated. use custom API (uim-custom.h) instead */
char *uim_symbol_value_str(const char *symbol_str);

/* an uim_code_converter implementation using iconv */
extern struct uim_code_converter *uim_iconv;

#ifdef __cplusplus
}
#endif
#endif
