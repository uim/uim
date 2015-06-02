/*

  Copyright (c) 2003-2013 uim Project http://code.google.com/p/uim/

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
 * key conversion utility for uim-gtk
 */

#include <config.h>

#include <glib.h>
#include <gtk/gtk.h>
#if GTK_CHECK_VERSION(2, 90, 0)
# include <gdk/gdkkeysyms-compat.h>
#else
# include <gdk/gdkkeysyms.h>
#endif
#ifdef GDK_WINDOWING_X11
#include <gdk/gdkx.h>
#include <X11/Xlib.h>
#include <X11/XKBlib.h>
#endif

#ifdef GDK_WINDOWING_X11
#define UIM_GTK_USE_JAPANESE_KANA_KEYBOARD_HACK 1
#endif

#include "uim/uim.h"
#include "uim/uim-scm.h"
#if UIM_GTK_USE_JAPANESE_KANA_KEYBOARD_HACK
#include "uim/uim-x-util.h"
#endif

#include "key-util-gtk.h"

#ifdef GDK_WINDOWING_X11
static guint g_mod1_mask, g_mod2_mask, g_mod3_mask, g_mod4_mask, g_mod5_mask;
static gint g_numlock_mask;
static guint g_modifier_state, g_pre_modifier_state;
#endif

void
im_uim_convert_keyevent(GdkEventKey *event, int *ukey, int *umod)
{
  int keyval = event->keyval;
  int mod = event->state;

  *umod = 0;
#ifdef GDK_WINDOWING_X11
  if (event->type == GDK_KEY_PRESS) {
    if (!mod || mod == GDK_LOCK_MASK ||
	mod == g_numlock_mask)
      g_modifier_state = 0;
  }
  g_pre_modifier_state = g_modifier_state; 
#endif

  /* 1. check key */
  if (keyval < 256)
    *ukey = keyval;
  else if (keyval >= GDK_F1 && keyval <= GDK_F35)
    *ukey = keyval - GDK_F1 + UKey_F1;
  else if (keyval >= GDK_KP_0 && keyval <= GDK_KP_9)
    *ukey = keyval - GDK_KP_0 + UKey_0;
#if GTK_CHECK_VERSION(2, 6, 0)
  else if (keyval >= GDK_dead_grave && keyval <= GDK_dead_horn)
#else
  else if (keyval >= GDK_dead_grave && keyval <= GDK_dead_belowdot)
#endif
    *ukey = keyval - GDK_dead_grave + UKey_Dead_Grave;
  else if (keyval >= GDK_Kanji && keyval <= GDK_Eisu_toggle)
    *ukey = keyval - GDK_Kanji + UKey_Kanji;
  else if (keyval >= GDK_Hangul && keyval <= GDK_Hangul_Special)
    *ukey = keyval - GDK_Hangul + UKey_Hangul;
  else if (keyval >= GDK_kana_fullstop && keyval <= GDK_semivoicedsound)
    *ukey = keyval - GDK_kana_fullstop + UKey_Kana_Fullstop;
  else {
    switch (keyval) {
    case GDK_BackSpace:
      *ukey = UKey_Backspace;
      break;
    case GDK_Delete:
      *ukey = UKey_Delete;
      break;
    case GDK_Insert:
      *ukey = UKey_Insert;
      break;
    case GDK_Escape:
      *ukey = UKey_Escape;
      break;
    case GDK_Tab:
    case GDK_ISO_Left_Tab:
      *ukey = UKey_Tab;
      break;
    case GDK_Return:
      *ukey = UKey_Return;
      break;
    case GDK_Left:
      *ukey = UKey_Left;
      break;
    case GDK_Up:
      *ukey = UKey_Up;
      break;
    case GDK_Right:
      *ukey = UKey_Right;
      break;
    case GDK_Down:
      *ukey = UKey_Down;
      break;
    case GDK_Prior:
      *ukey = UKey_Prior;
      break;
    case GDK_Next:
      *ukey = UKey_Next;
      break;
    case GDK_Home:
      *ukey = UKey_Home;
      break;
    case GDK_End:
      *ukey = UKey_End;
      break;
    case GDK_Multi_key:
      *ukey = UKey_Multi_key;
      break;
    case GDK_Codeinput:
      *ukey = UKey_Codeinput;
      break;
    case GDK_SingleCandidate:
      *ukey = UKey_SingleCandidate;
      break;
    case GDK_MultipleCandidate:
      *ukey = UKey_MultipleCandidate;
      break;
    case GDK_PreviousCandidate:
      *ukey = UKey_PreviousCandidate;
      break;
    case GDK_Mode_switch:
      *ukey = UKey_Mode_switch;
      break;
    case GDK_Shift_L:
    case GDK_Shift_R:
#ifdef GDK_WINDOWING_X11
      if (event->type == GDK_KEY_PRESS)
	g_modifier_state |= UMod_Shift;
      else
	g_modifier_state &= ~UMod_Shift;
#endif
      *ukey = UKey_Shift_key;
      break;
    case GDK_Control_L:
    case GDK_Control_R:
#ifdef GDK_WINDOWING_X11
      if (event->type == GDK_KEY_PRESS)
	g_modifier_state |= UMod_Control;
      else
	g_modifier_state &= ~UMod_Control;
#endif
      *ukey = UKey_Control_key;
      break;
    case GDK_Alt_L:
    case GDK_Alt_R:
#ifdef GDK_WINDOWING_X11
      if (event->type == GDK_KEY_PRESS)
	g_modifier_state |= UMod_Alt;
      else
	g_modifier_state &= ~UMod_Alt;
#endif
      *ukey = UKey_Alt_key;
      break;
    case GDK_Meta_L:
    case GDK_Meta_R:
#ifdef GDK_WINDOWING_X11
      if (event->type == GDK_KEY_PRESS)
	g_modifier_state |= UMod_Meta;
      else
	g_modifier_state &= ~UMod_Meta;
#endif
      *ukey = UKey_Meta_key;
      break;
    case GDK_Super_L:
    case GDK_Super_R:
#ifdef GDK_WINDOWING_X11
      if (event->type == GDK_KEY_PRESS)
	g_modifier_state |= UMod_Super;
      else
	g_modifier_state &= ~UMod_Super;
#endif
      *ukey = UKey_Super_key;
      break;
    case GDK_Hyper_L:
    case GDK_Hyper_R:
#ifdef GDK_WINDOWING_X11
      if (event->type == GDK_KEY_PRESS)
	g_modifier_state |= UMod_Hyper;
      else
	g_modifier_state &= ~UMod_Hyper;
#endif
      *ukey = UKey_Hyper_key;
      break;
    case GDK_Caps_Lock:
      *ukey = UKey_Caps_Lock;
      break;
    case GDK_Num_Lock:
      *ukey = UKey_Num_Lock;
      break;
    case GDK_Scroll_Lock:
      *ukey = UKey_Scroll_Lock;
      break;
    default:
      *ukey = UKey_Other;
     break;
    }
  }
#if UIM_GTK_USE_JAPANESE_KANA_KEYBOARD_HACK
  /* 1'. replace keysym for Japanese keyboard */
  *ukey = uim_x_kana_input_hack_translate_key(*ukey, event->hardware_keycode);
#endif

  /* 2. check modifier */
  if (mod & GDK_SHIFT_MASK)
    *umod |= UMod_Shift;
  if (mod & GDK_CONTROL_MASK)
    *umod |= UMod_Control;
#ifdef GDK_WINDOWING_X11
  if (mod & GDK_MOD1_MASK)
    *umod |= (g_mod1_mask & g_pre_modifier_state);
  if (mod & GDK_MOD2_MASK)
    *umod |= (g_mod2_mask & g_pre_modifier_state);
  if (mod & GDK_MOD3_MASK)
    *umod |= (g_mod3_mask & g_pre_modifier_state);
  if (mod & GDK_MOD4_MASK)
    *umod |= (g_mod4_mask & g_pre_modifier_state);
  if (mod & GDK_MOD5_MASK)
    *umod |= (g_mod5_mask & g_pre_modifier_state);
#else
  if (mod & GDK_MOD1_MASK)
    *umod |= UMod_Alt;
  if (mod & GDK_MOD3_MASK)  /* assuming mod3 */
    *umod |= UMod_Super;
  if (mod & GDK_MOD4_MASK)  /* assuming mod4 */
    *umod |= UMod_Hyper;
#endif
}

#ifdef GDK_WINDOWING_X11
static int
check_modifier(GSList *slist)
{
  int ret;
  GSList *tmp_list;

  ret = 0;
  for (tmp_list = slist; tmp_list; tmp_list = tmp_list->next) {
    switch (GPOINTER_TO_UINT(tmp_list->data)) {
    case XK_Shift_L:
    case XK_Shift_R:
      ret |= UMod_Shift;
      break;
    case XK_Control_L:
    case XK_Control_R:
      ret |= UMod_Control;
      break;
    case XK_Meta_L:
    case XK_Meta_R:
      ret |= UMod_Meta;
      break;
    case XK_Alt_L:
    case XK_Alt_R:
      ret |= UMod_Alt;
      break;
    case XK_Super_L:
    case XK_Super_R:
      ret |= UMod_Super;
      break;
    case XK_Hyper_L:
    case XK_Hyper_R:
      ret |= UMod_Hyper;
      break;
    default:
      break;
    }
  }
  return ret;
}
#endif

void
im_uim_init_modifier_keys()
{
#ifdef GDK_WINDOWING_X11
  int i, k = 0;
  int min_keycode, max_keycode, keysyms_per_keycode = 0;
  Display *display;
  GSList *mod1_list, *mod2_list, *mod3_list, *mod4_list, *mod5_list; 
  XModifierKeymap *map;
  KeySym *sym;

  g_modifier_state = 0;
  g_numlock_mask = 0;

  mod1_list = mod2_list = mod3_list = mod4_list = mod5_list = NULL;

  display = GDK_DISPLAY_XDISPLAY(gdk_display_get_default());
  map = XGetModifierMapping(display);
  XDisplayKeycodes(display, &min_keycode, &max_keycode);
  sym = XGetKeyboardMapping(display, min_keycode,
		  	    (max_keycode - min_keycode + 1),
			    &keysyms_per_keycode);
  for (i = 0; i < 8; i++) {
    int j;
    for (j = 0; j < map->max_keypermod; j++) {
      if (map->modifiermap[k]) {
	KeySym ks;
	int index = 0;
	do {
	  ks = XkbKeycodeToKeysym(display, map->modifiermap[k], 0, index);
	  index++;
	} while (!ks && index < keysyms_per_keycode);

	switch (i) {
	case ShiftMapIndex:
	  break;
	case LockMapIndex:
	  break;
	case ControlMapIndex:
	  break;
	case Mod1MapIndex:
	  mod1_list = g_slist_prepend(mod1_list, GUINT_TO_POINTER(ks));
	  g_mod1_mask = check_modifier(mod1_list);
	  break;
	case Mod2MapIndex:
	  mod2_list = g_slist_prepend(mod2_list, GUINT_TO_POINTER(ks));
	  g_mod2_mask = check_modifier(mod2_list);
	  break;
	case Mod3MapIndex:
	  mod3_list = g_slist_prepend(mod3_list, GUINT_TO_POINTER(ks));
	  g_mod3_mask = check_modifier(mod3_list);
	  break;
	case Mod4MapIndex:
	  mod4_list = g_slist_prepend(mod4_list, GUINT_TO_POINTER(ks));
	  g_mod4_mask = check_modifier(mod4_list);
	  break;
	case Mod5MapIndex:
	  mod5_list = g_slist_prepend(mod5_list, GUINT_TO_POINTER(ks));
	  g_mod5_mask = check_modifier(mod5_list);
	  break;
	default:
	  break;
	}
	if (ks == XK_Num_Lock)
	  g_numlock_mask |= (1 << i);
      }
      k++;
    }
  }
  g_slist_free(mod1_list);
  g_slist_free(mod2_list);
  g_slist_free(mod3_list);
  g_slist_free(mod4_list);
  g_slist_free(mod5_list);
  XFreeModifiermap(map);
  XFree(sym);

  if (uim_scm_c_bool(uim_scm_callf("require-dynlib", "s", "xkb")))
    uim_scm_callf("%xkb-set-display", "p", display);

#if UIM_GTK_USE_JAPANESE_KANA_KEYBOARD_HACK
  uim_x_kana_input_hack_init(display);
#endif
#endif
}
