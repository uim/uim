/*

  Copyright (c) 2003-2026 uim Project https://github.com/uim/uim

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
 * key utility for uim-gtk (GTK 4)
 *
 * GTK 4 note: GdkEventKey is gone -- GdkEvent is opaque now and its
 * fields are read through accessors (gdk_key_event_get_keyval(),
 * gdk_event_get_modifier_state(), gdk_event_get_event_type(),
 * gdk_key_event_get_keycode()), exactly as gtk4/immodule/uim-im-context.c
 * already does in uim_im_context_filter_keypress(). This header keeps
 * the same im_uim_convert_keyevent() signature semantics as the GTK 3
 * version, just taking a GdkEvent * instead of a GdkEventKey *.
 */

#ifndef UIM_GTK_KEY_UTIL_GTK_H
#define UIM_GTK_KEY_UTIL_GTK_H

/*
 * Initialize modifier key mappings.
 */
void im_uim_init_modifier_keys(void);

/*
 * Get ukey and umod from a GdkEvent's keyval and modifier state.
 * This function should be called at both key press and release events.
 */
void im_uim_convert_keyevent(GdkEvent *event, int *ukey, int *umod);

#endif
