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

/*
 * uim-wayland: an input method for Wayland compositors that implement
 * zwp_input_method_v1 (Weston, KWin).  The compositor starts this
 * program itself and hands it a zwp_input_method_context_v1 whenever
 * a text field is focused.  Key events arrive through the grabbed
 * wl_keyboard, go through libuim, and the results are sent back with
 * commit_string/preedit_string.  Keys uim doesn't consume are
 * forwarded to the focused client with the "key" request.
 */

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include <wayland-client.h>
#include <xkbcommon/xkbcommon.h>

#include <uim/uim.h>

#include <input-method-unstable-v1-client-protocol.h>

#define UIM_WAYLAND_PROGRAM_NAME "uim-wayland"

/* zwp_input_method_context_v1.preedit_styling refers to the
 * preedit_style enum of zwp_text_input_v1. */
enum uim_wayland_preedit_style {
  UIM_WAYLAND_PREEDIT_STYLE_DEFAULT = 0,
  UIM_WAYLAND_PREEDIT_STYLE_NONE = 1,
  UIM_WAYLAND_PREEDIT_STYLE_ACTIVE = 2,
  UIM_WAYLAND_PREEDIT_STYLE_INACTIVE = 3,
  UIM_WAYLAND_PREEDIT_STYLE_HIGHLIGHT = 4,
  UIM_WAYLAND_PREEDIT_STYLE_UNDERLINE = 5,
  UIM_WAYLAND_PREEDIT_STYLE_SELECTION = 6,
  UIM_WAYLAND_PREEDIT_STYLE_INCORRECT = 7
};

struct uim_wayland_candwin;

struct uim_wayland_preedit_segment {
  int attr;
  char *str;
};

/* Evdev keycodes are small; 1024 bits is plenty for the bookkeeping
 * of which pressed keys were forwarded to the client. */
#define UIM_WAYLAND_MAX_KEYCODE 1024

struct uim_wayland {
  struct wl_display *display;
  struct wl_registry *registry;
  struct wl_compositor *compositor;
  struct wl_shm *shm;
  struct zwp_input_method_v1 *input_method;
  struct zwp_input_panel_v1 *input_panel;

  /* The active context. NULL while no text field is focused. */
  struct zwp_input_method_context_v1 *context;
  struct wl_keyboard *keyboard;
  /* Serial from the last commit_state event. */
  uint32_t serial;

  struct xkb_context *xkb_context;
  struct xkb_keymap *xkb_keymap;
  struct xkb_state *xkb_state;

  uim_context uc;
  bool focused;

  struct uim_wayland_preedit_segment *segments;
  size_t n_segments;
  size_t segments_capacity;
  bool preedit_shown;

  struct uim_wayland_candwin *candwin;

  uint8_t forwarded_keys[UIM_WAYLAND_MAX_KEYCODE / 8];

  int helper_fd;
  bool running;
};

/* uim-wayland.c */
void uim_wayland_commit_string(struct uim_wayland *uw, const char *str);

/* key.c */
void uim_wayland_convert_key(xkb_keysym_t sym,
                             struct xkb_state *state,
                             int *ukey,
                             int *umod);

/* candwin.c */
struct uim_wayland_candwin *uim_wayland_candwin_new(struct uim_wayland *uw);
void uim_wayland_candwin_free(struct uim_wayland_candwin *cw);
void uim_wayland_candwin_activate(struct uim_wayland_candwin *cw,
                                  int nr,
                                  int display_limit);
void uim_wayland_candwin_select(struct uim_wayland_candwin *cw, int index);
void uim_wayland_candwin_shift_page(struct uim_wayland_candwin *cw,
                                    bool forward);
void uim_wayland_candwin_deactivate(struct uim_wayland_candwin *cw);

/* helper.c */
void uim_wayland_helper_connect(struct uim_wayland *uw);
void uim_wayland_helper_disconnect(struct uim_wayland *uw);
void uim_wayland_helper_dispatch(struct uim_wayland *uw);
void uim_wayland_helper_send(struct uim_wayland *uw, const char *message);
void uim_wayland_helper_send_im_list(struct uim_wayland *uw);
void uim_wayland_helper_focus_in(struct uim_wayland *uw);
void uim_wayland_helper_focus_out(struct uim_wayland *uw);
