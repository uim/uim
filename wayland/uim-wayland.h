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
 * Only input-method-v1.c talks the protocol.
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

/* wl_surface.preferred_buffer_scale needs version 6, and headers from
 * wayland 1.22 or later. */
#ifdef WL_SURFACE_PREFERRED_BUFFER_SCALE_SINCE_VERSION
#define UIM_WAYLAND_COMPOSITOR_VERSION 6
#else
#define UIM_WAYLAND_COMPOSITOR_VERSION 4
#endif

struct uim_wayland_candwin;

struct uim_wayland_preedit_segment {
  int attr;
  char *str;
};

/* A part of the preedit text, in bytes, with its UPreeditAttr. */
struct uim_wayland_preedit_span {
  uint32_t offset;
  uint32_t length;
  int attr;
};

struct uim_wayland;

/* What the protocol in use does for the rest of uim-wayland. The
 * requests are dropped while no text field is focused. */
struct uim_wayland_input_method {
  void (*commit_string)(struct uim_wayland *uw, const char *str);
  void (*set_preedit)(struct uim_wayland *uw,
                      const char *text,
                      uint32_t cursor,
                      const struct uim_wayland_preedit_span *spans,
                      size_t n_spans);
  /* index is the start of the text relative to the cursor, in bytes. */
  void (*delete_surrounding_text)(struct uim_wayland *uw,
                                  int32_t index,
                                  uint32_t length);
  void (*deactivate)(struct uim_wayland *uw);
  void (*destroy)(struct uim_wayland *uw);
};

struct uim_wayland_v1;

/* Evdev keycodes are small; 1024 bits is plenty for the bookkeeping
 * of which pressed keys were forwarded to the client and which were
 * pressed while the field was bypassed. */
#define UIM_WAYLAND_MAX_KEYCODE 1024

struct uim_wayland {
  struct wl_display *display;
  struct wl_registry *registry;
  struct wl_compositor *compositor;
  struct wl_shm *shm;
  /* The protocol in use. NULL until the compositor offers one. */
  const struct uim_wayland_input_method *input_method;
  struct uim_wayland_v1 *v1;
  struct zwp_input_panel_v1 *input_panel;
  /* For the pointer on the candidate window. */
  struct wl_seat *seat;
  struct wl_pointer *pointer;

  struct xkb_context *xkb_context;
  struct xkb_keymap *xkb_keymap;
  struct xkb_state *xkb_state;

  uim_context uc;
  bool focused;
  /* The field takes no composed text, a password field for instance.
   * Keys go straight to the application. */
  bool bypassed;

  /* The text around the cursor as the application last reported it,
   * with byte offsets into it. NULL when it has told us nothing. */
  char *surrounding_text;
  size_t surrounding_cursor;
  size_t surrounding_anchor;

  struct uim_wayland_preedit_segment *segments;
  size_t n_segments;
  size_t segments_capacity;
  bool preedit_shown;

  struct uim_wayland_candwin *candwin;

  uint8_t forwarded_keys[UIM_WAYLAND_MAX_KEYCODE / 8];
  /* Keys whose press uim didn't see. */
  uint8_t bypassed_keys[UIM_WAYLAND_MAX_KEYCODE / 8];

  int helper_fd;
  bool running;
};

/* uim-wayland.c */
void uim_wayland_debug(const char *format, ...)
  __attribute__((format(printf, 1, 2)));
void uim_wayland_commit_string(struct uim_wayland *uw, const char *str);
/* For the protocol in use to tell what the compositor says. */
void uim_wayland_activate(struct uim_wayland *uw);
void uim_wayland_deactivate(struct uim_wayland *uw);
void uim_wayland_reset(struct uim_wayland *uw);
void uim_wayland_set_bypassed(struct uim_wayland *uw, bool bypassed);
void uim_wayland_set_keymap(struct uim_wayland *uw,
                            uint32_t format,
                            int32_t fd,
                            uint32_t size);
void uim_wayland_set_modifiers(struct uim_wayland *uw,
                               uint32_t mods_depressed,
                               uint32_t mods_latched,
                               uint32_t mods_locked,
                               uint32_t group);
/* Whether the key goes on to the client. state is a
 * wl_keyboard_key_state. */
bool uim_wayland_filter_key(struct uim_wayland *uw,
                            uint32_t key,
                            uint32_t state);

/* input-method-v1.c */
/* Binds the global if it belongs to zwp_input_method_v1. */
bool uim_wayland_v1_bind(struct uim_wayland *uw,
                         struct wl_registry *registry,
                         uint32_t name,
                         const char *interface);

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
/* Its user data is the struct uim_wayland. */
extern const struct wl_pointer_listener uim_wayland_candwin_pointer_listener;

/* text.c */
void uim_wayland_text_set_surrounding(struct uim_wayland *uw,
                                      const char *text,
                                      uint32_t cursor,
                                      uint32_t anchor);
void uim_wayland_text_forget_surrounding(struct uim_wayland *uw);
int uim_wayland_text_acquire(void *ptr,
                             enum UTextArea text_id,
                             enum UTextOrigin origin,
                             int former_length,
                             int latter_length,
                             char **former,
                             char **latter);
int uim_wayland_text_delete(void *ptr,
                            enum UTextArea text_id,
                            enum UTextOrigin origin,
                            int former_length,
                            int latter_length);

/* helper.c */
void uim_wayland_helper_connect(struct uim_wayland *uw);
void uim_wayland_helper_disconnect(struct uim_wayland *uw);
void uim_wayland_helper_dispatch(struct uim_wayland *uw);
void uim_wayland_helper_send(struct uim_wayland *uw, const char *message);
void uim_wayland_helper_send_im_list(struct uim_wayland *uw);
void uim_wayland_helper_focus_in(struct uim_wayland *uw);
void uim_wayland_helper_focus_out(struct uim_wayland *uw);
