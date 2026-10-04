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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uim-wayland.h"

#include <uim/uim-util.h>

struct uim_wayland_v2 {
  struct zwp_input_method_manager_v2 *manager;
  struct zwp_virtual_keyboard_manager_v1 *virtual_keyboard_manager;
  struct zwp_input_method_v2 *input_method;
  /* Held only while a text field is focused, as the compositor sends
   * it every key of the seat. */
  struct zwp_input_method_keyboard_grab_v2 *keyboard_grab;
  /* Kept for the whole run: some compositors, Sway 1.9 and older for
   * instance, mishandle one created for each text field. */
  struct zwp_virtual_keyboard_v1 *virtual_keyboard;
  /* The compositor rejects keys on a virtual keyboard without one. */
  bool has_keymap;
  /* Keys pressed on the virtual keyboard and not released yet, with
   * the time of the last one sent. */
  uint8_t pressed_keys[UIM_WAYLAND_MAX_KEYCODE / 8];
  uint32_t time;

  /* The number of done events, which a commit request refers to. */
  uint32_t serial;
  /* What the events since the last done event ask for. activate and
   * deactivate take effect on done. */
  bool pending_active;
  bool pending_activate;
};

/* output */

static void
commit(struct uim_wayland_v2 *v2)
{
  zwp_input_method_v2_commit(v2->input_method, v2->serial);
}

static void
commit_string(struct uim_wayland *uw, const char *str)
{
  struct uim_wayland_v2 *v2 = uw->v2;

  zwp_input_method_v2_commit_string(v2->input_method, str);
  commit(v2);
}

/* The text input shows no styles, only the cursor. */
static void
set_preedit(struct uim_wayland *uw,
            const char *text,
            uint32_t cursor,
            const struct uim_wayland_preedit_span *spans,
            size_t n_spans)
{
  struct uim_wayland_v2 *v2 = uw->v2;
  (void)spans;
  (void)n_spans;

  zwp_input_method_v2_set_preedit_string(v2->input_method, text,
                                         (int32_t)cursor, (int32_t)cursor);
  commit(v2);
}

/* Only text that touches the cursor can be deleted. */
static void
delete_surrounding_text(struct uim_wayland *uw, int32_t index, uint32_t length)
{
  struct uim_wayland_v2 *v2 = uw->v2;
  int64_t end = (int64_t)index + length;

  if (index > 0 || end < 0)
    return;
  zwp_input_method_v2_delete_surrounding_text(v2->input_method,
                                              (uint32_t)-index,
                                              (uint32_t)end);
  commit(v2);
}

/* virtual keyboard */

static void
forward_key(struct uim_wayland_v2 *v2, uint32_t time, uint32_t key,
            uint32_t state)
{
  if (key < UIM_WAYLAND_MAX_KEYCODE) {
    if (state == WL_KEYBOARD_KEY_STATE_RELEASED)
      v2->pressed_keys[key / 8] &= ~(1 << (key % 8));
    else
      v2->pressed_keys[key / 8] |= 1 << (key % 8);
  }
  v2->time = time;
  zwp_virtual_keyboard_v1_key(v2->virtual_keyboard, time, key, state);
}

/* The releases of these keys go to the next text field, or nowhere, so
 * the application would repeat them forever. */
static void
release_pressed_keys(struct uim_wayland_v2 *v2)
{
  uint32_t key;

  for (key = 0; key < UIM_WAYLAND_MAX_KEYCODE; key++) {
    if (v2->pressed_keys[key / 8] & (1 << (key % 8)))
      forward_key(v2, v2->time, key, WL_KEYBOARD_KEY_STATE_RELEASED);
  }
}

/* grabbed keyboard */

static void
keyboard_keymap(void *data,
                struct zwp_input_method_keyboard_grab_v2 *keyboard_grab,
                uint32_t format,
                int32_t fd,
                uint32_t size)
{
  struct uim_wayland *uw = data;
  struct uim_wayland_v2 *v2 = uw->v2;
  (void)keyboard_grab;

  /* The keys forwarded to the client must mean what they meant here.
   * The descriptor is duplicated as the request is sent. */
  if (format == WL_KEYBOARD_KEYMAP_FORMAT_XKB_V1) {
    zwp_virtual_keyboard_v1_keymap(v2->virtual_keyboard, format, fd, size);
    v2->has_keymap = true;
  }
  uim_wayland_set_keymap(uw, format, fd, size);
}

static void
keyboard_key(void *data,
             struct zwp_input_method_keyboard_grab_v2 *keyboard_grab,
             uint32_t serial,
             uint32_t time,
             uint32_t key,
             uint32_t state)
{
  struct uim_wayland *uw = data;
  struct uim_wayland_v2 *v2 = uw->v2;
  (void)keyboard_grab;
  (void)serial;

  if (uim_wayland_filter_key(uw, key, state) && v2->has_keymap)
    forward_key(v2, time, key, state);
}

static void
keyboard_modifiers(void *data,
                   struct zwp_input_method_keyboard_grab_v2 *keyboard_grab,
                   uint32_t serial,
                   uint32_t mods_depressed,
                   uint32_t mods_latched,
                   uint32_t mods_locked,
                   uint32_t group)
{
  struct uim_wayland *uw = data;
  struct uim_wayland_v2 *v2 = uw->v2;
  (void)keyboard_grab;
  (void)serial;

  uim_wayland_set_modifiers(uw, mods_depressed, mods_latched, mods_locked,
                            group);
  /* The client only receives what we forward. */
  if (v2->has_keymap)
    zwp_virtual_keyboard_v1_modifiers(v2->virtual_keyboard, mods_depressed,
                                      mods_latched, mods_locked, group);
}

static void
keyboard_repeat_info(void *data,
                     struct zwp_input_method_keyboard_grab_v2 *keyboard_grab,
                     int32_t rate,
                     int32_t delay)
{
  (void)data;
  (void)keyboard_grab;
  /* Keys uim consumes don't repeat. The client repeats the ones it
   * gets on its own. */
  uim_wayland_debug("received repeat_info rate %d delay %d (ignored)",
                    rate, delay);
}

static const struct zwp_input_method_keyboard_grab_v2_listener
keyboard_grab_listener = {
  keyboard_keymap,
  keyboard_key,
  keyboard_modifiers,
  keyboard_repeat_info
};

/* activation */

static void
deactivate(struct uim_wayland *uw)
{
  struct uim_wayland_v2 *v2 = uw->v2;

  if (!uw->focused)
    return;

  uim_wayland_deactivate(uw);
  release_pressed_keys(v2);
  if (v2->keyboard_grab) {
    zwp_input_method_keyboard_grab_v2_release(v2->keyboard_grab);
    v2->keyboard_grab = NULL;
  }
}

static void
activate(struct uim_wayland *uw)
{
  struct uim_wayland_v2 *v2 = uw->v2;

  v2->keyboard_grab = zwp_input_method_v2_grab_keyboard(v2->input_method);
  zwp_input_method_keyboard_grab_v2_add_listener(v2->keyboard_grab,
                                                 &keyboard_grab_listener, uw);
  uim_wayland_activate(uw);
}

static void
input_method_activate(void *data, struct zwp_input_method_v2 *input_method)
{
  struct uim_wayland *uw = data;
  (void)input_method;

  uw->v2->pending_active = true;
  uw->v2->pending_activate = true;
}

static void
input_method_deactivate(void *data, struct zwp_input_method_v2 *input_method)
{
  struct uim_wayland *uw = data;
  (void)input_method;

  uw->v2->pending_active = false;
  uw->v2->pending_activate = false;
}

static void
input_method_surrounding_text(void *data,
                              struct zwp_input_method_v2 *input_method,
                              const char *text,
                              uint32_t cursor,
                              uint32_t anchor)
{
  (void)data;
  (void)input_method;
  (void)text;
  (void)cursor;
  (void)anchor;
}

static void
input_method_text_change_cause(void *data,
                               struct zwp_input_method_v2 *input_method,
                               uint32_t cause)
{
  (void)data;
  (void)input_method;
  (void)cause;
}

static void
input_method_content_type(void *data,
                          struct zwp_input_method_v2 *input_method,
                          uint32_t hint,
                          uint32_t purpose)
{
  (void)data;
  (void)input_method;
  (void)hint;
  (void)purpose;
}

/* An activate while active is another text field, or the same one
 * starting over: either way what was composed is left behind. */
static void
input_method_done(void *data, struct zwp_input_method_v2 *input_method)
{
  struct uim_wayland *uw = data;
  struct uim_wayland_v2 *v2 = uw->v2;
  (void)input_method;

  v2->serial++;
  if (v2->pending_activate)
    deactivate(uw);
  if (v2->pending_active && !uw->focused)
    activate(uw);
  else if (!v2->pending_active)
    deactivate(uw);
  v2->pending_activate = false;
}

static void
input_method_unavailable(void *data, struct zwp_input_method_v2 *input_method)
{
  struct uim_wayland *uw = data;
  (void)input_method;

  fprintf(stderr,
          "%s: another input method is running on the seat\n",
          UIM_WAYLAND_PROGRAM_NAME);
  uw->running = false;
}

static const struct zwp_input_method_v2_listener input_method_listener = {
  input_method_activate,
  input_method_deactivate,
  input_method_surrounding_text,
  input_method_text_change_cause,
  input_method_content_type,
  input_method_done,
  input_method_unavailable
};

static void
destroy(struct uim_wayland *uw)
{
  struct uim_wayland_v2 *v2 = uw->v2;

  if (v2->keyboard_grab)
    zwp_input_method_keyboard_grab_v2_release(v2->keyboard_grab);
  if (v2->input_method)
    zwp_input_method_v2_destroy(v2->input_method);
  if (v2->virtual_keyboard)
    zwp_virtual_keyboard_v1_destroy(v2->virtual_keyboard);
  if (v2->manager)
    zwp_input_method_manager_v2_destroy(v2->manager);
  if (v2->virtual_keyboard_manager)
    zwp_virtual_keyboard_manager_v1_destroy(v2->virtual_keyboard_manager);
  free(v2);
  uw->v2 = NULL;
}

static const struct uim_wayland_input_method input_method_v2 = {
  commit_string,
  set_preedit,
  delete_surrounding_text,
  deactivate,
  destroy
};

bool
uim_wayland_v2_offer(struct uim_wayland *uw,
                     uint32_t name,
                     const char *interface)
{
  if (strcmp(interface, zwp_input_method_manager_v2_interface.name) == 0)
    uw->v2_manager_name = name;
  else if (strcmp(interface,
                  zwp_virtual_keyboard_manager_v1_interface.name) == 0)
    uw->v2_virtual_keyboard_manager_name = name;
  else
    return false;
  return true;
}

void
uim_wayland_v2_start(struct uim_wayland *uw)
{
  struct uim_wayland_v2 *v2;

  if (!uw->v2_manager_name || !uw->v2_virtual_keyboard_manager_name ||
      !uw->seat)
    return;

  v2 = uw->v2 = uim_malloc(sizeof(*v2));
  memset(v2, 0, sizeof(*v2));
  v2->manager = wl_registry_bind(uw->registry, uw->v2_manager_name,
                                 &zwp_input_method_manager_v2_interface, 1);
  v2->virtual_keyboard_manager =
    wl_registry_bind(uw->registry, uw->v2_virtual_keyboard_manager_name,
                     &zwp_virtual_keyboard_manager_v1_interface, 1);
  v2->virtual_keyboard =
    zwp_virtual_keyboard_manager_v1_create_virtual_keyboard(
      v2->virtual_keyboard_manager, uw->seat);
  v2->input_method =
    zwp_input_method_manager_v2_get_input_method(v2->manager, uw->seat);
  zwp_input_method_v2_add_listener(v2->input_method, &input_method_listener,
                                   uw);
  uw->input_method = &input_method_v2;
}
