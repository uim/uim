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

/* zwp_input_method_v2.content_type refers to the content_hint and
 * content_purpose enums of zwp_text_input_v3, which number the purposes
 * differently from zwp_text_input_v1. Only the values uim-wayland acts
 * on are listed. */
enum content_hint {
  CONTENT_HINT_HIDDEN_TEXT = 0x40,
  CONTENT_HINT_SENSITIVE_DATA = 0x80
};

enum content_purpose {
  CONTENT_PURPOSE_DIGITS = 2,
  CONTENT_PURPOSE_NUMBER = 3,
  CONTENT_PURPOSE_PHONE = 4,
  CONTENT_PURPOSE_PASSWORD = 8,
  CONTENT_PURPOSE_PIN = 9,
  CONTENT_PURPOSE_DATE = 10,
  CONTENT_PURPOSE_TIME = 11,
  CONTENT_PURPOSE_DATETIME = 12
};

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
  /* The text around the cursor that the next done applies. NULL for
   * none, which is also what a done applies when no surrounding_text
   * event came before it. */
  char *pending_surrounding_text;
  uint32_t pending_surrounding_cursor;
  uint32_t pending_surrounding_anchor;
  /* The content type stays until changed or reset by activate. */
  uint32_t pending_content_hint;
  uint32_t pending_content_purpose;
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

/* The text input shows no styles, only the cursor. A commit without
 * set_preedit_string clears the preedit. wlroots-based compositors
 * pass an empty preedit string on as it is, which GTK takes as a
 * preedit still going on: terminals would keep hiding their cursor. */
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

  if (text[0] != '\0')
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
forget_pending_surrounding_text(struct uim_wayland_v2 *v2)
{
  free(v2->pending_surrounding_text);
  v2->pending_surrounding_text = NULL;
  v2->pending_surrounding_cursor = 0;
  v2->pending_surrounding_anchor = 0;
}

/* activate resets the text input state too. */
static void
input_method_activate(void *data, struct zwp_input_method_v2 *input_method)
{
  struct uim_wayland *uw = data;
  struct uim_wayland_v2 *v2 = uw->v2;
  (void)input_method;

  v2->pending_active = true;
  v2->pending_activate = true;
  forget_pending_surrounding_text(v2);
  v2->pending_content_hint = 0;
  v2->pending_content_purpose = 0;
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
  struct uim_wayland *uw = data;
  struct uim_wayland_v2 *v2 = uw->v2;
  (void)input_method;

  forget_pending_surrounding_text(v2);
  v2->pending_surrounding_text = uim_strdup(text);
  v2->pending_surrounding_cursor = cursor;
  v2->pending_surrounding_anchor = anchor;
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
  struct uim_wayland *uw = data;
  struct uim_wayland_v2 *v2 = uw->v2;
  (void)input_method;

  v2->pending_content_hint = hint;
  v2->pending_content_purpose = purpose;
}

/* Composing into a field that hides what is typed, or that takes only
 * digits, gives the user nothing and leaks the text into the preedit
 * of an input method that has no business seeing it. */
static bool
takes_composed_text(uint32_t hint, uint32_t purpose)
{
  if (hint & (CONTENT_HINT_HIDDEN_TEXT | CONTENT_HINT_SENSITIVE_DATA))
    return false;

  switch (purpose) {
  case CONTENT_PURPOSE_DIGITS:
  case CONTENT_PURPOSE_NUMBER:
  case CONTENT_PURPOSE_PHONE:
  case CONTENT_PURPOSE_PASSWORD:
  case CONTENT_PURPOSE_PIN:
  case CONTENT_PURPOSE_DATE:
  case CONTENT_PURPOSE_TIME:
  case CONTENT_PURPOSE_DATETIME:
    return false;
  default:
    return true;
  }
}

/* The surrounding text is set back to none after it is applied. */
static void
apply_text_input_state(struct uim_wayland *uw)
{
  struct uim_wayland_v2 *v2 = uw->v2;
  uint32_t hint = v2->pending_content_hint;
  uint32_t purpose = v2->pending_content_purpose;
  bool bypassed = !takes_composed_text(hint, purpose);

  if (bypassed != uw->bypassed)
    uim_wayland_debug("content type: hint %#x purpose %u, input method %s",
                      hint, purpose, bypassed ? "off" : "on");
  uim_wayland_set_bypassed(uw, bypassed);

  if (v2->pending_surrounding_text)
    uim_wayland_text_set_surrounding(uw, v2->pending_surrounding_text,
                                     v2->pending_surrounding_cursor,
                                     v2->pending_surrounding_anchor);
  else
    uim_wayland_text_forget_surrounding(uw);
  forget_pending_surrounding_text(v2);
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
  if (uw->focused)
    apply_text_input_state(uw);
  else
    forget_pending_surrounding_text(v2);
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
  free(v2->pending_surrounding_text);
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

/* The compositor places the popup near the text input by itself, so
 * text_input_rectangle isn't listened to. */
struct zwp_input_popup_surface_v2 *
uim_wayland_v2_get_input_popup_surface(struct uim_wayland *uw,
                                       struct wl_surface *surface)
{
  return zwp_input_method_v2_get_input_popup_surface(uw->v2->input_method,
                                                     surface);
}

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
