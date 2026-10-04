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

#include <stdlib.h>
#include <string.h>

#include "uim-wayland.h"

#include <uim/uim-util.h>

/* zwp_input_method_context_v1.preedit_styling refers to the
 * preedit_style enum of zwp_text_input_v1. */
enum preedit_style {
  PREEDIT_STYLE_DEFAULT = 0,
  PREEDIT_STYLE_HIGHLIGHT = 4,
  PREEDIT_STYLE_UNDERLINE = 5
};

/* zwp_input_method_context_v1.content_type refers to the content_hint
 * and content_purpose enums of zwp_text_input_v1. Only the values
 * uim-wayland acts on are listed. */
enum content_hint {
  CONTENT_HINT_HIDDEN_TEXT = 0x40,
  CONTENT_HINT_SENSITIVE_DATA = 0x80
};

enum content_purpose {
  CONTENT_PURPOSE_DIGITS = 2,
  CONTENT_PURPOSE_NUMBER = 3,
  CONTENT_PURPOSE_PHONE = 4,
  CONTENT_PURPOSE_PASSWORD = 8,
  CONTENT_PURPOSE_DATE = 9,
  CONTENT_PURPOSE_TIME = 10,
  CONTENT_PURPOSE_DATETIME = 11
};

struct uim_wayland_v1 {
  struct zwp_input_method_v1 *input_method;
  /* The active context. NULL while no text field is focused. */
  struct zwp_input_method_context_v1 *context;
  struct wl_keyboard *keyboard;
  /* Serial from the last commit_state event. */
  uint32_t serial;
};

/* output */

static void
commit_string(struct uim_wayland *uw, const char *str)
{
  struct uim_wayland_v1 *v1 = uw->v1;

  if (!v1->context)
    return;
  zwp_input_method_context_v1_commit_string(v1->context, v1->serial, str);
}

static uint32_t
preedit_style(int attr)
{
  if (attr & UPreeditAttr_Reverse)
    return PREEDIT_STYLE_HIGHLIGHT;
  if (attr & UPreeditAttr_UnderLine)
    return PREEDIT_STYLE_UNDERLINE;
  return PREEDIT_STYLE_DEFAULT;
}

static void
set_preedit(struct uim_wayland *uw,
            const char *text,
            uint32_t cursor,
            const struct uim_wayland_preedit_span *spans,
            size_t n_spans)
{
  struct uim_wayland_v1 *v1 = uw->v1;
  size_t i;

  if (!v1->context)
    return;
  for (i = 0; i < n_spans; i++)
    zwp_input_method_context_v1_preedit_styling(v1->context,
                                                spans[i].offset,
                                                spans[i].length,
                                                preedit_style(spans[i].attr));
  zwp_input_method_context_v1_preedit_cursor(v1->context, (int32_t)cursor);
  /* The second string is what the client commits if it loses the
   * context while the preedit is shown. */
  zwp_input_method_context_v1_preedit_string(v1->context, v1->serial,
                                             text, text);
}

static void
delete_surrounding_text(struct uim_wayland *uw, int32_t index, uint32_t length)
{
  struct uim_wayland_v1 *v1 = uw->v1;

  if (!v1->context)
    return;
  zwp_input_method_context_v1_delete_surrounding_text(v1->context,
                                                      index, length);
  /* A deletion is applied along with the commit that follows it, so
   * send one even though there is nothing to insert. */
  zwp_input_method_context_v1_commit_string(v1->context, v1->serial, "");
}

/* grabbed keyboard */

static void
keyboard_keymap(void *data,
                struct wl_keyboard *keyboard,
                uint32_t format,
                int32_t fd,
                uint32_t size)
{
  (void)keyboard;
  uim_wayland_set_keymap(data, format, fd, size);
}

static void
keyboard_enter(void *data,
               struct wl_keyboard *keyboard,
               uint32_t serial,
               struct wl_surface *surface,
               struct wl_array *keys)
{
  (void)data;
  (void)keyboard;
  (void)serial;
  (void)surface;
  (void)keys;
}

static void
keyboard_leave(void *data,
               struct wl_keyboard *keyboard,
               uint32_t serial,
               struct wl_surface *surface)
{
  (void)data;
  (void)keyboard;
  (void)serial;
  (void)surface;
}

static void
keyboard_key(void *data,
             struct wl_keyboard *keyboard,
             uint32_t serial,
             uint32_t time,
             uint32_t key,
             uint32_t state)
{
  struct uim_wayland *uw = data;
  struct uim_wayland_v1 *v1 = uw->v1;
  (void)keyboard;

  if (uim_wayland_filter_key(uw, key, state) && v1->context)
    zwp_input_method_context_v1_key(v1->context, serial, time, key, state);
}

static void
keyboard_modifiers(void *data,
                   struct wl_keyboard *keyboard,
                   uint32_t serial,
                   uint32_t mods_depressed,
                   uint32_t mods_latched,
                   uint32_t mods_locked,
                   uint32_t group)
{
  struct uim_wayland *uw = data;
  struct uim_wayland_v1 *v1 = uw->v1;
  (void)keyboard;

  uim_wayland_set_modifiers(uw, mods_depressed, mods_latched, mods_locked,
                            group);
  /* The client only receives what we forward. */
  if (v1->context)
    zwp_input_method_context_v1_modifiers(v1->context, serial,
                                          mods_depressed, mods_latched,
                                          mods_locked, group);
}

static void
keyboard_repeat_info(void *data,
                     struct wl_keyboard *keyboard,
                     int32_t rate,
                     int32_t delay)
{
  (void)data;
  (void)keyboard;
  /* A non-zero rate asks us to repeat keys ourselves, which isn't
   * implemented. A compositor that repeats keys on its own sends
   * WL_KEYBOARD_KEY_STATE_REPEATED instead, which is handled. */
  uim_wayland_debug("received repeat_info rate %d delay %d (ignored)",
                    rate, delay);
}

static const struct wl_keyboard_listener keyboard_listener = {
  keyboard_keymap,
  keyboard_enter,
  keyboard_leave,
  keyboard_key,
  keyboard_modifiers,
  keyboard_repeat_info
};

/* input method context */

static void
context_surrounding_text(void *data,
                         struct zwp_input_method_context_v1 *context,
                         const char *text,
                         uint32_t cursor,
                         uint32_t anchor)
{
  (void)context;
  uim_wayland_text_set_surrounding(data, text, cursor, anchor);
}

static void
context_reset(void *data, struct zwp_input_method_context_v1 *context)
{
  (void)context;
  uim_wayland_reset(data);
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
  case CONTENT_PURPOSE_DATE:
  case CONTENT_PURPOSE_TIME:
  case CONTENT_PURPOSE_DATETIME:
    return false;
  default:
    return true;
  }
}

static void
context_content_type(void *data,
                     struct zwp_input_method_context_v1 *context,
                     uint32_t hint,
                     uint32_t purpose)
{
  bool bypassed = !takes_composed_text(hint, purpose);
  (void)context;

  uim_wayland_debug("content type: hint %#x purpose %u, input method %s",
                    hint, purpose, bypassed ? "off" : "on");
  uim_wayland_set_bypassed(data, bypassed);
}

static void
context_invoke_action(void *data,
                      struct zwp_input_method_context_v1 *context,
                      uint32_t button,
                      uint32_t index)
{
  (void)data;
  (void)context;
  (void)button;
  (void)index;
}

static void
context_commit_state(void *data,
                     struct zwp_input_method_context_v1 *context,
                     uint32_t serial)
{
  struct uim_wayland *uw = data;
  (void)context;
  uw->v1->serial = serial;
}

static void
context_preferred_language(void *data,
                           struct zwp_input_method_context_v1 *context,
                           const char *language)
{
  (void)data;
  (void)context;
  (void)language;
}

static const struct zwp_input_method_context_v1_listener context_listener = {
  context_surrounding_text,
  context_reset,
  context_content_type,
  context_invoke_action,
  context_commit_state,
  context_preferred_language
};

/* activation */

static void
deactivate(struct uim_wayland *uw)
{
  struct uim_wayland_v1 *v1 = uw->v1;
  struct zwp_input_method_context_v1 *context = v1->context;

  if (!context)
    return;

  /* Detach first: the callbacks triggered by focus out must not talk
   * to a context that is going away. The client resets its own
   * preedit on deactivation. */
  v1->context = NULL;
  uim_wayland_deactivate(uw);
  if (v1->keyboard) {
    wl_keyboard_destroy(v1->keyboard);
    v1->keyboard = NULL;
  }
  zwp_input_method_context_v1_destroy(context);
}

static void
input_method_activate(void *data,
                      struct zwp_input_method_v1 *input_method,
                      struct zwp_input_method_context_v1 *context)
{
  struct uim_wayland *uw = data;
  struct uim_wayland_v1 *v1 = uw->v1;
  (void)input_method;

  deactivate(uw);

  v1->context = context;
  v1->serial = 0;
  zwp_input_method_context_v1_add_listener(context, &context_listener, uw);

  v1->keyboard = zwp_input_method_context_v1_grab_keyboard(context);
  wl_keyboard_add_listener(v1->keyboard, &keyboard_listener, uw);

  uim_wayland_activate(uw);
}

static void
input_method_deactivate(void *data,
                        struct zwp_input_method_v1 *input_method,
                        struct zwp_input_method_context_v1 *context)
{
  struct uim_wayland *uw = data;
  (void)input_method;

  if (context != uw->v1->context) {
    zwp_input_method_context_v1_destroy(context);
    return;
  }
  deactivate(uw);
}

static const struct zwp_input_method_v1_listener input_method_listener = {
  input_method_activate,
  input_method_deactivate
};

static void
destroy(struct uim_wayland *uw)
{
  if (uw->input_panel) {
    zwp_input_panel_v1_destroy(uw->input_panel);
    uw->input_panel = NULL;
  }
  if (uw->v1) {
    if (uw->v1->input_method)
      zwp_input_method_v1_destroy(uw->v1->input_method);
    free(uw->v1);
    uw->v1 = NULL;
  }
}

static const struct uim_wayland_input_method input_method_v1 = {
  commit_string,
  set_preedit,
  delete_surrounding_text,
  deactivate,
  destroy
};

bool
uim_wayland_v1_bind(struct uim_wayland *uw,
                    struct wl_registry *registry,
                    uint32_t name,
                    const char *interface)
{
  if (strcmp(interface, zwp_input_method_v1_interface.name) == 0) {
    if (!uw->v1) {
      uw->v1 = uim_malloc(sizeof(*uw->v1));
      memset(uw->v1, 0, sizeof(*uw->v1));
    }
    uw->v1->input_method = wl_registry_bind(registry, name,
                                            &zwp_input_method_v1_interface,
                                            1);
    zwp_input_method_v1_add_listener(uw->v1->input_method,
                                     &input_method_listener, uw);
    uw->input_method = &input_method_v1;
    return true;
  }
  if (strcmp(interface, zwp_input_panel_v1_interface.name) == 0) {
    uw->input_panel = wl_registry_bind(registry, name,
                                       &zwp_input_panel_v1_interface, 1);
    return true;
  }
  return false;
}
