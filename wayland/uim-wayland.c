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

#include <errno.h>
#include <locale.h>
#include <poll.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "uim-wayland.h"

#include <uim/uim-helper.h>
#include <uim/uim-im-switcher.h>
#include <uim/uim-util.h>


static volatile sig_atomic_t terminate_requested = 0;
static bool debug_enabled = false;

static void debug(const char *format, ...)
  __attribute__((format(printf, 1, 2)));

static void
debug(const char *format, ...)
{
  va_list args;

  if (!debug_enabled)
    return;
  fprintf(stderr, "%s: ", UIM_WAYLAND_PROGRAM_NAME);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputc('\n', stderr);
}

static void
terminate_handler(int sig)
{
  terminate_requested = sig;
}

/* forwarded key bookkeeping */

static void
set_forwarded(struct uim_wayland *uw, uint32_t key, bool forwarded)
{
  if (key >= UIM_WAYLAND_MAX_KEYCODE)
    return;
  if (forwarded)
    uw->forwarded_keys[key / 8] |= 1 << (key % 8);
  else
    uw->forwarded_keys[key / 8] &= ~(1 << (key % 8));
}

static bool
was_forwarded(struct uim_wayland *uw, uint32_t key)
{
  if (key >= UIM_WAYLAND_MAX_KEYCODE)
    return true;
  return (uw->forwarded_keys[key / 8] & (1 << (key % 8))) != 0;
}

/* text output */

void
uim_wayland_commit_string(struct uim_wayland *uw, const char *str)
{
  if (!uw->context || !str || str[0] == '\0')
    return;
  zwp_input_method_context_v1_commit_string(uw->context, uw->serial, str);
}

static void
commit_cb(void *ptr, const char *str)
{
  struct uim_wayland *uw = ptr;
  uim_wayland_commit_string(uw, str);
}

static void
clear_segments(struct uim_wayland *uw)
{
  size_t i;
  for (i = 0; i < uw->n_segments; i++)
    free(uw->segments[i].str);
  uw->n_segments = 0;
}

static void
preedit_clear_cb(void *ptr)
{
  struct uim_wayland *uw = ptr;
  clear_segments(uw);
}

static void
preedit_pushback_cb(void *ptr, int attr, const char *str)
{
  struct uim_wayland *uw = ptr;
  struct uim_wayland_preedit_segment *segment;

  if (!str)
    return;
  if (str[0] == '\0' &&
      !(attr & (UPreeditAttr_Cursor | UPreeditAttr_Separator)))
    return;

  if (uw->n_segments == uw->segments_capacity) {
    uw->segments_capacity = uw->segments_capacity ? uw->segments_capacity * 2 : 8;
    uw->segments = uim_realloc(uw->segments,
                               sizeof(*uw->segments) * uw->segments_capacity);
  }
  segment = &uw->segments[uw->n_segments++];
  segment->attr = attr;
  segment->str = uim_strdup(str);
}

static uint32_t
preedit_style(int attr)
{
  if (attr & UPreeditAttr_Reverse)
    return UIM_WAYLAND_PREEDIT_STYLE_HIGHLIGHT;
  if (attr & UPreeditAttr_UnderLine)
    return UIM_WAYLAND_PREEDIT_STYLE_UNDERLINE;
  return UIM_WAYLAND_PREEDIT_STYLE_DEFAULT;
}

static void
preedit_update_cb(void *ptr)
{
  struct uim_wayland *uw = ptr;
  size_t capacity = 256;
  size_t length = 0;
  char *text;
  int cursor = -1;
  size_t i;

  if (!uw->context)
    return;

  text = uim_malloc(capacity);
  text[0] = '\0';
  for (i = 0; i < uw->n_segments; i++) {
    struct uim_wayland_preedit_segment *segment = &uw->segments[i];
    const char *str = segment->str;
    size_t str_length;

    if (segment->attr & UPreeditAttr_Cursor)
      cursor = (int)length;
    if ((segment->attr & UPreeditAttr_Separator) && str[0] == '\0')
      str = "|";
    str_length = strlen(str);
    if (str_length == 0)
      continue;
    if (length + str_length + 1 > capacity) {
      while (length + str_length + 1 > capacity)
        capacity *= 2;
      text = uim_realloc(text, capacity);
    }
    memcpy(text + length, str, str_length);
    zwp_input_method_context_v1_preedit_styling(uw->context,
                                                (uint32_t)length,
                                                (uint32_t)str_length,
                                                preedit_style(segment->attr));
    length += str_length;
    text[length] = '\0';
  }

  if (length == 0 && !uw->preedit_shown) {
    free(text);
    return;
  }

  if (cursor < 0)
    cursor = (int)length;
  zwp_input_method_context_v1_preedit_cursor(uw->context, cursor);
  /* The second string is what the client commits if it loses the
   * context while the preedit is shown. */
  zwp_input_method_context_v1_preedit_string(uw->context, uw->serial,
                                             text, text);
  uw->preedit_shown = length > 0;
  free(text);
}

/* candidate window */

static void
cand_activate_cb(void *ptr, int nr, int display_limit)
{
  struct uim_wayland *uw = ptr;
  if (uw->context)
    uim_wayland_candwin_activate(uw->candwin, nr, display_limit);
}

static void
cand_select_cb(void *ptr, int index)
{
  struct uim_wayland *uw = ptr;
  if (uw->context)
    uim_wayland_candwin_select(uw->candwin, index);
}

static void
cand_shift_page_cb(void *ptr, int direction)
{
  struct uim_wayland *uw = ptr;
  if (uw->context)
    uim_wayland_candwin_shift_page(uw->candwin, direction != 0);
}

static void
cand_deactivate_cb(void *ptr)
{
  struct uim_wayland *uw = ptr;
  uim_wayland_candwin_deactivate(uw->candwin);
}

/* properties and IM switching */

static void
prop_list_update_cb(void *ptr, const char *str)
{
  struct uim_wayland *uw = ptr;
  char *message;

  uim_asprintf(&message, "prop_list_update\ncharset=UTF-8\n%s", str);
  uim_wayland_helper_send(uw, message);
  free(message);
}

static void
configuration_changed_cb(void *ptr)
{
  struct uim_wayland *uw = ptr;

  /* The list tells the toolbar which input method is in use, so only
   * publish it while we own the input. */
  if (!uw->focused)
    return;
  uim_wayland_helper_send_im_list(uw);
}

static void
update_default_im(struct uim_wayland *uw, const char *name)
{
  char *sym;
  uim_asprintf(&sym, "'%s", name);
  uim_prop_update_custom(uw->uc, "custom-preserved-default-im-name", sym);
  free(sym);
}

static void
switch_app_global_im_cb(void *ptr, const char *name)
{
  struct uim_wayland *uw = ptr;

  if (!uw->focused)
    return;
  /* There is a single context in this process, so there is nothing
   * else to switch and nothing to tell the other processes: an
   * im_change_this_application_only would switch whichever of them
   * believes it is focused. */
  update_default_im(uw, name);
}

static void
switch_system_global_im_cb(void *ptr, const char *name)
{
  struct uim_wayland *uw = ptr;
  char *message;

  update_default_im(uw, name);
  /* Other processes switch on this message; the helper server does
   * not reflect it back to us. */
  uim_asprintf(&message, "im_change_whole_desktop\n%s\n", name);
  uim_wayland_helper_send(uw, message);
  free(message);
}

/* grabbed keyboard */

static void
keyboard_keymap(void *data,
                struct wl_keyboard *keyboard,
                uint32_t format,
                int32_t fd,
                uint32_t size)
{
  struct uim_wayland *uw = data;
  char *map;
  (void)keyboard;

  if (format != WL_KEYBOARD_KEYMAP_FORMAT_XKB_V1) {
    close(fd);
    return;
  }
  map = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
  close(fd);
  if (map == MAP_FAILED) {
    fprintf(stderr, "%s: cannot mmap keymap: %s\n",
            UIM_WAYLAND_PROGRAM_NAME, strerror(errno));
    return;
  }

  if (uw->xkb_state)
    xkb_state_unref(uw->xkb_state);
  if (uw->xkb_keymap)
    xkb_keymap_unref(uw->xkb_keymap);
  uw->xkb_keymap = xkb_keymap_new_from_string(uw->xkb_context, map,
                                              XKB_KEYMAP_FORMAT_TEXT_V1,
                                              XKB_KEYMAP_COMPILE_NO_FLAGS);
  munmap(map, size);
  if (!uw->xkb_keymap) {
    fprintf(stderr, "%s: cannot compile keymap\n", UIM_WAYLAND_PROGRAM_NAME);
    uw->xkb_state = NULL;
    return;
  }
  uw->xkb_state = xkb_state_new(uw->xkb_keymap);
  debug("received keymap (%u bytes)", size);
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
  /* A compositor may send WL_KEYBOARD_KEY_STATE_REPEATED (wl_keyboard
   * version 10) for an auto-repeating key. Anything that isn't a
   * release is a press as far as uim is concerned; testing for
   * "pressed" instead would turn every repeat into an unbalanced
   * uim_release_key(). */
  const bool pressed = state != WL_KEYBOARD_KEY_STATE_RELEASED;
  xkb_keycode_t code = key + 8; /* evdev to xkb */
  xkb_keysym_t sym = XKB_KEY_NoSymbol;
  int ukey, umod;
  int pass_through;
  bool forward;
  (void)keyboard;

  debug("received key %u %s", key, pressed ? "pressed" : "released");
  if (!uw->context)
    return;

  if (uw->xkb_state)
    sym = xkb_state_key_get_one_sym(uw->xkb_state, code);
  uim_wayland_convert_key(sym, uw->xkb_state, &ukey, &umod);

  if (pressed) {
    pass_through = uim_press_key(uw->uc, ukey, umod);
    forward = pass_through != 0;
    set_forwarded(uw, key, forward);
  } else {
    pass_through = uim_release_key(uw->uc, ukey, umod);
    (void)pass_through;
    /* A release goes wherever its press went, so the client never
     * sees an unbalanced key. */
    forward = was_forwarded(uw, key);
  }

  debug("key %u (sym %#x ukey %d umod %#x) %s: %s", key, sym, ukey, umod,
        pressed ? "pressed" : "released",
        forward ? "forwarded" : "consumed");
  if (forward) {
    zwp_input_method_context_v1_key(uw->context, serial, time, key, state);
  }
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
  (void)keyboard;

  debug("received modifiers %#x/%#x/%#x group %u",
        mods_depressed, mods_latched, mods_locked, group);
  if (uw->xkb_state)
    xkb_state_update_mask(uw->xkb_state, mods_depressed, mods_latched,
                          mods_locked, 0, 0, group);
  /* The client only receives what we forward. */
  if (uw->context)
    zwp_input_method_context_v1_modifiers(uw->context, serial,
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
  debug("received repeat_info rate %d delay %d (ignored)", rate, delay);
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
  struct uim_wayland *uw = data;
  (void)context;

  uim_wayland_text_set_surrounding(uw, text, cursor, anchor);
}

static void
context_reset(void *data, struct zwp_input_method_context_v1 *context)
{
  struct uim_wayland *uw = data;
  (void)context;

  uim_reset_context(uw->uc);
  uim_wayland_text_forget_surrounding(uw);
  clear_segments(uw);
  preedit_update_cb(uw);
}

static void
context_content_type(void *data,
                     struct zwp_input_method_context_v1 *context,
                     uint32_t hint,
                     uint32_t purpose)
{
  (void)data;
  (void)context;
  (void)hint;
  (void)purpose;
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
  uw->serial = serial;
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
release_keyboard(struct uim_wayland *uw)
{
  if (uw->keyboard) {
    wl_keyboard_destroy(uw->keyboard);
    uw->keyboard = NULL;
  }
  if (uw->xkb_state) {
    xkb_state_unref(uw->xkb_state);
    uw->xkb_state = NULL;
  }
  if (uw->xkb_keymap) {
    xkb_keymap_unref(uw->xkb_keymap);
    uw->xkb_keymap = NULL;
  }
}

static void
deactivate(struct uim_wayland *uw)
{
  struct zwp_input_method_context_v1 *context = uw->context;

  if (!context)
    return;

  debug("deactivated");
  /* Detach first: the callbacks triggered by focus out must not talk
   * to a context that is going away. The client resets its own
   * preedit on deactivation. */
  uw->context = NULL;
  uw->focused = false;
  uim_wayland_candwin_deactivate(uw->candwin);
  uim_focus_out_context(uw->uc);
  /* We can't tell the client anything after deactivation, and clients
   * differ in what they do with a pending preedit (Chromium commits
   * it, others drop it). Drop it on our side too, so it doesn't show
   * up again in the next text field. */
  uim_reset_context(uw->uc);
  uim_wayland_helper_focus_out(uw);
  /* The text belonged to the field we are leaving. */
  uim_wayland_text_forget_surrounding(uw);
  clear_segments(uw);
  uw->preedit_shown = false;

  release_keyboard(uw);
  zwp_input_method_context_v1_destroy(context);
}

static void
input_method_activate(void *data,
                      struct zwp_input_method_v1 *input_method,
                      struct zwp_input_method_context_v1 *context)
{
  struct uim_wayland *uw = data;
  (void)input_method;

  deactivate(uw);

  uw->context = context;
  uw->serial = 0;
  uw->focused = true;
  uw->preedit_shown = false;
  memset(uw->forwarded_keys, 0, sizeof(uw->forwarded_keys));
  zwp_input_method_context_v1_add_listener(context, &context_listener, uw);

  uw->keyboard = zwp_input_method_context_v1_grab_keyboard(context);
  wl_keyboard_add_listener(uw->keyboard, &keyboard_listener, uw);

  uim_wayland_helper_focus_in(uw);
  uim_focus_in_context(uw->uc);
  uim_prop_list_update(uw->uc);
  debug("activated, input method: %s", uim_get_current_im_name(uw->uc));
}

static void
input_method_deactivate(void *data,
                        struct zwp_input_method_v1 *input_method,
                        struct zwp_input_method_context_v1 *context)
{
  struct uim_wayland *uw = data;
  (void)input_method;

  if (context != uw->context) {
    zwp_input_method_context_v1_destroy(context);
    return;
  }
  deactivate(uw);
}

static const struct zwp_input_method_v1_listener input_method_listener = {
  input_method_activate,
  input_method_deactivate
};

/* registry */

static void
registry_global(void *data,
                struct wl_registry *registry,
                uint32_t name,
                const char *interface,
                uint32_t version)
{
  struct uim_wayland *uw = data;

  if (strcmp(interface, wl_compositor_interface.name) == 0) {
    uw->compositor = wl_registry_bind(registry, name,
                                      &wl_compositor_interface,
                                      version < 4 ? version : 4);
  } else if (strcmp(interface, wl_shm_interface.name) == 0) {
    uw->shm = wl_registry_bind(registry, name, &wl_shm_interface, 1);
  } else if (strcmp(interface, zwp_input_method_v1_interface.name) == 0) {
    uw->input_method = wl_registry_bind(registry, name,
                                        &zwp_input_method_v1_interface, 1);
    zwp_input_method_v1_add_listener(uw->input_method,
                                     &input_method_listener, uw);
  } else if (strcmp(interface, zwp_input_panel_v1_interface.name) == 0) {
    uw->input_panel = wl_registry_bind(registry, name,
                                       &zwp_input_panel_v1_interface, 1);
  }
}

static void
registry_global_remove(void *data, struct wl_registry *registry, uint32_t name)
{
  (void)data;
  (void)registry;
  (void)name;
}

static const struct wl_registry_listener registry_listener = {
  registry_global,
  registry_global_remove
};

/* main loop */

static int
run(struct uim_wayland *uw)
{
  int display_fd = wl_display_get_fd(uw->display);

  uw->running = true;
  while (uw->running && !terminate_requested) {
    struct pollfd fds[2];
    int nfds = 1;
    short flush_events;
    int ret;

    while (wl_display_prepare_read(uw->display) != 0) {
      if (wl_display_dispatch_pending(uw->display) < 0)
        return EXIT_FAILURE;
    }
    /* EAGAIN means the compositor's socket is full and requests are
     * still queued. Waiting for POLLIN alone would leave them unsent
     * until some unrelated event arrives, so wait for the socket to
     * become writable as well and flush again on the next round. */
    flush_events = POLLIN;
    if (wl_display_flush(uw->display) < 0) {
      if (errno == EAGAIN) {
        flush_events |= POLLOUT;
      } else {
        wl_display_cancel_read(uw->display);
        break;
      }
    }

    fds[0].fd = display_fd;
    fds[0].events = flush_events;
    fds[0].revents = 0;
    if (uw->helper_fd >= 0) {
      fds[1].fd = uw->helper_fd;
      fds[1].events = POLLIN;
      fds[1].revents = 0;
      nfds = 2;
    }
    ret = poll(fds, nfds, -1);
    if (ret < 0) {
      wl_display_cancel_read(uw->display);
      if (errno == EINTR)
        continue;
      fprintf(stderr, "%s: poll failed: %s\n",
              UIM_WAYLAND_PROGRAM_NAME, strerror(errno));
      return EXIT_FAILURE;
    }

    if (fds[0].revents & POLLIN) {
      if (wl_display_read_events(uw->display) < 0)
        break;
    } else {
      wl_display_cancel_read(uw->display);
    }
    if (wl_display_dispatch_pending(uw->display) < 0)
      break;

    if (nfds == 2 && fds[1].revents) {
      if (fds[1].revents & (POLLERR | POLLNVAL))
        uim_wayland_helper_disconnect(uw);
      else
        uim_wayland_helper_dispatch(uw);
    }
  }

  if (wl_display_get_error(uw->display) != 0) {
    fprintf(stderr, "%s: disconnected from the compositor\n",
            UIM_WAYLAND_PROGRAM_NAME);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

static void
usage(FILE *stream)
{
  fprintf(stream,
          "Usage: %s [-h|--help] [-v|--version]\n"
          "\n"
          "Input method for Wayland compositors that implement\n"
          "zwp_input_method_v1, such as KWin and Weston. The compositor\n"
          "starts this program itself; see doc/uim-wayland.md.\n",
          UIM_WAYLAND_PROGRAM_NAME);
}

int
main(int argc, char **argv)
{
  struct uim_wayland *uw;
  struct sigaction action;
  const char *im_name;
  int status;

  /* libuim talks to input method helper processes over pipes and
   * doesn't guard those writes: uim_helper_send_message() only ignores
   * SIGPIPE around its own write, and the Scheme side doesn't at all.
   * A helper that dies would take this process down with it. The
   * stdout and stderr the compositor gave us can go away too. The
   * Wayland socket is safe on its own, libwayland sends with
   * MSG_NOSIGNAL. */
  signal(SIGPIPE, SIG_IGN);

  if (argc >= 2) {
    if (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0) {
      usage(stdout);
      return EXIT_SUCCESS;
    }
    if (strcmp(argv[1], "-v") == 0 || strcmp(argv[1], "--version") == 0) {
      printf("%s %s\n", UIM_WAYLAND_PROGRAM_NAME, PACKAGE_VERSION);
      return EXIT_SUCCESS;
    }
    usage(stderr);
    return EXIT_FAILURE;
  }

  setlocale(LC_ALL, "");
  debug_enabled = getenv("UIM_WAYLAND_DEBUG") != NULL;

  uw = uim_malloc(sizeof(*uw));
  memset(uw, 0, sizeof(*uw));
  uw->helper_fd = -1;

  /* uim must be ready before the first roundtrip: the compositor may
   * activate us as soon as we bind zwp_input_method_v1. */
  if (uim_init() < 0) {
    fprintf(stderr, "%s: uim_init() failed\n", UIM_WAYLAND_PROGRAM_NAME);
    return EXIT_FAILURE;
  }
  /* The returned string is only valid until the next libuim call. */
  im_name = uim_get_default_im_name(setlocale(LC_CTYPE, NULL));
  debug("default input method: %s", im_name);
  uw->uc = uim_create_context(uw, "UTF-8", NULL, im_name, uim_iconv,
                              commit_cb);
  if (!uw->uc) {
    fprintf(stderr, "%s: cannot create uim context\n",
            UIM_WAYLAND_PROGRAM_NAME);
    return EXIT_FAILURE;
  }
  uim_set_preedit_cb(uw->uc, preedit_clear_cb, preedit_pushback_cb,
                     preedit_update_cb);
  uim_set_candidate_selector_cb(uw->uc, cand_activate_cb, cand_select_cb,
                                cand_shift_page_cb, cand_deactivate_cb);
  uim_set_prop_list_update_cb(uw->uc, prop_list_update_cb);
  uim_set_text_acquisition_cb(uw->uc, uim_wayland_text_acquire,
                              uim_wayland_text_delete);
  uim_set_configuration_changed_cb(uw->uc, configuration_changed_cb);
  uim_set_im_switch_request_cb(uw->uc, switch_app_global_im_cb,
                               switch_system_global_im_cb);
  uim_wayland_helper_connect(uw);

  uw->xkb_context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
  if (!uw->xkb_context) {
    fprintf(stderr, "%s: cannot create xkb context\n",
            UIM_WAYLAND_PROGRAM_NAME);
    return EXIT_FAILURE;
  }

  uw->display = wl_display_connect(NULL);
  if (!uw->display) {
    fprintf(stderr, "%s: cannot connect to the Wayland display\n",
            UIM_WAYLAND_PROGRAM_NAME);
    return EXIT_FAILURE;
  }
  uw->registry = wl_display_get_registry(uw->display);
  wl_registry_add_listener(uw->registry, &registry_listener, uw);
  wl_display_roundtrip(uw->display);

  if (!uw->input_method) {
    fprintf(stderr,
            "%s: the compositor doesn't offer zwp_input_method_v1 to this\n"
            "process. The compositor must start %s itself as its input\n"
            "method; see doc/uim-wayland.md.\n",
            UIM_WAYLAND_PROGRAM_NAME, UIM_WAYLAND_PROGRAM_NAME);
    return EXIT_FAILURE;
  }
  if (!uw->compositor || !uw->shm || !uw->input_panel) {
    /* Everything but the candidate window still works. */
    fprintf(stderr,
            "%s: warning: no %s, candidates won't be shown\n",
            UIM_WAYLAND_PROGRAM_NAME,
            !uw->input_panel ? "zwp_input_panel_v1" : "wl_compositor/wl_shm");
  }

  uw->candwin = uim_wayland_candwin_new(uw);

  memset(&action, 0, sizeof(action));
  sigemptyset(&action.sa_mask);
  action.sa_handler = terminate_handler;
  sigaction(SIGINT, &action, NULL);
  sigaction(SIGTERM, &action, NULL);
  sigaction(SIGHUP, &action, NULL);

  status = run(uw);

  deactivate(uw);
  uim_wayland_helper_disconnect(uw);
  /* Release the uim context first: a Scheme release handler can still
   * reach the candidate window callbacks. */
  uim_release_context(uw->uc);
  uim_wayland_candwin_free(uw->candwin);
  uw->candwin = NULL;
  uim_quit();
  free(uw->segments);
  free(uw->surrounding_text);
  if (uw->input_panel)
    zwp_input_panel_v1_destroy(uw->input_panel);
  zwp_input_method_v1_destroy(uw->input_method);
  if (uw->shm)
    wl_shm_destroy(uw->shm);
  if (uw->compositor)
    wl_compositor_destroy(uw->compositor);
  wl_registry_destroy(uw->registry);
  xkb_context_unref(uw->xkb_context);
  wl_display_disconnect(uw->display);
  free(uw);

  return status;
}
