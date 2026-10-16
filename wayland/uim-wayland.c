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
#include <fcntl.h>
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
/* The handler may run on a GLib thread, so it wakes up poll() in the
 * main thread through this pipe. */
static int terminate_pipe[2] = {-1, -1};
static bool debug_enabled = false;

void
uim_wayland_debug(const char *format, ...)
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
  int saved_errno = errno;

  terminate_requested = sig;
  if (terminate_pipe[1] >= 0 && write(terminate_pipe[1], "", 1) < 0) {
    /* Full already: poll() wakes up anyway. */
  }
  errno = saved_errno;
}

static bool
open_terminate_pipe(void)
{
  int i;

  if (pipe(terminate_pipe) < 0)
    return false;
  for (i = 0; i < 2; i++) {
    fcntl(terminate_pipe[i], F_SETFD, FD_CLOEXEC);
    fcntl(terminate_pipe[i], F_SETFL,
          fcntl(terminate_pipe[i], F_GETFL) | O_NONBLOCK);
  }
  return true;
}

/* pressed key bookkeeping */

static void
set_key_bit(uint8_t *bits, uint32_t key, bool on)
{
  if (key >= UIM_WAYLAND_MAX_KEYCODE)
    return;
  if (on)
    bits[key / 8] |= 1 << (key % 8);
  else
    bits[key / 8] &= ~(1 << (key % 8));
}

static bool
key_bit(const uint8_t *bits, uint32_t key, bool fallback)
{
  if (key >= UIM_WAYLAND_MAX_KEYCODE)
    return fallback;
  return (bits[key / 8] & (1 << (key % 8))) != 0;
}

static void
set_forwarded(struct uim_wayland *uw, uint32_t key, bool forwarded)
{
  set_key_bit(uw->forwarded_keys, key, forwarded);
}

static bool
was_forwarded(struct uim_wayland *uw, uint32_t key)
{
  return key_bit(uw->forwarded_keys, key, true);
}

static void
set_bypassed(struct uim_wayland *uw, uint32_t key, bool bypassed)
{
  set_key_bit(uw->bypassed_keys, key, bypassed);
}

static bool
was_bypassed(struct uim_wayland *uw, uint32_t key)
{
  return key_bit(uw->bypassed_keys, key, uw->bypassed);
}

/* text output */

void
uim_wayland_commit_string(struct uim_wayland *uw, const char *str)
{
  if (!uw->focused || !str || str[0] == '\0')
    return;
  uw->input_method->commit_string(uw, str);
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

static void
preedit_update_cb(void *ptr)
{
  struct uim_wayland *uw = ptr;
  size_t capacity = 256;
  size_t length = 0;
  char *text;
  struct uim_wayland_preedit_span *spans;
  size_t n_spans = 0;
  int cursor = -1;
  size_t i;

  if (!uw->focused)
    return;

  text = uim_malloc(capacity);
  text[0] = '\0';
  spans = uim_malloc(sizeof(*spans) * (uw->n_segments + 1));
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
    spans[n_spans].offset = (uint32_t)length;
    spans[n_spans].length = (uint32_t)str_length;
    spans[n_spans].attr = segment->attr;
    n_spans++;
    length += str_length;
    text[length] = '\0';
  }

  if (length == 0 && !uw->preedit_shown) {
    free(spans);
    free(text);
    return;
  }

  if (cursor < 0)
    cursor = (int)length;
  uw->input_method->set_preedit(uw, text, (uint32_t)cursor, spans, n_spans);
  uw->preedit_shown = length > 0;
  free(spans);
  free(text);
}

/* candidate window */

static void
cand_activate_cb(void *ptr, int nr, int display_limit)
{
  struct uim_wayland *uw = ptr;
  if (uw->focused)
    uim_wayland_candwin_activate(uw->candwin, nr, display_limit);
}

static void
cand_select_cb(void *ptr, int index)
{
  struct uim_wayland *uw = ptr;
  if (uw->focused)
    uim_wayland_candwin_select(uw->candwin, index);
}

static void
cand_shift_page_cb(void *ptr, int direction)
{
  struct uim_wayland *uw = ptr;
  if (uw->focused)
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

/* keyboard */

void
uim_wayland_set_keymap(struct uim_wayland *uw,
                       uint32_t format,
                       int32_t fd,
                       uint32_t size)
{
  char *map;

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
  uim_wayland_debug("received keymap (%u bytes)", size);
}

static void
release_keymap(struct uim_wayland *uw)
{
  if (uw->xkb_state) {
    xkb_state_unref(uw->xkb_state);
    uw->xkb_state = NULL;
  }
  if (uw->xkb_keymap) {
    xkb_keymap_unref(uw->xkb_keymap);
    uw->xkb_keymap = NULL;
  }
}

bool
uim_wayland_filter_key(struct uim_wayland *uw, uint32_t key, uint32_t state)
{
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

  uim_wayland_debug("received key %u %s", key,
                    pressed ? "pressed" : "released");
  if (!uw->focused)
    return false;

  if (uw->xkb_state)
    sym = xkb_state_key_get_one_sym(uw->xkb_state, code);
  uim_wayland_convert_key(sym, uw->xkb_state, &ukey, &umod);

  /* The field may change what it takes while a key is held, so both
   * the client and uim get a release only when they got its press. */
  if (pressed) {
    set_bypassed(uw, key, uw->bypassed);
    if (uw->bypassed) {
      forward = true;
    } else {
      pass_through = uim_press_key(uw->uc, ukey, umod);
      forward = pass_through != 0;
    }
    set_forwarded(uw, key, forward);
  } else {
    if (!was_bypassed(uw, key)) {
      pass_through = uim_release_key(uw->uc, ukey, umod);
      (void)pass_through;
    }
    /* A release goes wherever its press went, so the client never
     * sees an unbalanced key. */
    forward = was_forwarded(uw, key);
  }

  uim_wayland_debug("key %u (sym %#x ukey %d umod %#x) %s: %s",
                    key, sym, ukey, umod,
                    pressed ? "pressed" : "released",
                    forward ? "forwarded" : "consumed");
  return forward;
}

void
uim_wayland_set_modifiers(struct uim_wayland *uw,
                          uint32_t mods_depressed,
                          uint32_t mods_latched,
                          uint32_t mods_locked,
                          uint32_t group)
{
  uim_wayland_debug("received modifiers %#x/%#x/%#x group %u",
                    mods_depressed, mods_latched, mods_locked, group);
  if (uw->xkb_state)
    xkb_state_update_mask(uw->xkb_state, mods_depressed, mods_latched,
                          mods_locked, 0, 0, group);
}

/* text field */

void
uim_wayland_reset(struct uim_wayland *uw)
{
  uim_reset_context(uw->uc);
  uim_wayland_text_forget_surrounding(uw);
  clear_segments(uw);
  preedit_update_cb(uw);
}

void
uim_wayland_set_bypassed(struct uim_wayland *uw, bool bypassed)
{
  if (bypassed == uw->bypassed)
    return;
  uw->bypassed = bypassed;
  if (!bypassed)
    return;

  /* Whatever is being composed would end up in a field that doesn't
   * take it, so it is dropped. The field may have been composed into
   * before it said what it takes, or it may have changed what it takes
   * while focused, when a password is hidden again for instance. */
  uim_wayland_candwin_deactivate(uw->candwin);
  uim_reset_context(uw->uc);
  clear_segments(uw);
  preedit_update_cb(uw);
}

void
uim_wayland_activate(struct uim_wayland *uw)
{
  uw->focused = true;
  /* A field says what it takes only after it is activated. */
  uw->bypassed = false;
  uw->preedit_shown = false;
  memset(uw->forwarded_keys, 0, sizeof(uw->forwarded_keys));
  memset(uw->bypassed_keys, 0, sizeof(uw->bypassed_keys));

  uim_wayland_helper_focus_in(uw);
  uim_focus_in_context(uw->uc);
  uim_prop_list_update(uw->uc);
  uim_wayland_debug("activated, input method: %s",
                    uim_get_current_im_name(uw->uc));
}

void
uim_wayland_deactivate(struct uim_wayland *uw)
{
  uim_wayland_debug("deactivated");
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
  release_keymap(uw);
}

/* seat */

static void
release_pointer(struct uim_wayland *uw)
{
  if (!uw->pointer)
    return;
  if (wl_pointer_get_version(uw->pointer) >= WL_POINTER_RELEASE_SINCE_VERSION)
    wl_pointer_release(uw->pointer);
  else
    wl_pointer_destroy(uw->pointer);
  uw->pointer = NULL;
}

static void
seat_capabilities(void *data, struct wl_seat *seat, uint32_t capabilities)
{
  struct uim_wayland *uw = data;
  bool has_pointer = (capabilities & WL_SEAT_CAPABILITY_POINTER) != 0;

  if (has_pointer && !uw->pointer) {
    uw->pointer = wl_seat_get_pointer(seat);
    wl_pointer_add_listener(uw->pointer,
                            &uim_wayland_candwin_pointer_listener, uw);
  } else if (!has_pointer) {
    release_pointer(uw);
  }
}

static void
seat_name(void *data, struct wl_seat *seat, const char *name)
{
  (void)data;
  (void)seat;
  (void)name;
}

static const struct wl_seat_listener seat_listener = {
  seat_capabilities,
  seat_name
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

  if (uim_wayland_v1_bind(uw, registry, name, interface) ||
      uim_wayland_v2_offer(uw, name, interface))
    return;
  if (strcmp(interface, wl_compositor_interface.name) == 0) {
    if (version > UIM_WAYLAND_COMPOSITOR_VERSION)
      version = UIM_WAYLAND_COMPOSITOR_VERSION;
    uw->compositor = wl_registry_bind(registry, name,
                                      &wl_compositor_interface, version);
  } else if (strcmp(interface, wl_shm_interface.name) == 0) {
    uw->shm = wl_registry_bind(registry, name, &wl_shm_interface, 1);
  } else if (strcmp(interface, wl_seat_interface.name) == 0 && !uw->seat) {
    /* Only the first seat. */
    uw->seat = wl_registry_bind(registry, name, &wl_seat_interface,
                                version < 5 ? version : 5);
    wl_seat_add_listener(uw->seat, &seat_listener, uw);
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
    struct pollfd fds[3];
    int nfds = 2;
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
    fds[1].fd = terminate_pipe[0];
    fds[1].events = POLLIN;
    fds[1].revents = 0;
    if (uw->helper_fd >= 0) {
      fds[2].fd = uw->helper_fd;
      fds[2].events = POLLIN;
      fds[2].revents = 0;
      nfds = 3;
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

    if (nfds == 3 && fds[2].revents) {
      if (fds[2].revents & (POLLERR | POLLNVAL))
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
          "zwp_input_method_v1, such as KWin and Weston, which start this\n"
          "program themselves, or zwp_input_method_v2, such as Sway; see\n"
          "doc/uim-wayland.md.\n",
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
  uim_wayland_debug("default input method: %s", im_name);
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
  /* zwp_input_method_v1 is offered only to the input method the
   * compositor started, which is what it is meant for. */
  if (!uw->input_method)
    uim_wayland_v2_start(uw);

  if (!uw->input_method) {
    fprintf(stderr,
            "%s: the compositor offers this process neither\n"
            "zwp_input_method_v1 nor zwp_input_method_v2 with\n"
            "zwp_virtual_keyboard_v1. KWin and Weston must start %s\n"
            "themselves as their input method; see doc/uim-wayland.md.\n",
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

  if (!open_terminate_pipe()) {
    fprintf(stderr, "%s: cannot create a pipe: %s\n",
            UIM_WAYLAND_PROGRAM_NAME, strerror(errno));
    return EXIT_FAILURE;
  }
  memset(&action, 0, sizeof(action));
  sigemptyset(&action.sa_mask);
  action.sa_handler = terminate_handler;
  sigaction(SIGINT, &action, NULL);
  sigaction(SIGTERM, &action, NULL);
  sigaction(SIGHUP, &action, NULL);

  status = run(uw);

  uw->input_method->deactivate(uw);
  uim_wayland_helper_disconnect(uw);
  /* Release the uim context first: a Scheme release handler can still
   * reach the candidate window callbacks. */
  uim_release_context(uw->uc);
  uim_wayland_candwin_free(uw->candwin);
  uw->candwin = NULL;
  uim_quit();
  free(uw->segments);
  free(uw->surrounding_text);
  release_pointer(uw);
  if (uw->seat) {
    if (wl_seat_get_version(uw->seat) >= WL_SEAT_RELEASE_SINCE_VERSION)
      wl_seat_release(uw->seat);
    else
      wl_seat_destroy(uw->seat);
  }
  uw->input_method->destroy(uw);
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
