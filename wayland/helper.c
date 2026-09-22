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

/* Communication with uim-helper-server: toolbar, IM switcher and so on. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <string.h>

#include "uim-wayland.h"

#include <uim/uim-helper.h>
#include <uim/uim-im-switcher.h>
#include <uim/uim-util.h>


/* uim_helper_init_client_fd() takes a callback without user data and
 * there is only ever one uim_wayland. */
static struct uim_wayland *helper_uw;

static void
helper_disconnect_cb(void)
{
  if (!helper_uw)
    return;
  uim_unset_uim_fd(helper_uw->uc);
  helper_uw->helper_fd = -1;
}

void
uim_wayland_helper_send(struct uim_wayland *uw, const char *message)
{
  if (uw->helper_fd < 0)
    return;
  uim_helper_send_message(uw->helper_fd, message);
}

void
uim_wayland_helper_connect(struct uim_wayland *uw)
{
  if (uw->helper_fd >= 0)
    return;

  helper_uw = uw;
  uw->helper_fd = uim_helper_init_client_fd(helper_disconnect_cb);
  if (uw->helper_fd < 0)
    return;
  uim_set_uim_fd(uw->uc, uw->helper_fd);
}

void
uim_wayland_helper_disconnect(struct uim_wayland *uw)
{
  if (uw->helper_fd < 0)
    return;
  /* uim_helper_close_client_fd() calls helper_disconnect_cb(). */
  uim_helper_close_client_fd(uw->helper_fd);
}

void
uim_wayland_helper_focus_in(struct uim_wayland *uw)
{
  uim_wayland_helper_connect(uw);
  uim_helper_client_focus_in(uw->uc);
}

void
uim_wayland_helper_focus_out(struct uim_wayland *uw)
{
  uim_helper_client_focus_out(uw->uc);
}

static char *
dup_or_empty(const char *str)
{
  return uim_strdup(str ? str : "");
}

void
uim_wayland_helper_send_im_list(struct uim_wayland *uw)
{
  uim_context uc = uw->uc;
  int n = uim_get_nr_im(uc);
  /* libuim hands out strings that only stay valid until the next
   * libuim call, so every one of them is copied before anything else
   * is asked for. */
  char *current_im_name = dup_or_empty(uim_get_current_im_name(uc));
  size_t capacity = 1024;
  size_t length = 0;
  char *message = uim_malloc(capacity);
  int i;

  message[0] = '\0';
#define APPEND(s)                                                       \
  do {                                                                  \
    const char *s_ = (s);                                               \
    size_t l_ = s_ ? strlen(s_) : 0;                                    \
    if (length + l_ + 1 > capacity) {                                   \
      while (length + l_ + 1 > capacity)                                \
        capacity *= 2;                                                  \
      message = uim_realloc(message, capacity);                         \
    }                                                                   \
    if (l_)                                                             \
      memcpy(message + length, s_, l_);                                 \
    length += l_;                                                       \
    message[length] = '\0';                                             \
  } while (0)

  APPEND("im_list\ncharset=UTF-8\n");
  for (i = 0; i < n; i++) {
    char *name = dup_or_empty(uim_get_im_name(uc, i));
    /* uim_get_im_language() returns an ISO 639-1 code such as "ja";
     * the helper protocol wants a human readable name. */
    char *langcode = dup_or_empty(uim_get_im_language(uc, i));
    char *lang = dup_or_empty(uim_get_language_name_from_locale(langcode));
    char *short_desc = dup_or_empty(uim_get_im_short_desc(uc, i));

    APPEND(name);
    APPEND("\t");
    APPEND(lang);
    APPEND("\t");
    APPEND(short_desc);
    APPEND("\t");
    if (strcmp(name, current_im_name) == 0)
      APPEND("selected");
    APPEND("\n");

    free(name);
    free(langcode);
    free(lang);
    free(short_desc);
  }
#undef APPEND

  uim_wayland_helper_send(uw, message);
  free(message);
  free(current_im_name);
}

/* Splits message into lines in place. Returns the number of lines. */
static int
split_lines(char *message, char **lines, int max_lines)
{
  int n = 0;
  char *p = message;

  while (n < max_lines) {
    char *newline;
    lines[n++] = p;
    newline = strchr(p, '\n');
    if (!newline)
      break;
    *newline = '\0';
    p = newline + 1;
  }
  return n;
}

static bool
has_prefix(const char *str, const char *prefix)
{
  return strncmp(str, prefix, strlen(prefix)) == 0;
}

static void
apply_im_change(struct uim_wayland *uw,
                const char *im_name,
                bool update_default)
{
  uim_switch_im(uw->uc, im_name);
  if (update_default) {
    char *sym;
    uim_asprintf(&sym, "'%s", im_name);
    uim_prop_update_custom(uw->uc, "custom-preserved-default-im-name", sym);
    free(sym);
  }
  uim_prop_list_update(uw->uc);
}

static void
handle_im_change(struct uim_wayland *uw, char *message)
{
  char *lines[3];
  int n = split_lines(message, lines, 3);

  if (n < 2 || lines[1][0] == '\0')
    return;

  /* The bus is desktop wide: the messages that are scoped to the
   * focused text area or application are for us only while we own the
   * input. */
  if (has_prefix(lines[0], "im_change_this_text_area_only")) {
    if (uw->focused)
      apply_im_change(uw, lines[1], false);
  } else if (has_prefix(lines[0], "im_change_whole_desktop")) {
    apply_im_change(uw, lines[1], true);
  } else if (has_prefix(lines[0], "im_change_this_application_only")) {
    if (uw->focused)
      apply_im_change(uw, lines[1], true);
  }
}

/*
 * commit_string from another process, e.g. the character dictionary:
 *   "commit_string\n" text "\n"
 * or, with an explicit charset,
 *   "commit_string\n" "charset=" charset "\n" text "\n"
 */
static void
handle_commit_string(struct uim_wayland *uw, char *message)
{
  char *lines[4];
  int n = split_lines(message, lines, 4);

  if (!uw->focused || n < 2)
    return;

  if (n >= 3 && has_prefix(lines[1], "charset=")) {
    const char *charset = lines[1] + strlen("charset=");
    void *cd = uim_iconv->create("UTF-8", charset);
    char *utf8 = uim_iconv->convert(cd, lines[2]);
    uim_iconv->release(cd);
    if (utf8) {
      uim_wayland_commit_string(uw, utf8);
      free(utf8);
    }
  } else {
    uim_wayland_commit_string(uw, lines[1]);
  }
}

static void
handle_message(struct uim_wayland *uw, char *message)
{
  if (has_prefix(message, "im_change")) {
    handle_im_change(uw, message);
  } else if (has_prefix(message, "prop_update_custom")) {
    char *lines[4];
    int n = split_lines(message, lines, 4);
    if (n >= 3)
      uim_prop_update_custom(uw->uc, lines[1], lines[2]);
  } else if (has_prefix(message, "custom_reload_notify")) {
    uim_prop_reload_configs();
  } else if (has_prefix(message, "prop_list_get")) {
    uim_prop_list_update(uw->uc);
  } else if (has_prefix(message, "prop_activate")) {
    char *lines[3];
    int n = split_lines(message, lines, 3);
    if (n >= 2)
      uim_prop_activate(uw->uc, lines[1]);
  } else if (has_prefix(message, "im_list_get")) {
    uim_wayland_helper_send_im_list(uw);
  } else if (has_prefix(message, "commit_string")) {
    handle_commit_string(uw, message);
  }
  /* "focus_in" from other uim clients: nothing to do, there is only
   * one context in this process. */
}

void
uim_wayland_helper_dispatch(struct uim_wayland *uw)
{
  char *message;

  if (uw->helper_fd < 0)
    return;

  /* On EOF this closes the fd and calls helper_disconnect_cb(), so
   * uw->helper_fd can be -1 when it returns. */
  uim_helper_read_proc(uw->helper_fd);

  /* Messages already buffered must be handled even when the same read
   * hit EOF, otherwise they are left in the buffer that libuim shares
   * with the next connection. */
  while ((message = uim_helper_get_message()) != NULL) {
    handle_message(uw, message);
    free(message);
  }
}
