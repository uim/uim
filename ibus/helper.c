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
 * Communication with uim-helper-server: toolbar, IM switcher and so on.
 *
 * libuim keeps one connection per process, but ibus-daemon asks this
 * process for an engine per input context. Only the engine that has
 * the focus talks for the process; the others keep quiet, as the
 * other applications' contexts would. All of them keep quiet while
 * another uim client, say a GTK application with GTK_IM_MODULE=uim,
 * has the focus.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <string.h>

#include <glib-unix.h>

#include "ibus-engine-uim.h"

#include <uim/uim-helper.h>
#include <uim/uim-im-switcher.h>
#include <uim/uim-util.h>

static GList *engines; /* of IBusUimEngine */
static IBusUimEngine *focused_engine;
/* Another uim client said "focus_in" after our engine got the focus. */
static gboolean focus_elsewhere;

static int helper_fd = -1;
static guint helper_watch;

/* The engine that talks for the process, if any. */
static IBusUimEngine *
talking_engine(void)
{
  return focus_elsewhere ? NULL : focused_engine;
}

static void
send_message(const char *message)
{
  if (helper_fd < 0)
    return;
  uim_helper_send_message(helper_fd, message);
}

static void
send_im_list(IBusUimEngine *engine)
{
  uim_context uc = engine->uc;
  int n = uim_get_nr_im(uc);
  /* libuim's strings are only valid until its next call. */
  char *current_im_name = g_strdup(uim_get_current_im_name(uc));
  GString *message = g_string_new("im_list\n"
                                  "charset=UTF-8\n");
  int i;

  for (i = 0; i < n; i++) {
    char *name = g_strdup(uim_get_im_name(uc, i));
    /* uim_get_im_language() returns an ISO 639-1 code such as "ja";
     * the helper protocol wants a human readable name. */
    char *langcode = g_strdup(uim_get_im_language(uc, i));
    char *lang =
      langcode ? g_strdup(uim_get_language_name_from_locale(langcode)) : NULL;
    char *short_desc = g_strdup(uim_get_im_short_desc(uc, i));

    g_string_append_printf(message, "%s\t%s\t%s\t%s\n",
                           name ? name : "",
                           lang ? lang : "",
                           short_desc ? short_desc : "",
                           name && current_im_name &&
                           strcmp(name, current_im_name) == 0 ?
                           "selected" : "");
    g_free(name);
    g_free(langcode);
    g_free(lang);
    g_free(short_desc);
  }
  send_message(message->str);
  g_string_free(message, TRUE);
  g_free(current_im_name);
}

static void
update_default_im(IBusUimEngine *engine, const char *name)
{
  char *sym = g_strdup_printf("'%s", name);
  uim_prop_update_custom(engine->uc, "custom-preserved-default-im-name", sym);
  g_free(sym);
}

/* The one that asked has switched itself. */
static void
switch_other_engines(IBusUimEngine *engine, const char *name)
{
  GList *node;

  for (node = engines; node; node = node->next) {
    IBusUimEngine *other = node->data;
    if (other != engine) {
      uim_switch_im(other->uc, name);
      uim_prop_list_update(other->uc);
    }
  }
}

/* callbacks from libuim */

static void
configuration_changed_cb(void *ptr)
{
  IBusUimEngine *engine = ptr;

  if (engine != talking_engine())
    return;
  send_im_list(engine);
}

static void
switch_app_global_im_cb(void *ptr, const char *name)
{
  IBusUimEngine *engine = ptr;

  /* All the engines here serve the applications that talk to IBus:
   * that is the application. Nothing is sent: an
   * im_change_this_application_only would switch whichever other
   * process believes it has the focus. */
  update_default_im(engine, name);
  switch_other_engines(engine, name);
}

static void
switch_system_global_im_cb(void *ptr, const char *name)
{
  IBusUimEngine *engine = ptr;
  char *message;

  update_default_im(engine, name);
  switch_other_engines(engine, name);
  /* Other processes switch on this message; the helper server does
   * not reflect it back to us. */
  message = g_strdup_printf("im_change_whole_desktop\n%s\n", name);
  send_message(message);
  g_free(message);
}

/* messages from uim-helper-server */

static void
apply_im_change(IBusUimEngine *engine, const char *name, gboolean default_im)
{
  uim_switch_im(engine->uc, name);
  if (default_im)
    update_default_im(engine, name);
  uim_prop_list_update(engine->uc);
}

static void
handle_im_change(char **lines)
{
  const char *name = lines[1];
  IBusUimEngine *engine = talking_engine();
  GList *node;

  if (!name || name[0] == '\0')
    return;

  if (g_str_has_prefix(lines[0], "im_change_this_text_area_only")) {
    if (engine)
      apply_im_change(engine, name, FALSE);
  } else if (g_str_has_prefix(lines[0], "im_change_whole_desktop")) {
    for (node = engines; node; node = node->next)
      apply_im_change(node->data, name, TRUE);
  } else if (g_str_has_prefix(lines[0], "im_change_this_application_only")) {
    /* Everybody gets this; it is for us while we have the focus. */
    if (engine) {
      for (node = engines; node; node = node->next)
        apply_im_change(node->data, name, TRUE);
    }
  }
}

/*
 * commit_string from another process, e.g. the character dictionary:
 *   "commit_string\n" text "\n"
 * or, with an explicit charset,
 *   "commit_string\n" "charset=" charset "\n" text "\n"
 */
static void
handle_commit_string(char **lines)
{
  IBusUimEngine *engine = talking_engine();

  if (!engine || !lines[1])
    return;

  if (g_str_has_prefix(lines[1], "charset=")) {
    const char *charset = lines[1] + strlen("charset=");
    char *utf8;

    if (!lines[2])
      return;
    utf8 = g_convert(lines[2], -1, "UTF-8", charset, NULL, NULL, NULL);
    if (utf8) {
      ibus_uim_engine_commit_string(engine, utf8);
      g_free(utf8);
    }
  } else {
    ibus_uim_engine_commit_string(engine, lines[1]);
  }
}

static void
handle_message(const char *message)
{
  char **lines = g_strsplit(message, "\n", 0);
  IBusUimEngine *engine = talking_engine();

  if (g_str_has_prefix(message, "im_change")) {
    handle_im_change(lines);
  } else if (g_str_has_prefix(message, "prop_update_custom")) {
    /* Custom values are shared by all the contexts in a process. */
    if (engines && lines[1] && lines[2]) {
      IBusUimEngine *any_engine = engines->data;
      uim_prop_update_custom(any_engine->uc, lines[1], lines[2]);
    }
  } else if (g_str_has_prefix(message, "custom_reload_notify")) {
    uim_prop_reload_configs();
  } else if (g_str_has_prefix(message, "prop_list_get")) {
    if (engine)
      uim_prop_list_update(engine->uc);
  } else if (g_str_has_prefix(message, "prop_activate")) {
    if (engine && lines[1])
      uim_prop_activate(engine->uc, lines[1]);
  } else if (g_str_has_prefix(message, "im_list_get")) {
    if (engine)
      send_im_list(engine);
  } else if (g_str_has_prefix(message, "commit_string")) {
    handle_commit_string(lines);
  } else if (g_str_has_prefix(message, "focus_in")) {
    /* The toolbar is theirs until we get the focus back. */
    focus_elsewhere = TRUE;
  }
  g_strfreev(lines);
}

/* connection */

static void
helper_disconnect_cb(void)
{
  GList *node;

  for (node = engines; node; node = node->next) {
    IBusUimEngine *engine = node->data;
    uim_unset_uim_fd(engine->uc);
  }
  helper_fd = -1;
}

static gboolean
helper_read_cb(gint fd, GIOCondition condition, gpointer user_data)
{
  char *message;
  (void)condition;
  (void)user_data;

  /* On EOF this closes the fd and calls helper_disconnect_cb(). */
  uim_helper_read_proc(fd);

  /* Messages already buffered must be handled even when the same read
   * hit EOF, otherwise they are left in the buffer that libuim shares
   * with the next connection. */
  while ((message = uim_helper_get_message()) != NULL) {
    handle_message(message);
    free(message);
  }

  if (helper_fd < 0) {
    helper_watch = 0;
    return G_SOURCE_REMOVE;
  }
  return G_SOURCE_CONTINUE;
}

/* Connects if we aren't, which starts uim-helper-server if nobody
 * has. A server that went away is looked for again at the next focus
 * in. */
static void
helper_connect(void)
{
  GList *node;

  if (helper_fd >= 0)
    return;

  helper_fd = uim_helper_init_client_fd(helper_disconnect_cb);
  if (helper_fd < 0)
    return;
  for (node = engines; node; node = node->next) {
    IBusUimEngine *engine = node->data;
    uim_set_uim_fd(engine->uc, helper_fd);
  }
  helper_watch = g_unix_fd_add(helper_fd,
                               G_IO_IN | G_IO_HUP | G_IO_ERR,
                               helper_read_cb,
                               NULL);
}

void
ibus_uim_helper_disconnect(void)
{
  if (helper_fd < 0)
    return;
  if (helper_watch) {
    g_source_remove(helper_watch);
    helper_watch = 0;
  }
  /* uim_helper_close_client_fd() calls helper_disconnect_cb(). */
  uim_helper_close_client_fd(helper_fd);
}

/* engines */

void
ibus_uim_helper_add_engine(IBusUimEngine *engine)
{
  engines = g_list_prepend(engines, engine);
  if (helper_fd >= 0)
    uim_set_uim_fd(engine->uc, helper_fd);
  uim_set_configuration_changed_cb(engine->uc, configuration_changed_cb);
  uim_set_im_switch_request_cb(engine->uc, switch_app_global_im_cb,
                               switch_system_global_im_cb);
}

void
ibus_uim_helper_remove_engine(IBusUimEngine *engine)
{
  engines = g_list_remove(engines, engine);
  if (focused_engine == engine)
    focused_engine = NULL;
}

void
ibus_uim_helper_focus_in(IBusUimEngine *engine)
{
  focused_engine = engine;
  focus_elsewhere = FALSE;
  helper_connect();
  uim_helper_client_focus_in(engine->uc);
}

void
ibus_uim_helper_focus_out(IBusUimEngine *engine)
{
  uim_helper_client_focus_out(engine->uc);
  if (focused_engine == engine)
    focused_engine = NULL;
}

void
ibus_uim_helper_prop_list_update(IBusUimEngine *engine, const char *str)
{
  char *message;

  if (engine != talking_engine())
    return;
  message = g_strdup_printf("prop_list_update\ncharset=UTF-8\n%s", str);
  send_message(message);
  g_free(message);
}
