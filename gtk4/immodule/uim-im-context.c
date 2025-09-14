/*
  Copyright (c) 2003-2025 uim Project https://github.com/uim/uim

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

#include <config.h>

#include "uim-im-context.h"

#include <gdk/gdkkeysyms.h>

#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <locale.h>

#include <uim/uim.h>
#include <uim/uim-util.h>
#include <uim/uim-helper.h>
#include <uim/uim-im-switcher.h>
#include <uim/gettext.h>
#include <uim/uim-scm.h>

#define DEFAULT_SEPARATOR_STR "|"

static gboolean have_focus = FALSE;

typedef struct preedit_segment
{
  int attr;
  gchar *str;
} preedit_segment;

static void
preedit_segment_clear(void *data)
{
  preedit_segment *segment = data;
  g_free(segment->str);
}

struct _UIMIMContext
{
  GtkIMContext parent;

  uim_context uc;

  GtkWidget *client_widget;
  GdkRectangle cursor_location;
  gboolean use_preedit;

  GArray *preedit_segments;
  size_t prev_preedit_len;

  GIOChannel *helper_channel;
  guint helper_watch_tag;
  GString *helper_buffer;
};

struct _UIMIMContextClass
{
  GtkIMContextClass parent_class;
};

G_DEFINE_DYNAMIC_TYPE(UIMIMContext, uim_im_context, GTK_TYPE_IM_CONTEXT)

/* callback functions for libuim */
static void
commit_cb(void *ptr, const char *str)
{
  UIMIMContext *uic = ptr;
  g_return_if_fail(str);
  g_signal_emit_by_name(uic, "commit", str);
}

static void
preedit_clear_cb(void *ptr)
{
  UIMIMContext *uic = ptr;
  g_array_set_size(uic->preedit_segments, 0);
}

static void
preedit_pushback_cb(void *ptr, int attr, const char *str)
{
  UIMIMContext *uic = (UIMIMContext *)ptr;
  g_return_if_fail(str);

  if (g_str_equal(str, "") &&
      !(attr & (UPreeditAttr_Cursor | UPreeditAttr_Separator)))
    return;

  preedit_segment segment;
  segment.attr = attr;
  segment.str = g_strdup(str);
  g_array_append_val(uic->preedit_segments, segment);
}

static int
uim_im_context_get_preedit_length(UIMIMContext *uic)
{
  size_t length = 0;
  for (guint i = 0; i < uic->preedit_segments->len; i++) {
    preedit_segment *segment =
      &g_array_index(uic->preedit_segments, preedit_segment, i);
    length += strlen(segment->str);
  }
  return length;
}

static void
preedit_update_cb(void *ptr)
{
  UIMIMContext *uic = (UIMIMContext *)ptr;

  const size_t preedit_len = uim_im_context_get_preedit_length(uic);

  if (uic->prev_preedit_len == 0 && preedit_len > 0)
    g_signal_emit_by_name(uic, "preedit-start");

  if (uic->prev_preedit_len > 0 || preedit_len > 0)
    g_signal_emit_by_name(uic, "preedit-changed");

  if (uic->prev_preedit_len > 0 && preedit_len == 0)
    g_signal_emit_by_name(uic, "preedit-end");

  uic->prev_preedit_len = preedit_len;
}

static void
prop_list_update_cb(void *ptr, const char *str)
{
  UIMIMContext *uic = (UIMIMContext *)ptr;

  {
    GString *prop_list_update = g_string_new(NULL);
    g_string_printf(prop_list_update,
                    "prop_list_update\n"
                    "charset=UTF-8\n"
                    "%s",
                    str);
    uim_helper_send_message(uim_get_uim_fd(uic->uc), prop_list_update->str);
    g_string_free(prop_list_update, TRUE);
  }
}

static void
uim_im_context_send_im_list(UIMIMContext *uic)
{
  int n = uim_get_nr_im(uic->uc);
  const char *current_im_name = uim_get_current_im_name(uic->uc);
  GString *message = g_string_new("im_list\n"
                                  "charset=UTF-8\n");
  for (int i = 0; i < n; i++) {
    /*
     * Return value of uim_get_im_language() is an ISO 639-1
     * compatible language code such as "ja". Since it is unfriendly
     * for human reading, we convert it into friendly one by
     * uim_get_language_name_from_locale() here.
     */
    const char *name = uim_get_im_name(uic->uc, i);
    const char *langcode = uim_get_im_language(uic->uc, i);
    const char *lang = uim_get_language_name_from_locale(langcode);
    const char *short_desc = uim_get_im_short_desc(uic->uc, i);

    g_string_append(message, name);
    g_string_append(message, "\t");
    if (lang)
      g_string_append(message, lang);
    g_string_append(message, "\t");
    if (short_desc)
      g_string_append(message, short_desc);
    g_string_append(message, "\t");
    if (strcmp(name, current_im_name) == 0)
      g_string_append(message, "selected");
    g_string_append(message, "\n");
  }
  uim_helper_send_message(uim_get_uim_fd(uic->uc), message->str);
  g_string_free(message, TRUE);
}

static void
configuration_changed_cb(void *ptr)
{
  UIMIMContext *uic = (UIMIMContext *)ptr;
  uim_im_context_send_im_list(uic);
}

static void
uim_im_context_switch_im(UIMIMContext *uic, const char *name)
{
  GString *im_name_sym = g_string_new("'");
  g_string_append(im_name_sym, name);
  uim_prop_update_custom(uic->uc,
                         "custom-preserved-default-im-name",
                         im_name_sym->str);
  g_string_free(im_name_sym, TRUE);
}

static void
switch_app_global_im_cb(void *ptr, const char *name)
{
  UIMIMContext *uic = ptr;

  /* TODO: Ensure getting focus? */
  if (!have_focus) {
    return;
  }

  uim_im_context_switch_im(uic, name);

  GString *message = g_string_new("");
  g_string_printf(message, "im_change_this_application_only\n%s\n", name);
  uim_helper_send_message(uim_get_uim_fd(uic->uc), message->str);
  g_string_free(message, TRUE);
}

static void
switch_system_global_im_cb(void *ptr, const char *name)
{
  UIMIMContext *uic = ptr;

  uim_im_context_switch_im(uic, name);

  /* Switch contexts of other processes. Bridges should not expect
   * that the helper-server reflect back the messaage to the
   * originating process.  -- YamaKen 2006-03-01 */
  GString *message = g_string_new("");
  g_string_printf(message, "im_change_whole_desktop\n%s\n", name);
  uim_helper_send_message(uim_get_uim_fd(uic->uc), message->str);
  g_string_free(message, TRUE);
}

/* uim helper related */

static void
uim_im_context_parse_helper_message_im_change(UIMIMContext *uic,
                                              const char *str)
{
  gchar **lines = g_strsplit(str, "\n", -1);
  gchar *im_name = lines[1];
  GString *im_name_sym = g_string_new(im_name);

  g_string_prepend_c(im_name_sym, '\'');

  if (g_str_has_prefix(str, "im_change_this_text_area_only")) {
    uim_switch_im(uic->uc, im_name);
    uim_prop_list_update(uic->uc);
  } else if (g_str_has_prefix(str, "im_change_whole_desktop")) {
    uim_switch_im(uic->uc, im_name);
    uim_prop_update_custom(uic->uc,
                           "custom-preserved-default-im-name",
                           im_name_sym->str);
    uim_prop_list_update(uic->uc);
  } else if (g_str_has_prefix(str, "im_change_this_application_only")) {
    if (have_focus) {
      uim_switch_im(uic->uc, im_name);
      uim_prop_update_custom(uic->uc,
                             "custom-preserved-default-im-name",
                             im_name_sym->str);
      uim_prop_list_update(uic->uc);
    }
  }
  g_strfreev(lines);
  g_string_free(im_name_sym, TRUE);
}

/* Copied from helper-common-gtk.c. Maybe we need common GTK+ utility file. */
static gchar *
get_charset(gchar *line)
{
  gchar **splitted = NULL;

  splitted = g_strsplit(line, "=", 0);

  if (splitted && splitted[0] && splitted[1] &&
      g_str_equal("charset", splitted[0])) {
    gchar *charset = g_strdup(splitted[1]);
    g_strfreev(splitted);
    return charset;
  } else {
    g_strfreev(splitted);
    return NULL;
  }
}

static void
uim_im_context_commit_string_from_other_process(UIMIMContext *uic,
                                                const gchar *str)
{
  gchar **lines = g_strsplit(str, "\n", 0);
  gchar *commit_string;

  if (!lines || !lines[0] || !lines[1] || !lines[2]) {
    g_strfreev(lines);
    return; /* Message is broken, do nothing. */
  }

  if (g_str_equal(lines[2], "")) {
    /* Assuming character encoding as UTF-8. */
    commit_string = lines[1];
    g_signal_emit_by_name(uic, "commit", commit_string);
  } else {
    /*
     * If second line exists, we assume the first line as a charset
     * specifier.  This (rotten) convention is influenced by old design
     * mistake (character encoding was forgotten!).
     */

    gchar *encoding, *commit_string_utf8;

    encoding = get_charset(lines[1]);
    commit_string = lines[2];
    commit_string_utf8 = g_convert(commit_string,
                                   strlen(commit_string),
                                   "UTF-8",
                                   encoding,
                                   NULL,  /* gsize *bytes_read */
                                   NULL,  /* size *bytes_written */
                                   NULL); /* GError **error */
    g_signal_emit_by_name(uic, "commit", commit_string_utf8);
    g_free(encoding);
    g_free(commit_string_utf8);
  }

  g_strfreev(lines);
}

static void
uim_im_context_update_candwin_pos_type(UIMIMContext *uic)
{
}

static void
uim_im_context_update_candwin_style(UIMIMContext *uic)
{
}

static void
uim_im_context_parse_helper_message(UIMIMContext *uic, const char *message)
{
  if (g_str_has_prefix(message, "im_change")) {
    uim_im_context_parse_helper_message_im_change(uic, message);
  } else if (g_str_has_prefix(message, "prop_update_custom")) {
    gchar **lines = g_strsplit(message, "\n", 0);
    if (lines && lines[0] && lines[1] && lines[2]) {
      uim_prop_update_custom(uic->uc, lines[1], lines[2]);
      if (g_str_equal(lines[1], "candidate-window-position"))
        uim_im_context_update_candwin_pos_type(uic);
      if (g_str_equal(lines[1], "candidate-window-style"))
        uim_im_context_update_candwin_style(uic);
    }
    g_strfreev(lines);
  } else if (g_str_has_prefix(message, "custom_reload_notify")) {
    uim_prop_reload_configs();
    uim_im_context_update_candwin_pos_type(uic);
    uim_im_context_update_candwin_style(uic);
  } else if (g_str_has_prefix(message, "prop_list_get")) {
    uim_prop_list_update(uic->uc);
  } else if (g_str_has_prefix(message, "prop_activate")) {
    gchar **lines = g_strsplit(message, "\n", 0);
    if (lines && lines[0]) {
      uim_prop_activate(uic->uc, lines[1]);
    }
    g_strfreev(lines);
  } else if (g_str_has_prefix(message, "im_list_get")) {
    uim_im_context_send_im_list(uic);
  } else if (g_str_has_prefix(message, "commit_string")) {
    uim_im_context_commit_string_from_other_process(uic, message);
  } else if (g_str_has_prefix(message, "focus_in")) {
    /* We don't need this if GTK notifies all focus in/out information. */
  }
}

static void
uim_im_context_dispose_helper(UIMIMContext *uic)
{
  uim_unset_uim_fd(uic->uc);
  if (uic->helper_watch_tag != 0) {
    g_source_remove(uic->helper_watch_tag);
    uic->helper_watch_tag = 0;
  }
  if (uic->helper_channel) {
    g_io_channel_unref(uic->helper_channel);
    uic->helper_channel = NULL;
  }
  g_string_truncate(uic->helper_buffer, 0);
}

static gboolean
helper_read_cb(GIOChannel *channel, GIOCondition c, gpointer data)
{
  UIMIMContext *uic = data;

#define BUFFER_SIZE 1024
  char buffer[BUFFER_SIZE];
  gsize bytes_read;
  GError *error = NULL;
  GIOStatus status =
    g_io_channel_read_chars(channel, buffer, BUFFER_SIZE, &bytes_read, &error);
  if (error) {
    g_warning("uim: failed to read from helper: %s: %d: %s",
              g_quark_to_string(error->domain),
              error->code,
              error->message);
    g_error_free(error);
  }

  if (bytes_read > 0) {
    g_string_append_len(uic->helper_buffer, buffer, bytes_read);
    char *message;
    while ((message = uim_helper_buffer_get_message(uic->helper_buffer->str))) {
      uim_im_context_parse_helper_message(uic, message);
      free(message);
    }
    g_string_truncate(uic->helper_buffer, strlen(uic->helper_buffer->str));
  }
#undef BUFFER_SIZE

  if (status == G_IO_STATUS_NORMAL) {
    return TRUE;
  } else {
    uic->helper_watch_tag = 0; /* "return FALSE" removes this callback. */
    uim_im_context_dispose_helper(uic);
    return FALSE;
  }
}

static void
uim_im_context_check_helper_connection(UIMIMContext *uic)
{
  if (uim_get_uim_fd(uic->uc) >= 0) {
    return;
  }

  int helper_fd = uim_helper_init_client_fd(NULL);
  if (helper_fd < 0) {
    return;
  }

  uim_im_context_dispose_helper(uic);
  uic->helper_channel = g_io_channel_unix_new(helper_fd);
  GError *error = NULL;
  GIOStatus status =
    g_io_channel_set_encoding(uic->helper_channel, NULL, &error);
  if (status != G_IO_STATUS_NORMAL) {
    if (error) {
      g_warning("uim: failed to set encoding for helper channel: %s: %d: %s",
                g_quark_to_string(error->domain),
                error->code,
                error->message);
    } else {
      g_warning("uim: failed to set encoding for helper channel.");
    }
  }
  g_clear_error(&error);
  g_io_channel_set_buffered(uic->helper_channel, FALSE);
  g_io_channel_set_close_on_unref(uic->helper_channel, TRUE);
  uic->helper_watch_tag = g_io_add_watch(uic->helper_channel,
                                         G_IO_IN | G_IO_HUP | G_IO_ERR,
                                         helper_read_cb,
                                         uic);
}

/* class functions */

static void
convert_key_event(GdkEvent *event, int *ukey, int *umod)
{
  int keyval = gdk_key_event_get_keyval(event);
  int mod = gdk_event_get_modifier_state(event);

  *umod = 0;

  /* 1. check key */
  if (keyval < 256)
    *ukey = keyval;
  else if (keyval >= GDK_KEY_F1 && keyval <= GDK_KEY_F35)
    *ukey = keyval - GDK_KEY_F1 + UKey_F1;
  else if (keyval >= GDK_KEY_KP_0 && keyval <= GDK_KEY_KP_9)
    *ukey = keyval - GDK_KEY_KP_0 + UKey_0;
  else if (keyval >= GDK_KEY_dead_grave && keyval <= GDK_KEY_dead_horn)
    *ukey = keyval - GDK_KEY_dead_grave + UKey_Dead_Grave;
  else if (keyval >= GDK_KEY_Kanji && keyval <= GDK_KEY_Eisu_toggle)
    *ukey = keyval - GDK_KEY_Kanji + UKey_Kanji;
  else if (keyval >= GDK_KEY_Hangul && keyval <= GDK_KEY_Hangul_Special)
    *ukey = keyval - GDK_KEY_Hangul + UKey_Hangul;
  else if (keyval >= GDK_KEY_kana_fullstop && keyval <= GDK_KEY_semivoicedsound)
    *ukey = keyval - GDK_KEY_kana_fullstop + UKey_Kana_Fullstop;
  else {
    switch (keyval) {
    case GDK_KEY_BackSpace:
      *ukey = UKey_Backspace;
      break;
    case GDK_KEY_Delete:
      *ukey = UKey_Delete;
      break;
    case GDK_KEY_Insert:
      *ukey = UKey_Insert;
      break;
    case GDK_KEY_Escape:
      *ukey = UKey_Escape;
      break;
    case GDK_KEY_Tab:
    case GDK_KEY_ISO_Left_Tab:
      *ukey = UKey_Tab;
      break;
    case GDK_KEY_Return:
      *ukey = UKey_Return;
      break;
    case GDK_KEY_Left:
      *ukey = UKey_Left;
      break;
    case GDK_KEY_Up:
      *ukey = UKey_Up;
      break;
    case GDK_KEY_Right:
      *ukey = UKey_Right;
      break;
    case GDK_KEY_Down:
      *ukey = UKey_Down;
      break;
    case GDK_KEY_Prior:
      *ukey = UKey_Prior;
      break;
    case GDK_KEY_Next:
      *ukey = UKey_Next;
      break;
    case GDK_KEY_Home:
      *ukey = UKey_Home;
      break;
    case GDK_KEY_End:
      *ukey = UKey_End;
      break;
    case GDK_KEY_Multi_key:
      *ukey = UKey_Multi_key;
      break;
    case GDK_KEY_Codeinput:
      *ukey = UKey_Codeinput;
      break;
    case GDK_KEY_SingleCandidate:
      *ukey = UKey_SingleCandidate;
      break;
    case GDK_KEY_MultipleCandidate:
      *ukey = UKey_MultipleCandidate;
      break;
    case GDK_KEY_PreviousCandidate:
      *ukey = UKey_PreviousCandidate;
      break;
    case GDK_KEY_Mode_switch:
      *ukey = UKey_Mode_switch;
      break;
    case GDK_KEY_Shift_L:
    case GDK_KEY_Shift_R:
      *ukey = UKey_Shift_key;
      break;
    case GDK_KEY_Control_L:
    case GDK_KEY_Control_R:
      *ukey = UKey_Control_key;
      break;
    case GDK_KEY_Alt_L:
    case GDK_KEY_Alt_R:
      *ukey = UKey_Alt_key;
      break;
    case GDK_KEY_Meta_L:
    case GDK_KEY_Meta_R:
      *ukey = UKey_Meta_key;
      break;
    case GDK_KEY_Super_L:
    case GDK_KEY_Super_R:
      *ukey = UKey_Super_key;
      break;
    case GDK_KEY_Hyper_L:
    case GDK_KEY_Hyper_R:
      *ukey = UKey_Hyper_key;
      break;
    case GDK_KEY_Caps_Lock:
      *ukey = UKey_Caps_Lock;
      break;
    case GDK_KEY_Num_Lock:
      *ukey = UKey_Num_Lock;
      break;
    case GDK_KEY_Scroll_Lock:
      *ukey = UKey_Scroll_Lock;
      break;
    default:
      *ukey = UKey_Other;
      break;
    }
  }

  /* check modifier */
  if (mod & GDK_SHIFT_MASK)
    *umod |= UMod_Shift;
  if (mod & GDK_CONTROL_MASK)
    *umod |= UMod_Control;
  if (mod & GDK_ALT_MASK)
    *umod |= UMod_Alt;
  if (mod & GDK_SUPER_MASK)
    *umod |= UMod_Super;
  if (mod & GDK_HYPER_MASK)
    *umod |= UMod_Hyper;
}

static gboolean
uim_im_context_filter_keypress(GtkIMContext *context, GdkEvent *event)
{
  UIMIMContext *uic = UIM_IM_CONTEXT(context);
  int ukey, umod;
  int handled;

  convert_key_event(event, &ukey, &umod);
  if (gdk_event_get_event_type(event) == GDK_KEY_RELEASE)
    handled = uim_release_key(uic->uc, ukey, umod);
  else
    handled = uim_press_key(uic->uc, ukey, umod);
  return handled;
}

static gboolean
uim_im_context_get_user_defined_color(UIMIMContext *uic,
                                      PangoColor *color,
                                      const gchar *uim_symbol)
{
  gboolean parsed = FALSE;
  char *literal = uim_scm_symbol_value_str(uim_symbol);
  if (literal && literal[0] != '\0')
    parsed = pango_color_parse(color, literal);
  free(literal);
  return parsed;
}

static void
uim_im_context_convert_preedit_segment(UIMIMContext *uic,
                                       struct preedit_segment *ps,
                                       GString *buffer,
                                       PangoAttrList *attrs)
{
  const gchar *segment_str = ps->str;
  if ((ps->attr & UPreeditAttr_Separator) && g_str_equal(segment_str, ""))
    segment_str = DEFAULT_SEPARATOR_STR;

  gsize begin = buffer->len;
  g_string_append(buffer, segment_str);

  if (!attrs) {
    return;
  }

  gsize end = buffer->len;

  if (ps->attr & UPreeditAttr_UnderLine) {
    PangoAttribute *attr = pango_attr_underline_new(PANGO_UNDERLINE_SINGLE);
    attr->start_index = begin;
    attr->end_index = end;
    pango_attr_list_change(attrs, attr);
  }

  if (ps->attr & UPreeditAttr_Separator) {
    const gchar *separator_fg_symbol, *separator_bg_symbol;

    if (ps->attr & UPreeditAttr_Reverse) {
      separator_fg_symbol = "reversed-separator-foreground";
      separator_bg_symbol = "reversed-separator-background";
    } else {
      separator_fg_symbol = "separator-foreground";
      separator_bg_symbol = "separator-background";
    }

    PangoColor color;
    if (uim_im_context_get_user_defined_color(uic,
                                              &color,
                                              separator_fg_symbol)) {
      PangoAttribute *attr =
        pango_attr_foreground_new(color.red, color.green, color.blue);
      attr->start_index = begin;
      attr->end_index = end;
      pango_attr_list_change(attrs, attr);
    }
    if (uim_im_context_get_user_defined_color(uic,
                                              &color,
                                              separator_bg_symbol)) {
      PangoAttribute *attr =
        pango_attr_background_new(color.red, color.green, color.blue);
      attr->start_index = begin;
      attr->end_index = end;
      pango_attr_list_change(attrs, attr);
    }
  } else if (ps->attr & UPreeditAttr_Reverse) {
    PangoColor color;
    if (uim_im_context_get_user_defined_color(uic,
                                              &color,
                                              "reversed-preedit-foreground") ||
        pango_color_parse(&color, "#fff")) {
      PangoAttribute *attr =
        pango_attr_foreground_new(color.red, color.green, color.blue);
      attr->start_index = begin;
      attr->end_index = end;
      pango_attr_list_change(attrs, attr);
    }
    if (uim_im_context_get_user_defined_color(uic,
                                              &color,
                                              "reversed-preedit-background") ||
        pango_color_parse(&color, "#000")) {
      PangoAttribute *attr =
        pango_attr_background_new(color.red, color.green, color.blue);
      attr->start_index = begin;
      attr->end_index = end;
      pango_attr_list_change(attrs, attr);
    }
  }
}

static void
uim_im_context_get_preedit_string(GtkIMContext *ic,
                                  gchar **str,
                                  PangoAttrList **attrs,
                                  gint *cursor_pos)
{
  UIMIMContext *uic = UIM_IM_CONTEXT(ic);

  if (attrs)
    *attrs = pango_attr_list_new();

  GString *buffer = g_string_new(NULL);
  for (guint i = 0; i < uic->preedit_segments->len; i++) {
    preedit_segment *segment =
      &g_array_index(uic->preedit_segments, preedit_segment, i);
    if ((segment->attr & UPreeditAttr_Cursor) && cursor_pos) {
      *cursor_pos = buffer->len;
    }
    uim_im_context_convert_preedit_segment(uic,
                                           segment,
                                           buffer,
                                           attrs ? *attrs : NULL);
  }

  if (str) {
    *str = g_string_free(buffer, FALSE);
  } else {
    g_string_free(buffer, TRUE);
  }
}

static void
uim_im_context_focus_in(GtkIMContext *ic)
{
  UIMIMContext *uic = UIM_IM_CONTEXT(ic);

  have_focus = TRUE;

  uim_im_context_check_helper_connection(uic);
  uim_helper_client_focus_in(uic->uc);
  uim_prop_list_update(uic->uc);

  uim_focus_in_context(uic->uc);
}

static void
uim_im_context_focus_out(GtkIMContext *ic)
{
  UIMIMContext *uic = UIM_IM_CONTEXT(ic);

  uim_focus_out_context(uic->uc);

  uim_im_context_check_helper_connection(uic);
  uim_helper_client_focus_out(uic->uc);

  have_focus = FALSE;
}

static void
uim_im_context_reset(GtkIMContext *ic)
{
  UIMIMContext *uic = UIM_IM_CONTEXT(ic);
  uim_reset_context(uic->uc);
  preedit_clear_cb(uic);
  preedit_update_cb(uic);
}

static void
uim_im_context_set_client_widget(GtkIMContext *ic, GtkWidget *widget)
{
  UIMIMContext *uic = UIM_IM_CONTEXT(ic);
  uic->client_widget = widget;
}

static void
uim_im_context_set_cursor_location(GtkIMContext *ic, GdkRectangle *area)
{
  UIMIMContext *uic = UIM_IM_CONTEXT(ic);
  uic->cursor_location = *area;
}

static void
uim_im_context_set_use_preedit(GtkIMContext *ic, gboolean use_preedit)
{
  UIMIMContext *uic = UIM_IM_CONTEXT(ic);
  uic->use_preedit = use_preedit;
}

static void
uim_im_context_init(UIMIMContext *uic)
{
  const char *im_name = uim_get_default_im_name(setlocale(LC_CTYPE, NULL));
  uic->uc =
    uim_create_context(uic, "UTF-8", NULL, im_name, uim_iconv, commit_cb);
  if (!uic->uc) {
    /* We can't notify any error information on initialization with
     * the current GtkIMContext API. So, we abort here. :< */
    g_error("uim: failed to create uim context.");
  }

  uic->client_widget = NULL;
  uic->use_preedit = TRUE;
  uic->preedit_segments = g_array_new(FALSE, TRUE, sizeof(preedit_segment));
  g_array_set_clear_func(uic->preedit_segments, preedit_segment_clear);
  uic->prev_preedit_len = 0;

  uic->helper_channel = NULL;
  uic->helper_watch_tag = 0;
  uic->helper_buffer = g_string_new(NULL);
  uim_im_context_check_helper_connection(uic);

  uim_set_preedit_cb(uic->uc,
                     preedit_clear_cb,
                     preedit_pushback_cb,
                     preedit_update_cb);
  uim_set_prop_list_update_cb(uic->uc, prop_list_update_cb);
  uim_set_configuration_changed_cb(uic->uc, configuration_changed_cb);
  uim_set_im_switch_request_cb(uic->uc,
                               switch_app_global_im_cb,
                               switch_system_global_im_cb);

  uim_prop_list_update(uic->uc);
}

static void
uim_im_context_dispose(GObject *obj)
{
  UIMIMContext *uic = UIM_IM_CONTEXT(obj);

  if (uic->client_widget) {
    uim_im_context_set_client_widget(GTK_IM_CONTEXT(uic), NULL);
  }

  uim_im_context_dispose_helper(uic);

  G_OBJECT_CLASS(uim_im_context_parent_class)->dispose(obj);
}

static void
uim_im_context_finalize(GObject *obj)
{
  UIMIMContext *uic = UIM_IM_CONTEXT(obj);

  g_string_free(uic->helper_buffer, TRUE);

  uim_release_context(uic->uc);

  G_OBJECT_CLASS(uim_im_context_parent_class)->finalize(obj);
}

static void
uim_im_context_class_init(UIMIMContextClass *klass)
{
  GtkIMContextClass *gtk_im_context_class = GTK_IM_CONTEXT_CLASS(klass);
  GObjectClass *object_class = G_OBJECT_CLASS(klass);

  gtk_im_context_class->filter_keypress = uim_im_context_filter_keypress;
  gtk_im_context_class->focus_in = uim_im_context_focus_in;
  gtk_im_context_class->focus_out = uim_im_context_focus_out;
  gtk_im_context_class->get_preedit_string = uim_im_context_get_preedit_string;
  gtk_im_context_class->reset = uim_im_context_reset;
  gtk_im_context_class->set_client_widget = uim_im_context_set_client_widget;
  gtk_im_context_class->set_cursor_location =
    uim_im_context_set_cursor_location;
  gtk_im_context_class->set_use_preedit = uim_im_context_set_use_preedit;

  object_class->dispose = uim_im_context_dispose;
  object_class->finalize = uim_im_context_finalize;
}

static void
uim_im_context_class_finalize(UIMIMContextClass *klass)
{
}

void
uim_im_context_load(GTypeModule *module)
{
  uim_im_context_register_type(module);
}
