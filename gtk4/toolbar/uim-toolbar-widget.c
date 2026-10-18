/*

  uim-toolbar-widget.c: engine-agnostic uim toolbar widget for GTK 4

  Copyright (c) 2003-2026 uim Project https://github.com/uim/uim

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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

#include <config.h>

#include "uim-toolbar-widget.h"

#include <ctype.h>
#include <string.h>
#include <sys/stat.h>

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <uim/uim-scm.h>
#include <uim/uim-custom.h>
#include <uim/gettext.h>

/* ------------------------------------------------------------------ *
 * Launcher command table.
 *
 * This is the single, data-driven place that lists the external
 * helper applications the toolbar can start. None of these entries
 * are input-method specific: every uim engine (anthy, mozc, skk, m17n
 * based ones, ...) shares the very same table, and whether a button
 * is shown is controlled purely by a uim-custom boolean the user sets
 * in the preference tool. Adding a new launcher is a one-line addition
 * here, not a per-frontend patch.
 * ------------------------------------------------------------------ */
typedef struct {
  const gchar *desc;                     /* tooltip / menu text */
  const gchar *label;                    /* fallback text when no icon */
  const gchar *icon;                     /* uim pixmap base name, or NULL */
  const gchar *command;                  /* helper binary to spawn */
  const gchar *custom_button_show_symbol;/* uim-custom boolean gating it */
  gboolean show_button;
} UimToolbarCommand;

static UimToolbarCommand uim_toolbar_command_table[] = {
  { N_("Switch input method"), NULL, "im_switcher",
    "uim-im-switcher-gtk4", "toolbar-show-switcher-button?", FALSE },
  { N_("Preference"), NULL, "preferences-desktop",
    "uim-pref-gtk4", "toolbar-show-pref-button?", FALSE },
  { N_("Japanese dictionary editor"), NULL, "uim-dict",
    "uim-dict-gtk4", "toolbar-show-dict-button?", FALSE },
  { N_("Input pad"), NULL, "input-keyboard",
    "uim-input-pad-ja-gtk4", "toolbar-show-input-pad-button?", FALSE },
  { N_("Handwriting input pad"), "H", "accessories-text-editor",
    "uim-tomoe-gtk", "toolbar-show-handwriting-input-pad-button?", FALSE },
  { N_("Help"), NULL, "help-browser",
    "uim-help", "toolbar-show-help-button?", FALSE },
};

static const guint uim_toolbar_command_table_len =
  G_N_ELEMENTS(uim_toolbar_command_table);

/* ------------------------------------------------------------------ */

typedef struct {
  GtkWidget *button;      /* the branch's own button, sits on the bar */
  GtkWidget *popover;
  GtkWidget *list_box;
  gboolean   is_radio;    /* TRUE if one of the leaves is marked "*" */
} PropGroup;

static void prop_group_free(gpointer data);

struct _UimToolbar {
  GtkBox parent_instance;

  UimToolbarKind kind;
  GtkSizeGroup *size_group;

  GPtrArray *prop_groups;   /* of PropGroup*, one per "branch" line */
  GPtrArray *tool_buttons;  /* of GtkWidget*, the launcher row */

  GtkWidget *app_menu_popover; /* right-click / long-press menu */

  GtkWidget *main_button; /* the placeholder "current state" button shown
                            * before uim answers with any branch/leaf;
                            * unparented (once) by the first
                            * prop_list_update() and NULLed out here so
                            * it is never touched again. */

  GHashTable *icon_cache;  /* name (+dark) -> GdkTexture* */
  gboolean with_dark_bg;
  gboolean custom_enabled;

  int uim_fd;
  guint io_watch_id;
  GIOChannel *io_channel;

  GtkWindow *host_window; /* weak; NULL unless a shell window registered */
};

G_DEFINE_TYPE(UimToolbar, uim_toolbar, GTK_TYPE_BOX)

/* the process hosts exactly one toolbar; the plain C uim-helper API
 * only accepts a no-argument disconnect callback, so a single back
 * pointer is unavoidable here (mirrors every prior uim frontend). */
static UimToolbar *the_toolbar = NULL;

static const char *
safe_gettext(const char *msgid)
{
  const char *p;

  for (p = msgid; *p && isascii((unsigned char)*p); p++)
    continue;

  return (*p) ? msgid : gettext(msgid);
}

static gboolean
has_n_strs(gchar **str_list, guint n)
{
  guint i;

  if (!str_list)
    return FALSE;

  for (i = 0; i < n; i++) {
    if (!str_list[i])
      return FALSE;
  }

  return TRUE;
}

/* ---------------------------- icons -------------------------------- */

static GdkTexture *
load_icon_texture(UimToolbar *self, const gchar *name)
{
  gchar *key;
  GdkTexture *texture;
  GString *filename;
  GdkPixbuf *pixbuf;
  struct stat st;

  key = g_strdup_printf("%s%s", name, self->with_dark_bg ? "#dark" : "#light");
  texture = g_hash_table_lookup(self->icon_cache, key);
  if (texture) {
    g_free(key);
    return texture;
  }

  filename = g_string_new(UIM_PIXMAPSDIR "/");
  g_string_append(filename, name);
  if (self->with_dark_bg)
    g_string_append(filename, "_dark_background");
  g_string_append(filename, ".png");

  if (self->with_dark_bg && stat(filename->str, &st) == -1) {
    /* not every icon has a dark-background variant */
    g_string_free(filename, TRUE);
    filename = g_string_new(UIM_PIXMAPSDIR "/");
    g_string_append(filename, name);
    g_string_append(filename, ".png");
  }

  pixbuf = gdk_pixbuf_new_from_file(filename->str, NULL);
  g_string_free(filename, TRUE);
  if (!pixbuf) {
    g_free(key);
    return NULL;
  }

  texture = gdk_texture_new_for_pixbuf(pixbuf);
  g_object_unref(pixbuf);

  g_hash_table_insert(self->icon_cache, key, texture);
  return texture;
}

static GtkWidget *
make_icon_or_label(UimToolbar *self, const gchar *icon_name,
                   const gchar *label_text)
{
  GdkTexture *texture = icon_name ? load_icon_texture(self, icon_name) : NULL;

  if (texture) {
    GtkWidget *image = gtk_image_new_from_paintable(GDK_PAINTABLE(texture));
    gtk_image_set_pixel_size(GTK_IMAGE(image), 16);
    return image;
  }

  return gtk_label_new(label_text ? label_text : "");
}

static void
reset_icon_cache(UimToolbar *self)
{
  g_hash_table_remove_all(self->icon_cache);
}

/* --------------------------- styling -------------------------------- */

static void
set_button_style(UimToolbar *self, GtkWidget *button)
{
  gtk_widget_add_css_class(button, "flat");

  switch (self->kind) {
  case UIM_TOOLBAR_KIND_ICON:
    gtk_widget_set_name(button, "uim-systray-button");
    break;
  case UIM_TOOLBAR_KIND_STANDALONE:
    gtk_widget_set_name(button, "uim-toolbar-button");
    break;
  case UIM_TOOLBAR_KIND_APPLET:
    gtk_widget_set_name(button, "uim-applet-button");
    break;
  }
}

static void
install_css(void)
{
  static gboolean installed = FALSE;
  GtkCssProvider *provider;

  if (installed)
    return;
  installed = TRUE;

  provider = gtk_css_provider_new();
  gtk_css_provider_load_from_string(provider,
    "#uim-systray-button { padding: 0 2px; }\n"
    "#uim-toolbar-button { padding: 0 5px; }\n"
    "#uim-applet-button { padding: 0 2px; }\n"
    ".uim-toolbar-menurow { padding: 2px 6px; }\n");
  gtk_style_context_add_provider_for_display(
    gdk_display_get_default(),
    GTK_STYLE_PROVIDER(provider),
    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref(provider);
}

/* ------------------------ helper protocol --------------------------- */

static void
save_default_im_internal(const char *im)
{
  uim_scm_callf("custom-set-value!", "yy",
               "custom-preserved-default-im-name", im);
  uim_custom_save_custom("custom-preserved-default-im-name");
}

static void
save_default_im(UimToolbar *self, const char *im)
{
  if (self->custom_enabled)
    uim_scm_call_with_gc_ready_stack(
      (uim_gc_gate_func_ptr)save_default_im_internal, (void *)im);
}

static gboolean
is_msg_imsw(const gchar *str)
{
  return g_str_has_prefix(str, "action_imsw_");
}

static gboolean
is_imsw_coverage_system_global(void)
{
  char *coverage = uim_scm_symbol_value_str("imsw-coverage");
  gboolean ret = coverage && !strcmp(coverage, "system-global");
  free(coverage);
  return ret;
}

static const char *
get_imsw_im(const gchar *str)
{
  return str + strlen("action_imsw_");
}

static void
send_prop_activate(UimToolbar *self, const gchar *action)
{
  GString *msg = g_string_new(action);

  g_string_prepend(msg, "prop_activate\n");
  g_string_append(msg, "\n");
  uim_helper_send_message(self->uim_fd, msg->str);

  if (is_msg_imsw(action) && is_imsw_coverage_system_global())
    save_default_im(self, get_imsw_im(action));

  g_string_free(msg, TRUE);
}

static void
launch_command(const gchar *command)
{
  GError *error = NULL;

  if (!command)
    return;

  if (!g_spawn_command_line_async(command, &error)) {
    GtkAlertDialog *dialog = gtk_alert_dialog_new(
      _("Cannot launch '%s'."), command);
    gtk_alert_dialog_show(dialog, NULL);
    g_object_unref(dialog);
    g_clear_error(&error);
  }
}

/* ---------------------------- prop groups ---------------------------- */

typedef struct {
  gchar *icon;
  gchar *label;
  gchar *tooltip;
  gchar *action;
  gboolean selected;
} PropLeaf;

static void
prop_leaf_free(gpointer data)
{
  PropLeaf *leaf = data;

  g_free(leaf->icon);
  g_free(leaf->label);
  g_free(leaf->tooltip);
  g_free(leaf->action);
  g_free(leaf);
}

static void
leaf_row_activated_cb(GtkListBox *list_box, GtkListBoxRow *row, gpointer data)
{
  UimToolbar *self = UIM_TOOLBAR(data);
  PropLeaf *leaf = g_object_get_data(G_OBJECT(row), "uim-leaf");

  gtk_popover_popdown(GTK_POPOVER(gtk_widget_get_ancestor(
    GTK_WIDGET(list_box), GTK_TYPE_POPOVER)));

  if (leaf && leaf->action)
    send_prop_activate(self, leaf->action);
}

static GtkWidget *
build_leaf_row(UimToolbar *self, PropLeaf *leaf, gboolean is_radio)
{
  GtkWidget *row, *box, *check, *icon, *label;

  row = gtk_list_box_row_new();
  gtk_widget_add_css_class(row, "uim-toolbar-menurow");
  g_object_set_data_full(G_OBJECT(row), "uim-leaf", leaf, prop_leaf_free);
  if (leaf->tooltip && *leaf->tooltip)
    gtk_widget_set_tooltip_text(row, leaf->tooltip);

  box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);

  if (is_radio) {
    check = gtk_image_new_from_icon_name(
      leaf->selected ? "object-select-symbolic" : NULL);
    gtk_widget_set_size_request(check, 16, 16);
    gtk_box_append(GTK_BOX(box), check);
  }

  icon = make_icon_or_label(self, leaf->icon, NULL);
  if (GTK_IS_LABEL(icon) && gtk_label_get_text(GTK_LABEL(icon))[0] == '\0')
    gtk_widget_set_visible(icon, FALSE);
  gtk_box_append(GTK_BOX(box), icon);

  label = gtk_label_new(leaf->label);
  gtk_label_set_xalign(GTK_LABEL(label), 0.0);
  gtk_widget_set_hexpand(label, TRUE);
  gtk_box_append(GTK_BOX(box), label);

  gtk_list_box_row_set_child(GTK_LIST_BOX_ROW(row), box);
  return row;
}

static PropGroup *
prop_group_new(UimToolbar *self, const gchar *icon_name,
              const gchar *iconic_label, const gchar *tooltip)
{
  PropGroup *group = g_new0(PropGroup, 1);

  group->button = gtk_button_new();
  gtk_button_set_child(GTK_BUTTON(group->button),
                       make_icon_or_label(self, icon_name, iconic_label));
  set_button_style(self, group->button);
  gtk_widget_set_tooltip_text(group->button, tooltip);
  gtk_size_group_add_widget(self->size_group, group->button);

  group->popover = gtk_popover_new();
  gtk_widget_set_parent(group->popover, group->button);

  group->list_box = gtk_list_box_new();
  gtk_list_box_set_selection_mode(GTK_LIST_BOX(group->list_box),
                                  GTK_SELECTION_NONE);
  g_signal_connect(group->list_box, "row-activated",
                   G_CALLBACK(leaf_row_activated_cb), self);
  gtk_popover_set_child(GTK_POPOVER(group->popover), group->list_box);

  g_signal_connect_swapped(group->button, "clicked",
                           G_CALLBACK(gtk_popover_popup), group->popover);

  return group;
}

static void
prop_group_add_leaf(PropGroup *group, UimToolbar *self, PropLeaf *leaf)
{
  if (leaf->selected)
    group->is_radio = TRUE;
}

static void
prop_group_free(gpointer data)
{
  PropGroup *group = data;

  /* Detach both widgets right here, rather than leaving group->button
   * for the caller to sweep up separately: a blanket "remove every
   * child of the box" sweep in prop_list_update() used to do that
   * job, but it could not tell a branch button apart from unrelated
   * widgets that are also parented to the box without being box
   * children (e.g. app_menu_popover, main_button), unparenting those
   * too and leaving their struct fields as dangling pointers.
   *
   * Order matters here: group->popover is parented onto group->button
   * (see prop_group_new()), so button is the last owner of a ref on
   * it. Unparenting button from the toolbar box *first* can drop
   * button's own last ref and finalize it while popover is still
   * attached as its child -- GTK4 requires a widget to have no
   * children left when it finalizes, so that ordering produced
   * "Finalizing <button>, but it still has children left: GtkPopover"
   * plus a cascade of GTK_IS_ACCESSIBLE/G_IS_OBJECT CRITICALs from
   * code that kept using the half-torn-down widget afterwards.
   * Detach the child (popover) before detaching its parent (button). */
  gtk_widget_unparent(group->popover);
  gtk_widget_unparent(group->button);
  g_free(group);
}

/* --------------------------- app menu -------------------------------- */

typedef struct {
  const gchar *label;
  const gchar *icon;
  const gchar *command;
} AppMenuEntry;

static void
app_menu_row_activated_cb(GtkListBox *list_box, GtkListBoxRow *row,
                          gpointer data)
{
  UimToolbar *self = UIM_TOOLBAR(data);
  const gchar *command = g_object_get_data(G_OBJECT(row), "uim-command");

  gtk_popover_popdown(GTK_POPOVER(self->app_menu_popover));

  if (command)
    launch_command(command);
  else
    gtk_window_destroy(self->host_window ? self->host_window :
                       GTK_WINDOW(gtk_widget_get_root(GTK_WIDGET(self))));
}

static GtkWidget *
build_app_menu_row(const gchar *label, const gchar *icon_name,
                   const gchar *command)
{
  GtkWidget *row = gtk_list_box_row_new();
  GtkWidget *box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
  GtkWidget *icon = gtk_image_new_from_icon_name(icon_name);
  GtkWidget *text = gtk_label_new(label);

  gtk_widget_add_css_class(row, "uim-toolbar-menurow");
  gtk_box_append(GTK_BOX(box), icon);
  gtk_label_set_xalign(GTK_LABEL(text), 0.0);
  gtk_widget_set_hexpand(text, TRUE);
  gtk_box_append(GTK_BOX(box), text);
  gtk_list_box_row_set_child(GTK_LIST_BOX_ROW(row), box);
  g_object_set_data_full(G_OBJECT(row), "uim-command",
                         g_strdup(command), g_free);
  return row;
}

static void
rebuild_app_menu(UimToolbar *self)
{
  GtkWidget *list_box;
  GtkWidget *child;
  guint i;

  if (self->app_menu_popover)
    gtk_widget_unparent(self->app_menu_popover);

  self->app_menu_popover = gtk_popover_new();
  gtk_widget_set_parent(self->app_menu_popover, GTK_WIDGET(self));
  gtk_popover_set_has_arrow(GTK_POPOVER(self->app_menu_popover), FALSE);

  list_box = gtk_list_box_new();
  gtk_list_box_set_selection_mode(GTK_LIST_BOX(list_box), GTK_SELECTION_NONE);
  g_signal_connect(list_box, "row-activated",
                   G_CALLBACK(app_menu_row_activated_cb), self);

  for (i = 0; i < uim_toolbar_command_table_len; i++) {
    UimToolbarCommand *entry = &uim_toolbar_command_table[i];
    gtk_list_box_append(GTK_LIST_BOX(list_box),
      build_app_menu_row(_(entry->desc), entry->icon, entry->command));
  }

  child = build_app_menu_row(_("Quit this toolbar"), "application-exit", NULL);
  gtk_list_box_append(GTK_LIST_BOX(list_box), child);

  gtk_popover_set_child(GTK_POPOVER(self->app_menu_popover), list_box);
}

static void
show_app_menu_at(UimToolbar *self, GtkWidget *anchor, double x, double y)
{
  GdkRectangle rect = { (int)x, (int)y, 1, 1 };

  if (gtk_widget_get_parent(self->app_menu_popover) != anchor) {
    gtk_widget_unparent(self->app_menu_popover);
    gtk_widget_set_parent(self->app_menu_popover, anchor);
  }
  gtk_popover_set_pointing_to(GTK_POPOVER(self->app_menu_popover), &rect);
  gtk_popover_popup(GTK_POPOVER(self->app_menu_popover));
}

static void
secondary_click_cb(GtkGestureClick *gesture, gint n_press,
                   gdouble x, gdouble y, gpointer data)
{
  UimToolbar *self = UIM_TOOLBAR(data);
  GtkWidget *widget = gtk_event_controller_get_widget(
    GTK_EVENT_CONTROLLER(gesture));

  show_app_menu_at(self, widget, x, y);
  gtk_gesture_set_state(GTK_GESTURE(gesture), GTK_EVENT_SEQUENCE_CLAIMED);
}

/* ---------------------- tool (launcher) buttons ---------------------- */

static void
tool_button_clicked_cb(GtkButton *button, gpointer data)
{
  const gchar *command = g_object_get_data(G_OBJECT(button), "uim-command");

  launch_command(command);
}

static void
clear_widget_list(GPtrArray *array, GtkWidget *box)
{
  guint i;

  for (i = 0; i < array->len; i++)
    gtk_box_remove(GTK_BOX(box), g_ptr_array_index(array, i));
  g_ptr_array_set_size(array, 0);
}

static void
rebuild_tool_buttons(UimToolbar *self)
{
  guint i;

  clear_widget_list(self->tool_buttons, GTK_WIDGET(self));

  for (i = 0; i < uim_toolbar_command_table_len; i++) {
    UimToolbarCommand *entry = &uim_toolbar_command_table[i];
    GtkWidget *button;

    if (!entry->show_button)
      continue;

    button = gtk_button_new();
    gtk_button_set_child(GTK_BUTTON(button),
                         make_icon_or_label(self, entry->icon, entry->label));
    set_button_style(self, button);
    gtk_widget_set_tooltip_text(button, _(entry->desc));
    gtk_size_group_add_widget(self->size_group, button);
    g_object_set_data(G_OBJECT(button), "uim-command", (gpointer)entry->command);
    g_signal_connect(button, "clicked",
                     G_CALLBACK(tool_button_clicked_cb), self);

    gtk_box_append(GTK_BOX(self), button);
    g_ptr_array_add(self->tool_buttons, button);
  }
}

/* ------------------------ property list parsing ----------------------- */

static gchar *
get_charset(gchar *line)
{
  gchar **tokens = g_strsplit(line, "=", 0);
  gchar *charset = NULL;

  if (tokens && tokens[0] && tokens[1] && !strcmp("charset", tokens[0]))
    charset = g_strdup(tokens[1]);
  g_strfreev(tokens);
  return charset;
}

static gchar *
convert_charset(const gchar *charset, const gchar *str)
{
  if (!charset)
    return NULL;
  return g_convert(str, strlen(str), "UTF-8", charset, NULL, NULL, NULL);
}

static void
helper_toolbar_check_custom(void)
{
  guint i;

  for (i = 0; i < uim_toolbar_command_table_len; i++)
    uim_toolbar_command_table[i].show_button =
      uim_scm_symbol_value_bool(
        uim_toolbar_command_table[i].custom_button_show_symbol);
}

static void
prop_list_update(UimToolbar *self, gchar **lines)
{
  gchar *charset = get_charset(lines[1]);
  PropGroup *group = NULL;
  char *display_time;
  gboolean is_hidden;
  guint i;

  g_ptr_array_set_size(self->prop_groups, 0); /* frees old groups+buttons,
                                                * see prop_group_free() */
  clear_widget_list(self->tool_buttons, GTK_WIDGET(self));

  /* The very first prop_list_update() replaces the placeholder
   * "current state" button with the real branch/leaf buttons; later
   * calls are no-ops here since main_button is NULL from then on.
   * (This used to be done by unconditionally removing every child of
   * the box, on the assumption that only leftover branch buttons
   * could still be attached -- but app_menu_popover and main_button
   * are *also* children of the box, just parented directly instead of
   * being box children, and that swept them up too without clearing
   * their struct fields, leaving dangling pointers behind.) */
  if (self->main_button) {
    gtk_widget_unparent(self->main_button);
    self->main_button = NULL;
  }

  display_time = uim_scm_c_symbol(uim_scm_symbol_value("toolbar-display-time"));
  is_hidden = strcmp(display_time, "mode") == 0;

  for (i = 0; lines[i] && strcmp("", lines[i]); i++) {
    gchar *utf8 = convert_charset(charset, lines[i]);
    gchar **cols = g_strsplit(utf8 ? utf8 : lines[i], "\t", 0);

    g_free(utf8);

    if (cols && cols[0]) {
      if (!strcmp("branch", cols[0]) && has_n_strs(cols, 4)) {
        const gchar *indication_id = cols[1];
        const gchar *iconic_label = safe_gettext(cols[2]);
        const gchar *tooltip = safe_gettext(cols[3]);

        group = prop_group_new(self, indication_id, iconic_label, tooltip);
        g_ptr_array_add(self->prop_groups, group);
        gtk_box_append(GTK_BOX(self), group->button);

        if (!is_hidden && (!strcmp(indication_id, "direct") ||
                           g_str_has_suffix(indication_id, "_direct")))
          is_hidden = TRUE;
      } else if (!strcmp("leaf", cols[0]) && has_n_strs(cols, 7) && group) {
        PropLeaf *leaf = g_new0(PropLeaf, 1);

        leaf->icon = g_strdup(cols[1]);
        leaf->label = g_strdup(safe_gettext(cols[3]));
        leaf->tooltip = g_strdup(safe_gettext(cols[4]));
        leaf->action = g_strdup(cols[5]);
        leaf->selected = !strcmp(cols[6], "*");

        prop_group_add_leaf(group, self, leaf);
        gtk_list_box_append(GTK_LIST_BOX(group->list_box),
                            build_leaf_row(self, leaf, group->is_radio));
      }
    }
    g_strfreev(cols);
  }

  /* radio-ness of a group is only known once every leaf has been seen;
   * redo the check marks now that group->is_radio is final. */
  for (i = 0; i < self->prop_groups->len; i++) {
    PropGroup *g = g_ptr_array_index(self->prop_groups, i);
    GtkWidget *row;
    int idx = 0;

    if (!g->is_radio)
      continue;
    for (row = gtk_widget_get_first_child(g->list_box); row;
        row = gtk_widget_get_next_sibling(row), idx++) {
      PropLeaf *leaf = g_object_get_data(G_OBJECT(row), "uim-leaf");
      GtkWidget *box = gtk_list_box_row_get_child(GTK_LIST_BOX_ROW(row));
      GtkWidget *check = gtk_widget_get_first_child(box);

      if (GTK_IS_IMAGE(check))
        gtk_image_set_from_icon_name(GTK_IMAGE(check),
          leaf->selected ? "object-select-symbolic" : NULL);
    }
  }

  rebuild_tool_buttons(self);

  is_hidden = is_hidden && strcmp(display_time, "always") != 0;
  if (self->host_window)
    gtk_widget_set_visible(GTK_WIDGET(self->host_window), !is_hidden);

  g_free(charset);
}

static void
helper_str_received(UimToolbar *self, gchar *str)
{
  gchar **lines = g_strsplit(str, "\n", 0);

  if (lines && lines[0]) {
    if (!strcmp("prop_list_update", lines[0])) {
      prop_list_update(self, lines);
    } else if (!strcmp("custom_reload_notify", lines[0])) {
      uim_prop_reload_configs();
      helper_toolbar_check_custom();
      self->with_dark_bg =
        uim_scm_symbol_value_bool("toolbar-icon-for-dark-background?");
      reset_icon_cache(self);
    }
  }
  g_strfreev(lines);
}

static gboolean
fd_read_cb(GIOChannel *channel, GIOCondition condition, gpointer data)
{
  UimToolbar *self = UIM_TOOLBAR(data);
  int fd = g_io_channel_unix_get_fd(channel);
  gchar *msg;

  uim_helper_read_proc(fd);
  while ((msg = uim_helper_get_message())) {
    helper_str_received(self, msg);
    free(msg);
  }

  return G_SOURCE_CONTINUE;
}

static void
helper_disconnect_cb(void)
{
  if (!the_toolbar)
    return;
  the_toolbar->uim_fd = -1;
  if (the_toolbar->io_watch_id) {
    g_source_remove(the_toolbar->io_watch_id);
    the_toolbar->io_watch_id = 0;
  }
  g_clear_pointer(&the_toolbar->io_channel, g_io_channel_unref);
}

static void
check_helper_connection(UimToolbar *self)
{
  if (self->uim_fd >= 0)
    return;

  self->uim_fd = uim_helper_init_client_fd(helper_disconnect_cb);
  if (self->uim_fd <= 0)
    return;

  self->io_channel = g_io_channel_unix_new(self->uim_fd);
  self->io_watch_id = g_io_add_watch(self->io_channel,
                                     G_IO_IN | G_IO_HUP | G_IO_ERR,
                                     fd_read_cb, self);
}

/* ----------------------------- GObject -------------------------------- */

static void
uim_toolbar_dispose(GObject *object)
{
  UimToolbar *self = UIM_TOOLBAR(object);

  if (self->io_watch_id)
    g_source_remove(self->io_watch_id);
  self->io_watch_id = 0;
  g_clear_pointer(&self->io_channel, g_io_channel_unref);

  g_clear_pointer(&self->prop_groups, g_ptr_array_unref);
  g_clear_pointer(&self->tool_buttons, g_ptr_array_unref);
  g_clear_pointer(&self->icon_cache, g_hash_table_unref);

  if (self->app_menu_popover) {
    gtk_widget_unparent(self->app_menu_popover);
    self->app_menu_popover = NULL;
  }

  g_clear_object(&self->size_group);

  if (the_toolbar == self)
    the_toolbar = NULL;

  G_OBJECT_CLASS(uim_toolbar_parent_class)->dispose(object);
}

static void
uim_toolbar_class_init(UimToolbarClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);

  object_class->dispose = uim_toolbar_dispose;
}

static void
uim_toolbar_init(UimToolbar *self)
{
  gtk_orientable_set_orientation(GTK_ORIENTABLE(self), GTK_ORIENTATION_HORIZONTAL);

  self->prop_groups = g_ptr_array_new_with_free_func(prop_group_free);
  self->tool_buttons = g_ptr_array_new();
  self->icon_cache = g_hash_table_new_full(g_str_hash, g_str_equal,
                                           g_free, g_object_unref);
  self->size_group = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
  self->uim_fd = -1;
}

/* ------------------------------ public API ----------------------------- */

GtkWidget *
uim_toolbar_new(UimToolbarKind kind)
{
  UimToolbar *self = g_object_new(UIM_TYPE_TOOLBAR, NULL);

  install_css();

  self->kind = kind;
  the_toolbar = self;

  if (uim_scm_symbol_value_bool("uim-toolbar-save-default-im?"))
    self->custom_enabled = (gboolean)uim_custom_enable();

  helper_toolbar_check_custom();
  self->with_dark_bg =
    uim_scm_symbol_value_bool("toolbar-icon-for-dark-background?");

  rebuild_app_menu(self);

  /* the always-present "current state" button, shown before uim
   * answers with any branch/leaf; a secondary click on it (or anywhere
   * else on the bar) opens the application menu. */
  self->main_button = gtk_button_new();
  gtk_button_set_child(GTK_BUTTON(self->main_button),
                       make_icon_or_label(self, "uim-icon", " x"));
  set_button_style(self, self->main_button);
  gtk_size_group_add_widget(self->size_group, self->main_button);
  gtk_box_append(GTK_BOX(self), self->main_button);

  {
    GtkGesture *secondary = gtk_gesture_click_new();
    gtk_gesture_single_set_button(GTK_GESTURE_SINGLE(secondary),
                                  GDK_BUTTON_SECONDARY);
    g_signal_connect(secondary, "pressed",
                     G_CALLBACK(secondary_click_cb), self);
    gtk_widget_add_controller(GTK_WIDGET(self), GTK_EVENT_CONTROLLER(secondary));
  }

  if (kind != UIM_TOOLBAR_KIND_ICON) {
    check_helper_connection(self);
    uim_helper_client_get_prop_list();
    uim_helper_send_message(self->uim_fd, "im_list_get\n");
  }

  return GTK_WIDGET(self);
}

void
uim_toolbar_set_host_window(UimToolbar *toolbar, GtkWindow *window)
{
  g_return_if_fail(UIM_IS_TOOLBAR(toolbar));

  toolbar->host_window = window;
}
