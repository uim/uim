/*

  uim-im-switcher-gtk4.c: input method switcher dialog for GTK 4

  Copyright (c) 2004-2026 uim Project https://github.com/uim/uim

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

  Like its GTK 3 predecessor, this dialog is entirely input-method
  agnostic: the list of engines to offer comes solely from uim's
  generic "im_list" helper message (name / language / description /
  is-current columns), so anthy, mozc, or any other engine that
  registers itself with uim shows up here with no per-engine code.
*/

#include <config.h>

#include <gtk/gtk.h>

#include <locale.h>
#include <string.h>
#include <stdlib.h>

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <uim/uim-custom.h>
#include <uim/uim-scm.h>
#include "uim/gettext.h"

/* ------------------------- list model item --------------------------- */

#define SWITCHER_TYPE_ITEM (switcher_item_get_type())
G_DECLARE_FINAL_TYPE(SwitcherItem, switcher_item, SWITCHER, ITEM, GObject)

struct _SwitcherItem {
  GObject parent_instance;
  gchar *name;
  gchar *language;
  gchar *description;
  gboolean current;
};

G_DEFINE_TYPE(SwitcherItem, switcher_item, G_TYPE_OBJECT)

static void
switcher_item_finalize(GObject *object)
{
  SwitcherItem *self = SWITCHER_ITEM(object);

  g_free(self->name);
  g_free(self->language);
  g_free(self->description);

  G_OBJECT_CLASS(switcher_item_parent_class)->finalize(object);
}

static void
switcher_item_class_init(SwitcherItemClass *klass)
{
  G_OBJECT_CLASS(klass)->finalize = switcher_item_finalize;
}

static void
switcher_item_init(SwitcherItem *self)
{
}

static SwitcherItem *
switcher_item_new(const gchar *name, const gchar *language,
                  const gchar *description, gboolean current)
{
  SwitcherItem *item = g_object_new(SWITCHER_TYPE_ITEM, NULL);

  item->name = g_strdup(name);
  item->language = g_strdup(language);
  item->description = g_strdup(description);
  item->current = current;
  return item;
}

/* ------------------------------ globals -------------------------------- */

enum switcher_coverage {
  IMSW_COVERAGE_WHOLE_DESKTOP,
  IMSW_COVERAGE_THIS_APPLICATION_ONLY,
  IMSW_COVERAGE_THIS_TEXT_AREA_ONLY
};

static guint read_tag;
static int uim_fd = -1;
static gchar *im_list_str_old;
static GtkWidget *switcher_window;
static GtkColumnView *switcher_view;
static GListStore *switcher_store;
static GtkSingleSelection *switcher_selection;
static gboolean custom_enabled;
static enum switcher_coverage coverage = IMSW_COVERAGE_WHOLE_DESKTOP;

static void check_helper_connection(void);
static void parse_helper_str_im_list(const char *im_list_str_new);

/* ------------------------------ helpers -------------------------------- */

static gchar *
get_selected_im_name(void)
{
  SwitcherItem *item =
    gtk_single_selection_get_selected_item(switcher_selection);

  return item ? g_strdup(item->name) : NULL;
}

static void
send_message_im_change(const gchar *type)
{
  gchar *im_name = get_selected_im_name();
  GString *msg;

  if (!im_name)
    return;

  check_helper_connection();
  msg = g_string_new(type);
  g_string_append(msg, im_name);
  g_string_append(msg, "\n");
  g_free(im_name);
  uim_helper_send_message(uim_fd, msg->str);
  g_string_free(msg, TRUE);
}

static void
save_default_im(void)
{
  gchar *im_name;

  if (!custom_enabled)
    return;

  im_name = get_selected_im_name();
  if (!im_name)
    return;

  uim_scm_callf("custom-set-value!", "yy",
               "custom-preserved-default-im-name", im_name);
  uim_custom_save_custom("custom-preserved-default-im-name");
  g_free(im_name);
}

static void
change_input_method(void)
{
  switch (coverage) {
  case IMSW_COVERAGE_WHOLE_DESKTOP:
    send_message_im_change("im_change_whole_desktop\n");
    save_default_im();
    break;
  case IMSW_COVERAGE_THIS_APPLICATION_ONLY:
    send_message_im_change("im_change_this_application_only\n");
    break;
  case IMSW_COVERAGE_THIS_TEXT_AREA_ONLY:
    send_message_im_change("im_change_this_text_area_only\n");
    break;
  }
}

static void
apply_clicked_cb(GtkButton *button, gpointer data)
{
  change_input_method();
}

static void
close_clicked_cb(GtkButton *button, gpointer data)
{
  gtk_window_destroy(GTK_WINDOW(switcher_window));
}

static void
ok_clicked_cb(GtkButton *button, gpointer data)
{
  change_input_method();
  gtk_window_destroy(GTK_WINDOW(switcher_window));
}

static void
coverage_toggled_cb(GtkCheckButton *button, gpointer data)
{
  if (gtk_check_button_get_active(button))
    coverage = GPOINTER_TO_INT(data);
}

/* ------------------------------ list view -------------------------------- */

static void
setup_label_cb(GtkSignalListItemFactory *factory, GtkListItem *list_item,
              gpointer data)
{
  GtkWidget *label = gtk_label_new(NULL);

  gtk_label_set_xalign(GTK_LABEL(label), 0.0);
  gtk_list_item_set_child(list_item, label);
}

static void
bind_name_cb(GtkSignalListItemFactory *factory, GtkListItem *list_item,
            gpointer data)
{
  SwitcherItem *item = gtk_list_item_get_item(list_item);
  GtkWidget *label = gtk_list_item_get_child(list_item);

  gtk_label_set_text(GTK_LABEL(label), item->name);
}

static void
bind_lang_cb(GtkSignalListItemFactory *factory, GtkListItem *list_item,
            gpointer data)
{
  SwitcherItem *item = gtk_list_item_get_item(list_item);
  GtkWidget *label = gtk_list_item_get_child(list_item);

  gtk_label_set_text(GTK_LABEL(label),
                     (item->language && *item->language) ?
                     item->language : "-");
}

static void
bind_desc_cb(GtkSignalListItemFactory *factory, GtkListItem *list_item,
            gpointer data)
{
  SwitcherItem *item = gtk_list_item_get_item(list_item);
  GtkWidget *label = gtk_list_item_get_child(list_item);

  gtk_label_set_text(GTK_LABEL(label),
                     (item->description && *item->description) ?
                     item->description : "-");
}

static GtkColumnViewColumn *
make_column(const gchar *title,
           GCallback bind_cb)
{
  GtkListItemFactory *factory = gtk_signal_list_item_factory_new();
  GtkColumnViewColumn *column;

  g_signal_connect(factory, "setup", G_CALLBACK(setup_label_cb), NULL);
  g_signal_connect(factory, "bind", bind_cb, NULL);

  column = gtk_column_view_column_new(title, factory);
  gtk_column_view_column_set_expand(column, TRUE);
  gtk_column_view_column_set_resizable(column, TRUE);
  return column;
}

static GtkWidget *
create_switcher_view(void)
{
  switcher_store = g_list_store_new(SWITCHER_TYPE_ITEM);
  switcher_selection = gtk_single_selection_new(G_LIST_MODEL(switcher_store));

  switcher_view =
    GTK_COLUMN_VIEW(gtk_column_view_new(GTK_SELECTION_MODEL(switcher_selection)));

  gtk_column_view_append_column(switcher_view,
    make_column(_("InputMethodName"), G_CALLBACK(bind_name_cb)));
  gtk_column_view_append_column(switcher_view,
    make_column(_("Language"), G_CALLBACK(bind_lang_cb)));
  gtk_column_view_append_column(switcher_view,
    make_column(_("Description"), G_CALLBACK(bind_desc_cb)));

  g_signal_connect_swapped(switcher_view, "activate",
                           G_CALLBACK(ok_clicked_cb), NULL);

  return GTK_WIDGET(switcher_view);
}

/* -------------------------------- window --------------------------------- */

static void
window_active_changed_cb(GObject *object, GParamSpec *pspec, gpointer data)
{
  if (gtk_window_is_active(GTK_WINDOW(object))) {
    check_helper_connection();
    uim_helper_send_message(uim_fd, "im_list_get\n");
  }
}

static GtkWidget *
labeled_button(const gchar *label, const gchar *css_class)
{
  GtkWidget *button = gtk_button_new_with_label(label);

  if (css_class)
    gtk_widget_add_css_class(button, css_class);
  return button;
}

static void
create_switcher(GtkApplication *app)
{
  GtkWidget *vbox, *hbox, *coverage_box, *frame, *button_box, *scrolled;
  GtkWidget *radio0, *radio1, *radio2, *button;

  /* Must be registered with the GtkApplication: otherwise GApplication
   * sees zero held windows the instant activate_cb() returns and quits
   * the whole process immediately (window flashes and disappears). */
  switcher_window = gtk_application_window_new(app);
  gtk_window_set_title(GTK_WINDOW(switcher_window),
                       _("uim input method switcher"));
  gtk_window_set_icon_name(GTK_WINDOW(switcher_window), "uim");
  gtk_window_set_default_size(GTK_WINDOW(switcher_window), 480, 360);

  g_signal_connect(switcher_window, "destroy",
                   G_CALLBACK(gtk_window_destroy), NULL);
  g_signal_connect(switcher_window, "notify::is-active",
                   G_CALLBACK(window_active_changed_cb), NULL);

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
  gtk_widget_set_margin_top(vbox, 8);
  gtk_widget_set_margin_bottom(vbox, 8);
  gtk_widget_set_margin_start(vbox, 8);
  gtk_widget_set_margin_end(vbox, 8);
  gtk_window_set_child(GTK_WINDOW(switcher_window), vbox);

  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
  gtk_widget_set_vexpand(hbox, TRUE);
  gtk_box_append(GTK_BOX(vbox), hbox);

  scrolled = gtk_scrolled_window_new();
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_set_hexpand(scrolled, TRUE);
  gtk_widget_set_vexpand(scrolled, TRUE);
  gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(scrolled),
                                create_switcher_view());
  gtk_box_append(GTK_BOX(hbox), scrolled);

  frame = gtk_frame_new(_("Effective coverage"));
  gtk_box_append(GTK_BOX(vbox), frame);

  coverage_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
  gtk_widget_set_margin_top(coverage_box, 10);
  gtk_widget_set_margin_bottom(coverage_box, 10);
  gtk_widget_set_margin_start(coverage_box, 10);
  gtk_widget_set_margin_end(coverage_box, 10);
  gtk_frame_set_child(GTK_FRAME(frame), coverage_box);

  radio0 = gtk_check_button_new_with_label(_("whole desktop"));
  radio1 = gtk_check_button_new_with_label(_("current application only"));
  radio2 = gtk_check_button_new_with_label(_("current text area only"));
  gtk_check_button_set_group(GTK_CHECK_BUTTON(radio1), GTK_CHECK_BUTTON(radio0));
  gtk_check_button_set_group(GTK_CHECK_BUTTON(radio2), GTK_CHECK_BUTTON(radio0));

  gtk_box_append(GTK_BOX(coverage_box), radio0);
  gtk_box_append(GTK_BOX(coverage_box), radio1);
  gtk_box_append(GTK_BOX(coverage_box), radio2);

  g_signal_connect(radio0, "toggled", G_CALLBACK(coverage_toggled_cb),
                   GINT_TO_POINTER(IMSW_COVERAGE_WHOLE_DESKTOP));
  g_signal_connect(radio1, "toggled", G_CALLBACK(coverage_toggled_cb),
                   GINT_TO_POINTER(IMSW_COVERAGE_THIS_APPLICATION_ONLY));
  g_signal_connect(radio2, "toggled", G_CALLBACK(coverage_toggled_cb),
                   GINT_TO_POINTER(IMSW_COVERAGE_THIS_TEXT_AREA_ONLY));

  gtk_check_button_set_active(GTK_CHECK_BUTTON(radio0), TRUE);

  button_box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
  gtk_widget_set_halign(button_box, GTK_ALIGN_END);
  gtk_box_append(GTK_BOX(vbox), button_box);

  button = labeled_button(_("_Apply"), NULL);
  gtk_button_set_use_underline(GTK_BUTTON(button), TRUE);
  g_signal_connect(button, "clicked", G_CALLBACK(apply_clicked_cb), NULL);
  gtk_box_append(GTK_BOX(button_box), button);

  button = labeled_button(_("_Close"), NULL);
  gtk_button_set_use_underline(GTK_BUTTON(button), TRUE);
  g_signal_connect(button, "clicked", G_CALLBACK(close_clicked_cb), NULL);
  gtk_box_append(GTK_BOX(button_box), button);

  button = labeled_button(_("_OK"), "suggested-action");
  gtk_button_set_use_underline(GTK_BUTTON(button), TRUE);
  g_signal_connect(button, "clicked", G_CALLBACK(ok_clicked_cb), NULL);
  gtk_box_append(GTK_BOX(button_box), button);

  gtk_widget_grab_focus(GTK_WIDGET(switcher_view));
  gtk_window_present(GTK_WINDOW(switcher_window));
}

/* ------------------------------ helper protocol -------------------------- */

static const char *
get_text(const char *str)
{
  return (str && *str) ? gettext(str) : "-";
}

static void
parse_helper_str_im_list(const char *im_list_str_new)
{
  gchar **lines;
  guint i;
  guint select_index = G_MAXUINT;

  if (im_list_str_old && !strcmp(im_list_str_new, im_list_str_old))
    return;

  lines = g_strsplit(im_list_str_new, "\n", -1);
  g_list_store_remove_all(switcher_store);

  for (i = 2; lines[i] && *lines[i]; i++) {
    gchar **info = g_strsplit(lines[i], "\t", -1);

    if (info && info[0] && info[1] && info[2]) {
      gboolean current = info[3] && *info[3];
      SwitcherItem *item =
        switcher_item_new(info[0], get_text(info[1]), get_text(info[2]),
                          current);

      if (current)
        select_index = g_list_model_get_n_items(G_LIST_MODEL(switcher_store));
      g_list_store_append(switcher_store, item);
      g_object_unref(item);
    }
    g_strfreev(info);
  }
  g_strfreev(lines);

  if (select_index != G_MAXUINT)
    gtk_single_selection_set_selected(switcher_selection, select_index);

  g_free(im_list_str_old);
  im_list_str_old = g_strdup(im_list_str_new);
}

static void
parse_helper_str(const char *sent_str)
{
  if (g_str_has_prefix(sent_str, "im_list")) {
    parse_helper_str_im_list(sent_str);
  } else if (g_str_has_prefix(sent_str, "im_switcher_start")) {
    uim_helper_send_message(uim_fd, "im_switcher_quit\n");
  } else if (g_str_has_prefix(sent_str, "im_switcher_quit")) {
    gtk_window_destroy(GTK_WINDOW(switcher_window));
  }
}

static void
helper_disconnect_cb(void)
{
  uim_fd = -1;
  if (read_tag) {
    g_source_remove(read_tag);
    read_tag = 0;
  }
}

static gboolean
fd_read_cb(GIOChannel *channel, GIOCondition c, gpointer p)
{
  int fd = g_io_channel_unix_get_fd(channel);
  char *msg;

  uim_helper_read_proc(fd);
  while ((msg = uim_helper_get_message())) {
    parse_helper_str(msg);
    free(msg);
  }
  return G_SOURCE_CONTINUE;
}

static void
check_helper_connection(void)
{
  if (uim_fd >= 0)
    return;

  uim_fd = uim_helper_init_client_fd(helper_disconnect_cb);
  if (uim_fd > 0) {
    GIOChannel *channel = g_io_channel_unix_new(uim_fd);

    read_tag = g_io_add_watch(channel, G_IO_IN | G_IO_HUP | G_IO_ERR,
                              fd_read_cb, NULL);
    g_io_channel_unref(channel);
  }
}

/* ---------------------------------- main ---------------------------------- */

static void
activate_cb(GtkApplication *app, gpointer user_data)
{
  create_switcher(app);
}

int
main(int argc, char *argv[])
{
  GtkApplication *app;
  int status;

  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8");

  if (uim_init() < 0) {
    fprintf(stderr, "uim_init() failed.\n");
    exit(EXIT_FAILURE);
  }
  custom_enabled = uim_custom_enable();

  check_helper_connection();
  /* let a pre-existing instance take over instead of stacking dialogs */
  uim_helper_send_message(uim_fd, "im_switcher_start\n");
  uim_helper_send_message(uim_fd, "im_list_get\n");

  app = gtk_application_new("org.uim.SwitcherGtk4", G_APPLICATION_NON_UNIQUE);
  g_signal_connect(app, "activate", G_CALLBACK(activate_cb), NULL);
  status = g_application_run(G_APPLICATION(app), argc, argv);
  g_object_unref(app);

  uim_quit();

  return status;
}
