/*

  Copyright (c) 2005-2026 uim Project https://github.com/uim/uim

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

  GTK 4 note: gtk_main(), gtk_dialog_run(), GtkContainer, GtkButtonBox,
  GTK_STOCK_*, GdkEventKey, GdkScreen, and GTK_WIN_POS_* are all gone.
  (Careful: a literal "GTK_STOCK_*" immediately followed by "/" closes
  this comment early, so identifiers here are separated by commas, not
  slashes.) The event loop is now a GtkApplication; blocking
  confirmation dialogs are now
  async GtkAlertDialog callbacks; "container add" calls became the
  type-specific setters (gtk_window_set_child, gtk_scrolled_window_set_child,
  gtk_frame_set_child); the button box at the bottom of the window is a
  plain end-aligned GtkBox; and screen size is read from the first
  GdkMonitor rather than GdkScreen, since GTK 4 dropped both GdkScreen
  and any portable way to explicitly position a top-level window.

*/

#include <config.h>

#include <glib.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <locale.h>

#include "uim/uim.h"
#include "uim/uim-custom.h"
#include "uim/uim-scm.h"
#include "uim/gettext.h"
#include "gtk-custom-widgets-gtk4.h"
#include "key-util-gtk4.h"

#define DEFAULT_WINDOW_WIDTH_MAX 800
#define DEFAULT_WINDOW_HEIGHT_MAX 600
#define USE_CHANGES_SENSITIVE_OK_BUTTON 0

static GtkWidget *pref_window = NULL;
static GtkWidget *pref_tree_view = NULL;
static GtkWidget *pref_hbox = NULL;
static GtkWidget *current_group_widget = NULL;

gboolean uim_pref_gtk_value_changed = FALSE;
static GtkWidget *pref_apply_button = NULL;
static GtkWidget *pref_ok_button = NULL;

enum
{
  GROUP_COLUMN = 0,
  GROUP_WIDGET = 1,
  GROUP_SYM = 2,
  NUM_COLUMNS
};

void uim_pref_gtk_mark_value_changed(void);
void uim_pref_gtk_unmark_value_changed(void);

static gboolean	pref_tree_selection_changed(GtkTreeSelection *selection,
					     gpointer data);
static GtkWidget *create_pref_treeview(void);
static GtkWidget *create_group_widget(const char *group_name);
static void create_sub_group_widgets(GtkWidget *parent_widget,
				     const char *parent_group);

void
uim_pref_gtk_mark_value_changed(void)
{
  uim_pref_gtk_value_changed = TRUE;
  gtk_widget_set_sensitive(pref_apply_button, TRUE);
#if USE_CHANGES_SENSITIVE_OK_BUTTON
  gtk_widget_set_sensitive(pref_ok_button, TRUE);
#endif
}

void
uim_pref_gtk_unmark_value_changed(void)
{
  uim_pref_gtk_value_changed = FALSE;
  gtk_widget_set_sensitive(pref_apply_button, FALSE);
#if USE_CHANGES_SENSITIVE_OK_BUTTON
  gtk_widget_set_sensitive(pref_ok_button, FALSE);
#endif
}

static gboolean
pref_tree_selection_changed(GtkTreeSelection *selection,
			     gpointer data)
{
  GtkTreeStore *store;
  GtkTreeIter iter;
  GtkTreeModel *model;
  char *group_name, *group_sym;
  GtkWidget *group_widget;

  if (gtk_tree_selection_get_selected(selection, &model, &iter) == FALSE)
    return TRUE;

  store = GTK_TREE_STORE(model);
  gtk_tree_model_get(model, &iter,
		     GROUP_COLUMN, &group_name,
		     GROUP_WIDGET, &group_widget,
		     GROUP_SYM, &group_sym,
		     -1);

  if (group_name == NULL)
    return TRUE;

  if (group_widget == NULL) {
    group_widget = create_group_widget(group_sym);
    gtk_tree_store_set(store, &iter, GROUP_WIDGET, group_widget, -1);
  }

  /* hide current selected group's widget */
  if (current_group_widget)
    gtk_widget_set_visible(current_group_widget, FALSE);

  /* whether group_widget is already packed or not */
  if (!gtk_widget_get_parent(group_widget))
    gtk_box_append(GTK_BOX(pref_hbox), group_widget);

  /* show selected group's widget */
  gtk_widget_set_visible(group_widget, TRUE);

  current_group_widget = group_widget;

  free(group_name);
  free(group_sym);
  return TRUE;
}


static void
do_quit(void)
{
  GApplication *app = g_application_get_default();

  if (app)
    g_application_quit(app);
}

static void
quit_confirm_alert_cb(GObject *source, GAsyncResult *result, gpointer data)
{
  GError *error = NULL;
  int button = gtk_alert_dialog_choose_finish(GTK_ALERT_DIALOG(source),
					      result, &error);

  if (error) {
    g_error_free(error);
    return;
  }

  /* button 1 == "_Quit" (see quit_confirm() below) */
  if (button == 1)
    do_quit();
}

static void
quit_confirm(void)
{
  if (uim_pref_gtk_value_changed) {
    GtkAlertDialog *alert;
    const char *buttons[] = { N_("_Cancel"), N_("_Quit"), NULL };

    alert = gtk_alert_dialog_new(
	"%s", _("Some value(s) have been changed.\n"
		"Do you really quit this program?"));
    gtk_alert_dialog_set_buttons(alert, (const char * const *)buttons);
    gtk_alert_dialog_set_cancel_button(alert, 0);
    gtk_alert_dialog_set_default_button(alert, 0);
    gtk_alert_dialog_choose(alert, GTK_WINDOW(pref_window), NULL,
			    quit_confirm_alert_cb, NULL);
    g_object_unref(alert);
  } else {
    do_quit();
  }
}

static gboolean
key_pressed_cb(GtkEventControllerKey *controller, guint keyval,
	      guint keycode, GdkModifierType state, gpointer data)
{
  if (keyval == GDK_KEY_Escape) {
    quit_confirm();
    return TRUE;
  }

  return FALSE;
}

static gboolean
close_request_cb(GtkWindow *window, gpointer data)
{
  quit_confirm();
  return TRUE; /* stop the default close handling; quit_confirm() decides */
}

static GtkWidget *
create_pref_treeview(void)
{
  GtkTreeStore *tree_store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeIter iter;
  char **primary_groups, **grp;
  GtkTreeSelection *selection;
  GtkTreePath *first_path;
  tree_store = gtk_tree_store_new (NUM_COLUMNS,
				   G_TYPE_STRING,
				   GTK_TYPE_WIDGET,
				   G_TYPE_STRING);

  pref_tree_view = gtk_tree_view_new();

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Group"),
						    renderer,
						    "text", GROUP_COLUMN,
						    (const gchar *)NULL);
  gtk_tree_view_column_set_sort_column_id(column, 0);
  gtk_tree_view_append_column(GTK_TREE_VIEW(pref_tree_view), column);

  primary_groups = uim_custom_primary_groups();
  for (grp = primary_groups; *grp; grp++) {
    struct uim_custom_group *group = uim_custom_group_get(*grp);
    gtk_tree_store_append (tree_store, &iter, NULL/* parent iter */);
    /* only set the widget of the first row for now */
    if (grp == primary_groups) {
      gtk_tree_store_set (tree_store, &iter,
			  GROUP_COLUMN, group->label,
			  GROUP_WIDGET, create_group_widget(*grp),
			  GROUP_SYM, *grp,
			  -1);
    } else {
      gtk_tree_store_set (tree_store, &iter,
			  GROUP_COLUMN, group->label,
			  GROUP_WIDGET, NULL,
			  GROUP_SYM, *grp,
			  -1);
    }
    uim_custom_group_free(group);
  }
  uim_custom_symbol_list_free( primary_groups );

  gtk_tree_view_set_model (GTK_TREE_VIEW(pref_tree_view),
			   GTK_TREE_MODEL(tree_store));
  g_object_unref (tree_store);
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (pref_tree_view));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);
  g_signal_connect (G_OBJECT(selection), "changed",
		    G_CALLBACK(pref_tree_selection_changed), NULL);

  first_path = gtk_tree_path_new_from_indices (0, -1);

  gtk_tree_view_set_cursor(GTK_TREE_VIEW(pref_tree_view),
			   first_path, NULL, FALSE);
  gtk_tree_path_free(first_path);

  return pref_tree_view;
}

static void
ok_button_clicked(GtkButton *button, gpointer user_data)
{
  if (uim_pref_gtk_value_changed) {
    uim_custom_save();
    uim_custom_broadcast_reload_request();
    uim_pref_gtk_unmark_value_changed();
  }

  do_quit();
}

static void
apply_button_clicked(GtkButton *button, gpointer user_data)
{
  if (uim_pref_gtk_value_changed) {
    uim_custom_save();
    uim_custom_broadcast_reload_request();
    uim_pref_gtk_unmark_value_changed();
  }
}

/*
 * Recursively walk every descendant of widget and reset any that carry
 * a uim-custom symbol back to its default value. uim_pref_gtk_set_default_value()
 * is a no-op for widgets with no attached symbol, so it is safe (and,
 * without GtkContainer's "is this a container" check, necessary in
 * GTK 4) to simply recurse into every widget regardless of type.
 */
static void
set_to_default_recursive(GtkWidget *widget)
{
  GtkWidget *child;

  uim_pref_gtk_set_default_value(widget);

  for (child = gtk_widget_get_first_child(widget);
       child != NULL;
       child = gtk_widget_get_next_sibling(child)) {
    set_to_default_recursive(child);
  }

  uim_pref_gtk_mark_value_changed();
}

static void
defaults_button_clicked(GtkButton *button, gpointer user_data)
{
  GtkWidget *child;

  for (child = gtk_widget_get_first_child(current_group_widget);
       child != NULL;
       child = gtk_widget_get_next_sibling(child)) {
    set_to_default_recursive(child);
  }
}

static GtkWidget *
create_setting_button_box(const char *group_name)
{
  GtkWidget *setting_button_box;
  GtkWidget *button;

  setting_button_box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
  gtk_widget_set_halign(setting_button_box, GTK_ALIGN_END);
  gtk_widget_set_hexpand(setting_button_box, TRUE);

  /* Defaults button */
  button = gtk_button_new_with_mnemonic(_("_Defaults"));
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(defaults_button_clicked), (gpointer) group_name);
  gtk_box_append(GTK_BOX(setting_button_box), button);
  gtk_widget_set_tooltip_text(button, _("Revert all changes to default"));

  /* Apply button */
  pref_apply_button = gtk_button_new_with_mnemonic(_("_Apply"));
  g_signal_connect(G_OBJECT(pref_apply_button), "clicked",
		   G_CALLBACK(apply_button_clicked), (gpointer) group_name);
  gtk_widget_set_sensitive(pref_apply_button, FALSE);
  gtk_box_append(GTK_BOX(setting_button_box), pref_apply_button);
  gtk_widget_set_tooltip_text(pref_apply_button, _("Apply all changes"));

  /* Cancel button */
  button = gtk_button_new_with_mnemonic(_("_Cancel"));
  g_signal_connect_swapped(G_OBJECT(button), "clicked",
			  G_CALLBACK(quit_confirm), NULL);
  gtk_box_append(GTK_BOX(setting_button_box), button);
  gtk_widget_set_tooltip_text(button, _("Quit this application without applying changes"));

  /* OK button */
  pref_ok_button = gtk_button_new_with_mnemonic(_("_OK"));
  g_signal_connect(G_OBJECT(pref_ok_button), "clicked",
		   G_CALLBACK(ok_button_clicked), (gpointer) group_name);
  gtk_box_append(GTK_BOX(setting_button_box), pref_ok_button);
#if USE_CHANGES_SENSITIVE_OK_BUTTON
  gtk_widget_set_sensitive(pref_ok_button, FALSE);
#endif
  gtk_widget_set_tooltip_text(pref_ok_button, _("Quit this application with applying changes"));

  return setting_button_box;
}

static GtkWidget *
create_group_widget(const char *group_name)
{
  GtkWidget *scrolled_win;
  GtkWidget *vbox;
  GtkWidget *group_label;
  struct uim_custom_group *group;
  char *label_text;

  scrolled_win = gtk_scrolled_window_new();
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win),
				 GTK_POLICY_NEVER,
				 GTK_POLICY_AUTOMATIC);
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
  gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(scrolled_win), vbox);

  gtk_widget_set_margin_start(vbox, 4);
  gtk_widget_set_margin_end(vbox, 4);
  gtk_widget_set_margin_top(vbox, 4);
  gtk_widget_set_margin_bottom(vbox, 4);

  group = uim_custom_group_get(group_name);

  if (group == NULL)
    return NULL;

  group_label = gtk_label_new("");
  label_text  = g_markup_printf_escaped("<span size=\"xx-large\">%s</span>",
					group->label);
  gtk_label_set_markup(GTK_LABEL(group_label), label_text);
  g_free(label_text);

  gtk_widget_set_margin_top(group_label, 8);
  gtk_widget_set_margin_bottom(group_label, 8);
  gtk_box_append(GTK_BOX(vbox), group_label);

  create_sub_group_widgets(vbox, group_name);

  uim_custom_group_free(group);

  return scrolled_win;
}

static void create_sub_group_widgets(GtkWidget *parent_widget, const char *parent_group)
{
    char **sgrp_syms = uim_custom_group_subgroups(parent_group);
    char **sgrp_sym;

    for (sgrp_sym = sgrp_syms; *sgrp_sym; sgrp_sym++)
    {
        struct uim_custom_group *sgrp =  uim_custom_group_get(*sgrp_sym);
	char **custom_syms, **custom_sym;
	GString *sgrp_str;
	GtkWidget *frame;
	GtkWidget *vbox;

	if (!sgrp)
	  continue;

	/* XXX quick hack to use AND expression of groups */
	sgrp_str = g_string_new("");
	g_string_printf(sgrp_str, "%s '%s", parent_group, *sgrp_sym);
	custom_syms = uim_custom_collect_by_group(sgrp_str->str);
	g_string_free(sgrp_str, TRUE);

	if (!custom_syms)
	  continue;
	if (!*custom_syms) {
	  uim_custom_symbol_list_free(custom_syms);
	  continue;
	}

	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
	gtk_widget_set_margin_start(vbox, 6);
	gtk_widget_set_margin_end(vbox, 6);
	gtk_widget_set_margin_top(vbox, 6);
	gtk_widget_set_margin_bottom(vbox, 6);

	if (strcmp(*sgrp_sym, "main")) {
	  frame = gtk_frame_new(sgrp->label);
	  /* GTK4's gtk_frame_set_label_align() dropped the yalign
	   * parameter (the label is always vertically centered on the
	   * frame's top border), so only xalign is passed here. */
	  gtk_frame_set_label_align(GTK_FRAME(frame), 0.02);
	  gtk_box_append(GTK_BOX(parent_widget), frame);

	  gtk_frame_set_child(GTK_FRAME(frame), vbox);
	} else {

	  /*
	   * Removing frame for 'main' subgroup. If you feel it
	   * strange, Replace it as you favor.  -- YamaKen 2005-02-06
	   */
	  gtk_box_append(GTK_BOX(parent_widget), vbox);
	}

	for (custom_sym = custom_syms; *custom_sym; custom_sym++) {
	  uim_pref_gtk_add_custom(vbox, *custom_sym);
	}
	uim_custom_symbol_list_free(custom_syms);

	uim_custom_group_free(sgrp);
    }

    uim_custom_symbol_list_free(sgrp_syms);
}

static GtkWidget *
create_pref_window(void)
{
  GtkWidget *window;
  GtkWidget *scrolled_win; /* treeview container */
  GtkWidget *vbox;
  GtkEventController *key_controller;

  pref_window = window = gtk_window_new();

  gtk_window_set_icon_name(GTK_WINDOW(pref_window), "uim");

  g_signal_connect(G_OBJECT(window), "close-request",
		   G_CALLBACK(close_request_cb), NULL);

  key_controller = gtk_event_controller_key_new();
  gtk_widget_add_controller(window, key_controller);
  g_signal_connect(key_controller, "key-pressed",
		   G_CALLBACK(key_pressed_cb), NULL);

  pref_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);

  scrolled_win = gtk_scrolled_window_new();
  gtk_scrolled_window_set_has_frame(GTK_SCROLLED_WINDOW(scrolled_win), TRUE);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win),
				 GTK_POLICY_NEVER,
				 GTK_POLICY_AUTOMATIC);
  gtk_box_append(GTK_BOX(pref_hbox), scrolled_win);

  gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(scrolled_win),
				create_pref_treeview());

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
  gtk_widget_set_margin_start(vbox, 8);
  gtk_widget_set_margin_end(vbox, 8);
  gtk_widget_set_margin_top(vbox, 8);
  gtk_widget_set_margin_bottom(vbox, 8);
  gtk_widget_set_vexpand(pref_hbox, TRUE);
  gtk_box_append(GTK_BOX(vbox), pref_hbox);
  gtk_box_append(GTK_BOX(vbox), create_setting_button_box("dummy-group-name"));
  gtk_window_set_child(GTK_WINDOW(window), vbox);

  {
    GdkDisplay *display;
    GListModel *monitors;
    gint w = DEFAULT_WINDOW_WIDTH_MAX, h = DEFAULT_WINDOW_HEIGHT_MAX;

    display = gtk_widget_get_display(window);
    monitors = gdk_display_get_monitors(display);
    if (monitors && g_list_model_get_n_items(monitors) > 0) {
      GdkMonitor *monitor = g_list_model_get_item(monitors, 0);
      GdkRectangle geom;

      gdk_monitor_get_geometry(monitor, &geom);
      w = CLAMP((gint)(geom.width  * 0.95), 0, DEFAULT_WINDOW_WIDTH_MAX);
      h = CLAMP((gint)(geom.height * 0.95), 0, DEFAULT_WINDOW_HEIGHT_MAX);
      g_object_unref(monitor);
    }
    gtk_window_set_default_size(GTK_WINDOW(window), w, h);
    /* GTK 4 dropped GTK_WIN_POS_* / gtk_window_move(): there is no
     * portable way to explicitly center a top-level window any more,
     * so we rely on the window manager/compositor's own placement. */
  }

  return window;
}

static void
dot_uim_warning_alert_cb(GObject *source, GAsyncResult *result, gpointer data)
{
  GError *error = NULL;

  gtk_alert_dialog_choose_finish(GTK_ALERT_DIALOG(source), result, &error);
  if (error)
    g_error_free(error);
}

static gboolean
check_dot_uim_file(gpointer data)
{
  GString *dot_uim;
  GtkAlertDialog *alert;
  const gchar *message =
    N_("The user customize file \"~/.uim\" is found.\n"
       "This file will override all conflicted settings set by\n"
       "this tool (stored in ~/.uim.d/customs/*.scm).\n"
       "Please check the file if you find your settings aren't applied.\n\n"
       "(To suppress this dialog, add following line to ~/.uim)\n"
       "(define uim-pref-suppress-dot-uim-warning-dialog? #t)");
  uim_bool suppress_dialog;

  suppress_dialog = uim_scm_symbol_value_bool("uim-pref-suppress-dot-uim-warning-dialog?");
  if (suppress_dialog) {
    return G_SOURCE_REMOVE;
  }

  dot_uim = g_string_new(g_get_home_dir());
  g_string_append(dot_uim, "/.uim");

  if (!g_file_test(dot_uim->str, G_FILE_TEST_EXISTS)) {
    g_string_free(dot_uim, TRUE);
    return G_SOURCE_REMOVE;
  }
  g_string_free(dot_uim, TRUE);

  alert = gtk_alert_dialog_new("%s", _(message));
  gtk_alert_dialog_choose(alert, pref_window ? GTK_WINDOW(pref_window) : NULL,
			  NULL, dot_uim_warning_alert_cb, NULL);
  g_object_unref(alert);

  return G_SOURCE_REMOVE;
}

static void
activate_cb(GtkApplication *app, gpointer user_data)
{
  if (uim_custom_enable()) {
    GtkWidget *pref;

    im_uim_init_modifier_keys();
    g_idle_add(check_dot_uim_file, NULL);
    pref = create_pref_window();
    gtk_window_set_application(GTK_WINDOW(pref), app);
    gtk_window_present(GTK_WINDOW(pref));
  } else {
    fprintf(stderr, "uim_custom_enable() failed.\n");
    uim_quit();
    exit(EXIT_FAILURE);
  }
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
    return -1;
  }

  app = gtk_application_new("org.uim.Pref", G_APPLICATION_NON_UNIQUE);
  g_signal_connect(app, "activate", G_CALLBACK(activate_cb), NULL);

  status = g_application_run(G_APPLICATION(app), argc, argv);
  g_object_unref(app);

  uim_quit();
  return status;
}
