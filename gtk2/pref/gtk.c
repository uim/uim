/*

  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim

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

#include <glib.h>
#include <gtk/gtk.h>
#if GTK_CHECK_VERSION(2, 90, 0)
# include <gdk/gdkkeysyms-compat.h>
#else
# include <gdk/gdkkeysyms.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <locale.h>

#include "uim/uim.h"
#include "uim/uim-custom.h"
#include "uim/uim-scm.h"
#include "uim/gettext.h"
#include "gtk-custom-widgets.h"
#include "../immodule/key-util-gtk.h"

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

/*
 *  2005-02-10 Takuro Ashie <ashie@homa.ne.jp>
 *    This feature is disabled according to [Anthy-dev 1795].
 */
#if 0
static void
save_confirm_dialog_response_cb(GtkDialog *dialog, gint arg, gpointer user_data)
{
  switch (arg)
  {
  case GTK_RESPONSE_YES:
    uim_custom_save();
    uim_custom_broadcast();
    uim_pref_gtk_value_changed = FALSE;
    break;
  case GTK_RESPONSE_NO:
    uim_pref_gtk_value_changed = FALSE;
    break;
  default:
    break;
  }
}
#endif

static gboolean
pref_tree_selection_changed(GtkTreeSelection *selection,
			     gpointer data)
{
  GtkTreeStore *store;
  GtkTreeIter iter;
  GtkTreeModel *model;
  char *group_name, *group_sym;
  GtkWidget *group_widget;

/*
 *  2005-02-10 Takuro Ashie <ashie@homa.ne.jp>
 *    This feature is disabled according to [Anthy-dev 1795].
 */
#if 0
  /* Preference save check should be here. */
  if (uim_pref_gtk_value_changed) {
    GtkWidget *dialog;
    dialog = gtk_message_dialog_new(NULL,
				    GTK_DIALOG_MODAL,
				    GTK_MESSAGE_QUESTION,
				    GTK_BUTTONS_YES_NO,
				    _("Some value(s) have been changed.\n"
				      "Save?"));
    gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(pref_window));
    gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER_ON_PARENT);
    g_signal_connect(G_OBJECT(dialog), "response",
		     G_CALLBACK(save_confirm_dialog_response_cb), NULL);
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
  }
#endif

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
    gtk_widget_hide(current_group_widget);

  /* whether group_widget is already packed or not */
  if (!gtk_widget_get_parent(group_widget))
    gtk_box_pack_start (GTK_BOX (pref_hbox), group_widget, TRUE, TRUE, 0);

  /* show selected group's widget */
  gtk_widget_show_all(group_widget);

  current_group_widget = group_widget;

  free(group_name);
  free(group_sym);
  return TRUE;
}


static void
quit_confirm_dialog_response_cb(GtkDialog *dialog, gint arg, gpointer user_data)
{
  gboolean *quit = user_data;

  switch (arg)
  {
  case GTK_RESPONSE_YES:
  case GTK_RESPONSE_OK:
    *quit = TRUE;
    break;
  case GTK_RESPONSE_CANCEL:
  case GTK_RESPONSE_NO:
    *quit = FALSE;
    break;
  default:
    break;
  }
}

static void
quit_confirm(void)
{
  if (uim_pref_gtk_value_changed) {
    GtkWidget *dialog;
    gboolean quit = FALSE;

    dialog = gtk_message_dialog_new(NULL,
				    GTK_DIALOG_MODAL,
				    GTK_MESSAGE_QUESTION,
				    GTK_BUTTONS_YES_NO,
				    "%s", _("Some value(s) have been changed.\n"
				      "Do you really quit this program?"));
    gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(pref_window));
    gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER_ON_PARENT);
    g_signal_connect(G_OBJECT(dialog), "response",
		     G_CALLBACK(quit_confirm_dialog_response_cb), &quit);
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);

    if (quit)
      gtk_main_quit();
  } else {
    gtk_main_quit();
  }
}

static gboolean
key_press_cb(GtkWidget *widget, GdkEventKey *event)
{
  if (event->keyval == GDK_Escape) {
    quit_confirm();
    return TRUE;
  }

  return FALSE;
}

static gint
delete_event_cb(GtkWidget *widget, gpointer data)
{
  quit_confirm();
  return TRUE;
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
  gtk_tree_view_set_rules_hint (GTK_TREE_VIEW(pref_tree_view), TRUE);
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (pref_tree_view));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);
  g_signal_connect (G_OBJECT(selection), "changed",
		    G_CALLBACK(pref_tree_selection_changed), NULL);

  first_path = gtk_tree_path_new_from_indices (0, -1);

  gtk_tree_view_set_cursor(GTK_TREE_VIEW(pref_tree_view),
			   first_path, NULL, FALSE);

  return pref_tree_view;
}

static void
ok_button_clicked(GtkButton *button, gpointer user_data)
{
  /*const char *group_name = user_data;*/

  if (uim_pref_gtk_value_changed) {
    uim_custom_save();
    uim_custom_broadcast_reload_request();
    uim_pref_gtk_unmark_value_changed();
  }

  gtk_main_quit();
}

static void
apply_button_clicked(GtkButton *button, gpointer user_data)
{
  /*const char *group_name = user_data;*/

  if (uim_pref_gtk_value_changed) {
    uim_custom_save();
    uim_custom_broadcast_reload_request();
    uim_pref_gtk_unmark_value_changed();
  }
}

static void
set_to_default_cb(GtkWidget *widget, gpointer data)
{
  uim_pref_gtk_set_default_value(widget);

  if (GTK_IS_CONTAINER(widget))
    gtk_container_foreach(GTK_CONTAINER(widget),
			  (GtkCallback) (set_to_default_cb), NULL);
  uim_pref_gtk_mark_value_changed();
}

static void
defaults_button_clicked(GtkButton *button, gpointer user_data)
{
  gtk_container_foreach(GTK_CONTAINER(current_group_widget),
			(GtkCallback) (set_to_default_cb), NULL);
}

static GtkWidget *
create_setting_button_box(const char *group_name)
{
  GtkWidget *setting_button_box;
  GtkWidget *button;

#if GTK_CHECK_VERSION(3, 2, 0)
  setting_button_box = gtk_button_box_new(GTK_ORIENTATION_HORIZONTAL);
#else
  setting_button_box = gtk_hbutton_box_new();
#endif
  gtk_button_box_set_layout(GTK_BUTTON_BOX(setting_button_box), GTK_BUTTONBOX_END);
  gtk_box_set_spacing(GTK_BOX(setting_button_box), 8);

  /* Defaults button */
  button = gtk_button_new_with_mnemonic(_("_Defaults"));
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(defaults_button_clicked), (gpointer) group_name);
  gtk_box_pack_start(GTK_BOX(setting_button_box), button, TRUE, TRUE, 8);
  gtk_widget_set_tooltip_text(button, _("Revert all changes to default"));


  /* Apply button */
  pref_apply_button = gtk_button_new_from_stock(GTK_STOCK_APPLY);
  g_signal_connect(G_OBJECT(pref_apply_button), "clicked",
		   G_CALLBACK(apply_button_clicked), (gpointer) group_name);
  gtk_widget_set_sensitive(pref_apply_button, FALSE);
  gtk_box_pack_start(GTK_BOX(setting_button_box), pref_apply_button, TRUE, TRUE, 8);
  gtk_widget_set_tooltip_text(pref_apply_button, _("Apply all changes"));

  /* Cancel button */
  button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(quit_confirm), NULL);
  gtk_box_pack_start(GTK_BOX(setting_button_box), button, TRUE, TRUE, 8);
  gtk_widget_set_tooltip_text(button, _("Quit this application without applying changes"));

  /* OK button */
  pref_ok_button = gtk_button_new_from_stock(GTK_STOCK_OK);
  g_signal_connect(G_OBJECT(pref_ok_button), "clicked",
		   G_CALLBACK(ok_button_clicked), (gpointer) group_name);
  gtk_box_pack_start(GTK_BOX(setting_button_box), pref_ok_button, TRUE, TRUE, 8);
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

  scrolled_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win),
				 GTK_POLICY_NEVER,
				 GTK_POLICY_AUTOMATIC);
#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
#else
  vbox = gtk_vbox_new(FALSE, 8);
#endif
#if GTK_CHECK_VERSION(3, 8, 0)
  gtk_container_add(GTK_CONTAINER(scrolled_win), vbox);
#else
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_win),
					vbox);
#endif

  gtk_container_set_border_width(GTK_CONTAINER(vbox), 4);

  group = uim_custom_group_get(group_name);

  if (group == NULL)
    return NULL;

  group_label = gtk_label_new("");
  label_text  = g_markup_printf_escaped("<span size=\"xx-large\">%s</span>",
					group->label);
  gtk_label_set_markup(GTK_LABEL(group_label), label_text);
  g_free(label_text);

  gtk_box_pack_start (GTK_BOX(vbox), group_label, FALSE, TRUE, 8);

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

#if GTK_CHECK_VERSION(3, 2, 0)
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
#else
	vbox = gtk_vbox_new(FALSE, 8);
#endif
	if (strcmp(*sgrp_sym, "main")) {
	  frame = gtk_frame_new(sgrp->label);
	  gtk_frame_set_label_align(GTK_FRAME(frame), 0.02, 0.5);
	  gtk_box_pack_start(GTK_BOX(parent_widget), frame, FALSE, FALSE, 0);

	  gtk_container_add(GTK_CONTAINER(frame), vbox);
	} else {

	  /*
	   * Removing frame for 'main' subgroup. If you feel it
	   * strange, Replace it as you favor.  -- YamaKen 2005-02-06
	   */
	  gtk_box_pack_start(GTK_BOX(parent_widget), vbox, FALSE, FALSE, 0);
	}

	gtk_container_set_border_width(GTK_CONTAINER(vbox), 6);

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
  GdkPixbuf *icon;

  pref_window = window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  icon = gdk_pixbuf_new_from_file(UIM_PIXMAPSDIR"/uim-icon.png", NULL);
  gtk_window_set_icon(GTK_WINDOW(pref_window), icon);

  g_signal_connect(G_OBJECT (window), "delete_event",
		   G_CALLBACK (delete_event_cb), NULL);
  g_signal_connect(G_OBJECT (window), "key_press_event",
		   G_CALLBACK (key_press_cb), NULL);


#if GTK_CHECK_VERSION(3, 2, 0)
  pref_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
#else
  pref_hbox = gtk_hbox_new(FALSE, 8);
#endif

  scrolled_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolled_win),
				      GTK_SHADOW_ETCHED_IN);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win),
				 GTK_POLICY_NEVER,
				 GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(pref_hbox), scrolled_win, FALSE, TRUE, 0);

  gtk_container_add(GTK_CONTAINER(scrolled_win), create_pref_treeview());

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
#else
  vbox = gtk_vbox_new(FALSE, 8);
#endif
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 8);
  gtk_box_pack_start(GTK_BOX(vbox), pref_hbox, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), create_setting_button_box("dummy-group-name"), FALSE, TRUE, 0);
  gtk_container_add(GTK_CONTAINER(window), vbox);

  {
    GdkScreen *scr;
    gint w, h;

    scr = gtk_window_get_screen(GTK_WINDOW(window));
    w = CLAMP(gdk_screen_get_width(scr)  * 0.95, 0, DEFAULT_WINDOW_WIDTH_MAX);
    h = CLAMP(gdk_screen_get_height(scr) * 0.95, 0, DEFAULT_WINDOW_HEIGHT_MAX);
    gtk_window_set_default_size(GTK_WINDOW(window), w, h);
    gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER_ALWAYS);
  }

  return window;
}

static gboolean
check_dot_uim_file(void)
{
  GString *dot_uim;
  GtkWidget *dialog;
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
    return FALSE;
  }

  dot_uim = g_string_new(g_get_home_dir());
  g_string_append(dot_uim, "/.uim");

  if (!g_file_test(dot_uim->str, G_FILE_TEST_EXISTS)) {
    g_string_free(dot_uim, TRUE);
    return FALSE;
  }
  g_string_free(dot_uim, TRUE);

  dialog = gtk_message_dialog_new(NULL,
				  GTK_DIALOG_MODAL,
				  GTK_MESSAGE_WARNING,
				  GTK_BUTTONS_OK,
				  "%s", _(message));
  if (pref_window) {
    gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(pref_window));
    gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER_ON_PARENT);
  }

  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(GTK_WIDGET(dialog));

  return FALSE;
}

int
main(int argc, char *argv[])
{
  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8");

  gtk_init(&argc, &argv);

  if (uim_init() < 0) {
    fprintf(stderr, "uim_init() failed.\n");
    return -1;
  }

  if (uim_custom_enable()) {
    GtkWidget *pref;

    im_uim_init_modifier_keys();
    g_idle_add((GSourceFunc) check_dot_uim_file, NULL);
    pref = create_pref_window();
    gtk_widget_show_all(pref);

    gtk_main();
  } else {
    fprintf(stderr, "uim_custom_enable() failed.\n");
    uim_quit();
    return -1;
  }

  uim_quit();
  return 0;
}
