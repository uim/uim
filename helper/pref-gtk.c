/*

  Copyright (c) 2005 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

#include <glib.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <string.h>
#include <stdlib.h>
#include <locale.h>

#include "uim/config.h"
#include <uim/uim.h>
#include <uim/uim-custom.h>
#include "uim/gettext.h"

#define OBJECT_DATA_UIM_CUSTOM    "uim-pref-gtk::uim-custom"
#define OBJECT_DATA_VALUE_CHANGED "uim-pref-gtk::value-changed"

static GtkWidget *pref_window = NULL;
static GtkWidget *pref_tree_view = NULL;
static GtkWidget *pref_hbox = NULL;
static GtkWidget *current_group_widget = NULL;
static GtkSizeGroup *spin_button_sgroup = NULL;
static gboolean value_changed = FALSE;
static struct OListPrefWin {
  GtkWidget *window;
  GtkWidget *tree_view[2];
  GtkWidget *up_button;
  GtkWidget *down_button;
  GtkWidget *left_button;
  GtkWidget *right_button;
} olist_pref_win = {
  NULL, {NULL, NULL},
};
static struct KeyPrefWin {
  GtkWidget *window;
  GtkWidget *tree_view;
  GtkWidget *add_button;
  GtkWidget *remove_button;
  GtkWidget *shift_toggle;
  GtkWidget *control_toggle;
  GtkWidget *alt_toggle;
  GtkWidget *keycode_entry;

  guint           grabbed_key_val;
  GdkModifierType grabbed_key_state;
} key_pref_win = {
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, 0,
};

enum
{
  GROUP_COLUMN=0,
  GROUP_WIDGET=1,
  NUM_COLUMNS
};

static gboolean	pref_tree_selection_changed(GtkTreeSelection *selection,
					     gpointer data);
static GtkWidget *create_pref_treeview(void);
static GtkWidget *create_group_widget(const char *group_name);

static void
save_confirm_dialog_response_cb(GtkDialog *dialog, gint arg, gpointer user_data)
{
  switch (arg)
  {
  case GTK_RESPONSE_YES:
    uim_custom_save();
    uim_custom_broadcast();
    g_object_set_data(G_OBJECT(current_group_widget),
		      OBJECT_DATA_VALUE_CHANGED,
		      GINT_TO_POINTER(FALSE));
    value_changed = FALSE;
    break;
  case GTK_RESPONSE_NO:
    break;
  default:
    break;
  }
}

static gboolean
pref_tree_selection_changed(GtkTreeSelection *selection,
			     gpointer data)
{
  GtkTreeStore *store;
  GtkTreeIter iter;
  GtkTreeModel *model;
  char *group_name;
  GtkWidget *group_widget;
  gboolean changed = FALSE;

  /* Preference save check should be here. */
  if (current_group_widget)
    changed = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(current_group_widget),
						OBJECT_DATA_VALUE_CHANGED));
  if (changed) {
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

  if(gtk_tree_selection_get_selected(selection, &model, &iter) == FALSE)
    return TRUE;

  store = GTK_TREE_STORE(model);
  gtk_tree_model_get(model, &iter,
		     GROUP_COLUMN, &group_name,
		     GROUP_WIDGET, &group_widget,
		     -1);

  if(group_name == NULL)
    return TRUE;

  /* hide current selected group's widget */
  if(current_group_widget)
    gtk_widget_hide(current_group_widget);

  /* whether group_widget is already packed or not */
  if(!gtk_widget_get_parent(group_widget))
    gtk_box_pack_start (GTK_BOX (pref_hbox), group_widget, TRUE, TRUE, 0);

  /* show selected group's widget */
  gtk_widget_show_all(group_widget);

  current_group_widget = group_widget;
  
  free(group_name);
  return TRUE;
}


static void
quit_confirm_dialog_response_cb(GtkDialog *dialog, gint arg, gpointer user_data)
{
  gboolean *quit = user_data;

  switch (arg)
  {
  case GTK_RESPONSE_OK:
    *quit = TRUE;
    break;
  case GTK_RESPONSE_CANCEL:
    *quit = FALSE;
    break;
  default:
    break;
  }
}

static void
quit_confirm(void)
{
  if (value_changed) {
    GtkWidget *dialog;
    gboolean quit = FALSE;

    dialog = gtk_message_dialog_new(NULL,
				    GTK_DIALOG_MODAL,
				    GTK_MESSAGE_QUESTION,
				    GTK_BUTTONS_OK_CANCEL,
				    _("Some value(s) have been changed.\n"
				      "Do you realy quit this program?"));
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

static void
delete_event_cb(GtkWidget *widget, gpointer data)
{
  quit_confirm();
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
				   GTK_TYPE_WIDGET);
  
  pref_tree_view = gtk_tree_view_new();

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Group"),
						    renderer,
						    "text", GROUP_COLUMN,
						    NULL);
  gtk_tree_view_column_set_sort_column_id(column, 0);
  gtk_tree_view_append_column(GTK_TREE_VIEW(pref_tree_view), column);
  
  primary_groups = uim_custom_primary_groups();
  for (grp = primary_groups; *grp; grp++) {
    struct uim_custom_group *group = uim_custom_group_get(*grp);
    gtk_tree_store_append (tree_store, &iter, NULL/* parent iter */);
    gtk_tree_store_set (tree_store, &iter,
			GROUP_COLUMN, group->label,
			GROUP_WIDGET, create_group_widget(*grp),
			-1);
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
custom_check_button_toggled_cb(GtkToggleButton *button, gpointer user_data)
{
  struct uim_custom *custom;
  uim_bool rv;
  gboolean active;

  active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button));

  custom = g_object_get_data(G_OBJECT(button), OBJECT_DATA_UIM_CUSTOM);
  g_return_if_fail(custom);

  if (custom->type == UCustom_Bool) {
    custom->value->as_bool = active;
    rv = uim_custom_set(custom);

    if (rv) {
      value_changed = TRUE;
      g_object_set_data(G_OBJECT(current_group_widget),
			OBJECT_DATA_VALUE_CHANGED,
			GINT_TO_POINTER(TRUE));
    } else {
      g_printerr("Faild to set bool value for \"%s\".\n", custom->symbol);
      /* FIXME! reset the widget */
    }
  } else {
    g_printerr("Invalid value type passed for \"%s\".\n", custom->symbol);
  }
}


static void
update_custom_type_bool_cb(void *ptr, const char *custom_sym)
{
  struct uim_custom *custom = uim_custom_get(custom_sym);
  GtkWidget *check_button = GTK_WIDGET(ptr);
  
  if (!custom || custom->type != UCustom_Bool)
    return;

  g_object_set_data_full(G_OBJECT(check_button),
			 OBJECT_DATA_UIM_CUSTOM, custom,
			 (GDestroyNotify) uim_custom_free);

  gtk_widget_set_sensitive(check_button, custom->is_active);

  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check_button),
			       custom->value->as_bool);
}

static void
add_custom_type_bool(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *check_button;
  hbox = gtk_hbox_new(FALSE, 8);

  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  check_button = gtk_check_button_new();
  g_object_set_data_full(G_OBJECT(check_button),
			 OBJECT_DATA_UIM_CUSTOM, custom,
			 (GDestroyNotify) uim_custom_free);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check_button),
			       custom->value->as_bool);
  g_signal_connect(G_OBJECT(check_button), "toggled",
		   G_CALLBACK(custom_check_button_toggled_cb), NULL);

  uim_custom_cb_add(custom->symbol, check_button, update_custom_type_bool_cb);

  gtk_box_pack_start (GTK_BOX (hbox), check_button, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
}

static void
custom_adjustment_value_changed(GtkAdjustment *adj, gpointer user_data)
{
  struct uim_custom *custom;
  uim_bool rv;

  custom = g_object_get_data(G_OBJECT(adj), OBJECT_DATA_UIM_CUSTOM);
  g_return_if_fail(custom);

  if (custom->type == UCustom_Int) {
    custom->value->as_int = adj->value;
    rv = uim_custom_set(custom);

    if (rv) {
      value_changed = TRUE;
      g_object_set_data(G_OBJECT(current_group_widget),
			OBJECT_DATA_VALUE_CHANGED,
			GINT_TO_POINTER(FALSE));
    } else {
      g_printerr("Faild to set int value for \"%s\".\n", custom->symbol);
      /* FIXME! reset the widget */
    }
  } else {
    g_printerr("Invalid value type passed for \"%s\".\n", custom->symbol);
  }
}

static void
add_custom_type_integer(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkAdjustment *adjustment;
  GtkWidget *spin;
  hbox = gtk_hbox_new(FALSE, 8);

  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  adjustment = (GtkAdjustment*)gtk_adjustment_new(custom->value->as_int, /* initial size */
						  custom->range->as_int.min, /* minimum */
						  custom->range->as_int.max, /* maximum */
						  1.0,
						  10.0,
						  100.0);
  g_object_set_data_full(G_OBJECT(adjustment),
			 OBJECT_DATA_UIM_CUSTOM, custom,
			 (GDestroyNotify) uim_custom_free);

  spin = gtk_spin_button_new(adjustment, 1.0, 0);
  gtk_size_group_add_widget(spin_button_sgroup, spin);
  gtk_box_pack_end (GTK_BOX (hbox), spin, FALSE, TRUE, 0);
  
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);

  g_signal_connect(G_OBJECT(adjustment), "value-changed",
		   G_CALLBACK(custom_adjustment_value_changed), NULL);
}

static void
custom_entry_changed_cb(GtkEntry *entry, gpointer user_data)
{
  struct uim_custom *custom;
  uim_bool rv;

  custom = g_object_get_data(G_OBJECT(entry), OBJECT_DATA_UIM_CUSTOM);
  g_return_if_fail(custom);

  if (custom->type == UCustom_Str || custom->type == UCustom_Pathname) {
    const char *str = gtk_entry_get_text(GTK_ENTRY(entry));

    if (custom->type == UCustom_Str) {
      free(custom->value->as_str);
      custom->value->as_str = strdup(str);
      rv = uim_custom_set(custom);
    } else if (custom->type == UCustom_Pathname) {
      free(custom->value->as_pathname);
      custom->value->as_pathname = strdup(str);
      rv = uim_custom_set(custom);
    } else {
      rv = UIM_FALSE;
    }

    if (rv) {
      value_changed = TRUE;
      g_object_set_data(G_OBJECT(current_group_widget),
			OBJECT_DATA_VALUE_CHANGED,
			GINT_TO_POINTER(FALSE));
    } else {
      g_printerr("Faild to set str value for \"%s\".\n", custom->symbol);
      /* FIXME! reset the widget */
    }
  } else {
    g_printerr("Invalid value type passed for \"%s\".\n", custom->symbol);
  }
}


static void
update_custom_type_string_cb(void *ptr, const char *custom_sym)
{
  struct uim_custom *custom = uim_custom_get(custom_sym);
  GtkWidget *entry = GTK_WIDGET(ptr);
  
  if (!custom || custom->type != UCustom_Str)
    return;
  
  gtk_widget_set_sensitive(entry, custom->is_active);
  gtk_entry_set_text(GTK_ENTRY(entry), custom->value->as_str);

  uim_custom_free(custom);
}

static void
add_custom_type_string(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *entry;

  hbox = gtk_hbox_new(FALSE, 8);

  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  entry = gtk_entry_new();
  g_object_set_data_full(G_OBJECT(entry),
			 OBJECT_DATA_UIM_CUSTOM, custom,
			 (GDestroyNotify) uim_custom_free);
  gtk_entry_set_text(GTK_ENTRY(entry), custom->value->as_str);
  gtk_box_pack_start (GTK_BOX (hbox), entry, TRUE, TRUE, 0);
  g_signal_connect(G_OBJECT(entry), "changed",
		   G_CALLBACK(custom_entry_changed_cb), NULL);

  gtk_widget_set_sensitive(entry, custom->is_active);

  uim_custom_cb_add(custom->symbol, entry, update_custom_type_string_cb);

  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
}

static void
custom_pathname_button_clicked_cb(GtkWidget *button, GtkWidget *entry)
{
  GtkWidget *dialog;
  dialog = gtk_file_chooser_dialog_new (_("Specify file"),
					NULL,
					GTK_FILE_CHOOSER_ACTION_OPEN,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
					NULL);
  
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {
    char *filename;    
    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    if(filename) {
      gtk_entry_set_text(GTK_ENTRY(entry), filename);
      g_free (filename);
    }
  }

  gtk_widget_destroy (dialog);
}

static void
add_custom_type_pathname(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *entry;
  GtkWidget *button;

  hbox = gtk_hbox_new(FALSE, 8);
 
  label = gtk_label_new(custom->label);
  gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);

  entry = gtk_entry_new();
  g_object_set_data_full(G_OBJECT(entry),
			 OBJECT_DATA_UIM_CUSTOM, custom,
			 (GDestroyNotify) uim_custom_free);
  gtk_entry_set_text(GTK_ENTRY(entry), custom->value->as_pathname);
  gtk_box_pack_start (GTK_BOX (hbox), entry, TRUE, TRUE, 0);
  g_signal_connect(G_OBJECT(entry), "changed",
		   G_CALLBACK(custom_entry_changed_cb), NULL);

  button = gtk_button_new_with_label(_("File"));

  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(custom_pathname_button_clicked_cb), entry);

  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
}

static void
custom_combo_box_changed(GtkComboBox *combo_box, gpointer user_data)
{
  struct uim_custom *custom;
  uim_bool rv;

  custom = g_object_get_data(G_OBJECT(combo_box), OBJECT_DATA_UIM_CUSTOM);
  g_return_if_fail(custom);

  if (custom->type != UCustom_Choice) {
    gint i, num = gtk_combo_box_get_active(combo_box);
    struct uim_custom_choice *choice = NULL;

    /* check length of custom->range->as_choice.valid_items */
    for (i = 0;
	 custom->range->as_choice.valid_items &&
	   custom->range->as_choice.valid_items[i];
	 i++)
    {
      if (i == num) {
	choice = custom->range->as_choice.valid_items[i];
      }
    }
    g_return_if_fail(choice);

    free(custom->value->as_choice->symbol);
    free(custom->value->as_choice->label);
    free(custom->value->as_choice->desc);

    custom->value->as_choice->symbol = choice->symbol ? strdup(choice->symbol): NULL;
    custom->value->as_choice->label  = choice->label  ? strdup(choice->label) : NULL;
    custom->value->as_choice->desc   = choice->desc   ? strdup(choice->desc)  : NULL;

    rv = uim_custom_set(custom);

    if (rv) {
      value_changed = TRUE;
      g_object_set_data(G_OBJECT(current_group_widget),
			OBJECT_DATA_VALUE_CHANGED,
			GINT_TO_POINTER(FALSE));
    } else {
      g_printerr("Faild to set str value for \"%s\".\n", custom->symbol);
      /* FIXME! reset the widget */
    }
  } else {
  }
}

static void
update_custom_type_choice_cb(void *ptr, const char *custom_sym)
{
  struct uim_custom *custom = uim_custom_get(custom_sym);
  GtkWidget *combobox = GTK_WIDGET(ptr);
  struct uim_custom_choice **item;
  gint i = 0, default_index = 0;
  gchar *default_symbol;

  if (!custom || custom->type != UCustom_Choice)
    return;
  
  g_object_set_data_full(G_OBJECT(combobox),
			 OBJECT_DATA_UIM_CUSTOM, custom,
			 (GDestroyNotify) uim_custom_free);
  gtk_widget_set_sensitive(combobox, custom->is_active);

  item = custom->range->as_choice.valid_items;
  if(item == NULL || *item == NULL)
    return;
  
  g_signal_handlers_disconnect_by_func(G_OBJECT(combobox), (gpointer)custom_combo_box_changed, NULL);

  gtk_list_store_clear(GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(combobox))));
  
  default_symbol = custom->value->as_choice->symbol;

  while(*item) {
    gtk_combo_box_append_text(GTK_COMBO_BOX(combobox), (*item)->label);
    if(!strcmp(default_symbol, (*item)->symbol))
      default_index = i;    
    i++;
    item++;
  }
  gtk_combo_box_set_active(GTK_COMBO_BOX(combobox), default_index);
  g_signal_connect(G_OBJECT(combobox), "changed",
		   G_CALLBACK(custom_combo_box_changed), NULL);
}

static void
add_custom_type_choice(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *combobox;
  struct uim_custom_choice **item;
  gint i, default_index = 0;
  gchar *default_symbol;

  hbox = gtk_hbox_new(FALSE, 8);
  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  combobox = gtk_combo_box_new_text();
  g_object_set_data_full(G_OBJECT(combobox),
			 OBJECT_DATA_UIM_CUSTOM, custom,
			 (GDestroyNotify) uim_custom_free);
  
  default_symbol = custom->value->as_choice->symbol;

  for(i = 0, item = custom->range->as_choice.valid_items; *item; i++, item++) {
    gtk_combo_box_append_text(GTK_COMBO_BOX(combobox),
			      (*item)->label);
    if(!strcmp(default_symbol, (*item)->symbol))
      default_index = i;
  }
  gtk_combo_box_set_active(GTK_COMBO_BOX(combobox), default_index);
  gtk_box_pack_start (GTK_BOX (hbox), combobox, FALSE, TRUE, 0);
  g_signal_connect(G_OBJECT(combobox), "changed",
		   G_CALLBACK(custom_combo_box_changed), NULL);

  gtk_widget_set_sensitive(combobox, custom->is_active);
  
  uim_custom_cb_add(custom->symbol, combobox, update_custom_type_choice_cb);

  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
}

static void
olist_pref_dialog_response_cb(GtkDialog *dialog, gint action, GtkEntry *key_entry)
{
  gtk_widget_destroy(GTK_WIDGET(dialog));
}

static void
olist_pref_entry_set_value(GtkEntry *entry)
{
  struct uim_custom *custom;
  struct uim_custom_choice *item;
  GString *str;
  gint i;

  custom = g_object_get_data(G_OBJECT(entry), OBJECT_DATA_UIM_CUSTOM);

  str = g_string_new("");

  if (custom->value->as_olist) {
    for (item = custom->value->as_olist[0], i = 0;
	 item;
	 item = custom->value->as_olist[++i])
    {
      if (i != 0)
	g_string_append(str, ",");
      g_string_append(str, item->label);
    }
  } else {
    /* error message */
  }

  gtk_entry_set_text(GTK_ENTRY(entry), str->str);

  g_string_free(str, TRUE);
}

static void
olist_pref_tree_view_set_value(GtkEntry *olist_entry,
			       gboolean left_view,
			       gboolean right_view)
{
  struct uim_custom *custom;
  struct uim_custom_choice *item;
  GtkTreeView *view;
  GtkListStore *store;
  GtkTreeIter iter;
  gint i;

  custom = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM);

  /* left tree view */
  if (left_view) {
    view = GTK_TREE_VIEW(olist_pref_win.tree_view[0]);
    store = GTK_LIST_STORE(gtk_tree_view_get_model(view));
    gtk_list_store_clear(store);

    for (item = custom->value->as_olist[0], i = 0;
	 item;
	 item = custom->value->as_olist[++i])
    {
      gtk_list_store_append(store, &iter);
      gtk_list_store_set(store, &iter,
			 0, item->label,
			 1, item,
			 -1);
    }
  }

  /* right tree view */
  if (right_view) {
    view = GTK_TREE_VIEW(olist_pref_win.tree_view[1]);
    store = GTK_LIST_STORE(gtk_tree_view_get_model(view));
    gtk_list_store_clear(store);

    for (item = custom->range->as_olist.valid_items[0], i = 0;
	 item;
	 item = custom->range->as_olist.valid_items[++i])
    {
      struct uim_custom_choice *item2;
      gint j;
      gboolean enabled = FALSE;

      for (item2 = custom->value->as_olist[0], j = 0;
	   item2;
	   item2 = custom->value->as_olist[++j])
      {
	if (!strcmp(item->symbol, item2->symbol)) {
	  enabled = TRUE;
	  break;
	}
      }

      if (!enabled) {
	gtk_list_store_append(store, &iter);
	gtk_list_store_set(store, &iter,
			   0, item->label,
			   1, item,
			   -1);
      }
    }
  }
}

static void
set_olist_buttons_sensitive(GtkEntry *olist_entry)
{
  struct uim_custom *custom;
  GtkWidget *view;
  GtkTreeSelection *selection;
  gint num;
  gboolean is_selected1 = FALSE, is_selected2 = FALSE;
  gboolean is_multiple = FALSE, is_first = FALSE, is_end = FALSE;

  custom = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM);
  view = olist_pref_win.tree_view[0];
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  num = gtk_tree_selection_count_selected_rows(selection);
  if (num > 0) {
    is_selected1 = TRUE;

    if (num > 1) {
      is_multiple = TRUE;
    } else {
      GtkTreeModel *model;
      GList *rows = gtk_tree_selection_get_selected_rows(selection, &model);
      GtkTreePath *path = rows->data;
      GtkTreeIter iter;

      if (path && !gtk_tree_path_prev(path))
	  is_first = TRUE;

      if (gtk_tree_model_get_iter(model, &iter, path)) {
	if (!gtk_tree_model_iter_next(model, &iter)) {
	  is_end = TRUE;
	}
      }

      g_list_foreach (rows, (GFunc)gtk_tree_path_free, NULL);
      g_list_free (rows);
    }
  }

  view = olist_pref_win.tree_view[1];
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  num = gtk_tree_selection_count_selected_rows(selection);
  if (num > 0)
    is_selected2 = TRUE;

  gtk_widget_set_sensitive(olist_pref_win.up_button,
			   is_selected1 && !is_first && !is_multiple);
  gtk_widget_set_sensitive(olist_pref_win.down_button,
			   is_selected1 && !is_end && !is_multiple);
  gtk_widget_set_sensitive(olist_pref_win.right_button, is_selected1);
  gtk_widget_set_sensitive(olist_pref_win.left_button,  is_selected2);
}

static void
olist_pref_up_button_clicked_cb(GtkWidget *widget, GtkEntry *olist_entry)
{
  struct uim_custom *custom;
  struct uim_custom_choice *choice;
  GtkWidget *view;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter1, iter2;
  GtkTreePath *path;
  GList *rows = NULL;
  gint num;
  gboolean rv;
  uim_bool urv;
  gint *indices, idx;

  custom = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM);
  view = olist_pref_win.tree_view[0];

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  num = gtk_tree_selection_count_selected_rows(selection);
  if (num < 1)
    return;
  if (num > 1)
    return;

  rows = gtk_tree_selection_get_selected_rows(selection, &model);
  path = rows->data;

  rv = gtk_tree_model_get_iter(model, &iter1, path);
  if (!rv)
    goto ERROR;

  indices = gtk_tree_path_get_indices(path);
  if (!indices)
    goto ERROR;
  idx = *indices;

  rv = gtk_tree_path_prev(path);
  if (!rv)
    goto ERROR;

  /* real move */
  choice = custom->value->as_olist[idx];
  custom->value->as_olist[idx] = custom->value->as_olist[idx - 1];
  custom->value->as_olist[idx - 1] = choice;
  urv = uim_custom_set(custom);
  if (urv == UIM_FALSE)
    return;

  value_changed = TRUE;

  /* sync the view */
  rv = gtk_tree_model_get_iter(model, &iter2, path);
  if (!rv)
    goto ERROR;

  gtk_list_store_swap(GTK_LIST_STORE(model), &iter1, &iter2);
  path = gtk_tree_model_get_path(model, &iter1);
  gtk_tree_view_scroll_to_cell(GTK_TREE_VIEW(view), path, NULL,
			       FALSE, 0.0, 0.0);
  gtk_tree_path_free(path);
  set_olist_buttons_sensitive(olist_entry);
  olist_pref_entry_set_value(GTK_ENTRY(olist_entry));

ERROR:
  g_list_foreach(rows, (GFunc)gtk_tree_path_free, NULL);
  g_list_free(rows);
}

static void
olist_pref_down_button_clicked_cb(GtkWidget *widget, GtkEntry *olist_entry)
{
  struct uim_custom *custom;
  struct uim_custom_choice *choice;
  GtkWidget *view;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter1, iter2;
  GtkTreePath *path;
  GList *rows = NULL;
  gint *indices, idx, num;
  gboolean rv;
  uim_bool urv;

  custom = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM);
  view = olist_pref_win.tree_view[0];

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  num = gtk_tree_selection_count_selected_rows(selection);
  if (num < 1)
    return;
  if (num > 1)
    return;

  rows = gtk_tree_selection_get_selected_rows(selection, &model);
  path = rows->data;
  if (!path)
    goto ERROR;
  indices = gtk_tree_path_get_indices(path);
  if (!indices)
    goto ERROR;
  idx = *indices;

  rv = gtk_tree_model_get_iter(model, &iter1, path);
  if (!rv)
    goto ERROR;

  iter2 = iter1;

  rv = gtk_tree_model_iter_next(model, &iter2);
  if (!rv)
    goto ERROR;

  /* real move */
  choice = custom->value->as_olist[idx];
  custom->value->as_olist[idx] = custom->value->as_olist[idx + 1];
  custom->value->as_olist[idx + 1] = choice;
  urv = uim_custom_set(custom);
  if (urv == UIM_FALSE)
    goto ERROR;

  value_changed = TRUE;

  /* sync the view */
  gtk_list_store_swap(GTK_LIST_STORE(model), &iter1, &iter2);
  path = gtk_tree_model_get_path(model, &iter1);
  gtk_tree_view_scroll_to_cell(GTK_TREE_VIEW(view), path, NULL,
			       FALSE, 0.0, 0.0);
  gtk_tree_path_free(path);
  set_olist_buttons_sensitive(olist_entry);
  olist_pref_entry_set_value(GTK_ENTRY(olist_entry));

ERROR:
  g_list_foreach(rows, (GFunc)gtk_tree_path_free, NULL);
  g_list_free(rows);
}

static void
olist_pref_left_button_clicked_cb(GtkWidget *widget, GtkEntry *olist_entry)
{
  struct uim_custom *custom;
  struct uim_custom_choice *choice = NULL;
  GtkWidget *view;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GtkTreePath *path;
  gint num;
  GList *rows = NULL, *node;
  uim_bool urv;

  custom = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM);
  for (num = 0; custom->value->as_olist[num]; num++);

  view = olist_pref_win.tree_view[1];

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  rows = gtk_tree_selection_get_selected_rows(selection, &model);
  if (!rows)
    return;

  for (node = rows; node; node = g_list_next(node)) {
    path = node->data;

    gtk_tree_model_get_iter(model, &iter, path);
    gtk_tree_model_get(model, &iter,
		       1, &choice,
		       -1);

    num++;
    custom->value->as_olist = realloc(custom->value->as_olist,
				      sizeof(struct uim_custom_choice *) * (num + 1));
    custom->value->as_olist[num - 1] = malloc(sizeof(struct uim_custom_choice));
    custom->value->as_olist[num - 1]->symbol = choice->symbol ? strdup(choice->symbol) : NULL;
    custom->value->as_olist[num - 1]->label  = choice->label  ? strdup(choice->label)  : NULL;
    custom->value->as_olist[num - 1]->desc   = choice->desc   ? strdup(choice->desc)   : NULL;
    custom->value->as_olist[num] = NULL;
  }
  
  urv = uim_custom_set(custom);

  if (urv != UIM_FALSE) {
    olist_pref_entry_set_value(GTK_ENTRY(olist_entry));
    olist_pref_tree_view_set_value(GTK_ENTRY(olist_entry), TRUE, TRUE);
    value_changed = TRUE;
    /* FIXME! reset the selection */
  } else {
    /* error message */
  }

  g_list_foreach(rows, (GFunc)gtk_tree_path_free, NULL);
  g_list_free(rows);
}

static void
olist_pref_right_button_clicked_cb(GtkWidget *widget, GtkEntry *olist_entry)
{
  struct uim_custom *custom;
  struct uim_custom_choice *choice;
  GtkWidget *view;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreePath *path;
  gint *indices, idx, num;
  GList *rows = NULL, *node;
  uim_bool urv;

  custom = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM);
  view = olist_pref_win.tree_view[0];

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  rows = gtk_tree_selection_get_selected_rows(selection, &model);
  if (!rows)
    return;

  for (num = 0; custom->value->as_olist[num]; num++);

  for (node = g_list_last(rows); node; node = g_list_previous(node)) {
    path = node->data;
    indices = gtk_tree_path_get_indices(path);
    if (!indices)
      continue;
    idx = *indices;

    choice = custom->value->as_olist[idx];
    free(choice->symbol);
    free(choice->label);
    free(choice->desc);
    free(choice);

    memmove(custom->value->as_olist + idx,
	    custom->value->as_olist + idx + 1,
	    sizeof(struct uim_custom_choice *) * (num - idx));
    custom->value->as_olist = realloc(custom->value->as_olist,
				      sizeof(struct uim_custom_choice *) * num);
    num--;
  }

  urv = uim_custom_set(custom);

  if (urv != UIM_FALSE) {
    olist_pref_entry_set_value(GTK_ENTRY(olist_entry));
    olist_pref_tree_view_set_value(GTK_ENTRY(olist_entry), TRUE, TRUE);
    value_changed = TRUE;
    /* FIXME! reset the selection */
  } else {
    /* error message */
  }

  g_list_foreach(rows, (GFunc)gtk_tree_path_free, NULL);
  g_list_free(rows);
}

static gboolean
olist_pref_selection_changed(GtkTreeSelection *selection,
			     GtkEntry *olist_entry)
{
  set_olist_buttons_sensitive(olist_entry);

  return TRUE;
}

static void
choose_olist_clicked_cb(GtkWidget *widget, GtkEntry *olist_entry)
{
  GtkWidget *dialog, *hbox, *vbox, *scrwin, *table;
  GtkWidget *tree_view, *button, *arrow, *label;
  GtkListStore *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;

  dialog = gtk_dialog_new_with_buttons("ordered list", GTK_WINDOW(pref_window),
				       GTK_DIALOG_MODAL,
				       GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
				       NULL);
  olist_pref_win.window = dialog;
  gtk_window_set_default_size(GTK_WINDOW(dialog), 400, 250);
  g_signal_connect(G_OBJECT(dialog), "response",
		   G_CALLBACK(olist_pref_dialog_response_cb), olist_entry);

  hbox = gtk_hbox_new(FALSE, 0);
  gtk_container_set_border_width(GTK_CONTAINER(hbox), 4);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), hbox,
		     TRUE, TRUE, 0);
  gtk_widget_show(hbox);

  /* left tree view */
  vbox = gtk_vbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), vbox,
		     TRUE, TRUE, 0);
  gtk_widget_show(vbox);

  label = gtk_label_new(_("Enabled"));
  gtk_box_pack_start(GTK_BOX(vbox), label,
		     FALSE, FALSE, 4);
  gtk_widget_show(label);

  scrwin = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrwin),
				 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrwin),
				      GTK_SHADOW_IN);
  gtk_box_pack_start(GTK_BOX(vbox), scrwin,
		     TRUE, TRUE, 0);
  gtk_widget_show(scrwin);

  store = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_POINTER);
  tree_view = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
  olist_pref_win.tree_view[0] = tree_view;
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(tree_view), FALSE);
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
  gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
  g_signal_connect (G_OBJECT(selection), "changed",
		    G_CALLBACK(olist_pref_selection_changed), olist_entry);
  gtk_container_add(GTK_CONTAINER(scrwin), tree_view);
  gtk_widget_show(tree_view);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Enabled items"),
						    renderer,
						    "text", 0,
						    NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);

  g_object_unref(store);

  /* button area */
  vbox = gtk_vbox_new(TRUE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), vbox,
		     FALSE, FALSE, 4);
  gtk_widget_show(vbox);

  table = gtk_table_new(3, 0, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), 3);
  gtk_table_set_col_spacings(GTK_TABLE(table), 3);
  gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);
  gtk_widget_show(table);

  /* up button */
  button = gtk_button_new();
  olist_pref_win.up_button = button;
  gtk_table_attach_defaults(GTK_TABLE(table), button,
			    0, 1,
			    0, 1);
  gtk_widget_show(button);
  arrow = gtk_arrow_new(GTK_ARROW_UP, GTK_SHADOW_NONE);
  gtk_container_add(GTK_CONTAINER(button), arrow);
  gtk_widget_show(arrow);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(olist_pref_up_button_clicked_cb), olist_entry);

  /* down button */
  button = gtk_button_new();
  olist_pref_win.down_button = button;
  gtk_table_attach_defaults(GTK_TABLE(table), button,
			    0, 1,
			    2, 3);
  gtk_widget_show(button);
  arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_NONE);
  gtk_container_add(GTK_CONTAINER(button), arrow);
  gtk_widget_show(arrow);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(olist_pref_down_button_clicked_cb), olist_entry);

  /* left button */
  button = gtk_button_new();
  olist_pref_win.left_button = button;
  gtk_table_attach_defaults(GTK_TABLE(table), button,
			    0, 1,
			    1, 2);
  gtk_widget_show(button);
  arrow = gtk_arrow_new(GTK_ARROW_LEFT, GTK_SHADOW_NONE);
  gtk_container_add(GTK_CONTAINER(button), arrow);
  gtk_widget_show(arrow);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(olist_pref_left_button_clicked_cb), olist_entry);

  /* right button */
  button = gtk_button_new();
  olist_pref_win.right_button = button;
  gtk_table_attach_defaults(GTK_TABLE(table), button,
			    2, 3,
			    1, 2);
  gtk_widget_show(button);
  arrow = gtk_arrow_new(GTK_ARROW_RIGHT, GTK_SHADOW_NONE);
  gtk_container_add(GTK_CONTAINER(button), arrow);
  gtk_widget_show(arrow);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(olist_pref_right_button_clicked_cb), olist_entry);

  /* right tree view */
  vbox = gtk_vbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), vbox,
		     TRUE, TRUE, 0);
  gtk_widget_show(vbox);

  label = gtk_label_new(_("Disabled"));
  gtk_box_pack_start(GTK_BOX(vbox), label,
		     FALSE, FALSE, 4);
  gtk_widget_show(label);

  scrwin = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrwin),
				 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrwin),
				      GTK_SHADOW_IN);
  gtk_box_pack_start(GTK_BOX(vbox), scrwin,
		     TRUE, TRUE, 0);
  gtk_widget_show(scrwin);

  store = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_POINTER);
  tree_view = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
  olist_pref_win.tree_view[1] = tree_view;
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(tree_view), FALSE);
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
  gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
  g_signal_connect (G_OBJECT(selection), "changed",
		    G_CALLBACK(olist_pref_selection_changed), olist_entry);
  gtk_container_add(GTK_CONTAINER(scrwin), tree_view);
  gtk_widget_show(tree_view);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Disabled items"),
						    renderer,
						    "text", 0,
						    NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);

  g_object_unref(store);

  /* set value */
  olist_pref_tree_view_set_value(olist_entry, TRUE, TRUE);
  set_olist_buttons_sensitive(olist_entry);

  /* show dialog */
  gtk_widget_show(dialog);
}

static void
add_custom_type_orderedlist(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label, *entry, *button;

  hbox = gtk_hbox_new(FALSE, 8);
  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  entry = gtk_entry_new();
  gtk_entry_set_editable(GTK_ENTRY(entry), FALSE);
  g_object_set_data_full(G_OBJECT(entry),
			 OBJECT_DATA_UIM_CUSTOM, custom,
			 (GDestroyNotify) uim_custom_free);
  olist_pref_entry_set_value(GTK_ENTRY(entry));
  gtk_box_pack_start (GTK_BOX (hbox), entry, TRUE, TRUE, 0);

  button = gtk_button_new_with_label(_("Choose..."));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(choose_olist_clicked_cb), entry);

  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
}

static gboolean
key_pref_selection_changed(GtkTreeSelection *selection,
			   gpointer data)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  gboolean selected;

  selected = gtk_tree_selection_get_selected(selection, &model, &iter);

  gtk_widget_set_sensitive(key_pref_win.remove_button, selected);

  return TRUE;
}

static void
key_pref_set_value(guint keyval, GdkModifierType mod)
{
  gchar keystr[256] = {0};
  gint len = sizeof(keystr) / sizeof(gchar);

  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(key_pref_win.shift_toggle),
			       mod & GDK_SHIFT_MASK);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(key_pref_win.control_toggle),
			       mod & GDK_CONTROL_MASK);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(key_pref_win.alt_toggle),
			       mod & GDK_MOD1_MASK);

  switch (keyval) {
  case GDK_space:
    /*
     * "space" is not proper uim keysym and only exists for user
     * convenience. It is converted to " " by uim-custom
     */
    g_snprintf(keystr, len, "space");
    break;
  case GDK_BackSpace:
    g_snprintf(keystr, len, "backspace");
    break;
  case GDK_Delete:
    g_snprintf(keystr, len, "delete");
    break;
  case GDK_Escape:
    g_snprintf(keystr, len, "escape");
    break;
  case GDK_Tab:
    g_snprintf(keystr, len, "tab");
    break;
  case GDK_Return:
    g_snprintf(keystr, len, "return");
    break;
  case GDK_Left:
    g_snprintf(keystr, len, "left");
    break;
  case GDK_Up:
    g_snprintf(keystr, len, "up");
    break;
  case GDK_Right:
    g_snprintf(keystr, len, "right");
    break;
  case GDK_Down:
    g_snprintf(keystr, len, "down");
    break;
  case GDK_Prior:
    g_snprintf(keystr, len, "prior");
    break;
  case GDK_Next:
    g_snprintf(keystr, len, "next");
    break;
  case GDK_Home:
    g_snprintf(keystr, len, "home");
    break;
  case GDK_End:
    g_snprintf(keystr, len, "end");
    break;
  case GDK_Kanji:
  case GDK_Zenkaku_Hankaku:
    g_snprintf(keystr, len, "zenkaku-hankaku");
    break;
  case GDK_Multi_key:
    g_snprintf(keystr, len, "Multi_key");
    break;
  case GDK_Mode_switch:
    g_snprintf(keystr, len, "Mode_switch");
    break;
  case GDK_Henkan_Mode:
    g_snprintf(keystr, len, "Henkan_Mode");
    break;
  case GDK_Muhenkan:
    g_snprintf(keystr, len, "Muhenkan");
    break;
  case GDK_Shift_L:
  case GDK_Shift_R:
    g_snprintf(keystr, len, "Shift_key");
    break;
  case GDK_Control_L:
  case GDK_Control_R:
    g_snprintf(keystr, len, "Control_key");
    break;
  case GDK_Alt_L:
  case GDK_Alt_R:
    g_snprintf(keystr, len, "Alt_key");
    break;
  case GDK_Meta_L:
  case GDK_Meta_R:
    g_snprintf(keystr, len, "Meta_key");
    break;
  case GDK_Super_L:
  case GDK_Super_R:
    g_snprintf(keystr, len, "Super_key");
    break;
  case GDK_Hyper_L:
  case GDK_Hyper_R:
    g_snprintf(keystr, len, "Hyper_key");
    break;
  default:
    if (keyval >= GDK_F1 && keyval <= GDK_F35) {
      g_snprintf(keystr, len, "F%d", keyval - GDK_F1 + 1);
    } else if (keyval >= GDK_F1 && keyval <= GDK_F35) {
      g_snprintf(keystr, len, "%d", keyval - GDK_KP_0 + UKey_0);
    } else if (keyval < 256) {
      /*
       * Downcase alphabet keys for easy-to-recognize key
       * configuration.  uim-custom performs implicit shift key
       * encoding/decoding appropriately.
       */
      keystr[0] = g_ascii_tolower(keyval);
      keystr[1] = '\0';
    } else {
      /* UKey_Other */
    }
    break;
  }

  gtk_entry_set_text(GTK_ENTRY(key_pref_win.keycode_entry), keystr);

  key_pref_win.grabbed_key_val   = 0;
  key_pref_win.grabbed_key_state = 0;
}

static gboolean
grab_win_key_press_cb (GtkWidget *widget, GdkEventKey *event,
		       GtkEntry *key_entry)
{
  key_pref_win.grabbed_key_val   = event->keyval;
  key_pref_win.grabbed_key_state = event->state;

  return FALSE;
}

static gboolean
grab_win_key_release_cb (GtkWidget *widget, GdkEventKey *event,
			 GtkEntry *key_entry)
{
  key_pref_set_value(key_pref_win.grabbed_key_val,
		     key_pref_win.grabbed_key_state);

  g_signal_handlers_disconnect_by_func(G_OBJECT(widget),
				       (gpointer) grab_win_key_press_cb,
				       key_entry);
  g_signal_handlers_disconnect_by_func(G_OBJECT(widget),
				       (gpointer) grab_win_key_release_cb,
				       key_entry);

  gtk_dialog_response(GTK_DIALOG(widget), 0);

  return TRUE;
}

static gboolean
key_choose_entry_key_press_cb (GtkWidget *widget, GdkEventKey *event,
			       GtkEntry *key_entry)
{
  key_pref_set_value(event->keyval, event->state);
  return TRUE;
}

static void
choose_key_button_clicked_cb(GtkWidget *widget, GtkEntry *key_entry)
{
  GtkWidget *dialog;
  gint rv;

  dialog = gtk_message_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(widget)),
				  GTK_DIALOG_MODAL,
				  GTK_MESSAGE_INFO,
				  GTK_BUTTONS_CANCEL,
				  _("Press any key to grab..."));
  gtk_window_set_title(GTK_WINDOW(dialog), "Grabbing a key");
  g_signal_connect(G_OBJECT(dialog), "key-press-event",
		   G_CALLBACK(grab_win_key_press_cb), key_entry);
  g_signal_connect(G_OBJECT(dialog), "key-release-event",
		   G_CALLBACK(grab_win_key_release_cb), key_entry);

  gtk_widget_realize(dialog);
  gdk_keyboard_grab(GTK_WIDGET(dialog)->window,
		    TRUE, GDK_CURRENT_TIME);


  rv = gtk_dialog_run(GTK_DIALOG(dialog));

  gtk_widget_destroy(dialog);
}

static void
key_pref_entry_set_value(GtkEntry *entry)
{
  GString *str;
  struct uim_custom *custom;
  struct uim_custom_key *key;
  gint i;

  custom = g_object_get_data(G_OBJECT(entry), OBJECT_DATA_UIM_CUSTOM);
  g_return_if_fail(custom);

  str = g_string_new("");

  if (custom->value->as_key) {
    for (key = custom->value->as_key[0], i = 0;
	 key;
	 key = custom->value->as_key[++i])
    {
      if (i != 0)
	g_string_append(str, ",");
      g_string_append(str, key->literal);
    }
  } else {
    /* error message */
  }

  gtk_entry_set_text(GTK_ENTRY(entry), str->str);

  g_string_free(str, TRUE);

}

static void
key_pref_add_button_clicked_cb(GtkWidget *widget, GtkEntry *key_entry)
{
  struct uim_custom *custom;
  const char *key_code;
  GString *str;
  GtkTreeModel *model;
  GtkTreeIter iter;
  gint num;
  uim_bool rv;

  g_return_if_fail(GTK_IS_ENTRY(key_entry));

  custom = g_object_get_data(G_OBJECT(key_entry), OBJECT_DATA_UIM_CUSTOM);
  g_return_if_fail(custom);

  key_code = gtk_entry_get_text(GTK_ENTRY(key_pref_win.keycode_entry));
  if (!key_code || !*key_code)
    return;

  str = g_string_new("");

  if (GTK_TOGGLE_BUTTON(key_pref_win.shift_toggle)->active)
    g_string_append(str, "<Shift>");
  if (GTK_TOGGLE_BUTTON(key_pref_win.control_toggle)->active)
    g_string_append(str, "<Control>");
  if (GTK_TOGGLE_BUTTON(key_pref_win.alt_toggle)->active)
    g_string_append(str, "<Alt>");

  g_string_append(str, key_code);

  for (num = 0; custom->value->as_key[num]; num++);
  num++;

  custom->value->as_key = realloc(custom->value->as_key,
				  sizeof(struct uim_custom_key *) * (num + 1));

  custom->value->as_key[num - 1] = malloc(sizeof(struct uim_custom_key));
  custom->value->as_key[num - 1]->type = UCustomKey_Regular;
  custom->value->as_key[num - 1]->editor_type = UCustomKeyEditor_Basic;
  custom->value->as_key[num - 1]->literal = strdup(str->str);
  custom->value->as_key[num - 1]->label = strdup("");
  custom->value->as_key[num - 1]->desc = strdup("");
  custom->value->as_key[num] = NULL;

  rv = uim_custom_set(custom);

  if (rv != UIM_FALSE) {
    value_changed = TRUE;
    g_object_set_data(G_OBJECT(current_group_widget),
		      OBJECT_DATA_VALUE_CHANGED,
		      GINT_TO_POINTER(TRUE));

    model = gtk_tree_view_get_model(GTK_TREE_VIEW(key_pref_win.tree_view));
    gtk_list_store_append(GTK_LIST_STORE(model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(model), &iter,
		       0, str->str,
		       -1);
    key_pref_entry_set_value(key_entry);
  } else {
    g_printerr("Faild to set key value for \"%s\".\n", custom->symbol);
  }

  g_string_free(str, TRUE);
}

static void
key_pref_remove_button_clicked_cb(GtkWidget *widget, GtkEntry *key_entry)
{
  struct uim_custom *custom;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GtkTreePath *path;
  gboolean selected;
  gint num, *indices;
  uim_bool rv;

  g_return_if_fail(GTK_IS_ENTRY(key_entry));

  custom = g_object_get_data(G_OBJECT(key_entry), OBJECT_DATA_UIM_CUSTOM);
  g_return_if_fail(custom);

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(key_pref_win.tree_view));

  selected = gtk_tree_selection_get_selected(selection, &model, &iter);
  if (!selected)
    return;

  for (num = 0; custom->value->as_key[num]; num++);

  path = gtk_tree_model_get_path(model, &iter);
  indices = gtk_tree_path_get_indices(path);

  if (num > 0 && *indices < num) {
    free(custom->value->as_key[*indices]->literal);
    free(custom->value->as_key[*indices]->label);
    free(custom->value->as_key[*indices]->desc);
    free(custom->value->as_key[*indices]);

    memmove(custom->value->as_key + *indices,
	    custom->value->as_key + *indices + 1,
	    sizeof(struct uim_custom_key *) * (num - *indices));
    custom->value->as_key = realloc(custom->value->as_key,
				    sizeof(struct uim_custom_key *) * num);

    rv = uim_custom_set(custom);

    if (rv != UIM_FALSE) {
      value_changed = TRUE;
      g_object_set_data(G_OBJECT(current_group_widget),
			OBJECT_DATA_VALUE_CHANGED,
			GINT_TO_POINTER(TRUE));
      gtk_list_store_remove(GTK_LIST_STORE(model), &iter);

      key_pref_entry_set_value(key_entry);
    } else {
      g_printerr("Faild to set key value for \"%s\".\n", custom->symbol);
    }
  } else {
    /* error */
  }

  gtk_tree_path_free(path);

  selected = gtk_tree_selection_get_selected(selection, &model, &iter);
  gtk_widget_set_sensitive(key_pref_win.remove_button, selected);
}

static void
key_val_entry_changed_cb(GtkEntry *entry, GtkEntry *key_entry)
{
  const char *str = gtk_entry_get_text(entry);

  /* FIXME! validate key value */
  gtk_widget_set_sensitive(key_pref_win.add_button, str && *str);
}

static void
key_pref_dialog_response_cb(GtkDialog *dialog, gint action, GtkEntry *key_entry)
{
  gtk_widget_destroy(GTK_WIDGET(dialog));
}

static void
choose_key_clicked_cb(GtkWidget *widget, GtkEntry *key_entry)
{
  GtkWidget *dialog, *scrwin, *view, *hbox, *vbox, *button, *entry;
  GtkTreeSelection *selection;
  GtkListStore *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  struct uim_custom *custom;
  struct uim_custom_key *key;
  gint i;

  g_return_if_fail(GTK_IS_ENTRY(key_entry));

  custom = g_object_get_data(G_OBJECT(key_entry), OBJECT_DATA_UIM_CUSTOM);
  g_return_if_fail(custom);

  /* setup key pref dialog */
  dialog = gtk_dialog_new_with_buttons("key", GTK_WINDOW(pref_window),
				       GTK_DIALOG_MODAL,
				       GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
				       NULL);
  key_pref_win.window = dialog;
  gtk_window_set_default_size(GTK_WINDOW(dialog), 250, 200);
  g_signal_connect(G_OBJECT(dialog), "response",
		   G_CALLBACK(key_pref_dialog_response_cb), key_entry);

  hbox = gtk_hbox_new(FALSE, 0);
  gtk_container_set_border_width(GTK_CONTAINER(hbox), 4);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), hbox,
		     TRUE, TRUE, 0);
  gtk_widget_show(hbox);

  scrwin = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrwin),
				 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrwin), GTK_SHADOW_IN);
  gtk_box_pack_start(GTK_BOX(hbox), scrwin,
		     TRUE, TRUE, 0);
  gtk_widget_show(scrwin);

  store = gtk_list_store_new(1, G_TYPE_STRING);
  view = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
  key_pref_win.tree_view = view;
  gtk_container_add(GTK_CONTAINER(scrwin), view);
  gtk_widget_show(view);

  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(view), FALSE);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Key preference"),
						    renderer,
						    "text", 0,
						    NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(view), column);

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);
  g_signal_connect (G_OBJECT(selection), "changed",
		    G_CALLBACK(key_pref_selection_changed), key_entry);

  vbox = gtk_vbox_new(TRUE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), vbox, FALSE, FALSE, 4);
  gtk_widget_show(vbox);

#if 0
  button = gtk_button_new_from_stock(GTK_STOCK_GO_UP);
  gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 2);
  gtk_widget_show(button);

  button = gtk_button_new_from_stock(GTK_STOCK_GO_DOWN);
  gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 2);
  gtk_widget_show(button);
#endif

  button = gtk_button_new_from_stock(GTK_STOCK_ADD);
  key_pref_win.add_button = button;
  gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 2);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(key_pref_add_button_clicked_cb), key_entry);
  gtk_widget_set_sensitive(button, FALSE);
  gtk_widget_show(button);

  button = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
  key_pref_win.remove_button = button;
  gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 2);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(key_pref_remove_button_clicked_cb), key_entry);
  gtk_widget_set_sensitive(button, FALSE);
  gtk_widget_show(button);

  hbox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), hbox, FALSE, FALSE, 0);
  gtk_container_set_border_width(GTK_CONTAINER(hbox), 4);
  gtk_widget_show(hbox);

  button = gtk_check_button_new_with_mnemonic(_("_Shift"));
  key_pref_win.shift_toggle = button;
  gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
  gtk_widget_show(button);

  button = gtk_check_button_new_with_mnemonic(_("_Control"));
  key_pref_win.control_toggle = button;
  gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
  gtk_widget_show(button);

  button = gtk_check_button_new_with_mnemonic(_("_Alt"));
  key_pref_win.alt_toggle = button;
  gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
  gtk_widget_show(button);

  entry = gtk_entry_new();
  gtk_entry_set_editable(GTK_ENTRY(entry), FALSE);
  key_pref_win.keycode_entry = entry;
  gtk_widget_set_size_request(GTK_WIDGET(entry), 100, -1);
  gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 4);
  g_signal_connect(G_OBJECT(entry), "changed",
		   G_CALLBACK(key_val_entry_changed_cb), key_entry);
  g_signal_connect(G_OBJECT(entry), "key-press-event",
		   G_CALLBACK(key_choose_entry_key_press_cb), key_entry);
  gtk_widget_show(entry);

  button = gtk_button_new_with_label(_("..."));
  gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 4);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(choose_key_button_clicked_cb), key_entry);
  gtk_widget_show(button);

  /* set value */
  if (custom->value->as_key) {
    for (key = custom->value->as_key[0], i = 0;
	 key;
	 key = custom->value->as_key[++i])
    {
      GtkTreeIter iter;
      gtk_list_store_append(store, &iter);
      gtk_list_store_set(store, &iter,
			 0, key->literal,
			 -1);
    }
  } else {
    /* error message */
  }

  /* show dialog */
  gtk_widget_show(dialog);

  /* clean */
  g_object_unref(G_OBJECT(store));
}

static void
update_custom_type_key_cb(void *ptr, const char *custom_sym)
{
  struct uim_custom *custom = uim_custom_get(custom_sym);
  GtkWidget *entry = GTK_WIDGET(ptr);

  if (!custom || custom->type != UCustom_Key)
    return;
  
  g_object_set_data_full(G_OBJECT(entry),
			 OBJECT_DATA_UIM_CUSTOM, custom,
			 (GDestroyNotify) uim_custom_free);
  gtk_widget_set_sensitive(entry, custom->is_active);
}

static void
add_custom_type_key(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label, *entry, *button;

  hbox = gtk_hbox_new(FALSE, 8);
  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  entry = gtk_entry_new();
  gtk_entry_set_editable(GTK_ENTRY(entry), FALSE);
  g_object_set_data_full(G_OBJECT(entry),
			 OBJECT_DATA_UIM_CUSTOM, custom,
			 (GDestroyNotify) uim_custom_free);
  gtk_box_pack_start (GTK_BOX (hbox), entry, TRUE, TRUE, 0);
  key_pref_entry_set_value(GTK_ENTRY(entry));

  button = gtk_button_new_with_label(_("Choose..."));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(choose_key_clicked_cb), entry);
  uim_custom_cb_add(custom->symbol, entry, update_custom_type_key_cb);
  
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
}

static void
add_custom(GtkWidget *vbox, const char *custom_sym)
{
  struct uim_custom *custom;
  custom = uim_custom_get(custom_sym);

  if (custom) {
    /* Should be rewritten with table. */
    switch (custom->type) {
    case UCustom_Bool:
      add_custom_type_bool(vbox, custom);
      break;
    case UCustom_Int:
      add_custom_type_integer(vbox, custom);
      break;
    case UCustom_Str:
      add_custom_type_string(vbox, custom);
      break;
    case UCustom_Pathname:
      add_custom_type_pathname(vbox, custom);
      break;
    case UCustom_Choice:
      add_custom_type_choice(vbox, custom);
      break;
    case UCustom_OrderedList:
      add_custom_type_orderedlist(vbox, custom);
      break;
    case UCustom_Key:
      add_custom_type_key(vbox, custom);
      break;
    default:
      g_printerr("Invalid custom type: %d\n", custom->type);
      uim_custom_free(custom);
      break;
    }
  } else {
    g_printerr("Faild to get uim_custom object for %s.\n", custom_sym);
  }
}

static gboolean
pref_tree_model_foreach_unset_value_changed_fn(GtkTreeModel *model,
					       GtkTreePath *path,
					       GtkTreeIter *iter,
					       gpointer data)
{
  GtkWidget *widget = NULL;

  gtk_tree_model_get(model, iter,
		     GROUP_WIDGET, &widget,
		     -1);
  if (widget)
    g_object_set_data(G_OBJECT(widget), OBJECT_DATA_VALUE_CHANGED,
		      GINT_TO_POINTER(FALSE));

  return FALSE;
}

static void
ok_button_clicked(GtkButton *button, gpointer user_data)
{
  /*const char *group_name = user_data;*/

  if (value_changed) {
    uim_custom_save();
    uim_custom_broadcast();
    value_changed = FALSE;
    gtk_tree_model_foreach(
      gtk_tree_view_get_model(GTK_TREE_VIEW(pref_tree_view)),
      pref_tree_model_foreach_unset_value_changed_fn,
      NULL);
  }

  gtk_main_quit();
}

static void
apply_button_clicked(GtkButton *button, gpointer user_data)
{
  /*const char *group_name = user_data;*/

  if (value_changed) {
    uim_custom_save();
    uim_custom_broadcast();
    value_changed = FALSE;
    gtk_tree_model_foreach(
      gtk_tree_view_get_model(GTK_TREE_VIEW(pref_tree_view)), 
      pref_tree_model_foreach_unset_value_changed_fn,
      NULL);
  }
}

static GtkWidget *
create_setting_button_box(const char *group_name)
{
  GtkWidget *setting_button_box;
  GtkWidget *button;

  setting_button_box = gtk_hbutton_box_new();
  gtk_button_box_set_layout(GTK_BUTTON_BOX(setting_button_box), GTK_BUTTONBOX_END);
  gtk_box_set_spacing(GTK_BOX(setting_button_box), 8);

  /* Apply button */
  button = gtk_button_new_from_stock(GTK_STOCK_APPLY);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(apply_button_clicked), (gpointer) group_name);
  gtk_box_pack_start(GTK_BOX(setting_button_box), button, TRUE, TRUE, 8);

  /* Cancel button */
  button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(quit_confirm), NULL);
  gtk_box_pack_start(GTK_BOX(setting_button_box), button, TRUE, TRUE, 8);

  /* OK button */
  button = gtk_button_new_from_stock(GTK_STOCK_OK);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(ok_button_clicked), (gpointer) group_name);
  gtk_box_pack_start(GTK_BOX(setting_button_box), button, TRUE, TRUE, 8);
  return setting_button_box;
}

static GtkWidget *
create_group_widget(const char *group_name)
{
  GtkWidget *vbox;
  GtkWidget *group_label;
  GtkWidget *setting_button_box;
  struct uim_custom_group *group;
  char **custom_syms, **custom_sym;
  char *label_text;
  spin_button_sgroup = gtk_size_group_new(GTK_SIZE_GROUP_VERTICAL);
  vbox = gtk_vbox_new(FALSE, 8);

  gtk_container_set_border_width(GTK_CONTAINER(vbox), 4);

  group = uim_custom_group_get(group_name);

  if(group == NULL)
    return NULL;

  group_label = gtk_label_new("");
  label_text  = g_markup_printf_escaped("<span size=\"xx-large\">%s</span>",
					group->label);
  gtk_label_set_markup(GTK_LABEL(group_label), label_text);
  g_free(label_text);

  gtk_box_pack_start (GTK_BOX(vbox), group_label, FALSE, TRUE, 8);

  custom_syms = uim_custom_collect_by_group(group_name);
  if (custom_syms) {
    for (custom_sym = custom_syms; *custom_sym; custom_sym++) {
      add_custom(vbox, *custom_sym);
    }
    uim_custom_symbol_list_free(custom_syms);
  }
  
  uim_custom_group_free(group);

  setting_button_box = create_setting_button_box(group_name);
  gtk_box_pack_end(GTK_BOX(vbox), setting_button_box, FALSE, FALSE, 8);

  g_object_unref(spin_button_sgroup);
  return vbox;
}

static GtkWidget *
create_pref_window(void)
{
  GtkWidget *window;
  GtkWidget *scrolled_win; /* treeview container */

  pref_window = window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  
  g_signal_connect(G_OBJECT (window), "delete_event",
		   G_CALLBACK (delete_event_cb), NULL);

  pref_hbox = gtk_hbox_new(FALSE, 8);

  scrolled_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolled_win),
				       GTK_SHADOW_ETCHED_IN);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win),
				  GTK_POLICY_NEVER,
				  GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(pref_hbox), scrolled_win, FALSE, TRUE, 0);

  gtk_container_add(GTK_CONTAINER(scrolled_win), create_pref_treeview());
  gtk_container_add(GTK_CONTAINER(window), pref_hbox);

  {
  GdkScreen *scr = gtk_window_get_screen(GTK_WINDOW(window));
  gtk_window_set_default_size(GTK_WINDOW(window),
			      gdk_screen_get_width(scr)  / 2,
			      gdk_screen_get_height(scr) / 2);
  gtk_window_set_position(GTK_WINDOW(window),
			  GTK_WIN_POS_CENTER_ALWAYS);
  }
  
  return window;
}

int 
main (int argc, char *argv[])
{
  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8");

  gtk_set_locale();
  
  gtk_init(&argc, &argv);

  if (uim_init() < 0) {
    fprintf(stderr, "uim_init() failed.\n");
    return -1;
  }

  if (uim_custom_enable()) {
    GtkWidget *pref;
  
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
