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

#include <gtk/gtk.h>
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
				    _("Value was changed.\n"
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
quit_confirm (void)
{
  if (value_changed) {
    GtkWidget *dialog;
    gboolean quit = FALSE;

    dialog = gtk_message_dialog_new(NULL,
				    GTK_DIALOG_MODAL,
				    GTK_MESSAGE_QUESTION,
				    GTK_BUTTONS_OK_CANCEL,
				    _("Value was changed.\n"
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
  gtk_tree_view_set_model (GTK_TREE_VIEW(pref_tree_view), GTK_TREE_MODEL(tree_store));
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

  if (custom->type == UCustom_Choice) {
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

    custom->value->as_choice->symbol = strdup(choice->symbol);
    custom->value->as_choice->label  = strdup(choice->label);
    custom->value->as_choice->desc   = strdup(choice->desc);

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

  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
}

static void
add_custom_type_key(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;

  hbox = gtk_hbox_new(FALSE, 8);
  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  /*
  g_object_set_data_full(G_OBJECT(hoge),
			 OBJECT_DATA_UIM_CUSTOM, custom,
			 (GDestroyNotify) uim_custom_free);
  */

  gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);
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
  GtkWidget *pref;
  
  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8");

  gtk_set_locale();
  
  gtk_init(&argc, &argv);

  uim_init();
  uim_custom_init();  

  pref = create_pref_window();

  gtk_widget_show_all(pref);

  gtk_main();

  uim_quit();

  return 0;
}
