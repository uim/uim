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

static GtkWidget *pref_tree_view;
static GtkWidget *pref_hbox;
static GtkWidget *current_group_widget;
static GtkSizeGroup *spin_button_sgroup;

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

static gboolean
pref_tree_selection_changed(GtkTreeSelection *selection,
			     gpointer data)
{
  GtkTreeStore *store;
  GtkTreeIter iter;
  GtkTreeModel *model;
  char *group_name;
  GtkWidget *group_widget;

  /* Preference save check should be here. */

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
delete_event_cb(GtkWidget *widget, gpointer data)
{
  gtk_main_quit();
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
    gtk_tree_store_append (tree_store, &iter, NULL/* parent iter */);
    gtk_tree_store_set (tree_store, &iter,
			GROUP_COLUMN, *grp,
			GROUP_WIDGET, create_group_widget(*grp),
			-1);
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
add_custom_type_bool(GtkWidget *vbox, const struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *check_button;
  hbox = gtk_hbox_new(FALSE, 8);

  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  check_button = gtk_check_button_new();

  gtk_box_pack_start (GTK_BOX (hbox), check_button, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
}


static void
add_custom_type_integer(GtkWidget *vbox, const struct uim_custom *custom)
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
  
  spin = gtk_spin_button_new(adjustment, 1.0, 0);
  gtk_size_group_add_widget(spin_button_sgroup, spin);
  gtk_box_pack_end (GTK_BOX (hbox), spin, FALSE, TRUE, 0);
  
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
}

static void
add_custom_type_string(GtkWidget *vbox, const struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *entry;

  hbox = gtk_hbox_new(FALSE, 8);

  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  entry = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(entry), custom->value->as_str);
  gtk_box_pack_start (GTK_BOX (hbox), entry, TRUE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
}


static void
add_custom_type_pathname(GtkWidget *vbox, const struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *entry;
  GtkWidget *button;

  hbox = gtk_hbox_new(FALSE, 8);
 
  label = gtk_label_new(custom->label); 
  gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);

  entry = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(entry), custom->value->as_pathname);
  gtk_box_pack_start (GTK_BOX (hbox), entry, TRUE, TRUE, 0);

  button = gtk_button_new_with_label("File");

  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
}

static void
add_custom_type_choice(GtkWidget *vbox, const struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *combobox;
  struct uim_custom_choice **item;
  gint i, default_index;
  gchar *default_symbol;

  hbox = gtk_hbox_new(FALSE, 8);
  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  combobox = gtk_combo_box_new_text();
  
  default_symbol = custom->default_value->as_choice->symbol;

  for(i = 0, item = custom->range->as_choice.valid_items; *item; i++, item++) {
    gtk_combo_box_append_text(GTK_COMBO_BOX(combobox),
			      (*item)->label);
    if(default_symbol == (*item)->symbol);
    default_index = i;
  }
  gtk_combo_box_set_active(GTK_COMBO_BOX(combobox), default_index);
  gtk_box_pack_start (GTK_BOX (hbox), combobox, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
}

static void
add_custom_type_key(GtkWidget *vbox, const struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;

  hbox = gtk_hbox_new(FALSE, 8);
  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

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
    }
  uim_custom_free(custom);
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
  gtk_box_pack_start(GTK_BOX(setting_button_box), button, TRUE, TRUE, 8);

  /* Cancel button */
  button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(gtk_main_quit), NULL);
  gtk_box_pack_start(GTK_BOX(setting_button_box), button, TRUE, TRUE, 8);

  /* OK button */
  button = gtk_button_new_from_stock(GTK_STOCK_OK);
  /*  g_signal_connect(G_OBJECT(button), "clicked",
      G_CALLBACK(ok_button_clicked), group_name);*/
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

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  
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
