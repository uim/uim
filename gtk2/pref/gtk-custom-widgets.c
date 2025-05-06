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

#include "gtk-custom-widgets.h"
#include <gdk/gdkkeysyms.h>

#include <string.h>
#include <stdlib.h>
#include <locale.h>

#include "uim/uim.h"
#include "uim/uim-custom.h"
#include "uim/gettext.h"

#include "../immodule/key-util-gtk.h"
#include "gtk-keytab.h"

#define DEFAULT_OLIST_WINDOW_WIDTH    480
#define DEFAULT_OLIST_WINDOW_HEIGHT   350
#define DEFAULT_KEYCONF_WINDOW_WIDTH  280
#define DEFAULT_KEYCONF_WINDOW_HEIGHT 220
#define DEFAULT_TABLE_WINDOW_WIDTH    340
#define DEFAULT_TABLE_WINDOW_HEIGHT   280
#define OBJECT_DATA_UIM_CUSTOM_SYM    "uim-pref-gtk::uim-custom-sym"

extern gboolean uim_pref_gtk_value_changed;

extern void uim_pref_gtk_mark_value_changed(void);

static GtkSizeGroup *spin_button_sgroup = NULL;

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
  GtkWidget *keycode_entry;

  gint grabbed_key_val;
  gint grabbed_key_state;
} key_pref_win = {
  NULL, NULL, NULL, NULL, NULL, 0, 0,
};

static void uimpref_file_entry_class_init(UimPrefFileEntryClass *klass);
static void uimpref_file_entry_init(UimPrefFileEntry *entry);

GType
uimpref_file_entry_get_type(void)
{
  static GType uimpref_file_entry_type = 0;

  if (!uimpref_file_entry_type)
  {
    static const GTypeInfo uimpref_file_entry_info =
    {
      sizeof(UimPrefFileEntryClass),
      NULL,
      NULL,
      (GClassInitFunc)uimpref_file_entry_class_init,
      NULL,
      NULL,
      sizeof(UimPrefFileEntry),
      0,
      (GInstanceInitFunc)uimpref_file_entry_init,
    };

    uimpref_file_entry_type = g_type_register_static(GTK_TYPE_ENTRY,
		    "UimPrefFileEntry", &uimpref_file_entry_info, 0);
  }

  return uimpref_file_entry_type;
}

static void
uimpref_file_entry_class_init(UimPrefFileEntryClass *klass)
{
}

static void
uimpref_file_entry_init(UimPrefFileEntry *entry)
{
  entry->type = UCustomPathnameType_RegularFile;
}

GtkWidget *
uimpref_file_entry_new()
{
  return GTK_WIDGET(g_object_new(uimpref_file_entry_get_type(), NULL));
}


static void
custom_check_button_toggled_cb(GtkToggleButton *button, gpointer user_data)
{
  const char *custom_sym;
  struct uim_custom *custom;
  uim_bool rv;
  gboolean active;

  custom_sym = g_object_get_data(G_OBJECT(button), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_Bool);

  active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button));
  custom->value->as_bool = active;
  rv = uim_custom_set(custom);

  if (rv) {
    uim_pref_gtk_mark_value_changed();
  } else {
    g_printerr("Failed to set bool value for \"%s\".\n", custom->symbol);
    /* FIXME! reset the widget */
  }

  uim_custom_free(custom);
}

static void
sync_value_bool(GtkCheckButton *button)
{
  const char *custom_sym;
  struct uim_custom *custom;

  g_signal_handlers_block_by_func(G_OBJECT(button),
				  (gpointer)(uintptr_t) custom_check_button_toggled_cb,
				  NULL);

  custom_sym = g_object_get_data(G_OBJECT(button), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_Bool);

  gtk_widget_set_sensitive(GTK_WIDGET(button), custom->is_active);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), custom->value->as_bool);

  uim_custom_free(custom);

  g_signal_handlers_unblock_by_func(G_OBJECT(button),
				    (gpointer)(uintptr_t) custom_check_button_toggled_cb,
				    NULL);
}

static void
update_custom_type_bool_cb(void *ptr, const char *custom_sym)
{
  sync_value_bool(GTK_CHECK_BUTTON(ptr));
}

static void
add_custom_type_bool(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *check_button;
#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
#else
  hbox = gtk_hbox_new(FALSE, 8);
#endif

  check_button = gtk_check_button_new_with_label(custom->label);
  g_object_set_data_full(G_OBJECT(check_button),
			 OBJECT_DATA_UIM_CUSTOM_SYM, g_strdup(custom->symbol),
			 (GDestroyNotify) g_free);

  sync_value_bool(GTK_CHECK_BUTTON(check_button));

  g_signal_connect(G_OBJECT(check_button), "toggled",
		   G_CALLBACK(custom_check_button_toggled_cb), NULL);
  uim_custom_cb_add(custom->symbol, check_button, update_custom_type_bool_cb);

  gtk_box_pack_start (GTK_BOX (hbox), check_button, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
}



static void
custom_spin_button_value_changed(GtkSpinButton *spin, gpointer user_data)
{
  const char *custom_sym;
  struct uim_custom *custom;
  uim_bool rv;

  custom_sym = g_object_get_data(G_OBJECT(spin), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_Int);

  custom->value->as_int = gtk_spin_button_get_value(spin);
  rv = uim_custom_set(custom);

  if (rv) {
    uim_pref_gtk_mark_value_changed();
  } else {
    g_printerr("Failed to set int value for \"%s\".\n", custom->symbol);
    /* FIXME! reset the widget */
  }

  uim_custom_free(custom);
}

static void
sync_value_int(GtkSpinButton *spin)
{
  const char *custom_sym;
  struct uim_custom *custom;
  GtkAdjustment *adj;

  g_signal_handlers_block_by_func(G_OBJECT(spin),
				  (gpointer)(uintptr_t) custom_spin_button_value_changed,
				  NULL);

  adj = gtk_spin_button_get_adjustment(spin);

  custom_sym = g_object_get_data(G_OBJECT(spin), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_Int);

  gtk_widget_set_sensitive(gtk_widget_get_parent(GTK_WIDGET(spin)),
      custom->is_active);
  if (custom->range->as_int.min != (int) gtk_adjustment_get_lower(adj) ||
      custom->range->as_int.max != (int) gtk_adjustment_get_upper(adj))
    gtk_spin_button_set_range(spin, custom->range->as_int.min, custom->range->as_int.max);
  if (custom->value->as_int != (int) gtk_spin_button_get_value(spin)) {
    gtk_spin_button_set_value(spin, custom->value->as_int);
  }

  uim_custom_free(custom);

  g_signal_handlers_unblock_by_func(G_OBJECT(spin),
				    (gpointer)(uintptr_t) custom_spin_button_value_changed,
				    NULL);
}

static void
update_custom_type_int_cb(void *ptr, const char *custom_sym)
{
  sync_value_int(GTK_SPIN_BUTTON(ptr));
}

static void
add_custom_type_integer(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkAdjustment *adjustment;
  GtkWidget *spin;

  if (!spin_button_sgroup)
    spin_button_sgroup = gtk_size_group_new(GTK_SIZE_GROUP_VERTICAL);

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
#else
  hbox = gtk_hbox_new(FALSE, 8);
#endif

  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  adjustment = (GtkAdjustment*)gtk_adjustment_new(custom->value->as_int, /* initial size */
						  custom->range->as_int.min, /* minimum */
						  custom->range->as_int.max, /* maximum */
						  1.0,
						  10.0,
						  0);
  spin = gtk_spin_button_new(adjustment, 1.0, 0);
  gtk_size_group_add_widget(spin_button_sgroup, spin);
  gtk_box_pack_end (GTK_BOX (hbox), spin, FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);

  g_object_set_data_full(G_OBJECT(adjustment),
			 OBJECT_DATA_UIM_CUSTOM_SYM, g_strdup(custom->symbol),
			 (GDestroyNotify) g_free);
  g_object_set_data_full(G_OBJECT(spin),
			 OBJECT_DATA_UIM_CUSTOM_SYM, g_strdup(custom->symbol),
			 (GDestroyNotify) g_free);

  sync_value_int(GTK_SPIN_BUTTON(spin));

  g_signal_connect(G_OBJECT(spin), "value-changed",
		   G_CALLBACK(custom_spin_button_value_changed), NULL);
  uim_custom_cb_add(custom->symbol, spin, update_custom_type_int_cb);
}



static void
custom_entry_changed_cb(GtkEntry *entry, gpointer user_data)
{
  const char *custom_sym;
  struct uim_custom *custom;
  uim_bool rv;
  const char *str;

  custom_sym = g_object_get_data(G_OBJECT(entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom &&
		   (custom->type == UCustom_Str ||
		    custom->type == UCustom_Pathname));

  str = gtk_entry_get_text(GTK_ENTRY(entry));

  if (custom->type == UCustom_Str) {
    free(custom->value->as_str);
    custom->value->as_str = strdup(str);
    rv = uim_custom_set(custom);
  } else if (custom->type == UCustom_Pathname) {
    free(custom->value->as_pathname->str);
    custom->value->as_pathname->str = strdup(str);
    rv = uim_custom_set(custom);
  } else {
    rv = UIM_FALSE;
  }

  if (rv) {
    uim_pref_gtk_mark_value_changed();
  } else {
    g_printerr("Failed to set str value for \"%s\".\n", custom->symbol);
    /* FIXME! reset the widget */
  }

  uim_custom_free(custom);
}

static void
sync_value_string(GtkEntry *entry)
{
  const char *custom_sym;
  struct uim_custom *custom;

  g_signal_handlers_block_by_func(G_OBJECT(entry),
				  (gpointer)(uintptr_t) custom_entry_changed_cb,
				  NULL);

  custom_sym = g_object_get_data(G_OBJECT(entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom &&
		   (custom->type == UCustom_Str ||
		    custom->type == UCustom_Pathname));

  gtk_widget_set_sensitive(gtk_widget_get_parent(GTK_WIDGET(entry)),
      custom->is_active);
  if (custom->type == UCustom_Str) {
    gtk_entry_set_text(GTK_ENTRY(entry), custom->value->as_str);
  } else if (custom->type == UCustom_Pathname) {
    gtk_entry_set_text(GTK_ENTRY(entry), custom->value->as_pathname->str);
  }

  uim_custom_free(custom);

  g_signal_handlers_unblock_by_func(G_OBJECT(entry),
				    (gpointer)(uintptr_t) custom_entry_changed_cb,
				    NULL);
}

static void
update_custom_type_string_cb(void *ptr, const char *custom_sym)
{
  sync_value_string(GTK_ENTRY(ptr));
}

static void
add_custom_type_string(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *entry;

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
#else
  hbox = gtk_hbox_new(FALSE, 8);
#endif

  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  entry = gtk_entry_new();
  gtk_box_pack_start (GTK_BOX (hbox), entry, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);

  g_object_set_data_full(G_OBJECT(entry),
			 OBJECT_DATA_UIM_CUSTOM_SYM, g_strdup(custom->symbol),
			 (GDestroyNotify) g_free);

  sync_value_string(GTK_ENTRY(entry));

  g_signal_connect(G_OBJECT(entry), "changed",
		   G_CALLBACK(custom_entry_changed_cb), NULL);
  uim_custom_cb_add(custom->symbol, entry, update_custom_type_string_cb);
}



static void
custom_pathname_button_clicked_cb(GtkWidget *button, GtkWidget *entry)
{
  GtkWidget *dialog;
  GtkFileChooserAction action;

  switch (UIMPREF_FILE_ENTRY(entry)->type) {
    case UCustomPathnameType_Directory:
      action = GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER;
      break;
    case UCustomPathnameType_RegularFile:
    default:
      action = GTK_FILE_CHOOSER_ACTION_OPEN;
      break;
  }
  dialog = gtk_file_chooser_dialog_new (_("Specify file"),
					NULL,
					action,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
					(const gchar *)NULL);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {
    char *filename;
    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    if (filename) {
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
  const char *button_label;

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
#else
  hbox = gtk_hbox_new(FALSE, 8);
#endif

  label = gtk_label_new(custom->label);
  gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);

  entry = uimpref_file_entry_new();
  UIMPREF_FILE_ENTRY(entry)->type = custom->value->as_pathname->type;
  gtk_box_pack_start (GTK_BOX (hbox), entry, TRUE, TRUE, 0);

  /* Since both pathname type opens the file dialog to select an item
   * rather than open it, the label should always be "Select..." here.
   * The type is obvious for uses even if the button label does not
   * indicate it. Information about the action the button causes is
   * more important. Even if a better label has been found, it should
   * not contain the term 'directory' since GNOME uses 'folder' for
   * it.  -- YamaKen 2006-01-21 */
  switch (custom->value->as_pathname->type) {
    case UCustomPathnameType_Directory:
      button_label = N_("Select...");
      break;
    case UCustomPathnameType_RegularFile:
    default:
      button_label = N_("Select...");
      break;
  }
  button = gtk_button_new_with_label(dgettext(GETTEXT_PACKAGE, button_label));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);

  g_object_set_data_full(G_OBJECT(entry),
			 OBJECT_DATA_UIM_CUSTOM_SYM, g_strdup(custom->symbol),
			 (GDestroyNotify) g_free);

  sync_value_string(GTK_ENTRY(entry));

  g_signal_connect(G_OBJECT(entry), "changed",
		   G_CALLBACK(custom_entry_changed_cb), NULL);

  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(custom_pathname_button_clicked_cb), entry);
  uim_custom_cb_add(custom->symbol, entry, update_custom_type_string_cb);
}



static void
custom_combo_box_changed(GtkComboBox *combo_box, gpointer user_data)
{
  const char *custom_sym;
  struct uim_custom *custom;
  struct uim_custom_choice *choice = NULL;
  gint i, num;
  uim_bool rv;

  custom_sym = g_object_get_data(G_OBJECT(combo_box), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom_sym && custom->type == UCustom_Choice);

  num = gtk_combo_box_get_active(combo_box);

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
    uim_pref_gtk_mark_value_changed();
  } else {
    g_printerr("Failed to set str value for \"%s\".\n", custom->symbol);
    /* FIXME! reset the widget */
  }

  uim_custom_free(custom);
}

static void
sync_value_choice(GtkComboBox *combobox)
{
  const char *custom_sym;
  struct uim_custom *custom;
  struct uim_custom_choice **item;
  gint i = 0, default_index = 0;
  gchar *default_symbol;

  custom_sym = g_object_get_data(G_OBJECT(combobox), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail (custom && custom->type == UCustom_Choice);

  item = custom->range->as_choice.valid_items;
  if (item == NULL || *item == NULL) {
    uim_custom_free(custom);
    return;
  }

  g_signal_handlers_block_by_func(G_OBJECT(combobox),
				  (gpointer)(uintptr_t) custom_combo_box_changed, NULL);

  gtk_list_store_clear(GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(combobox))));

  default_symbol = custom->value->as_choice->symbol;

  while (*item) {
#if GTK_CHECK_VERSION(2, 24, 0)
    gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combobox),
        (*item)->label);
#else
    gtk_combo_box_append_text(GTK_COMBO_BOX(combobox), (*item)->label);
#endif
    if (!strcmp(default_symbol, (*item)->symbol))
      default_index = i;
    i++;
    item++;
  }
  gtk_combo_box_set_active(GTK_COMBO_BOX(combobox), default_index);

  gtk_widget_set_sensitive(gtk_widget_get_parent(GTK_WIDGET(combobox)),
      custom->is_active);

  g_signal_handlers_unblock_by_func(G_OBJECT(combobox),
				    (gpointer)(uintptr_t) custom_combo_box_changed, NULL);

  uim_custom_free(custom);
}

static void
update_custom_type_choice_cb(void *ptr, const char *custom_sym)
{
  sync_value_choice(GTK_COMBO_BOX(ptr));
}

static void
add_custom_type_choice(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *combobox;

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
#else
  hbox = gtk_hbox_new(FALSE, 8);
#endif
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);

  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

#if GTK_CHECK_VERSION(2, 24, 0)
  combobox = gtk_combo_box_text_new();
#else
  combobox = gtk_combo_box_new_text();
#endif
  gtk_box_pack_start (GTK_BOX (hbox), combobox, FALSE, TRUE, 0);

  g_object_set_data_full(G_OBJECT(combobox),
			 OBJECT_DATA_UIM_CUSTOM_SYM, g_strdup(custom->symbol),
			 (GDestroyNotify) g_free);

  sync_value_choice(GTK_COMBO_BOX(combobox));

  g_signal_connect(G_OBJECT(combobox), "changed",
		   G_CALLBACK(custom_combo_box_changed), NULL);
  uim_custom_cb_add(custom->symbol, combobox, update_custom_type_choice_cb);
}



static void
olist_pref_dialog_response_cb(GtkDialog *dialog, gint action, GtkEntry *key_entry)
{
  gtk_widget_destroy(GTK_WIDGET(dialog));
}

static void
sync_value_olist(GtkEntry *entry)
{
  const char *custom_sym;
  struct uim_custom *custom;
  struct uim_custom_choice *item;
  GString *str;
  gint i;

  custom_sym = g_object_get_data(G_OBJECT(entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_OrderedList);

  str = g_string_new("");

  if (custom->value->as_olist) {
    for (item = custom->value->as_olist[0], i = 0;
	 item;
	 item = custom->value->as_olist[++i])
    {
      if (i != 0)
	g_string_append(str, ", ");
      g_string_append_printf(str, "\"%s\"", item->label);
    }
  } else {
    /* error message */
  }

  gtk_entry_set_text(GTK_ENTRY(entry), str->str);
  gtk_widget_set_sensitive(gtk_widget_get_parent(GTK_WIDGET(entry)),
      custom->is_active);

  g_string_free(str, TRUE);
  uim_custom_free(custom);
}

static void
olist_pref_tree_view_set_value(GtkEntry *olist_entry,
			       gboolean left_view,
			       gboolean right_view)
{
  const char *custom_sym;
  struct uim_custom *custom;
  struct uim_custom_choice *item;
  GtkTreeView *view;
  GtkListStore *store;
  GtkTreeIter iter;
  gint i;

  custom_sym = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_OrderedList);

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
			 1, item->symbol,
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
			   1, item->symbol,
			   -1);
      }
    }
  }

  uim_custom_free(custom);
}

static void
set_olist_buttons_sensitive(GtkEntry *olist_entry)
{
  const char *custom_sym;
  struct uim_custom *custom;
  GtkWidget *view;
  GtkTreeSelection *selection;
  gint num;
  gboolean is_selected1 = FALSE, is_selected2 = FALSE;
  gboolean is_multiple = FALSE, is_first = FALSE, is_end = FALSE;

  custom_sym = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_OrderedList);

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

  uim_custom_free(custom);
}

static void
olist_pref_up_button_clicked_cb(GtkWidget *widget, GtkEntry *olist_entry)
{
  const char *custom_sym;
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

  custom_sym = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);;
  g_return_if_fail(custom && custom->type == UCustom_OrderedList);

  view = olist_pref_win.tree_view[0];

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  num = gtk_tree_selection_count_selected_rows(selection);
  if (num < 1) {
    uim_custom_free(custom);
    return;
  }
  if (num > 1) {
    uim_custom_free(custom);
    return;
  }

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

  uim_pref_gtk_mark_value_changed();

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
  sync_value_olist(GTK_ENTRY(olist_entry));

ERROR:
  uim_custom_free(custom);
  g_list_foreach(rows, (GFunc)gtk_tree_path_free, NULL);
  g_list_free(rows);
}

static void
olist_pref_down_button_clicked_cb(GtkWidget *widget, GtkEntry *olist_entry)
{
  const char *custom_sym;
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

  custom_sym = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_OrderedList);

  view = olist_pref_win.tree_view[0];

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  num = gtk_tree_selection_count_selected_rows(selection);
  if (num < 1) {
    uim_custom_free(custom);
    return;
  }
  if (num > 1) {
    uim_custom_free(custom);
    return;
  }

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

  uim_pref_gtk_mark_value_changed();

  /* sync the view */
  gtk_list_store_swap(GTK_LIST_STORE(model), &iter1, &iter2);
  path = gtk_tree_model_get_path(model, &iter1);
  gtk_tree_view_scroll_to_cell(GTK_TREE_VIEW(view), path, NULL,
			       FALSE, 0.0, 0.0);
  gtk_tree_path_free(path);
  set_olist_buttons_sensitive(olist_entry);
  sync_value_olist(GTK_ENTRY(olist_entry));

ERROR:
  uim_custom_free(custom);
  g_list_foreach(rows, (GFunc)gtk_tree_path_free, NULL);
  g_list_free(rows);
}

static void
olist_pref_left_button_clicked_cb(GtkWidget *widget, GtkEntry *olist_entry)
{
  const char *custom_sym;
  struct uim_custom *custom;
  GtkWidget *view;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GtkTreePath *path;
  gint num;
  GList *rows = NULL, *node;
  uim_bool urv;

  custom_sym = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_OrderedList);

  view = olist_pref_win.tree_view[1];

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  rows = gtk_tree_selection_get_selected_rows(selection, &model);
  if (!rows) {
    uim_custom_free(custom);
    return;
  }

  for (num = 0; custom->value->as_olist[num]; num++);

  for (node = rows; node; node = g_list_next(node)) {
    char *symbol = NULL;
    struct uim_custom_choice *choice = NULL;
    gint i;

    path = node->data;

    gtk_tree_model_get_iter(model, &iter, path);
    gtk_tree_model_get(model, &iter,
		       1, &symbol,
		       -1);

    if (!symbol)
      continue;

    for (i = 0; custom->range->as_olist.valid_items[i]->symbol; i++) {
      if (!strcmp(custom->range->as_olist.valid_items[i]->symbol, symbol)) {
	choice = custom->range->as_olist.valid_items[i];
	break;
      }
    }

    g_free(symbol);

    if (!choice)
      continue;

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
    sync_value_olist(GTK_ENTRY(olist_entry));
    olist_pref_tree_view_set_value(GTK_ENTRY(olist_entry), TRUE, TRUE);
    uim_pref_gtk_mark_value_changed();
  } else {
    /* error message */
  }

  uim_custom_free(custom);
  g_list_foreach(rows, (GFunc)gtk_tree_path_free, NULL);
  g_list_free(rows);
}

static void
olist_pref_right_button_clicked_cb(GtkWidget *widget, GtkEntry *olist_entry)
{
  const char *custom_sym;
  struct uim_custom *custom;
  struct uim_custom_choice *choice;
  GtkWidget *view;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreePath *path;
  gint *indices, idx, num;
  GList *rows = NULL, *node;
  uim_bool urv;

  custom_sym = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_OrderedList);

  view = olist_pref_win.tree_view[0];

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  rows = gtk_tree_selection_get_selected_rows(selection, &model);
  if (!rows) {
    uim_custom_free(custom);
    return;
  }

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
    sync_value_olist(GTK_ENTRY(olist_entry));
    olist_pref_tree_view_set_value(GTK_ENTRY(olist_entry), TRUE, TRUE);
    uim_pref_gtk_mark_value_changed();
    /* FIXME! reset the selection */
  } else {
    /* error message */
  }

  uim_custom_free(custom);
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
  const char *custom_sym;
  struct uim_custom *custom;
  GtkWidget *dialog, *hbox, *vbox, *scrwin, *table;
  GtkWidget *tree_view, *button, *arrow, *label;
  GtkListStore *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;

  custom_sym = g_object_get_data(G_OBJECT(olist_entry), OBJECT_DATA_UIM_CUSTOM_SYM);;
  g_return_if_fail(custom_sym);
  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom);

  dialog = gtk_dialog_new_with_buttons(
    custom->label, GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(olist_entry))),
    GTK_DIALOG_MODAL,
    GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
    NULL);

  uim_custom_free(custom);

  olist_pref_win.window = dialog;
  gtk_window_set_default_size(GTK_WINDOW(dialog),
			      DEFAULT_OLIST_WINDOW_WIDTH,
			      DEFAULT_OLIST_WINDOW_HEIGHT);
  g_signal_connect(G_OBJECT(dialog), "response",
		   G_CALLBACK(olist_pref_dialog_response_cb), olist_entry);

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
#else
  hbox = gtk_hbox_new(FALSE, 0);
#endif
  gtk_container_set_border_width(GTK_CONTAINER(hbox), 4);
  gtk_box_pack_start(GTK_BOX(gtk_dialog_get_content_area(GTK_DIALOG(dialog))),
      hbox, TRUE, TRUE, 0);
  gtk_widget_show(hbox);

  /* left tree view */
#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
#else
  vbox = gtk_vbox_new(FALSE, 0);
#endif
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

  store = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);
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
						    (const gchar *)NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);

  g_object_unref(store);

  /* button area */
#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
#else
  vbox = gtk_vbox_new(TRUE, 0);
#endif
  gtk_box_pack_start(GTK_BOX(hbox), vbox,
		     FALSE, FALSE, 4);
  gtk_widget_show(vbox);

#if GTK_CHECK_VERSION(3, 4, 0)
  table = gtk_grid_new();
  gtk_grid_set_row_spacing(GTK_GRID(table), 3);
  gtk_grid_set_column_spacing(GTK_GRID(table), 3);
#else
  table = gtk_table_new(3, 6, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), 3);
  gtk_table_set_col_spacings(GTK_TABLE(table), 3);
#endif
  gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);
  gtk_widget_show(table);

  /* up button */
  button = gtk_button_new();
  olist_pref_win.up_button = button;
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_widget_set_hexpand(button, TRUE);
  gtk_widget_set_vexpand(button, TRUE);
  gtk_grid_attach(GTK_GRID(table), button, 1, 0, 1, 1);
#else
  gtk_table_attach_defaults(GTK_TABLE(table), button,
			    1, 2,
			    0, 1);
#endif
  gtk_widget_show(button);
  arrow = gtk_arrow_new(GTK_ARROW_UP, GTK_SHADOW_NONE);
  gtk_container_add(GTK_CONTAINER(button), arrow);
  gtk_widget_show(arrow);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(olist_pref_up_button_clicked_cb), olist_entry);

  /* down button */
  button = gtk_button_new();
  olist_pref_win.down_button = button;
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_widget_set_hexpand(button, TRUE);
  gtk_widget_set_vexpand(button, TRUE);
  gtk_grid_attach(GTK_GRID(table), button, 1, 2, 1, 1);
#else
  gtk_table_attach_defaults(GTK_TABLE(table), button,
			    1, 2,
			    2, 3);
#endif
  gtk_widget_show(button);
  arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_NONE);
  gtk_container_add(GTK_CONTAINER(button), arrow);
  gtk_widget_show(arrow);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(olist_pref_down_button_clicked_cb), olist_entry);

  /* left button */
  button = gtk_button_new();
  olist_pref_win.left_button = button;
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_widget_set_hexpand(button, TRUE);
  gtk_widget_set_vexpand(button, TRUE);
  gtk_grid_attach(GTK_GRID(table), button, 0, 1, 1, 1);
#else
  gtk_table_attach_defaults(GTK_TABLE(table), button,
			    0, 1,
			    1, 2);
#endif
  gtk_widget_show(button);
  arrow = gtk_arrow_new(GTK_ARROW_LEFT, GTK_SHADOW_NONE);
  gtk_container_add(GTK_CONTAINER(button), arrow);
  gtk_widget_show(arrow);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(olist_pref_left_button_clicked_cb), olist_entry);

  /* right button */
  button = gtk_button_new();
  olist_pref_win.right_button = button;
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_widget_set_hexpand(button, TRUE);
  gtk_widget_set_vexpand(button, TRUE);
  gtk_grid_attach(GTK_GRID(table), button, 2, 1, 1, 1);
#else
  gtk_table_attach_defaults(GTK_TABLE(table), button,
			    2, 3,
			    1, 2);
#endif
  gtk_widget_show(button);
  arrow = gtk_arrow_new(GTK_ARROW_RIGHT, GTK_SHADOW_NONE);
  gtk_container_add(GTK_CONTAINER(button), arrow);
  gtk_widget_show(arrow);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(olist_pref_right_button_clicked_cb), olist_entry);

  /* right tree view */
#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
#else
  vbox = gtk_vbox_new(FALSE, 0);
#endif
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

  store = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);
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
						    (const gchar *)NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);

  g_object_unref(store);

  /* set value */
  olist_pref_tree_view_set_value(olist_entry, TRUE, TRUE);
  set_olist_buttons_sensitive(olist_entry);

  /* show dialog */
  gtk_widget_show(dialog);
}

static void
update_custom_type_olist_cb(void *ptr, const char *custom_sym)
{
  sync_value_olist(GTK_ENTRY(ptr));
}

static void
add_custom_type_orderedlist(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label, *entry, *button;

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
#else
  hbox = gtk_hbox_new(FALSE, 8);
#endif
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);

  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  entry = gtk_entry_new();
  gtk_editable_set_editable(GTK_EDITABLE(entry), FALSE);
  gtk_box_pack_start (GTK_BOX (hbox), entry, TRUE, TRUE, 0);

  button = gtk_button_new_with_label(_("Edit..."));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);

  g_object_set_data_full(G_OBJECT(entry),
			 OBJECT_DATA_UIM_CUSTOM_SYM, g_strdup(custom->symbol),
			 (GDestroyNotify) g_free);

  sync_value_olist(GTK_ENTRY(entry));

  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(choose_olist_clicked_cb), entry);
  uim_custom_cb_add(custom->symbol, entry, update_custom_type_olist_cb);
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
key_pref_set_value(gint ukey, gint umod)
{
  GString *keystr;
  const char *sym;

  keystr = g_string_new("");
  /*
   * Ignore Shift modifier for printable char keys for
   * easy-to-recognize key configuration.  uim-custom performs
   * implicit shift key encoding/decoding appropriately.
   */
  if (((ukey >= 256) || !g_ascii_isgraph(ukey)) &&
		  (umod & UMod_Shift))
    g_string_append(keystr, "<Shift>");
  if (umod & UMod_Control)
    g_string_append(keystr, "<Control>");
  if (umod & UMod_Alt)
    g_string_append(keystr, "<Alt>");
  if (umod & UMod_Meta)
    g_string_append(keystr, "<Meta>");
  if (umod & UMod_Super)
    g_string_append(keystr, "<Super>");
  if (umod & UMod_Hyper)
    g_string_append(keystr, "<Hyper>");

  switch (ukey) {
  case 0x20:
    /*
     * "space" is not proper uim keysym and only exists for user
     * convenience. It is converted to " " by uim-custom
     */
    g_string_append(keystr, "space");
    break;
  default:
#if 0
    if (keyval >= GDK_KP_0 && keyval <= GDK_KP_9) {
      g_string_append_printf(keystr, "%d", keyval - GDK_KP_0 + UKey_0);
#endif
    if (ukey < 128) {
      g_string_append_printf(keystr, "%c", ukey);
    } else if ((sym = uim_pref_get_keysym(ukey))) {
      g_string_append(keystr, sym);
    } else {
      /* UKey_Other */
    }
    break;
  }

  gtk_entry_set_text(GTK_ENTRY(key_pref_win.keycode_entry), keystr->str);

  key_pref_win.grabbed_key_val   = 0;
  key_pref_win.grabbed_key_state = 0;

  g_string_free(keystr, TRUE);
}

static gboolean
grab_win_key_press_cb (GtkWidget *widget, GdkEventKey *event,
		       GtkEntry *key_entry)
{
  im_uim_convert_keyevent(event, &key_pref_win.grabbed_key_val,
				 &key_pref_win.grabbed_key_state);

  return TRUE;
}

static gboolean
grab_win_key_release_cb (GtkWidget *widget, GdkEventKey *event,
			 GtkEntry *key_entry)
{
  key_pref_set_value(key_pref_win.grabbed_key_val,
		     key_pref_win.grabbed_key_state);

  im_uim_convert_keyevent(event, &key_pref_win.grabbed_key_val,
				 &key_pref_win.grabbed_key_state);

  g_signal_handlers_disconnect_by_func(G_OBJECT(widget),
				       (gpointer)(uintptr_t) grab_win_key_press_cb,
				       key_entry);
  g_signal_handlers_disconnect_by_func(G_OBJECT(widget),
				       (gpointer)(uintptr_t) grab_win_key_release_cb,
				       key_entry);

  gtk_dialog_response(GTK_DIALOG(widget), 0);

  return TRUE;
}

static gboolean
key_choose_entry_key_press_cb (GtkWidget *widget, GdkEventKey *event,
			       GtkEntry *key_entry)
{
  int ukey, umod;

  im_uim_convert_keyevent(event, &ukey, &umod);
  key_pref_set_value(ukey, umod);

  return TRUE;
}

static gboolean
key_choose_entry_key_release_cb (GtkWidget *widget, GdkEventKey *event,
			       GtkEntry *key_entry)
{
  int ukey, umod;

  im_uim_convert_keyevent(event, &ukey, &umod);

  return TRUE;
}

static void
choose_key_button_clicked_cb(GtkWidget *widget, GtkEntry *key_entry)
{
#if GTK_CHECK_VERSION(3, 0, 0)
  GdkDevice *device = gtk_get_current_event_device();
  GdkDevice *keyboard = gdk_device_get_associated_device(device);
#endif
  GtkWidget *dialog;

  dialog = gtk_message_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(widget)),
				  GTK_DIALOG_MODAL,
				  GTK_MESSAGE_INFO,
				  GTK_BUTTONS_CANCEL,
				  "%s", _("Press keys to grab (e.g. <Control>a)"));
  gtk_window_set_title(GTK_WINDOW(dialog), _("Grabbing keys"));
  g_signal_connect(G_OBJECT(dialog), "key-press-event",
		   G_CALLBACK(grab_win_key_press_cb), key_entry);
  g_signal_connect(G_OBJECT(dialog), "key-release-event",
		   G_CALLBACK(grab_win_key_release_cb), key_entry);

  gtk_widget_realize(dialog);
#if GTK_CHECK_VERSION(3, 0, 0)
  gdk_device_grab(keyboard, gtk_widget_get_window(GTK_WIDGET(dialog)),
                    GDK_OWNERSHIP_NONE, TRUE,
                    GDK_KEY_PRESS_MASK, NULL, GDK_CURRENT_TIME);
#else
  gdk_keyboard_grab(gtk_widget_get_window(GTK_WIDGET(dialog)),
		    TRUE, GDK_CURRENT_TIME);
#endif

  gtk_dialog_run(GTK_DIALOG(dialog));

  gtk_widget_destroy(dialog);
}

static void
sync_value_key(GtkEntry *entry)
{
  GString *str;
  const char *custom_sym;
  struct uim_custom *custom;
  struct uim_custom_key *key;
  gint i;

  custom_sym = g_object_get_data(G_OBJECT(entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_Key);

  str = g_string_new("");

  if (custom->value->as_key) {
    for (key = custom->value->as_key[0], i = 0;
	 key;
	 key = custom->value->as_key[++i])
    {
      if (i != 0)
	g_string_append(str, ", ");
      g_string_append_printf(str, "\"%s\"", key->literal);
    }
  } else {
    /* error message */
  }

  gtk_entry_set_text(GTK_ENTRY(entry), str->str);
  gtk_widget_set_sensitive(gtk_widget_get_parent(GTK_WIDGET(entry)),
      custom->is_active);

  g_string_free(str, TRUE);
  uim_custom_free(custom);
}

static void
key_pref_add_button_clicked_cb(GtkWidget *widget, GtkEntry *key_entry)
{
  const char *custom_sym;
  struct uim_custom *custom;
  const char *key_code;
  GString *str;
  GtkTreeModel *model;
  GtkTreeIter iter;
  gint num;
  uim_bool rv;

  g_return_if_fail(GTK_IS_ENTRY(key_entry));

  custom_sym = g_object_get_data(G_OBJECT(key_entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_Key);

  key_code = gtk_entry_get_text(GTK_ENTRY(key_pref_win.keycode_entry));
  if (!key_code || !*key_code) {
    uim_custom_free(custom);
    return;
  }

  str = g_string_new("");

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
    uim_pref_gtk_mark_value_changed();
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(key_pref_win.tree_view));
    gtk_list_store_append(GTK_LIST_STORE(model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(model), &iter,
		       0, str->str,
		       -1);
    sync_value_key(key_entry);
  } else {
    g_printerr("Failed to set key value for \"%s\".\n", custom->symbol);
  }

  g_string_free(str, TRUE);
  uim_custom_free(custom);
}

static void
key_pref_remove_button_clicked_cb(GtkWidget *widget, GtkEntry *key_entry)
{
  const char *custom_sym;
  struct uim_custom *custom;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GtkTreePath *path;
  gboolean selected;
  gint num, *indices;
  uim_bool rv;

  g_return_if_fail(GTK_IS_ENTRY(key_entry));

  custom_sym = g_object_get_data(G_OBJECT(key_entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_Key);

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(key_pref_win.tree_view));

  selected = gtk_tree_selection_get_selected(selection, &model, &iter);
  if (!selected) {
    uim_custom_free(custom);
    return;
  }

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
      uim_pref_gtk_mark_value_changed();
      gtk_list_store_remove(GTK_LIST_STORE(model), &iter);

      sync_value_key(key_entry);
    } else {
      g_printerr("Failed to set key value for \"%s\".\n", custom->symbol);
    }
  } else {
    /* error */
  }

  gtk_tree_path_free(path);

  selected = gtk_tree_selection_get_selected(selection, &model, &iter);
  gtk_widget_set_sensitive(key_pref_win.remove_button, selected);

  uim_custom_free(custom);
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
  GtkWidget *dialog, *scrwin, *view, *hbox, *vbox, *label, *button, *entry;
  GtkTreeSelection *selection;
  GtkListStore *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  const char *custom_sym;
  struct uim_custom *custom;
  struct uim_custom_key *key;
  gint i;
  gchar title[256];

  g_return_if_fail(GTK_IS_ENTRY(key_entry));

  custom_sym = g_object_get_data(G_OBJECT(key_entry), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_Key);

  g_snprintf(title, sizeof(title), _("%s - key configuration"), custom->label);

  /* setup key pref dialog */
  dialog = gtk_dialog_new_with_buttons(
    title,
    GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(key_entry))),
    GTK_DIALOG_MODAL,
    GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
    NULL);
  key_pref_win.window = dialog;
  gtk_window_set_default_size(GTK_WINDOW(dialog),
			      DEFAULT_KEYCONF_WINDOW_WIDTH,
			      DEFAULT_KEYCONF_WINDOW_HEIGHT);
  g_signal_connect(G_OBJECT(dialog), "response",
		   G_CALLBACK(key_pref_dialog_response_cb), key_entry);

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
#else
  hbox = gtk_hbox_new(FALSE, 0);
#endif
  gtk_container_set_border_width(GTK_CONTAINER(hbox), 4);
  gtk_box_pack_start(GTK_BOX(gtk_dialog_get_content_area(GTK_DIALOG(dialog))),
      hbox, TRUE, TRUE, 0);
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
						    (const gchar *)NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(view), column);

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);
  g_signal_connect (G_OBJECT(selection), "changed",
		    G_CALLBACK(key_pref_selection_changed), key_entry);

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
#else
  vbox = gtk_vbox_new(TRUE, 0);
#endif
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

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
#else
  hbox = gtk_hbox_new(FALSE, 0);
#endif
  gtk_box_pack_start(GTK_BOX(gtk_dialog_get_content_area(GTK_DIALOG(dialog))),
      hbox, FALSE, FALSE, 0);
  gtk_container_set_border_width(GTK_CONTAINER(hbox), 4);
  gtk_widget_show(hbox);

  label = gtk_label_new(_("Key:"));
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 4);
  gtk_widget_show(label);

  entry = gtk_entry_new();
  gtk_editable_set_editable(GTK_EDITABLE(entry), FALSE);
  key_pref_win.keycode_entry = entry;
  gtk_widget_set_size_request(GTK_WIDGET(entry), 100, -1);
  gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 4);
  g_signal_connect(G_OBJECT(entry), "changed",
		   G_CALLBACK(key_val_entry_changed_cb), key_entry);
  g_signal_connect(G_OBJECT(entry), "key-press-event",
		   G_CALLBACK(key_choose_entry_key_press_cb), key_entry);
  g_signal_connect(G_OBJECT(entry), "key-release-event",
		   G_CALLBACK(key_choose_entry_key_release_cb), key_entry);
  gtk_widget_show(entry);

  button = gtk_button_new_with_label(_("Grab..."));
  gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 4);
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
  uim_custom_free(custom);
}

static void
update_custom_type_key_cb(void *ptr, const char *custom_sym)
{
  sync_value_key(GTK_ENTRY(ptr));
}

static void
add_custom_type_key(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label, *entry, *button;

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
#else
  hbox = gtk_hbox_new(FALSE, 8);
#endif
  label = gtk_label_new(custom->label);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  entry = gtk_entry_new();
  gtk_editable_set_editable(GTK_EDITABLE(entry), FALSE);
  g_object_set_data_full(G_OBJECT(entry),
			 OBJECT_DATA_UIM_CUSTOM_SYM, g_strdup(custom->symbol),
			 (GDestroyNotify) g_free);
  gtk_box_pack_start (GTK_BOX (hbox), entry, TRUE, TRUE, 0);

  sync_value_key(GTK_ENTRY(entry));

  button = gtk_button_new_with_label(_("Edit..."));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(choose_key_clicked_cb), entry);
  uim_custom_cb_add(custom->symbol, entry, update_custom_type_key_cb);

  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
}

static void
table_pref_dialog_response_cb(GtkDialog *dialog, gint action,
                              GtkWidget *tree_view)
{
  gpointer table_label = g_object_get_data(G_OBJECT(tree_view), "label");
  const char *custom_sym;
  struct uim_custom *custom;
  char ***custom_table;
  GtkTreeModel *model;
  gint n_rows;
  int row;
  int column;
  gint n_columns;
  GtkTreePath *path;
  GtkTreeIter iter;
  uim_bool rv;

  custom_sym = g_object_get_data(G_OBJECT(table_label),
                                 OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_Table);

  custom_table = custom->value->as_table;
  for (row = 0; custom_table[row]; row++) {
    for (column = 0; custom_table[row][column]; column++) {
      free(custom_table[row][column]);
    }
    free(custom_table[row]);
  }

  model = gtk_tree_view_get_model(GTK_TREE_VIEW(tree_view));
  n_rows = gtk_tree_model_iter_n_children(model, NULL);
  custom_table = (char ***)malloc(sizeof(char **) * (n_rows + 1));
  custom_table[n_rows] = NULL;

  n_columns = gtk_tree_model_get_n_columns(model);

  path = gtk_tree_path_new_first();
  gtk_tree_model_get_iter(model, &iter, path);
  gtk_tree_path_free(path);

  for (row = 0; row < n_rows; row++) {
    int n_columnsForRow = n_columns;
    /* the number of column may differ from row to row */
    for (column = 0; column < n_columns; column++) {
      GValue value = {0, };
      gtk_tree_model_get_value(model, &iter, column, &value);
      if (!g_value_get_string(&value)) {
        n_columnsForRow = column;
        break;
      }
    }
    custom_table[row]
        = (char **)malloc(sizeof(char *) * (n_columnsForRow + 1));
    custom_table[row][n_columnsForRow] = NULL;
    for (column = 0; column < n_columnsForRow; column++) {
      GValue value = {0, };
      const gchar *str;
      gtk_tree_model_get_value(model, &iter, column, &value);
      str = g_value_get_string(&value);
      custom_table[row][column] = strdup(str ? str : "");
    }
    gtk_tree_model_iter_next(model, &iter);
  }
  custom->value->as_table = custom_table;

  rv = uim_custom_set(custom);

  if (!rv) {
    g_printerr("Failed to set table value for \"%s\".\n", custom->symbol);
    /* FIXME! reset the widget */
  }
  uim_custom_free(custom);

  gtk_widget_destroy(GTK_WIDGET(dialog));
}

static void
sync_value_table(GtkLabel *label)
{
  const char *custom_sym;
  struct uim_custom *custom;

  g_return_if_fail(label);

  custom_sym = g_object_get_data(G_OBJECT(label), OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_Table);

  gtk_widget_set_sensitive(gtk_widget_get_parent(GTK_WIDGET(label)),
                           custom->is_active);
  uim_custom_free(custom);
}

static void
table_pref_renderer_edited(GtkCellRendererText *renderer,
                           gchar *path,
                           gchar *new_text,
                           GtkTreeView *tree_view)
{
  GtkTreeModel *model = gtk_tree_view_get_model(tree_view);
  GtkTreePath *tree_path = gtk_tree_path_new_from_string(path);
  GtkTreeIter iter;
  gint column = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(renderer),
                                "column"));
  gint num = gtk_tree_model_iter_n_children(model, NULL);

  if (column < 0 || column >= num) {
    gtk_tree_path_free(tree_path);
    return;
  }

  if (!gtk_tree_model_get_iter(model, &iter, tree_path))
    return;

  gtk_tree_path_free(tree_path);
  gtk_list_store_set(GTK_LIST_STORE(model), &iter, column,
                     new_text, -1);
  uim_pref_gtk_mark_value_changed();
}

static GtkWidget*
create_table_tree_view(struct uim_custom *custom)
{
  GtkTreeSelection *selection;
  char ***custom_table;
  gint n_columns;
  GType *types;
  GtkListStore *list_store;
  GtkTreeIter iter;
  int row;
  int column;
  struct uim_custom_choice **item;
  GtkWidget *tree_view = gtk_tree_view_new();
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
  gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);

  custom_table = custom->value->as_table;
  /* the number of column may differ from row to row */
  n_columns = -1;
  for (row = 0; custom_table[row]; row++) {
    for (column = 0; custom_table[row][column]; column++) {
      if (n_columns < column)
        n_columns = column;
    }
  }
  n_columns++;

  types = g_new0(GType, n_columns);
  for (column = 0; column < n_columns; column++) {
    types[column] = G_TYPE_STRING;
  }
  list_store = gtk_list_store_newv(n_columns, types);
  for (row = 0; custom_table[row]; row++) {
    gboolean expanded = FALSE;
    gtk_list_store_append(list_store, &iter);
    for (column = 0; column < n_columns; column++) {
      GValue value = {0, };
      if (!custom_table[row][column])
        expanded = TRUE;
      g_value_init(&value, G_TYPE_STRING);
      g_value_set_string(&value, expanded ? NULL : custom_table[row][column]);
      gtk_list_store_set_value(list_store, &iter, column, &value);
    }
  }
  gtk_tree_view_set_model(GTK_TREE_VIEW(tree_view), GTK_TREE_MODEL(list_store));

  g_object_unref(list_store);
  g_free(types);

  column = 0;
  for (item = custom->range->as_table_header.valid_items;
          *item; item++) {
    GtkCellRenderer *renderer = gtk_cell_renderer_text_new();
    g_object_set (renderer,
                  "editable", TRUE,
                  NULL);
    g_object_set_data(G_OBJECT(renderer), "column", GINT_TO_POINTER(column));
    gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(tree_view),
                                                -1, (*item)->label,
                                                renderer,
                                                "text", column, NULL);

    g_signal_connect(G_OBJECT(renderer), "edited",
                     G_CALLBACK(table_pref_renderer_edited),
                     GTK_TREE_VIEW(tree_view));
    column++;
  }
  gtk_widget_show(tree_view);
  return tree_view;
}

static void
table_pref_add_button_clicked_cb(GtkWidget *widget, GtkTreeView *tree_view)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  gint n_columns;
  int column;
  GtkTreePath *path;

  model = gtk_tree_view_get_model(tree_view);
  gtk_list_store_append(GTK_LIST_STORE(model), &iter);

  n_columns = gtk_tree_model_get_n_columns(model);
  for (column = 0; column < n_columns; column++) {
    GValue value = {0, };
    g_value_init(&value, G_TYPE_STRING);
    g_value_set_static_string(&value, "");
    gtk_list_store_set_value(GTK_LIST_STORE(model), &iter, column, &value);
  }

  path = gtk_tree_model_get_path(model, &iter);
  gtk_tree_view_scroll_to_cell(tree_view, path, NULL,
                               FALSE, 0.0, 0.0);
  gtk_tree_path_free(path);
  uim_pref_gtk_mark_value_changed();
}

static void
table_pref_remove_button_clicked_cb(GtkWidget *widget, GtkTreeView *tree_view)
{
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter;
  gboolean selected;
  GtkTreePath *path;
  gint *indices;
  gint num;

  selection = gtk_tree_view_get_selection(tree_view);

  selected = gtk_tree_selection_get_selected(selection, &model, &iter);
  if (!selected) {
    return;
  }

  path = gtk_tree_model_get_path(model, &iter);
  indices = gtk_tree_path_get_indices(path);
  num = gtk_tree_model_iter_n_children(model, NULL);

  if (num < 0 || *indices >= num) {
    gtk_tree_path_free(path);
    return;
  }

  gtk_list_store_remove(GTK_LIST_STORE(model), &iter);

  gtk_tree_path_free(path);
  uim_pref_gtk_mark_value_changed();
}

static void
table_pref_move_button_clicked_cb(GtkWidget *widget, GtkTreeView *tree_view,
                                  gboolean up)
{
  GtkTreeSelection *selection;
  gint num;
  GtkTreeModel *model;
  GList *rows = NULL;
  GtkTreePath *path;
  gboolean rv;
  GtkTreeIter iter1;
  GtkTreeIter iter2;

  selection = gtk_tree_view_get_selection(tree_view);
  num = gtk_tree_selection_count_selected_rows(selection);
  if (num != 1)
    return;

  rows = gtk_tree_selection_get_selected_rows(selection, &model);
  path = rows->data;

  rv = gtk_tree_model_get_iter(model, &iter1, path);
  if (!rv)
    goto ERROR;

  if (up) {
    rv = gtk_tree_path_prev(path);
    if (!rv)
      goto ERROR;
  } else {
    gtk_tree_path_next(path);
  }

  /* sync the view */
  rv = gtk_tree_model_get_iter(model, &iter2, path);
  if (!rv)
    goto ERROR;

  gtk_list_store_swap(GTK_LIST_STORE(model), &iter1, &iter2);
  path = gtk_tree_model_get_path(model, &iter1);
  gtk_tree_view_scroll_to_cell(tree_view, path, NULL,
                               FALSE, 0.0, 0.0);
  gtk_tree_path_free(path);
  uim_pref_gtk_mark_value_changed();

ERROR:
  g_list_foreach(rows, (GFunc)gtk_tree_path_free, NULL);
  g_list_free(rows);

}

static void
table_pref_up_button_clicked_cb(GtkWidget *widget, GtkTreeView *tree_view)
{
  table_pref_move_button_clicked_cb(widget, tree_view, TRUE);
}

static void
table_pref_down_button_clicked_cb(GtkWidget *widget, GtkTreeView *tree_view)
{
  table_pref_move_button_clicked_cb(widget, tree_view, FALSE);
}

static void
create_table_button(GtkWidget* vbox, const char *item,
                    void (func)(GtkWidget *, GtkTreeView *),
                    GtkWidget *tree_view)
{
  GtkWidget *button = gtk_button_new_from_stock(item);
  gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 2);
  g_signal_connect(G_OBJECT(button), "clicked",
                   G_CALLBACK(func), GTK_TREE_VIEW(tree_view));
  gtk_widget_show(button);
}

static void
choose_table_clicked_cb(GtkWidget *widget, GtkWidget *table_label)
{
  GtkWidget *dialog;
  GtkWidget *hbox;
  const char *custom_sym;
  struct uim_custom *custom;
  GtkWidget *tree_view;
  GtkWidget *scrwin;
  GtkWidget *vbox;

  custom_sym = g_object_get_data(G_OBJECT(table_label),
                                 OBJECT_DATA_UIM_CUSTOM_SYM);
  g_return_if_fail(custom_sym);

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom && custom->type == UCustom_Table);

  dialog = gtk_dialog_new_with_buttons(
    dgettext(GETTEXT_PACKAGE, custom->label),
    GTK_WINDOW(gtk_widget_get_toplevel(table_label)),
    GTK_DIALOG_MODAL,
    GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
    NULL);
  gtk_window_set_default_size(GTK_WINDOW(dialog),
                              DEFAULT_TABLE_WINDOW_WIDTH,
                              DEFAULT_TABLE_WINDOW_HEIGHT);

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
#else
  hbox = gtk_hbox_new(FALSE, 0);
#endif
  gtk_container_set_border_width(GTK_CONTAINER(hbox), 4);
  gtk_box_pack_start(GTK_BOX(gtk_dialog_get_content_area(GTK_DIALOG(dialog))),
      hbox, TRUE, TRUE, 0);
  gtk_widget_show(hbox);

  tree_view = create_table_tree_view(custom);
  g_object_set_data(G_OBJECT(tree_view),
                    "label", table_label);
  g_signal_connect(G_OBJECT(dialog), "response",
                   G_CALLBACK(table_pref_dialog_response_cb), tree_view);

  scrwin = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrwin),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrwin),
                                      GTK_SHADOW_IN);
  gtk_box_pack_start(GTK_BOX(hbox), scrwin,
                     TRUE, TRUE, 0);
  gtk_widget_show(scrwin);

  gtk_container_add(GTK_CONTAINER(scrwin), tree_view);

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_button_box_new(GTK_ORIENTATION_VERTICAL);
#else
  vbox = gtk_vbutton_box_new();
#endif
  gtk_button_box_set_layout(GTK_BUTTON_BOX(vbox), GTK_BUTTONBOX_SPREAD);
  gtk_box_pack_start(GTK_BOX(hbox), vbox, FALSE, FALSE, 4);
  gtk_widget_show(vbox);

  create_table_button(vbox, GTK_STOCK_ADD,
                      table_pref_add_button_clicked_cb, tree_view);
  create_table_button(vbox, GTK_STOCK_REMOVE,
                      table_pref_remove_button_clicked_cb, tree_view);
  create_table_button(vbox, GTK_STOCK_GO_UP,
                      table_pref_up_button_clicked_cb, tree_view);
  create_table_button(vbox, GTK_STOCK_GO_DOWN,
                      table_pref_down_button_clicked_cb, tree_view);

  gtk_widget_show(dialog);

  uim_custom_free(custom);
}

static void
update_custom_type_table_cb(void *ptr, const char *custom_sym)
{
  sync_value_table(GTK_LABEL(ptr));
}

static void
add_custom_type_table(GtkWidget *vbox, struct uim_custom *custom)
{
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *button;

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
#else
  hbox = gtk_hbox_new(FALSE, 8);
#endif

  label = gtk_label_new(custom->label);
  g_object_set_data_full(G_OBJECT(label),
                         OBJECT_DATA_UIM_CUSTOM_SYM, g_strdup(custom->symbol),
                         (GDestroyNotify) g_free);
  uim_custom_cb_add(custom->symbol, label, update_custom_type_table_cb);
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);

  button = gtk_button_new_with_label(_("Edit..."));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
  g_signal_connect(G_OBJECT(button), "clicked",
                   G_CALLBACK(choose_table_clicked_cb), label);

  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);

  sync_value_table(GTK_LABEL(label));
}



void
uim_pref_gtk_add_custom(GtkWidget *vbox, const char *custom_sym)
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
    case UCustom_Table:
      add_custom_type_table(vbox, custom);
      break;
    default:
      g_printerr("Invalid custom type: %d\n", custom->type);
      break;
    }
  } else {
    g_printerr("Failed to get uim_custom object for %s.\n", custom_sym);
  }

  uim_custom_free(custom);
}

void
uim_pref_gtk_set_default_value(GtkWidget *widget)
{
  const char *custom_sym;
  struct uim_custom *custom;
  union uim_custom_value *value, *defval;
  uim_bool rv;
  gint i, num;
  int type;

  custom_sym = g_object_get_data(G_OBJECT(widget), OBJECT_DATA_UIM_CUSTOM_SYM);
  if (!custom_sym) return;

  custom = uim_custom_get(custom_sym);
  g_return_if_fail(custom);

  value = custom->value;
  defval = custom->default_value;
  type = custom->type;

  switch (type) {
  case UCustom_Bool:
    value->as_bool = defval->as_bool;
    break;
  case UCustom_Int:
    value->as_int = defval->as_int;
    break;
  case UCustom_Str:
    free(value->as_str);
    value->as_str = strdup(defval->as_str);
    break;
  case UCustom_Pathname:
    free(value->as_pathname->str);
    value->as_pathname->str = strdup(defval->as_pathname->str);
    value->as_pathname->type = defval->as_pathname->type;
    break;
  case UCustom_Choice:
    free(value->as_choice->symbol);
    free(value->as_choice->label);
    free(value->as_choice->desc);
    value->as_choice->symbol = strdup(defval->as_choice->symbol);
    value->as_choice->label  = strdup(defval->as_choice->label);
    value->as_choice->desc   = strdup(defval->as_choice->desc);
    break;
  case UCustom_OrderedList:
    for (num = 0; defval->as_olist[num]; num++);
    for (i = 0; value->as_olist[i]; i++) {
      free(value->as_olist[i]->symbol);
      free(value->as_olist[i]->label);
      free(value->as_olist[i]->desc);
      free(value->as_olist[i]);
    }
    value->as_olist = realloc(value->as_olist,
			      sizeof(struct uim_custom_choice *) * (num + 1));
    for (i = 0; i < num; i++) {
      value->as_olist[i] = malloc(sizeof(struct uim_custom_choice));
      value->as_olist[i]->symbol = strdup(defval->as_olist[i]->symbol);
      value->as_olist[i]->label  = strdup(defval->as_olist[i]->label);
      value->as_olist[i]->desc   = strdup(defval->as_olist[i]->desc);
    }
    value->as_olist[num] = NULL;
    break;
  case UCustom_Key:
    for (num = 0; defval->as_key[num]; num++);
    for (i = 0; value->as_key[i]; i++) {
      free(value->as_key[i]->literal);
      free(value->as_key[i]->label);
      free(value->as_key[i]->desc);
      free(value->as_key[i]);
    }
    value->as_key = realloc(value->as_key,
			    sizeof(struct uim_custom_key *) * (num + 1));
    for (i = 0; i < num; i++) {
      value->as_key[i] = malloc(sizeof(struct uim_custom_key));
      *value->as_key[i] = *defval->as_key[i];
      value->as_key[i]->literal = strdup(defval->as_key[i]->literal);
      value->as_key[i]->label   = strdup(defval->as_key[i]->label);
      value->as_key[i]->desc    = strdup(defval->as_key[i]->desc);
    }
    value->as_key[num] = NULL;
    break;
  case UCustom_Table:
    {
      char ***custom_table = value->as_table;
      char ***default_table = defval->as_table;
      int row;
      int column;
      for (row = 0; custom_table[row]; row++) {
        for (column = 0; custom_table[row][column]; column++) {
          free(custom_table[row][column]);
        }
        free(custom_table[row]);
      }
      default_table = defval->as_table;
      for (row = 0; default_table[row]; row++)
        ;
      custom_table = (char ***)malloc(sizeof(char **) * (row + 1));
      custom_table[row] = 0;
  
      value->as_table = custom_table;
  
      for (row = 0; default_table[row]; row++) {
        /* the number of column may differ from row to row */
        for (column = 0; default_table[row][column]; column++)
          ;
        custom_table[row] = (char **)malloc(sizeof(char *) * (column + 1));
        custom_table[row][column] = 0;
        for (column = 0; default_table[row][column]; column++)
          custom_table[row][column] = strdup(default_table[row][column]);
      }
    }
    break;
  default:
    uim_custom_free(custom);
    return;
  }

  rv = uim_custom_set(custom);
  if (rv == UIM_FALSE) {
    g_printerr("Failed to set value for \"%s\".\n", custom->symbol);
    uim_custom_free(custom);
    return;
  }
  uim_custom_free(custom);

  switch (type) {
  case UCustom_Bool:
    sync_value_bool(GTK_CHECK_BUTTON(widget));
    break;
  case UCustom_Int:
    sync_value_int(GTK_SPIN_BUTTON(widget));
    break;
  case UCustom_Str:
    sync_value_string(GTK_ENTRY(widget));
    break;
  case UCustom_Pathname:
    sync_value_string(GTK_ENTRY(widget));
    break;
  case UCustom_Choice:
    sync_value_choice(GTK_COMBO_BOX(widget));
    break;
  case UCustom_OrderedList:
    sync_value_olist(GTK_ENTRY(widget));
    break;
  case UCustom_Key:
    sync_value_key(GTK_ENTRY(widget));
    break;
  case UCustom_Table:
    sync_value_table(GTK_LABEL(widget));
    break;
  default:
    break;
  }
  return;
}
