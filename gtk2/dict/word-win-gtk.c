/*

  Copyright (c) 2004-2013 uim Project https://github.com/uim/uim

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

#include <stdlib.h>
#include <string.h>
#include <gtk/gtk.h>

#include "gettext.h"

#include "util.h"
#include "anthy.h"
#include "word-win-gtk.h"
#include "cclass-dialog.h"

enum {
  WORD_ADDED_SIGNAL,
  LAST_SIGNAL
};

static char *pos_broad[] = {
  N_("Substantive"),
  N_("Verb"),
  N_("Adjective"),
  N_("Adverb"),
  N_("Etc"),
};


static void word_window_class_init  (WordWindowClass *window);
static void word_window_init        (WordWindow      *window);

static GtkWidget *word_window_necessary_create   (WordWindow  *window);
static GtkWidget *word_window_additional_create  (WordWindow  *window);
static void       word_window_add                (WordWindow  *window);
static gboolean   word_window_validate_values    (WordWindow  *window);
static void       word_window_cclass_reset       (WordWindow *window);

static gboolean   idle_wordwin_destroy           (gpointer data);

/* calbacks for this */
static void word_window_response            (GtkDialog   *dialog,
					     gint         arg);
static void message_dialog_response_cb      (GtkDialog   *dialog,
					     gint         arg,
					     WordWindow  *window);
static void cclass_combobox_changed_cb      (GtkComboBox *combobox,
					     WordWindow  *window);
static void button_cclass_browse_clicked_cb (GtkButton   *button,
					     WordWindow  *window);

static gint word_window_signals[LAST_SIGNAL] = {0};


GType
word_window_get_type(void) {
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(WordWindowClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc)word_window_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(WordWindow),
      0, /* n_preallocs */
      (GInstanceInitFunc)word_window_init /* instance_init */
    };
    type = g_type_register_static(GTK_TYPE_DIALOG, "WordWindow", &info, 0);
  }
  return type;
}

static void
word_window_class_init (WordWindowClass *klass)
{
  GtkDialogClass *dialog_class = (GtkDialogClass *) klass;

  word_window_signals[WORD_ADDED_SIGNAL]
    = g_signal_new ("word-added",
		    G_TYPE_FROM_CLASS(klass),
		    G_SIGNAL_RUN_FIRST,
		    G_STRUCT_OFFSET(WordWindowClass, word_added),
		    NULL, NULL,
		    g_cclosure_marshal_VOID__VOID,
		    G_TYPE_NONE, 0);

  /* GtkDialog class */
  dialog_class->response = word_window_response;

  /* WordWindow class */
  klass->word_added = NULL;
}

static void
word_window_init(WordWindow *window)
{
  GtkWidget *hbox;
  GtkWidget *vbox1, *vbox2;
  GtkWidget *label;
  GtkWidget *alignment, *table, *check;
#if 0
  GtkWidget *frame;
  GtkWidget *button_add, *button_clear;
#endif

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
#else
  vbox1 = gtk_vbox_new(FALSE, 10);
#endif
  gtk_container_set_border_width(GTK_CONTAINER(vbox1), 10);

  /* Necessary infomation area */
#if GTK_CHECK_VERSION(3, 2, 0)
  vbox2 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);
#else
  vbox2 = gtk_vbox_new(FALSE, 6);
#endif
  gtk_box_pack_start(GTK_BOX(vbox1), vbox2, TRUE, TRUE, 0);

  label = gtk_label_new(_("Necessary infomation"));
  gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
  gtk_box_pack_start(GTK_BOX(vbox2), label, TRUE, TRUE, 0);

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
#else
  hbox = gtk_hbox_new(FALSE, 0);
#endif
  gtk_box_pack_start(GTK_BOX(vbox2), hbox, TRUE, TRUE, 0);

  label = gtk_label_new("     ");
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

  alignment = gtk_alignment_new(0.0, 0.5, 0.0, 0.0);
  gtk_box_pack_start(GTK_BOX(hbox), alignment, TRUE, TRUE, 0);

  gtk_container_add(GTK_CONTAINER(alignment),
		    word_window_necessary_create(window));

  /* Additional infomation area */
#if GTK_CHECK_VERSION(3, 2, 0)
  vbox2 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);
#else
  vbox2 = gtk_vbox_new(FALSE, 6);
#endif
  gtk_box_pack_start(GTK_BOX(vbox1), vbox2, TRUE, TRUE, 0);

  label = gtk_label_new (_("Additional infomation"));
  gtk_misc_set_alignment(GTK_MISC(label), 0.0, 1.0);
  gtk_box_pack_start(GTK_BOX(vbox2), label, TRUE, TRUE, 0);

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
#else
  hbox = gtk_hbox_new(FALSE, 0);
#endif
  gtk_box_pack_start(GTK_BOX(vbox2), hbox, TRUE, TRUE, 0);

  label = gtk_label_new("     ");
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

  table = word_window_additional_create(window);
  gtk_box_pack_start(GTK_BOX(hbox), table, TRUE, TRUE, 0);

  /* action area */
  check = gtk_check_button_new_with_label(_("continuance"));
  gtk_box_pack_start(GTK_BOX(gtk_dialog_get_action_area(GTK_DIALOG(window))),
		     check, FALSE, FALSE, 0);
  gtk_widget_show(check);
  window->continuance = check;

#if 0
  vbox2 = gtk_vbutton_box_new();
  gtk_button_box_set_layout(GTK_BUTTON_BOX(vbox2), GTK_BUTTONBOX_START);
  gtk_box_set_spacing(GTK_BOX(vbox2), 10);
  gtk_widget_show(vbox2);
  gtk_box_pack_start(GTK_BOX(hbox), vbox2, FALSE, FALSE, 10);

  button_add = gtk_button_new_with_label(_("Add"));
  gtk_box_pack_start(GTK_BOX(vbox2), button_add, FALSE, FALSE, 0);
  g_signal_connect(G_OBJECT(button_add), "clicked",
		   G_CALLBACK(add_button_clicked_cb), window);
  gtk_widget_show(button_add);

  button_clear = gtk_button_new_with_label(_("Clear"));
  gtk_box_pack_start(GTK_BOX(vbox2), button_clear, FALSE, FALSE, 0);
  g_signal_connect(G_OBJECT(button_clear), "clicked",
		   G_CALLBACK(clear_button_clicked_cb), window);
  gtk_widget_show(button_clear);
#endif

  gtk_window_set_title(GTK_WINDOW(window), _("Add a word"));
  gtk_dialog_add_button(GTK_DIALOG(window),
			GTK_STOCK_CLEAR, WORD_WINDOW_RESPONSE_CLEAR);
  gtk_dialog_add_buttons(GTK_DIALOG(window),
			 GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
			 GTK_STOCK_ADD,WORD_WINDOW_RESPONSE_ADD,
			 NULL);
  gtk_box_pack_start(GTK_BOX(gtk_dialog_get_content_area(GTK_DIALOG(window))),
      vbox1, TRUE, TRUE, 0);
  gtk_widget_show_all(vbox1);

  window->pane = GTK_WIDGET(vbox1);
#if 0
  window->add = GTK_WIDGET(button_add);
  window->clear = GTK_WIDGET(button_clear);
#else
  window->add   = NULL;
  window->clear = NULL;
#endif

  window->dict = NULL;
}

GtkWidget *
word_window_new(WordWindowType mode, uim_dict *dict)
{
  GtkWidget *widget;
  gchar title[256];
  gint type;

  g_return_val_if_fail(dict, NULL);

  widget = GTK_WIDGET(g_object_new(WORD_WINDOW_TYPE, NULL));

#if 1 /* should be moved to word_window_construct() */
  if (dict)
    WORD_WINDOW(widget)->dict = uim_dict_ref(dict);

  word_window_cclass_reset(WORD_WINDOW(widget));

  if (mode == WORD_WINDOW_MODE_EDIT) {
    g_snprintf(title, sizeof(title), _("Edit the word (%s)"),
	       _(dict->identifier));
    gtk_widget_hide(WORD_WINDOW(widget)->continuance);
  } else {
    g_snprintf(title, sizeof(title), _("Add a word (%s)"),
	       _(dict->identifier));
  }

  gtk_window_set_title(GTK_WINDOW(widget), title);
  type = dict_identifier_to_word_type(dict->identifier);
  if (type == WORD_TYPE_CANNA)
    gtk_widget_set_sensitive(WORD_WINDOW(widget)->freq, FALSE);
#endif

  return widget;
}

static GtkWidget *
word_window_necessary_create(WordWindow *window)
{
  GtkWidget *label;
  GtkWidget *table1;
  GtkWidget *entry_phon, *entry_desc;
  GtkWidget *combobox_pos_broad;
  GtkAdjustment *adjustment_freq;
  GtkWidget *alignment, *spin_freq;
  int i;

#if GTK_CHECK_VERSION(3, 4, 0)
  table1 = gtk_grid_new();
  gtk_grid_set_row_spacing(GTK_GRID(table1), 5);
  gtk_grid_set_column_spacing(GTK_GRID(table1), 5);
#else
  table1 = gtk_table_new(5, 5, FALSE);
#endif

  label = gtk_label_new_with_mnemonic(_("_Phonetic:"));
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_widget_set_hexpand(label, TRUE);
  gtk_grid_attach(GTK_GRID(table1), label, 0, 0, 1, 1);
#else
  gtk_table_attach(GTK_TABLE(table1), label, 0, 1, 0, 1, GTK_FILL, 0, 5, 5);
#endif

  entry_phon = gtk_entry_new();
  gtk_label_set_mnemonic_widget(GTK_LABEL(label), entry_phon);
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_grid_attach(GTK_GRID(table1), entry_phon, 1, 0, 1, 1);
#else
  gtk_table_attach(GTK_TABLE(table1), entry_phon, 1, 2, 0, 1, 0, 0, 5, 5);
#endif

  label = gtk_label_new_with_mnemonic(_("_Literal:"));
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_widget_set_hexpand(label, TRUE);
  gtk_grid_attach(GTK_GRID(table1), label, 0, 1, 1, 1);
#else
  gtk_table_attach(GTK_TABLE(table1), label, 0, 1, 1, 2, GTK_FILL, 0, 5, 5);
#endif

  entry_desc = gtk_entry_new();
  gtk_label_set_mnemonic_widget(GTK_LABEL(label), entry_desc);
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_grid_attach(GTK_GRID(table1), entry_desc, 1, 1, 1, 1);
#else
  gtk_table_attach(GTK_TABLE(table1), entry_desc, 1, 2, 1, 2, 0, 0, 5, 5);
#endif

  label = gtk_label_new_with_mnemonic(_("Part of _Speech:"));
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_widget_set_hexpand(label, TRUE);
  gtk_grid_attach(GTK_GRID(table1), label, 0, 2, 1, 1);
#else
  gtk_table_attach(GTK_TABLE(table1), label, 0, 1, 2, 3, GTK_FILL, 0, 5, 5);
#endif
  {
    GtkWidget *alignment_pos_broad;
    gint pos_num;

    pos_num = sizeof(pos_broad) / sizeof(pos_broad[0]);
#if GTK_CHECK_VERSION(2, 24, 0)
    combobox_pos_broad = gtk_combo_box_text_new();
#else
    combobox_pos_broad = gtk_combo_box_new_text();
#endif
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), combobox_pos_broad);
    gtk_widget_show(combobox_pos_broad);

    for (i = 0; i < pos_num; i++) {
#if GTK_CHECK_VERSION(2, 24, 0)
      gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combobox_pos_broad),
				_(pos_broad[i]));
#else
      gtk_combo_box_append_text(GTK_COMBO_BOX(combobox_pos_broad),
				_(pos_broad[i]));
#endif
    }

    alignment_pos_broad = gtk_alignment_new(0, 0.5, 0, 0);
    gtk_container_add(GTK_CONTAINER(alignment_pos_broad),
		      combobox_pos_broad);
    gtk_widget_show(alignment_pos_broad);
#if GTK_CHECK_VERSION(3, 4, 0)
    gtk_widget_set_hexpand(alignment_pos_broad, TRUE);
    gtk_widget_set_vexpand(alignment_pos_broad, TRUE);
    gtk_grid_attach(GTK_GRID(table1), alignment_pos_broad,
                    1, 2, 1, 1);
#else
    gtk_table_attach(GTK_TABLE(table1), alignment_pos_broad,
		     1, 2, 2, 3, GTK_FILL, GTK_FILL, 5, 5);
#endif
    gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_pos_broad), 0);

    g_signal_connect(G_OBJECT(combobox_pos_broad), "changed",
		     G_CALLBACK(cclass_combobox_changed_cb), window);
  }

  label = gtk_label_new_with_mnemonic(_("_Frequency:"));
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  gtk_widget_show(label);
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_widget_set_hexpand(label, TRUE);
  gtk_grid_attach(GTK_GRID(table1), label, 0, 4, 1, 1);
#else
  gtk_table_attach(GTK_TABLE(table1), label, 0, 1, 4, 5, GTK_FILL, 0, 5, 5);
#endif

  alignment = gtk_alignment_new(0.0, 0.5, 0.0, 0.0);
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_widget_set_hexpand(alignment, TRUE);
  gtk_grid_attach(GTK_GRID(table1), alignment,
                  1, 4, 1, 1);
#else
  gtk_table_attach(GTK_TABLE(table1), alignment,
		   1, 2, 4, 5, GTK_FILL, 0, 5, 5);
#endif
  gtk_widget_show(alignment);
  adjustment_freq = (GtkAdjustment*)gtk_adjustment_new(1.0, 1.0, 65535.0,
						       1.0, 100.0, 0);
  spin_freq = gtk_spin_button_new(adjustment_freq, 1.0, 0);
  gtk_label_set_mnemonic_widget(GTK_LABEL(label), spin_freq);
  gtk_container_add(GTK_CONTAINER(alignment), spin_freq);
  gtk_widget_show(spin_freq);
  window->phon = GTK_WIDGET(entry_phon);
  window->desc = GTK_WIDGET(entry_desc);
  window->freq = GTK_WIDGET(spin_freq);
  window->combobox_pos_broad = GTK_WIDGET(combobox_pos_broad);

  return table1;
}

static GtkWidget *
word_window_additional_create(WordWindow *window)
{
  GtkWidget *table;
  GtkWidget *label, *entry, *button;

#if GTK_CHECK_VERSION(3, 4, 0)
  table = gtk_grid_new();
  gtk_grid_set_row_spacing(GTK_GRID(table), 5);
  gtk_grid_set_column_spacing(GTK_GRID(table), 5);
#else
  table = gtk_table_new(5, 5, FALSE);
#endif

  label = gtk_label_new_with_mnemonic(_("Part of Speech(_narrow):"));
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  gtk_widget_show(label);
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_widget_set_hexpand(label, TRUE);
  gtk_grid_attach(GTK_GRID(table), label, 0, 0, 1, 1);
#else
  gtk_table_attach(GTK_TABLE(table), label, 0, 1, 0, 1, GTK_FILL, 0, 5, 5);
#endif

  entry = gtk_entry_new();
  gtk_editable_set_editable(GTK_EDITABLE(entry), FALSE);
  gtk_label_set_mnemonic_widget(GTK_LABEL(label), entry);
  gtk_widget_show(entry);
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_widget_set_hexpand(entry, TRUE);
  gtk_grid_attach(GTK_GRID(table), entry, 1, 0, 1, 1);
#else
  gtk_table_attach(GTK_TABLE(table), entry, 1, 2, 0, 1, GTK_FILL, 0, 5, 5);
#endif

  button = gtk_button_new_with_mnemonic(_("_Browse..."));
  gtk_widget_show(button);
#if GTK_CHECK_VERSION(3, 4, 0)
  gtk_widget_set_hexpand(button, TRUE);
  gtk_grid_attach(GTK_GRID(table), button, 2, 0, 1, 1);
#else
  gtk_table_attach(GTK_TABLE(table), button, 2, 3, 0, 1, GTK_FILL, 0, 5, 5);
#endif
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(button_cclass_browse_clicked_cb), window);

  window->cclass_code = entry;

  return table;
}

void
word_window_set_word (WordWindow *window, uim_word *w)
{
  gchar *phonetic, *literal, *cclass;
  GtkAdjustment *adj;
  gint cclass_type;

  g_return_if_fail(IS_WORD_WINDOW(window));

  phonetic = charset_convert(w->phon, w->charset, "UTF-8");
  literal = charset_convert(w->desc, w->charset, "UTF-8");
  cclass = charset_convert(w->cclass_code, w->charset, "UTF-8");

  gtk_entry_set_text(GTK_ENTRY(window->phon), phonetic);
  gtk_entry_set_text(GTK_ENTRY(window->desc), literal);
  adj = gtk_spin_button_get_adjustment(GTK_SPIN_BUTTON(window->freq));
  gtk_adjustment_set_value(adj, w->freq);

  if (!strcmp(w->charset, "UTF-8")) {
    gchar *desc = utf8_to_eucjp(w->cclass_code);
    cclass_type = find_cclass_type_from_desc(desc);
    g_free(desc);
  } else
    cclass_type = find_cclass_type_from_desc(w->cclass_code);

  if (cclass_type >= 0)
    gtk_combo_box_set_active(GTK_COMBO_BOX(window->combobox_pos_broad),
			     cclass_type);

  gtk_entry_set_text(GTK_ENTRY(window->cclass_code), cclass);

  g_free(phonetic);
  g_free(literal);
  g_free(cclass);
}

void
word_window_clear(WordWindow *window)
{
  g_return_if_fail(IS_WORD_WINDOW(window));

  gtk_entry_set_text(GTK_ENTRY(window->phon), "");
  gtk_entry_set_text(GTK_ENTRY(window->desc), "");
  /* gtk_entry_set_text(GTK_ENTRY(window->cclass_code), ""); */
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(window->freq), 1);
  gtk_combo_box_set_active(GTK_COMBO_BOX(window->combobox_pos_broad), 0);
}

gboolean
word_window_is_continuance_mode (WordWindow *window)
{
  g_return_val_if_fail(IS_WORD_WINDOW(window), FALSE);

  return gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(window->continuance));
}

static void
word_window_add(WordWindow *window)
{
  gboolean valid;
  const char *utf8_phonetic, *utf8_literal, *utf8_cclass_desc;
  gchar *phonetic, *literal, *cclass_desc, *cclass_native = NULL;
  gint freq, ret, pos_id;
  GtkWidget *dialog;
  uim_word_type type;

  g_return_if_fail(IS_WORD_WINDOW(window));
  g_return_if_fail(window->dict);

  valid = word_window_validate_values(window);
  if (!valid) return;

  utf8_phonetic = gtk_entry_get_text(GTK_ENTRY(window->phon));
  utf8_literal = gtk_entry_get_text(GTK_ENTRY(window->desc));
  utf8_cclass_desc = gtk_entry_get_text(GTK_ENTRY(window->cclass_code));
  pos_id = gtk_combo_box_get_active(GTK_COMBO_BOX(window->combobox_pos_broad));
  freq = gtk_spin_button_get_value(GTK_SPIN_BUTTON(window->freq));

  phonetic = charset_convert(utf8_phonetic, "UTF-8", window->dict->charset);
  literal = charset_convert(utf8_literal, "UTF-8", window->dict->charset);
  cclass_desc = utf8_to_eucjp(utf8_cclass_desc);
  type = dict_identifier_to_word_type(window->dict->identifier);

  if (cclass_desc)
    cclass_native = g_strdup(find_code_from_desc(cclass_desc, pos_id));
  if (!cclass_native)
    cclass_native = g_strdup("");

  if (phonetic != NULL && literal != NULL) {
#if 1 /* FIXME! */
    uim_word *word = malloc(sizeof(uim_word));

    word->type        = type;
    word->charset     = window->dict->charset;
    word->phon        = phonetic;
    word->desc        = literal;
    word->cclass_code = cclass_desc;
    word->cclass_native = cclass_native;
    word->freq        = freq;

    word->okuri          = 0;
    word->following_kana = NULL;
    word->annotation     = NULL;
#else
    uim_word *word = uim_dict_create_word(window->dict, ...);
#endif

    ret = uim_dict_add_word(window->dict, word);

#if 1 /* FIXME! */
    g_free(phonetic);
    g_free(literal);
    g_free(cclass_desc);
    free(word);
#endif
  } else {
    ret = 0;
  }

  if (ret == 0) {
    dialog = gtk_message_dialog_new(NULL,
				    GTK_DIALOG_MODAL,
				    GTK_MESSAGE_ERROR,
				    GTK_BUTTONS_CLOSE,
				    "%s", _("Word registration failed."));
  } else {
    g_signal_emit(G_OBJECT(window), word_window_signals[WORD_ADDED_SIGNAL], 0);
    dialog = gtk_message_dialog_new(NULL,
				    GTK_DIALOG_MODAL,
				    GTK_MESSAGE_INFO,
				    GTK_BUTTONS_CLOSE,
				    "%s", _("Word registration succeded."));
  }

  g_signal_connect(G_OBJECT(dialog), "response",
		   G_CALLBACK(message_dialog_response_cb), window);
  gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(window));
  gtk_dialog_run(GTK_DIALOG(dialog));

  word_window_clear(window);

  /* do not destroy the window when the continuance check box is checked */
  if (!word_window_is_continuance_mode(window)) {
    g_idle_add(idle_wordwin_destroy, window);
    uim_dict_unref(window->dict);
  }
}

static gboolean
word_window_validate_values(WordWindow *window)
{
  const gchar *text;
  GtkWidget *dialog;

  /* phonetic */
  text = gtk_entry_get_text(GTK_ENTRY(window->phon));
  if (!text || !*text) {
    dialog = gtk_message_dialog_new(NULL,
				    GTK_DIALOG_MODAL,
				    GTK_MESSAGE_ERROR,
				    GTK_BUTTONS_CLOSE,
				    "%s", _("Phonetic is empty!"));
    g_signal_connect(G_OBJECT(dialog), "response",
		     G_CALLBACK(message_dialog_response_cb), window);
    gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(window));
    gtk_dialog_run(GTK_DIALOG(dialog));
    return FALSE;
  }

  /* Literal */
  text = gtk_entry_get_text(GTK_ENTRY(window->desc));
  if (!text || !*text) {
    dialog = gtk_message_dialog_new(NULL,
				    GTK_DIALOG_MODAL,
				    GTK_MESSAGE_ERROR,
				    GTK_BUTTONS_CLOSE,
				    "%s", _("Literal is empty!"));
    g_signal_connect(G_OBJECT(dialog), "response",
		     G_CALLBACK(message_dialog_response_cb), window);
    gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(window));
    gtk_dialog_run(GTK_DIALOG(dialog));
    return FALSE;
  }

  /* Frequency */
  /* implement me */

  /* Part of speech */
  text = gtk_entry_get_text(GTK_ENTRY(window->cclass_code));
  if (!text || !*text) {
    dialog = gtk_message_dialog_new(NULL,
				    GTK_DIALOG_MODAL,
				    GTK_MESSAGE_ERROR,
				    GTK_BUTTONS_CLOSE,
				    "%s", _("Part of speech is empty!"));
    g_signal_connect(G_OBJECT(dialog), "response",
		     G_CALLBACK(message_dialog_response_cb), window);
    gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(window));
    gtk_dialog_run(GTK_DIALOG(dialog));
    return FALSE;
  }

  return TRUE;
}

static gboolean
idle_wordwin_destroy(gpointer data)
{
  gtk_widget_destroy(GTK_WIDGET(data));

  return FALSE;
}


/*
 *  callbacks
 */
static void
word_window_response(GtkDialog *dialog, gint arg)
{
  switch (arg)
  {
  case WORD_WINDOW_RESPONSE_ADD:
    word_window_add(WORD_WINDOW(dialog));
    break;
  case WORD_WINDOW_RESPONSE_CLEAR:
    word_window_clear(WORD_WINDOW(dialog));
    break;
  case GTK_RESPONSE_CLOSE:
  case GTK_RESPONSE_CANCEL:
    g_idle_add(idle_wordwin_destroy, dialog);
    uim_dict_unref(WORD_WINDOW(dialog)->dict);
    break;
  default:
    break;
  }
}

static void
message_dialog_response_cb(GtkDialog *dialog, gint arg, WordWindow *window)
{
  switch (arg)
  {
  case GTK_RESPONSE_CLOSE:
    g_idle_add(idle_wordwin_destroy, dialog);
    break;
  default:
    break;
  }
}

static void
word_window_cclass_reset (WordWindow *window)
{
  GtkEntry *entry;
  gint type;
  const gchar *desc;
  gchar *utf8_desc;

  g_return_if_fail(IS_WORD_WINDOW(window));
  g_return_if_fail(window->dict);

  entry = GTK_ENTRY(window->cclass_code);

  gtk_entry_set_text(GTK_ENTRY(window->cclass_code), "");

  type = gtk_combo_box_get_active(GTK_COMBO_BOX(window->combobox_pos_broad));
  switch (type) {
  case POS_VERB:
    desc = verb_code[0].desc;
    break;
  case POS_ADJECTIVE:
    desc = adjective_code[0].desc;
    break;
  case POS_ADVERB:
    desc = adverb_code[0].desc;
    break;
  case POS_ETC:
    desc = etc_code[0].desc;
    break;
  case POS_SUBSTANTIVE:
  default:
    desc = substantive_code[0].desc;
    break;
  }

  utf8_desc = eucjp_to_utf8(desc);
  if (utf8_desc) {
    gtk_entry_set_text(entry, utf8_desc);
    g_free(utf8_desc);
  } else {
    /* error dialog */
  }
}

static void
cclass_combobox_changed_cb(GtkComboBox *combobox, WordWindow *window)
{
  word_window_cclass_reset(window);
}

static void
button_cclass_browse_clicked_cb(GtkButton *button, WordWindow *window)
{
  int type;
  char *cclass_code, *utf8_cclass_desc;
  const char *cclass_desc;
  gint system;

  system = dict_identifier_to_support_type(window->dict->identifier);

  type = gtk_combo_box_get_active(GTK_COMBO_BOX(window->combobox_pos_broad));
  cclass_code = cclass_dialog(type, system);
  if (!cclass_code) return;

  cclass_desc = find_desc_from_code_with_type(cclass_code, type);

  if (cclass_desc) {
    utf8_cclass_desc = eucjp_to_utf8(cclass_desc);
  } else {
    utf8_cclass_desc = g_strdup("");
  }

  gtk_entry_set_text(GTK_ENTRY(window->cclass_code), utf8_cclass_desc);

  g_free(utf8_cclass_desc);
}
