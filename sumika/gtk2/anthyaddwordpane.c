/*
 *  $Id:$
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the name of authors nor the names of its contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 */

#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "anthy.h"
#include "cannadic.h"

#include "anthyaddwordpane.h"
#include "posdialog.h"
#include "charset.h"
#include "uim/config.h"
#include "uim/gettext.h"

static char *pos_broad[] = {
    N_("Verb"),
    N_("Substantive"),
    N_("Adjective"),
    N_("Adverb"),
    N_("Etc"),
};

/* callbacks */
static void cb_add_button_clicked	   (GtkButton *, AnthyAddWordPane *);
static void cb_clear_button_clicked	   (GtkButton *, AnthyAddWordPane *);
static void cb_button_cclass_browse_clicked(GtkButton *, AnthyAddWordPane *);

static void cb_add_button_clicked (GtkButton *button,
				   AnthyAddWordPane *pane)
{
    GtkWidget *dialog;
    const gchar *utf8_phonetic, *utf8_literal;
    const gchar *cclass_code;
    char *euc_phonetic, *euc_literal;
    int freq;
    int ret;

    utf8_phonetic = gtk_entry_get_text(GTK_ENTRY(pane->phon));
    utf8_literal  = gtk_entry_get_text(GTK_ENTRY(pane->desc));
    cclass_code   = gtk_entry_get_text(GTK_ENTRY(pane->cclass_code));
    freq = gtk_spin_button_get_value(pane->freq);

    euc_phonetic = charset_convert(utf8_phonetic, "UTF-8", "EUC-JP");
    euc_literal = charset_convert(utf8_literal, "UTF-8", "EUC-JP");

    if(euc_phonetic != NULL && euc_literal != NULL) {
	ret = add_anthy_priv_dic_with_flags(euc_phonetic, euc_literal,
					    cclass_code, freq);
    } else {
	ret = -1;
    }

    if(ret == -1) {
	dialog = gtk_message_dialog_new(NULL,
					GTK_DIALOG_MODAL,
					GTK_MESSAGE_ERROR,
					GTK_BUTTONS_CLOSE,
					_("Word registration failed."));
    } else {
	dialog = gtk_message_dialog_new(NULL,
					GTK_DIALOG_MODAL,
					GTK_MESSAGE_INFO,
					GTK_BUTTONS_CLOSE,
					_("Word registration succeded."));
	/* clear all data */
	cb_clear_button_clicked(NULL, pane);
    }
    gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);

    if(euc_phonetic != NULL)
	g_free(euc_phonetic);
    if(euc_literal != NULL)
	g_free(euc_literal);
/*
    if(utf8_phonetic != NULL)
	g_free(utf8_phonetic);
    if(utf8_literal != NULL)
	g_free(utf8_literal);
*/
}

static void cb_clear_button_clicked (GtkButton *button,
				     AnthyAddWordPane *pane)
{
    gtk_entry_set_text(GTK_ENTRY(pane->phon), "");
    gtk_entry_set_text(GTK_ENTRY(pane->desc), "");
    gtk_entry_set_text(GTK_ENTRY(pane->cclass_code), "");
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane->freq), 1);
    gtk_combo_box_set_active(GTK_COMBO_BOX(pane->combobox_pos_broad), 0);
}

static void cb_button_cclass_browse_clicked(GtkButton *button,
					    AnthyAddWordPane *pane)
{
    gint id;
    gchar *code;
    id = gtk_combo_box_get_active(GTK_COMBO_BOX(pane->combobox_pos_broad));
    code = cannadic_pos_dialog(id, SUPPORT_ANTHY);
    if(code != NULL) {
	gtk_entry_set_text(GTK_ENTRY(pane->cclass_code), code);
	g_free(code);
    }
}

AnthyAddWordPane *anthy_addwordpane_new(void) {
    AnthyAddWordPane *pane;
    GtkWidget *hbox;
    GtkWidget *vbox1, *vbox2;
    GtkWidget *table1;
    GtkWidget *label_phon, *label_desc;
    GtkWidget *label_cclass_code_narrow, *label_cclass_code_broad;
    GtkWidget *label_freq;
    GtkWidget *entry_phon, *entry_desc, *entry_cclass_code_narrow;
    GtkWidget *spin_freq;
    GtkAdjustment *adjustment_freq;
    GtkWidget *button_cclass_browse;
    GtkWidget *button_add, *button_clear;
    GtkWidget *combobox_pos_broad;
    int i;

    pane = g_malloc0(sizeof(AnthyAddWordPane));

    hbox = gtk_hbox_new(FALSE, 15);
    gtk_container_set_border_width(GTK_CONTAINER(hbox), 15);

    vbox1 = gtk_vbox_new(FALSE, 0);
    gtk_widget_show(vbox1);
    gtk_box_pack_start(GTK_BOX(hbox), vbox1, FALSE, FALSE, 0);

    table1 = gtk_table_new(5, 5, FALSE);
    gtk_widget_show(table1);
    gtk_box_pack_start(GTK_BOX(vbox1), table1, FALSE, FALSE, 0);

    label_phon = gtk_label_new(_("Phonetic:"));
    gtk_misc_set_alignment(GTK_MISC(label_phon), 1.0, 0.5);
    gtk_widget_show(label_phon);
    gtk_table_attach(GTK_TABLE(table1), label_phon,
		     0, 1, 0, 1, GTK_FILL, 0, 5, 5);

    entry_phon = gtk_entry_new();
    gtk_table_attach(GTK_TABLE(table1), entry_phon,
		     1, 2, 0, 1, 0, 0, 5, 5);
    gtk_widget_show(entry_phon);

    label_desc = gtk_label_new(_("Literal:"));
    gtk_misc_set_alignment(GTK_MISC(label_desc), 1.0, 0.5);
    gtk_widget_show(label_desc);
    gtk_table_attach(GTK_TABLE(table1), label_desc,
		     0, 1, 1, 2, GTK_FILL, 0, 5, 5);

    entry_desc = gtk_entry_new();
    gtk_table_attach(GTK_TABLE(table1), entry_desc,
		     1, 2, 1, 2, 0, 0, 5, 5);
    gtk_widget_show(entry_desc);

    label_cclass_code_broad = gtk_label_new(_("Part of Speech(broad):"));
    gtk_misc_set_alignment(GTK_MISC(label_cclass_code_broad), 1.0, 0.5);
    gtk_widget_show(label_cclass_code_broad);
    gtk_table_attach(GTK_TABLE(table1), label_cclass_code_broad,
		     0, 1, 2, 3, GTK_FILL, 0, 5, 5);
    {
	GtkWidget *alignment_pos_broad;
	gint pos_num;

	pos_num = sizeof(pos_broad) / sizeof(pos_broad[0]);
	combobox_pos_broad = gtk_combo_box_new_text();
	gtk_widget_show(combobox_pos_broad);

	for(i = 0; i < pos_num; i++) {
	    gtk_combo_box_append_text(GTK_COMBO_BOX(combobox_pos_broad),
				      pos_broad[i]);
	}

	alignment_pos_broad = gtk_alignment_new(0, 0.5, 0, 0);
	gtk_container_add(GTK_CONTAINER(alignment_pos_broad),
			  combobox_pos_broad);
	gtk_widget_show(alignment_pos_broad);
	gtk_table_attach(GTK_TABLE(table1), alignment_pos_broad,
			 1, 2, 2, 3, GTK_FILL, GTK_FILL, 5, 5);
	gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_pos_broad), 0);
    }

    label_cclass_code_narrow = gtk_label_new(_("Part of Speech(narrow):"));
    gtk_misc_set_alignment(GTK_MISC(label_cclass_code_narrow), 1.0, 0.5);
    gtk_widget_show(label_cclass_code_narrow);
    gtk_table_attach(GTK_TABLE(table1), label_cclass_code_narrow,
		     0, 1, 3, 4, GTK_FILL, 0, 5, 5);

    entry_cclass_code_narrow = gtk_entry_new();
    gtk_widget_show(entry_cclass_code_narrow);
    gtk_table_attach(GTK_TABLE(table1), entry_cclass_code_narrow,
		     1, 2, 3, 4, GTK_FILL, 0, 5, 5);

    button_cclass_browse = gtk_button_new_with_label(_("Browse..."));
    gtk_widget_show(button_cclass_browse);
    gtk_table_attach(GTK_TABLE(table1), button_cclass_browse,
		     2, 3, 3, 4, GTK_FILL, 0, 5, 5);

    g_signal_connect(G_OBJECT(button_cclass_browse), "clicked",
		     G_CALLBACK(cb_button_cclass_browse_clicked), pane);

    label_freq = gtk_label_new(_("Frequency:"));
    gtk_misc_set_alignment(GTK_MISC(label_freq), 1.0, 0.5);
    gtk_widget_show(label_freq);
    gtk_table_attach(GTK_TABLE(table1), label_freq,
		     0, 1, 4, 5, GTK_FILL, 0, 5, 5);

    adjustment_freq = (GtkAdjustment*)gtk_adjustment_new(1.0, 1.0, 65535.0, 1.0, 100.0, 100.0); /* 1-65535 */
    spin_freq = gtk_spin_button_new(adjustment_freq, 1.0, 0);
    gtk_widget_show(spin_freq);
    gtk_table_attach(GTK_TABLE(table1), spin_freq,
		     1, 2, 4, 5, GTK_FILL, 0, 5, 5);

    vbox2 = gtk_vbutton_box_new();
    gtk_button_box_set_layout(GTK_BUTTON_BOX(vbox2), GTK_BUTTONBOX_START);
    gtk_box_set_spacing(GTK_BOX(vbox2), 10);
    gtk_widget_show(vbox2);
    gtk_box_pack_start(GTK_BOX(hbox), vbox2, FALSE, FALSE, 50);

    button_add = gtk_button_new_with_label(_("Add"));
    gtk_box_pack_start(GTK_BOX(vbox2), button_add, FALSE, FALSE, 0);
    g_signal_connect(G_OBJECT(button_add), "clicked",
		     G_CALLBACK(cb_add_button_clicked), pane);
    gtk_widget_show(button_add);

    button_clear = gtk_button_new_with_label(_("Clear"));
    gtk_box_pack_start(GTK_BOX(vbox2), button_clear, FALSE, FALSE, 0);
    g_signal_connect(G_OBJECT(button_clear), "clicked",
		     G_CALLBACK(cb_clear_button_clicked), pane);
    gtk_widget_show(button_clear);

    pane->pane = GTK_WIDGET(hbox);
    pane->phon = GTK_WIDGET(entry_phon);
    pane->desc = GTK_WIDGET(entry_desc);
    pane->freq = GTK_WIDGET(spin_freq);
    pane->cclass_code = GTK_WIDGET(entry_cclass_code_narrow);
    pane->combobox_pos_broad = GTK_WIDGET(combobox_pos_broad);

    pane->add = GTK_WIDGET(button_add);
    pane->clear = GTK_WIDGET(button_clear);
    return pane;
}
