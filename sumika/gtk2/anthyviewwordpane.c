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

#include <stdlib.h>

#include "anthyviewwordpane.h"
#include "wordlist_view.h"

#include "cannadic.h"
#include "charset.h"
#include "anthy.h"
#include "uim/gettext.h"

/* callbacks */
static void cb_anthy_bt_update(GtkButton *button, AnthyViewWordPane *pane);
static void cb_anthy_bt_delete(GtkButton *button, AnthyViewWordPane *pane);
static void cb_anthy_bt_edit(GtkButton *button, AnthyViewWordPane *pane);
static gboolean cb_searchword_focusout(GtkWidget *, GdkEventFocus *,AnthyViewWordPane *);

static gboolean cb_searchword_focusout(GtkWidget *widget, GdkEventFocus *focus, AnthyViewWordPane *pane)
{
    int num;
    gchar *searchword;
    gchar *eucjp_phonetic, *eucjp_literal, *eucjp_pos;
    word *list = NULL, *pos = NULL;

    searchword = gtk_editable_get_chars (GTK_EDITABLE(pane->searchword),0,-1);

    wordlist_view_clear(WORDLIST_VIEW(pane->wordlist_anthy));
    if(searchword != NULL) {
	num = read_anthy_priv_dic_list(&list);
	if(list == NULL)
	    return;
	
	for(pos = list; pos != NULL; pos = pos->next) {
	    eucjp_phonetic = pos->phon;
	    eucjp_literal = pos->desc;
	    eucjp_pos = pos->cclass_code;
	    pos->phon = eucjp_to_utf8(eucjp_phonetic);
	    pos->desc = eucjp_to_utf8(eucjp_literal);
	    pos->cclass_code = eucjp_to_utf8(find_pos_from_code(eucjp_pos, SUPPORT_ANTHY));
	    g_free(eucjp_phonetic);
	    g_free(eucjp_literal);
	    g_free(eucjp_pos);
	    if(strstr(pos->phon, searchword) != NULL) {
		wordlist_view_set_values(WORDLIST_VIEW(pane->wordlist_anthy), pos);
	    }
	}
	word_free_list(list);

        g_free(searchword);
    }
    return FALSE;
}

static void cb_anthy_bt_update(GtkButton *button, AnthyViewWordPane *pane) {
    int num, i;
    word *list = NULL, *pos = NULL;
    char *eucjp_phon, *eucjp_desc;
    char *eucjp_cclass_code;

    wordlist_view_clear(WORDLIST_VIEW(pane->wordlist_anthy));

    num = read_anthy_priv_dic_list(&list);
    if(list == NULL)
	return;

    for(pos = list; pos != NULL; pos = pos->next) {
	eucjp_phon = pos->phon;
	eucjp_desc = pos->desc;
	eucjp_cclass_code = pos->cclass_code;

	pos->phon = eucjp_to_utf8(eucjp_phon);
	pos->desc = eucjp_to_utf8(eucjp_desc);
	pos->cclass_code = eucjp_to_utf8(find_pos_from_code(eucjp_cclass_code, SUPPORT_ANTHY));
	wordlist_view_set_values(WORDLIST_VIEW(pane->wordlist_anthy), pos);
    }
    word_free_list(list);
    gtk_button_set_label(button, _("Reload"));

    for(i = 1;i < 3; i++)
	gtk_widget_set_sensitive(pane->bt_dicfunc[i], TRUE);

    gtk_widget_set_sensitive(pane->searchword, TRUE);
}

static void cb_anthy_bt_delete(GtkButton *button, AnthyViewWordPane *pane) {
    GtkWidget *dialog;
    GList *selected_data;
    word *data;
    char *eucjp_pos, *code;
    int ret;

    selected_data = wordlist_view_get_selected_data_list(WORDLIST_VIEW(pane->wordlist_anthy));
    if(selected_data == NULL)
	return;

    data = selected_data->data;

/* TODO: Must handle multiple selection */

    eucjp_pos = utf8_to_eucjp(data->cclass_code);
    code = find_code_from_pos(eucjp_pos, SUPPORT_ANTHY);
    ret = delete_anthy_priv_dic(utf8_to_eucjp(data->phon),
				utf8_to_eucjp(data->desc),
				code);
    free(eucjp_pos);
    free(code);
/* End of TODO */

    if(ret) {
	dialog = gtk_message_dialog_new(NULL,
					GTK_DIALOG_MODAL,
					GTK_MESSAGE_INFO,
					GTK_BUTTONS_CLOSE,
					_("Word deletion succeded."));
    } else {
	dialog = gtk_message_dialog_new(NULL,
					GTK_DIALOG_MODAL,
					GTK_MESSAGE_ERROR,
					GTK_BUTTONS_CLOSE,
					_("Word deletion failed."));
    }
    gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);

    wordlist_view_remove_selected_data(WORDLIST_VIEW(pane->wordlist_anthy));

    g_list_free(selected_data);
}

static void cb_anthy_bt_edit(GtkButton *button, AnthyViewWordPane *pane) {
    GList *selected_data;
    GtkWidget *dialog;
    GtkWidget *table;
    GtkWidget *label_phon, *label_desc, *label_cclass_code, *label_freq;
    GtkWidget *entry_phon, *entry_desc;
    GtkWidget *entry_cclass_code, *entry_freq;
    word *data;
    gchar freq_str[6];
    gint result;
    const gchar *utf8_phonetic, *utf8_literal;
    const gchar *cclass_code;
    char *euc_phonetic, *euc_literal;
    int freq, ret;

    selected_data = wordlist_view_get_selected_data_list(WORDLIST_VIEW(pane->wordlist_anthy));
    if(selected_data == NULL)
	return;

    data = selected_data->data;

    dialog = gtk_dialog_new_with_buttons(_("Edit"), NULL,
					 GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
					 GTK_STOCK_OK,
					 GTK_RESPONSE_ACCEPT,
					 GTK_STOCK_CANCEL,
					 GTK_RESPONSE_REJECT,
					 NULL);
 
    gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);

    table = gtk_table_new(5, 2, FALSE);
    gtk_widget_show(table);
    gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), table, FALSE, FALSE, 0);

    label_phon = gtk_label_new(_("Phonetic:"));
    gtk_misc_set_alignment(GTK_MISC(label_phon), 1.0, 0.5);
    gtk_widget_show(label_phon);
    gtk_table_attach(GTK_TABLE(table), label_phon,
		     0, 1, 0, 1, GTK_FILL, 0, 5, 5);

    entry_phon = gtk_entry_new();
    gtk_table_attach(GTK_TABLE(table), entry_phon,
		     1, 2, 0, 1, 0, 0, 5, 5);
    gtk_widget_show(entry_phon);
    gtk_entry_set_text(GTK_ENTRY(entry_phon), data->phon);


    label_desc = gtk_label_new(_("Literal:"));
    gtk_misc_set_alignment(GTK_MISC(label_desc), 1.0, 0.5);
    gtk_widget_show(label_desc);
    gtk_table_attach(GTK_TABLE(table), label_desc,
		     0, 1, 1, 2, GTK_FILL, 0, 5, 5);

    entry_desc = gtk_entry_new();
    gtk_table_attach(GTK_TABLE(table), entry_desc,
		     1, 2, 1, 2, 0, 0, 5, 5);
    gtk_widget_show(entry_desc);
    gtk_entry_set_text(GTK_ENTRY(entry_desc), data->desc);

    label_cclass_code = gtk_label_new(_("Part of Speech(narrow):"));
    gtk_misc_set_alignment(GTK_MISC(label_cclass_code), 1.0, 0.5);
    gtk_widget_show(label_cclass_code);
    gtk_table_attach(GTK_TABLE(table), label_cclass_code,
		     0, 1, 3, 4, GTK_FILL, 0, 5, 5);

    entry_cclass_code = gtk_entry_new();
    gtk_widget_show(entry_cclass_code);
    gtk_table_attach(GTK_TABLE(table), entry_cclass_code,
		     1, 2, 3, 4, GTK_FILL, 0, 5, 5);
    gtk_entry_set_text(GTK_ENTRY(entry_cclass_code), data->cclass_code);

    label_freq = gtk_label_new(_("Frequency:"));
    gtk_misc_set_alignment(GTK_MISC(label_freq), 1.0, 0.5);
    gtk_widget_show(label_freq);
    gtk_table_attach(GTK_TABLE(table), label_freq,
		     0, 1, 4, 5, GTK_FILL, 0, 5, 5);

    entry_freq = gtk_entry_new();
    gtk_widget_show(entry_freq);
    gtk_entry_set_max_length(GTK_ENTRY(entry_freq), 5); /* 0 - 65536 */
    g_snprintf(freq_str, sizeof(freq_str), "%d", data->freq);
    gtk_entry_set_text(GTK_ENTRY(entry_freq), freq_str);
    gtk_table_attach(GTK_TABLE(table), entry_freq,
		     1, 2, 4, 5, GTK_FILL, 0, 5, 5);

    result = gtk_dialog_run(GTK_DIALOG(dialog));

    switch(result) {
    case GTK_RESPONSE_ACCEPT:
	ret = delete_anthy_priv_dic(data->phon, data->desc,
				    data->cclass_code);

	utf8_phonetic = gtk_entry_get_text(GTK_ENTRY(entry_phon));
	utf8_literal = gtk_entry_get_text(GTK_ENTRY(entry_desc));
	cclass_code = gtk_entry_get_text(GTK_ENTRY(entry_cclass_code));
	freq = atoi(gtk_entry_get_text(GTK_ENTRY(entry_freq)));
	
	euc_phonetic = charset_convert(utf8_phonetic, "UTF-8", "EUC-JP");
	euc_literal = charset_convert(utf8_literal, "UTF-8", "EUC-JP");

	if(euc_phonetic != NULL && euc_literal != NULL) {
	    ret = add_anthy_priv_dic_with_flags(euc_phonetic, euc_literal,
						cclass_code, freq);
	}

	if(euc_phonetic != NULL)
	    free(euc_phonetic);
	if(euc_literal != NULL)
	    free(euc_literal);
	if(utf8_phonetic != NULL)
	    g_free(utf8_phonetic);
	if(utf8_literal != NULL)
	    g_free(utf8_literal);
	break;
    default:
	break;
    }

    gtk_widget_destroy(dialog);
    g_list_free(selected_data);
}

int create_viewwordpane(AnthyViewWordPane *pane) {
    GtkWidget *label;
    GtkWidget *hbox2;
    GtkWidget *vbox1, *vbox2;
    GtkWidget *wordlist;

    gchar *commands[] = { N_("Load"), N_("Edit"), N_("Delete") };
    void *cb_commands[] = { cb_anthy_bt_update,
			    cb_anthy_bt_edit,
			    cb_anthy_bt_delete };
    gint num_commands = 3;

    int i;

    pane->pane = gtk_hbox_new(FALSE, 15);
    gtk_container_set_border_width(GTK_CONTAINER(pane->pane), 15);

    vbox1 = gtk_vbox_new(FALSE, 0);
    gtk_widget_show(vbox1);
    gtk_box_pack_start(GTK_BOX(pane->pane), vbox1, TRUE, TRUE, 0);

    hbox2 = gtk_hbox_new(FALSE, 10);
    gtk_box_pack_start(GTK_BOX(vbox1), hbox2, FALSE, FALSE, 0);
    gtk_widget_show(hbox2);

    /* in hbox */
    wordlist = wordlist_view_new_with_attributes("editable", FALSE,
						 "freq_show", TRUE,
						 "cclass_code_show", TRUE,
						 NULL);
    gtk_box_pack_start(GTK_BOX(vbox1), wordlist, TRUE, TRUE, 10);
    gtk_widget_show(wordlist);
    pane->wordlist_anthy = wordlist;

    vbox2 = gtk_vbutton_box_new();
    gtk_button_box_set_layout(GTK_BUTTON_BOX(vbox2), GTK_BUTTONBOX_START);
    gtk_box_set_spacing(GTK_BOX(vbox2), 10);
    gtk_box_pack_start(GTK_BOX(pane->pane), vbox2, FALSE, FALSE, 10);
    gtk_widget_show(vbox2);

    /* in vbox2 */
    for(i = 0; i < num_commands; i++) {
	pane->bt_dicfunc[i] = gtk_button_new_with_label(_(commands[i]));
	gtk_box_pack_start(GTK_BOX(vbox2), pane->bt_dicfunc[i], FALSE, FALSE, 5);
	gtk_widget_set_sensitive(pane->bt_dicfunc[i], FALSE);
	g_signal_connect(G_OBJECT(pane->bt_dicfunc[i]), "clicked",
			 G_CALLBACK(cb_commands[i]), pane);
	gtk_widget_show(pane->bt_dicfunc[i]);
    }
    gtk_widget_set_sensitive(pane->bt_dicfunc[0],TRUE);

    /* in hbox2 */
    label = gtk_label_new_with_mnemonic(_("Search:"));
    gtk_box_pack_start(GTK_BOX(hbox2), label, FALSE, FALSE, 5);
    gtk_widget_show(label);

    pane->searchword = gtk_entry_new();
    gtk_box_pack_start(GTK_BOX(hbox2), pane->searchword, FALSE, FALSE, 5);
    gtk_widget_show(pane->searchword);
    gtk_widget_set_sensitive(pane->searchword, FALSE);
    g_signal_connect(G_OBJECT(pane->searchword), "focus-out-event",
		    	G_CALLBACK(cb_searchword_focusout), pane);

    return 0;
}
