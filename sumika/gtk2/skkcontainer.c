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

#include "skkcontainer.h"
#include "charset.h"
#include "wordlist_view.h"

#include "skk.h"
#include "uim/gettext.h"

/* callbacks */
static void cb_skk_bt_update(GtkButton *button, SKKContainer *container);
static void cb_skk_bt_delete(GtkButton *button, SKKContainer *container);
static void cb_skk_bt_save(GtkButton *button, SKKContainer *container);
static void cb_skk_bt_edit(GtkButton *button, SKKContainer *container);

void cb_skk_bt_update(GtkButton *button, SKKContainer *container) {
    word *list, *pos;
    char *eucjp_phon, *eucjp_desc;
    int i, ret;

    wordlist_view_clear(WORDLIST_VIEW(container->wordlist_skk));

    /* TODO: Implement Progressbar Window */
    ret = read_skk_dic(&list);

    if(ret == -1) {
	GtkWidget *dialog;
	dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL,
					GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE,
					_("Cannot open $HOME/.skk-jisho."));

	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
	gtk_dialog_run(GTK_DIALOG(dialog));
	gtk_widget_destroy(dialog);
	return;
    }

    g_object_freeze_notify(G_OBJECT(container->wordlist_skk));
    for(pos = list; pos != NULL; pos = pos->next) {
	eucjp_phon = pos->phon;
	eucjp_desc = pos->desc;
	pos->phon = eucjp_to_utf8(eucjp_phon);
	pos->desc = eucjp_to_utf8(eucjp_desc);

	if(pos->okuri == 0) {
	    pos->okuri = TRUE;
	} else {
	    pos->okuri = FALSE;
	}

	wordlist_view_set_values(WORDLIST_VIEW(container->wordlist_skk),
				 pos);
    }
    word_free_list(list);
    g_object_thaw_notify(G_OBJECT(container->wordlist_skk));

    gtk_button_set_label(button, _("Reload"));
    for(i = 1;i < 4; i++) {
	gtk_widget_set_sensitive(container->bt_dicfunc[i], TRUE);
    }
}

void cb_skk_bt_edit(GtkButton *button, SKKContainer *container) {
/*
	GtkTreeView *treeview = GTK_TREE_VIEW(container->tv_skkwordlist);
	GtkTreeSelection *selection = gtk_tree_view_get_selection(treeview);
	GtkTreeModel *model = gtk_tree_view_get_model(treeview);
	GtkTreeIter iter;
	gchar *yomi, *kanji;

	if(!gtk_tree_selection_get_selected(selection, &model, &iter))
		return;
	gtk_tree_model_get(model, &iter, SKK_YOMI_COLUMN, &yomi, SKK_KANJI_COLUMN, &kanji, -1);
	g_print("Yomi: %s, Kanjj: %s\n", yomi, kanji);
*/
}

void cb_skk_bt_delete(GtkButton *button, SKKContainer *container) {
    wordlist_view_remove_selected_data(WORDLIST_VIEW(container->wordlist_skk));
}

void cb_skk_bt_save(GtkButton *button, SKKContainer *container) {
    GList *list, *pos;
    word *data, *skklist = NULL;
    int ret = 0, okuri = 0;
    gchar *eucjp_phon, *eucjp_desc;

    list = wordlist_view_get_all_data_list(WORDLIST_VIEW(container->wordlist_skk));

    for(pos = g_list_first(list); pos != NULL; pos = g_list_next(pos)) {
	data = pos->data;

	if(data) {
	    if(data->okuri == TRUE)
		okuri = 0;
	    else
		okuri = 1;
	}

	eucjp_phon = utf8_to_eucjp(data->phon);
	eucjp_desc = utf8_to_eucjp(data->desc);
	word_append(&skklist, WORD_TYPE_SKK,
		    eucjp_phon, eucjp_desc,
		    NULL, 0, okuri, NULL); /* XXX */
    }

    ret = write_skk_dic(skklist);
    word_free_list(skklist);
}

void create_skkcontainer(SKKContainer *container) {
    GtkWidget *wordlist;
    GtkWidget *vbox, *hbox;

    gchar *commands[] = { N_("Load"),
			  N_("Edit"),
			  N_("Delete"),
			  N_("Save") };
    void *cb_commands[] = { cb_skk_bt_update,
			    cb_skk_bt_edit,
			    cb_skk_bt_delete,
			    cb_skk_bt_save };

    int i;

    container->container = gtk_vbox_new(FALSE, 10);
    gtk_container_set_border_width(GTK_CONTAINER(container->container), 10);

    hbox = gtk_hbox_new(FALSE, 10);
    gtk_widget_show(hbox);
    gtk_box_pack_start(GTK_BOX(container->container), hbox, TRUE, TRUE, 5);

    wordlist = wordlist_view_new_with_attributes("editable", TRUE,
						 "cclass_code_show", FALSE,
						 "freq_show", FALSE,
						 "okuri_show", TRUE,
						 "selection_mode", GTK_SELECTION_MULTIPLE,
						 NULL);
    gtk_widget_show(wordlist);
    gtk_box_pack_start(GTK_BOX(hbox), wordlist, TRUE, TRUE, 10);
    container->wordlist_skk = wordlist;

    vbox = gtk_vbutton_box_new();
    gtk_button_box_set_layout(GTK_BUTTON_BOX(vbox), GTK_BUTTONBOX_START);
    gtk_box_set_spacing(GTK_BOX(vbox), 10);
    gtk_widget_show(vbox);
    gtk_box_pack_start(GTK_BOX(hbox), vbox, FALSE, FALSE, 30);

    for(i = 0; i < 4; i++) {
	container->bt_dicfunc[i] = gtk_button_new_with_label(_(commands[i]));
	g_signal_connect(G_OBJECT(container->bt_dicfunc[i]), "clicked",
			 G_CALLBACK(cb_commands[i]), container);
	gtk_box_pack_start(GTK_BOX(vbox), container->bt_dicfunc[i], FALSE, FALSE, 5);
	gtk_widget_show(container->bt_dicfunc[i]);
	gtk_widget_set_sensitive(container->bt_dicfunc[i], FALSE);
    }
    gtk_widget_set_sensitive(container->bt_dicfunc[0],TRUE);

}

void show_skkcontainer(SKKContainer *container, gboolean flags) {
    if(flags) {
	gtk_widget_show(container->container);
    }
}

void clean_skkcontainer(SKKContainer *container) {
    return;
}
