/*
 *  $Id:$
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
 *                2005-2013 uim Project https://github.com/uim/uim
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
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 */

#include <config.h>

#include <gtk/gtk.h>
#include <stdio.h>

#include "cclass-dialog.h"
#include "canna-cclass.h"
#include "util.h"

#include "gettext.h"


enum {
    COLUMN_ID,
    COLUMN_POS,
    COLUMN_EXAMPLE,
    N_COLUMNS_POS
};

gchar *cclass_dialog(gint pos_type, gint system) {
    GtkWidget *dialog;
    GtkWidget *scrollwin_pos;
    GtkWidget *treeview_pos;

    GtkTreeViewColumn *column;
    GtkListStore *store;
    GtkTreeSelection *selection;
    GtkCellRenderer *renderer;
    GtkTreeIter iter;
    gint id, result;
    gchar *pos_utf8 = NULL;
    category_code *code = NULL;
    gint narrow_size = 0, i;

    /* construct dialog */
    dialog = gtk_dialog_new_with_buttons(_("Part of Speech"), NULL,
					 GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
					 GTK_STOCK_CANCEL,
					 GTK_RESPONSE_REJECT,
					 GTK_STOCK_OK,
					 GTK_RESPONSE_ACCEPT,
					 NULL);

    gtk_widget_set_size_request(GTK_WIDGET(dialog), 400, 350);
    gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
    scrollwin_pos = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrollwin_pos),
				   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrollwin_pos),
					GTK_SHADOW_IN);

    gtk_box_pack_start(GTK_BOX(gtk_dialog_get_content_area(GTK_DIALOG(dialog))),
		       scrollwin_pos,
		       TRUE, TRUE, 10);
    gtk_widget_show(scrollwin_pos);

    treeview_pos = gtk_tree_view_new();
    gtk_container_add(GTK_CONTAINER(scrollwin_pos),
		      treeview_pos);
    gtk_widget_show(treeview_pos);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes("ID",
						      renderer,
						      "text", COLUMN_ID,
						      NULL);
    gtk_tree_view_column_set_visible(column, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(treeview_pos), column);
    gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(treeview_pos),
				      FALSE);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Part of Speech"),
						      renderer,
						      "text", COLUMN_POS,
						      NULL);
    gtk_tree_view_append_column(GTK_TREE_VIEW(treeview_pos), column);
    gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(treeview_pos),
				      TRUE);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Example"),
						      renderer,
						      "text", COLUMN_EXAMPLE,
						      NULL);
    gtk_tree_view_append_column(GTK_TREE_VIEW(treeview_pos), column);
    gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(treeview_pos),
				      TRUE);

    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview_pos));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);

    store = gtk_list_store_new(N_COLUMNS_POS,
			       G_TYPE_INT,     /* ID */
			       G_TYPE_STRING,  /* POS */
			       G_TYPE_STRING); /* EXAMPLE */
    gtk_tree_view_set_model(GTK_TREE_VIEW(treeview_pos),
			    GTK_TREE_MODEL(store));

    switch (pos_type) {
    case POS_SUBSTANTIVE:
	narrow_size = nr_substantive_code;
	code = substantive_code;
	break;
    case POS_ADVERB:
	narrow_size = nr_adverb_code;
	code = adverb_code;
	break;
    case POS_VERB:
	narrow_size = nr_verb_code;
	code = verb_code;
	break;
    case POS_ADJECTIVE:
	narrow_size = nr_adjective_code;
	code = adjective_code;
	break;
    case POS_ETC:
	narrow_size = nr_etc_code;
	code = etc_code;
	break;
    }

    for (i = 0; i < narrow_size; i++) {
	if (code[i].type & system) {
	    gtk_list_store_append(store, &iter);
	    gtk_list_store_set(store, &iter,
			       COLUMN_ID, i,
			       COLUMN_POS, eucjp_to_utf8(code[i].desc),
			       COLUMN_EXAMPLE, eucjp_to_utf8(code[i].example),
			       -1);
	}
    }

    result = gtk_dialog_run(GTK_DIALOG(dialog));

    switch (result) {
    case GTK_RESPONSE_ACCEPT:
	if (gtk_tree_selection_get_selected(selection, NULL, &iter)) {
	    gtk_tree_model_get(GTK_TREE_MODEL(store), &iter,
			       COLUMN_ID, &id, -1);
	    pos_utf8 = g_strdup(code[id].code);
	}
	break;
    default:
	break;
    }
    gtk_widget_destroy(dialog);

    return pos_utf8;
}
