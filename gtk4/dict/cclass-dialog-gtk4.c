/*
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
 *                2005-2026 uim Project https://github.com/uim/uim
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

#include "cclass-dialog-gtk4.h"
#include "canna-cclass.h"
#include "util.h"

#include "gettext.h"

enum {
  COLUMN_ID,
  COLUMN_POS,
  COLUMN_EXAMPLE,
  N_COLUMNS_POS
};

typedef struct {
  GtkListStore *store;
  category_code *code;
  CclassDialogFunc callback;
  gpointer user_data;
} CclassDialogState;

static void
cclass_dialog_response_cb(GtkDialog *dialog, gint response, gpointer data)
{
  CclassDialogState *state = data;
  gchar *result = NULL;

  if (response == GTK_RESPONSE_ACCEPT) {
    GtkTreeView *view = GTK_TREE_VIEW(g_object_get_data(G_OBJECT(dialog),
                                                        "treeview"));
    GtkTreeSelection *selection = gtk_tree_view_get_selection(view);
    GtkTreeIter iter;

    if (gtk_tree_selection_get_selected(selection, NULL, &iter)) {
      gint id;

      gtk_tree_model_get(GTK_TREE_MODEL(state->store), &iter,
                         COLUMN_ID, &id, -1);
      result = g_strdup(state->code[id].code);
    }
  }

  state->callback(result, state->user_data);

  gtk_window_destroy(GTK_WINDOW(dialog));
  g_free(state);
}

void
cclass_dialog(GtkWindow *parent, gint pos_type, gint system,
             CclassDialogFunc callback, gpointer user_data)
{
  GtkWidget *dialog;
  GtkWidget *scrollwin_pos;
  GtkWidget *treeview_pos;
  GtkTreeViewColumn *column;
  GtkListStore *store;
  GtkTreeSelection *selection;
  GtkCellRenderer *renderer;
  GtkTreeIter iter;
  category_code *code = NULL;
  gint narrow_size = 0, i;
  CclassDialogState *state;

  dialog = gtk_dialog_new_with_buttons(_("Part of Speech"), parent,
                                       GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                       _("_Cancel"), GTK_RESPONSE_REJECT,
                                       _("_OK"), GTK_RESPONSE_ACCEPT,
                                       NULL);
  gtk_widget_set_size_request(dialog, 400, 350);

  scrollwin_pos = gtk_scrolled_window_new();
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrollwin_pos),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_set_vexpand(scrollwin_pos, TRUE);
  gtk_widget_set_margin_top(scrollwin_pos, 10);
  gtk_widget_set_margin_bottom(scrollwin_pos, 10);
  gtk_widget_set_margin_start(scrollwin_pos, 10);
  gtk_widget_set_margin_end(scrollwin_pos, 10);
  gtk_box_append(GTK_BOX(gtk_dialog_get_content_area(GTK_DIALOG(dialog))),
                scrollwin_pos);

  treeview_pos = gtk_tree_view_new();
  gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(scrollwin_pos), treeview_pos);
  g_object_set_data(G_OBJECT(dialog), "treeview", treeview_pos);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes("ID", renderer,
                                                    "text", COLUMN_ID, NULL);
  gtk_tree_view_column_set_visible(column, FALSE);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeview_pos), column);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Part of Speech"),
                                                    renderer,
                                                    "text", COLUMN_POS, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeview_pos), column);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Example"), renderer,
                                                    "text", COLUMN_EXAMPLE, NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeview_pos), column);

  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(treeview_pos), TRUE);

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview_pos));
  gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);

  store = gtk_list_store_new(N_COLUMNS_POS,
                             G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING);
  gtk_tree_view_set_model(GTK_TREE_VIEW(treeview_pos), GTK_TREE_MODEL(store));

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
      gchar *pos_utf8 = eucjp_to_utf8(code[i].desc);
      gchar *example_utf8 = eucjp_to_utf8(code[i].example);

      gtk_list_store_append(store, &iter);
      gtk_list_store_set(store, &iter,
                         COLUMN_ID, i,
                         COLUMN_POS, pos_utf8,
                         COLUMN_EXAMPLE, example_utf8,
                         -1);
      g_free(pos_utf8);
      g_free(example_utf8);
    }
  }

  state = g_new0(CclassDialogState, 1);
  state->store = store;
  state->code = code;
  state->callback = callback;
  state->user_data = user_data;

  g_signal_connect(dialog, "response",
                   G_CALLBACK(cclass_dialog_response_cb), state);
  gtk_window_present(GTK_WINDOW(dialog));
}
