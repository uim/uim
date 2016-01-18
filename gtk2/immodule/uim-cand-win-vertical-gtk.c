/*

  copyright (c) 2011-2013 uim Project https://github.com/uim/uim

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

#include "uim-cand-win-vertical-gtk.h"
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <uim/uim.h>
#include <uim/uim-scm.h>

enum {
  INDEX_CHANGED_SIGNAL,
  NR_SIGNALS
};

enum {
  TERMINATOR = -1,
  COLUMN_HEADING,
  COLUMN_CANDIDATE,
  COLUMN_ANNOTATION,
  LISTSTORE_NR_COLUMNS
};

static void uim_cand_win_vertical_gtk_init(UIMCandWinVerticalGtk *cwin);
static void uim_cand_win_vertical_gtk_class_init(UIMCandWinGtkClass *klass);
static void uim_cand_win_vertical_gtk_dispose(GObject *obj);

static gboolean	tree_selection_change		(GtkTreeSelection *selection,
						 GtkTreeModel *model,
						 GtkTreePath *path,
						 gboolean path_currently_selected,
						 gpointer data);
static gboolean tree_selection_changed		(GtkTreeSelection *selection,
						 gpointer data);
static gboolean tree_view_button_press		(GtkWidget *widget,
						 GdkEventButton *event,
						 gpointer data);


static GType cand_win_vertical_type = 0;
static GTypeInfo const object_info = {
  sizeof (UIMCandWinVerticalGtkClass),
  (GBaseInitFunc) NULL,
  (GBaseFinalizeFunc) NULL,
  (GClassInitFunc) uim_cand_win_vertical_gtk_class_init,
  (GClassFinalizeFunc) NULL,
  NULL,                       /* class_data */
  sizeof (UIMCandWinVerticalGtk),
  0,                          /* n_preallocs */
  (GInstanceInitFunc) uim_cand_win_vertical_gtk_init,
};

static UIMCandWinGtkClass *parent_class = NULL;

GType
uim_cand_win_vertical_gtk_get_type(void)
{
  if (!cand_win_vertical_type)
    cand_win_vertical_type = g_type_register_static(UIM_TYPE_CAND_WIN_GTK, "UIMCandWinVerticalGtk",
					   &object_info, (GTypeFlags)0);
  return cand_win_vertical_type;
}

GType
uim_cand_win_vertical_gtk_register_type(GTypeModule *module)
{
  if (!cand_win_vertical_type)
    cand_win_vertical_type = g_type_module_register_type(module,
						UIM_TYPE_CAND_WIN_GTK,
						"UIMCandWinVerticalGtk",
						&object_info, 0);
  return cand_win_vertical_type;
}

static void
uim_cand_win_vertical_gtk_class_init (UIMCandWinGtkClass *klass)
{
  GObjectClass *object_class = (GObjectClass *) klass;

  parent_class = g_type_class_peek_parent (klass);
  object_class->dispose = uim_cand_win_vertical_gtk_dispose;

  klass->set_index = (void (*)(UIMCandWinGtk *, gint))uim_cand_win_vertical_gtk_set_index;
  klass->set_page = (void (*)(UIMCandWinGtk *, gint))uim_cand_win_vertical_gtk_set_page;
}

static void
uim_cand_win_vertical_gtk_init (UIMCandWinVerticalGtk *vertical_cwin)
{
  UIMCandWinGtk *cwin;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;

  cwin = UIM_CAND_WIN_GTK(vertical_cwin);

  cwin->view = gtk_tree_view_new();
  gtk_container_add(GTK_CONTAINER(cwin->scrolled_window), cwin->view);

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(cwin->view));

  gtk_tree_selection_set_select_function(selection,
					 tree_selection_change,
					 cwin,
					 NULL);
  g_signal_connect (G_OBJECT(selection), "changed",
		    G_CALLBACK(tree_selection_changed), cwin);

  renderer = gtk_cell_renderer_text_new();
  g_object_set(renderer, "scale", 0.8, (const gchar *)NULL);

  column = gtk_tree_view_column_new_with_attributes("No",
						    renderer,
						    "text", COLUMN_HEADING,
						    (const gchar *)NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(cwin->view), column);
  gtk_tree_view_column_set_sizing (column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);

  renderer = gtk_cell_renderer_text_new();
  g_object_set(renderer, "scale", 1.2, (const gchar *)NULL);
  /*  g_object_set(renderer, "size-points", 20.0, NULL); */
  column = gtk_tree_view_column_new_with_attributes("Text",
						    renderer,
						    "text", COLUMN_CANDIDATE,
						    (const gchar *)NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(cwin->view), column);
  gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(cwin->view), TRUE);
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(cwin->view), FALSE);
  gtk_tree_view_column_set_sizing (column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);

  g_signal_connect(G_OBJECT(cwin->view), "button-press-event",
		   G_CALLBACK(tree_view_button_press), cwin);

  gtk_widget_show(cwin->view);
}

static void
uim_cand_win_vertical_gtk_dispose (GObject *obj)
{
  if (G_OBJECT_CLASS (parent_class)->dispose)
    G_OBJECT_CLASS (parent_class)->dispose(obj);
}

UIMCandWinVerticalGtk *
uim_cand_win_vertical_gtk_new (void)
{
  GObject *obj = g_object_new(UIM_TYPE_CAND_WIN_VERTICAL_GTK,
			      "type", GTK_WINDOW_POPUP,
			      NULL);

  return UIM_CAND_WIN_VERTICAL_GTK(obj);
}

static gboolean
tree_selection_change(GtkTreeSelection *selection,
		      GtkTreeModel *model,
		      GtkTreePath *path,
		      gboolean path_currently_selected,
		      gpointer data)
{
  UIMCandWinVerticalGtk *vertical_cwin = UIM_CAND_WIN_VERTICAL_GTK(data);
  UIMCandWinGtk *cwin = UIM_CAND_WIN_GTK(vertical_cwin);
  gint *indicies;
  gint idx;

  if (!cwin)
    return TRUE;
 
  if (cwin->block_index_selection)
    return TRUE;

  indicies = gtk_tree_path_get_indices(path);
  g_return_val_if_fail(indicies, TRUE);
  idx = *indicies + cwin->display_limit * cwin->page_index;

  if (!path_currently_selected && cwin->candidate_index != idx) {
    if (cwin->candidate_index >= 0) {
      cwin->candidate_index = idx;
      /* if emit "index-changed" here and IM deactivates this candwin,
       * activates new candwin and selects a candidate on new candwin
       * from index-changed callback, SEGV occurs in gtk because gtk tries to
       * select on old candwin after return of this tree_selection_change().
       * To avoid SEGV, instead of emitting before selection change by gtk,
       * emit after selection changed by gtk. */
      cwin->index_changed = TRUE;
    }

    uim_cand_win_gtk_update_label(cwin);

    if (cwin->candidate_index < 0)
      return FALSE;
    else
      return TRUE;
  } else {
    uim_cand_win_gtk_update_label(cwin);

    return TRUE;
  }
}

static gboolean
tree_selection_changed(GtkTreeSelection *selection,
		       gpointer data)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  UIMCandWinVerticalGtk *vertical_cwin = UIM_CAND_WIN_VERTICAL_GTK(data);
  UIMCandWinGtk *cwin = UIM_CAND_WIN_GTK(vertical_cwin);

  if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
    char *annotation = NULL;

    gtk_tree_model_get(model, &iter,
		       COLUMN_ANNOTATION, &annotation,
		       -1);

    if (annotation && *annotation) {
      if (!cwin->sub_window.window)
	uim_cand_win_gtk_create_sub_window(cwin);
      gtk_text_buffer_set_text(
	gtk_text_view_get_buffer(GTK_TEXT_VIEW(cwin->sub_window.text_view)),
	annotation, -1);
      uim_cand_win_gtk_layout_sub_window(cwin);
      gtk_widget_show(cwin->sub_window.window);
      cwin->sub_window.active = TRUE;
    } else {
      if (cwin->sub_window.window) {
	gtk_widget_hide(cwin->sub_window.window);
	cwin->sub_window.active = FALSE;
      }
    }
    free(annotation);
  } else {
    if (cwin->sub_window.window) {
      gtk_widget_hide(cwin->sub_window.window);
      cwin->sub_window.active = FALSE;
    }
  }

  if (cwin->index_changed) {
    cwin->index_changed = FALSE;
    g_signal_emit_by_name(G_OBJECT(cwin), "index-changed");
  }

  return TRUE;
}

static gboolean
tree_view_button_press(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
  UIMCandWinVerticalGtk *vertical_cwin;
  UIMCandWinGtk *cwin;
  GtkTreePath *path;
  gboolean exist, retval = FALSE;
  gint *indicies;

  g_return_val_if_fail(GTK_IS_TREE_VIEW(widget), FALSE);
  g_return_val_if_fail(UIM_CAND_WIN_VERTICAL_GTK(data), FALSE);

  vertical_cwin = UIM_CAND_WIN_VERTICAL_GTK(data);
  cwin = UIM_CAND_WIN_GTK(vertical_cwin);

  exist = gtk_tree_view_get_path_at_pos(GTK_TREE_VIEW(widget),
					event->x, event->y,
					&path, NULL, NULL, NULL);
  if (!exist)
    return FALSE;

  indicies = gtk_tree_path_get_indices(path);

  /* don't relay button press event to empty row */
  if (cwin->display_limit * cwin->page_index + *indicies >= cwin->nr_candidates)
    retval = TRUE;

  gtk_tree_path_free(path);

  return retval;
}


void
uim_cand_win_vertical_gtk_set_index(UIMCandWinVerticalGtk *vertical_cwin, gint index)
{
  UIMCandWinGtk *cwin;
  UIMCandWinVerticalGtkClass *vertical_cwin_class;
  UIMCandWinGtkClass *cwin_class;

  g_return_if_fail(UIM_IS_CAND_WIN_VERTICAL_GTK(vertical_cwin));
  cwin = UIM_CAND_WIN_GTK(vertical_cwin);

  /* call parent method */
  vertical_cwin_class = UIM_CAND_WIN_VERTICAL_GTK_GET_CLASS(vertical_cwin);
  cwin_class = g_type_class_peek_parent(vertical_cwin_class);
  cwin_class->set_index(cwin, index);

  if (cwin->candidate_index >= 0) {
    GtkTreePath *path;
    gint pos = index;

    if (cwin->display_limit)
      pos = cwin->candidate_index % cwin->display_limit;

    path = gtk_tree_path_new_from_indices(pos, -1);
    gtk_tree_view_set_cursor(GTK_TREE_VIEW(cwin->view),
			     path, NULL, FALSE);
    gtk_tree_path_free(path);

  } else {
    GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(cwin->view));

    gtk_tree_selection_unselect_all(selection);
    uim_cand_win_gtk_update_label(cwin);
  }
}

void
uim_cand_win_vertical_gtk_set_page(UIMCandWinVerticalGtk *vertical_cwin, gint page)
{
  guint len, new_page;
  gint new_index;
  UIMCandWinGtk *cwin;

  g_return_if_fail(UIM_IS_CAND_WIN_VERTICAL_GTK(vertical_cwin));
  cwin = UIM_CAND_WIN_GTK(vertical_cwin);
  g_return_if_fail(cwin->stores);

  len = cwin->stores->len;
  g_return_if_fail(len);

  if (page < 0)
    new_page = len - 1;
  else if (page >= (gint) len)
    new_page = 0;
  else
    new_page = page;

  gtk_tree_view_set_model(GTK_TREE_VIEW(cwin->view),
			  GTK_TREE_MODEL(cwin->stores->pdata[new_page]));

  cwin->page_index = new_page;

  if (cwin->display_limit) {
    if (cwin->candidate_index >= 0)
      new_index
        = (new_page * cwin->display_limit) + (cwin->candidate_index % cwin->display_limit);
    else
      new_index = -1;
  } else {
    new_index = cwin->candidate_index;
  }

  if (new_index >= (gint) cwin->nr_candidates)
    new_index = cwin->nr_candidates - 1;

  uim_cand_win_gtk_set_index(cwin, new_index);
}
