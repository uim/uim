/*

  copyright (c) 2003-2010 uim Project http://code.google.com/p/uim/

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

#include "uim-cand-win-gtk.h"
#include <string.h>
#include <stdlib.h>
#include <uim/uim.h>
#include <uim/uim-scm.h>

#define NR_CANDIDATES 20 /* FIXME! not used yet */
#define DEFAULT_MIN_WINDOW_WIDTH 80

enum {
  INDEX_CHANGED_SIGNAL,
  NR_SIGNALS
};

enum {
  TERMINATOR = -1,
  COLUMN_HEADING,
  COLUMN_CANDIDATE,
  COLUMN_ANNOTATION,
  NR_COLUMNS
};

static void	uim_cand_win_gtk_init		(UIMCandWinGtk *cwin);
static void	uim_cand_win_gtk_class_init	(UIMCandWinGtkClass *klass);
static void	uim_cand_win_gtk_dispose	(GObject *obj);
static void	uim_cand_win_gtk_map		(GtkWidget *widget);
static void	uim_cand_win_gtk_unmap		(GtkWidget *widget);
static void	uim_cand_win_gtk_create_sub_window(UIMCandWinGtk *cwin);
static void	uim_cand_win_gtk_layout_sub_window(UIMCandWinGtk *cwin);

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


static GType cand_win_type = 0;
static GTypeInfo const object_info = {
  sizeof (UIMCandWinGtkClass),
  (GBaseInitFunc) NULL,
  (GBaseFinalizeFunc) NULL,
  (GClassInitFunc) uim_cand_win_gtk_class_init,
  (GClassFinalizeFunc) NULL,
  NULL,                       /* class_data */
  sizeof (UIMCandWinGtk),
  0,                          /* n_preallocs */
  (GInstanceInitFunc) uim_cand_win_gtk_init,
};

static GtkWindowClass *parent_class = NULL;
static gint cand_win_gtk_signals[NR_SIGNALS] = {0};

GType
uim_cand_win_gtk_get_type(void)
{
  if (!cand_win_type)
    cand_win_type = g_type_register_static(GTK_TYPE_WINDOW, "UIMCandWinGtk",
					   &object_info, (GTypeFlags)0);
  return cand_win_type;
}

GType
uim_cand_win_gtk_register_type(GTypeModule *module)
{
  if (!cand_win_type)
    cand_win_type = g_type_module_register_type(module,
						GTK_TYPE_WINDOW,
						"UIMCandWinGtk",
						&object_info, 0);
  return cand_win_type;
}

static void
uim_cand_win_gtk_class_init (UIMCandWinGtkClass *klass)
{
  GObjectClass *object_class = (GObjectClass *) klass;
  GtkWidgetClass *widget_class = (GtkWidgetClass *) klass;

  parent_class = g_type_class_peek_parent (klass);
  object_class->dispose = uim_cand_win_gtk_dispose;

  cand_win_gtk_signals[INDEX_CHANGED_SIGNAL]
    = g_signal_new("index-changed",
		   G_TYPE_FROM_CLASS(klass),
		   G_SIGNAL_RUN_FIRST,
		   G_STRUCT_OFFSET(UIMCandWinGtkClass, index_changed),
		   NULL, NULL,
		   g_cclosure_marshal_VOID__VOID,
		   G_TYPE_NONE, 0);

  widget_class->map   = uim_cand_win_gtk_map;
  widget_class->unmap = uim_cand_win_gtk_unmap;
}

void
uim_cand_win_gtk_get_window_pos_type(UIMCandWinGtk *cwin)
{
  char *win_pos;

  win_pos = uim_scm_symbol_value_str("candidate-window-position");
  if (win_pos && !strcmp(win_pos, "left")) {
    cwin->position = UIM_CAND_WIN_POS_LEFT;
  } else if (win_pos && !strcmp(win_pos, "right")) {
    cwin->position = UIM_CAND_WIN_POS_RIGHT;
  } else {
    cwin->position = UIM_CAND_WIN_POS_CARET;
  }
  free(win_pos);
}

static void
uim_cand_win_gtk_init (UIMCandWinGtk *cwin)
{
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkWidget *frame;
  GtkWidget *vbox;
  GtkTreeSelection *selection;

  /* init struct */
  cwin->scrolled_window = gtk_scrolled_window_new(NULL, NULL);
  cwin->view = gtk_tree_view_new();
  cwin->num_label = gtk_label_new("");

  cwin->stores = g_ptr_array_new();

  cwin->nr_candidates = 0;
  cwin->display_limit = 0;
  cwin->candidate_index = -1;
  cwin->page_index = 0;

  uim_cand_win_gtk_get_window_pos_type(cwin);

  cwin->cursor.x = cwin->cursor.y = 0;
  cwin->cursor.width = cwin->cursor.height = 0;

  cwin->sub_window.window          = NULL;
  cwin->sub_window.scrolled_window = NULL;
  cwin->sub_window.text_view       = NULL;
  cwin->sub_window.active          = FALSE;

  /* build window */
  vbox = gtk_vbox_new(FALSE, 0);

  gtk_box_pack_start(GTK_BOX(vbox), cwin->scrolled_window, TRUE, TRUE, 0);
  uim_cand_win_gtk_set_scrollable(cwin, FALSE);

  gtk_container_add(GTK_CONTAINER(cwin->scrolled_window), cwin->view);
  gtk_box_pack_start(GTK_BOX(vbox), cwin->num_label, FALSE, FALSE, 0);

  frame = gtk_frame_new(NULL);

  gtk_container_add(GTK_CONTAINER(frame), vbox);
  gtk_container_add(GTK_CONTAINER(cwin), frame);
  gtk_container_set_border_width(GTK_CONTAINER(cwin), 0);

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(cwin->view));

  gtk_tree_selection_set_select_function(selection,
					 tree_selection_change,
					 cwin,
					 NULL);
  g_signal_connect (G_OBJECT(selection), "changed",
		    G_CALLBACK(tree_selection_changed), cwin);

  renderer = gtk_cell_renderer_text_new();
  g_object_set(renderer, "scale", 0.8, NULL);

  column = gtk_tree_view_column_new_with_attributes("No",
						    renderer,
						    "text", COLUMN_HEADING,
						    NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(cwin->view), column);
  gtk_tree_view_column_set_sizing (column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);

  renderer = gtk_cell_renderer_text_new();
  g_object_set(renderer, "scale", 1.2, NULL);
  /*  g_object_set(renderer, "size-points", 20.0, NULL); */
  column = gtk_tree_view_column_new_with_attributes("Text",
						    renderer,
						    "text", COLUMN_CANDIDATE,
						    NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(cwin->view), column);
  gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(cwin->view), TRUE);
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(cwin->view), FALSE);
  gtk_tree_view_column_set_sizing (column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);

  g_signal_connect(G_OBJECT(cwin->view), "button-press-event",
		   G_CALLBACK(tree_view_button_press), cwin);

  /* set size */
  /* gtk_widget_set_size_request(cwin->view, -1, -1); */

  /* show children */
  gtk_widget_show(cwin->scrolled_window);
  gtk_widget_show(cwin->view);
  gtk_widget_show(cwin->num_label);
  gtk_widget_show(vbox);
  gtk_widget_show(frame);

  gtk_widget_set_size_request(cwin->num_label, DEFAULT_MIN_WINDOW_WIDTH, -1);
  gtk_window_set_default_size(GTK_WINDOW(cwin), DEFAULT_MIN_WINDOW_WIDTH, -1);
  gtk_window_set_resizable(GTK_WINDOW(cwin), FALSE);
}

static void
uim_cand_win_gtk_dispose (GObject *obj)
{
  UIMCandWinGtk *cwin;

  g_return_if_fail(UIM_IS_CAND_WIN_GTK(obj));

  cwin = UIM_CAND_WIN_GTK(obj);

  if (cwin->stores) {
    guint i;

    for (i = 0; i < cwin->stores->len; i++) {
      if (cwin->stores->pdata[i])
	g_object_unref(G_OBJECT(cwin->stores->pdata[i]));
    }
    g_ptr_array_free(cwin->stores, TRUE);
    cwin->stores = NULL;
  }

  if (cwin->sub_window.window) {
    gtk_widget_destroy(cwin->sub_window.window);
    cwin->sub_window.window          = NULL;
    cwin->sub_window.scrolled_window = NULL;
    cwin->sub_window.text_view       = NULL;
  }

  if (G_OBJECT_CLASS (parent_class)->dispose)
    G_OBJECT_CLASS (parent_class)->dispose(obj);
}

static void        
uim_cand_win_gtk_map (GtkWidget *widget)
{
  UIMCandWinGtk *cwin = UIM_CAND_WIN_GTK(widget);

  if (cwin->sub_window.active)
    gtk_widget_show(cwin->sub_window.window);

  if (GTK_WIDGET_CLASS (parent_class)->map)
    GTK_WIDGET_CLASS (parent_class)->map(widget);
}


static void        
uim_cand_win_gtk_unmap (GtkWidget *widget)
{
  UIMCandWinGtk *cwin = UIM_CAND_WIN_GTK(widget);

  if (cwin->sub_window.window)
    gtk_widget_hide(cwin->sub_window.window);

  if (GTK_WIDGET_CLASS (parent_class)->unmap)
    GTK_WIDGET_CLASS (parent_class)->unmap(widget);
}

UIMCandWinGtk *
uim_cand_win_gtk_new (void)
{
  GObject *obj = g_object_new(UIM_TYPE_CAND_WIN_GTK,
			      "type", GTK_WINDOW_POPUP,
			      NULL);
  return UIM_CAND_WIN_GTK(obj);
}

static void
update_label(UIMCandWinGtk *cwin)
{
  char label_str[20];

  if (cwin->candidate_index >= 0)
    g_snprintf(label_str, sizeof(label_str), "%d / %d",
	       cwin->candidate_index + 1 , cwin->nr_candidates);
  else
    g_snprintf(label_str, sizeof(label_str), "- / %d",
	       cwin->nr_candidates);

  gtk_label_set_text(GTK_LABEL(cwin->num_label), label_str);
}

static gboolean
tree_selection_change(GtkTreeSelection *selection,
		      GtkTreeModel *model,
		      GtkTreePath *path,
		      gboolean path_currently_selected,
		      gpointer data)
{
  UIMCandWinGtk *cwin = data;
  gint *indicies;
  gint idx;

  if (!cwin)
    return TRUE;

  indicies = gtk_tree_path_get_indices(path);
  g_return_val_if_fail(indicies, TRUE);
  idx = *indicies + cwin->display_limit * cwin->page_index;

  if (!path_currently_selected && cwin->candidate_index != idx) {
    if (cwin->candidate_index >= 0) {
      cwin->candidate_index = idx;
      g_signal_emit(G_OBJECT(cwin),
		    cand_win_gtk_signals[INDEX_CHANGED_SIGNAL], 0);
    }

    update_label(cwin);

    if (cwin->candidate_index < 0)
      return FALSE;
    else
      return TRUE;
  } else {
    update_label(cwin);

    return TRUE;
  }
}

static gboolean
tree_selection_changed(GtkTreeSelection *selection,
		       gpointer data)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  UIMCandWinGtk *cwin = UIM_CAND_WIN_GTK(data);

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

  return TRUE;
}

static gboolean
tree_view_button_press(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
  UIMCandWinGtk *cwin;
  GtkTreePath *path;
  gboolean exist, retval = FALSE;
  gint *indicies;

  g_return_val_if_fail(GTK_IS_TREE_VIEW(widget), FALSE);
  g_return_val_if_fail(UIM_CAND_WIN_GTK(data), FALSE);

  cwin = UIM_CAND_WIN_GTK(data);

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
uim_cand_win_gtk_set_nr_candidates(UIMCandWinGtk *cwin,
				   guint nr,
				   guint display_limit)
{
  gint i, nr_stores = 1;

  g_return_if_fail(UIM_IS_CAND_WIN_GTK(cwin));

  cwin->nr_candidates = nr;
  cwin->display_limit = display_limit;

  if (cwin->stores == NULL)
    cwin->stores = g_ptr_array_new();

  /* remove old data */
  if (cwin->page_index >= 0 && cwin->page_index < (int) cwin->stores->len) {
    /* Remove data from current page to shrink the window */
    if (cwin->stores->pdata[cwin->page_index])
      gtk_list_store_clear(cwin->stores->pdata[cwin->page_index]);
  }
  for (i = cwin->stores->len - 1; i >= 0; i--) {
    GtkListStore *store = g_ptr_array_remove_index(cwin->stores, i);
    if (G_OBJECT(store))
      g_object_unref(G_OBJECT(store));
  }
  /* calculate number of GtkListStores to create */
  if (display_limit) {
    nr_stores = nr / display_limit;
    if (cwin->nr_candidates > display_limit * nr_stores)
      nr_stores++;
  }

  /* setup dummy array */
  for (i = 0; i < nr_stores; i++)
    g_ptr_array_add(cwin->stores, NULL);
}

void
uim_cand_win_gtk_set_candidates(UIMCandWinGtk *cwin,
				guint display_limit,
				GSList *candidates)
{
  gint i, nr_stores = 1;

  g_return_if_fail(UIM_IS_CAND_WIN_GTK(cwin));

  if (cwin->stores == NULL)
    cwin->stores = g_ptr_array_new();

  /* remove old data */
  if (cwin->page_index >= 0 && cwin->page_index < (int) cwin->stores->len) {
    /* Remove data from current page to shrink the window */
    if (cwin->stores->pdata[cwin->page_index])
      gtk_list_store_clear(cwin->stores->pdata[cwin->page_index]);
  }
  for (i = cwin->stores->len - 1; i >= 0; i--) {
    GtkListStore *store = g_ptr_array_remove_index(cwin->stores, i);
    if (store)
      g_object_unref(G_OBJECT(store));
  }

  cwin->candidate_index = -1;
  cwin->nr_candidates = g_slist_length(candidates);
  cwin->display_limit = display_limit;

  cwin->sub_window.active = FALSE;

  if (candidates == NULL)
    return;

  /* calculate number of GtkListStores to create */
  if (display_limit) {
    nr_stores = cwin->nr_candidates / display_limit;
    if (cwin->nr_candidates > display_limit * nr_stores)
      nr_stores++;
  }

  /* create GtkListStores, and set candidates */
  for (i = 0; i < nr_stores; i++) {
    GtkListStore *store = gtk_list_store_new(NR_COLUMNS,
					     G_TYPE_STRING,
					     G_TYPE_STRING,
					     G_TYPE_STRING);
    GSList *node;
    guint j;

    g_ptr_array_add(cwin->stores, store);

    /* set candidates */
    for (j = i * display_limit, node = g_slist_nth(candidates, j);
	 display_limit ? j < display_limit * (i + 1) : j < cwin->nr_candidates;
	 j++, node = g_slist_next(node))
    {
      GtkTreeIter ti;

      if (node) {
	uim_candidate cand = node->data;
	gtk_list_store_append(store, &ti);
        gtk_list_store_set(store, &ti,
			   COLUMN_HEADING,    uim_candidate_get_heading_label(cand),
			   COLUMN_CANDIDATE,  uim_candidate_get_cand_str(cand),
			   COLUMN_ANNOTATION, uim_candidate_get_annotation_str(cand),
			   TERMINATOR);
      } else {
#if 0
        /*
	 * 2004-07-22 Takuro Ashie <ashie@good-day.co.jp>
	 *
	 * FIXME!:
	 *   I think we shoudn't set any data for empty row.
	 *   It may cause incorrect action.
	 */
	gtk_list_store_append(store, &ti);
        gtk_list_store_set(store, &ti,
			   COLUMN_HEADING,    "",
			   COLUMN_CANDIDATE,  "",
			   COLUMN_ANNOTATION, NULL,
			   TERMINATOR);
#endif
      }
    }
  }

  uim_cand_win_gtk_set_page(cwin, 0);

  update_label(cwin);
}

void
uim_cand_win_gtk_set_page_candidates(UIMCandWinGtk *cwin,
				     guint page,
				     GSList *candidates)
{
  GtkListStore *store;
  GSList *node;
  gint j, len;

  g_return_if_fail(UIM_IS_CAND_WIN_GTK(cwin));

  if (candidates == NULL)
    return;

  cwin->sub_window.active = FALSE;
  len = g_slist_length(candidates);

  /* create GtkListStores, and set candidates */
  store = gtk_list_store_new(NR_COLUMNS,
			     G_TYPE_STRING,
			     G_TYPE_STRING,
			     G_TYPE_STRING);

  cwin->stores->pdata[page] = store;

  /* set candidates */
  for (j = 0, node = g_slist_nth(candidates, j);
       j < len;
       j++, node = g_slist_next(node))
  {
    GtkTreeIter ti;

    if (node) {
      uim_candidate cand = node->data;
      gtk_list_store_append(store, &ti);
      gtk_list_store_set(store, &ti,
			 COLUMN_HEADING,    uim_candidate_get_heading_label(cand),
			 COLUMN_CANDIDATE,  uim_candidate_get_cand_str(cand),
			 COLUMN_ANNOTATION, uim_candidate_get_annotation_str(cand),
			 TERMINATOR);
    }
  }
}

void
uim_cand_win_gtk_clear_candidates(UIMCandWinGtk *cwin)
{
  uim_cand_win_gtk_set_candidates(cwin, 0, NULL);
}

guint
uim_cand_win_gtk_get_nr_candidates(UIMCandWinGtk *cwin)
{
  g_return_val_if_fail(UIM_IS_CAND_WIN_GTK(cwin), 0);

  return cwin->nr_candidates;
}

gint
uim_cand_win_gtk_get_index(UIMCandWinGtk *cwin)
{
  g_return_val_if_fail(UIM_IS_CAND_WIN_GTK(cwin), -1);

  return cwin->candidate_index;
}

void
uim_cand_win_gtk_set_index(UIMCandWinGtk *cwin, gint index)
{
  gint new_page;

  g_return_if_fail(UIM_IS_CAND_WIN_GTK(cwin));

  if (index >= (gint) cwin->nr_candidates)
    cwin->candidate_index = 0;
  else
    cwin->candidate_index = index;

  if (cwin->candidate_index >= 0 && cwin->display_limit)
    new_page = cwin->candidate_index / cwin->display_limit;
  else
    new_page = cwin->page_index;

  if (cwin->page_index != new_page)
    uim_cand_win_gtk_set_page(cwin, new_page);

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
    update_label(cwin);
  }
}

guint
uim_cand_win_gtk_get_nr_pages(UIMCandWinGtk *cwin)
{
  g_return_val_if_fail(UIM_IS_CAND_WIN_GTK(cwin), 0);
  g_return_val_if_fail(UIM_IS_CAND_WIN_GTK(cwin->stores), 0);

  return cwin->stores->len;
}

gint
uim_cand_win_gtk_get_page(UIMCandWinGtk *cwin)
{
  g_return_val_if_fail(UIM_IS_CAND_WIN_GTK(cwin), -1);

  return cwin->page_index;
}

void
uim_cand_win_gtk_set_page(UIMCandWinGtk *cwin, gint page)
{
  guint len, new_page;
  gint new_index;

  g_return_if_fail(UIM_IS_CAND_WIN_GTK(cwin));
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

void
uim_cand_win_gtk_set_scrollable(UIMCandWinGtk *cwin, gboolean scrollable)
{
  GtkPolicyType policy = scrollable ? GTK_POLICY_AUTOMATIC : GTK_POLICY_NEVER;

  g_return_if_fail(UIM_IS_CAND_WIN_GTK(cwin));

  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cwin->scrolled_window),
				 GTK_POLICY_NEVER, policy);
}

void
uim_cand_win_gtk_shift_page(UIMCandWinGtk *cwin, gboolean forward)
{
  g_return_if_fail(UIM_IS_CAND_WIN_GTK(cwin));

  if (forward) {
    uim_cand_win_gtk_set_page(cwin, cwin->page_index + 1);
  } else {
    uim_cand_win_gtk_set_page(cwin, cwin->page_index - 1);
  }
}

guint
uim_cand_win_gtk_query_new_page_by_cand_select(UIMCandWinGtk *cwin, gint index)
{
  guint new_page;

  g_return_val_if_fail(UIM_IS_CAND_WIN_GTK(cwin), 0);

  if (index >= (gint)cwin->nr_candidates)
    index = 0;

  if (index >= 0 && cwin->display_limit)
    new_page = index / cwin->display_limit;
  else
    new_page = cwin->page_index;

  return new_page;
}

guint
uim_cand_win_gtk_query_new_page_by_shift_page(UIMCandWinGtk *cwin,
					    gboolean forward)
{
  gint index;
  guint new_page, len;

  g_return_val_if_fail(UIM_IS_CAND_WIN_GTK(cwin), 0);

  len = cwin->stores->len;

  if (forward)
    index = cwin->page_index + 1;
  else
    index = cwin->page_index - 1;

  if (index < 0)
    new_page = len - 1;
  else if (index >= (gint)len)
    new_page = 0;
  else
    new_page = index;

  return new_page;
}

void
uim_cand_win_gtk_layout(UIMCandWinGtk *cwin,
			gint topwin_x, gint topwin_y,
			gint topwin_width, gint topwin_height)
{
  GtkRequisition req;
  int  x, y;
  int  cursor_x, cursor_y;
  int  sc_he, cw_he; /*screen height, candidate window height*/
  int  sc_wi, cw_wi;

  g_return_if_fail(UIM_IS_CAND_WIN_GTK(cwin));

  gtk_widget_size_request(GTK_WIDGET(cwin), &req);
  cw_wi = req.width;
  cw_he = req.height;

  sc_he = gdk_screen_get_height(gdk_screen_get_default ());
  sc_wi = gdk_screen_get_width (gdk_screen_get_default ());

  /* FIXME */
  switch (cwin->position) {
  case UIM_CAND_WIN_POS_LEFT:
    cursor_x = 0;
    break;
  case UIM_CAND_WIN_POS_RIGHT:
    cursor_x = topwin_width - cw_wi;
    break;
  default:
    cursor_x = cwin->cursor.x;
    break;
  }
  cursor_y = cwin->cursor.y;

  if (sc_wi <  topwin_x + cursor_x + cw_wi) {
    x = topwin_x + cursor_x - cw_wi;
  } else {
    x = topwin_x + cursor_x;
  }

  if (sc_he <  topwin_y + cursor_y +  cwin->cursor.height + cw_he) {
    y = topwin_y + cursor_y - cw_he;
  } else {
    y = topwin_y + cursor_y +  cwin->cursor.height;
  }

  gtk_window_move(GTK_WINDOW(cwin), x, y);

  uim_cand_win_gtk_layout_sub_window(cwin);
}

void
uim_cand_win_gtk_set_cursor_location(UIMCandWinGtk *cwin, GdkRectangle *area)
{
  g_return_if_fail(UIM_CAND_WIN_GTK(cwin));
  g_return_if_fail(area);

  cwin->cursor = *area;
}

#define UIM_ANNOTATION_WIN_WIDTH 200
#define UIM_ANNOTATION_WIN_HEIGHT 230

static void
uim_cand_win_gtk_create_sub_window(UIMCandWinGtk *cwin)
{
  GtkWidget *window, *scrwin, *text_view, *frame;
  GdkGeometry hints;

  if (cwin->sub_window.window)
    return;

  cwin->sub_window.window = window = gtk_window_new(GTK_WINDOW_POPUP);
  gtk_window_set_default_size(GTK_WINDOW(window), UIM_ANNOTATION_WIN_WIDTH, UIM_ANNOTATION_WIN_HEIGHT);

  frame = gtk_frame_new(NULL);
  gtk_container_set_border_width(GTK_CONTAINER(frame), 0);

  hints.max_width = UIM_ANNOTATION_WIN_WIDTH;
  hints.max_height = UIM_ANNOTATION_WIN_HEIGHT;
  gtk_window_set_geometry_hints(GTK_WINDOW(window), frame, &hints, GDK_HINT_MAX_SIZE);

  cwin->sub_window.scrolled_window = scrwin = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrwin),
				 GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

  cwin->sub_window.text_view = text_view = gtk_text_view_new();
  gtk_text_view_set_editable(GTK_TEXT_VIEW(text_view), FALSE);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text_view), GTK_WRAP_WORD);
  gtk_widget_show(text_view);

  gtk_container_add(GTK_CONTAINER(scrwin), text_view);
  gtk_container_add(GTK_CONTAINER(frame), scrwin);
  gtk_container_add(GTK_CONTAINER(window), frame);
  gtk_widget_show(frame);
  gtk_widget_show(scrwin);
  gtk_widget_show(text_view);
}

static void
uim_cand_win_gtk_layout_sub_window(UIMCandWinGtk *cwin)
{
  gint x, y, w, h, d, sw, sh, x2, y2, w2, h2, d2;

  if (!cwin->sub_window.window)
    return;

  gdk_window_get_geometry(GTK_WIDGET(cwin)->window,
			  &x, &y, &w, &h, &d);
  gdk_window_get_origin(GTK_WIDGET(cwin)->window, &x, &y);

  sw = gdk_screen_get_width  (gdk_screen_get_default ());
  sh = gdk_screen_get_height (gdk_screen_get_default ()); 
 gdk_window_get_geometry(cwin->sub_window.window->window,
			  &x2, &y2, &w2, &h2, &d2);
  if (x + w + w2 > sw)
    x = x - w2;
  else
    x = x + w;

  gtk_window_move(GTK_WINDOW(cwin->sub_window.window), x, y);
}
