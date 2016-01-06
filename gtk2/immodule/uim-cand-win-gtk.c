/*

  copyright (c) 2003-2013 uim Project https://github.com/uim/uim

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
static void	uim_cand_win_gtk_real_set_index		(UIMCandWinGtk *cwin,
							 gint index);
static void	uim_cand_win_gtk_real_set_page		(UIMCandWinGtk *cwin,
							 gint page);
static void	uim_cand_win_gtk_real_create_sub_window(UIMCandWinGtk *cwin);
static void	uim_cand_win_gtk_real_layout_sub_window	(UIMCandWinGtk *cwin);

static void	pagebutton_clicked(GtkButton *button, gpointer data);

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

  klass->set_index = uim_cand_win_gtk_real_set_index;
  klass->set_page = uim_cand_win_gtk_real_set_page;
  klass->create_sub_window = uim_cand_win_gtk_real_create_sub_window;
  klass->layout_sub_window = uim_cand_win_gtk_real_layout_sub_window;
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
  GtkWidget *frame;
  GtkWidget *vbox;
  GtkWidget *hbox;

  /* init struct */
  cwin->scrolled_window = gtk_scrolled_window_new(NULL, NULL);
  cwin->num_label = gtk_label_new("");

  cwin->stores = g_ptr_array_new();

  cwin->nr_candidates = 0;
  cwin->display_limit = 0;
  cwin->candidate_index = -1;
  cwin->page_index = 0;

  uim_cand_win_gtk_get_window_pos_type(cwin);

  cwin->block_index_selection = FALSE;
  cwin->index_changed = FALSE;

  cwin->cursor.x = cwin->cursor.y = 0;
  cwin->cursor.width = cwin->cursor.height = 0;

  cwin->sub_window.window          = NULL;
  cwin->sub_window.scrolled_window = NULL;
  cwin->sub_window.text_view       = NULL;
  cwin->sub_window.active          = FALSE;

  /* build window */
#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
#else
  vbox = gtk_vbox_new(FALSE, 0);
#endif

  gtk_box_pack_start(GTK_BOX(vbox), cwin->scrolled_window, TRUE, TRUE, 0);
  uim_cand_win_gtk_set_scrollable(cwin, FALSE);

  /* hbox with prev and next page button: [[<] num_label [>]] */
#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
#else
  hbox = gtk_hbox_new(FALSE, 0);
#endif
  cwin->prev_page_button = gtk_button_new_with_label("<");
  cwin->next_page_button = gtk_button_new_with_label(">");
  gtk_box_pack_start(GTK_BOX(hbox), GTK_WIDGET(cwin->prev_page_button),
      TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), cwin->num_label, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(hbox), GTK_WIDGET(cwin->next_page_button),
      TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
  g_signal_connect(cwin->prev_page_button, "clicked",
      G_CALLBACK(pagebutton_clicked), cwin);
  g_signal_connect(cwin->next_page_button, "clicked",
      G_CALLBACK(pagebutton_clicked), cwin);

  frame = gtk_frame_new(NULL);

  gtk_container_add(GTK_CONTAINER(frame), vbox);
  gtk_container_add(GTK_CONTAINER(cwin), frame);
  gtk_container_set_border_width(GTK_CONTAINER(cwin), 0);

  /* set size */
  /* gtk_widget_set_size_request(cwin->view, -1, -1); */

  /* show children */
  gtk_widget_show(cwin->scrolled_window);
  gtk_widget_show_all(hbox);
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

void
uim_cand_win_gtk_update_label(UIMCandWinGtk *cwin)
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

void
uim_cand_win_gtk_set_nr_candidates(UIMCandWinGtk *cwin,
				   guint nr,
				   guint display_limit)
{
  gint i, nr_stores = 1;

  g_return_if_fail(UIM_IS_CAND_WIN_GTK(cwin));

  cwin->nr_candidates = nr;
  cwin->display_limit = display_limit;

  if (nr <= display_limit) {
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->prev_page_button), FALSE);
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->next_page_button), FALSE);
  } else {
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->prev_page_button), TRUE);
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->next_page_button), TRUE);
  }

  if (cwin->stores == NULL)
    cwin->stores = g_ptr_array_new();

  /* remove old data */
  if (cwin->page_index >= 0 && cwin->page_index < (int) cwin->stores->len) {
    /* Remove data from current page to shrink the window */
    if (cwin->stores->pdata[cwin->page_index]) {
      cwin->block_index_selection = TRUE;
      gtk_list_store_clear(cwin->stores->pdata[cwin->page_index]);
      cwin->block_index_selection = FALSE;
    }
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

  if (cwin->nr_candidates <= cwin->display_limit) {
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->prev_page_button), FALSE);
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->next_page_button), FALSE);
  } else {
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->prev_page_button), TRUE);
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->next_page_button), TRUE);
  }

  uim_cand_win_gtk_set_page(cwin, 0);

  uim_cand_win_gtk_update_label(cwin);
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
  UIM_CAND_WIN_GTK_GET_CLASS (cwin)->set_index(cwin, index);
}

static void
uim_cand_win_gtk_real_set_index(UIMCandWinGtk *cwin, gint index)
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
  UIM_CAND_WIN_GTK_GET_CLASS (cwin)->set_page(cwin, page);
}

static void
uim_cand_win_gtk_real_set_page(UIMCandWinGtk *cwin, gint page)
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

  /* XXX: change to call virtual update_view()
  gtk_tree_view_set_model(GTK_TREE_VIEW(cwin->view),
			  GTK_TREE_MODEL(cwin->stores->pdata[new_page]));
  */

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

static void
pagebutton_clicked(GtkButton *button, gpointer data)
{
  UIMCandWinGtk *cwin = UIM_CAND_WIN_GTK(data);
  gboolean has_candidates = FALSE;

  if (cwin->candidate_index < 0) {
    /* if candidate_index < 0, "index-changed" signal is not emitted
     * and candidates for new page is not set.
     */
    cwin->candidate_index = cwin->page_index * cwin->display_limit;
  }
  if (button == GTK_BUTTON(cwin->prev_page_button)) {
    uim_cand_win_gtk_shift_page(cwin, FALSE);
  } else if (button == GTK_BUTTON(cwin->next_page_button)) {
    uim_cand_win_gtk_shift_page(cwin, TRUE);
  } else {
    return;
  }
  if (cwin->stores->pdata[cwin->page_index]) {
    has_candidates = TRUE;
  }
  if (cwin->candidate_index >= 0) {
    g_signal_emit(G_OBJECT(cwin),
                  cand_win_gtk_signals[INDEX_CHANGED_SIGNAL], 0);
  }
  /* if got candidates, update view */
  if (!has_candidates && cwin->stores->pdata[cwin->page_index]) {
    uim_cand_win_gtk_set_page(cwin, cwin->page_index);
  }
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

#if GTK_CHECK_VERSION(3, 0, 0)
  gtk_widget_get_preferred_size(GTK_WIDGET(cwin), &req, NULL);
#else
  gtk_widget_size_request(GTK_WIDGET(cwin), &req);
#endif
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
    /* x = topwin_x + cursor_x - cw_wi; */
    x = sc_wi - cw_wi;
  } else {
    x = topwin_x + cursor_x;
  }

  if (sc_he <  topwin_y + cursor_y +  cwin->cursor.height + cw_he) {
    y = topwin_y + cursor_y - cw_he;
  } else {
    y = topwin_y + cursor_y +  cwin->cursor.height;
  }

  gtk_window_move(GTK_WINDOW(cwin), x, y);
#if GTK_CHECK_VERSION(3, 7, 8)
  if (gtk_widget_get_mapped(cwin->view) && GTK_IS_TREE_VIEW(cwin->view))
    gtk_widget_queue_resize_no_redraw(cwin->view);
#endif

  uim_cand_win_gtk_layout_sub_window(cwin);
}

void
uim_cand_win_gtk_set_cursor_location(UIMCandWinGtk *cwin, GdkRectangle *area)
{
  g_return_if_fail(UIM_CAND_WIN_GTK(cwin));
  g_return_if_fail(area);

  cwin->cursor = *area;
}

void
uim_cand_win_gtk_create_sub_window(UIMCandWinGtk *cwin)
{
  UIM_CAND_WIN_GTK_GET_CLASS (cwin)->create_sub_window(cwin);
}

#define UIM_ANNOTATION_WIN_WIDTH 200
#define UIM_ANNOTATION_WIN_HEIGHT 200

void
uim_cand_win_gtk_real_create_sub_window(UIMCandWinGtk *cwin)
{
  GtkWidget *window, *scrwin, *text_view, *frame;
  GdkGeometry hints;

  if (cwin->sub_window.window)
    return;

  cwin->sub_window.window = window = gtk_window_new(GTK_WINDOW_POPUP);

  frame = gtk_frame_new(NULL);
  gtk_container_set_border_width(GTK_CONTAINER(frame), 0);

  hints.min_width = UIM_ANNOTATION_WIN_WIDTH;
  hints.min_height = UIM_ANNOTATION_WIN_HEIGHT;
  hints.max_width = UIM_ANNOTATION_WIN_WIDTH;
  hints.max_height = UIM_ANNOTATION_WIN_HEIGHT;
  gtk_window_set_geometry_hints(GTK_WINDOW(window), frame, &hints, GDK_HINT_MAX_SIZE | GDK_HINT_MIN_SIZE);

  cwin->sub_window.scrolled_window = scrwin = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrwin),
				 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  cwin->sub_window.text_view = text_view = gtk_text_view_new();
  gtk_text_view_set_editable(GTK_TEXT_VIEW(text_view), FALSE);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text_view), GTK_WRAP_WORD_CHAR);
  gtk_widget_show(text_view);

  gtk_container_add(GTK_CONTAINER(scrwin), text_view);
  gtk_container_add(GTK_CONTAINER(frame), scrwin);
  gtk_container_add(GTK_CONTAINER(window), frame);
  gtk_widget_show(frame);
  gtk_widget_show(scrwin);
  gtk_widget_show(text_view);
}


void
uim_cand_win_gtk_layout_sub_window(UIMCandWinGtk *cwin)
{
  UIM_CAND_WIN_GTK_GET_CLASS (cwin)->layout_sub_window(cwin);
}

void
uim_cand_win_gtk_real_layout_sub_window(UIMCandWinGtk *cwin)
{
#if GTK_CHECK_VERSION(2, 90, 0)
  gint x, y, w, h, sw, sh, x2, y2, w2, h2;
#else
  gint x, y, w, h, d, sw, sh, x2, y2, w2, h2, d2;
#endif
  GdkRectangle rect;
  GtkTreePath *path;
  GtkTreeViewColumn *focus_column;

  if (!cwin->sub_window.window)
    return;

  gtk_tree_view_get_cursor(GTK_TREE_VIEW(cwin->view), &path, &focus_column);
  gtk_tree_view_get_cell_area(GTK_TREE_VIEW(cwin->view), path, NULL, &rect);
  gtk_tree_path_free(path);

#if GTK_CHECK_VERSION(2, 90, 0)
  gdk_window_get_geometry(gtk_widget_get_window(GTK_WIDGET(cwin)),
			  &x, &y, &w, &h);
#else
  gdk_window_get_geometry(gtk_widget_get_window(GTK_WIDGET(cwin)),
			  &x, &y, &w, &h, &d);
#endif
  gdk_window_get_origin(gtk_widget_get_window(GTK_WIDGET(cwin)), &x, &y);

  sw = gdk_screen_get_width  (gdk_screen_get_default ());
  sh = gdk_screen_get_height (gdk_screen_get_default ()); 
#if GTK_CHECK_VERSION(2, 90, 0)
  gdk_window_get_geometry(gtk_widget_get_window(cwin->sub_window.window),
			  &x2, &y2, &w2, &h2);
#else
  gdk_window_get_geometry(gtk_widget_get_window(cwin->sub_window.window),
			  &x2, &y2, &w2, &h2, &d2);
#endif
  if (x + w + w2 > sw)
    x = x - w2;
  else
    x = x + w;

  if ((y + rect.y + h2) > sh)
    y = sh - h2;
  else
    y = y + rect.y;

  gtk_window_move(GTK_WINDOW(cwin->sub_window.window), x, y);
}
