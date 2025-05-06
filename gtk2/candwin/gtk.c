/*

  Copyright (c) 2003-2013 uim Project https://github.com/uim/uim

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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <glib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "../gtk2/immodule/caret-state-indicator.h"

#define UIM_TYPE_CANDIDATE_WINDOW	(candidate_window_get_type())
#define UIM_CANDIDATE_WINDOW(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), candidate_window_get_type(), UIMCandidateWindow))
#define UIM_IS_CANDIDATE_WINDOW(obj)	(G_TYPE_CHECK_INSTANCE_TYPE ((obj), UIM_TYPE_CANDIDATE_WINDOW))
#define UIM_IS_CANDIDATE_WINDOW_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), UIM_TYPE_CANDIDATE_WINDOW))
#define UIM_CANDIDATE_WINDOW_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), UIM_TYPE_CANDIDATE_WINDOW, UIMCandidateWindowClass))

#define UIM_ANNOTATION_WIN_WIDTH 200
#define UIM_ANNOTATION_WIN_HEIGHT 200

typedef struct _UIMCandidateWindow	UIMCandidateWindow;
typedef struct _UIMCandidateWindowClass	UIMCandidateWindowClass;

struct _UIMCandidateWindow {
  GtkWindow parent;

  GtkWidget *view;
  GtkWidget *num_label;
  GtkWidget *prev_page_button;
  GtkWidget *next_page_button;

  GPtrArray *stores;

  guint nr_candidates;
  guint display_limit;
  gint candidate_index;
  gint page_index;

  gint pos_x;
  gint pos_y;
  gint width;
  gint height;

  GtkWidget *caret_state_indicator;

  gboolean is_active;
  gboolean need_hilite;
  gboolean need_page_update;

  /* sub window */
  struct sub_window {
    GtkWidget *window;
    GtkWidget *scrolled_window;
    GtkWidget *text_view;
    gboolean active;
  } sub_window;
};

struct _UIMCandidateWindowClass {
  GtkWindowClass parent_class;

  /* signals */
  void (*index_changed) (UIMCandidateWindowClass *candwin);
};

static UIMCandidateWindow *cwin; /* use single candwin */

GType candidate_window_get_type(void);
UIMCandidateWindow *candidate_window_new(void);

/* copied from uim-cand-win-gtk.c */
static gint uim_cand_win_gtk_get_index(UIMCandidateWindow *cwin);
static void uim_cand_win_gtk_set_index(UIMCandidateWindow *cwin, gint index);
static void uim_cand_win_gtk_set_page(UIMCandidateWindow *cwin, gint page);
static void uim_cand_win_gtk_set_page_candidates(UIMCandidateWindow *cwin, guint page, GSList *candidates);
static void uim_cand_win_gtk_create_sub_window(UIMCandidateWindow *cwin);
static void uim_cand_win_gtk_layout_sub_window(UIMCandidateWindow *cwin);

static void uim_cand_win_gtk_layout(void);

#define NR_CANDIDATES 10 /* FIXME! not used */
#define CANDWIN_DEFAULT_WIDTH	80

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

static void candidate_window_init(UIMCandidateWindow *cwin);
static void candidate_window_class_init(UIMCandidateWindowClass *klass);

static gboolean tree_selection_change(GtkTreeSelection *selection,
				      GtkTreeModel *model,
				      GtkTreePath *path,
				      gboolean path_currently_selected,
				      gpointer data);
static gboolean tree_selection_changed(GtkTreeSelection *selection,
				       gpointer data);

#if 0
static gboolean tree_view_button_press(GtkWidget *widget, GdkEventButton *event, gpointer data);
#endif
static gboolean configure_event_cb(GtkWidget *widget, GdkEventConfigure *event, gpointer data);

static GType candidate_window_type = 0;
static GTypeInfo const object_info = {
  sizeof (UIMCandidateWindowClass),
  NULL,
  NULL,
  (GClassInitFunc) candidate_window_class_init,
  NULL,
  NULL,
  sizeof(UIMCandidateWindow),
  0,
  (GInstanceInitFunc) candidate_window_init,
};

static gint cand_win_gtk_signals[NR_SIGNALS] = {0};

static unsigned int read_tag;

static void init_candidate_win(void);
static void candwin_activate(gchar **str);
static void candwin_update(gchar **str);
static void candwin_move(char **str);
static void candwin_show(void);
static void candwin_deactivate(void);
static void candwin_set_nr_candidates(gchar **str);
static void candwin_set_page_candidates(gchar **str);
static void candwin_show_page(gchar **str);
static void str_parse(char *str);

static void index_changed_cb(UIMCandidateWindow *cwin)
{
  fprintf(stdout, "index\n");
  fprintf(stdout, "%d\n\n", uim_cand_win_gtk_get_index(cwin));
  fflush(stdout);
}

GType
candidate_window_get_type(void)
{
  if (!candidate_window_type)
    candidate_window_type = g_type_register_static(GTK_TYPE_WINDOW,
		    "UIMCandWinGtk", &object_info, (GTypeFlags)0);
  return candidate_window_type;
}

static void candidate_window_class_init(UIMCandidateWindowClass *klass)
{
  cand_win_gtk_signals[INDEX_CHANGED_SIGNAL]
    = g_signal_new("index-changed",
		   G_TYPE_FROM_CLASS(klass),
		   G_SIGNAL_RUN_FIRST,
		   G_STRUCT_OFFSET(UIMCandidateWindowClass, index_changed),
		   NULL, NULL,
		   g_cclosure_marshal_VOID__VOID,
		   G_TYPE_NONE, 0);
}

UIMCandidateWindow *
candidate_window_new(void)
{
  GObject *obj = g_object_new(UIM_TYPE_CANDIDATE_WINDOW, "type",
		  GTK_WINDOW_POPUP, NULL);
  return UIM_CANDIDATE_WINDOW(obj);
}

/* copied from uim-cand-win-gtk.c */
static void
update_label(UIMCandidateWindow *cwin)
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

/* copied from uim-cand-win-gtk.c */
static gboolean
tree_selection_change(GtkTreeSelection *selection,
		      GtkTreeModel *model,
		      GtkTreePath *path,
		      gboolean path_currently_selected,
		      gpointer data)
{
  /* candidate_window *cwin = data; */
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

/* copied from uim-cand-win-gtk.c */
static gboolean
tree_selection_changed(GtkTreeSelection *selection,
                       gpointer data)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  /* UIMCandidateWindow *cwin = UIM_CANDIDATE_WINDOW(data); */

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

#if 0
/* copied from uim-cand-win-gtk.c */
static gboolean
tree_view_button_press(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
  GtkTreePath *path;
  gboolean exist, retval = FALSE;
  gint *indicies;

  fprintf(stderr, "tree_view_button_press\n");
  g_return_val_if_fail(GTK_IS_TREE_VIEW(widget), FALSE);
  g_return_val_if_fail(UIM_CANDIDATE_WINDOW(data), FALSE);

  /* cwin = UIM_CANDIDATE_WINDOW(data); */

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
#endif

static void
pagebutton_clicked(GtkButton *button, gpointer data)
{
  UIMCandidateWindow *cwin = UIM_CANDIDATE_WINDOW(data);

  if (cwin->candidate_index < 0) {
    /* if candidate_index < 0, "index-changed" signal is not emitted
     * and candidates for new page is not set.
     */
    cwin->candidate_index = cwin->page_index * cwin->display_limit;
  }
  if (button == GTK_BUTTON(cwin->prev_page_button)) {
    uim_cand_win_gtk_set_page(cwin, cwin->page_index - 1);
  } else if (button == GTK_BUTTON(cwin->next_page_button)) {
    uim_cand_win_gtk_set_page(cwin, cwin->page_index + 1);
  } else {
    return;
  }
  if (cwin->candidate_index >= 0) {
    g_signal_emit(G_OBJECT(cwin),
                  cand_win_gtk_signals[INDEX_CHANGED_SIGNAL], 0);
  }
  if (!cwin->stores->pdata[cwin->page_index]) {
    /*       candwin                         uim-xim
     * pagebutton_clicked()
     *            ---------"index"------------>
     *                                      InputContext::candidate_select()
     *            <---"set_page_candidates"----
     *                                        Canddisp::select()
     *            <--------"select"------------
     * candwin_update()
     *   uim_cand_win_gtk_set_index()
     *     uim_cand_win_gtk_set_page()
     */
    cwin->need_page_update = TRUE;
  }
}

static void
cb_tree_view_destroy(GtkWidget *widget, GPtrArray *stores)
{
  gint i;

  g_return_if_fail(GTK_IS_TREE_VIEW(widget));

  for (i = cwin->stores->len - 1; i >= 0; i--) {
    GtkListStore *store = g_ptr_array_remove_index(cwin->stores, i);
    if (store) {
      gtk_list_store_clear(store);
      g_object_unref(G_OBJECT(store));
    }
  }
  g_ptr_array_free(cwin->stores, TRUE);
}

static void
init_candidate_win(void) {
  cwin = candidate_window_new();
  g_signal_connect(G_OBJECT(cwin), "index-changed",
		   G_CALLBACK(index_changed_cb), NULL);
  g_signal_connect(G_OBJECT(cwin), "configure_event",
		   G_CALLBACK(configure_event_cb), NULL);
}

static void
candidate_window_init(UIMCandidateWindow *cwin)
{
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkWidget *scrolled_window;
  GtkWidget *vbox;
  GtkWidget *hbox;
  GtkWidget *frame;
  GtkTreeSelection *selection;
  GdkRectangle cursor_location;

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
#else
  vbox = gtk_vbox_new(FALSE, 0);
#endif
  frame = gtk_frame_new(NULL);

  cwin->stores = g_ptr_array_new();

  gtk_window_set_default_size(GTK_WINDOW(cwin),
		  CANDWIN_DEFAULT_WIDTH, -1);


  scrolled_window = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				 GTK_POLICY_NEVER,
				 GTK_POLICY_NEVER);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);

  cwin->view = gtk_tree_view_new();
  g_signal_connect(G_OBJECT(cwin->view), "destroy",
  		   G_CALLBACK(cb_tree_view_destroy), cwin->stores);
  gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(cwin->view), TRUE);
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(cwin->view), FALSE);
  gtk_container_add(GTK_CONTAINER(scrolled_window), cwin->view);

  gtk_container_add(GTK_CONTAINER(frame), vbox);
  gtk_container_add(GTK_CONTAINER(cwin), frame);
  gtk_container_set_border_width(GTK_CONTAINER(frame), 0);

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(cwin->view));

  gtk_tree_selection_set_select_function(selection,
		  			 tree_selection_change,
					 cwin,
					 NULL);
  g_signal_connect(G_OBJECT(selection), "changed",
		   G_CALLBACK(tree_selection_changed), cwin);

  renderer = gtk_cell_renderer_text_new();
  g_object_set(renderer, "scale", 0.8, (const gchar *)NULL);

  column = gtk_tree_view_column_new_with_attributes("No",
						    renderer,
						    "text",
						    COLUMN_HEADING,
						    (const gchar *)NULL);
  gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
  gtk_tree_view_append_column(GTK_TREE_VIEW(cwin->view), column);

  renderer = gtk_cell_renderer_text_new();
  g_object_set(renderer, "scale", 1.2, (const gchar *)NULL);
  /* g_object_set(renderer, "size-points", 20.0, NULL); */
  column = gtk_tree_view_column_new_with_attributes("Text",
						    renderer,
						    "text",
						    COLUMN_CANDIDATE,
						    (const gchar *)NULL);
  gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
  gtk_tree_view_append_column(GTK_TREE_VIEW(cwin->view), column);

  cwin->num_label = gtk_label_new("");

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

#if 0
  g_signal_connect(G_OBJECT(cwin->view), "button-press-event",
  		   G_CALLBACK(tree_view_button_press), cwin);
#endif

  cwin->pos_x = 0;
  cwin->pos_y = 0;
  cwin->is_active = FALSE;
  cwin->need_hilite = FALSE;
  cwin->need_page_update = FALSE;
  cwin->caret_state_indicator = caret_state_indicator_new();

  cursor_location.x = 0;
  cursor_location.y = 0;
  cursor_location.height = 0;
  caret_state_indicator_set_cursor_location(cwin->caret_state_indicator, &cursor_location);

  cwin->sub_window.window = NULL;
  cwin->sub_window.scrolled_window = NULL;
  cwin->sub_window.text_view = NULL;
  cwin->sub_window.active = FALSE;
}

static void
candwin_activate(gchar **str)
{
  gsize rbytes, wbytes;
  gint i, nr_stores = 1;
  guint j = 1;
  gchar *utf8_str;
  const gchar *charset;
  guint display_limit;
  GSList *candidates = NULL;

  if (cwin->stores == NULL)
    cwin->stores = g_ptr_array_new();

  /* remove old data */
  for (i = cwin->stores->len - 1; i >= 0; i--) {
    GtkListStore *store = g_ptr_array_remove_index(cwin->stores, i);
    if (store) {
      gtk_list_store_clear(store);
      g_object_unref(G_OBJECT(store));
    }
  }

  if (!strncmp(str[1], "charset=", 8))
    charset = str[1] + 8;
  else
    charset = "UTF-8";

  if (!strncmp(str[2], "display_limit=", 14)) {
    display_limit = atoi(str[2] + 14);
    i = 3;
  } else {
    display_limit = 0;
    i = 2;
  }

  for ( ; str[i]; i++) {
    if (strcmp(str[i], "") == 0) {
      break;
    }
    utf8_str = g_convert(str[i],
			 -1,
			 "UTF-8",
			 charset,
			 &rbytes, &wbytes, NULL);

    candidates = g_slist_prepend(candidates, utf8_str);
    j++;
  }
  candidates = g_slist_reverse(candidates);

  cwin->candidate_index = -1;
  cwin->nr_candidates = j - 1;
  cwin->display_limit = display_limit;
  cwin->need_hilite = FALSE;
  cwin->need_page_update = FALSE;

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
    GtkListStore *store = gtk_list_store_new(NR_COLUMNS, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
    GSList *node;

    g_ptr_array_add(cwin->stores, store);

    /* set candidates */
    for (j = i * display_limit, node = g_slist_nth(candidates, j);
	 display_limit ? j < display_limit * (i + 1) : j < cwin->nr_candidates;
	 j++, node = g_slist_next(node))
    {
      GtkTreeIter ti;
      if (node) {
	gchar *str = node->data;
	gchar **column = g_strsplit(str, "\a", 3);
	gtk_list_store_append(store, &ti);
	gtk_list_store_set(store, &ti,
			   COLUMN_HEADING, column[0],
			   COLUMN_CANDIDATE, column[1],
			   COLUMN_ANNOTATION, column[2],
			   TERMINATOR);
	g_strfreev(column);
	g_free(str);
      } else {
	/* No need to set any data for empty row. */
      }
    }
  }
  g_slist_free(candidates);

  if (cwin->nr_candidates <= cwin->display_limit) {
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->prev_page_button), FALSE);
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->next_page_button), FALSE);
  } else {
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->prev_page_button), TRUE);
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->next_page_button), TRUE);
  }

  uim_cand_win_gtk_set_page(cwin, 0);
  update_label(cwin);

  gtk_widget_show_all(GTK_WIDGET(cwin));
  cwin->is_active = TRUE;
}

static void
candwin_update(gchar **str)
{
  int index, need_hilite;
  sscanf(str[1], "%d", &index);
  sscanf(str[2], "%d", &need_hilite);
  cwin->need_hilite = (need_hilite == 1) ? TRUE : FALSE;

  uim_cand_win_gtk_set_index(cwin, index);
}

static void
candwin_move(char **str)
{
  sscanf(str[1], "%d", &cwin->pos_x);
  sscanf(str[2], "%d", &cwin->pos_y);

  uim_cand_win_gtk_layout();
}

static void
candwin_show(void)
{
  if (cwin->is_active) {
    gtk_widget_show_all(GTK_WIDGET(cwin));
    if (cwin->sub_window.active)
      gtk_widget_show(cwin->sub_window.window);
  }
}

static void
candwin_deactivate(void)
{
  gtk_widget_hide(GTK_WIDGET(cwin));
  cwin->is_active = FALSE;
  if (cwin->sub_window.window)
    gtk_widget_hide(cwin->sub_window.window);
}

static void
caret_state_show(gchar **str)
{
  int timeout;

  sscanf(str[1], "%d", &timeout);
  caret_state_indicator_update(cwin->caret_state_indicator, cwin->pos_x, cwin->pos_y, str[2]);
  if (timeout != 0)
    caret_state_indicator_set_timeout(cwin->caret_state_indicator, timeout * 1000);
  gtk_widget_show_all(GTK_WIDGET(cwin->caret_state_indicator));
}

static void
caret_state_update()
{
  caret_state_indicator_update(cwin->caret_state_indicator, cwin->pos_x, cwin->pos_y, NULL);
}

static void
caret_state_hide()
{
  gtk_widget_hide(cwin->caret_state_indicator);
}

static void
candwin_set_nr_candidates(gchar **str)
{
  guint nr, display_limit;
  gint i, nr_stores = 1;

  sscanf(str[1], "%ud", &nr);
  sscanf(str[2], "%ud", &display_limit);

  cwin->candidate_index = -1;
  cwin->nr_candidates = nr;
  cwin->display_limit = display_limit;
  cwin->need_hilite = FALSE;
  cwin->need_page_update = FALSE;
  cwin->is_active = TRUE;

  if (cwin->nr_candidates <= cwin->display_limit) {
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->prev_page_button), FALSE);
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->next_page_button), FALSE);
  } else {
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->prev_page_button), TRUE);
    gtk_widget_set_sensitive(GTK_WIDGET(cwin->next_page_button), TRUE);
  }

  if (cwin->stores == NULL)
    cwin->stores = g_ptr_array_new();

  /* remove old data */
  for (i = cwin->stores->len - 1; i >= 0; i--) {
    GtkListStore *store = g_ptr_array_remove_index(cwin->stores, i);
    if (store) {
      gtk_list_store_clear(store);
      g_object_unref(G_OBJECT(store));
    }
  }

  /* calculate number of GtkListStores to create */
  if (display_limit) {
    nr_stores = nr / display_limit;
    if (nr > display_limit * nr_stores)
      nr_stores++;
  }

  /* setup dummy array */
  for (i = 0; i < nr_stores; i++)
    g_ptr_array_add(cwin->stores, NULL);
}

static void
candwin_set_page_candidates(gchar **str)
{
  gsize rbytes, wbytes;
  gint i;
  guint j = 1;
  gchar *utf8_str;
  const gchar *charset;
  GSList *candidates = NULL;
  int page;

  if (!strncmp(str[1], "charset=", 8))
    charset = str[1] + 8;
  else
    charset = "UTF-8";

  if (!strncmp(str[2], "page=", 5)) {
    page = atoi(str[2] + 5);
    i = 3;
  } else {
    /* shouldn't happen */
    page = 0;
    i = 2;
  }

  for ( ; str[i]; i++) {
    if (strcmp(str[i], "") == 0) {
      break;
    }
    utf8_str = g_convert(str[i],
			 -1,
			 "UTF-8",
			 charset,
			 &rbytes, &wbytes, NULL);

    candidates = g_slist_prepend(candidates, utf8_str);
    j++;
  }
  candidates = g_slist_reverse(candidates);

  uim_cand_win_gtk_set_page_candidates(cwin, page, candidates);
  g_slist_free(candidates);
}

static void
candwin_show_page(gchar **str)
{
  int page;

  sscanf(str[1], "%d", &page);

  uim_cand_win_gtk_set_page(cwin, page);
  gtk_widget_show_all(GTK_WIDGET(cwin));
#if GTK_CHECK_VERSION(3, 7, 8)
  gtk_widget_queue_resize_no_redraw(cwin->view);
#endif
}

static void str_parse(gchar *str)
{
  gchar **tmp;
  gchar *command;

  tmp = g_strsplit(str, "\f", 0);
  command = tmp[0];

  if (command) {
    if (strcmp("activate", command) == 0) {
      candwin_activate(tmp);
    } else if (strcmp("select", command) == 0) {
      candwin_update(tmp);
    } else if (strcmp("show", command) == 0) {
      candwin_show();
    } else if (strcmp("hide", command) == 0) {
      gtk_widget_hide(GTK_WIDGET(cwin));
      if (cwin->sub_window.window)
        gtk_widget_hide(cwin->sub_window.window);
    } else if (strcmp("move", command) == 0) {
      candwin_move(tmp);
    } else if (strcmp("deactivate", command) == 0) {
      candwin_deactivate();
    } else if (strcmp("show_caret_state", command) == 0) {
      caret_state_show(tmp);
    } else if (strcmp("update_caret_state", command) == 0) {
      caret_state_update();
    } else if (strcmp("hide_caret_state", command) == 0) {
      caret_state_hide();
    } else if (strcmp("set_nr_candidates", command) == 0) {
      candwin_set_nr_candidates(tmp);
    } else if (strcmp("set_page_candidates", command) == 0) {
      candwin_set_page_candidates(tmp);
    } else if (strcmp("show_page", command) == 0) {
      candwin_show_page(tmp);
    }
  }
  g_strfreev(tmp);
}

#define CANDIDATE_BUFFER_SIZE	4096
static gboolean
read_cb(GIOChannel *channel, GIOCondition c, gpointer p)
{
  char buf[CANDIDATE_BUFFER_SIZE];
  char *read_buf = strdup("");
  int i = 0;
  int n;
  gchar **tmp;
  int fd = g_io_channel_unix_get_fd(channel);

  while (uim_helper_fd_readable(fd) > 0) {
    n = read(fd, buf, CANDIDATE_BUFFER_SIZE - 1);
    if (n == 0) {
      close(fd);
      exit(EXIT_FAILURE);
    }
    if (n == -1)
      return TRUE;
    buf[n] = '\0';
    read_buf = realloc(read_buf, strlen(read_buf) + n + 1);
    strcat(read_buf, buf);
  }

  tmp = g_strsplit(read_buf, "\f\f", 0);

  while (tmp[i]) {
    str_parse(tmp[i]);
    i++;
  }
  g_strfreev(tmp);
  free(read_buf);
  return TRUE;
}

int
main(int argc, char *argv[])
{
  GIOChannel *channel;

  /* disable uim context in annotation window */
  setenv("GTK_IM_MODULE", "gtk-im-context-simple", 1);

  gtk_init(&argc, &argv);
  if (uim_init() < 0)
    return 0;

  init_candidate_win();

  channel = g_io_channel_unix_new(0);
  read_tag = g_io_add_watch(channel, G_IO_IN | G_IO_HUP | G_IO_ERR,
			    read_cb, 0);
  g_io_channel_unref(channel);

  gtk_main();
  uim_quit();

  return 0;
}

/* copied from uim-cand-win-gtk.c */
static gint
uim_cand_win_gtk_get_index(UIMCandidateWindow *cwin)
{
  g_return_val_if_fail(UIM_IS_CANDIDATE_WINDOW(cwin), -1);

  return cwin->candidate_index;
}

/* copied from uim-cand-win-gtk.c */
static void
uim_cand_win_gtk_set_index(UIMCandidateWindow *cwin, gint index)
{
  gint new_page;

  g_return_if_fail(UIM_IS_CANDIDATE_WINDOW(cwin));

  if (index >= (gint) cwin->nr_candidates)
    cwin->candidate_index = 0;
  else
    cwin->candidate_index = index;

  if (cwin->candidate_index >= 0 && cwin->display_limit)
    new_page = cwin->candidate_index / cwin->display_limit;
  else
    new_page = cwin->page_index;

  if (cwin->page_index != new_page || cwin->need_page_update)
    uim_cand_win_gtk_set_page(cwin, new_page);

  if (cwin->candidate_index >= 0 && cwin->need_hilite) {
    GtkTreePath *path;
    gint pos = index;

    if (cwin->display_limit)
      pos = cwin->candidate_index % cwin->display_limit;

    path = gtk_tree_path_new_from_indices(pos, -1);
    gtk_tree_view_set_cursor(GTK_TREE_VIEW(cwin->view),
			     path, NULL, FALSE);
    gtk_tree_path_free(path);

  } else {
    GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(cwin
->view));

    gtk_tree_selection_unselect_all(selection);
    update_label(cwin);
  }
}

/* copied from uim-cand-win-gtk.c */
static void
uim_cand_win_gtk_set_page(UIMCandidateWindow *cwin, gint page)
{
  guint len, new_page;
  gint new_index;

  g_return_if_fail(UIM_IS_CANDIDATE_WINDOW(cwin));
  g_return_if_fail(cwin->stores);

  len = cwin->stores->len;
  g_return_if_fail(len);

  if (page < 0)
    new_page = len - 1;
  else if (page >= (gint) len)
    new_page = 0;
  else
    new_page = page;

  if (cwin->stores->pdata[new_page]) {
    gtk_tree_view_set_model(GTK_TREE_VIEW(cwin->view),
                            GTK_TREE_MODEL(cwin->stores->pdata[new_page]));
  }

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

 /* shrink the window */
  gtk_window_resize(GTK_WINDOW(cwin), CANDWIN_DEFAULT_WIDTH, 1);

  cwin->need_page_update = FALSE; /* avoid infinite loop with set_index() */
  uim_cand_win_gtk_set_index(cwin, new_index);
}

/* copied from uim-cand-win-gtk.c and adjusted */
static void
uim_cand_win_gtk_set_page_candidates(UIMCandidateWindow *cwin,
				     guint page,
				     GSList *candidates)
{
  GtkListStore *store;
  GSList *node;
  gint j, len;

  g_return_if_fail(UIM_IS_CANDIDATE_WINDOW(cwin));

  if (candidates == NULL)
    return;

  cwin->sub_window.active = FALSE;
  len = g_slist_length(candidates);

  /* create GtkListStores, and set candidates */
  store = gtk_list_store_new(NR_COLUMNS, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);

  cwin->stores->pdata[page] = store;
  /* set candidates */
  for (j = 0, node = g_slist_nth(candidates, j);
       j < len;
       j++, node = g_slist_next(node))
  {
    GtkTreeIter ti;

    if (node) {
      gchar *str = node->data;
      gchar **column = g_strsplit(str, "\a", 3);
      gtk_list_store_append(store, &ti);
      gtk_list_store_set(store, &ti,
			 COLUMN_HEADING, column[0],
			 COLUMN_CANDIDATE, column[1],
			 COLUMN_ANNOTATION, column[2],
			 TERMINATOR);

      g_strfreev(column);
      g_free(str);
    }
  }
}

static void
uim_cand_win_gtk_layout()
{
  int x, y;
  int screen_width, screen_height;

  screen_width = gdk_screen_get_width(gdk_screen_get_default());
  screen_height = gdk_screen_get_height(gdk_screen_get_default());

  if (screen_width < cwin->pos_x + cwin->width)
    x = cwin->pos_x - cwin->width;
  else
    x = cwin->pos_x;

  if (screen_height < cwin->pos_y + cwin->height)
    y = cwin->pos_y - cwin->height - 20; /* FIXME: Preedit height is needed to
					    be sent by uim-xim */
  else
    y = cwin->pos_y;

  gtk_window_move(GTK_WINDOW(cwin), x, y);

  uim_cand_win_gtk_layout_sub_window(cwin);
}

/* copied from uim-cand-win-gtk.c */
static void
uim_cand_win_gtk_create_sub_window(UIMCandidateWindow *cwin)
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

/* copied from uim-cand-win-gtk.c */
static void
uim_cand_win_gtk_layout_sub_window(UIMCandidateWindow *cwin)
{
#if GTK_CHECK_VERSION(2, 90, 0)
  gint x, y, w, h, sw, sh, x2, y2, w2, h2;
#else
  gint x, y, w, h, sw, sh, x2, y2, w2, h2, d, d2;
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

static gboolean
configure_event_cb(GtkWidget *widget, GdkEventConfigure *event, gpointer data)
{
  cwin->width = event->width;
  cwin->height = event->height;

  uim_cand_win_gtk_layout();

  return FALSE;
}
