/*

  Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
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
#include <glib/gprintf.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#define UIM_TYPE_CANDIDATE_WINDOW	(candidate_window_get_type())
#define UIM_CANDIDATE_WINDOW(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), candidate_window_get_type(), UIMCandidateWindow))
#define UIM_IS_CANDIDATE_WINDOW(obj)	(G_TYPE_CHECK_INSTANCE_TYPE ((obj), UIM_TYPE_CANDIDATE_WINDOW))
#define UIM_IS_CANDIDATE_WINDOW_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), UIM_TYPE_CANDIDATE_WINDOW))
#define UIM_CANDIDATE_WINDOW_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), UIM_TYPE_CANDIDATE_WINDOW, UIMCandidateWindowClass))

typedef struct _UIMCandidateWindow	UIMCandidateWindow;
typedef struct _UIMCandidateWindowClass	UIMCandidateWindowClass;

struct _UIMCandidateWindow {
  GtkWindow parent;

  GtkWidget *scrolled_window;
  GtkWidget *view;
  GtkWidget *num_label;

  GPtrArray *stores;

  guint nr_candidates;
  guint display_limit;
  gint candidate_index;
  gint page_index;

  gint pos_x;
  gint pos_y;
  gint width;
  gint height;

  gboolean is_active;
};

struct _UIMCandidateWindowClass {
  GtkWindowClass parent_class;

  /* signals */
  void (*index_changed) (UIMCandidateWindowClass *candwin);
};

static UIMCandidateWindow *cwin; /* use static one */

GType candidate_window_get_type(void);
UIMCandidateWindow *candidate_window_new(void);

/* copied from uim-cand-win-gtk.c */
static gint uim_cand_win_gtk_get_index(UIMCandidateWindow *cwin);
static void uim_cand_win_gtk_set_index(UIMCandidateWindow *cwin, gint index);
static void uim_cand_win_gtk_set_page(UIMCandidateWindow *cwin, gint page);

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
  COLUMN_CANDIDATE
};

static void candidate_window_init(UIMCandidateWindow *cwin);
static void candidate_window_class_init(UIMCandidateWindowClass *klass);

static gboolean tree_selection_changed(GtkTreeSelection *selection,
				       GtkTreeModel *model,
				       GtkTreePath *path,
				       gboolean path_currently_selected,
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

static int read_tag;

static void init_candidate_win(void);
static void candwin_activate(gchar **str);
static void candwin_update(gchar **str);
static void candwin_move(char **str);
static void candwin_show(void);
static void candwin_deactivate(void);
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
		    "UIMCandidateWindow", &object_info, (GTypeFlags)0);
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
tree_selection_changed(GtkTreeSelection *selection,
		       GtkTreeModel *model,
		       GtkTreePath *path,
		       gboolean path_currently_selected,
		       gpointer data)
{
  /* candidate_window *cwin = data; */
  gint *indicies;
  gint idx;

  if (!&cwin)
    return TRUE;

  indicies = gtk_tree_path_get_indices(path);
  g_return_val_if_fail(indicies, TRUE);
  idx = *indicies + cwin->display_limit * cwin->page_index;

  if (!path_currently_selected && cwin->candidate_index != idx) {
    cwin->candidate_index = idx;
    g_signal_emit(G_OBJECT(cwin),
		  cand_win_gtk_signals[INDEX_CHANGED_SIGNAL], 0);

  }

  update_label(cwin);

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
cb_tree_view_destroy(GtkWidget *widget, GPtrArray *stores)
{
  gint i;

  g_return_if_fail(GTK_IS_TREE_VIEW(widget));

  for (i = cwin->stores->len - 1; i >= 0; i--) {
    GtkListStore *store = g_ptr_array_remove_index(cwin->stores, i);
    gtk_list_store_clear(store);
    g_object_unref(G_OBJECT(store));
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
  GtkWidget *vbox;
  GtkTreeSelection *selection;
  
  vbox = gtk_vbox_new(FALSE, 0);

  cwin->stores = g_ptr_array_new();
  
  gtk_container_set_border_width(GTK_CONTAINER(cwin), 1);
  gtk_window_set_default_size(GTK_WINDOW(cwin),
		  CANDWIN_DEFAULT_WIDTH, -1);
  

  cwin->scrolled_window = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cwin->scrolled_window),
				 GTK_POLICY_NEVER,
				 GTK_POLICY_NEVER);
  gtk_box_pack_start(GTK_BOX(vbox), cwin->scrolled_window, TRUE, TRUE, 0);
  
  cwin->view = gtk_tree_view_new();
  g_signal_connect(G_OBJECT(cwin->view), "destroy", 
  		   G_CALLBACK(cb_tree_view_destroy), cwin->stores);
  gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(cwin->view), TRUE);
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(cwin->view), FALSE);
  gtk_container_add(GTK_CONTAINER(cwin->scrolled_window), cwin->view);

  gtk_container_add(GTK_CONTAINER(cwin), vbox);
    
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(cwin->view));

  gtk_tree_selection_set_select_function(selection,
		  			 tree_selection_changed,
					 cwin,
					 NULL);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes("No",
						    renderer,
						    "text",
						    COLUMN_HEADING,
						    NULL);
  gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
  gtk_tree_view_append_column(GTK_TREE_VIEW(cwin->view), column);
  
  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes("Text",
						    renderer,
						    "text",
						    COLUMN_CANDIDATE,
						    NULL);
  gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
  gtk_tree_view_append_column(GTK_TREE_VIEW(cwin->view), column);
  
  cwin->num_label = gtk_label_new("");

  gtk_box_pack_start(GTK_BOX(vbox), cwin->num_label, FALSE, FALSE, 0);

#if 0
  g_signal_connect(G_OBJECT(cwin->view), "button-press-event", 
  		   G_CALLBACK(tree_view_button_press), cwin);
#endif

  cwin->pos_x = 0;
  cwin->pos_y = 0;
  cwin->is_active = FALSE;

  gtk_widget_show(cwin->scrolled_window);
  gtk_widget_show(cwin->view);
  gtk_widget_show(cwin->num_label);
  gtk_widget_show_all(vbox);
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
    gtk_list_store_clear(store);
    g_object_unref(G_OBJECT(store));
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
    
    candidates = g_slist_append(candidates, utf8_str);
    j++;
  }

  cwin->candidate_index = -1;
  cwin->nr_candidates = j - 1;
  cwin->display_limit = display_limit;

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
    GtkListStore *store = gtk_list_store_new(2, G_TYPE_UINT, G_TYPE_STRING);
    GSList *node;

    g_ptr_array_add(cwin->stores, store);

    /* set candidates */
    for (j = i * display_limit, node = g_slist_nth(candidates, j);
	 display_limit ? j < display_limit * (i + 1) : j < cwin->nr_candidates;
	 j++, node = g_slist_next(node))
    {
      GtkTreeIter ti;
      if (node) {
	gchar *cand = node->data;
	gtk_list_store_append(store, &ti);
	gtk_list_store_set(store, &ti,
			   COLUMN_HEADING, j + 1,
			   COLUMN_CANDIDATE, cand,
			   TERMINATOR);
	g_free(cand);
      } else {
	/* No need to set any data for empty row. */
      }
    }
  }

  uim_cand_win_gtk_set_page(cwin, 0);
  gtk_widget_show_all(GTK_WIDGET(cwin));
  cwin->is_active = TRUE;
}

static void
candwin_update(gchar **str)
{
  int index;
  sscanf(str[1], "%d", &index);

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
  if (cwin->is_active)
    gtk_widget_show_all(GTK_WIDGET(cwin));
}

static void
candwin_deactivate(void)
{
  gtk_widget_hide(GTK_WIDGET(cwin));
  cwin->is_active = FALSE;
}

static void str_parse(gchar *str)
{
  gchar **tmp;
  int i = 0;

  tmp = g_strsplit(str, "\n", 0);

  while (tmp[i]) {
    if (strcmp("activate", tmp[i]) == 0) {
      candwin_activate(tmp);
    } else if (strcmp("select", tmp[i]) == 0) {
      candwin_update(tmp);
    } else if (strcmp("show", tmp[i]) == 0) {
      candwin_show();
    } else if (strcmp("hide", tmp[i]) == 0) {
      gtk_widget_hide_all(GTK_WIDGET(cwin));
    } else if (strcmp("move", tmp[i]) == 0) {
      candwin_move(tmp);
    } else if (strcmp("deactivate", tmp[i]) == 0) {
      candwin_deactivate();
    }
    i++;
  }
  g_strfreev(tmp);
}

static void
read_cb(gpointer p, int fd, GdkInputCondition c)
{
  char buf[1024];
  int i = 0;
  int n;
  gchar **tmp;

  n = read(fd, buf, 1024 - 1);
  if (n == 0) {
    close(fd);
    exit(-1);
  }
  if (n == -1)
    return;

  buf[n] = '\0';
  tmp = g_strsplit(buf, "\n\n", 0);

  while (tmp[i]) {
    str_parse(tmp[i]);
    i++;
  }
  g_strfreev(tmp);
  return;
}

int
main(int argc, char *argv[])
{
  gtk_set_locale();
  gtk_init(&argc, &argv);

  init_candidate_win();

  read_tag = gdk_input_add(0, (GdkInputCondition)GDK_INPUT_READ,
			   read_cb, 0);
  gtk_main();
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
    GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(cwin
->view));

    gtk_tree_selection_unselect_all(selection);
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

  if (page < 0) {
    gtk_tree_view_set_model(GTK_TREE_VIEW(cwin->view),
			    GTK_TREE_MODEL(cwin->stores->pdata[len - 1]));
    new_page = len - 1;
  } else if (page >= (gint) len) {
    gtk_tree_view_set_model(GTK_TREE_VIEW(cwin->view),
			    GTK_TREE_MODEL(cwin->stores->pdata[0]));
    new_page = 0;
  } else {
    gtk_tree_view_set_model(GTK_TREE_VIEW(cwin->view),

			    GTK_TREE_MODEL(cwin->stores->pdata[page]));
    new_page = page;
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

  uim_cand_win_gtk_set_index(cwin, new_index);
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
}

static gboolean
configure_event_cb(GtkWidget *widget, GdkEventConfigure *event, gpointer data)
{
  cwin->width = event->width;
  cwin->height = event->height;

  uim_cand_win_gtk_layout();

  return FALSE;
}
