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

#include "uim-cand-win-tbl-gtk.h"
#include <string.h>
#include <stdlib.h>
#include <uim/uim.h>
#include <uim/uim-scm.h>

#define DEFAULT_MIN_WINDOW_WIDTH 80

enum {
  TERMINATOR = -1,
  COLUMN_HEADING,
  COLUMN_CANDIDATE1,
  COLUMN_CANDIDATE2,
  COLUMN_CANDIDATE3,
  COLUMN_CANDIDATE4,
  COLUMN_CANDIDATE5,
  COLUMN_CANDIDATE6,
  COLUMN_CANDIDATE7,
  COLUMN_CANDIDATE8,
  COLUMN_CANDIDATE9,
  COLUMN_CANDIDATE10,
  COLUMN_CANDIDATE11,
  COLUMN_CANDIDATE12,
  COLUMN_CANDIDATE13,
  NR_COLUMNS
};

#define LABELCHAR_NR_COLUMNS 13
#define LABELCHAR_NR_ROWS 8
#define LABELCHAR_NR_CELLS (LABELCHAR_NR_COLUMNS * LABELCHAR_NR_ROWS)
#define INDEX(row,col) ((row) * LABELCHAR_NR_COLUMNS + (col))
/* 106 keyboard */
static gchar default_labelchar_table[LABELCHAR_NR_CELLS] = {
  '1','2','3','4','5', '6','7','8','9','0',   '-','^','\\',
  'q','w','e','r','t', 'y','u','i','o','p',   '@','[','\0',
  'a','s','d','f','g', 'h','j','k','l',';',   ':',']','\0',
  'z','x','c','v','b', 'n','m',',','.','/',   '\0','\0',' ',
  '!','"','#','$','%', '&','\'','(',')','\0', '=','~','|',
  'Q','W','E','R','T', 'Y','U','I','O','P',   '`','{','\0',
  'A','S','D','F','G', 'H','J','K','L','+',   '*','}','\0',
  'Z','X','C','V','B', 'N','M','<','>','?',   '_','\0','\0',
};
/* labelchar_table consists of four blocks
 *   blockLR  blockA
 *   blockLRS blockAS
 */
#define BLOCK_A_ROW_START 0
#define BLOCK_A_ROW_END 4
#define BLOCK_A_COLUMN_START 10
#define BLOCK_A_COLUMN_END LABELCHAR_NR_COLUMNS
#define BLOCK_LRS_ROW_START BLOCK_A_ROW_END
#define BLOCK_LRS_ROW_END LABELCHAR_NR_ROWS
#define BLOCK_LRS_COLUMN_START 0
#define BLOCK_LRS_COLUMN_END BLOCK_A_COLUMN_START
#define BLOCK_AS_ROW_START BLOCK_LRS_ROW_START
#define BLOCK_AS_ROW_END BLOCK_LRS_ROW_END
#define BLOCK_AS_COLUMN_START BLOCK_LRS_COLUMN_END
#define BLOCK_AS_COLUMN_END LABELCHAR_NR_COLUMNS

#define BLOCK_SPACING 20
#define HOMEPOSITION_SPACING 2
#define SPACING_LEFT_BLOCK_COLUMN 4
#define SPACING_RIGHT_BLOCK_COLUMN (BLOCK_A_COLUMN_START - 1)
#define SPACING_UP_BLOCK_ROW (BLOCK_A_ROW_END - 1)
#define SPACING_LEFTHAND_FAR_COLUMN 3
#define SPACING_RIGHTHAND_FAR_COLUMN 5
#define SPACING_UPPER_FAR_ROW 0
#define SPACING_SHIFT_UPPER_FAR_ROW 4

struct index_button {
  gint cand_index_in_page;
  GtkButton *button;
};

static void	uim_cand_win_tbl_gtk_init		(UIMCandWinTblGtk *cwin);
static void	uim_cand_win_tbl_gtk_class_init	(UIMCandWinGtkClass *klass);
static void	uim_cand_win_tbl_gtk_dispose	(GObject *obj);
static gchar	*init_labelchar_table(void);
static void	button_clicked(GtkButton *button, gpointer data);
static void	show_table(GtkTable *view, GPtrArray *buttons);
static GtkButton *get_button(GPtrArray *buttons, gint idx);


static GType cand_win_tbl_type = 0;
static GTypeInfo const object_info = {
  sizeof (UIMCandWinTblGtkClass),
  (GBaseInitFunc) NULL,
  (GBaseFinalizeFunc) NULL,
  (GClassInitFunc) uim_cand_win_tbl_gtk_class_init,
  (GClassFinalizeFunc) NULL,
  NULL,                       /* class_data */
  sizeof (UIMCandWinTblGtk),
  0,                          /* n_preallocs */
  (GInstanceInitFunc) uim_cand_win_tbl_gtk_init,
};

static GtkWindowClass *parent_class = NULL;

GType
uim_cand_win_tbl_gtk_get_type(void)
{
  if (!cand_win_tbl_type)
    cand_win_tbl_type = g_type_register_static(UIM_TYPE_CAND_WIN_GTK, "UIMCandWinTblGtk",
					   &object_info, (GTypeFlags)0);
  return cand_win_tbl_type;
}

GType
uim_cand_win_tbl_gtk_register_type(GTypeModule *module)
{
  if (!cand_win_tbl_type)
    cand_win_tbl_type = g_type_module_register_type(module,
						UIM_TYPE_CAND_WIN_GTK,
						"UIMCandWinTblGtk",
						&object_info, 0);
  return cand_win_tbl_type;
}

static void
uim_cand_win_tbl_gtk_class_init (UIMCandWinGtkClass *klass)
{
  GObjectClass *object_class = (GObjectClass *) klass;

  parent_class = g_type_class_peek_parent (klass);
  object_class->dispose = uim_cand_win_tbl_gtk_dispose;

  klass->set_candidates = (void (*)(UIMCandWinGtk *, guint, GSList *))uim_cand_win_tbl_gtk_set_candidates;
  klass->set_page_candidates = (void (*)(UIMCandWinGtk *, guint, GSList *))uim_cand_win_tbl_gtk_set_page_candidates;
  klass->set_index = (void (*)(UIMCandWinGtk *, gint))uim_cand_win_tbl_gtk_set_index;
  klass->set_page = (void (*)(UIMCandWinGtk *, gint))uim_cand_win_tbl_gtk_set_page;
}

static void
uim_cand_win_tbl_gtk_init (UIMCandWinTblGtk *ctblwin)
{
  gint row, col;
  GtkWidget *viewport;
  UIMCandWinGtk *cwin;
  cwin = UIM_CAND_WIN_GTK(ctblwin);

  ctblwin->buttons = g_ptr_array_new();
  ctblwin->labelchar_table = init_labelchar_table();

  gtk_widget_destroy(cwin->view);
  cwin->view = gtk_table_new(LABELCHAR_NR_ROWS, LABELCHAR_NR_COLUMNS, FALSE);
  viewport = gtk_viewport_new(NULL, NULL);
  gtk_container_add(GTK_CONTAINER(viewport), cwin->view);
  gtk_container_add(GTK_CONTAINER(cwin->scrolled_window), viewport);
  gtk_container_set_resize_mode(GTK_CONTAINER(viewport), GTK_RESIZE_PARENT);
  for (row = 0; row < LABELCHAR_NR_ROWS; row++) {
    for (col = 0; col < LABELCHAR_NR_COLUMNS; col++) {
      GtkWidget *button;
      struct index_button *idxbutton;
      button = gtk_button_new_with_label("  ");
      g_signal_connect(button, "clicked", G_CALLBACK(button_clicked), ctblwin);
      gtk_table_attach_defaults(GTK_TABLE(cwin->view), button,
                                col, col + 1, row, row + 1);
      idxbutton = g_malloc(sizeof(struct index_button));
      if (idxbutton) {
        idxbutton->button = GTK_BUTTON(button);
        idxbutton->cand_index_in_page = -1;
      }
      g_ptr_array_add(ctblwin->buttons, idxbutton);
    }
  }
  gtk_table_set_col_spacing(GTK_TABLE(cwin->view), SPACING_LEFT_BLOCK_COLUMN,
      BLOCK_SPACING);
  gtk_table_set_col_spacing(GTK_TABLE(cwin->view), SPACING_RIGHT_BLOCK_COLUMN,
      BLOCK_SPACING);
  gtk_table_set_row_spacing(GTK_TABLE(cwin->view), SPACING_UP_BLOCK_ROW,
      BLOCK_SPACING);
  gtk_table_set_col_spacing(GTK_TABLE(cwin->view), SPACING_LEFTHAND_FAR_COLUMN,
      HOMEPOSITION_SPACING);
  gtk_table_set_col_spacing(GTK_TABLE(cwin->view), SPACING_RIGHTHAND_FAR_COLUMN,
      HOMEPOSITION_SPACING);
  gtk_table_set_row_spacing(GTK_TABLE(cwin->view), SPACING_UPPER_FAR_ROW,
      HOMEPOSITION_SPACING);
  gtk_table_set_row_spacing(GTK_TABLE(cwin->view), SPACING_SHIFT_UPPER_FAR_ROW,
      HOMEPOSITION_SPACING);

  gtk_widget_show_all(cwin->view);
  gtk_widget_show(viewport);

  gtk_widget_set_size_request(cwin->num_label, DEFAULT_MIN_WINDOW_WIDTH, -1);
  gtk_window_set_default_size(GTK_WINDOW(cwin), DEFAULT_MIN_WINDOW_WIDTH, -1);
  gtk_window_set_resizable(GTK_WINDOW(cwin), FALSE);
}

static gchar *
init_labelchar_table(void)
{
  gchar *table;
  uim_lisp list;
  size_t len = 0;
  uim_lisp *ary0, *ary;
  guint i;

  list = uim_scm_symbol_value("uim-candwin-prog-layout");
  if (list == NULL || !uim_scm_listp(list)) {
    return default_labelchar_table;
  }
  ary0 = ary = (uim_lisp *)uim_scm_list2array(list, &len, NULL);
  if (ary == NULL || len <= 0) {
    if (ary0) {
      free(ary0);
    }
    return default_labelchar_table;
  }
  table = (gchar *)g_malloc(LABELCHAR_NR_CELLS);
  if (table == NULL) {
    free(ary0);
    return default_labelchar_table;
  }
  for (i = 0; i < LABELCHAR_NR_CELLS; i++, ary++) {
    table[i] = '\0';
    if (i < len) {
      char *str = uim_scm_c_str(*ary);
      if (str) {
        table[i] = *str;
        free(str);
      }
    }
  }
  free(ary0);
  return table;
}

static void
button_clicked(GtkButton *button, gpointer data)
{
  UIMCandWinTblGtk *ctblwin = data;
  UIMCandWinGtk *cwin = UIM_CAND_WIN_GTK(ctblwin);
  gint i;
  gint idx = -1;

  for (i = 0; i < LABELCHAR_NR_CELLS; i++) {
    GtkButton *p;
    struct index_button *idxbutton;
    idxbutton = g_ptr_array_index(ctblwin->buttons, i);
    if (!idxbutton) {
      continue;
    }
    p = idxbutton->button;
    if (p == button) {
      GtkReliefStyle relief;
      relief = gtk_button_get_relief(button);
      if (relief != GTK_RELIEF_NORMAL) {
        return;
      }
      idx = idxbutton->cand_index_in_page;
      break;
    }
  }
  if (idx >= 0 && cwin->display_limit) {
    if (idx >= (gint)cwin->display_limit) {
      idx %= cwin->display_limit;
    }
    cwin->candidate_index = cwin->page_index * cwin->display_limit + idx;
  } else {
    cwin->candidate_index = idx;
  }
  if (cwin->candidate_index >= (gint)cwin->nr_candidates) {
    cwin->candidate_index = -1;
  }
  g_signal_emit_by_name(G_OBJECT(cwin), "index-changed");
}

static void
uim_cand_win_tbl_gtk_dispose (GObject *obj)
{
  UIMCandWinTblGtk *ctblwin;

  g_return_if_fail(UIM_IS_CAND_WIN_TBL_GTK(obj));

  ctblwin = UIM_CAND_WIN_TBL_GTK(obj);

  if (ctblwin->labelchar_table != default_labelchar_table) {
    g_free(ctblwin->labelchar_table);
    ctblwin->labelchar_table = NULL;
  }
  if (ctblwin->buttons) {
    guint i;
    for (i = 0; i < ctblwin->buttons->len; i++) {
      if (ctblwin->buttons->pdata[i]) {
        g_free(ctblwin->buttons->pdata[i]);
        /* GtkButton is destroyed by container */
      }
    }
    g_ptr_array_free(ctblwin->buttons, TRUE);
    ctblwin->buttons = NULL;
  }

  if (G_OBJECT_CLASS (parent_class)->dispose)
    G_OBJECT_CLASS (parent_class)->dispose(obj);
}

UIMCandWinTblGtk *
uim_cand_win_tbl_gtk_new (void)
{
  GObject *obj = g_object_new(UIM_TYPE_CAND_WIN_TBL_GTK,
			      "type", GTK_WINDOW_POPUP,
			      NULL);
  return UIM_CAND_WIN_TBL_GTK(obj);
}

static void
get_row_column(gchar *labelchar_table, const gchar labelchar, gint *row, gint *col)
{
  gint i;
  for (i = 0; i < LABELCHAR_NR_CELLS; i++) {
    if (labelchar_table[i] == labelchar) {
      *row = i / LABELCHAR_NR_COLUMNS;
      *col = i % LABELCHAR_NR_COLUMNS;
      return;
    }
  }
  *row = 0;
  *col = 0;
}

static void
set_candidate(UIMCandWinTblGtk *ctblwin, GSList *node, GtkListStore *store, gint idx)
{
  if (node) {
    GtkTreeIter ti;
    gint row = 0;
    gint col = 0;
    gint i;
    const char *heading_label = NULL;
    const char *cand_str = NULL;
    uim_candidate cand = node->data;
    struct index_button *idxbutton;

    heading_label = uim_candidate_get_heading_label(cand);
    cand_str = uim_candidate_get_cand_str(cand);

    get_row_column(ctblwin->labelchar_table, heading_label[0], &row, &col);
    gtk_tree_model_get_iter_first(GTK_TREE_MODEL(store), &ti);
    for (i = 0; i < row; i++) {
      gtk_tree_model_iter_next(GTK_TREE_MODEL(store), &ti);
    }
    gtk_list_store_set(store, &ti, col, cand_str, TERMINATOR);

    idxbutton = g_ptr_array_index(ctblwin->buttons, INDEX(row, col));
    if (idxbutton) {
      idxbutton->cand_index_in_page = idx;
    }
  } else {
    /* No need to set any data for empty row. */
  }
}

void
uim_cand_win_tbl_gtk_set_candidates(UIMCandWinTblGtk *ctblwin,
				guint display_limit,
				GSList *candidates)
{
  gint i, nr_stores = 1;
  UIMCandWinGtk *cwin;

  g_return_if_fail(UIM_IS_CAND_WIN_TBL_GTK(ctblwin));
  cwin = UIM_CAND_WIN_GTK(ctblwin);

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
    GtkListStore *store = gtk_list_store_new(LABELCHAR_NR_COLUMNS,
        G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
        G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
        G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
	G_TYPE_STRING);
    GSList *node;
    guint j;

    g_ptr_array_add(cwin->stores, store);

    for (j = 0; j < LABELCHAR_NR_ROWS; j++) {
      GtkTreeIter ti;
      gtk_list_store_append(store, &ti);
    }

    /* set candidates */
    for (j = i * display_limit, node = g_slist_nth(candidates, j);
	 display_limit ? j < display_limit * (i + 1) : j < cwin->nr_candidates;
	 j++, node = g_slist_next(node))
    {
      set_candidate(ctblwin, node, store, j);
    }
  }

  uim_cand_win_gtk_set_page(cwin, 0);

  uim_cand_win_gtk_update_label(cwin);
}

void
uim_cand_win_tbl_gtk_set_page_candidates(UIMCandWinTblGtk *ctblwin,
				     guint page,
				     GSList *candidates)
{
  GtkListStore *store;
  GSList *node;
  gint j, len;
  UIMCandWinGtk *cwin;

  g_return_if_fail(UIM_IS_CAND_WIN_TBL_GTK(ctblwin));
  cwin = UIM_CAND_WIN_GTK(ctblwin);

  if (candidates == NULL)
    return;

  cwin->sub_window.active = FALSE;
  len = g_slist_length(candidates);

  /* create GtkListStores, and set candidates */
  store = gtk_list_store_new(LABELCHAR_NR_COLUMNS, G_TYPE_STRING,
      G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
      G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
      G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);

  cwin->stores->pdata[page] = store;

  for (j = 0; j < LABELCHAR_NR_ROWS; j++) {
    GtkTreeIter ti;
    gtk_list_store_append(store, &ti);
  }

  /* set candidates */
  for (j = 0, node = g_slist_nth(candidates, j);
       j < len;
       j++, node = g_slist_next(node))
  {
    set_candidate(ctblwin, node, store, j);
  }
}

void
uim_cand_win_tbl_gtk_set_index(UIMCandWinTblGtk *ctblwin, gint index)
{
  gint new_page;
  UIMCandWinGtk *cwin;

  g_return_if_fail(UIM_IS_CAND_WIN_TBL_GTK(ctblwin));
  cwin = UIM_CAND_WIN_GTK(ctblwin);

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

  uim_cand_win_gtk_update_label(cwin);
}

static void
update_table_button(GtkTreeModel *model, GPtrArray *buttons, gchar *labelchar_table)
{
  GtkTreeIter ti;
  gint row, col;
  gboolean hasValue = TRUE;
  gtk_tree_model_get_iter_first(model, &ti);
  for (row = 0; row < LABELCHAR_NR_ROWS; row++) {
    for (col = 0; col < LABELCHAR_NR_COLUMNS; col++) {
      GValue value = {0};
      const gchar *str = NULL;
      GtkButton *button = NULL;
      button = get_button(buttons, INDEX(row, col));
      if (hasValue) {
        gtk_tree_model_get_value(model, &ti, col, &value);
        str = g_value_get_string(&value);
      }
      if (str == NULL) {
        str = "  ";
	if (labelchar_table[INDEX(row, col)] == '\0') {
          gtk_button_set_relief(button, GTK_RELIEF_NONE);
	} else {
          gtk_button_set_relief(button, GTK_RELIEF_HALF);
	}
        gtk_widget_set_sensitive(GTK_WIDGET(button), FALSE);
      } else {
        gtk_button_set_relief(button, GTK_RELIEF_NORMAL);
        gtk_widget_set_sensitive(GTK_WIDGET(button), TRUE);
      }
      gtk_button_set_label(button, str);
      if (hasValue) {
        g_value_unset(&value);
      }
    }
    if (hasValue) {
      hasValue = gtk_tree_model_iter_next(model, &ti);
    }
  }
}

void
uim_cand_win_tbl_gtk_set_page(UIMCandWinTblGtk *ctblwin, gint page)
{
  guint len, new_page;
  gint new_index;
  UIMCandWinGtk *cwin;

  g_return_if_fail(UIM_IS_CAND_WIN_TBL_GTK(ctblwin));
  cwin = UIM_CAND_WIN_GTK(ctblwin);
  g_return_if_fail(cwin->stores);

  len = cwin->stores->len;
  g_return_if_fail(len);

  if (page < 0)
    new_page = len - 1;
  else if (page >= (gint) len)
    new_page = 0;
  else
    new_page = page;

  update_table_button(GTK_TREE_MODEL(cwin->stores->pdata[new_page]),
                      ctblwin->buttons, ctblwin->labelchar_table);
  show_table(GTK_TABLE(cwin->view), ctblwin->buttons);

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

static gboolean
is_empty_block(GPtrArray *buttons, gint rowstart, gint rowend, gint colstart, gint colend)
{
  gint row, col;
  for (row = rowstart; row < rowend; row++) {
    for (col = colstart; col < colend; col++) {
      GtkButton *button = NULL;
      GtkReliefStyle relief;
      button = get_button(buttons, INDEX(row, col));
      relief = gtk_button_get_relief(button);
      if (relief == GTK_RELIEF_NORMAL) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

static void
show_table(GtkTable *view, GPtrArray *buttons)
{
  /* hide empty blocks.
   * pattern0(full table)
   *   blockLR  blockA
   *   blockLRS blockAS  (for shift key)
   * pattern1(minimal blocks)
   *   blockLR
   * pattern2(without shift blocks)
   *   blockLR  blockA
   * pattern3(without symbol blocks)
   *   blockLR
   *   blockLRS
   */
  gint row, col;
  gint hide_row, hide_col;
  gint row_spacing, col_spacing;
  gboolean blockA, blockAS, blockLRS;
  blockA = !is_empty_block(buttons, BLOCK_A_ROW_START, BLOCK_A_ROW_END,
      BLOCK_A_COLUMN_START, BLOCK_A_COLUMN_END);
  blockAS = !is_empty_block(buttons, BLOCK_AS_ROW_START, BLOCK_AS_ROW_END,
      BLOCK_AS_COLUMN_START, BLOCK_AS_COLUMN_END);
  blockLRS = !is_empty_block(buttons, BLOCK_LRS_ROW_START, BLOCK_LRS_ROW_END,
      BLOCK_LRS_COLUMN_START, BLOCK_LRS_COLUMN_END);

  hide_row = LABELCHAR_NR_ROWS;
  hide_col = LABELCHAR_NR_COLUMNS;
  if (blockAS) { /* pattern0(full table) */
    hide_row = LABELCHAR_NR_ROWS;
    hide_col = LABELCHAR_NR_COLUMNS;
  } else if (blockLRS) {
    if (blockA) { /* pattern0(full table) */
      hide_row = LABELCHAR_NR_ROWS;
      hide_col = LABELCHAR_NR_COLUMNS;
    } else { /* pattern3(without symbol blocks) */
      hide_row = LABELCHAR_NR_ROWS;
      hide_col = BLOCK_A_COLUMN_START;
    }
  } else if (blockA) { /* pattern2(without shift blocks) */
    hide_row = BLOCK_A_ROW_END;
    hide_col = LABELCHAR_NR_COLUMNS;
  } else { /* pattern1(minimal blocks) */
    hide_row = BLOCK_A_ROW_END;
    hide_col = BLOCK_A_COLUMN_START;
  }

  for (row = 0; row < LABELCHAR_NR_ROWS; row++) {
    for (col = 0; col < LABELCHAR_NR_COLUMNS; col++) {
      GtkButton *button = NULL;
      button = get_button(buttons, INDEX(row, col));
      if (row >= hide_row || col >= hide_col) {
        gtk_widget_hide(GTK_WIDGET(button));
      } else {
        gtk_widget_show(GTK_WIDGET(button));
      }
    }
  }
  if (hide_col <= BLOCK_A_COLUMN_START) {
    col_spacing = 0;
  } else {
    col_spacing = BLOCK_SPACING;
  }
  if (hide_row <= BLOCK_LRS_ROW_START) {
    row_spacing = 0;
  } else {
    row_spacing = BLOCK_SPACING;
  }
  gtk_table_set_col_spacing(view, SPACING_RIGHT_BLOCK_COLUMN, col_spacing);
  gtk_table_set_row_spacing(view, SPACING_UP_BLOCK_ROW, row_spacing);
  if (row_spacing) {
    gtk_table_set_row_spacing(view, SPACING_SHIFT_UPPER_FAR_ROW,
        HOMEPOSITION_SPACING);
  } else {
    gtk_table_set_row_spacing(view, SPACING_SHIFT_UPPER_FAR_ROW, 0);
  }
  /* gtk_table_resize(view, hide_row, hide_col); */
  gtk_widget_show(GTK_WIDGET(view));
}

static GtkButton *
get_button(GPtrArray *buttons, gint idx)
{
  GtkButton *button = NULL;
  struct index_button *idxbutton;

  idxbutton = g_ptr_array_index(buttons, idx);
  if (idxbutton) {
    button = idxbutton->button;
  }
  return button;
}
