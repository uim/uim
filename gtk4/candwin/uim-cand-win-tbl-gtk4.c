/*

  copyright (c) 2003-2026 uim Project https://github.com/uim/uim

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


  GTK4 dropped GtkTable (and its per-column/per-row "spacing after this
  line" API) in favor of GtkGrid, which only supports one uniform
  row-spacing and column-spacing value for the whole grid. The block
  gaps this table layout relies on (separating the home-row keyboard
  blocks from the far/shifted blocks) are reproduced here with
  per-button margins instead, which is the direct GTK4 equivalent of
  "extra space after this particular row/column".

  GtkButton's GTK_RELIEF_NONE/HALF/NORMAL styles were also replaced by
  the simpler has-frame boolean; the NORMAL/HALF distinction (labeled
  vs. unlabeled-but-active cell) is not visually reproducible any more
  and both simply get a frame.
*/

#include <config.h>

#include "uim-cand-win-tbl-gtk4.h"
#include <string.h>
#include <stdlib.h>
#include <uim/uim.h>
#include <uim/uim-scm.h>

#define DEFAULT_MIN_WINDOW_WIDTH 80

enum {
  TERMINATOR = -1,
  COLUMN_HEADING,
  COLUMN_CANDIDATE,
  COLUMN_ANNOTATION,
  LISTSTORE_NR_COLUMNS
};

#define TABLE_NR_COLUMNS 13
#define TABLE_NR_ROWS 8
#define TABLE_NR_CELLS (TABLE_NR_COLUMNS * TABLE_NR_ROWS)
#define CELLINDEX(row,col) ((row) * TABLE_NR_COLUMNS + (col))
/* 106 keyboard */
static gchar default_tbl_cell2label[TABLE_NR_CELLS] = {
  '1','2','3','4','5', '6','7','8','9','0',   '-','^','\\',
  'q','w','e','r','t', 'y','u','i','o','p',   '@','[','\0',
  'a','s','d','f','g', 'h','j','k','l',';',   ':',']','\0',
  'z','x','c','v','b', 'n','m',',','.','/',   '\0','\0',' ',
  '!','"','#','$','%', '&','\'','(',')','\0', '=','~','|',
  'Q','W','E','R','T', 'Y','U','I','O','P',   '`','{','\0',
  'A','S','D','F','G', 'H','J','K','L','+',   '*','}','\0',
  'Z','X','C','V','B', 'N','M','<','>','?',   '_','\0','\0',
};
/* table consists of four blocks
 *   blockLR  blockA
 *   blockLRS blockAS
 */
#define BLOCK_A_ROW_START 0
#define BLOCK_A_ROW_END 4
#define BLOCK_A_COLUMN_START 10
#define BLOCK_A_COLUMN_END TABLE_NR_COLUMNS
#define BLOCK_LRS_ROW_START BLOCK_A_ROW_END
#define BLOCK_LRS_ROW_END TABLE_NR_ROWS
#define BLOCK_LRS_COLUMN_START 0
#define BLOCK_LRS_COLUMN_END BLOCK_A_COLUMN_START
#define BLOCK_AS_ROW_START BLOCK_LRS_ROW_START
#define BLOCK_AS_ROW_END BLOCK_LRS_ROW_END
#define BLOCK_AS_COLUMN_START BLOCK_LRS_COLUMN_END
#define BLOCK_AS_COLUMN_END TABLE_NR_COLUMNS
#define BLOCK_LR_NR_CELLS (BLOCK_A_ROW_END * BLOCK_A_COLUMN_START)
#define BLOCK_LRS_NR_CELLS ((BLOCK_LRS_ROW_END - BLOCK_LRS_ROW_START) * (BLOCK_LRS_COLUMN_END - BLOCK_LRS_COLUMN_START))

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
static gchar	*init_tbl_cell2label(void);
static void	button_clicked(GtkButton *button, gpointer data);
static void	clear_button(struct index_button *idxbutton,
                             const gchar *tbl_cell2label, gint cell_index);
static void	show_table(UIMCandWinTblGtk *ctblwin);
static GtkButton *get_button(GPtrArray *buttons, gint idx);
static void	set_col_spacing(UIMCandWinTblGtk *ctblwin, gint col, gint spacing);
static void	set_row_spacing(UIMCandWinTblGtk *ctblwin, gint row, gint spacing);


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

static void
uim_cand_win_tbl_gtk_class_init (UIMCandWinGtkClass *klass)
{
  GObjectClass *object_class = (GObjectClass *) klass;

  parent_class = g_type_class_peek_parent (klass);
  object_class->dispose = uim_cand_win_tbl_gtk_dispose;

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
  ctblwin->tbl_cell2label = init_tbl_cell2label();

  cwin->view = gtk_grid_new();
  gtk_grid_set_row_homogeneous(GTK_GRID(cwin->view), TRUE);
  gtk_grid_set_column_homogeneous(GTK_GRID(cwin->view), TRUE);
  viewport = gtk_viewport_new(NULL, NULL);
  gtk_viewport_set_child(GTK_VIEWPORT(viewport), cwin->view);
  gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(cwin->scrolled_window), viewport);

  for (row = 0; row < TABLE_NR_ROWS; row++) {
    for (col = 0; col < TABLE_NR_COLUMNS; col++) {
      GtkWidget *button;
      struct index_button *idxbutton;
      button = gtk_button_new_with_label("  ");
      g_signal_connect(button, "clicked", G_CALLBACK(button_clicked), ctblwin);
      gtk_grid_attach(GTK_GRID(cwin->view), button, col, row, 1, 1);
      idxbutton = g_malloc(sizeof(struct index_button));
      if (idxbutton) {
        idxbutton->button = GTK_BUTTON(button);
        clear_button(idxbutton, ctblwin->tbl_cell2label, CELLINDEX(row, col));
      }
      g_ptr_array_add(ctblwin->buttons, idxbutton);
    }
  }
  set_col_spacing(ctblwin, SPACING_LEFT_BLOCK_COLUMN, BLOCK_SPACING);
  set_col_spacing(ctblwin, SPACING_RIGHT_BLOCK_COLUMN, BLOCK_SPACING);
  set_row_spacing(ctblwin, SPACING_UP_BLOCK_ROW, BLOCK_SPACING);
  set_col_spacing(ctblwin, SPACING_LEFTHAND_FAR_COLUMN, HOMEPOSITION_SPACING);
  set_col_spacing(ctblwin, SPACING_RIGHTHAND_FAR_COLUMN, HOMEPOSITION_SPACING);
  set_row_spacing(ctblwin, SPACING_UPPER_FAR_ROW, HOMEPOSITION_SPACING);
  set_row_spacing(ctblwin, SPACING_SHIFT_UPPER_FAR_ROW, HOMEPOSITION_SPACING);

  gtk_widget_set_size_request(cwin->num_label, DEFAULT_MIN_WINDOW_WIDTH, -1);
  gtk_window_set_default_size(GTK_WINDOW(cwin), DEFAULT_MIN_WINDOW_WIDTH, -1);
  gtk_window_set_resizable(GTK_WINDOW(cwin), FALSE);
}

/* Emulate GtkTable's "spacing after column N" with margin-end on every
 * button in that column. */
static void
set_col_spacing(UIMCandWinTblGtk *ctblwin, gint col, gint spacing)
{
  gint row;

  for (row = 0; row < TABLE_NR_ROWS; row++) {
    struct index_button *idxbutton = g_ptr_array_index(ctblwin->buttons, CELLINDEX(row, col));
    if (idxbutton && idxbutton->button)
      gtk_widget_set_margin_end(GTK_WIDGET(idxbutton->button), spacing);
  }
}

/* Emulate GtkTable's "spacing after row N" with margin-bottom on every
 * button in that row. */
static void
set_row_spacing(UIMCandWinTblGtk *ctblwin, gint row, gint spacing)
{
  gint col;

  for (col = 0; col < TABLE_NR_COLUMNS; col++) {
    struct index_button *idxbutton = g_ptr_array_index(ctblwin->buttons, CELLINDEX(row, col));
    if (idxbutton && idxbutton->button)
      gtk_widget_set_margin_bottom(GTK_WIDGET(idxbutton->button), spacing);
  }
}

static gchar *
init_tbl_cell2label(void)
{
  gchar *table;
  uim_lisp list;
  size_t len = 0;
  uim_lisp *ary0, *ary;
  guint i;

  list = uim_scm_symbol_value("uim-candwin-prog-layout");
  if (list == NULL || !uim_scm_listp(list)) {
    return default_tbl_cell2label;
  }
  ary0 = ary = (uim_lisp *)uim_scm_list2array(list, &len, NULL);
  if (ary == NULL || len <= 0) {
    free(ary0);
    return default_tbl_cell2label;
  }
  table = (gchar *)g_malloc0(TABLE_NR_CELLS);
  if (table == NULL) {
    free(ary0);
    return default_tbl_cell2label;
  }
  for (i = 0; i < len && i < TABLE_NR_CELLS; i++, ary++) {
    char *str;
    if (!uim_scm_strp(*ary)) {
      /* XXX: output notify message? */
      g_free(table);
      free(ary0);
      return default_tbl_cell2label;
    }
    str = uim_scm_c_str(*ary);
    if (str) {
      /* XXX: only use first char */
      table[i] = *str;
      free(str);
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

  for (i = 0; i < TABLE_NR_CELLS; i++) {
    GtkButton *p;
    struct index_button *idxbutton;
    idxbutton = g_ptr_array_index(ctblwin->buttons, i);
    if (!idxbutton) {
      continue;
    }
    p = idxbutton->button;
    if (p == button) {
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

  if (ctblwin->tbl_cell2label != default_tbl_cell2label) {
    g_free(ctblwin->tbl_cell2label);
    ctblwin->tbl_cell2label = NULL;
  }
  if (ctblwin->buttons) {
    guint i;
    for (i = 0; i < ctblwin->buttons->len; i++) {
      g_free(ctblwin->buttons->pdata[i]);
      /* GtkButton is destroyed with the grid */
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
  GObject *obj = g_object_new(UIM_TYPE_CAND_WIN_TBL_GTK, NULL);
  return UIM_CAND_WIN_TBL_GTK(obj);
}

static GtkButton*
assign_cellbutton(GPtrArray *buttons, const gchar *tbl_cell2label,
    const gchar labelchar, gint cand_index, gint display_limit,
    gboolean *has_label)
{
  gint i;
  struct index_button *idxbutton;

  if (labelchar != '\0') {
    /* find button by labelchar */
    for (i = 0; i < TABLE_NR_CELLS; i++) {
      if (tbl_cell2label[i] == labelchar) {
        idxbutton = g_ptr_array_index(buttons, i);
        if (!idxbutton) {
          continue;
        }
        if (idxbutton->cand_index_in_page != -1) {
          break; /* already used */
        }
        idxbutton->cand_index_in_page = cand_index;
        *has_label = TRUE;
        return idxbutton->button;
      }
    }
  }
  /* labelchar not found || already used */

  /* find free cell */
  for (i = 0; i < TABLE_NR_CELLS; i++) {
    if (display_limit && display_limit <= BLOCK_LR_NR_CELLS + BLOCK_LRS_NR_CELLS
        && i % TABLE_NR_COLUMNS >= BLOCK_A_COLUMN_START) {
      /* skip blockA which is far from home position */
      i += TABLE_NR_COLUMNS - BLOCK_A_COLUMN_START - 1;
      continue;
    }
    idxbutton = g_ptr_array_index(buttons, i);
    if (!idxbutton) {
      continue;
    }
    if (idxbutton->cand_index_in_page == -1) {
      idxbutton->cand_index_in_page = cand_index;
      *has_label = FALSE;
      return idxbutton->button;
    }
  }

  /* failed to assign button */
  *has_label = FALSE;
  return NULL;
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
clear_button(struct index_button *idxbutton, const gchar *tbl_cell2label,
    gint cell_index)
{
  GtkButton *button = NULL;
  gboolean is_blank_cell = (tbl_cell2label[cell_index] == '\0') ? TRUE : FALSE;

  idxbutton->cand_index_in_page = -1;
  button = idxbutton->button;
  gtk_button_set_has_frame(button, !is_blank_cell);
  gtk_widget_set_sensitive(GTK_WIDGET(button), FALSE);
  gtk_button_set_label(button, "  ");
}

static void
clear_all_buttons(GPtrArray *buttons, const gchar *tbl_cell2label)
{
  gint i;

  for (i = 0; i < TABLE_NR_CELLS; i++) {
    struct index_button *idxbutton;

    idxbutton = g_ptr_array_index(buttons, i);
    if (idxbutton && idxbutton->cand_index_in_page != -1) {
      clear_button(idxbutton, tbl_cell2label, i);
    }
  }
}

static void
update_table_button(GtkTreeModel *model, GPtrArray *buttons,
    const gchar *tbl_cell2label, gint display_limit)
{
  GtkTreeIter ti;
  gboolean has_next;
  gint cand_index = 0;

  clear_all_buttons(buttons, tbl_cell2label);
  has_next = gtk_tree_model_get_iter_first(model, &ti);
  while (has_next) {
    gchar *heading = NULL;
    gchar *cand_str = NULL;
    GtkButton *button = NULL;

    gtk_tree_model_get(model, &ti, COLUMN_HEADING, &heading,
        COLUMN_CANDIDATE, &cand_str, TERMINATOR);
    if (cand_str != NULL) {
      gboolean has_label = FALSE;
      gchar ch = (heading == NULL) ? '\0' : heading[0];
      button = assign_cellbutton(buttons, tbl_cell2label, ch, cand_index,
          display_limit, &has_label);
      if (button != NULL) {
        /* NORMAL vs HALF relief no longer distinguishable in GTK4;
         * both simply get a frame. */
        gtk_button_set_has_frame(button, TRUE);
        gtk_widget_set_sensitive(GTK_WIDGET(button), TRUE);
        gtk_button_set_label(button, cand_str);
      }
    }

    g_free(cand_str);
    g_free(heading);
    cand_index++;
    has_next = gtk_tree_model_iter_next(model, &ti);
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

  if (cwin->stores->pdata[new_page]) {
    update_table_button(GTK_TREE_MODEL(cwin->stores->pdata[new_page]),
                        ctblwin->buttons, ctblwin->tbl_cell2label,
                        cwin->display_limit);
    show_table(ctblwin);
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

  uim_cand_win_gtk_set_index(cwin, new_index);
}

static gboolean
is_empty_block(GPtrArray *buttons, gint rowstart, gint rowend, gint colstart, gint colend)
{
  gint row, col;
  for (row = rowstart; row < rowend; row++) {
    for (col = colstart; col < colend; col++) {
      struct index_button *idxbutton;
      idxbutton = g_ptr_array_index(buttons, CELLINDEX(row, col));
      if (idxbutton && idxbutton->cand_index_in_page != -1) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

static void
show_table(UIMCandWinTblGtk *ctblwin)
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
  GPtrArray *buttons = ctblwin->buttons;
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

  hide_row = TABLE_NR_ROWS;
  hide_col = TABLE_NR_COLUMNS;
  if (blockAS) { /* pattern0(full table) */
    hide_row = TABLE_NR_ROWS;
    hide_col = TABLE_NR_COLUMNS;
  } else if (blockLRS) {
    if (blockA) { /* pattern0(full table) */
      hide_row = TABLE_NR_ROWS;
      hide_col = TABLE_NR_COLUMNS;
    } else { /* pattern3(without symbol blocks) */
      hide_row = TABLE_NR_ROWS;
      hide_col = BLOCK_A_COLUMN_START;
    }
  } else if (blockA) { /* pattern2(without shift blocks) */
    hide_row = BLOCK_A_ROW_END;
    hide_col = TABLE_NR_COLUMNS;
  } else { /* pattern1(minimal blocks) */
    hide_row = BLOCK_A_ROW_END;
    hide_col = BLOCK_A_COLUMN_START;
  }

  for (row = 0; row < TABLE_NR_ROWS; row++) {
    for (col = 0; col < TABLE_NR_COLUMNS; col++) {
      GtkButton *button = NULL;
      button = get_button(buttons, CELLINDEX(row, col));
      if (row >= hide_row || col >= hide_col) {
        gtk_widget_set_visible(GTK_WIDGET(button), FALSE);
      } else {
        gtk_widget_set_visible(GTK_WIDGET(button), TRUE);
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
  set_col_spacing(ctblwin, SPACING_RIGHT_BLOCK_COLUMN, col_spacing);
  set_row_spacing(ctblwin, SPACING_UP_BLOCK_ROW, row_spacing);
  if (row_spacing) {
    set_row_spacing(ctblwin, SPACING_SHIFT_UPPER_FAR_ROW, HOMEPOSITION_SPACING);
  } else {
    set_row_spacing(ctblwin, SPACING_SHIFT_UPPER_FAR_ROW, 0);
  }
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
