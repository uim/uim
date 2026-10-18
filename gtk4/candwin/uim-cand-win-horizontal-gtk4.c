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


  Rendering note
  --------------
  GTK3's version of this cell grid drew the selection highlight itself,
  by hand, in a "draw" signal handler on each cell's label (label_draw()
  painted the theme's "selected" background/foreground with Cairo,
  keyed off a GTK_STATE_FLAG_SELECTED it computed manually). GTK4
  removed the "draw" signal in favor of GtkSnapshot/the widget
  snapshot() vfunc, which would require a bespoke GtkWidget subclass
  just to reproduce what the *theme itself* already knows how to draw.

  This port takes the simpler and more idiomatic GTK4 route instead:
  each cell is an ordinary GtkButton, and "selected" is expressed by
  toggling the real GTK_STATE_FLAG_SELECTED state flag on that button
  with gtk_widget_set_state_flags()/unset_state_flags(). The theme then
  renders the selection exactly as it would for any other selected
  widget, with no custom drawing code needed at all. Each cell also
  keeps a direct pointer to its label instead of round-tripping through
  the removed GtkBin/gtk_bin_get_child().
*/

#include <config.h>

#include "uim-cand-win-horizontal-gtk4.h"
#include "cand-win-x11-util.h"
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <uim/uim.h>
#include <uim/uim-scm.h>

#define DEFAULT_MIN_WINDOW_WIDTH 60

enum {
  TERMINATOR = -1,
  COLUMN_HEADING,
  COLUMN_CANDIDATE,
  COLUMN_ANNOTATION,
  LISTSTORE_NR_COLUMNS
};

#define DEFAULT_NR_CELLS 10

struct index_button {
  gint cand_index_in_page;
  GtkWidget *button; /* GtkButton */
  GtkWidget *label;  /* direct child, replaces removed gtk_bin_get_child() */
};

static void uim_cand_win_horizontal_gtk_init(UIMCandWinHorizontalGtk *cwin);
static void uim_cand_win_horizontal_gtk_class_init(UIMCandWinGtkClass *klass);
static void uim_cand_win_horizontal_gtk_dispose(GObject *obj);
static void button_clicked(GtkButton *button, gpointer data);
static void clear_button(struct index_button *idxbutton, gint cell_index);
static void show_table(GtkGrid *view, GPtrArray *buttons);
static void scale_label(GtkWidget *label, double factor);
static void select_button(UIMCandWinHorizontalGtk *horizontal_cwin,
			   struct index_button *idxbutton);
static gboolean get_button_root_position(GtkWidget *widget, GtkWidget *toplevel,
					  gint *x, gint *y);


static GType cand_win_horizontal_type = 0;
static GTypeInfo const object_info = {
  sizeof (UIMCandWinHorizontalGtkClass),
  (GBaseInitFunc) NULL,
  (GBaseFinalizeFunc) NULL,
  (GClassInitFunc) uim_cand_win_horizontal_gtk_class_init,
  (GClassFinalizeFunc) NULL,
  NULL,                       /* class_data */
  sizeof (UIMCandWinHorizontalGtk),
  0,                          /* n_preallocs */
  (GInstanceInitFunc) uim_cand_win_horizontal_gtk_init,
};

static GtkWindowClass *parent_class = NULL;

GType
uim_cand_win_horizontal_gtk_get_type(void)
{
  if (!cand_win_horizontal_type)
    cand_win_horizontal_type = g_type_register_static(UIM_TYPE_CAND_WIN_GTK, "UIMCandWinHorizontalGtk",
						   &object_info, (GTypeFlags)0);
  return cand_win_horizontal_type;
}

static void
uim_cand_win_horizontal_gtk_class_init (UIMCandWinGtkClass *klass)
{
  GObjectClass *object_class = (GObjectClass *) klass;

  parent_class = g_type_class_peek_parent (klass);
  object_class->dispose = uim_cand_win_horizontal_gtk_dispose;

  klass->set_index = (void (*)(UIMCandWinGtk *, gint))uim_cand_win_horizontal_gtk_set_index;
  klass->set_page = (void (*)(UIMCandWinGtk *, gint))uim_cand_win_horizontal_gtk_set_page;
  klass->create_sub_window = (void (*)(UIMCandWinGtk *))uim_cand_win_horizontal_gtk_create_sub_window;
  klass->layout_sub_window = (void (*)(UIMCandWinGtk *))uim_cand_win_horizontal_gtk_layout_sub_window;
}

static struct index_button *
new_cell_button(UIMCandWinHorizontalGtk *horizontal_cwin, gint index)
{
  GtkWidget *button, *label;
  struct index_button *idxbutton;

  button = gtk_button_new();
  label = gtk_label_new("");
  gtk_button_set_child(GTK_BUTTON(button), label);
  gtk_widget_set_hexpand(button, TRUE);
  gtk_widget_set_vexpand(button, TRUE);
  g_signal_connect(button, "clicked", G_CALLBACK(button_clicked), horizontal_cwin);

  idxbutton = g_malloc(sizeof(struct index_button));
  idxbutton->button = button;
  idxbutton->label = label;
  scale_label(label, PANGO_SCALE_LARGE);
  idxbutton->cand_index_in_page = -1;

  gtk_grid_attach(GTK_GRID(UIM_CAND_WIN_GTK(horizontal_cwin)->view), button,
                  index, 0, 1, 1);

  return idxbutton;
}

static void
uim_cand_win_horizontal_gtk_init (UIMCandWinHorizontalGtk *horizontal_cwin)
{
  gint col;
  GtkWidget *viewport;
  UIMCandWinGtk *cwin;

  cwin = UIM_CAND_WIN_GTK(horizontal_cwin);

  horizontal_cwin->buttons = g_ptr_array_new();
  horizontal_cwin->selected = NULL;

  cwin->view = gtk_grid_new();
  gtk_grid_set_column_spacing(GTK_GRID(cwin->view), 10);
  viewport = gtk_viewport_new(NULL, NULL);
  gtk_viewport_set_child(GTK_VIEWPORT(viewport), cwin->view);
  gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(cwin->scrolled_window), viewport);

  for (col = 0; col < DEFAULT_NR_CELLS; col++) {
    struct index_button *idxbutton = new_cell_button(horizontal_cwin, col);
    g_ptr_array_add(horizontal_cwin->buttons, idxbutton);
  }

  gtk_widget_set_size_request(cwin->num_label, DEFAULT_MIN_WINDOW_WIDTH, -1);
  gtk_window_set_default_size(GTK_WINDOW(cwin), DEFAULT_MIN_WINDOW_WIDTH, -1);
  gtk_window_set_resizable(GTK_WINDOW(cwin), FALSE);
}

static void
select_button(UIMCandWinHorizontalGtk *horizontal_cwin, struct index_button *idxbutton)
{
  struct index_button *prev_selected = horizontal_cwin->selected;

  if (prev_selected && prev_selected != idxbutton)
    gtk_widget_unset_state_flags(prev_selected->button, GTK_STATE_FLAG_SELECTED);

  if (idxbutton)
    gtk_widget_set_state_flags(idxbutton->button, GTK_STATE_FLAG_SELECTED, FALSE);

  horizontal_cwin->selected = idxbutton;
}

static void
button_clicked(GtkButton *button, gpointer data)
{
  UIMCandWinHorizontalGtk *horizontal_cwin = data;
  UIMCandWinGtk *cwin = UIM_CAND_WIN_GTK(horizontal_cwin);
  gint i;
  gint idx = -1;

  for (i = 0; i < (gint)horizontal_cwin->buttons->len; i++) {
    struct index_button *idxbutton = g_ptr_array_index(horizontal_cwin->buttons, i);
    if (!idxbutton)
      continue;
    if (idxbutton->button == GTK_WIDGET(button)) {
      idx = idxbutton->cand_index_in_page;
      select_button(horizontal_cwin, idxbutton);
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
uim_cand_win_horizontal_gtk_dispose (GObject *obj)
{
  UIMCandWinHorizontalGtk *horizontal_cwin;

  g_return_if_fail(UIM_IS_CAND_WIN_HORIZONTAL_GTK(obj));

  horizontal_cwin = UIM_CAND_WIN_HORIZONTAL_GTK(obj);

  if (horizontal_cwin->buttons) {
    guint i;
    for (i = 0; i < horizontal_cwin->buttons->len; i++) {
      g_free(horizontal_cwin->buttons->pdata[i]);
      /* the GtkButton itself is destroyed with the grid */
    }
    g_ptr_array_free(horizontal_cwin->buttons, TRUE);
    horizontal_cwin->buttons = NULL;
  }
  horizontal_cwin->selected = NULL;

  if (G_OBJECT_CLASS (parent_class)->dispose)
    G_OBJECT_CLASS (parent_class)->dispose(obj);
}

UIMCandWinHorizontalGtk *
uim_cand_win_horizontal_gtk_new (void)
{
  GObject *obj = g_object_new(UIM_TYPE_CAND_WIN_HORIZONTAL_GTK, NULL);

  return UIM_CAND_WIN_HORIZONTAL_GTK(obj);
}

static struct index_button *
assign_cellbutton(UIMCandWinHorizontalGtk *horizontal_cwin,
		  gint cand_index, gint display_limit)
{
  struct index_button *idxbutton;
  int len;
  GPtrArray *buttons;

  buttons = horizontal_cwin->buttons;
  len = buttons->len;

  if (len <= cand_index) {
    idxbutton = new_cell_button(horizontal_cwin, cand_index);
    idxbutton->cand_index_in_page = cand_index;
    g_ptr_array_add(buttons, idxbutton);
  } else {
    idxbutton = g_ptr_array_index(buttons, cand_index);
    idxbutton->cand_index_in_page = cand_index;
  }

  return idxbutton;
}

void
uim_cand_win_horizontal_gtk_set_index(UIMCandWinHorizontalGtk *horizontal_cwin, gint index)
{
  gint new_page;
  UIMCandWinGtk *cwin;

  g_return_if_fail(UIM_IS_CAND_WIN_HORIZONTAL_GTK(horizontal_cwin));
  cwin = UIM_CAND_WIN_GTK(horizontal_cwin);

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
    gint pos;
    struct index_button *idxbutton;

    if (cwin->display_limit)
      pos = cwin->candidate_index % cwin->display_limit;
    else
      pos = cwin->candidate_index;

    idxbutton = g_ptr_array_index(horizontal_cwin->buttons, pos);
    select_button(horizontal_cwin, idxbutton);

    /* show subwin */
    if (cwin->stores->pdata[new_page]) {
      char *annotation = NULL;
      GtkTreeModel *model = GTK_TREE_MODEL(cwin->stores->pdata[new_page]);
      GtkTreeIter iter;

      gtk_tree_model_iter_nth_child(model, &iter, NULL, pos);
      gtk_tree_model_get(model, &iter, COLUMN_ANNOTATION, &annotation, -1);

      if (annotation && *annotation) {
	if (!cwin->sub_window.window)
          uim_cand_win_gtk_create_sub_window(cwin);
	gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(cwin->sub_window.text_view)), annotation, -1);
	uim_cand_win_gtk_layout_sub_window(cwin);
	gtk_widget_set_visible(cwin->sub_window.window, TRUE);
	cwin->sub_window.active = TRUE;
      } else {
        if (cwin->sub_window.window) {
          gtk_widget_set_visible(cwin->sub_window.window, FALSE);
          cwin->sub_window.active = FALSE;
	}
      }
      free(annotation);
    }
  } else {
    select_button(horizontal_cwin, NULL);
    if (cwin->sub_window.window) {
      gtk_widget_set_visible(cwin->sub_window.window, FALSE);
      cwin->sub_window.active = FALSE;
    }
  }

  uim_cand_win_gtk_update_label(cwin);
}

static void
scale_label(GtkWidget *label, double scale)
{
  PangoAttrList *attrs = pango_attr_list_new();
  PangoAttribute *attr = pango_attr_scale_new(scale);

  pango_attr_list_insert(attrs, attr);
  if (GTK_IS_LABEL(label))
    gtk_label_set_attributes(GTK_LABEL(label), attrs);
  pango_attr_list_unref(attrs);
}

static void
clear_button(struct index_button *idxbutton, gint cell_index)
{
  idxbutton->cand_index_in_page = -1;
  gtk_label_set_text(GTK_LABEL(idxbutton->label), "");
  scale_label(idxbutton->label, PANGO_SCALE_LARGE);
  gtk_widget_unset_state_flags(idxbutton->button, GTK_STATE_FLAG_SELECTED);
}

static void
clear_all_buttons(GPtrArray *buttons)
{
  gint i;

  for (i = 0; i < (gint)buttons->len; i++) {
    struct index_button *idxbutton;

    idxbutton = g_ptr_array_index(buttons, i);
    if (idxbutton && idxbutton->cand_index_in_page != -1) {
      clear_button(idxbutton, i);
    }
  }
}

static void
update_table_button(UIMCandWinHorizontalGtk *horizontal_cwin, guint new_page)
{
  UIMCandWinGtk *cwin;
  GtkTreeModel *model;
  GPtrArray *buttons;
  GtkTreeIter ti;
  gboolean has_next;
  gint display_limit, len, cand_index = 0;

  cwin = UIM_CAND_WIN_GTK(horizontal_cwin);
  if (!cwin->stores->pdata[new_page]) {
    return;
  }
  model = GTK_TREE_MODEL(cwin->stores->pdata[new_page]);
  buttons = horizontal_cwin->buttons;
  display_limit = cwin->display_limit;
  len = buttons->len;

  clear_all_buttons(buttons);
  has_next = gtk_tree_model_get_iter_first(model, &ti);
  while (has_next) {
    gchar *heading;
    gchar *cand_str;

    gtk_tree_model_get(model, &ti, COLUMN_HEADING, &heading,
        COLUMN_CANDIDATE, &cand_str, TERMINATOR);
    if (cand_str != NULL) {
      struct index_button *idxbutton = assign_cellbutton(horizontal_cwin, cand_index, display_limit);
      if (idxbutton != NULL) {
        if (heading && heading[0] != '\0') {
          gchar *text = g_strdup_printf("%s: %s", heading, cand_str);
          gtk_label_set_text(GTK_LABEL(idxbutton->label), text);
          g_free(text);
	} else {
          gtk_label_set_text(GTK_LABEL(idxbutton->label), cand_str);
	}
	scale_label(idxbutton->label, PANGO_SCALE_LARGE);
      }
    }

    g_free(cand_str);
    g_free(heading);
    cand_index++;
    has_next = gtk_tree_model_iter_next(model, &ti);
  }

  if (cand_index < len) {
    gint i;
    for (i = len - 1; i >= cand_index; i--) {
      struct index_button *idxbutton;
      idxbutton = g_ptr_array_index(buttons, i);
      if (idxbutton == horizontal_cwin->selected)
        horizontal_cwin->selected = NULL;
      gtk_grid_remove(GTK_GRID(cwin->view), idxbutton->button);
      g_free(idxbutton);
      g_ptr_array_remove_index(buttons, i);
    }
  }
}

void
uim_cand_win_horizontal_gtk_set_page(UIMCandWinHorizontalGtk *horizontal_cwin, gint page)
{
  guint len, new_page;
  gint new_index;
  UIMCandWinGtk *cwin;

  g_return_if_fail(UIM_IS_CAND_WIN_HORIZONTAL_GTK(horizontal_cwin));
  cwin = UIM_CAND_WIN_GTK(horizontal_cwin);
  g_return_if_fail(cwin->stores);

  len = cwin->stores->len;
  g_return_if_fail(len);

  if (page < 0)
    new_page = len - 1;
  else if (page >= (gint) len)
    new_page = 0;
  else
    new_page = page;

  update_table_button(horizontal_cwin, new_page);
  show_table(GTK_GRID(cwin->view), horizontal_cwin->buttons);

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

  /* check if (cwin->candidate_index != new_index) ?? */
  uim_cand_win_gtk_set_index(cwin, new_index);
}

static void
show_table(GtkGrid *view, GPtrArray *buttons)
{
  gint col;

  for (col = 0; col < (gint)buttons->len; col++) {
    struct index_button *idxbutton = g_ptr_array_index(buttons, col);
    gtk_widget_set_visible(idxbutton->button, TRUE);
  }
  gtk_widget_set_visible(GTK_WIDGET(view), TRUE);
}

#define UIM_ANNOTATION_WIN_WIDTH 280
#define UIM_ANNOTATION_WIN_HEIGHT 140

void
uim_cand_win_horizontal_gtk_create_sub_window(UIMCandWinHorizontalGtk *horizontal_cwin)
{
  GtkWidget *window, *scrwin, *text_view, *frame;
  UIMCandWinGtk *cwin;

  g_return_if_fail(UIM_IS_CAND_WIN_HORIZONTAL_GTK(horizontal_cwin));
  cwin = UIM_CAND_WIN_GTK(horizontal_cwin);

  if (cwin->sub_window.window)
    return;

  cwin->sub_window.window = window = gtk_window_new();
  gtk_window_set_decorated(GTK_WINDOW(window), FALSE);
  gtk_window_set_default_size(GTK_WINDOW(window),
			      UIM_ANNOTATION_WIN_WIDTH,
			      UIM_ANNOTATION_WIN_HEIGHT);
  gtk_window_set_resizable(GTK_WINDOW(window), FALSE);

  frame = gtk_frame_new(NULL);

  cwin->sub_window.scrolled_window = scrwin = gtk_scrolled_window_new();
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrwin),
                                 GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

  cwin->sub_window.text_view = text_view = gtk_text_view_new();
  gtk_text_view_set_editable(GTK_TEXT_VIEW(text_view), FALSE);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text_view), GTK_WRAP_WORD_CHAR);

  gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(scrwin), text_view);
  gtk_frame_set_child(GTK_FRAME(frame), scrwin);
  gtk_window_set_child(GTK_WINDOW(window), frame);
}

/* Translate widget's top-left corner into toplevel's coordinate space,
 * then add toplevel's own screen origin -- the GTK4 way to learn a
 * plain (non-native, non-GdkSurface-backed) widget's absolute screen
 * position. */
static gboolean
get_button_root_position(GtkWidget *widget, GtkWidget *toplevel, gint *x, gint *y)
{
  double bx = 0, by = 0;
  gint tx, ty;

  if (!gtk_widget_translate_coordinates(widget, toplevel, 0, 0, &bx, &by))
    return FALSE;
  if (!cand_win_get_window_origin(toplevel, &tx, &ty))
    return FALSE;

  *x = tx + (gint)bx;
  *y = ty + (gint)by;
  return TRUE;
}

void
uim_cand_win_horizontal_gtk_layout_sub_window(UIMCandWinHorizontalGtk *horizontal_cwin)
{
  UIMCandWinGtk *cwin;
  gint cwin_x, cwin_y, bx, by;
  GtkRequisition cwin_req;

  g_return_if_fail(UIM_IS_CAND_WIN_HORIZONTAL_GTK(horizontal_cwin));
  cwin = UIM_CAND_WIN_GTK(horizontal_cwin);

  if (!cwin->sub_window.window)
    return;

  if (!cand_win_get_window_origin(GTK_WIDGET(cwin), &cwin_x, &cwin_y))
    return;

  gtk_widget_get_preferred_size(GTK_WIDGET(cwin), &cwin_req, NULL);

  bx = cwin_x;
  by = cwin_y;
  if (horizontal_cwin->selected) {
    struct index_button *idxbutton = horizontal_cwin->selected;
    get_button_root_position(idxbutton->button, GTK_WIDGET(cwin), &bx, &by);
  }

  cand_win_move_window(cwin->sub_window.window, bx, cwin_y + cwin_req.height);
}
