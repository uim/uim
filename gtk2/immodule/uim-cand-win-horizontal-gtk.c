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

#include "uim-cand-win-horizontal-gtk.h"
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
  GtkEventBox *button;
};

static void uim_cand_win_horizontal_gtk_init(UIMCandWinHorizontalGtk *cwin);
static void uim_cand_win_horizontal_gtk_class_init(UIMCandWinGtkClass *klass);
static void uim_cand_win_horizontal_gtk_dispose(GObject *obj);
static void button_clicked(GtkEventBox *button, GdkEventButton *event, gpointer data);
#if GTK_CHECK_VERSION(2, 90, 0)
static gboolean label_draw(GtkWidget *label, cairo_t *cr, gpointer data);
#else
static gboolean label_exposed(GtkWidget *label, GdkEventExpose *event, gpointer data);
#endif
static void clear_button(struct index_button *idxbutton, gint cell_index);
#if GTK_CHECK_VERSION(3, 4, 0)
static void show_table(GtkGrid *view, GPtrArray *buttons);
#else
static void show_table(GtkTable *view, GPtrArray *buttons);
#endif
static void scale_label(GtkEventBox *button, double factor);


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

GType
uim_cand_win_horizontal_gtk_register_type(GTypeModule *module)
{
  if (!cand_win_horizontal_type)
    cand_win_horizontal_type = g_type_module_register_type(module,
						UIM_TYPE_CAND_WIN_GTK,
						"UIMCandWinHorizontalGtk",
						&object_info, 0);
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

static void
uim_cand_win_horizontal_gtk_init (UIMCandWinHorizontalGtk *horizontal_cwin)
{
  gint col;
  GtkWidget *viewport;
  UIMCandWinGtk *cwin;

  cwin = UIM_CAND_WIN_GTK(horizontal_cwin);

  horizontal_cwin->buttons = g_ptr_array_new();
  horizontal_cwin->selected = NULL;

#if GTK_CHECK_VERSION(3, 4, 0)
  cwin->view = gtk_grid_new();
  gtk_grid_set_column_spacing(GTK_GRID(cwin->view), 10);
#else
  cwin->view = gtk_table_new(1, DEFAULT_NR_CELLS, FALSE);
  gtk_table_set_col_spacings(GTK_TABLE(cwin->view), 10);
#endif
  viewport = gtk_viewport_new(NULL, NULL);
  gtk_container_add(GTK_CONTAINER(viewport), cwin->view);
  gtk_container_add(GTK_CONTAINER(cwin->scrolled_window), viewport);
  gtk_container_set_resize_mode(GTK_CONTAINER(viewport), GTK_RESIZE_PARENT);
  for (col = 0; col < DEFAULT_NR_CELLS; col++) {
    GtkWidget *button;
    GtkWidget *label;
    struct index_button *idxbutton;

    button = gtk_event_box_new();
    gtk_event_box_set_above_child(GTK_EVENT_BOX(button), TRUE);
    label = gtk_label_new("");
    gtk_container_add(GTK_CONTAINER(button), label);
    scale_label(GTK_EVENT_BOX(button), PANGO_SCALE_LARGE);
    g_signal_connect(button, "button-press-event", G_CALLBACK(button_clicked), horizontal_cwin);
#if GTK_CHECK_VERSION(2, 90, 0)
    g_signal_connect_after(label, "draw", G_CALLBACK(label_draw), horizontal_cwin);
#else
    g_signal_connect_after(label, "expose-event", G_CALLBACK(label_exposed), horizontal_cwin);
#endif
#if GTK_CHECK_VERSION(3, 4, 0)
    gtk_widget_set_hexpand(button, TRUE);
    gtk_widget_set_vexpand(button, TRUE);
    gtk_grid_attach(GTK_GRID(cwin->view), button, col, 0, 1, 1);
#else
    gtk_table_attach_defaults(GTK_TABLE(cwin->view), button, col, col + 1, 0, 1);
#endif
    idxbutton = g_malloc(sizeof(struct index_button));
    if (idxbutton) {
      idxbutton->button = GTK_EVENT_BOX(button);
      clear_button(idxbutton, col);
    }
    g_ptr_array_add(horizontal_cwin->buttons, idxbutton);
  }

  gtk_widget_show_all(cwin->view);
  gtk_widget_show(viewport);

  gtk_widget_set_size_request(cwin->num_label, DEFAULT_MIN_WINDOW_WIDTH, -1);
  gtk_window_set_default_size(GTK_WINDOW(cwin), DEFAULT_MIN_WINDOW_WIDTH, -1);
  gtk_window_set_resizable(GTK_WINDOW(cwin), FALSE);
}

#if !GTK_CHECK_VERSION(2, 90, 0)
static void
get_layout_x(GtkLabel *label, gint *xp)
{
  GtkMisc *misc;
  GtkWidget *widget; 
  gfloat xalign;
  gint req_width, x;
  
  misc = GTK_MISC(label);
  widget = GTK_WIDGET(label);

  if (gtk_widget_get_direction(widget) == GTK_TEXT_DIR_LTR)
    xalign = misc->xalign;
  else
    xalign = 1.0 - misc->xalign;

  req_width = widget->requisition.width;

  x = floor(widget->allocation.x + (gint)misc->xpad +
	     xalign * (widget->allocation.width - req_width));

  if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR)
    x = MAX(x, widget->allocation.x + misc->xpad);
  else
    x = MIN(x, widget->allocation.x + widget->allocation.width - misc->xpad);

  if (xp)
    *xp = x;
}
#endif

#if GTK_CHECK_VERSION(2, 90, 0)
static gboolean
label_draw(GtkWidget *label, cairo_t *cr, gpointer data)
{
  UIMCandWinHorizontalGtk *horizontal_cwin = data;
  struct index_button *selected;
  GtkWidget *selected_label = NULL;
  GdkRGBA *bg_color, *fg_color;
  GtkStyleContext *context;
  PangoLayout *layout;
  gint x, y;
  GtkStateFlags state;

  selected = horizontal_cwin->selected;
  if (selected)
    selected_label = gtk_bin_get_child(GTK_BIN(selected->button));

  layout = gtk_label_get_layout(GTK_LABEL(label));
  gtk_label_get_layout_offsets(GTK_LABEL(label), &x, &y);

  context = gtk_widget_get_style_context(label);

  if (label == selected_label)
    state = GTK_STATE_FLAG_SELECTED;
  else
    state = GTK_STATE_FLAG_NORMAL;

  gtk_style_context_get (context, state, "background-color", &bg_color, "color", &fg_color, NULL);

  cairo_save(cr);
  gdk_cairo_set_source_rgba(cr, bg_color);
  cairo_paint(cr);
  cairo_restore(cr);
  gdk_rgba_free(bg_color);
  gdk_rgba_free(fg_color);

  gtk_style_context_set_state (context, state);
  gtk_render_layout (context, cr, x, y, layout);

  return FALSE;
}
#else
static gboolean
label_exposed(GtkWidget *label, GdkEventExpose *event, gpointer data)
{
  UIMCandWinHorizontalGtk *horizontal_cwin = data;
  struct index_button *selected;
  GtkWidget *selected_label = NULL;

  selected = horizontal_cwin->selected;
  if (selected)
    selected_label = gtk_bin_get_child(GTK_BIN(selected->button));

  if (label == selected_label) {
    gint x;
    get_layout_x(GTK_LABEL(label), &x);
    gdk_draw_layout_with_colors(label->window,
		      label->style->black_gc, x, 0,
		      GTK_LABEL(label)->layout,
		      &label->style->text[GTK_STATE_SELECTED],
		      &label->style->bg[GTK_STATE_SELECTED]);
  }

  return FALSE;
}
#endif

static void
button_clicked(GtkEventBox *button, GdkEventButton *event, gpointer data)
{
  UIMCandWinHorizontalGtk *horizontal_cwin = data;
  UIMCandWinGtk *cwin = UIM_CAND_WIN_GTK(horizontal_cwin);
  gint i;
  gint idx = -1;
  struct index_button *prev_selected;

  prev_selected = horizontal_cwin->selected;
  if (prev_selected) {
    GtkWidget *label = gtk_bin_get_child(GTK_BIN(prev_selected->button));
    gtk_widget_queue_draw(label);
  }

  for (i = 0; i < (gint)horizontal_cwin->buttons->len; i++) {
    GtkEventBox *p;
    struct index_button *idxbutton;
    idxbutton = g_ptr_array_index(horizontal_cwin->buttons, i);
    if (!idxbutton) {
      continue;
    }
    p = idxbutton->button;
    if (p == button) {
      GtkWidget *label = gtk_bin_get_child(GTK_BIN(button));
      idx = idxbutton->cand_index_in_page;
      gtk_widget_queue_draw(label);
      horizontal_cwin->selected = idxbutton;
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
      /* GtkEventBox is destroyed by container */
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
  GObject *obj = g_object_new(UIM_TYPE_CAND_WIN_HORIZONTAL_GTK,
			      "type", GTK_WINDOW_POPUP,
			      NULL);

  return UIM_CAND_WIN_HORIZONTAL_GTK(obj);
}

static GtkEventBox*
assign_cellbutton(UIMCandWinHorizontalGtk *horizontal_cwin,
		  gint cand_index, gint display_limit)
{
  struct index_button *idxbutton;
  int len;
  GPtrArray *buttons;

  buttons = horizontal_cwin->buttons;
  len = buttons->len;

  if (len <= cand_index) {
    GtkWidget *button;
    GtkWidget *label;

    button = gtk_event_box_new();
    gtk_event_box_set_above_child(GTK_EVENT_BOX(button), TRUE);
    label = gtk_label_new("");
    gtk_container_add(GTK_CONTAINER(button), label);
    scale_label(GTK_EVENT_BOX(button), PANGO_SCALE_LARGE);
    g_signal_connect(button, "button-press-event", G_CALLBACK(button_clicked), horizontal_cwin);
#if GTK_CHECK_VERSION(2, 90, 0)
    g_signal_connect_after(label, "draw", G_CALLBACK(label_draw), horizontal_cwin);
#else
    g_signal_connect_after(label, "expose-event", G_CALLBACK(label_exposed), horizontal_cwin);
#endif
#if GTK_CHECK_VERSION(3, 4, 0)
    gtk_widget_set_hexpand(button, TRUE);
    gtk_widget_set_vexpand(button, TRUE);
    gtk_grid_attach(GTK_GRID(UIM_CAND_WIN_GTK(horizontal_cwin)->view), button,
                    cand_index, 0, 1, 1);
#else
    gtk_table_attach_defaults(GTK_TABLE(UIM_CAND_WIN_GTK(horizontal_cwin)->view), button, cand_index, cand_index + 1, 0, 1);
#endif
    idxbutton = g_malloc(sizeof(struct index_button));
    if (idxbutton) {
      idxbutton->button = GTK_EVENT_BOX(button);
      clear_button(idxbutton, cand_index);
      idxbutton->cand_index_in_page = cand_index;
    }
    g_ptr_array_add(horizontal_cwin->buttons, idxbutton);
  } else {
    idxbutton = g_ptr_array_index(buttons, cand_index);
    idxbutton->cand_index_in_page = cand_index;
  }

  return idxbutton->button;
}

void
uim_cand_win_horizontal_gtk_set_index(UIMCandWinHorizontalGtk *horizontal_cwin, gint index)
{
  gint new_page, prev_index;
  UIMCandWinGtk *cwin;

  g_return_if_fail(UIM_IS_CAND_WIN_HORIZONTAL_GTK(horizontal_cwin));
  cwin = UIM_CAND_WIN_GTK(horizontal_cwin);

  prev_index = cwin->candidate_index;
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
    struct index_button *idxbutton, *prev_selected;
    GtkWidget *label;

    if (cwin->display_limit)
      pos = cwin->candidate_index % cwin->display_limit;
    else
      pos = cwin->candidate_index;

    idxbutton = g_ptr_array_index(horizontal_cwin->buttons, pos);
    prev_selected = (gpointer)horizontal_cwin->selected;
    if (prev_selected && prev_index != cwin->candidate_index) {
      label = gtk_bin_get_child(GTK_BIN(prev_selected->button));
      gtk_widget_queue_draw(label);
    }
    label = gtk_bin_get_child(GTK_BIN(idxbutton->button));
    gtk_widget_queue_draw(label);
    horizontal_cwin->selected = idxbutton;

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
	gtk_widget_show(cwin->sub_window.window);
	cwin->sub_window.active = TRUE;
      } else {
        if (cwin->sub_window.window) {
          gtk_widget_hide(cwin->sub_window.window);
          cwin->sub_window.active = FALSE;
	}
      }
      free(annotation);
    }
  } else {
    horizontal_cwin->selected = NULL;
    if (cwin->sub_window.window) {
      gtk_widget_hide(cwin->sub_window.window);
      cwin->sub_window.active = FALSE;
    }
  }

  uim_cand_win_gtk_update_label(cwin);
}

static void
scale_label(GtkEventBox *button, double scale)
{
  GtkWidget *label;
  PangoAttrList *attrs = pango_attr_list_new();
  PangoAttribute *attr = pango_attr_scale_new(scale);

  pango_attr_list_insert(attrs, attr);
  label = gtk_bin_get_child(GTK_BIN(button));
  if (GTK_IS_LABEL(label))
    gtk_label_set_attributes(GTK_LABEL(label), attrs);
  pango_attr_list_unref(attrs);
}

static void
clear_button(struct index_button *idxbutton,
    gint cell_index)
{
  GtkEventBox *button;
  GtkWidget *label;

  idxbutton->cand_index_in_page = -1;
  button = idxbutton->button;

  label = gtk_bin_get_child(GTK_BIN(button));
  gtk_label_set_text(GTK_LABEL(label), "");
  scale_label(button, PANGO_SCALE_LARGE);
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
    GtkEventBox *button = NULL;

    gtk_tree_model_get(model, &ti, COLUMN_HEADING, &heading,
        COLUMN_CANDIDATE, &cand_str, TERMINATOR);
    if (cand_str != NULL) {
      button = assign_cellbutton(horizontal_cwin, cand_index, display_limit);
      if (button != NULL) {
        GtkWidget *label;
        label = gtk_bin_get_child(GTK_BIN(button));
        if (heading && heading[0] != '\0') {
          gchar *text = g_strdup_printf("%s: %s", heading, cand_str);
          gtk_label_set_text(GTK_LABEL(label), text);
          g_free(text);
	} else {
          gtk_label_set_text(GTK_LABEL(label), cand_str);
	}
	scale_label(button, PANGO_SCALE_LARGE);
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
      gtk_widget_destroy(GTK_WIDGET(idxbutton->button));
      g_free(idxbutton);
      g_ptr_array_remove_index(buttons, i);
    }
#if !GTK_CHECK_VERSION(3, 4, 0)
    gtk_table_resize(GTK_TABLE(cwin->view), 1, cand_index);
#endif
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
#if GTK_CHECK_VERSION(3, 4, 0)
  show_table(GTK_GRID(cwin->view), horizontal_cwin->buttons);
#else
  show_table(GTK_TABLE(cwin->view), horizontal_cwin->buttons);
#endif

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
#if GTK_CHECK_VERSION(3, 4, 0)
show_table(GtkGrid *view, GPtrArray *buttons)
#else
show_table(GtkTable *view, GPtrArray *buttons)
#endif
{
  gint col;

  for (col = 0; col < (gint)buttons->len; col++) {
    GtkEventBox *button = NULL;
    struct index_button *idxbutton;

    idxbutton = g_ptr_array_index(buttons, col);
    button = idxbutton->button;

    gtk_widget_show_all(GTK_WIDGET(button));
  }
  gtk_widget_show(GTK_WIDGET(view));
}

#define UIM_ANNOTATION_WIN_WIDTH 280
#define UIM_ANNOTATION_WIN_HEIGHT 140

void
uim_cand_win_horizontal_gtk_create_sub_window(UIMCandWinHorizontalGtk *horizontal_cwin)
{
  GtkWidget *window, *scrwin, *text_view, *frame;
  GdkGeometry hints;
  UIMCandWinGtk *cwin;

  g_return_if_fail(UIM_IS_CAND_WIN_HORIZONTAL_GTK(horizontal_cwin));
  cwin = UIM_CAND_WIN_GTK(horizontal_cwin);

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
                                 GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

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
uim_cand_win_horizontal_gtk_layout_sub_window(UIMCandWinHorizontalGtk *horizontal_cwin)
{
  UIMCandWinGtk *cwin;
#if GTK_CHECK_VERSION(2, 90, 0)
  gint x, y, w, h, x2, y2, w2, h2, x3, y3;
#else
  gint x, y, w, h, d, x2, y2, w2, h2, d2, x3, y3;
#endif
  struct index_button *idxbutton;

  g_return_if_fail(UIM_IS_CAND_WIN_HORIZONTAL_GTK(horizontal_cwin));
  cwin = UIM_CAND_WIN_GTK(horizontal_cwin);

  if (!cwin->sub_window.window)
    return;

#if GTK_CHECK_VERSION(2, 90, 0)
  gdk_window_get_geometry(gtk_widget_get_window(GTK_WIDGET(cwin)),
                          &x, &y, &w, &h);
#else
  gdk_window_get_geometry(gtk_widget_get_window(GTK_WIDGET(cwin)),
                          &x, &y, &w, &h, &d);
#endif
  gdk_window_get_origin(gtk_widget_get_window(GTK_WIDGET(cwin)), &x, &y);

#if GTK_CHECK_VERSION(2, 90, 0)
  gdk_window_get_geometry(gtk_widget_get_window(cwin->sub_window.window),
                          &x2, &y2, &w2, &h2);
#else
  gdk_window_get_geometry(gtk_widget_get_window(cwin->sub_window.window),
                          &x2, &y2, &w2, &h2, &d2);
#endif

  if (horizontal_cwin->selected) {
    GtkWidget *button;
    idxbutton = horizontal_cwin->selected;
    button = GTK_WIDGET(idxbutton->button);
    gdk_window_get_origin(gtk_widget_get_window(button), &x3, &y3);

#if GTK_CHECK_VERSION(2, 18, 0)
    if (!gtk_widget_get_has_window(button)) {
      GtkAllocation allocation;
      gtk_widget_get_allocation(button, &allocation);
      x3 += allocation.x;
    }
#else
    if (GTK_WIDGET_NO_WINDOW(button))
      x3 += button->allocation.x;
#endif
  }
  y = y + h;

  gtk_window_move(GTK_WINDOW(cwin->sub_window.window), x3, y);
}
