/*
  Copyright (c) 2003-2026 uim Project https://github.com/uim/uim

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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <config.h>

#include "uim-candidates-view.h"

#include <string.h>

#include <uim/uim.h>

/*
 * A candidate window for GTK 4.
 *
 * GTK 4 has no GTK_WINDOW_POPUP so this is a GtkPopover. The IM
 * context parents it to the client widget. The client widget doesn't
 * know this popover and never calls gtk_popover_present() for it in
 * size-allocate, so this view presents itself whenever its contents
 * or the cursor location change.
 */

typedef struct
{
  gchar *heading_label;
  gchar *candidate;
  gchar *annotation;
} Candidate;

static Candidate *
candidate_new(uim_candidate uim_cand)
{
  Candidate *candidate = g_new0(Candidate, 1);
  candidate->heading_label =
    g_strdup(uim_candidate_get_heading_label(uim_cand));
  candidate->candidate = g_strdup(uim_candidate_get_cand_str(uim_cand));
  candidate->annotation = g_strdup(uim_candidate_get_annotation_str(uim_cand));
  return candidate;
}

static void
candidate_free(gpointer data)
{
  Candidate *candidate = data;
  g_free(candidate->heading_label);
  g_free(candidate->candidate);
  g_free(candidate->annotation);
  g_free(candidate);
}

static void
page_free(gpointer data)
{
  /* Not fetched pages are NULL. */
  if (data)
    g_ptr_array_unref(data);
}

struct _UIMCandidatesView
{
  GtkPopover parent;

  GtkWidget *list_box;
  GtkWidget *prev_page_button;
  GtkWidget *next_page_button;
  GtkWidget *page_label;

  /* Each element is a GPtrArray of Candidate or NULL (not fetched yet). */
  GPtrArray *pages;
  guint n_candidates;
  guint display_limit;
  gint candidate_index; /* -1: no candidate is selected */
  gint page_index;

  UIMCandidatesViewPosition position;
  GdkRectangle cursor_location;

  gboolean block_index_changed;
};

enum {
  INDEX_CHANGED,
  SHIFT_PAGE_REQUESTED,
  N_SIGNALS
};

static guint signals[N_SIGNALS] = {0};

G_DEFINE_DYNAMIC_TYPE(UIMCandidatesView, uim_candidates_view, GTK_TYPE_POPOVER)

static void
uim_candidates_view_update_page_label(UIMCandidatesView *view)
{
  gchar *label;
  if (view->candidate_index >= 0) {
    label =
      g_strdup_printf("%d / %d", view->candidate_index + 1, view->n_candidates);
  } else {
    label = g_strdup_printf("- / %d", view->n_candidates);
  }
  gtk_label_set_text(GTK_LABEL(view->page_label), label);
  g_free(label);
}

static void
uim_candidates_view_present(UIMCandidatesView *view)
{
  GtkWidget *widget = GTK_WIDGET(view);
  GtkWidget *parent = gtk_widget_get_parent(widget);
  GdkRectangle rect = view->cursor_location;

  if (!parent)
    return;

  switch (view->position) {
  case UIM_CANDIDATES_VIEW_POSITION_LEFT:
    rect.x = 0;
    rect.width = 1;
    break;
  case UIM_CANDIDATES_VIEW_POSITION_RIGHT:
    rect.x = MAX(gtk_widget_get_width(parent) - 1, 0);
    rect.width = 1;
    break;
  case UIM_CANDIDATES_VIEW_POSITION_CARET:
  default:
    break;
  }

  gtk_popover_set_pointing_to(GTK_POPOVER(view), &rect);

  /* set_pointing_to() re-presents only when the rectangle changes.
   * Re-present explicitly so that content changes are shown too. */
  if (gtk_widget_get_mapped(widget))
    gtk_popover_present(GTK_POPOVER(view));
}

static GtkWidget *
uim_candidates_view_create_row(UIMCandidatesView *view, Candidate *candidate)
{
  GtkWidget *box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);

  GtkWidget *heading_label = gtk_label_new(candidate->heading_label);
  gtk_label_set_xalign(GTK_LABEL(heading_label), 1.0);
  gtk_label_set_width_chars(GTK_LABEL(heading_label), 2);
  gtk_widget_add_css_class(heading_label, "dim-label");
  gtk_box_append(GTK_BOX(box), heading_label);

  GtkWidget *candidate_label = gtk_label_new(candidate->candidate);
  gtk_label_set_xalign(GTK_LABEL(candidate_label), 0.0);
  gtk_widget_set_hexpand(candidate_label, TRUE);
  gtk_box_append(GTK_BOX(box), candidate_label);

  if (candidate->annotation && candidate->annotation[0] != '\0') {
    GtkWidget *annotation_label = gtk_label_new(candidate->annotation);
    gtk_label_set_xalign(GTK_LABEL(annotation_label), 0.0);
    gtk_widget_add_css_class(annotation_label, "dim-label");
    gtk_box_append(GTK_BOX(box), annotation_label);
  }

  GtkWidget *row = gtk_list_box_row_new();
  gtk_widget_set_focusable(row, FALSE);
  gtk_list_box_row_set_child(GTK_LIST_BOX_ROW(row), box);
  return row;
}

static void
uim_candidates_view_update_selection(UIMCandidatesView *view)
{
  GtkListBox *list_box = GTK_LIST_BOX(view->list_box);
  GtkListBoxRow *row = NULL;

  if (view->candidate_index >= 0 && view->display_limit > 0) {
    gint page = view->candidate_index / view->display_limit;
    if (page == view->page_index) {
      gint row_index = view->candidate_index % view->display_limit;
      row = gtk_list_box_get_row_at_index(list_box, row_index);
    }
  } else if (view->candidate_index >= 0) {
    row = gtk_list_box_get_row_at_index(list_box, view->candidate_index);
  }

  view->block_index_changed = TRUE;
  if (row)
    gtk_list_box_select_row(list_box, row);
  else
    gtk_list_box_unselect_all(list_box);
  view->block_index_changed = FALSE;
}

static void
uim_candidates_view_update_rows(UIMCandidatesView *view)
{
  GtkListBox *list_box = GTK_LIST_BOX(view->list_box);
  GtkWidget *child;

  view->block_index_changed = TRUE;
  while ((child = gtk_widget_get_first_child(view->list_box)))
    gtk_list_box_remove(list_box, child);
  view->block_index_changed = FALSE;

  if (view->page_index < 0 || (guint)view->page_index >= view->pages->len)
    return;

  GPtrArray *candidates = g_ptr_array_index(view->pages, view->page_index);
  if (!candidates)
    return;

  for (guint i = 0; i < candidates->len; i++) {
    Candidate *candidate = g_ptr_array_index(candidates, i);
    gtk_list_box_append(list_box,
                        uim_candidates_view_create_row(view, candidate));
  }
}

static void
uim_candidates_view_update(UIMCandidatesView *view)
{
  uim_candidates_view_update_rows(view);
  uim_candidates_view_update_selection(view);
  uim_candidates_view_update_page_label(view);
  uim_candidates_view_present(view);
}

static void
row_selected_cb(GtkListBox *list_box, GtkListBoxRow *row, gpointer data)
{
  UIMCandidatesView *view = data;

  if (view->block_index_changed)
    return;
  if (!row)
    return;

  gint row_index = gtk_list_box_row_get_index(row);
  if (view->display_limit > 0)
    view->candidate_index = view->page_index * view->display_limit + row_index;
  else
    view->candidate_index = row_index;
  uim_candidates_view_update_page_label(view);
  g_signal_emit(view, signals[INDEX_CHANGED], 0);
}

static void
page_button_clicked_cb(GtkButton *button, gpointer data)
{
  UIMCandidatesView *view = data;
  gboolean forward = button == GTK_BUTTON(view->next_page_button);

  /* Delegate entirely to a "shift-page-requested" listener
   * (uim-im-context.c), which pre-fetches the target page and
   * performs the actual shift + backend index sync + re-show, using
   * exactly the same sequence as the keyboard-driven page-shift path
   * (cand_shift_page_cb). The view itself no longer shifts pages or
   * emits "index-changed" here. */
  g_signal_emit(view, signals[SHIFT_PAGE_REQUESTED], 0, forward);
}

static void
uim_candidates_view_init(UIMCandidatesView *view)
{
  GtkWidget *widget = GTK_WIDGET(view);

  view->pages = g_ptr_array_new_with_free_func(page_free);
  view->n_candidates = 0;
  view->display_limit = 0;
  view->candidate_index = -1;
  view->page_index = 0;
  view->position = UIM_CANDIDATES_VIEW_POSITION_CARET;
  view->cursor_location.x = 0;
  view->cursor_location.y = 0;
  view->cursor_location.width = 0;
  view->cursor_location.height = 0;
  view->block_index_changed = FALSE;

  /* Never take keyboard focus/grab away from the client widget. */
  gtk_popover_set_autohide(GTK_POPOVER(view), FALSE);
  gtk_popover_set_has_arrow(GTK_POPOVER(view), FALSE);
  gtk_popover_set_cascade_popdown(GTK_POPOVER(view), FALSE);
  gtk_popover_set_position(GTK_POPOVER(view), GTK_POS_BOTTOM);
  /* Align the left edge of this view with the pointing rectangle. */
  gtk_widget_set_halign(widget, GTK_ALIGN_START);
  gtk_widget_set_can_focus(widget, FALSE);
  gtk_widget_add_css_class(widget, "uim-candidates");

  GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);

  view->list_box = gtk_list_box_new();
  gtk_list_box_set_selection_mode(GTK_LIST_BOX(view->list_box),
                                  GTK_SELECTION_SINGLE);
  g_signal_connect(view->list_box,
                   "row-selected",
                   G_CALLBACK(row_selected_cb),
                   view);
  gtk_box_append(GTK_BOX(vbox), view->list_box);

  /* [<] i / n [>] */
  GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
  gtk_widget_set_margin_top(hbox, 6);
  view->prev_page_button = gtk_button_new_with_label("<");
  g_signal_connect(view->prev_page_button,
                   "clicked",
                   G_CALLBACK(page_button_clicked_cb),
                   view);
  gtk_box_append(GTK_BOX(hbox), view->prev_page_button);
  view->page_label = gtk_label_new("");
  gtk_widget_set_hexpand(view->page_label, TRUE);
  gtk_widget_set_margin_start(view->page_label, 6);
  gtk_widget_set_margin_end(view->page_label, 6);
  gtk_box_append(GTK_BOX(hbox), view->page_label);
  view->next_page_button = gtk_button_new_with_label(">");
  g_signal_connect(view->next_page_button,
                   "clicked",
                   G_CALLBACK(page_button_clicked_cb),
                   view);
  gtk_box_append(GTK_BOX(hbox), view->next_page_button);
  gtk_box_append(GTK_BOX(vbox), hbox);

  gtk_popover_set_child(GTK_POPOVER(view), vbox);

  uim_candidates_view_update_page_label(view);
}

static void
uim_candidates_view_dispose(GObject *object)
{
  UIMCandidatesView *view = UIM_CANDIDATES_VIEW(object);

  g_clear_pointer(&view->pages, g_ptr_array_unref);

  G_OBJECT_CLASS(uim_candidates_view_parent_class)->dispose(object);
}

static void
uim_candidates_view_class_init(UIMCandidatesViewClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);

  object_class->dispose = uim_candidates_view_dispose;

  /* Emitted when a user selects a candidate or a page by this view. */
  signals[INDEX_CHANGED] = g_signal_new("index-changed",
                                        G_TYPE_FROM_CLASS(klass),
                                        G_SIGNAL_RUN_LAST,
                                        0,
                                        NULL,
                                        NULL,
                                        NULL,
                                        G_TYPE_NONE,
                                        0);

  /* Emitted before a mouse-driven page shift (the "<"/">" buttons)
   * takes effect, so a listener can pre-fetch the target page's
   * candidates first. forward is TRUE for ">", FALSE for "<". */
  signals[SHIFT_PAGE_REQUESTED] = g_signal_new("shift-page-requested",
                                               G_TYPE_FROM_CLASS(klass),
                                               G_SIGNAL_RUN_LAST,
                                               0,
                                               NULL,
                                               NULL,
                                               NULL,
                                               G_TYPE_NONE,
                                               1,
                                               G_TYPE_BOOLEAN);
}

static void
uim_candidates_view_class_finalize(UIMCandidatesViewClass *klass)
{
}

void
uim_candidates_view_load(GTypeModule *module)
{
  uim_candidates_view_register_type(module);
}

UIMCandidatesView *
uim_candidates_view_new(void)
{
  return g_object_new(UIM_TYPE_CANDIDATES_VIEW, NULL);
}

void
uim_candidates_view_set_n_candidates(UIMCandidatesView *view,
                                     guint n_candidates,
                                     guint display_limit)
{
  g_return_if_fail(UIM_IS_CANDIDATES_VIEW(view));

  view->n_candidates = n_candidates;
  view->display_limit = display_limit;
  view->candidate_index = -1;
  view->page_index = 0;

  guint n_pages = 1;
  if (display_limit > 0) {
    n_pages = n_candidates / display_limit;
    if (n_candidates > display_limit * n_pages)
      n_pages++;
    if (n_pages == 0)
      n_pages = 1;
  }
  g_ptr_array_set_size(view->pages, 0);
  for (guint i = 0; i < n_pages; i++)
    g_ptr_array_add(view->pages, NULL);

  gboolean have_multiple_pages =
    display_limit > 0 && n_candidates > display_limit;
  gtk_widget_set_sensitive(view->prev_page_button, have_multiple_pages);
  gtk_widget_set_sensitive(view->next_page_button, have_multiple_pages);

  uim_candidates_view_update(view);
}

guint
uim_candidates_view_get_n_candidates(UIMCandidatesView *view)
{
  g_return_val_if_fail(UIM_IS_CANDIDATES_VIEW(view), 0);
  return view->n_candidates;
}

guint
uim_candidates_view_get_display_limit(UIMCandidatesView *view)
{
  g_return_val_if_fail(UIM_IS_CANDIDATES_VIEW(view), 0);
  return view->display_limit;
}

gboolean
uim_candidates_view_has_page_candidates(UIMCandidatesView *view, guint page)
{
  g_return_val_if_fail(UIM_IS_CANDIDATES_VIEW(view), FALSE);
  if (page >= view->pages->len)
    return FALSE;
  return g_ptr_array_index(view->pages, page) != NULL;
}

void
uim_candidates_view_set_page_candidates(UIMCandidatesView *view,
                                        guint page,
                                        GSList *candidates)
{
  g_return_if_fail(UIM_IS_CANDIDATES_VIEW(view));
  g_return_if_fail(page < view->pages->len);

  GPtrArray *page_candidates = g_ptr_array_new_with_free_func(candidate_free);
  for (GSList *node = candidates; node; node = g_slist_next(node))
    g_ptr_array_add(page_candidates, candidate_new(node->data));

  GPtrArray *old = g_ptr_array_index(view->pages, page);
  if (old)
    g_ptr_array_unref(old);
  g_ptr_array_index(view->pages, page) = page_candidates;

  if ((gint)page == view->page_index)
    uim_candidates_view_update(view);
}

void
uim_candidates_view_clear_candidates(UIMCandidatesView *view)
{
  g_return_if_fail(UIM_IS_CANDIDATES_VIEW(view));
  uim_candidates_view_set_n_candidates(view, 0, 0);
}

gint
uim_candidates_view_get_index(UIMCandidatesView *view)
{
  g_return_val_if_fail(UIM_IS_CANDIDATES_VIEW(view), -1);
  return view->candidate_index;
}

void
uim_candidates_view_set_index(UIMCandidatesView *view, gint index)
{
  g_return_if_fail(UIM_IS_CANDIDATES_VIEW(view));

  if (index >= (gint)view->n_candidates)
    view->candidate_index = 0;
  else
    view->candidate_index = index;

  gint new_page;
  if (view->candidate_index >= 0 && view->display_limit > 0)
    new_page = view->candidate_index / view->display_limit;
  else
    new_page = view->page_index;

  if (view->page_index != new_page) {
    /* uim_candidates_view_set_page() updates the view. */
    uim_candidates_view_set_page(view, new_page);
  } else {
    uim_candidates_view_update_selection(view);
    uim_candidates_view_update_page_label(view);
    uim_candidates_view_present(view);
  }
}

guint
uim_candidates_view_get_n_pages(UIMCandidatesView *view)
{
  g_return_val_if_fail(UIM_IS_CANDIDATES_VIEW(view), 0);
  return view->pages->len;
}

gint
uim_candidates_view_get_page(UIMCandidatesView *view)
{
  g_return_val_if_fail(UIM_IS_CANDIDATES_VIEW(view), -1);
  return view->page_index;
}

void
uim_candidates_view_set_page(UIMCandidatesView *view, gint page)
{
  g_return_if_fail(UIM_IS_CANDIDATES_VIEW(view));

  gint n_pages = view->pages->len;
  g_return_if_fail(n_pages > 0);

  gint new_page;
  if (page < 0)
    new_page = n_pages - 1;
  else if (page >= n_pages)
    new_page = 0;
  else
    new_page = page;

  view->page_index = new_page;

  gint new_index;
  if (view->display_limit > 0) {
    if (view->candidate_index >= 0) {
      new_index = (new_page * view->display_limit) +
                  (view->candidate_index % view->display_limit);
    } else {
      new_index = -1;
    }
  } else {
    new_index = view->candidate_index;
  }
  if (new_index >= (gint)view->n_candidates)
    new_index = view->n_candidates - 1;
  view->candidate_index = new_index;

  uim_candidates_view_update(view);
}

void
uim_candidates_view_shift_page(UIMCandidatesView *view, gboolean forward)
{
  g_return_if_fail(UIM_IS_CANDIDATES_VIEW(view));

  if (forward)
    uim_candidates_view_set_page(view, view->page_index + 1);
  else
    uim_candidates_view_set_page(view, view->page_index - 1);
}

guint
uim_candidates_view_query_new_page_by_cand_select(UIMCandidatesView *view,
                                                  gint index)
{
  g_return_val_if_fail(UIM_IS_CANDIDATES_VIEW(view), 0);

  if (index >= (gint)view->n_candidates)
    index = 0;

  if (index >= 0 && view->display_limit > 0)
    return index / view->display_limit;
  else
    return view->page_index;
}

guint
uim_candidates_view_query_new_page_by_shift_page(UIMCandidatesView *view,
                                                 gboolean forward)
{
  g_return_val_if_fail(UIM_IS_CANDIDATES_VIEW(view), 0);

  gint n_pages = view->pages->len;
  gint page = forward ? view->page_index + 1 : view->page_index - 1;
  if (page < 0)
    return n_pages - 1;
  else if (page >= n_pages)
    return 0;
  else
    return page;
}

void
uim_candidates_view_set_position(UIMCandidatesView *view,
                                 UIMCandidatesViewPosition position)
{
  g_return_if_fail(UIM_IS_CANDIDATES_VIEW(view));

  view->position = position;
  uim_candidates_view_present(view);
}

void
uim_candidates_view_set_cursor_location(UIMCandidatesView *view,
                                        const GdkRectangle *area)
{
  g_return_if_fail(UIM_IS_CANDIDATES_VIEW(view));
  g_return_if_fail(area);

  /* Clients report the same location on every preedit update.
   * Re-presenting each time can stall the GSK renderer. */
  if (area->x == view->cursor_location.x &&
      area->y == view->cursor_location.y &&
      area->width == view->cursor_location.width &&
      area->height == view->cursor_location.height)
    return;

  view->cursor_location = *area;
  uim_candidates_view_present(view);
}
