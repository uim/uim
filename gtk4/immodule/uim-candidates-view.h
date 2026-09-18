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

#pragma once

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define UIM_TYPE_CANDIDATES_VIEW uim_candidates_view_get_type()
G_DECLARE_FINAL_TYPE(
  UIMCandidatesView, uim_candidates_view, UIM, CANDIDATES_VIEW, GtkPopover)

typedef enum {
  UIM_CANDIDATES_VIEW_POSITION_CARET,
  UIM_CANDIDATES_VIEW_POSITION_LEFT,
  UIM_CANDIDATES_VIEW_POSITION_RIGHT
} UIMCandidatesViewPosition;

void
uim_candidates_view_load(GTypeModule *module);

UIMCandidatesView *
uim_candidates_view_new(void);

/* candidates */
void
uim_candidates_view_set_n_candidates(UIMCandidatesView *view,
                                     guint n_candidates,
                                     guint display_limit);
guint
uim_candidates_view_get_n_candidates(UIMCandidatesView *view);
guint
uim_candidates_view_get_display_limit(UIMCandidatesView *view);
gboolean
uim_candidates_view_has_page_candidates(UIMCandidatesView *view, guint page);
/* candidates: GSList of uim_candidate. Contents are copied. */
void
uim_candidates_view_set_page_candidates(UIMCandidatesView *view,
                                        guint page,
                                        GSList *candidates);
void
uim_candidates_view_clear_candidates(UIMCandidatesView *view);

/* index */
gint
uim_candidates_view_get_index(UIMCandidatesView *view);
void
uim_candidates_view_set_index(UIMCandidatesView *view, gint index);

/* page */
guint
uim_candidates_view_get_n_pages(UIMCandidatesView *view);
gint
uim_candidates_view_get_page(UIMCandidatesView *view);
void
uim_candidates_view_set_page(UIMCandidatesView *view, gint page);
void
uim_candidates_view_shift_page(UIMCandidatesView *view, gboolean forward);
guint
uim_candidates_view_query_new_page_by_cand_select(UIMCandidatesView *view,
                                                  gint index);
guint
uim_candidates_view_query_new_page_by_shift_page(UIMCandidatesView *view,
                                                 gboolean forward);

/* placement */
void
uim_candidates_view_set_position(UIMCandidatesView *view,
                                 UIMCandidatesViewPosition position);
/* area: in the coordinates of the parent widget of this view. */
void
uim_candidates_view_set_cursor_location(UIMCandidatesView *view,
                                        const GdkRectangle *area);

G_END_DECLS
