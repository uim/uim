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

#ifndef UIM_GTK_UIM_CAND_WIN_GTK_H
#define UIM_GTK_UIM_CAND_WIN_GTK_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define UIM_TYPE_CAND_WIN_GTK		(uim_cand_win_gtk_get_type ())
#define UIM_CAND_WIN_GTK(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), UIM_TYPE_CAND_WIN_GTK, UIMCandWinGtk))
#define UIM_CAND_WIN_GTK_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), UIM_TYPE_CAND_WIN_GTK, UIMCandWinGtkClass))
#define UIM_IS_CAND_WIN_GTK(obj)	(G_TYPE_CHECK_INSTANCE_TYPE ((obj), UIM_TYPE_CAND_WIN_GTK))
#define UIM_IS_CAND_WIN_GTK_CLASS(klass)(G_TYPE_CHECK_CLASS_TYPE ((klass), UIM_TYPE_CAND_WIN_GTK))
#define UIM_CAND_WIN_GTK_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), UIM_TYPE_CAND_WIN_GTK, UIMCandWinGtkClass))

typedef struct _UIMCandWinGtk      UIMCandWinGtk;
typedef struct _UIMCandWinGtkClass UIMCandWinGtkClass;

typedef enum {
  UIM_CAND_WIN_POS_CARET,
  UIM_CAND_WIN_POS_LEFT,
  UIM_CAND_WIN_POS_RIGHT
} UimCandWinPos;

struct _UIMCandWinGtk {
  GtkWindow	 parent;

  GtkWidget     *scrolled_window;
  GtkWidget	*view;
  GtkWidget	*num_label;
  GtkWidget	*prev_page_button;
  GtkWidget	*next_page_button;

  GPtrArray	*stores;

  guint		 nr_candidates;
  guint		 display_limit;
  gint		 candidate_index;
  gint		 page_index;

  UimCandWinPos	 position;

  GdkRectangle	 cursor;
  gboolean block_index_selection;
  gboolean index_changed;

  /* sub window */
  struct sub_window {
    GtkWidget	*window;
    GtkWidget	*scrolled_window;
    GtkWidget	*text_view;
    gboolean     active;
  } sub_window;
};

struct _UIMCandWinGtkClass {
  GtkWindowClass parent_class;

  /* signals */
  void (*index_changed) (UIMCandWinGtkClass *cwin);

  /* member functions */
  void (*set_index)		(UIMCandWinGtk *cwin, gint index);
  void (*set_page)		(UIMCandWinGtk *cwin, gint page);
  void (*create_sub_window)	(UIMCandWinGtk *cwin);
  void (*layout_sub_window)	(UIMCandWinGtk *cwin);
};


GType		uim_cand_win_gtk_register_type		(GTypeModule *module);
GType		uim_cand_win_gtk_get_type		(void);
UIMCandWinGtk  *uim_cand_win_gtk_new			(void);

void		uim_cand_win_gtk_set_candidates		(UIMCandWinGtk *cwin,
							 guint disp_limit,
							 GSList *candidates);
void		uim_cand_win_gtk_set_page_candidates	(UIMCandWinGtk *cwin,
							 guint page,
							 GSList *candidates);
void		uim_cand_win_gtk_set_nr_candidates	(UIMCandWinGtk *cwin,
							 guint nr,
							 guint disp_limit);
void		uim_cand_win_gtk_clear_candidates	(UIMCandWinGtk *cwin);
guint		uim_cand_win_gtk_get_nr_candidates	(UIMCandWinGtk *cwin);
gint		uim_cand_win_gtk_get_index		(UIMCandWinGtk *cwin);
void		uim_cand_win_gtk_set_index		(UIMCandWinGtk *cwin,
							 gint index);

guint		uim_cand_win_gtk_get_nr_pages		(UIMCandWinGtk *cwin);
gint		uim_cand_win_gtk_get_page		(UIMCandWinGtk *cwin);
void		uim_cand_win_gtk_set_page		(UIMCandWinGtk *cwin,
							 gint page);
void		uim_cand_win_gtk_shift_page		(UIMCandWinGtk *cwin,
							 gboolean forward);
guint		uim_cand_win_gtk_query_new_page_by_cand_select
							(UIMCandWinGtk *cwin,
							 gint index);
guint		uim_cand_win_gtk_query_new_page_by_shift_page
							(UIMCandWinGtk *cwin,
							 gboolean forward);

void		uim_cand_win_gtk_set_scrollable		(UIMCandWinGtk *cwin,
							 gboolean scrollable);

void		uim_cand_win_gtk_layout			(UIMCandWinGtk *cwin,
							 gint topwin_x,
							 gint topwin_y,
							 gint topwin_width,
							 gint topwin_height);
void		uim_cand_win_gtk_set_cursor_location	(UIMCandWinGtk *cwin,
							 GdkRectangle *area);
void		uim_cand_win_gtk_get_window_pos_type	(UIMCandWinGtk *cwin);

void		uim_cand_win_gtk_update_label		(UIMCandWinGtk *cwin);

void		uim_cand_win_gtk_create_sub_window(UIMCandWinGtk *cwin);
void		uim_cand_win_gtk_layout_sub_window(UIMCandWinGtk *cwin);

G_END_DECLS

#endif /*UIM_GTK_UIM_CAND_WIN_GTK_H */
