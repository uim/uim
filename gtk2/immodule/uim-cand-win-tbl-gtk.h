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

#ifndef UIM_GTK_UIM_CAND_WIN_TBL_GTK_H
#define UIM_GTK_UIM_CAND_WIN_TBL_GTK_H

#include "uim-cand-win-gtk.h"

G_BEGIN_DECLS

#define UIM_TYPE_CAND_WIN_TBL_GTK (uim_cand_win_tbl_gtk_get_type ())
#define UIM_CAND_WIN_TBL_GTK(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), UIM_TYPE_CAND_WIN_TBL_GTK, UIMCandWinTblGtk))
#define UIM_CAND_WIN_TBL_GTK_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST((klass), UIM_TYPE_CAND_WIN_TBL_GTK, UIMCandWinTblGtkClass))
#define UIM_IS_CAND_WIN_TBL_GTK(obj) (G_TYPE_CHECK_INSTANCE_TYPE((obj), UIM_TYPE_CAND_WIN_TBL_GTK))
#define UIM_IS_CAND_WIN_TBL_GTK_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), UIM_TYPE_CAND_WIN_TBL_GTK))
#define UIM_CAND_WIN_TBL_GTK_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS((obj), UIM_TYPE_CAND_WIN_TBL_GTK, UIMCandWinTblGtkClass))

typedef struct _UIMCandWinTblGtk UIMCandWinTblGtk;
typedef struct _UIMCandWinTblGtkClass UIMCandWinTblGtkClass;

struct _UIMCandWinTblGtk {
  UIMCandWinGtk parent;
  GPtrArray *buttons;
  gchar *tbl_cell2label;
};

struct _UIMCandWinTblGtkClass {
  UIMCandWinGtkClass parent_class;
};


GType uim_cand_win_tbl_gtk_register_type(GTypeModule *module);
GType uim_cand_win_tbl_gtk_get_type(void);
UIMCandWinTblGtk *uim_cand_win_tbl_gtk_new(void);

void uim_cand_win_tbl_gtk_set_index(UIMCandWinTblGtk *cwin, gint index);
void uim_cand_win_tbl_gtk_set_page(UIMCandWinTblGtk *cwin, gint page);

G_END_DECLS

#endif /* UIM_GTK_UIM_CAND_WIN_TBL_GTK_H */
