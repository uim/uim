/*
 *  $Id:$
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the name of authors nor the names of its contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 */

#ifndef __WORDLIST_VIEW_H__
#define __WORDLIST_VIEW_H__

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <gtk/gtkscrolledwindow.h>
#include <gtk/gtktreemodel.h>
#include <gtk/gtktreeview.h>

#include "word.h"

#ifdef __cplusplus
extern "C" {
#endif

#define WORDLIST_VIEW_TYPE		(wordlist_view_get_type())
#define WORDLIST_VIEW(obj)		(G_TYPE_CHECK_INSTANCE_CAST((obj), WORDLIST_VIEW_TYPE, WordlistView))
#define WORDLIST_VIEW_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST((klass), WORDLIST_VIEW_TYPE, WordlistViewClass))
#define IS_WORDLIST_VIEW(obj)		(G_TYPE_CHECK_INSTANCE_TYPE((obj), WORDLIST_VIEW_TYPE))
#define IS_WORDLIST_VIEW_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE((klass), WORDLIST_VIEW_TYPE)
#define IS_WORDLIST_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS((obj), WORDLIST_VIEW_TYPE, WordlistViewClass))

typedef struct _WordlistView WordlistView;
typedef struct _WordlistViewClass WordlistViewClass;
typedef struct _WordlistInfo WordlistInfo;

struct _WordlistView {
    GtkScrolledWindow container;

    GtkTreeView  *view;
    GtkTreeModel *model;

    GtkTreeViewColumn *word_type_column;
    GtkTreeViewColumn *phon_column;
    GtkTreeViewColumn *desc_column;
    GtkTreeViewColumn *cclass_code_column;
    GtkTreeViewColumn *freq_column;
    GtkTreeViewColumn *okuri_column;

    GtkTreeSelection *selection;
    /* private */

    /* properties */
    gboolean cclass_code_show	: 1;
    gboolean freq_show		: 1;
    gboolean okuri_show         : 1;
    gboolean editable		: 1;

    GtkSelectionMode selection_mode;
};

struct _WordlistViewClass {
    GtkScrolledWindowClass parent_class;
};

GType      wordlist_view_get_type            (void);
GtkWidget *wordlist_view_new                 (void);
GtkWidget *wordlist_view_new_with_attributes (const gchar *first_property_name,
					      ...);
void   wordlist_view_set_values              (WordlistView *, word *);
GList *wordlist_view_get_data_list           (WordlistView *);
void   wordlist_view_clear                   (WordlistView *);
void   wordlist_view_remove_selected_data    (WordlistView *);

void   wordlist_view_set_visible_cclass_code_column (WordlistView *, gboolean);
void   wordlist_view_set_visible_freq_column        (WordlistView *, gboolean);
void   wordlist_view_set_visible_okuri_column       (WordlistView *, gboolean);
void   wordlist_view_set_editable                   (WordlistView *, gboolean);
void   wordlist_view_set_selection_mode             (WordlistView *,
						     GtkSelectionMode);

gboolean wordlist_view_get_visible_cclass_code_column (WordlistView *);
gboolean wordlist_view_get_visible_freq_column        (WordlistView *);
gboolean wordlist_view_get_visible_okuri_column       (WordlistView *);
gboolean wordlist_view_get_editable                   (WordlistView *);
GList   *wordlist_view_get_selected_data_list         (WordlistView *);
GList   *wordlist_view_get_all_data_list              (WordlistView *);

GtkSelectionMode wordlist_view_get_selection_mode (WordlistView *);

#ifdef __cplusplus
}
#endif
#endif /* __WORDLIST_VIEW_H__ */
