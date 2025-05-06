/*
 *  $Id:$
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
 *                2005-2013 uim Project https://github.com/uim/uim
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
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 */

#ifndef UIM_DICT_WORD_LIST_VIEW_H
#define UIM_DICT_WORD_LIST_VIEW_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <gtk/gtk.h>

#include "dict.h"

#ifdef __cplusplus
extern "C" {
#endif

#define WORD_LIST_VIEW_TYPE		(word_list_view_get_type())
#define WORD_LIST_VIEW(obj)		(G_TYPE_CHECK_INSTANCE_CAST((obj), WORD_LIST_VIEW_TYPE, WordListView))
#define WORD_LIST_VIEW_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST((klass), WORD_LIST_VIEW_TYPE, WordListViewClass))
#define IS_WORD_LIST_VIEW(obj)		(G_TYPE_CHECK_INSTANCE_TYPE((obj), WORD_LIST_VIEW_TYPE))
#define IS_WORD_LIST_VIEW_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE((klass), WORD_LIST_VIEW_TYPE)
#define WORD_LIST_VIEW_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS((obj), WORD_LIST_VIEW_TYPE, WordListViewClass))

typedef struct _WordListView WordListView;
typedef struct _WordListViewClass WordListViewClass;
typedef struct _WordListInfo WordListInfo;

struct _WordListView {
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

    uim_dict *dict;

    /* private */

    /* properties */
    gboolean cclass_code_show	: 1;
    gboolean freq_show		: 1;
    gboolean okuri_show         : 1;
    gboolean editable		: 1;

    GtkSelectionMode selection_mode;
};

struct _WordListViewClass {
    GtkScrolledWindowClass parent_class;
};

GType      word_list_view_get_type            (void);
GtkWidget *word_list_view_new                 (void);
GtkWidget *word_list_view_new_with_attributes (const gchar *first_property_name,
					       ...);
void   word_list_view_set_dict                         (WordListView *view,
							uim_dict     *dict);
void   word_list_view_set_values                       (WordListView *view,
							uim_word     *word_list);
GList *word_list_view_get_data_list                    (WordListView *view);
void   word_list_view_clear                            (WordListView *view);
void   word_list_view_refresh                          (WordListView *view);
void   word_list_view_remove_selected_data             (WordListView *view);

void   word_list_view_set_visible_cclass_code_column   (WordListView *view,
							gboolean      visible);
void   word_list_view_set_visible_freq_column          (WordListView *view,
							gboolean      visible);
void   word_list_view_set_visible_okuri_column         (WordListView *view,
							gboolean      visible);
void   word_list_view_set_editable                     (WordListView *view,
							gboolean      visible);
void   word_list_view_set_selection_mode               (WordListView *view,
							GtkSelectionMode selection_mode);

gboolean word_list_view_get_visible_cclass_code_column (WordListView *view);
gboolean word_list_view_get_visible_freq_column        (WordListView *view);
gboolean word_list_view_get_visible_okuri_column       (WordListView *view);
gboolean word_list_view_get_editable                   (WordListView *view);
GList   *word_list_view_get_selected_data_list         (WordListView *view);
GList   *word_list_view_get_all_data_list              (WordListView *view);

GtkSelectionMode word_list_view_get_selection_mode (WordListView *);

#ifdef __cplusplus
}
#endif
#endif /* UIM_DICT_WORD_LIST_VIEW_H */
