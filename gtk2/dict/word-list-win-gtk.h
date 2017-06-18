/*

  Copyright (c) 2004-2013 uim Project https://github.com/uim/uim

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

#ifndef UIM_DICT_ANTHY_WORD_LIST_GTK_H
#define UIM_DICT_ANTHY_WORD_LIST_GTK_H

#include <gtk/gtk.h>

void dict_window_destroy_cb(GtkWidget *widget, gpointer data);

G_BEGIN_DECLS

#define WORD_LIST_WINDOW_TYPE		(word_list_window_get_type())
#define WORD_LIST_WINDOW(obj)		(G_TYPE_CHECK_INSTANCE_CAST((obj), WORD_LIST_WINDOW_TYPE, WordListWindow))
#define WORD_LIST_WINDOW_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST((klass), WORD_LIST_WINDOW_TYPE, WordListWindowClass))
#define IS_WORD_LIST_WINDOW(obj)	(G_TYPE_CHECK_INSTANCE_TYPE((obj), WORD_LIST_WINDOW_TYPE))
#define IS_WORD_LIST_WINDOW_CLASS(klass)(G_TYPE_CHECK_CLASS_TYPE((klass), WORD_LIST_WINDOW_TYPE)
#define WORD_LIST_WINDOW_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS((obj), WORD_LIST_WINDOW_TYPE, WordListWindowClass))

typedef struct _WordListWindow      WordListWindow;
typedef struct _WordListWindowClass WordListWindowClass;

struct _WordListWindow {
  GtkWindow       parent;
  GtkWidget      *word_list;
  GtkWidget      *statusbar;
  GtkActionGroup *action_group;
  GtkUIManager   *ui_manager;
  DictEnumDictionaryType dictionary_type;
};

struct _WordListWindowClass {
  GtkWindowClass parent_class;
};

GType      word_list_window_get_type (void);
GtkWidget *word_list_window_new      (int type);

G_END_DECLS

enum {
  PROP_0,
  PROP_DICTIONARY_TYPE,
  PROP_LAST
};

GType dict_enum_dictionary_type_get_type (void);
#define DICT_TYPE_ENUM_DICTIONARY_TYPE dict_enum_dictionary_type_get_type ()

DictEnumDictionaryType dict_get_dictionary_type(GtkWidget *window);

#endif /* UIM_DICT_ANTHY_WORD_LIST_GTK_H */
