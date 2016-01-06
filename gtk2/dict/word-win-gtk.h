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

#ifndef UIM_DICT_ANTHY_WORD_GTK_H
#define UIM_DICT_ANTHY_WORD_GTK_H

#include <gtk/gtk.h>
#include "dict.h"
#include "word.h"

G_BEGIN_DECLS

#define WORD_WINDOW_TYPE		(word_window_get_type())
#define WORD_WINDOW(obj)		(G_TYPE_CHECK_INSTANCE_CAST((obj), WORD_WINDOW_TYPE, WordWindow))
#define WORD_WINDOW_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST((klass), WORD_WINDOW_TYPE, WordWindowClass))
#define IS_WORD_WINDOW(obj)		(G_TYPE_CHECK_INSTANCE_TYPE((obj), WORD_WINDOW_TYPE))
#define IS_WORD_WINDOW_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE((klass), WORD_WINDOW_TYPE)
#define IS_WORD_WIDNOW_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS((obj), WORD_WINDOW_TYPE, WordWindowClass))

typedef struct _WordWindow      WordWindow;
typedef struct _WordWindowClass WordWindowClass;

typedef enum {
  WORD_WINDOW_MODE_ADD,
  WORD_WINDOW_MODE_EDIT
}  WordWindowType;

typedef enum {
  WORD_WINDOW_RESPONSE_NONE,
  WORD_WINDOW_RESPONSE_ADD,
  WORD_WINDOW_RESPONSE_CLEAR
} WordWindowResponseCode;

struct _WordWindow {
  GtkDialog  parent;

  GtkWidget *pane;

  GtkWidget *phon;
  GtkWidget *desc;
  GtkWidget *combobox_pos_broad;

  GtkWidget *cclass_code;
  GtkWidget *freq;

  GtkWidget *continuance;

  GtkWidget *add;
  GtkWidget *clear;

  uim_dict *dict;

  WordWindowType mode;
};

struct _WordWindowClass {
  GtkDialogClass parent_class;

  /*-- signals --*/
  void (*word_added) (WordWindow *window);
};

GType      word_window_get_type            (void);
GtkWidget *word_window_new                 (WordWindowType type,
					    uim_dict *dict);
void       word_window_set_dict            (WordWindow *window);
void       word_window_set_word            (WordWindow *window,
					    uim_word   *w);
void       word_window_clear               (WordWindow *window);
gboolean   word_window_is_continuance_mode (WordWindow *window);

G_END_DECLS

#endif /* UIM_DICT_ANTHY_WORD_GTK_H */
