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

#ifndef __WORDAPPEND_PANE_H__
#define __WORDAPPEND_PANE_H__

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <gtk/gtkhbox.h>
#include <gtk/gtkdialog.h>
#include <gtk/gtkentry.h>
#include <gtk/gtkspinbutton.h>
#include <gtk/gtktreeview.h>
#include <gtk/gtktreemodel.h>
#include <gtk/gtkcombobox.h>
#include <gtk/gtkcheckbutton.h>
#include <gtk/gtkframe.h>

#include "word.h"

#ifdef __cplusplus
extern "C" {
#endif

#define ADDWORD_PANE_TYPE		(addword_pane_get_type())
#define ADDWORD_PANE(obj)		(G_TYPE_CHECK_INSTANCE_CAST((obj), ADDWORD_PANE_TYPE, AddWordPane))
#define ADDWORD_PANE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST((klass), ADDWORD_PANE_TYPE, AddWordPaneClass))
#define IS_ADDWORD_PANE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE((obj), ADDWORD_PANE_TYPE))
#define IS_ADDWORD_PANE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE((klass), ADDWORD_PANE_TYPE)
#define IS_ADDWORD_PANE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS((obj), ADDWORD_PANE_TYPE, AddWordPaneClass))

typedef struct _AddWordPane AddWordPane;
typedef struct _AddWordPaneClass AddWordPaneClass;
typedef struct _AddWordPaneInfo AddWordPaneInfo;

typedef enum {
    ADDWORDPANE_MODE_NOTHING,
    ADDWORDPANE_MODE_ANTHY,
    ADDWORDPANE_MODE_CANNA,
/*    ADDWORDPANE_MODE_PRIME, */
/*    ADDWORDPANE_MODE_ANTHYCANNA, */
/*    ADDWORDPANE_MODE_ANTHYPRIME, */
/*    ADDWORDPANE_MODE_CANNAPRIME, */
    ADDWORDPANE_MODE_ALL,
} addwordpane_mode;

struct _AddWordPane {
    GtkHBox container;

    GtkEntry *phonetic;
    GtkEntry *literal;
    GtkComboBox *combo_pos;
    GtkEntry *pos_narrow;
    GtkButton *pos_browse;
/*    GtkEntry *frequency; */
    GtkSpinButton *frequency;

    GtkFrame *frame_checkbutton;
    GtkCheckButton *check_anthy;
    GtkCheckButton *check_canna;
    GtkCheckButton *check_prime;

    GtkButton *add;
    GtkButton *clear;

    /* private */

    /* properties */
    addwordpane_mode mode;
};

struct _AddWordPaneClass {
    GtkHBoxClass parent_class;
};

GtkWidget *addword_pane_new	 (void);
void	  addword_pane_set_mode (AddWordPane *pane, addwordpane_mode mode);
addwordpane_mode addword_pane_get_mode (AddWordPane *pane);
#ifdef __cplusplus
}
#endif
#endif /* __WORDAPPEND_PANE_H__ */
