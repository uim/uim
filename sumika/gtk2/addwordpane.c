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

#include "addwordpane.h"
/* #include "prime.h" */
#include "anthy.h"
#include "cannadic.h"
#include "uim/config.h"
#include "uim/gettext.h"
#include "charset.h"
#include "posdialog.h"

#include <gtk/gtk.h>

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static void addword_pane_class_init (AddWordPaneClass * klass);
static void addword_pane_init (AddWordPane * pane);
static void addword_pane_finalize (GObject * object);
static void addword_pane_destroy (GtkObject * object);

static void addword_pane_set_property (GObject * object,
				       guint prop_id,
				       const GValue * value,
				       GParamSpec * pspec);
static void addword_pane_get_property (GObject * object,
				       guint prop_id,
				       GValue * value, GParamSpec * pspec);

enum {
    PROP_0,
    PROP_MODE,
};

/* callbacks */
static void cb_addwordpane_clearbutton_clicked(GtkButton *, AddWordPane *);
static void cb_addwordpane_addbutton_clicked(GtkButton *, AddWordPane *);
static void cb_addwordpane_posbrowsebutton_clicked(GtkButton *, AddWordPane *);

static GtkHBoxClass *parent_class = NULL;

static gchar *pos_broad[] = {
    N_("Verb"),
    N_("Substantive"),
    N_("Adjective"),
    N_("Adverb"),
    N_("Etc"),
};

GType addword_pane_get_type(void) {
    static GType type = 0;

    if(type == 0) {
	static const GTypeInfo info = {
	    sizeof(AddWordPaneClass),
	    NULL, /* base_init */
	    NULL, /* base_finalize */
	    (GClassInitFunc)addword_pane_class_init,
	    NULL, /* class_finalize */
	    NULL, /* class_data */
	    sizeof(AddWordPane),
	    0, /* n_preallocs */
	    (GInstanceInitFunc)addword_pane_init /* instance_init */
	};
	type = g_type_register_static(GTK_TYPE_HBOX, "AddWordPane", &info, 0);
    }
    return type;
}

static void addword_pane_class_init(AddWordPaneClass *klass) {
    GObjectClass   *gobject_class = G_OBJECT_CLASS(klass);
    GtkObjectClass *object_class  = GTK_OBJECT_CLASS(klass);

    parent_class = g_type_class_peek_parent(klass);

    gobject_class->get_property = addword_pane_get_property;
    gobject_class->set_property = addword_pane_set_property;
    gobject_class->finalize = addword_pane_finalize;
    object_class->destroy = addword_pane_destroy;

    g_object_class_install_property(gobject_class,
				    PROP_MODE,
				    g_param_spec_int("mode",
						     _("to be written"),
						     _("to be written"),
						     ADDWORDPANE_MODE_NOTHING,
						     ADDWORDPANE_MODE_ALL,
						     ADDWORDPANE_MODE_ALL,
						     G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static void addword_pane_init(AddWordPane *pane) {
    GtkWidget *widget;
    GtkWidget *vbox1, *vbox2;
    GtkWidget *hbox1;
    GtkWidget *table1;
    GtkWidget *alignment_pos;
    GtkWidget *label_phonetic, *label_literal;
    GtkWidget *label_pos_broad, *label_pos_narrow;
    GtkWidget *label_freq;
    GtkWidget *frame_checkbox;
    GtkAdjustment *adjustment_freq;

    gint i, num;

    GTK_WIDGET_SET_FLAGS(pane, GTK_CAN_FOCUS | GTK_RECEIVES_DEFAULT);
    gtk_container_set_border_width(GTK_CONTAINER(&pane->container), 15);

    vbox1 = gtk_vbox_new(FALSE, 0);
    gtk_widget_show(vbox1);
    gtk_box_pack_start(GTK_BOX(&pane->container), vbox1, FALSE, FALSE, 0);

    table1 = gtk_table_new(6, 5, FALSE);
    gtk_widget_show(table1);
    gtk_box_pack_start(GTK_BOX(vbox1), table1, FALSE, FALSE, 0);

    label_phonetic = gtk_label_new(_("Phonetic:"));
    gtk_misc_set_alignment(GTK_MISC(label_phonetic), 1.0, 0.5);
    gtk_widget_show(label_phonetic);
    gtk_table_attach(GTK_TABLE(table1), label_phonetic,
		     0, 1, 0, 1, GTK_FILL, 0, 5, 5);

    widget = gtk_entry_new();
    gtk_table_attach(GTK_TABLE(table1), widget,
		     1, 2, 0, 1, 0, 0, 5, 5);
    gtk_widget_show(widget);
    pane->phonetic = GTK_ENTRY(widget);

    label_literal = gtk_label_new(_("Literal:"));
    gtk_misc_set_alignment(GTK_MISC(label_literal), 1.0, 0.5);
    gtk_widget_show(label_literal);
    gtk_table_attach(GTK_TABLE(table1), label_literal,
		     0, 1, 1, 2, GTK_FILL, 0, 5, 5);

    widget = gtk_entry_new();
    gtk_table_attach(GTK_TABLE(table1), widget,
		     1, 2, 1, 2, 0, 0, 5, 5);
    gtk_widget_show(widget);
    pane->literal = GTK_ENTRY(widget);

    label_pos_broad = gtk_label_new(_("Part of Speech(broad):"));
    gtk_misc_set_alignment(GTK_MISC(label_pos_broad), 1.0, 0.5);
    gtk_widget_show(label_pos_broad);
    gtk_table_attach(GTK_TABLE(table1), label_pos_broad,
		     0, 1, 2, 3, GTK_FILL, 0, 5, 5);
    {
	gint pos_num;

	pos_num = sizeof(pos_broad) / sizeof(pos_broad[0]);


	widget = gtk_combo_box_new_text();
	gtk_widget_show(widget);

	for(i = 0; i < pos_num; i++) {
	    gtk_combo_box_append_text(GTK_COMBO_BOX(widget),
				      pos_broad[i]);
	}

	alignment_pos = gtk_alignment_new(0, 0.5, 0, 0);
	gtk_container_add(GTK_CONTAINER(alignment_pos),
			  widget);
	gtk_widget_show(alignment_pos);
	gtk_table_attach(GTK_TABLE(table1), alignment_pos,
			 1, 2, 2, 3, GTK_FILL, GTK_FILL, 5, 5);
	gtk_combo_box_set_active(GTK_COMBO_BOX(widget), 0);

	pane->combo_pos = GTK_COMBO_BOX(widget);
    }

    label_pos_narrow = gtk_label_new(_("Part of Speech(narrow):"));
    gtk_misc_set_alignment(GTK_MISC(label_pos_narrow), 1.0, 0.5);
    gtk_widget_show(label_pos_narrow);
    gtk_table_attach(GTK_TABLE(table1), label_pos_narrow,
		     0, 1, 3, 4, GTK_FILL, 0, 5, 5);

    widget = gtk_entry_new();
    gtk_widget_show(widget);
    gtk_table_attach(GTK_TABLE(table1), widget,
		     1, 2, 3, 4, GTK_FILL, 0, 5, 5);
    pane->pos_narrow = GTK_ENTRY(widget);

    widget = gtk_button_new_with_label(_("Browse..."));
    gtk_widget_show(widget);
    gtk_table_attach(GTK_TABLE(table1), widget,
		     2, 3, 3, 4, GTK_FILL, 0, 5, 5);
    g_signal_connect(G_OBJECT(widget), "clicked",
		     G_CALLBACK(cb_addwordpane_posbrowsebutton_clicked), pane);
    pane->pos_browse = GTK_BUTTON(widget);

    label_freq = gtk_label_new(_("Frequency:"));
    gtk_misc_set_alignment(GTK_MISC(label_freq), 1.0, 0.5);
    gtk_widget_show(label_freq);
    gtk_table_attach(GTK_TABLE(table1), label_freq,
		     0, 1, 4, 5, GTK_FILL, 0, 5, 5);
    /* 1-65535 */
    adjustment_freq = (GtkAdjustment*)gtk_adjustment_new(1.0, 1.0,
							 65535.0, 1.0,
							 100.0, 100.0);
    widget = gtk_spin_button_new(adjustment_freq, 1.0, 0);
    gtk_widget_show(widget);
    gtk_table_attach(GTK_TABLE(table1), widget,
		     1, 2, 4, 5, GTK_FILL, 0, 5, 5);
    pane->frequency = GTK_SPIN_BUTTON(widget);

    widget = gtk_frame_new(_("System:"));
    gtk_widget_show(widget);
    gtk_table_attach(GTK_TABLE(table1), widget,
		     1, 5, 5, 6, GTK_FILL, 0, 5, 5);
    pane->frame_checkbutton = GTK_FRAME(widget);

    hbox1 = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(hbox1);
    gtk_container_add(GTK_CONTAINER(pane->frame_checkbutton), hbox1);

    widget = gtk_check_button_new_with_label("Anthy");
    gtk_widget_show(widget);
    gtk_box_pack_start(GTK_BOX(hbox1), widget, FALSE, FALSE, 5);
    pane->check_anthy = GTK_CHECK_BUTTON(widget);

    widget = gtk_check_button_new_with_label("PRIME");
    gtk_widget_show(widget);
    gtk_widget_set_sensitive(GTK_WIDGET(widget), FALSE);
    gtk_box_pack_start(GTK_BOX(hbox1), widget, FALSE, FALSE, 5);
    pane->check_prime = GTK_CHECK_BUTTON(widget);


    vbox2 = gtk_vbutton_box_new();
    gtk_button_box_set_layout(GTK_BUTTON_BOX(vbox2), GTK_BUTTONBOX_START);
    gtk_box_set_spacing(GTK_BOX(vbox2), 10);
    gtk_widget_show(vbox2);
    gtk_box_pack_start(GTK_BOX(&pane->container), vbox2, FALSE, FALSE, 50);

    widget = gtk_button_new_with_label(_("Add"));
    gtk_box_pack_start(GTK_BOX(vbox2), widget, FALSE, FALSE, 0);
    g_signal_connect(G_OBJECT(widget), "clicked",
		     G_CALLBACK(cb_addwordpane_addbutton_clicked), pane);
    gtk_widget_show(widget);
    pane->add = GTK_BUTTON(widget);

    widget = gtk_button_new_with_label(_("Clear"));
    gtk_box_pack_start(GTK_BOX(vbox2), widget, FALSE, FALSE, 0);
    g_signal_connect(G_OBJECT(widget), "clicked",
		     G_CALLBACK(cb_addwordpane_clearbutton_clicked), pane);
    gtk_widget_show(widget);
    pane->clear = GTK_BUTTON(widget);
}

static void addword_pane_finalize(GObject *object) {
    if(G_OBJECT_CLASS(parent_class)->finalize) {
	G_OBJECT_CLASS(parent_class)->finalize(object);
    }
}

static void addword_pane_destroy(GtkObject *object) {
    if(GTK_OBJECT_CLASS(parent_class)->destroy) {
	GTK_OBJECT_CLASS(parent_class)->destroy(object);
    }
}

GtkWidget *addword_pane_new(void) {
    return GTK_WIDGET(g_object_new(ADDWORD_PANE_TYPE, NULL));
}

static void cb_addwordpane_clearbutton_clicked(GtkButton *button, AddWordPane *pane)
{
    gtk_combo_box_set_active(GTK_COMBO_BOX(pane->combo_pos), 0);
    gtk_entry_set_text(GTK_ENTRY(pane->phonetic), "");
    gtk_entry_set_text(GTK_ENTRY(pane->literal), "");
    gtk_entry_set_text(GTK_ENTRY(pane->pos_narrow), "");
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(pane->frequency), 1);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_anthy), FALSE);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_prime), FALSE);
}

static void cb_addwordpane_addbutton_clicked(GtkButton *button, AddWordPane *pane)
{
    const gchar *phonetic, *literal, *pos;
    gchar *defineword;
    char *euc_phonetic, *euc_literal;
    unsigned char *euc_defineword;
    gint freq;
    gint ret = -1, ret2 = -1, len = 0;

    phonetic = gtk_entry_get_text(pane->phonetic);
    literal  = gtk_entry_get_text(pane->literal);
    pos      = gtk_entry_get_text(pane->pos_narrow);
    freq     = gtk_spin_button_get_value(pane->frequency);

    euc_phonetic = charset_convert(phonetic, "UTF-8", "EUC-JP");
    euc_literal  = charset_convert(literal, "UTF-8", "EUC-JP");

    if(euc_phonetic != NULL && euc_literal != NULL) {
	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane->check_anthy)) == TRUE) {
	    ret = add_anthy_priv_dic_with_flags(euc_phonetic, euc_literal,
						pos, freq);
	}
	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane->check_prime)) == TRUE) {
	    ; /* do nothing */
	}
    }
/*
    if(phonetic != NULL)
	g_free(phonetic);
    if(literal != NULL)
	g_free(literal);
    if(pos != NULL)
	g_free(pos);
*/
    if(euc_phonetic != NULL)
	g_free(euc_phonetic);
    if(euc_literal != NULL)
	g_free(euc_literal);

    cb_addwordpane_clearbutton_clicked(NULL, pane);
}

static void cb_addwordpane_posbrowsebutton_clicked(GtkButton *button, AddWordPane *pane)
{
    gchar *code = NULL;
    gint system, id;

    id = gtk_combo_box_get_active(GTK_COMBO_BOX(pane->combo_pos));
    system = 0;
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane->check_anthy)) == TRUE)
	system |= SUPPORT_ANTHY;

    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pane->check_prime)) == TRUE) {
	; /* do nothing */
    }

    if(system != 0)
	code = cannadic_pos_dialog(id, system);

    if(code != NULL) {
	gtk_entry_set_text(pane->pos_narrow, code);
	free(code);
    }
}

void addword_pane_set_mode(AddWordPane *pane, addwordpane_mode mode) {
    g_return_if_fail(IS_ADDWORD_PANE(pane));

    switch(mode) {
    case ADDWORDPANE_MODE_ANTHY:
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_anthy), TRUE);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_anthy), TRUE);
/*	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_prime), FALSE); */
/*	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_prime, FALSE)); */
	gtk_widget_hide(GTK_WIDGET(pane->frame_checkbutton));
	break;
/*    case ADDWORDPANE_MODE_PRIME:
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_anthy), FALSE);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_anthy), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_canna), FALSE);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_canna), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_prime), TRUE);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_prime), TRUE);
	gtk_widget_hide(GTK_WIDGET(pane->frame_checkbutton));
	break;
    case ADDWORDPANE_MODE_ANTHYCANNA:
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_anthy), TRUE);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_anthy), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_canna), TRUE);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_canna), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_prime), FALSE);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_prime), FALSE);
	gtk_widget_show(GTK_WIDGET(pane->frame_checkbutton));
	break;
    case ADDWORDPANE_MODE_ANTHYPRIME:
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_anthy), TRUE);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_anthy), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_canna), FALSE);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_canna), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_prime), TRUE);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pane->check_prime), FALSE);
	gtk_widget_show(GTK_WIDGET(pane->frame_checkbutton));
	break;
    case ADDWORDPANE_MODE_CANNAPRIME:
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_anthy), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_canna), TRUE);
	gtk_widget_set_sensitive(GTK_WIDGET(pane->check_prime), TRUE);
	gtk_widget_show(GTK_WIDGET(pane->frame_checkbutton));
	break;
*/
    default:
	break;
    }
    pane->mode = mode;
}

addwordpane_mode addword_pane_get_mode(AddWordPane *pane) {
    g_return_val_if_fail(IS_ADDWORD_PANE(pane), 0);

    return pane->mode;
}

static void addword_pane_set_property (GObject * object,
				       guint prop_id,
				       const GValue * value,
				       GParamSpec * pspec)
{
    AddWordPane *pane = ADDWORD_PANE(object);

    switch(prop_id) {
    case PROP_MODE:
	addword_pane_set_mode(pane, g_value_get_int(value));
	break;
    default:
	G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
	break;
    }
}

static void addword_pane_get_property (GObject * object,
				       guint prop_id,
				       GValue * value, GParamSpec * pspec)
{
    AddWordPane *pane = ADDWORD_PANE(object);

    switch(prop_id) {
    case PROP_MODE:
	g_value_set_int(value, pane->mode);
	break;
    default:
	G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
	break;
    }
}
