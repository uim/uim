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

#include <gtk/gtk.h>

#include <stdio.h>
#include <stdlib.h>

#include "anthyimportwordpane.h"

#include "charset.h"
#include "word.h"
#include "anthy.h"
#include "cannadic.h"
#include "uim/config.h"
#include "uim/gettext.h"


static gchar *dictionary_type[] = {
	"DixChange",
	"cannadic",
/*	"ATOK",
	"MS-IME", */
};

/* dummy functions */
static void dixchange_import (const char* filename, int type) { g_print("dixchange_import called\n"); }
static void dixchange_export (const char* filename, int type) { g_print("dixchange_export called\n"); }

static void cb_browse_button_clicked(GtkButton *button, GtkEntry *entry) {
    GtkWidget *filebrowser;
    gint ret;
    const gchar *filename;

    filebrowser = gtk_file_selection_new(_("Select File"));
    gtk_window_set_position(GTK_WINDOW(filebrowser), GTK_WIN_POS_CENTER);
    ret = gtk_dialog_run(GTK_DIALOG(filebrowser));

    switch(ret) {
    case GTK_RESPONSE_OK:
	filename = gtk_file_selection_get_filename(GTK_FILE_SELECTION(filebrowser));
	gtk_entry_set_text(entry, filename);
	gtk_widget_destroy(filebrowser);
	break;
    case GTK_RESPONSE_CANCEL:
	gtk_widget_destroy(filebrowser);
	break;
    }
}

static void cb_export_button_clicked(GtkButton *button, AnthyImportWordPane *pane)
{
    GtkWidget *dialog;
    const gchar *filename;
    gint type, ret;
    int (*export_commands[])(const char*, int) = {
	dixchange_export,
	cannadic_export,
/*		atok_export,
		msime_export, */
    };

    filename = gtk_entry_get_text(GTK_ENTRY(pane->exportfilename));
    type = gtk_combo_box_get_active(GTK_COMBO_BOX(pane->combo_box_dic_export_type));

    if(filename != NULL && filename[0] != '\0')
	ret = export_commands[type](filename, 0);

    if(ret == -1) {
	dialog = gtk_message_dialog_new(NULL,
					GTK_DIALOG_MODAL,
					GTK_MESSAGE_ERROR,
					GTK_BUTTONS_CLOSE,
					_("Exportion failed."));
	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
	gtk_dialog_run(GTK_DIALOG(dialog));
	gtk_widget_destroy(dialog);
    }
}

static void cb_import_button_clicked(GtkButton *button, AnthyImportWordPane *pane)
{
    const gchar *filename;
    gint type, ret;
    int (*import_commands[])(const char*, int) = {
	dixchange_import,
	cannadic_import,
/*		atok_import,
		msime_import, */
    };

    filename = gtk_entry_get_text(GTK_ENTRY(pane->importfilename));
    type = gtk_combo_box_get_active(GTK_COMBO_BOX(pane->combo_box_dic_import_type));
    if(filename != NULL && filename[0] != '\0')
	ret = import_commands[type](filename, 0);
}

int create_anthy_importwordpane(AnthyImportWordPane *pane) {
    GtkWidget *vbox1, *vbox2;
    GtkWidget *hbox1, *hbox2;
    GtkWidget *hbox3, *hbox4;
    GtkWidget *label1, *label2;
    GtkWidget *frame1, *frame2;
/*	GtkWidget *menu, *menuitem; */
    gint i, dictionary_type_num;

    dictionary_type_num = sizeof(dictionary_type) / sizeof(dictionary_type[0]);

    pane->pane = gtk_hbox_new(FALSE, 5);
    gtk_container_set_border_width(GTK_CONTAINER(pane->pane), 15);

    vbox1 = gtk_vbox_new(FALSE, 0);
    gtk_widget_show(vbox1);
    gtk_box_pack_start(GTK_BOX(pane->pane), vbox1, FALSE, FALSE, 0);

    /* Import */
    frame1 = gtk_frame_new(_("Import"));
    gtk_widget_show(frame1);
    gtk_box_pack_start(GTK_BOX(vbox1), frame1, FALSE, FALSE, 0);

    hbox2 = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(hbox2);
    gtk_container_set_border_width(GTK_CONTAINER(hbox2), 10);
    gtk_container_add(GTK_CONTAINER(frame1), hbox2);

    label1 = gtk_label_new_with_mnemonic(_("Filename:"));
    gtk_widget_show(label1);
    gtk_box_pack_start(GTK_BOX(hbox2), label1, FALSE, FALSE, 0);

    pane->importfilename = gtk_entry_new();
    gtk_widget_show(pane->importfilename);
    gtk_box_pack_start(GTK_BOX(hbox2), pane->importfilename, FALSE, FALSE, 5);

    pane->button_import_getfilename = gtk_button_new_with_label(_("Browse..."));
    gtk_widget_show(pane->button_import_getfilename);
    gtk_box_pack_start(GTK_BOX(hbox2), pane->button_import_getfilename,
		       FALSE, FALSE, 10);
    g_signal_connect(G_OBJECT(pane->button_import_getfilename), "clicked",
		     G_CALLBACK(cb_browse_button_clicked),
		     GTK_ENTRY(pane->importfilename));

    pane->combo_box_dic_import_type = gtk_combo_box_new_text();
    gtk_widget_show(pane->combo_box_dic_import_type);
    for(i = 0; i < dictionary_type_num; i++) {
	gtk_combo_box_append_text(GTK_COMBO_BOX(pane->combo_box_dic_import_type),
				  dictionary_type[i]);
    }
    gtk_box_pack_start(GTK_BOX(hbox2), pane->combo_box_dic_import_type,
		       FALSE, FALSE, 10);

    pane->button_doimport = gtk_button_new_with_label(_("Import"));
    gtk_box_pack_start(GTK_BOX(hbox2), pane->button_doimport,
		       FALSE, FALSE, 20);
    gtk_widget_show(pane->button_doimport);
    g_signal_connect(G_OBJECT(pane->button_doimport), "clicked",
		     G_CALLBACK(cb_import_button_clicked), pane);


    /* Export */
    frame2 = gtk_frame_new(_("Export"));
    gtk_widget_show(frame2);
    gtk_box_pack_start(GTK_BOX(vbox1), frame2, FALSE, FALSE, 10);

    hbox3 = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(hbox3);
    gtk_container_set_border_width(GTK_CONTAINER(hbox3), 10);
    gtk_container_add(GTK_CONTAINER(frame2), hbox3);

    label2 = gtk_label_new_with_mnemonic(_("Filename:"));
    gtk_widget_show(label2);
    gtk_box_pack_start(GTK_BOX(hbox3), label2, FALSE, FALSE, 0);

    pane->exportfilename = gtk_entry_new();
    gtk_widget_show(pane->exportfilename);
    gtk_box_pack_start(GTK_BOX(hbox3), pane->exportfilename, FALSE, FALSE, 5);

    pane->button_export_getfilename = gtk_button_new_with_label(_("Browse..."));
    gtk_widget_show(pane->button_export_getfilename);
    gtk_box_pack_start(GTK_BOX(hbox3), pane->button_export_getfilename,
		       FALSE, FALSE, 10);
    g_signal_connect(G_OBJECT(pane->button_export_getfilename), "clicked",
		     G_CALLBACK(cb_browse_button_clicked),
		     GTK_ENTRY(pane->exportfilename));
    pane->combo_box_dic_export_type = gtk_combo_box_new_text();
    gtk_widget_show(pane->combo_box_dic_export_type);
    for(i = 0; i < dictionary_type_num; i++) {
	gtk_combo_box_append_text(GTK_COMBO_BOX(pane->combo_box_dic_export_type),
				  dictionary_type[i]);

    }
    gtk_box_pack_start(GTK_BOX(hbox3), pane->combo_box_dic_export_type,
		       FALSE, FALSE, 10);
    pane->button_doexport = gtk_button_new_with_label(_("Export"));
    gtk_box_pack_start(GTK_BOX(hbox3), pane->button_doexport,
		       FALSE, FALSE, 20);
    gtk_widget_show(pane->button_doexport);

    g_signal_connect(G_OBJECT(pane->button_doexport), "clicked",
		     G_CALLBACK(cb_export_button_clicked), pane);
    return 0;
}

int show_anthy_importwordpane(AnthyImportWordPane *pane) {
    gtk_widget_show(pane->pane);
    return 0;
}
