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

#include "anthy.h"
#include "topwin.h"

#include "uim/gettext.h"

/* prototype */
static int cb_delete_topwindow(GtkWidget *widget, GdkEvent *event, TopWindow *topwin);
static void cb_menu_file(TopWindow *topwin, guint action, GtkWidget *widget);
static void cb_menu_help(TopWindow *topwin, guint action, GtkWidget *widget);

static void cb_menu_option_switch_imsystem(GtkAction *action,
				    GtkRadioAction *current,
				    TopWindow *topwin);

static void clean_topwindow(TopWindow *topwin);

static void file_exit_action_cb (GtkAction *action, TopWindow *topwin);
static void help_about_action_cb(GtkAction *action, TopWindow *topwin);

/* globals */
gboolean support_anthy;
gboolean support_prime;

/* Main menu*/
GtkActionEntry menu_action_entries[] = {
    { "FileMenu",   NULL, "_File"   },
    { "OptionMenu", NULL, "_Option" },
    { "HelpMenu",   NULL, "_Help"   },
    { "Quit", GTK_STOCK_QUIT, "E_xit", "<control>Q", "_Quit Sumika", file_exit_action_cb },
    { "About", NULL, "_About", NULL, "_About Sumika", help_about_action_cb },
};
static guint n_menu_action_entries = G_N_ELEMENTS(menu_action_entries);

/* Radio menu */
GtkRadioActionEntry im_radio_entries[] = {
    { "Anthy", NULL, "Anthy", NULL, "_Anthy", MAIN_MENU_OPTION_ANTHY },
    { "SKK",   NULL, "SKK",   NULL, "_SKK"  , MAIN_MENU_OPTION_SKK   },
/*  { "PRIME", NULL, "PRIME", NULL, "_PRIME", MAIN_MENU_OPTION_PRIME }, */
    { "IntegrationMode", NULL, "_Integration Mode",
      NULL, "_Integration Mode", MAIN_MENU_OPTION_INTEGRATE },
    { "uim",   NULL, "uim",   NULL, "uim",   MAIN_MENU_OPTION_UIM },
};

static guint n_im_radio_entries = G_N_ELEMENTS(im_radio_entries);

static const char ui_info[] =
"<ui>"
"  <menubar name='MainMenu'>"
"    <menu action='FileMenu'>"
"      <menuitem action='Quit' />"
"    </menu>"
"    <menu action='OptionMenu'>"
"      <menuitem action='Anthy' />"
"      <menuitem action='SKK' />"
/* "      <menuitem action='PRIME' />" */
"      <menuitem action='IntegrationMode' />"
"      <menuitem action='uim' />"
"    </menu>"
"    <menu action='HelpMenu'>"
"      <menuitem action='About' />"
"    </menu>"
"  </menubar>"
"</ui>";

static int cb_delete_topwindow(GtkWidget *widget,
			GdkEvent *event,
			TopWindow *topwin)
{
    clean_topwindow(topwin);
    gtk_main_quit();
    return FALSE;
}

static void file_exit_action_cb (GtkAction *action, TopWindow *topwin)
{
    clean_topwindow(topwin);
    gtk_main_quit();
}

void help_about_action_cb(GtkAction *action, TopWindow *topwin) {
    GtkWidget *about_dialog, *label1;
    gchar about_name[] = { "<span size=\"20000\">sumika " VERSION "</span>\n\n"
			   "<span size=\"14000\">Copyright 2003-2004"
			   "Masahito Omote &lt;omote@utyuuzin.net&gt;\n"
			   "All rights reserved.</span>\n"
    };

    about_dialog = gtk_dialog_new_with_buttons(_("About sumika"), NULL,
					       GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
					       GTK_STOCK_OK,
					       GTK_RESPONSE_ACCEPT, NULL);
    gtk_container_set_border_width(GTK_CONTAINER(about_dialog), 8);

    label1 = gtk_label_new(NULL);
    gtk_widget_show(label1);
    gtk_label_set_markup(GTK_LABEL(label1), about_name);
    gtk_box_pack_start(GTK_BOX(GTK_DIALOG(about_dialog)->vbox),
		       label1, FALSE, FALSE, 0);
    
    gtk_window_set_position(GTK_WINDOW(about_dialog), GTK_WIN_POS_CENTER);
    gtk_dialog_run(GTK_DIALOG(about_dialog));
    
    gtk_widget_destroy(about_dialog);
}

void cb_menu_option_switch_imsystem(GtkAction *action,
				    GtkRadioAction *current,
				    TopWindow *topwin)
{
    gint value;

    value = gtk_radio_action_get_current_value(current);
    
    switch(value) {
    case MAIN_MENU_OPTION_ANTHY:
	if(topwin->skkcontainer != NULL)
	    gtk_widget_hide((topwin->skkcontainer)->container);
	if(topwin->anthycontainer != NULL)
	    gtk_widget_show((topwin->anthycontainer)->container);
	if(topwin->addwordpane != NULL)
	    gtk_widget_hide(topwin->addwordpane);
	if(topwin->uimconfigcontainer != NULL)
	    gtk_widget_hide((topwin->uimconfigcontainer)->container);
	break;
    case MAIN_MENU_OPTION_SKK:
	if(topwin->anthycontainer != NULL)
	    gtk_widget_hide((topwin->anthycontainer)->container);
	if(topwin->skkcontainer != NULL)
	    gtk_widget_show((topwin->skkcontainer)->container);
	if(topwin->addwordpane != NULL)
	    gtk_widget_hide(topwin->addwordpane);
	if(topwin->uimconfigcontainer != NULL)
	    gtk_widget_hide((topwin->uimconfigcontainer)->container);
	break;
    case MAIN_MENU_OPTION_INTEGRATE:
	if(topwin->anthycontainer != NULL)
	    gtk_widget_hide((topwin->anthycontainer)->container);
	if(topwin->skkcontainer != NULL)
	    gtk_widget_hide((topwin->skkcontainer)->container);
	if(topwin->addwordpane != NULL)
	    gtk_widget_show(topwin->addwordpane);
	if(topwin->uimconfigcontainer != NULL)
	    gtk_widget_hide((topwin->uimconfigcontainer)->container);
	break;
    case MAIN_MENU_OPTION_UIM:
	if(topwin->anthycontainer != NULL)
	    gtk_widget_hide((topwin->anthycontainer)->container);
	if(topwin->skkcontainer != NULL)
	    gtk_widget_hide((topwin->skkcontainer)->container);
	if(topwin->addwordpane != NULL)
	    gtk_widget_hide(topwin->addwordpane);
	if(topwin->uimconfigcontainer != NULL)
	    gtk_widget_show((topwin->uimconfigcontainer)->container);
	break;
    default:
	break;
    }
}

int create_topwindow(TopWindow *topwin) {
    GtkWidget *vbox;
    GtkAccelGroup *accel_group;
    GtkActionGroup *actions;
    GtkUIManager *ui;

    gint use_anthy;
    gint use_uim;

    topwin->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
/*	gtk_window_set_default_size(GTK_OBJECT(win->window), 400, 200); */
    gtk_widget_set_size_request(GTK_WIDGET(topwin->window), 600, 400);

    g_signal_connect(G_OBJECT(topwin->window), "delete_event",
		     G_CALLBACK(cb_delete_topwindow), topwin);

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_widget_show(vbox);
    gtk_container_add(GTK_CONTAINER(topwin->window), vbox);

    actions = gtk_action_group_new("Actions");
    gtk_action_group_add_actions(actions, menu_action_entries,
				 n_menu_action_entries, topwin);
    gtk_action_group_add_radio_actions(actions,
				       im_radio_entries,
				       n_im_radio_entries,
				       MAIN_MENU_OPTION_ANTHY,
				       G_CALLBACK(cb_menu_option_switch_imsystem),
				       topwin);

    ui = gtk_ui_manager_new();
    gtk_ui_manager_insert_action_group(ui, actions, 0);
    gtk_window_add_accel_group(GTK_WINDOW(topwin->window),
			       gtk_ui_manager_get_accel_group(ui));
    
    if(!gtk_ui_manager_add_ui_from_string(ui, ui_info, -1, NULL))
	return -1;
    
    gtk_box_pack_start(GTK_BOX(vbox),
		       gtk_ui_manager_get_widget (ui, "/MainMenu"),
		       FALSE, FALSE, 0);

    use_anthy = anthydic_init();
    use_uim   = uimapi_init();
    if(use_anthy == 0) {
	topwin->anthycontainer = g_malloc(sizeof(AnthyContainer));
	if(topwin->anthycontainer != NULL) {
	    create_anthycontainer(topwin->anthycontainer);
	    gtk_box_pack_start(GTK_BOX(vbox),
			       (topwin->anthycontainer)->container,
			       TRUE, TRUE, 0);
	}
    } else {
	GtkWidget *menuitem;
	menuitem = gtk_ui_manager_get_widget(ui, "/MainMenu/OptionMenu/Anthy");
	gtk_widget_hide(menuitem);
	topwin->anthycontainer = NULL;
    }

    topwin->skkcontainer = g_malloc(sizeof(SKKContainer));
    if(topwin->skkcontainer != NULL) {
	create_skkcontainer(topwin->skkcontainer);
	gtk_box_pack_start(GTK_BOX(vbox),
			   (topwin->skkcontainer)->container,
			   TRUE, TRUE, 0);
    }
    
    
    topwin->addwordpane = addword_pane_new();
    if(topwin->addwordpane != NULL) {
	if(use_anthy == 0) addword_pane_set_mode(ADDWORD_PANE(topwin->addwordpane), ADDWORDPANE_MODE_ANTHY);
	if(use_anthy != 0) addword_pane_set_mode(ADDWORD_PANE(topwin->addwordpane), ADDWORDPANE_MODE_NOTHING);
	gtk_box_pack_start(GTK_BOX(vbox), topwin->addwordpane, TRUE, TRUE, 0);
    } else {
	GtkWidget *menuitem;
	menuitem = gtk_ui_manager_get_widget(ui, "/MainMenu/OptionMenu/IntegrationMode");
	gtk_widget_hide(menuitem);
	topwin->addwordpane = NULL;
    }

    if(use_uim == 0) {
	topwin->uimconfigcontainer = uim_config_container_new();
	if(topwin->uimconfigcontainer != NULL) {
	    gtk_box_pack_start(GTK_BOX(vbox),
			       (topwin->uimconfigcontainer)->container,
			       TRUE, TRUE, 0);
	}
    } else {
	GtkWidget *menuitem;
	menuitem = gtk_ui_manager_get_widget(ui, "/MainMenu/OptionMenu/uim");
	gtk_widget_hide(menuitem);
	topwin->uimconfigcontainer = NULL;
    }
    return 0;
}

int show_topwindow(TopWindow *topwin) {
    /* XXX */
    if(topwin->anthycontainer != NULL)
	show_anthycontainer(topwin->anthycontainer, TRUE);
    if(topwin->skkcontainer != NULL)
	show_skkcontainer(topwin->skkcontainer, FALSE);

    gtk_widget_show(topwin->window);
    return 0;
}

void clean_topwindow(TopWindow *topwin) {
    g_return_if_fail(topwin);
    g_return_if_fail(topwin->anthycontainer);
    clean_anthycontainer(topwin->anthycontainer);
}
