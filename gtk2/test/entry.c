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

/* example-start entry entry.c */

#include <stdio.h>
#include <stdlib.h>
#include <gtk/gtk.h>

void enter_callback(GtkWidget *, GtkWidget *);
void entry_toggle_editable(GtkWidget *, GtkWidget *);
void entry_toggle_visibility(GtkWidget *c, GtkWidget *);

void enter_callback( GtkWidget *widget,
                     GtkWidget *entry )
{
  gchar *entry_text;
  entry_text = (gchar*)gtk_entry_get_text(GTK_ENTRY(entry));
  printf("Entry contents: %s\n", entry_text);
}

void entry_toggle_editable( GtkWidget *checkbutton,
                            GtkWidget *entry )
{
  gtk_editable_set_editable(GTK_EDITABLE(entry),
			 gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton)));
}

void entry_toggle_visibility( GtkWidget *checkbutton,
                              GtkWidget *entry )
{
  gtk_entry_set_visibility(GTK_ENTRY(entry),
			 gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton)));
}

int main( int   argc,
          char *argv[] )
{

    GtkWidget *window;
    GtkWidget *vbox, *hbox;
    GtkWidget *entry;
    GtkWidget *button;
    GtkWidget *check;
    gint position;

    GTimer *tim = g_timer_new();

    gtk_init (&argc, &argv);

    g_timer_start(tim);

    /* create a new window */
    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_widget_set_size_request( GTK_WIDGET (window), 200, 100);
    gtk_window_set_title(GTK_WINDOW (window), "GTK Entry");
    g_signal_connect(G_OBJECT (window), "delete_event",
                       G_CALLBACK(exit), NULL);

#if GTK_CHECK_VERSION(3, 2, 0)
    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
    vbox = gtk_vbox_new (FALSE, 0);
#endif
    gtk_container_add (GTK_CONTAINER (window), vbox);
    gtk_widget_show (vbox);

    entry = gtk_entry_new ();
    gtk_entry_set_max_length (GTK_ENTRY(entry), 50);
    g_signal_connect(G_OBJECT(entry), "activate",
		       G_CALLBACK(enter_callback),
		       entry);
    gtk_entry_set_text (GTK_ENTRY (entry), "hello");
    gtk_editable_insert_text (GTK_EDITABLE (entry), " world", -1, &position);
    gtk_editable_select_region (GTK_EDITABLE (entry),
			     0, gtk_entry_get_text_length(GTK_ENTRY(entry)));
    gtk_box_pack_start (GTK_BOX (vbox), entry, TRUE, TRUE, 0);
    gtk_widget_show (entry);

#if GTK_CHECK_VERSION(3, 2, 0)
    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
    hbox = gtk_hbox_new (FALSE, 0);
#endif
    gtk_container_add (GTK_CONTAINER (vbox), hbox);
    gtk_widget_show (hbox);
                                  
    check = gtk_check_button_new_with_label("Editable");
    gtk_box_pack_start (GTK_BOX (hbox), check, TRUE, TRUE, 0);
    g_signal_connect (G_OBJECT(check), "toggled",
			G_CALLBACK(entry_toggle_editable), entry);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), TRUE);
    gtk_widget_show (check);
    
    check = gtk_check_button_new_with_label("Visible");
    gtk_box_pack_start (GTK_BOX (hbox), check, TRUE, TRUE, 0);
    g_signal_connect (G_OBJECT(check), "toggled",
			G_CALLBACK(entry_toggle_visibility), entry);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), TRUE);
    gtk_widget_show (check);
                                   
    button = gtk_button_new_with_label ("Close");
    g_signal_connect_swapped (G_OBJECT (button), "clicked",
			       G_CALLBACK(exit),
			       G_OBJECT (window));
    gtk_box_pack_start (GTK_BOX (vbox), button, TRUE, TRUE, 0);
#if GTK_CHECK_VERSION(2, 18, 0)
    gtk_widget_set_can_default (button, TRUE);
#else
    GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
#endif
    gtk_widget_grab_default (button);
    gtk_widget_show (button);
    
    gtk_widget_show(window);

    g_print("elapsed time: %f\n",g_timer_elapsed(tim,NULL));
    g_timer_destroy(tim);

    gtk_main();
    return(0);
}
/* example-end */
