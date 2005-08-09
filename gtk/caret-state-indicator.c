/*

  Copyright (c) 2005 uim Project http://uim.freedesktop.org/

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

#include <gtk/gtk.h>
#include "caret-state-indicator.h"

#include <uim/uim.h>
#include "config.h"
#include "uim/uim-helper.h"
#include "uim/gettext.h"

/*
 * caret state indicator is a state indicator nearby the caret.
 */

#define DEFAULT_WINDOW_WIDTH  20
#define DEFAULT_WINDOW_HEIGHT 20

static gint
get_current_time(void);
static gint
caret_state_indicator_timeout(gpointer data);

/* This function is not correct, size of tv_sec is glong, not gint */
static gint
get_current_time(void)
{
  GTimeVal result;
  g_get_current_time(&result);
  return result.tv_sec;
}

static gint
caret_state_indicator_timeout(gpointer data)
{
  GtkWidget *window = GTK_WIDGET(data);
  gint timeout      = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(window), "timeout"));
  gint called_time  = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(window), "called_time"));
  gint current_time = get_current_time();
  if((current_time - called_time)*1000 >= timeout) {
    gtk_widget_hide(window);
  }
  return 0;
}

static gint
caret_state_indicator_paint_window (GtkWidget *window)
{
  GtkRequisition req;

  gtk_widget_size_request (window, &req);
  gtk_paint_flat_box (window->style, window->window,
		      GTK_STATE_NORMAL, GTK_SHADOW_OUT,
		      NULL, GTK_WIDGET(window), "tooltip",
		      0, 0, DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);

  return FALSE;
}

GtkWidget *
caret_state_indicator_new(void)
{
  GtkWidget *window;
  GtkWidget *label;

  window = gtk_window_new(GTK_WINDOW_POPUP);
  label  = gtk_label_new("");
  gtk_container_add(GTK_CONTAINER(window), label);

  gtk_window_set_default_size(GTK_WINDOW(window),
			      DEFAULT_WINDOW_WIDTH,
			      DEFAULT_WINDOW_HEIGHT);
    gtk_widget_set_app_paintable (window, TRUE);

    g_signal_connect(window, "expose_event",
		     G_CALLBACK (caret_state_indicator_paint_window), 
		     NULL);

  gtk_misc_set_alignment (GTK_MISC (label), 0.5, 0.5);

  g_object_set_data(G_OBJECT(window), "label", label);

  return window;
}

void
caret_state_indicator_update(GtkWidget *window, gint topwin_x, gint topwin_y, const gchar *str)
{
  GtkWidget *label = g_object_get_data(G_OBJECT(window), "label");
  gint cursor_x = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(window), "cursor_x"));
  gint cursor_y = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(window), "cursor_y"));

  if(str) {
    gchar **labels;
    
    labels = g_strsplit(str, "\t", 2);

    gtk_label_set_text(GTK_LABEL(label), labels[0]);
    g_strfreev(labels);
  }
  gtk_window_move(GTK_WINDOW(window), topwin_x + cursor_x, topwin_y + cursor_y + 3);
}

void
caret_state_indicator_set_cursor_location(GtkWidget *window, GdkRectangle *cursor_location)
{
  g_object_set_data(G_OBJECT(window), "cursor_x",
		    GINT_TO_POINTER(cursor_location->x));
  g_object_set_data(G_OBJECT(window), "cursor_y",
		    GINT_TO_POINTER(cursor_location->y+cursor_location->height));
}


void
caret_state_indicator_set_timeout(GtkWidget *window, gint timeout)
{
  gint current_time = get_current_time();

  g_timeout_add(timeout, caret_state_indicator_timeout, (gpointer)window);
  g_object_set_data(G_OBJECT(window), "timeout", GINT_TO_POINTER(timeout));
  g_object_set_data(G_OBJECT(window), "called_time", GINT_TO_POINTER(current_time));
  /* "called_time" stores the time of last calling of this function */
}
