/*

  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim

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

#include <config.h>

#include <gtk/gtk.h>
#include <string.h>

#include "uim/uim.h"
#include "uim/uim-helper.h"
#include "uim/gettext.h"

#include "caret-state-indicator.h"
/*
 * caret state indicator is a state indicator nearby the caret.
 */

#define DEFAULT_WINDOW_WIDTH  20
#define DEFAULT_WINDOW_HEIGHT 20

static gint get_current_time(void);
static gint caret_state_indicator_timeout(gpointer data);

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
  gint timeout, called_time, current_time;

  timeout = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(window), "timeout"));
  called_time = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(window),
				"called_time"));
  current_time = get_current_time();

  if ((current_time - called_time) * 1000 >= timeout)
    gtk_widget_hide(window);

  g_object_set_data(G_OBJECT(window), "timeout-tag", GUINT_TO_POINTER(0));

  return FALSE;
}

static gint
#if GTK_CHECK_VERSION(2, 90, 0)
caret_state_indicator_paint_window(GtkWidget *window, cairo_t *cr)
#else
caret_state_indicator_paint_window(GtkWidget *window)
#endif
{
#if GTK_CHECK_VERSION(2, 90, 0)
  gtk_render_frame(gtk_widget_get_style_context(window), cr,
             0, 0,
             gtk_widget_get_allocated_width(window),
             gtk_widget_get_allocated_height(window));
#else
  gtk_paint_flat_box(gtk_widget_get_style(window),
             gtk_widget_get_window(window),
             GTK_STATE_NORMAL, GTK_SHADOW_OUT, NULL, GTK_WIDGET(window),
             "tooltip", 0, 0, -1, -1);
#endif

  return FALSE;
}

static gint
caret_state_indicator_destroy_cb(GtkWidget *window)
{
  GList *label_list, *frame_list;

  label_list = g_object_get_data(G_OBJECT(window), "labels");
  frame_list = g_object_get_data(G_OBJECT(window), "frames");

  g_list_free(label_list);
  g_list_free(frame_list);

  return FALSE;
}

GtkWidget *
caret_state_indicator_new(void)
{
  GtkWidget *window, *label, *hbox, *frame;
  GList *label_list = NULL, *frame_list = NULL;

  window = gtk_window_new(GTK_WINDOW_POPUP);
  label  = gtk_label_new("");
  frame = gtk_frame_new(NULL);
  gtk_container_add(GTK_CONTAINER(frame), label);
#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
#else
  hbox = gtk_hbox_new(TRUE, 0);
#endif
  gtk_box_pack_start(GTK_BOX(hbox), frame, TRUE, TRUE, 0);
  gtk_container_add(GTK_CONTAINER(window), hbox);

  gtk_window_set_default_size(GTK_WINDOW(window),
			      DEFAULT_WINDOW_WIDTH,
			      DEFAULT_WINDOW_HEIGHT);
  gtk_widget_set_app_paintable(window, TRUE);

#if GTK_CHECK_VERSION(2, 90, 0)
  g_signal_connect(window, "draw",
		   G_CALLBACK(caret_state_indicator_paint_window), 
		   NULL);
#else
  g_signal_connect(window, "expose_event",
		   G_CALLBACK(caret_state_indicator_paint_window), 
		   NULL);
#endif
  g_signal_connect(window, "destroy",
		   G_CALLBACK(caret_state_indicator_destroy_cb), 
		   NULL);

  gtk_misc_set_alignment(GTK_MISC(label), 0.5, 0.5);

  label_list = g_list_append(label_list, label);
  frame_list = g_list_append(frame_list, frame);
  g_object_set_data(G_OBJECT(window), "frames", frame_list);
  g_object_set_data(G_OBJECT(window), "labels", label_list);
  g_object_set_data(G_OBJECT(window), "hbox", hbox);

  return window;
}

void
caret_state_indicator_update(GtkWidget *window, gint topwin_x, gint topwin_y, const gchar *str)
{
  gint cursor_x, cursor_y;

  g_return_if_fail(window != NULL);

  cursor_x = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(window), "cursor_x"));
  cursor_y = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(window), "cursor_y"));

  if (str) {
    gchar **cols;
    GtkWidget *label, *hbox, *frame;
    GList *label_list, *frame_list, *list1, *list2;
    gint i;

    list1 = label_list = g_object_get_data(G_OBJECT(window), "labels");
    list2 = frame_list = g_object_get_data(G_OBJECT(window), "frames");
    hbox = g_object_get_data(G_OBJECT(window), "hbox");

    cols = g_strsplit(str, "\t", 0);
    for (i = 0; cols[i] && strcmp("", cols[i]); i++) {
      if (label_list) {
	label = label_list->data;
	gtk_label_set_text(GTK_LABEL(label), cols[i]);
      } else {
	label = gtk_label_new(cols[i]);
	frame = gtk_frame_new(NULL);
	gtk_container_add(GTK_CONTAINER(frame), label);
	gtk_box_pack_start(GTK_BOX(hbox), frame, TRUE, TRUE, 0);
	list1 = g_list_append(list1, label);
	label_list = g_list_find(list1, label);
	list2 = g_list_append(list2, frame);
	frame_list = g_list_find(list2, frame);
      }
      label_list = label_list->next;
      frame_list = frame_list->next;
    }

    while (label_list) {
      label = label_list->data;
      frame = frame_list->data;
      label_list = label_list->next;
      frame_list = frame_list->next;
      gtk_container_remove(GTK_CONTAINER(frame), label);
      gtk_container_remove(GTK_CONTAINER(hbox), frame);
      list1 = g_list_remove(list1, label);
      list2 = g_list_remove(list2, frame);
    }
    g_object_set_data(G_OBJECT(window), "labels", list1);
    g_object_set_data(G_OBJECT(window), "frames", list2);

    g_strfreev(cols);
  }

  gtk_window_move(GTK_WINDOW(window), topwin_x + cursor_x,
		  topwin_y + cursor_y + 3);
}

void
caret_state_indicator_set_cursor_location(GtkWidget *window, GdkRectangle *cursor_location)
{
  g_return_if_fail(window != NULL);

  g_object_set_data(G_OBJECT(window), "cursor_x",
		    GINT_TO_POINTER(cursor_location->x));
  g_object_set_data(G_OBJECT(window), "cursor_y",
		    GINT_TO_POINTER(cursor_location->y +
				    cursor_location->height));
}

void
caret_state_indicator_set_timeout(GtkWidget *window, gint timeout)
{
  gint current_time;
  guint tag, oldtag;

  g_return_if_fail(window != NULL);

  oldtag = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(window), "timeout-tag"));

  if (oldtag > 0)
    g_source_remove(oldtag);

  current_time = get_current_time();
  tag = g_timeout_add(timeout, caret_state_indicator_timeout, (gpointer)window);

  g_object_set_data(G_OBJECT(window), "timeout-tag", GUINT_TO_POINTER(tag));
  g_object_set_data(G_OBJECT(window), "timeout", GINT_TO_POINTER(timeout));
  /* "called_time" stores the latest time when this function is called */
  g_object_set_data(G_OBJECT(window), "called_time",
		    GINT_TO_POINTER(current_time));
}
