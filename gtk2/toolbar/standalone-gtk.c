/*

  toolbar-standalone-gtk.c: toolbar implementation with GTK+

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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <locale.h>
#include <uim/gettext.h>
#include <gtk/gtk.h>
#include "uim/uim.h"

extern GtkWidget *uim_toolbar_standalone_new(void);

static gboolean toolbar_dragging = FALSE;
static gint window_drag_start_x = -1, window_drag_start_y = -1;
static gint pointer_drag_start_x = -1, pointer_drag_start_y = -1;
static void size_request_cb(GtkWidget *widget, GtkRequisition *req,
                            gpointer data);

#if GLIB_CHECK_VERSION(2, 6, 0)

static void
parse_options(gint argc, gchar **argv)
{

}

#endif

static void
delete_event(GtkWidget *widget, gpointer data)
{
  gtk_main_quit();
}

static gboolean
button_press_event_cb(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
  GdkCursor *cursor;
  GtkWidget *toolbar;
  gint height, width;
#if GTK_CHECK_VERSION(3, 0, 0)
  GdkDevice *device = gtk_get_current_event_device();
#endif

  switch (event->type) {
  case GDK_BUTTON_PRESS:
    cursor = gdk_cursor_new(GDK_FLEUR);
#if GTK_CHECK_VERSION(3, 0, 0)
    gdk_device_grab(device, gtk_widget_get_window(widget),
                     GDK_OWNERSHIP_NONE, FALSE,
#else
    gdk_pointer_grab(gtk_widget_get_window(widget), FALSE,
#endif
		     GDK_BUTTON_RELEASE_MASK |
		     GDK_POINTER_MOTION_MASK,
#if !GTK_CHECK_VERSION(3, 0, 0)
		     NULL,
#endif
		     cursor, event->time);
#if GTK_CHECK_VERSION(3, 0, 0)
    g_object_unref(cursor);
#else
    gdk_cursor_unref(cursor);
#endif

    gtk_window_get_position(GTK_WINDOW(widget),
		    	    &window_drag_start_x,
			    &window_drag_start_y);
    pointer_drag_start_x = (gint)event->x_root;
    pointer_drag_start_y = (gint)event->y_root;
    toolbar_dragging = TRUE;
    break;
  case GDK_2BUTTON_PRESS:
    toolbar = GTK_WIDGET(data);
#if GTK_CHECK_VERSION(2, 18, 0)
    if (gtk_widget_get_visible(toolbar)) {
#else
    if (GTK_WIDGET_VISIBLE(toolbar)) {
#endif
      gtk_window_get_size(GTK_WINDOW(widget), &width, &height);
      gtk_widget_hide(toolbar);
    } else {
      height = -1;
      gtk_widget_show(toolbar);
    }
#if GTK_CHECK_VERSION(2, 90, 0)
    {
      GtkRequisition minimum_size, natural_size;
      gtk_widget_get_preferred_size(widget, &minimum_size, &natural_size);
      if (height > 0)
        natural_size.height = height;
      else if (height == 0)
        natural_size.height = minimum_size.height;
      size_request_cb(widget, &natural_size, NULL);
    }
#else
    gtk_widget_set_size_request(widget, -1, height);
#endif
    break;
  default:
    break;
  }

  return FALSE;
}

static void
helper_win_set_position(GtkWidget *window, gint x, gint y)
{
  gint wx = x, wy = y;
  gint w, h, sc_w, sc_h;

  sc_w = gdk_screen_width();
  sc_h = gdk_screen_height();

#if GTK_CHECK_VERSION(2, 90, 0)
  w = gdk_window_get_width(gtk_widget_get_window(window));
  h = gdk_window_get_height(gtk_widget_get_window(window));
#else
  gdk_drawable_get_size(gtk_widget_get_window(window), &w, &h);
#endif

  if (wx < 0)
    wx = 0;
  else if (wx > sc_w - w)
    wx = sc_w - w;
  if (wy < 0)
    wy = 0;
  else if (wy > sc_h - h)
    wy = sc_h -h;

  g_object_set_data(G_OBJECT(window), "position_x", GINT_TO_POINTER(wx));
  g_object_set_data(G_OBJECT(window), "position_y", GINT_TO_POINTER(wy));
  gtk_window_move(GTK_WINDOW(window), wx, wy);
}

static gboolean
motion_notify_event_cb(GtkWidget *widget, GdkEventMotion *event, gpointer data)
{
  if (toolbar_dragging) {
    gint wx, wy;

    wx = window_drag_start_x + ((gint)event->x_root - pointer_drag_start_x);
    wy = window_drag_start_y + ((gint)event->y_root - pointer_drag_start_y);

    helper_win_set_position(widget, wx, wy);

    return TRUE;
  }

  return FALSE;
}

static gboolean
button_release_event_cb(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
  if (!toolbar_dragging)
    return FALSE;

#if GTK_CHECK_VERSION(3, 0, 0)
  gdk_device_ungrab(gtk_get_current_event_device(), event->time);
#else
  gdk_pointer_ungrab(event->time);
#endif

  pointer_drag_start_x = -1;
  pointer_drag_start_y = -1;
  toolbar_dragging = FALSE;

  return FALSE;
}

#if GTK_CHECK_VERSION(2, 90, 0)
static gboolean
handle_draw_cb(GtkWidget *widget, cairo_t *cr)
{
  gtk_render_handle(gtk_widget_get_style_context(widget), cr,
		   0, 0,
		   gtk_widget_get_allocated_width(widget),
		   gtk_widget_get_allocated_height(widget));
  return FALSE;
}

#else
static gboolean
handle_expose_event_cb(GtkWidget *widget, GdkEventExpose *event)
{
  GdkRectangle *rect = &event->area;

#if GTK_CHECK_VERSION(2, 18, 0)
  GtkAllocation allocation;
  gtk_widget_get_allocation(widget, &allocation);
  gtk_paint_handle(gtk_widget_get_style(widget), gtk_widget_get_window(widget),
		   GTK_STATE_NORMAL, GTK_SHADOW_OUT,
		   rect, widget, "handlebox",
		   allocation.x, allocation.y,
		   allocation.width, allocation.height,
		   GTK_ORIENTATION_VERTICAL);
#else
  gtk_paint_handle(widget->style, widget->window,
                   GTK_STATE_NORMAL, GTK_SHADOW_OUT,
                   rect, widget, "handlebox",
                   widget->allocation.x, widget->allocation.y,
                   widget->allocation.width, widget->allocation.height,
                   GTK_ORIENTATION_VERTICAL);
#endif

  return FALSE;
}
#endif

static void
size_allocate_cb(GtkWidget *widget, GtkAllocation *allocation, gpointer user_data)
{
  gint x, y;

#if GTK_CHECK_VERSION(2, 20, 0)
  if (gtk_widget_get_mapped(widget)) {
#else
  if (GTK_WIDGET_MAPPED(widget)) {
#endif
    gtk_window_get_position(GTK_WINDOW(widget), &x, &y);
    helper_win_set_position(widget, x, y);
  }
}

static void
size_request_cb(GtkWidget *widget, GtkRequisition *req, gpointer data)
{

#if GTK_CHECK_VERSION(2, 20, 0)
  if (gtk_widget_get_mapped(widget)) {
#else
  if (GTK_WIDGET_MAPPED(widget)) {
#endif
    gint width, height;
    gtk_window_get_size(GTK_WINDOW(widget), &width, &height);

    if (width > req->width) {
      gtk_window_resize(GTK_WINDOW(widget), req->width, req->height);
    }
  }
}

int
main(int argc, char *argv[])
{
  GtkWidget *toolbar;
  GtkWidget *window;
  GtkWidget *hbox;
  GtkWidget *handle;
  GtkWidget *frame;

  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8");

  uim_init();

  gtk_init(&argc, &argv);

  window = gtk_window_new(GTK_WINDOW_POPUP);

  gtk_window_set_type_hint(GTK_WINDOW(window), GDK_WINDOW_TYPE_HINT_DOCK);

  gtk_window_set_skip_taskbar_hint(GTK_WINDOW(window), TRUE);
  gtk_window_set_decorated(GTK_WINDOW(window), FALSE);
  gtk_window_stick(GTK_WINDOW(window));

  gtk_widget_add_events(window, GDK_BUTTON_PRESS_MASK);

  frame = gtk_frame_new(NULL);
  gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_OUT);
  gtk_container_add(GTK_CONTAINER(window), frame);

#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
#else
  hbox = gtk_hbox_new(FALSE, 0);
#endif
  gtk_container_add(GTK_CONTAINER(frame), hbox);

  handle = gtk_drawing_area_new();
  gtk_widget_set_size_request(handle, 8, -1);
  gtk_box_pack_start(GTK_BOX(hbox), handle, FALSE, FALSE, 0);

  toolbar = (GtkWidget*)uim_toolbar_standalone_new();
  gtk_box_pack_start(GTK_BOX(hbox), toolbar, FALSE, FALSE, 0);

#if GTK_CHECK_VERSION(2, 90, 0)
  g_signal_connect(G_OBJECT(handle), "draw",
		   G_CALLBACK(handle_draw_cb), NULL);
#else
  g_signal_connect(G_OBJECT(handle), "expose-event",
		   G_CALLBACK(handle_expose_event_cb), NULL);
#endif

  g_signal_connect(G_OBJECT(window), "delete_event",
		   G_CALLBACK(delete_event), NULL);
  g_signal_connect(G_OBJECT(window), "button-press-event",
		   G_CALLBACK(button_press_event_cb), toolbar);
  g_signal_connect(G_OBJECT(window), "button-release-event",
		   G_CALLBACK(button_release_event_cb), NULL);
  g_signal_connect(G_OBJECT(window), "motion-notify-event",
		   G_CALLBACK(motion_notify_event_cb), NULL);
  g_signal_connect(G_OBJECT(window), "size-allocate",
		   G_CALLBACK(size_allocate_cb), NULL);
#if !GTK_CHECK_VERSION(2, 90, 0)
  g_signal_connect(G_OBJECT(window), "size-request",
		   G_CALLBACK(size_request_cb), NULL);
#endif

  gtk_widget_show_all(GTK_WIDGET(window));

  if (argc > 1) {
    gint x, y;
    if (!gtk_window_parse_geometry(GTK_WINDOW(window), argv[1])) {

#if GLIB_CHECK_VERSION(2, 6, 0)
      parse_options(argc, argv);
#else
      g_warning(_("Unable to parse the geometry string '%s'"), argv[1]);
#endif
    }
    gtk_window_get_position(GTK_WINDOW(window), &x, &y);
    g_object_set_data(G_OBJECT(window), "position_x", GINT_TO_POINTER(x));
    g_object_set_data(G_OBJECT(window), "position_y", GINT_TO_POINTER(y));
  } else {
    gint x, y, w, h, sc_w, sc_h;
    gint panel_height = 32; /* FIXME! */

    gtk_window_get_size(GTK_WINDOW(window), &w, &h);
    sc_w = gdk_screen_width();
    sc_h = gdk_screen_height();

    x = sc_w - w;
    y = sc_h - h - panel_height; /* FIXME! */
    helper_win_set_position(window, x, y);
  }

  gtk_main();

  uim_quit();

  return 0;
}

