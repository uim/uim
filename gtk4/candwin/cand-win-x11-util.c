/*

  cand-win-x11-util.c: absolute window positioning helpers for gtk4/candwin

  Copyright (c) 2003-2026 uim Project https://github.com/uim/uim

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

#include "cand-win-x11-util.h"

#ifdef GDK_WINDOWING_X11
#include <gdk/x11/gdkx.h>
#endif

void
cand_win_move_window(GtkWidget *widget, gint x, gint y)
{
#ifdef GDK_WINDOWING_X11
  GdkSurface *surface;

  if (!gtk_widget_get_realized(widget))
    return;

  surface = gtk_native_get_surface(GTK_NATIVE(widget));
  if (!surface || !GDK_IS_X11_SURFACE(surface))
    return;

  XMoveWindow(GDK_SURFACE_XDISPLAY(surface), GDK_SURFACE_XID(surface), x, y);
#endif
}

gboolean
cand_win_get_window_origin(GtkWidget *widget, gint *x, gint *y)
{
#ifdef GDK_WINDOWING_X11
  GdkSurface *surface;
  Display *xdisplay;
  Window xwindow, root, child;
  int rx, ry;

  if (!gtk_widget_get_realized(widget))
    return FALSE;

  surface = gtk_native_get_surface(GTK_NATIVE(widget));
  if (!surface || !GDK_IS_X11_SURFACE(surface))
    return FALSE;

  xdisplay = GDK_SURFACE_XDISPLAY(surface);
  xwindow = GDK_SURFACE_XID(surface);
  root = XDefaultRootWindow(xdisplay);

  if (!XTranslateCoordinates(xdisplay, xwindow, root, 0, 0, &rx, &ry, &child))
    return FALSE;

  *x = rx;
  *y = ry;
  return TRUE;
#else
  return FALSE;
#endif
}

void
cand_win_get_monitor_size(GtkWidget *widget, gint *width, gint *height)
{
  GdkDisplay *display;
  GdkSurface *surface;
  GdkMonitor *monitor = NULL;

  /* sane fallback in case no monitor can be determined at all */
  *width = 1024;
  *height = 768;

  display = gtk_widget_get_display(widget);
  if (!display)
    return;

  if (gtk_widget_get_realized(widget)) {
    surface = gtk_native_get_surface(GTK_NATIVE(widget));
    if (surface)
      monitor = gdk_display_get_monitor_at_surface(display, surface);
  }

  if (!monitor) {
    GListModel *monitors = gdk_display_get_monitors(display);
    if (monitors && g_list_model_get_n_items(monitors) > 0)
      monitor = g_list_model_get_item(monitors, 0);
  } else {
    g_object_ref(monitor);
  }

  if (monitor) {
    GdkRectangle geom;
    gdk_monitor_get_geometry(monitor, &geom);
    *width = geom.width;
    *height = geom.height;
    g_object_unref(monitor);
  }
}
