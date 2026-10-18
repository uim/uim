/*

  cand-win-x11-util.h: absolute window positioning helpers for gtk4/candwin

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


  GTK4 removed gtk_window_move()/gtk_window_get_position() and the whole
  GdkScreen/GdkWindow geometry API with no portable replacement, because
  Wayland compositors generally do not let a client place its own
  surface. The uim candidate window, however, is a helper process that
  is *told* an absolute screen position by the client application over
  the uim-helper IPC protocol (the caret location), and must place
  itself there -- there is no window manager involved that could do
  this for it.

  This header centralizes the same pragmatic fallback used by the
  gtk4/toolbar standalone shell:

    * on X11, move/query the surface directly via Xlib
      (GDK_WINDOWING_X11) -- this still works because X11 has always
      allowed a client to reposition its own window;
    * on Wayland (or any other backend), positioning silently becomes
      a no-op; the window still shows, just wherever the compositor
      puts it. This is a documented, unsupported limitation on
      Wayland, matching upstream GTK4's own removal of the API.

  Screen size (needed to keep the popup on-screen) is read through the
  GTK4 GdkMonitor API, which is portable across backends.
*/

#ifndef UIM_GTK4_CAND_WIN_X11_UTIL_H
#define UIM_GTK4_CAND_WIN_X11_UTIL_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

/* Move widget's toplevel surface so its origin is at (x, y) in root
 * (screen) coordinates. No-op when not running on X11. */
void cand_win_move_window(GtkWidget *widget, gint x, gint y);

/* Query widget's toplevel surface origin in root (screen) coordinates.
 * Returns FALSE (leaving *x/*y untouched) when not running on X11 or
 * the surface isn't realized yet. */
gboolean cand_win_get_window_origin(GtkWidget *widget, gint *x, gint *y);

/* Fetch the usable size of the monitor widget is (or would be) shown
 * on, falling back to the first monitor of widget's display. */
void cand_win_get_monitor_size(GtkWidget *widget, gint *width, gint *height);

G_END_DECLS

#endif /* UIM_GTK4_CAND_WIN_X11_UTIL_H */
