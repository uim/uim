/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* eggtrayicon.c
 * Copyright (C) 2002 Anders Carlsson <andersca@gnu.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <config.h>

#include "uim/gettext.h"
#include <string.h>

#include "eggtrayicon.h"

#if !GTK_CHECK_VERSION(2, 90, 0)
#include <gdkconfig.h>
#endif
#include <gdk/gdkx.h>
#include <X11/Xatom.h>
#include <gtk/gtk.h>

#ifndef EGG_COMPILATION
#ifndef _
#define _(x) dgettext (GETTEXT_PACKAGE, x)
#define N_(x) x
#endif
#else
#define _(x) x
#define N_(x) x
#endif

#define SYSTEM_TRAY_REQUEST_DOCK    0
#define SYSTEM_TRAY_BEGIN_MESSAGE   1
#define SYSTEM_TRAY_CANCEL_MESSAGE  2

#define SYSTEM_TRAY_ORIENTATION_HORZ 0
#define SYSTEM_TRAY_ORIENTATION_VERT 1

enum {
  PROP_0,
  PROP_ORIENTATION
};
         
static GtkPlugClass *parent_class = NULL;

static void egg_tray_icon_init (EggTrayIcon *icon);
static void egg_tray_icon_class_init (EggTrayIconClass *klass);

#if GLIB_CHECK_VERSION(2, 13, 1)
static void egg_tray_icon_constructed  (GObject    *object);
#endif
static void egg_tray_icon_dispose      (GObject    *object);

static void egg_tray_icon_get_property (GObject    *object,
					guint       prop_id,
					GValue     *value,
					GParamSpec *pspec);

static void    egg_tray_icon_realize   (GtkWidget   *widget);
static void    egg_tray_icon_style_set (GtkWidget   *widget,
                                        GtkStyle    *previous_style);
static gboolean egg_tray_icon_delete   (GtkWidget   *widget,
                                        GdkEventAny *event);
static gboolean egg_tray_icon_expose   (GtkWidget      *widget,
                                        GdkEventExpose *event);

static void egg_tray_icon_clear_manager_window     (EggTrayIcon *icon);
static void egg_tray_icon_update_manager_window    (EggTrayIcon *icon);
static void egg_tray_icon_manager_window_destroyed (EggTrayIcon *icon);
static GdkFilterReturn egg_tray_icon_manager_filter (GdkXEvent *xevent,
                                                     GdkEvent  *event,
                                                     gpointer   user_data);

GType
egg_tray_icon_get_type (void)
{
  static GType our_type = 0;

  if (our_type == 0)
    {
      static const GTypeInfo our_info =
      {
	sizeof (EggTrayIconClass),
	(GBaseInitFunc) NULL,
	(GBaseFinalizeFunc) NULL,
	(GClassInitFunc) egg_tray_icon_class_init,
	NULL, /* class_finalize */
	NULL, /* class_data */
	sizeof (EggTrayIcon),
	0,    /* n_preallocs */
	(GInstanceInitFunc) egg_tray_icon_init
      };

      our_type = g_type_register_static (GTK_TYPE_PLUG, "EggTrayIcon", &our_info, 0);
    }

  return our_type;
}

static void
egg_tray_icon_init (EggTrayIcon *icon)
{
  icon->stamp = 1;
  icon->orientation = GTK_ORIENTATION_HORIZONTAL;
  
  gtk_widget_set_app_paintable (GTK_WIDGET (icon), TRUE);
  gtk_widget_add_events (GTK_WIDGET (icon), GDK_PROPERTY_CHANGE_MASK);
}

static void
egg_tray_icon_class_init (EggTrayIconClass *klass)
{
  GObjectClass *gobject_class = (GObjectClass *)klass;
  GtkWidgetClass *widget_class = (GtkWidgetClass *)klass;

  parent_class = g_type_class_peek_parent (klass);

  gobject_class->get_property = egg_tray_icon_get_property;
#if GLIB_CHECK_VERSION(2, 13, 1)
  gobject_class->constructed = egg_tray_icon_constructed;
#endif
  gobject_class->dispose = egg_tray_icon_dispose;

  widget_class->realize = egg_tray_icon_realize;
  widget_class->style_set = egg_tray_icon_style_set;
  widget_class->delete_event = egg_tray_icon_delete;
#if !GTK_CHECK_VERSION(2, 90, 0)
  widget_class->expose_event = egg_tray_icon_expose;
#endif

  g_object_class_install_property (gobject_class,
				   PROP_ORIENTATION,
				   g_param_spec_enum ("orientation",
						      _("Orientation"),
						      _("The orientation of the tray."),
						      GTK_TYPE_ORIENTATION,
						      GTK_ORIENTATION_HORIZONTAL,
						      G_PARAM_READABLE));

}

static void
#if GLIB_CHECK_VERSION(2, 13, 1)
egg_tray_icon_constructed (GObject *object)
#else
egg_tray_icon_realize_internal (GtkWidget *object)
#endif
{
  /* Do setup that depends on the screen; screen has been set at this point */

  EggTrayIcon *icon = EGG_TRAY_ICON (object);
  GdkScreen *screen = gtk_widget_get_screen (GTK_WIDGET (object));
  GdkWindow *root_window = gdk_screen_get_root_window (screen);
  GdkDisplay *display = gtk_widget_get_display (GTK_WIDGET (object));
  Display *xdisplay = gdk_x11_display_get_xdisplay (display);
  char buffer[256];

  g_snprintf (buffer, sizeof (buffer),
              "_NET_SYSTEM_TRAY_S%d",
              gdk_screen_get_number (screen));

  icon->selection_atom = XInternAtom (xdisplay, buffer, False);

  icon->manager_atom = XInternAtom (xdisplay, "MANAGER", False);

  icon->system_tray_opcode_atom = XInternAtom (xdisplay,
                                               "_NET_SYSTEM_TRAY_OPCODE",
                                               False);

  icon->orientation_atom = XInternAtom (xdisplay,
                                        "_NET_SYSTEM_TRAY_ORIENTATION",
                                        False);

  icon->visual_atom = XInternAtom (xdisplay,
                                   "_NET_SYSTEM_TRAY_VISUAL",
                                   False);

  /* Add a root window filter so that we get changes on MANAGER */
  gdk_window_add_filter (root_window,
                         egg_tray_icon_manager_filter, icon);

  egg_tray_icon_update_manager_window (icon);
}

static void
egg_tray_icon_clear_manager_window (EggTrayIcon *icon)
{
  GdkDisplay *display = gtk_widget_get_display (GTK_WIDGET (icon));

  if (icon->manager_window != None)
    {
      GdkWindow *gdkwin;

#if GTK_CHECK_VERSION(2, 24, 0)
      gdkwin = gdk_x11_window_lookup_for_display (display,
                                              icon->manager_window);
#else
      gdkwin = gdk_window_lookup_for_display (display,
                                              icon->manager_window);
#endif

      gdk_window_remove_filter (gdkwin, egg_tray_icon_manager_filter, icon);

      icon->manager_window = None;
      icon->manager_visual = NULL;
    }
}

static void
egg_tray_icon_dispose (GObject *object)
{
  EggTrayIcon *icon = EGG_TRAY_ICON (object);
  GtkWidget *widget = GTK_WIDGET (object);
  GdkWindow *root_window = gdk_screen_get_root_window (gtk_widget_get_screen (widget));

  egg_tray_icon_clear_manager_window (icon);

  gdk_window_remove_filter (root_window, egg_tray_icon_manager_filter, icon);
}

static void
egg_tray_icon_get_property (GObject    *object,
			    guint       prop_id,
			    GValue     *value,
			    GParamSpec *pspec)
{
  EggTrayIcon *icon = EGG_TRAY_ICON (object);

  switch (prop_id)
    {
    case PROP_ORIENTATION:
      g_value_set_enum (value, icon->orientation);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static gboolean
egg_tray_icon_expose (GtkWidget *widget,
                      GdkEventExpose *event)
{
  EggTrayIcon *icon = EGG_TRAY_ICON (widget);
  GtkWidget *focus_child;
  gint border_width, x, y, width, height;
  gboolean retval = FALSE;

#if GTK_CHECK_VERSION(2, 8, 0)
  if (icon->manager_visual_rgba)
    {
      /* Clear to transparent */
      cairo_t *cr = gdk_cairo_create (gtk_widget_get_window(widget));
      cairo_set_source_rgba (cr, 0, 0, 0, 0);
      cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
      gdk_cairo_region (cr, event->region);
      cairo_fill (cr);
      cairo_destroy (cr); 
    }
  else
#endif
    {
      /* Clear to parent-relative pixmap */
      gdk_window_clear_area (gtk_widget_get_window(widget),
          event->area.x, event->area.y,
                             event->area.width, event->area.height);
    }

#if !GTK_CHECK_VERSION(2, 90, 0)
  if (GTK_WIDGET_CLASS (parent_class)->expose_event)
    retval = GTK_WIDGET_CLASS (parent_class)->expose_event (widget, event);
#endif

  focus_child = gtk_container_get_focus_child(GTK_CONTAINER (widget));
#if GTK_CHECK_VERSION(2, 18, 0)
  if (focus_child && gtk_widget_has_focus (focus_child))
    {
      GtkAllocation allocation;
      border_width = gtk_container_get_border_width(GTK_CONTAINER (widget));

      gtk_widget_get_allocation(widget, &allocation);

      x = allocation.x + border_width;
      y = allocation.y + border_width;

      width  = allocation.width  - 2 * border_width;
      height = allocation.height - 2 * border_width;

#if GTK_CHECK_VERSION(2, 90, 0)
      gtk_render_focus (gtk_widget_get_style_context(widget),
                        gtk_widget_get_window(widget),
                        x, y, width, height);
#else
      gtk_paint_focus (gtk_widget_get_style(widget),
                       gtk_widget_get_window(widget),
                       gtk_widget_get_state (widget),
                       &event->area, widget, "tray_icon",
                       x, y, width, height);
#endif
    }
#else
  if (focus_child && GTK_WIDGET_HAS_FOCUS (focus_child))
    {
      border_width = GTK_CONTAINER (widget)->border_width;

      x = widget->allocation.x + border_width;
      y = widget->allocation.y + border_width;

      width  = widget->allocation.width  - 2 * border_width;
      height = widget->allocation.height - 2 * border_width;
 
      gtk_paint_focus (widget->style, widget->window,
                       GTK_WIDGET_STATE (widget),
                       &event->area, widget, "tray_icon",
                       x, y, width, height);
    }
#endif
  return retval;
}

static void
egg_tray_icon_get_orientation_property (EggTrayIcon *icon)
{
  GdkScreen *screen = gtk_widget_get_screen (GTK_WIDGET (icon));
  GdkDisplay *display = gdk_screen_get_display (screen);
  Display *xdisplay = GDK_DISPLAY_XDISPLAY (display);

  Atom type;
  int format;
  union {
	gulong *prop;
	guchar *prop_ch;
  } prop = { NULL };
  gulong nitems;
  gulong bytes_after;
  int error, result;

  g_assert (icon->manager_window != None);
  
  gdk_error_trap_push ();
  type = None;
  result = XGetWindowProperty (xdisplay,
			       icon->manager_window,
			       icon->orientation_atom,
			       0, G_MAXLONG, FALSE,
			       XA_CARDINAL,
			       &type, &format, &nitems,
			       &bytes_after, &(prop.prop_ch));
  error = gdk_error_trap_pop ();

  if (error || result != Success)
    return;

  if (type == XA_CARDINAL)
    {
      GtkOrientation orientation;

      orientation = (prop.prop [0] == SYSTEM_TRAY_ORIENTATION_HORZ) ?
					GTK_ORIENTATION_HORIZONTAL :
					GTK_ORIENTATION_VERTICAL;

      if (icon->orientation != orientation)
	{
	  icon->orientation = orientation;

	  g_object_notify (G_OBJECT (icon), "orientation");
	}
    }

  if (type != None)
    XFree (prop.prop);
}

static void
egg_tray_icon_get_visual_property (EggTrayIcon *icon)
{
  GdkScreen *screen = gtk_widget_get_screen (GTK_WIDGET (icon));
  GdkDisplay *display = gdk_screen_get_display (screen);
  Display *xdisplay = GDK_DISPLAY_XDISPLAY (display);

  Atom type;
  int format;
  union {
        gulong *prop;
        guchar *prop_ch;
  } prop = { NULL };
  gulong nitems;
  gulong bytes_after;
  int error, result;
  GdkVisual *visual;
  gint red_prec;
  gint blue_prec;
  gint green_prec;
  gint depth;

  g_assert (icon->manager_window != None);

  gdk_error_trap_push ();
  type = None;
  result = XGetWindowProperty (xdisplay,
                               icon->manager_window,
                               icon->visual_atom,
                               0, G_MAXLONG, FALSE,
                               XA_VISUALID,
                               &type, &format, &nitems,
                               &bytes_after, &(prop.prop_ch));
  error = gdk_error_trap_pop ();

  visual = NULL;

  if (!error && result == Success &&
      type == XA_VISUALID && nitems == 1 && format == 32)
    {
      VisualID visual_id = prop.prop[0];
      visual = gdk_x11_screen_lookup_visual (screen, visual_id);
    }

  icon->manager_visual = visual;
#if GTK_CHECK_VERSION(2, 22, 0)
  gdk_visual_get_red_pixel_details(visual, NULL, NULL, &red_prec);
  gdk_visual_get_blue_pixel_details(visual, NULL, NULL, &blue_prec);
  gdk_visual_get_green_pixel_details(visual, NULL, NULL, &green_prec);
  depth = gdk_visual_get_depth(visual);
#else
  red_prec = visual->red_prec;
  blue_prec = visual->blue_prec;
  green_prec = visual->blue_prec;
  depth = visual->depth;
#endif
  icon->manager_visual_rgba = visual != NULL &&
    (red_prec + blue_prec + green_prec < depth);

  /* For the background-relative hack we use when we aren't using a real RGBA
   * visual, we can't be double-buffered */
  gtk_widget_set_double_buffered (GTK_WIDGET (icon), icon->manager_visual_rgba);

  if (type != None)
    XFree (prop.prop);
}

static GdkFilterReturn
egg_tray_icon_manager_filter (GdkXEvent *xevent, GdkEvent *event, gpointer user_data)
{
  EggTrayIcon *icon = user_data;
  XEvent *xev = (XEvent *)xevent;

  if (xev->xany.type == ClientMessage &&
      xev->xclient.message_type == icon->manager_atom &&
      xev->xclient.data.l[1] == (long)icon->selection_atom)
    {
      egg_tray_icon_update_manager_window (icon);
    }
  else if (xev->xany.window == icon->manager_window)
    {
      if (xev->xany.type == PropertyNotify &&
	  xev->xproperty.atom == icon->orientation_atom)
	{
	  egg_tray_icon_get_orientation_property (icon);
	}
      else if (xev->xany.type == DestroyNotify)
	{
	  egg_tray_icon_manager_window_destroyed (icon);
	}
    }

  return GDK_FILTER_CONTINUE;
}

static void
egg_tray_icon_send_manager_message (EggTrayIcon *icon,
				    long         message,
				    Window       window,
				    long         data1,
				    long         data2,
				    long         data3)
{
  XClientMessageEvent ev;
  Display *display;
  
  memset (&ev, 0, sizeof (ev));
  ev.type = ClientMessage;
  ev.window = window;
  ev.message_type = icon->system_tray_opcode_atom;
  ev.format = 32;
  ev.data.l[0]
      = gdk_x11_get_server_time (gtk_widget_get_window(GTK_WIDGET (icon)));
  ev.data.l[1] = message;
  ev.data.l[2] = data1;
  ev.data.l[3] = data2;
  ev.data.l[4] = data3;

  display = GDK_DISPLAY_XDISPLAY (gtk_widget_get_display (GTK_WIDGET (icon)));
  
  gdk_error_trap_push ();
  XSendEvent (display,
	      icon->manager_window, False, NoEventMask, (XEvent *)&ev);
  gdk_display_sync (gtk_widget_get_display (GTK_WIDGET (icon)));
  gdk_error_trap_pop ();
}

static void
egg_tray_icon_send_dock_request (EggTrayIcon *icon)
{
  egg_tray_icon_send_manager_message (icon,
				      SYSTEM_TRAY_REQUEST_DOCK,
				      icon->manager_window,
				      gtk_plug_get_id (GTK_PLUG (icon)),
				      0, 0);
}

static void
egg_tray_icon_update_manager_window (EggTrayIcon *icon)
{
  GtkWidget *widget = GTK_WIDGET (icon);
  GdkScreen *screen = gtk_widget_get_screen (widget);
  GdkDisplay *display = gdk_screen_get_display (screen);
  Display *xdisplay = GDK_DISPLAY_XDISPLAY (display);
  
  if (icon->manager_window != None)
    return;

  
  XGrabServer (xdisplay);
  
  icon->manager_window = XGetSelectionOwner (xdisplay,
					     icon->selection_atom);

  if (icon->manager_window != None)
    XSelectInput (xdisplay,
		  icon->manager_window, StructureNotifyMask|PropertyChangeMask);

  XUngrabServer (xdisplay);
  XFlush (xdisplay);
  
  if (icon->manager_window != None)
    {
      GdkWindow *gdkwin;

#if GTK_CHECK_VERSION(2, 24, 0)
      gdkwin = gdk_x11_window_lookup_for_display (gtk_widget_get_display (GTK_WIDGET (icon)),
					      icon->manager_window);
#else
      gdkwin = gdk_window_lookup_for_display (gtk_widget_get_display (GTK_WIDGET (icon)),
					      icon->manager_window);
#endif
      
      gdk_window_add_filter (gdkwin, egg_tray_icon_manager_filter, icon);

      egg_tray_icon_get_orientation_property (icon);
      egg_tray_icon_get_visual_property (icon);

#if GTK_CHECK_VERSION(2, 20, 0)
      if (gtk_widget_get_realized (GTK_WIDGET (icon)))
#else
      if (GTK_WIDGET_REALIZED (GTK_WIDGET (icon)))
#endif
        {
          if ((icon->manager_visual == NULL &&
               gtk_widget_get_visual (widget) == gdk_screen_get_system_visual (screen)) ||
              (icon->manager_visual == gtk_widget_get_visual (widget)))
            {
              /* Already have the right visual, can just dock
               */
              egg_tray_icon_send_dock_request (icon);
            }
          else
            {
              /* Need to re-realize the widget to get the right visual
               */
              gtk_widget_hide (widget);
              gtk_widget_unrealize (widget);
              gtk_widget_show (widget);
            }
        }
    }
}

static void
egg_tray_icon_manager_window_destroyed (EggTrayIcon *icon)
{
  g_return_if_fail (icon->manager_window != None);

  egg_tray_icon_clear_manager_window (icon);
}

static gboolean
egg_tray_icon_delete (GtkWidget   *widget,
                      GdkEventAny *event)
{
  /* A bug in X server versions up to x.org 1.5.0 means that:
   * XFixesChangeSaveSet(...., SaveSetRoot, SaveSetUnmap) doesn't work properly
   * and we'll left mapped in a separate toplevel window if the tray is destroyed.
   * For simplicity just get rid of our X window and start over.
   */
  gtk_widget_hide (widget);
  gtk_widget_unrealize (widget);
  gtk_widget_show (widget);

  /* Handled it, don't destroy the tray icon */
  return TRUE;
}

#if !GTK_CHECK_VERSION(2, 90, 0)
static void
egg_tray_icon_set_colormap (EggTrayIcon *icon)
{
  GdkScreen *screen = gtk_widget_get_screen (GTK_WIDGET (icon));
  GdkColormap *colormap;
  GdkVisual *visual = icon->manager_visual;
  gboolean new_colormap = FALSE;

  /* To avoid uncertainty about colormaps, _NET_SYSTEM_TRAY_VISUAL is supposed
   * to be either the screen default visual or a TrueColor visual; ignore it
   * if it is something else
   */
  if (visual && visual->type != GDK_VISUAL_TRUE_COLOR)
    visual = NULL;

  if (visual == NULL || visual == gdk_screen_get_system_visual (screen))
    colormap = gdk_screen_get_system_colormap (screen);
  else if (visual == gdk_screen_get_rgb_visual (screen))
    colormap = gdk_screen_get_rgb_colormap (screen);
#if GTK_CHECK_VERSION(2, 8, 0)
  else if (visual == gdk_screen_get_rgba_visual (screen))
    colormap = gdk_screen_get_rgba_colormap (screen);
#endif
  else
    {
      colormap = gdk_colormap_new (visual, FALSE);
      new_colormap = TRUE;
    }

  gtk_widget_set_colormap (GTK_WIDGET (icon), colormap);

  if (new_colormap)
    g_object_unref (colormap);
}
#endif

static void
egg_tray_icon_realize (GtkWidget *widget)
{
  EggTrayIcon *icon = EGG_TRAY_ICON (widget);

#if !GLIB_CHECK_VERSION(2, 13, 1)
  egg_tray_icon_realize_internal (widget);
#endif

#if !GTK_CHECK_VERSION(2, 90, 0)
  egg_tray_icon_set_colormap (icon);
#endif

  GTK_WIDGET_CLASS (parent_class)->realize (widget);
  if (icon->manager_visual_rgba)
    {
      /* Set a transparent background */
      GdkColor transparent = { 0, 0, 0, 0 }; /* Only pixel=0 matters */
      gdk_window_set_background (gtk_widget_get_window(widget), &transparent);
    }
  else
    {
      /* Set a parent-relative background pixmap */
#if GTK_CHECK_VERSION(2, 90, 0)
      gdk_window_set_background_pattern (gtk_widget_get_window(widget), NULL);
#else
      gdk_window_set_back_pixmap (gtk_widget_get_window(widget), NULL, TRUE);
#endif
    }

  if (icon->manager_window != None)
    egg_tray_icon_send_dock_request (icon);
}

static void
egg_tray_icon_style_set (GtkWidget *widget, GtkStyle *previous_style)
{
  /* The default handler resets the background according to the style. We either

   * use a transparent background or a parent-relative background and ignore the

   * style background. So, just don't chain up.
   */
}

guint
egg_tray_icon_send_message (EggTrayIcon *icon,
			    gint         timeout,
			    const gchar *message,
			    gint         len)
{
  guint stamp;
  Display *xdisplay;
  
  g_return_val_if_fail (EGG_IS_TRAY_ICON (icon), 0);
  g_return_val_if_fail (timeout >= 0, 0);
  g_return_val_if_fail (message != NULL, 0);
		     
  if (icon->manager_window == None)
    return 0;

  if (len < 0)
    len = strlen (message);

  stamp = icon->stamp++;
  
  /* Get ready to send the message */
  egg_tray_icon_send_manager_message (icon, SYSTEM_TRAY_BEGIN_MESSAGE,
				      (Window)gtk_plug_get_id (GTK_PLUG (icon)),
				      timeout, len, stamp);

  /* Now to send the actual message */
  xdisplay = GDK_DISPLAY_XDISPLAY (gtk_widget_get_display (GTK_WIDGET (icon)));
  gdk_error_trap_push ();
  while (len > 0)
    {
      XClientMessageEvent ev;

      memset (&ev, 0, sizeof (ev));
      ev.type = ClientMessage;
      ev.window = (Window)gtk_plug_get_id (GTK_PLUG (icon));
      ev.format = 8;
      ev.message_type = XInternAtom (xdisplay,
				     "_NET_SYSTEM_TRAY_MESSAGE_DATA", False);
      if (len > 20)
	{
	  memcpy (&ev.data, message, 20);
	  len -= 20;
	  message += 20;
	}
      else
	{
	  memcpy (&ev.data, message, len);
	  len = 0;
	}

      XSendEvent (xdisplay,
		  icon->manager_window, False, StructureNotifyMask, (XEvent *)&ev);
    }
  gdk_display_sync (gtk_widget_get_display (GTK_WIDGET (icon)));
  gdk_error_trap_pop ();

  return stamp;
}

void
egg_tray_icon_cancel_message (EggTrayIcon *icon,
			      guint        id)
{
  g_return_if_fail (EGG_IS_TRAY_ICON (icon));
  g_return_if_fail (id > 0);
  egg_tray_icon_send_manager_message (icon, SYSTEM_TRAY_CANCEL_MESSAGE,
				      (Window)gtk_plug_get_id (GTK_PLUG (icon)),
				      id, 0, 0);
}

EggTrayIcon *
egg_tray_icon_new_for_screen (GdkScreen *screen, const char *name)
{
  g_return_val_if_fail (GDK_IS_SCREEN (screen), NULL);

  return g_object_new (EGG_TYPE_TRAY_ICON, "screen", screen, "title", name, NULL);
}

EggTrayIcon*
egg_tray_icon_new (const gchar *name)
{
  return g_object_new (EGG_TYPE_TRAY_ICON, "title", name, NULL);
}

GtkOrientation
egg_tray_icon_get_orientation (EggTrayIcon *icon)
{
  g_return_val_if_fail (EGG_IS_TRAY_ICON (icon), GTK_ORIENTATION_HORIZONTAL);

  return icon->orientation;
}
