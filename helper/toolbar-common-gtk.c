/*

  Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
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
#include <gtk/gtk.h>
#include <string.h>
#include <stdlib.h>

#include <uim/uim.h>
#include "uim/config.h"
#include "uim/uim-helper.h"
#include "uim/gettext.h"

#define BUTTON_WIDTH  22
#define BUTTON_HEIGHT 22

static GtkWidget *hbox;
static GtkWidget *tmp_button;
static GList *menu_buttons;
static GtkWidget *prop_menu;
static GtkWidget *right_click_menu;
static GtkSizeGroup *button_size_group;

static unsigned int read_tag;
static int uim_fd;
static GtkWidget *helper_parent_widget;

static gboolean
prop_button_pressed(GtkButton *prop_button, GdkEventButton *event, GtkMenuShell *prop_menu);
static gboolean
prop_button_released(GtkButton *prop_button, GdkEventButton *event, gpointer dummy);

static GtkWidget *
switcher_button_create(void);
static void
switcher_button_pressed(GtkButton *prop_button, GdkEventButton *event, gpointer user_data);

static GtkWidget *
pref_button_create(void);
static void
pref_button_pressed(GtkButton *prop_button, GdkEventButton *event, gpointer user_data);

static void
calc_menu_position(GtkMenu *prop_menu, gint *x, gint *y, gboolean *push_in, GtkWidget *prop_button);

static void
prop_menu_activate(GtkMenu *menu_item, gpointer data);

static void
hbox_hierarchy_changed(GtkWidget *widget, GtkWidget *widget2, gpointer data);

GtkWidget *
uim_helper_toolbar_new(void);


static void
prop_menu_activate(GtkMenu *menu_item, gpointer data)
{
  GString *tmp = g_string_new((gchar*)g_object_get_data(G_OBJECT(menu_item), "prop_name"));
  g_string_prepend(tmp, "prop_activate\n");
  g_string_append(tmp, "\n");
  uim_helper_send_message(uim_fd, tmp->str);
  g_string_free(tmp, TRUE);
}

static void
button_destroy(gpointer data, gpointer user_data)
{
  GList *list = g_object_get_data(data, "prop_label");
  g_list_foreach(list, (GFunc)g_free, NULL);
  g_list_free(list);

  list = g_object_get_data(data, "prop_tooltip");
  g_list_foreach(list, (GFunc)g_free, NULL);
  g_list_free(list);

  list = g_object_get_data(data, "prop_name");
  g_list_foreach(list, (GFunc)g_free, NULL);
  g_list_free(list);

  list = g_object_get_data(data, "prop_state");
  g_list_foreach(list, (GFunc)g_free, NULL);
  g_list_free(list);

  gtk_widget_destroy(GTK_WIDGET(data));
}

static void
helper_disconnect_cb(void)
{
  uim_fd = -1;
  g_source_remove(read_tag);
}


static void
helper_applet_prop_list_update(gchar **tmp)
{
  GtkWidget *button = NULL; /* quiet gcc */
  GtkTooltips *tooltip;
  int i = 0;
  gchar **tmp2 = NULL;
  gchar *charset = NULL;

  tmp2 = g_strsplit(tmp[1], "=", 0);

  if(tmp2 && tmp2[0] && tmp2[1] && strcmp("charset", tmp2[0]) == 0) {
    charset = g_strdup(tmp2[1]);
    g_strfreev(tmp2);
  } else {
    g_strfreev(tmp2);
    return;
  }

  if(menu_buttons) {
    g_list_foreach(menu_buttons, button_destroy, NULL);
    g_list_free(menu_buttons);
    menu_buttons = NULL;
  }

  while(tmp[i] && strcmp("", tmp[i]) != 0) {
    if (charset) {
      gchar *utf8_str;
      utf8_str = g_convert(tmp[i], strlen(tmp[i]),
			   "UTF-8", charset,
			   NULL, /* gsize *bytes_read */
			   NULL, /*size *bytes_written */
			   NULL); /* GError **error*/

      tmp2 = g_strsplit(utf8_str, "\t", 0);
      g_free(utf8_str);
    } else {
      tmp2 = g_strsplit(tmp[i], "\t", 0);
    }

    if(tmp2 && tmp2[0]) 
      {
	if(strcmp("branch", tmp2[0]) == 0) {
	  if(tmp_button) {
	    gtk_widget_destroy(tmp_button); tmp_button = NULL;
	  }
	  button = gtk_button_new_with_label(tmp2[1]);
	  gtk_button_set_relief(GTK_BUTTON(button), GTK_RELIEF_NONE);
	  gtk_size_group_add_widget(button_size_group, button);
	  tooltip = gtk_tooltips_new();
	  gtk_tooltips_set_tip(tooltip, button, tmp2[2], NULL);

	  gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 0);
	  menu_buttons = g_list_append(menu_buttons, button);
	  
	  g_signal_connect(G_OBJECT(button), "button-press-event",
			   G_CALLBACK(prop_button_pressed), NULL);
	  g_signal_connect(G_OBJECT(button), "button-release-event",
			   G_CALLBACK(prop_button_released), NULL);
	} else if(strcmp("leaf", tmp2[0]) == 0) {
	  GList *label_list   = g_object_get_data(G_OBJECT(button), "prop_label");
	  GList *tooltip_list = g_object_get_data(G_OBJECT(button), "prop_tooltip");
	  GList *name_list    = g_object_get_data(G_OBJECT(button), "prop_name");

	  label_list   = g_list_append(label_list, g_strdup(tmp2[2]));
	  tooltip_list = g_list_append(tooltip_list, g_strdup(tmp2[3]));
	  name_list    = g_list_append(name_list,  g_strdup(tmp2[4]));

	  g_object_set_data(G_OBJECT(button), "prop_label",
			    label_list);
	  g_object_set_data(G_OBJECT(button), "prop_tooltip",
			    tooltip_list);
	  g_object_set_data(G_OBJECT(button), "prop_name",
			    name_list);

	  if (tmp2[5]) {
            GList *state_list = g_object_get_data(G_OBJECT(button), "prop_state");
	      
            state_list = g_list_append(state_list, g_strdup(tmp2[5]));

            g_object_set_data(G_OBJECT(button), "prop_state",
				state_list);
	  }

	}
	g_strfreev(tmp2);
      }
    i++;
  }

  /* create button for exec switcher */
  button = switcher_button_create();
  gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 0);
  menu_buttons = g_list_append(menu_buttons, button);

  button = pref_button_create();
  gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 0);
  menu_buttons = g_list_append(menu_buttons, button);

  gtk_widget_show_all(hbox);

  if(charset)
    g_free(charset);
}

static void
helper_applet_prop_label_update(gchar **lines)
{
  GtkWidget *button;
  unsigned int i = 0;
  gchar **pair = NULL;
  gchar *charset = NULL;

  if(lines && lines[1] ) {
    pair = g_strsplit(lines[1], "=", 0);
  } else {
    return;
  }

  if(pair && pair[0] && pair[1] && strcmp("charset", pair[0]) == 0) {
    charset = g_strdup(pair[1]);
    g_strfreev(pair);
  } else {
    g_strfreev(pair);
    return;
  }

  while(lines[i] && strcmp("", lines[i]) != 0) {
    i++;
  }

  if(!menu_buttons || i - 2 != g_list_length(menu_buttons)) {
    uim_helper_client_get_prop_list();
    return;
  }
  
  i = 1; /* resetting temporary variable */

  while(lines[i] && strcmp("", lines[i]) != 0) {

    if (charset) {
      gchar *utf8_str;
      utf8_str = g_convert(lines[i], strlen(lines[i]),
			   "UTF-8", charset,
			   NULL, /* gsize *bytes_read */
			   NULL, /*size *bytes_written */
			   NULL); /* GError **error*/

      pair = g_strsplit(utf8_str, "\t", 0);
      g_free(utf8_str);
    } else {
      pair = g_strsplit(lines[i], "\t", 0);
    }
    
    if(pair && pair[0] && pair[1])
      {
	button = g_list_nth_data(menu_buttons, i - 2 );
	gtk_button_set_label(GTK_BUTTON(button), pair[0]);
      }
    g_strfreev(pair);
    i++;
  }

  if(charset)
    g_free(charset);
}


static void
helper_applet_parse_helper_str(gchar *str)
{
  gchar **lines;
  lines = g_strsplit(str, "\n", 0);

  if(lines && lines[0]) {
    if( strcmp("prop_list_update", lines[0]) == 0) {
      helper_applet_prop_list_update(lines);
    } else if( strcmp("prop_label_update", lines[0]) == 0) {
      helper_applet_prop_label_update(lines);
    }
    g_strfreev(lines);
  }
}

static gboolean
uim_applet_fd_read_cb(GIOChannel *channel, GIOCondition c, gpointer p)
{
  gchar *tmp;
  int fd = g_io_channel_unix_get_fd(channel);
  
  uim_helper_read_proc(fd);
  while ((tmp = uim_helper_get_message())) {

    helper_applet_parse_helper_str(tmp);
    free(tmp); tmp = NULL;
  }
  return TRUE;
}

static void
menu_switcher_activated(GtkMenu *menu_item, gpointer data)
{
  system("uim-im-switcher &");
}

static void
menu_pref_activated(GtkMenu *menu_item, gpointer data)
{
  system("uim-pref-gtk &");
}

static void
menu_quit_activated(GtkMenu *menu_item, gpointer data)
{
  gtk_main_quit();
}

static GtkWidget *
right_click_menu_create(void)
{
  GtkWidget *menu;
  GtkWidget *menu_item;
  GtkWidget *img;
  gchar *path;

  menu = gtk_menu_new();
  path = g_strconcat(UIM_PIXMAPSDIR, "/switcher-icon.png", NULL);
  img = gtk_image_new_from_file(path);
  g_free(path);

  menu_item = gtk_image_menu_item_new_with_label("Execute uim's input method switcher.");
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_item), img);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_item);
  g_signal_connect(G_OBJECT(menu_item), "activate", 
		   G_CALLBACK(menu_switcher_activated), NULL);

  img = gtk_image_new_from_stock(GTK_STOCK_PREFERENCES, GTK_ICON_SIZE_MENU);
  menu_item = gtk_image_menu_item_new_with_label("Execute uim's preference tool.");
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_item), img);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_item);
  g_signal_connect(G_OBJECT(menu_item), "activate", 
		   G_CALLBACK(menu_pref_activated), NULL);

  img = gtk_image_new_from_stock(GTK_STOCK_QUIT, GTK_ICON_SIZE_MENU);
  menu_item = gtk_image_menu_item_new_with_label("Quit this toolbar.");
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_item), img);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_item);
  g_signal_connect(G_OBJECT(menu_item), "activate", 
		   G_CALLBACK(menu_quit_activated), NULL);


  gtk_widget_show_all(menu);

  return menu;
}

static gboolean
prop_right_button_pressed(GtkButton *prop_button, GdkEventButton *event, gpointer dummy)
{
  gtk_menu_popup(GTK_MENU(right_click_menu), NULL, NULL, 
		 (GtkMenuPositionFunc) calc_menu_position,
		 (gpointer)prop_button, event->button, 
		 gtk_get_current_event_time());
  return FALSE;
}


static gboolean
prop_button_pressed(GtkButton *prop_button, GdkEventButton *event, GtkMenuShell *prop_menu)
{ 
  if(event->button == 3) {
    prop_right_button_pressed(prop_button, event, prop_menu);
  } else if(event->button == 2) {
    if(helper_parent_widget)
      gtk_propagate_event(GTK_WIDGET(helper_parent_widget), (GdkEvent *) event);
  }
  return FALSE;
}

static void
check_helper_connection()
{
  if(uim_fd < 0) {
    uim_fd = uim_helper_init_client_fd(helper_disconnect_cb);
    if(uim_fd > 0) {
      GIOChannel *channel;
      channel = g_io_channel_unix_new(uim_fd);
      read_tag = g_io_add_watch(channel, G_IO_IN | G_IO_HUP | G_IO_ERR,
				uim_applet_fd_read_cb, NULL);
      g_io_channel_unref(channel);
    }
  }
}

static gboolean
prop_button_released(GtkButton *prop_button, GdkEventButton *event, gpointer dummy)
{
  GtkWidget *menu_item;
  GtkTooltips *tooltip;
  GList *menu_item_list;
  GList *label_list   = g_object_get_data(G_OBJECT(prop_button), "prop_label");
  GList *tooltip_list = g_object_get_data(G_OBJECT(prop_button), "prop_tooltip");
  GList *name_list    = g_object_get_data(G_OBJECT(prop_button), "prop_name");
  GList *state_list   = g_object_get_data(G_OBJECT(prop_button), "prop_state");
  gchar *label;
  gchar *flag;
  int i = 0;
  gboolean is_radio = FALSE;

  check_helper_connection();
  
  if (!event) {
    return FALSE;
  } else if (event->button == 2 || event->button == 3) {
    if(helper_parent_widget)
      gtk_propagate_event(GTK_WIDGET(helper_parent_widget), (GdkEvent *) event);
    return FALSE;
  }

  menu_item_list = gtk_container_get_children(GTK_CONTAINER(prop_menu));
 
  while((menu_item = g_list_nth_data(menu_item_list, i)) != NULL) {    
    gtk_container_remove(GTK_CONTAINER(prop_menu), menu_item);
    i++;
  }

  /* check if state_list contains state data */
  i = 0;
  while ((flag = g_list_nth_data(state_list, i)) != NULL) {
    if (strcmp("*", flag) == 0)
      is_radio = TRUE;

    i++;
  }

  i = 0;
  while ((label = g_list_nth_data(label_list, i)) != NULL) {
    if (is_radio) {
      menu_item = gtk_check_menu_item_new_with_label(label);
#if GTK_CHECK_VERSION(2, 4, 0)
      gtk_check_menu_item_set_draw_as_radio(GTK_CHECK_MENU_ITEM(menu_item), TRUE);
#endif
      flag = g_list_nth_data(state_list, i);
      if (flag && strcmp("*", flag) == 0)
        gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM(menu_item), TRUE);
    } else {
      /* no state data */
      menu_item = gtk_menu_item_new_with_label(label);
    }

    /* tooltips */
    tooltip = gtk_tooltips_new();
    gtk_tooltips_set_tip(tooltip, menu_item, g_list_nth_data(tooltip_list, i), NULL);
    
    /* add to the menu */
    gtk_menu_shell_append(GTK_MENU_SHELL(prop_menu), menu_item);
    gtk_widget_show(menu_item);
    g_signal_connect(G_OBJECT(menu_item), "activate", 
		     G_CALLBACK(prop_menu_activate), prop_menu);
    g_object_set_data_full(G_OBJECT(menu_item), "prop_name",
			   g_list_nth_data(name_list, i),
			   (GDestroyNotify)g_free);
    i++;
  }
  gtk_menu_popup(GTK_MENU(prop_menu), NULL, NULL, 
		 (GtkMenuPositionFunc) calc_menu_position,
		 (gpointer)prop_button, event->button, 
		 gtk_get_current_event_time());

  return FALSE;
}

static void
calc_menu_position(GtkMenu *prop_menu, gint *x, gint *y, gboolean *push_in, GtkWidget *prop_button)
{
  gint sc_height, sc_width, menu_width, menu_height, button_height;
  GtkRequisition requisition;
  
  g_return_if_fail(x && y);
  g_return_if_fail(GTK_IS_BUTTON(prop_button));
  
  gdk_window_get_origin(prop_button->window, x, y);
  gdk_drawable_get_size(prop_button->window, NULL, &button_height);
  
  sc_height = gdk_screen_get_height(gdk_screen_get_default ());
  sc_width  = gdk_screen_get_width (gdk_screen_get_default ());
  
  gtk_widget_size_request(GTK_WIDGET(prop_menu), &requisition);
  
  menu_width  =  requisition.width;
  menu_height =  requisition.height;
  
  
  if(*y + button_height + menu_height < sc_height){
    *y = *y + button_height;
  } else {
    *y = *y - menu_height;
  }
  
  if(*x + menu_width > sc_width){
    *x = sc_width - menu_width;
  }
}

static GtkWidget *
switcher_button_create(void)
{
  GtkWidget *button;
  GtkTooltips *tooltip;
  GtkWidget *img;
  gchar *path;
  button = gtk_button_new();
  path = g_strconcat(UIM_PIXMAPSDIR, "/switcher-icon.png", NULL);
  img = gtk_image_new_from_file(path);
  g_free(path);
  gtk_container_add(GTK_CONTAINER(button), img);
  gtk_button_set_relief(GTK_BUTTON(button), GTK_RELIEF_NONE);
  gtk_size_group_add_widget(button_size_group, button);
  g_signal_connect(G_OBJECT(button), "button_press_event",
		   G_CALLBACK(switcher_button_pressed), NULL);
  
  /* tooltip */
  tooltip = gtk_tooltips_new();
  gtk_tooltips_set_tip(tooltip, button, _("Execute uim's input method switcher."), NULL);

  return button;
}

static void
switcher_button_pressed(GtkButton *prop_button, GdkEventButton *event, gpointer user_data)
{ 
  if (event->button == 2 || event->button == 3) {
    if (helper_parent_widget)
      gtk_propagate_event(GTK_WIDGET(helper_parent_widget), (GdkEvent *) event);
    } else {
      /* exec uim-im-switcher */
      system("uim-im-switcher &");
    }
}

static GtkWidget *
pref_button_create(void)
{

  GtkWidget *button;
  GtkTooltips *tooltip;
  GtkWidget *img;
  button = gtk_button_new();
  img = gtk_image_new_from_stock(GTK_STOCK_PREFERENCES, GTK_ICON_SIZE_MENU);
  gtk_button_set_relief(GTK_BUTTON(button), GTK_RELIEF_NONE);
  gtk_size_group_add_widget(button_size_group, button);
  g_signal_connect(G_OBJECT(button), "button_press_event",
		   G_CALLBACK(pref_button_pressed), NULL);
  gtk_container_add(GTK_CONTAINER(button), img);

  /* tooltip */
  tooltip = gtk_tooltips_new();
  gtk_tooltips_set_tip(tooltip, button, _("Execute uim's preference tool."), NULL);

  return button;
}

static void
pref_button_pressed(GtkButton *prop_button, GdkEventButton *event, gpointer user_data)
{ 
  if (event->button == 2 || event->button == 3) {
    if (helper_parent_widget)
      gtk_propagate_event(GTK_WIDGET(helper_parent_widget), (GdkEvent *) event);
    } else {
      /* exec uim-pref */
      system("uim-pref-gtk &");
    }
}

static void
hbox_hierarchy_changed(GtkWidget *widget, GtkWidget *widget2, gpointer data)
{
  helper_parent_widget =  gtk_widget_get_parent(widget);
}


GtkWidget *
uim_helper_toolbar_new(void)
{
  hbox = gtk_hbox_new(FALSE, 0);
  prop_menu = gtk_menu_new();
  right_click_menu = right_click_menu_create();
  button_size_group = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
 
  g_signal_connect(G_OBJECT(hbox), "hierarchy-changed",
		   G_CALLBACK(hbox_hierarchy_changed), hbox);

  if(!menu_buttons){
    tmp_button = gtk_button_new_with_label("?");
    gtk_button_set_relief(GTK_BUTTON(tmp_button), GTK_RELIEF_NONE);
  
    g_signal_connect(G_OBJECT(tmp_button), "button_press_event",
		     G_CALLBACK(prop_button_pressed), NULL);
    
    gtk_box_pack_start(GTK_BOX(hbox), tmp_button, FALSE, FALSE, 0);
  
  }
  uim_fd = -1;
  check_helper_connection();
  uim_helper_client_get_prop_list();

  return hbox; 
}
