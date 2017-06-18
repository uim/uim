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

#include <config.h>

#include <gtk/gtk.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>

#include <uim/uim.h>
#include "uim/uim-helper.h"
#include "uim/uim-scm.h"
#include "uim/uim-custom.h"
#include "uim/gettext.h"

#define OBJECT_DATA_PROP_BUTTONS "PROP_BUTTONS"
#define OBJECT_DATA_TOOL_BUTTONS "TOOL_BUTTONS"
#define OBJECT_DATA_SIZE_GROUP "SIZE_GROUP"
#define OBJECT_DATA_TOOLBAR_TYPE "TOOLBAR_TYPE"
#define OBJECT_DATA_BUTTON_TYPE "BUTTON_TYPE"
#define OBJECT_DATA_COMMAND "COMMAND"

/* exported functions */
GtkWidget *uim_toolbar_standalone_new(void);
GtkWidget *uim_toolbar_trayicon_new(void);
GtkWidget *uim_toolbar_applet_new(void);
void uim_toolbar_check_helper_connection(GtkWidget *widget);
void uim_toolbar_get_im_list(void);
void uim_toolbar_launch_helper_application(const char *command);


enum {
  TYPE_STANDALONE,
  TYPE_APPLET,
  TYPE_ICON
};

enum {
  BUTTON_PROP,
  BUTTON_TOOL
};

struct _CommandEntry {
  const gchar *desc;
  const gchar *label;
  const gchar *icon;
  const gchar *command;
  const gchar *custom_button_show_symbol;
  uim_bool show_button;
};

/* FIXME! command menu and buttons should be customizable. */
static struct _CommandEntry command_entry[] = {
  {
    N_("Switch input method"),
    NULL,
    "im_switcher",
#if GTK_CHECK_VERSION(2, 90, 0)
    "uim-im-switcher-gtk3",
#else
    "uim-im-switcher-gtk",
#endif
    "toolbar-show-switcher-button?",
    UIM_FALSE
  },

  {
    N_("Preference"),
    NULL,
    GTK_STOCK_PREFERENCES,
#if GTK_CHECK_VERSION(2, 90, 0)
    "uim-pref-gtk3",
#else
    "uim-pref-gtk",
#endif
    "toolbar-show-pref-button?",
    UIM_FALSE
  },

  {
    N_("Japanese dictionary editor"),
    NULL,
    "uim-dict",
#if GTK_CHECK_VERSION(2, 90, 0)
    "uim-dict-gtk3",
#else
    "uim-dict-gtk",
#endif
    "toolbar-show-dict-button?",
    UIM_FALSE
  },

  {
    N_("Input pad"),
    NULL,
    GTK_STOCK_BOLD,
#if GTK_CHECK_VERSION(2, 90, 0)
    "uim-input-pad-ja-gtk3",
#else
    "uim-input-pad-ja",
#endif
    "toolbar-show-input-pad-button?",
    UIM_FALSE
  },

  {
    N_("Handwriting input pad"),
    "H",
#if GTK_CHECK_VERSION(2, 6, 0)
    GTK_STOCK_EDIT,
#else
    NULL,
#endif
    "uim-tomoe-gtk",
    "toolbar-show-handwriting-input-pad-button?",
    UIM_FALSE
  },

  {
    N_("Help"),
    NULL,
    GTK_STOCK_HELP,
    "uim-help",
    "toolbar-show-help-button?",
    UIM_FALSE
  }
};

static guint command_entry_len = sizeof(command_entry) / sizeof(struct _CommandEntry);

static GtkWidget *im_menu;
static GtkWidget *prop_menu;
static GtkWidget *right_click_menu;
static unsigned int read_tag;
static int uim_fd;
static GtkIconFactory *uim_factory;
static GList *uim_icon_list;
static gboolean prop_menu_showing = FALSE;
static gboolean custom_enabled;
static gboolean with_dark_bg;

static void set_button_style(GtkWidget *button, gint type);
static const char *safe_gettext(const char *msgid);
static gboolean has_n_strs(gchar **str_list, guint n);
static gboolean register_icon(const gchar *name);
static void reset_icon(void);

static void
set_button_style(GtkWidget *button, gint type)
{
#if GTK_CHECK_VERSION(2, 90, 0)
    GtkStyleContext *context = gtk_widget_get_style_context(button);
    GtkCssProvider *provider = gtk_css_provider_new();
    switch (type) {
    case TYPE_ICON:
        gtk_css_provider_load_from_data(provider,
                                        "#uim-systray-button {\n"
                                        " -GtkWidget-focus-line-width: 0;\n"
                                        " -GtkWidget-focus-padding: 0;\n"
                                        " padding-top: 0;\n"
                                        " padding-bottom: 0;\n"
                                        " padding-left: 2px;\n"
                                        " padding-right: 2px;\n"
                                        "}\n", -1, NULL);
	break;
    case TYPE_STANDALONE:
        gtk_css_provider_load_from_data(provider,
                                        "#uim-toolbar-button {\n"
                                        " padding-left: 5px;\n"
                                        " padding-right: 5px;\n"
                                        "}\n", -1, NULL);
	break;
    case TYPE_APPLET:
         gtk_css_provider_load_from_data(provider,
                                        "#uim-applet-button {\n"
                                        " padding-left: 2px;\n"
                                        " padding-right: 2px;\n"
                                        "}\n", -1, NULL);
	break;
   }
    gtk_style_context_add_provider(context,
                                   GTK_STYLE_PROVIDER(provider),
                                   GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
    g_object_unref(provider);
#endif
    switch (type) {
    case TYPE_ICON:
        gtk_widget_set_name(button, "uim-systray-button");
	break;
    case TYPE_STANDALONE:
        gtk_widget_set_name(button, "uim-toolbar-button");
	break;
    case TYPE_APPLET:
        gtk_widget_set_name(button, "uim-applet-button");
	break;
    }
}

static const char *
safe_gettext(const char *msgid)
{
  const char *p;

  for (p = msgid; *p && isascii(*p); p++)
    continue;

  return (*p) ? msgid : gettext(msgid);
}

static gboolean
has_n_strs(gchar **str_list, guint n)
{
  guint i;

  if (!str_list)
    return FALSE;

  for (i = 0; i < n; i++) {
    if (!str_list[i])
      return FALSE;
  }

  return TRUE;
}

static void
calc_menu_position(GtkMenu *menu, gint *x, gint *y, gboolean *push_in,
		   GtkWidget *button)
{
  gint sc_height, sc_width, menu_width, menu_height, button_height;
  GtkRequisition requisition;

  g_return_if_fail(x && y);
  g_return_if_fail(GTK_IS_BUTTON(button));

  gdk_window_get_origin(gtk_widget_get_window(button), x, y);
#if GTK_CHECK_VERSION(2, 90, 0)
  button_height = gdk_window_get_height(gtk_widget_get_window(button));
#else
  gdk_drawable_get_size(gtk_widget_get_window(button), NULL, &button_height);
#endif

#if GTK_CHECK_VERSION(2, 18, 0)
  if (!gtk_widget_get_has_window(button)) {
    GtkAllocation allocation;
    gtk_widget_get_allocation(button, &allocation);
    *x += allocation.x;
  }
#else
  if (GTK_WIDGET_NO_WINDOW(button))
    *x += button->allocation.x;
#endif

  sc_height = gdk_screen_get_height(gdk_screen_get_default());
  sc_width = gdk_screen_get_width(gdk_screen_get_default());

#if GTK_CHECK_VERSION(3, 0, 0)
  gtk_widget_get_preferred_size(GTK_WIDGET(menu), &requisition, NULL);
#else
  gtk_widget_size_request(GTK_WIDGET(menu), &requisition);
#endif

  menu_width = requisition.width;
  menu_height = requisition.height;

  if (*y + button_height + menu_height < sc_height)
    *y = *y + button_height;
  else {
    if (*y + button_height < sc_height / 2)
      *y = *y + button_height;
    else
      *y = *y - menu_height;
  }

  if (*x + menu_width > sc_width)
    *x = sc_width - menu_width;
}

static void
right_click_menu_quit_activated(GtkMenu *menu_item, gpointer data)
{
  gtk_main_quit();
  uim_quit();
}

void
uim_toolbar_launch_helper_application(const char *command)
{
  if (command) {
    if (!g_spawn_command_line_async(command, NULL)) {
      GtkWidget *dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL,
          GTK_MESSAGE_WARNING, GTK_BUTTONS_OK,
          _("Cannot launch '%s'."), command);
      gtk_dialog_run(GTK_DIALOG(dialog));
      gtk_widget_destroy(GTK_WIDGET(dialog));
    }
  }
}

static void
right_click_menu_activated(GtkMenu *menu_item, gpointer data)
{
  const char *command = data;
  uim_toolbar_launch_helper_application(command);
}

static gboolean
right_button_pressed(GtkButton *button, GdkEventButton *event, gpointer data)
{
  gtk_menu_popup(GTK_MENU(right_click_menu), NULL, NULL,
		 (GtkMenuPositionFunc)calc_menu_position,
		 (gpointer)button, event->button,
		 gtk_get_current_event_time());

  return FALSE;
}

static void
save_default_im_internal(const char *im)
{
  uim_scm_callf("custom-set-value!",
		"yy",
		"custom-preserved-default-im-name",
		im);
  uim_custom_save_custom("custom-preserved-default-im-name");
}

static void
save_default_im(const char *im)
{
  if (custom_enabled)
    uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)save_default_im_internal, (void *)im);
}

static gboolean
is_msg_imsw(const gchar *str)
{
  return g_str_has_prefix(str, "action_imsw_");
}

static gboolean
is_imsw_coverage_system_global()
{
  char *coverage;
  gboolean ret;

  coverage = uim_scm_symbol_value_str("imsw-coverage");

  ret =  (gboolean)!strcmp(coverage, "system-global");
  free(coverage);

  return ret;
}

static const char*
get_imsw_im(const gchar *str)
{
  /* skip "action_imsw_" */
  return str + strlen("action_imsw_");
}

static void
prop_menu_activate(GtkMenu *menu_item, gpointer data)
{
  GString *msg;
  const gchar *str;

  str = g_object_get_data(G_OBJECT(menu_item), "prop_action");
  msg = g_string_new(str);
  g_string_prepend(msg, "prop_activate\n");
  g_string_append(msg, "\n");
  uim_helper_send_message(uim_fd, msg->str);
  if (is_msg_imsw(str) && is_imsw_coverage_system_global()) {
    const char *im = get_imsw_im(str);
    save_default_im(im);
  }

  g_string_free(msg, TRUE);
}

static gboolean
prop_menu_shell_deactivate(GtkMenuShell *menu_shell, gpointer data)
{
  prop_menu_showing = FALSE;

  return FALSE;
}

static void
popup_prop_menu(GtkButton *prop_button, GdkEventButton *event,
		GtkWidget *widget)
{
  GtkWidget *menu_item, *hbox, *label, *img;
  GList *menu_item_list, *icon_list, *label_list, *tooltip_list, *action_list,
	*state_list, *list;
  int i, selected = -1;

  uim_toolbar_check_helper_connection(widget);

  menu_item_list = gtk_container_get_children(GTK_CONTAINER(prop_menu));
  icon_list = g_object_get_data(G_OBJECT(prop_button), "prop_icon");
  label_list = g_object_get_data(G_OBJECT(prop_button), "prop_label");
  tooltip_list = g_object_get_data(G_OBJECT(prop_button), "prop_tooltip");
  action_list = g_object_get_data(G_OBJECT(prop_button), "prop_action");
  state_list = g_object_get_data(G_OBJECT(prop_button), "prop_state");

  list = menu_item_list;
  while (list) {
    gtk_widget_destroy(list->data);
    list = list->next;
  }
  g_list_free(menu_item_list);

  gtk_widget_destroy(prop_menu);
  prop_menu = gtk_menu_new();

  /* check selected item */
  i = 0;
  while (state_list) {
    if (!strcmp("*", state_list->data)) {
      selected = i;
      break;
    }
    state_list = state_list->next;
    i++;
  }

  i = 0;
  while (label_list) {
    if (selected != -1) {
      menu_item = gtk_check_menu_item_new();
      label = gtk_label_new(label_list->data);
#if GTK_CHECK_VERSION(3, 2, 0)
      hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
#else
      hbox = gtk_hbox_new(FALSE, 0);
#endif
#if GTK_CHECK_VERSION(2, 4, 0)
      gtk_check_menu_item_set_draw_as_radio(GTK_CHECK_MENU_ITEM(menu_item),
					    TRUE);
#endif
      if (register_icon(icon_list->data))
	img = gtk_image_new_from_stock(icon_list->data, GTK_ICON_SIZE_MENU);
      else
	img = gtk_image_new_from_stock("null", GTK_ICON_SIZE_MENU);
      if (img) {
	gtk_box_pack_start(GTK_BOX(hbox), img, FALSE, FALSE, 3);
	gtk_widget_show(img);
      }
      gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 3);
      gtk_container_add(GTK_CONTAINER(menu_item), hbox);
      gtk_widget_show(label);
      gtk_widget_show(hbox);
      if (i == selected)
	gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu_item), TRUE);
    } else {
      menu_item = gtk_image_menu_item_new_with_label(label_list->data);
      if (register_icon(icon_list->data)) {
	img = gtk_image_new_from_stock(icon_list->data, GTK_ICON_SIZE_MENU);
	gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_item), img); 
#if GTK_CHECK_VERSION(2, 16, 0)
	gtk_image_menu_item_set_always_show_image(GTK_IMAGE_MENU_ITEM(menu_item), TRUE);
#endif
      }
    }

    /* tooltips */
    gtk_widget_set_tooltip_text(menu_item,
			 tooltip_list ? tooltip_list->data : NULL);

    /* add to the menu */
    gtk_menu_shell_append(GTK_MENU_SHELL(prop_menu), menu_item);

    gtk_widget_show(menu_item);
    g_signal_connect(G_OBJECT(menu_item), "activate",
		     G_CALLBACK(prop_menu_activate), prop_menu);
    g_object_set_data(G_OBJECT(menu_item), "prop_action",
		      action_list? action_list->data : NULL);
    label_list = label_list->next;
    if (icon_list)
      icon_list = icon_list->next;
    if (action_list)
      action_list = action_list->next;
    if (tooltip_list)
      tooltip_list = tooltip_list->next;
    i++;
  }

  g_signal_connect(G_OBJECT(GTK_MENU_SHELL(prop_menu)), "deactivate",
		   G_CALLBACK(prop_menu_shell_deactivate), NULL);

  gtk_menu_popup(GTK_MENU(prop_menu), NULL, NULL,
		 (GtkMenuPositionFunc)calc_menu_position,
		 (gpointer)prop_button, event->button,
		 gtk_get_current_event_time());
  prop_menu_showing = TRUE;
}

static gboolean
button_pressed(GtkButton *button, GdkEventButton *event, GtkWidget *widget)
{
  gint toolbar_type, button_type;

  switch (event->button) {
  case 3:
    toolbar_type = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget),
			    OBJECT_DATA_TOOLBAR_TYPE));
    if (toolbar_type == TYPE_APPLET)
      gtk_propagate_event(gtk_widget_get_parent(GTK_WIDGET(button)),
		      				(GdkEvent *)event);
    else
      right_button_pressed(button, event, widget);
    break;
  case 2:
    gtk_propagate_event(gtk_widget_get_parent(GTK_WIDGET(button)),
			(GdkEvent *)event);
    break;
  case 1:
  default:
    button_type = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(button),
			    OBJECT_DATA_BUTTON_TYPE));
    if (button_type == BUTTON_PROP)
      popup_prop_menu(button, event, widget);
    break;
  }

  return FALSE;
}

static gboolean
prop_button_released(GtkButton *button, GdkEventButton *event,
		     GtkWidget *widget)
{
  switch (event->button) {
  case 2:
  case 3:
    gtk_propagate_event(gtk_widget_get_parent(GTK_WIDGET(button)),
			(GdkEvent *)event);
    break;
  default:
    break;
  }

  return FALSE;
}

static void
tool_button_clicked_cb(GtkButton *tool_button, GtkWidget *widget)
{
  const gchar *command;

  command = g_object_get_data(G_OBJECT(tool_button), OBJECT_DATA_COMMAND);
  if (command)
    if (!g_spawn_command_line_async(command, NULL)) {
      GtkWidget *dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL,
          GTK_MESSAGE_WARNING, GTK_BUTTONS_OK,
          _("Cannot launch '%s'."), command);
      gtk_dialog_run(GTK_DIALOG(dialog));
      gtk_widget_destroy(GTK_WIDGET(dialog));
    }
}


static void
list_data_free(GList *list)
{
  g_list_foreach(list, (GFunc)g_free, NULL);
  g_list_free(list);
}

static void
prop_data_flush(gpointer data)
{
  GList *list;
  list = g_object_get_data(data, "prop_icon");
  list_data_free(list);
  list = g_object_get_data(data, "prop_label");
  list_data_free(list);
  list = g_object_get_data(data, "prop_tooltip");
  list_data_free(list);
  list = g_object_get_data(data, "prop_action");
  list_data_free(list);
  list = g_object_get_data(data, "prop_state");
  list_data_free(list);

  g_object_set_data(G_OBJECT(data), "prop_icon", NULL);
  g_object_set_data(G_OBJECT(data), "prop_label", NULL);
  g_object_set_data(G_OBJECT(data), "prop_tooltip", NULL);
  g_object_set_data(G_OBJECT(data), "prop_action", NULL);
  g_object_set_data(G_OBJECT(data), "prop_state", NULL);
}

static void
prop_button_destroy(gpointer data, gpointer user_data)
{
  prop_data_flush(data);
  gtk_widget_destroy(GTK_WIDGET(data));
}

static void
tool_button_destroy(gpointer data, gpointer user_data)
{
  gtk_widget_destroy(GTK_WIDGET(data));
}

static GtkWidget*
button_create(GtkWidget *widget, GtkSizeGroup *sg, const gchar *icon_name,
              const gchar *label, gint type)
{
  GtkWidget *button;

  if (register_icon(icon_name)) {
    GtkWidget *img = gtk_image_new_from_stock(icon_name, GTK_ICON_SIZE_MENU);
    button = gtk_button_new();
    gtk_container_add(GTK_CONTAINER(button), img);
  } else {
    button = gtk_button_new_with_label(label);
  }

  set_button_style(button, type);

  gtk_button_set_relief(GTK_BUTTON(button), GTK_RELIEF_NONE);
  gtk_size_group_add_widget(sg, button);
  g_object_set_data(G_OBJECT(button), OBJECT_DATA_BUTTON_TYPE,
		    GINT_TO_POINTER(BUTTON_PROP));

  g_signal_connect(G_OBJECT(button), "button-press-event",
		   G_CALLBACK(button_pressed), widget);

  return button;
}

static GtkWidget *
prop_button_create(GtkWidget *widget, const gchar *icon_name,
		   const gchar *label, const gchar *tip_text)
{
  GtkWidget *button;
  GtkSizeGroup *sg;
  gint type;

  sg = g_object_get_data(G_OBJECT(widget), OBJECT_DATA_SIZE_GROUP);
  type = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget),
		         OBJECT_DATA_TOOLBAR_TYPE));

  button = button_create(widget, sg, icon_name, label, type);

  gtk_widget_set_tooltip_text(button, tip_text);

  g_signal_connect(G_OBJECT(button), "button-release-event",
		   G_CALLBACK(prop_button_released), widget);

  return button;
}

static void
prop_button_append_menu(GtkWidget *button,
			const gchar *icon_name,
			const gchar *label, const gchar *tooltip,
			const gchar *action, const gchar *state)
{
  GList *icon_list, *label_list, *tooltip_list, *action_list, *state_list;

  icon_list = g_object_get_data(G_OBJECT(button), "prop_icon");
  label_list = g_object_get_data(G_OBJECT(button), "prop_label");
  tooltip_list = g_object_get_data(G_OBJECT(button), "prop_tooltip");
  action_list = g_object_get_data(G_OBJECT(button), "prop_action");
  state_list = g_object_get_data(G_OBJECT(button), "prop_state");

  icon_list = g_list_append(icon_list, g_strdup(icon_name));
  label_list = g_list_append(label_list, g_strdup(label));
  tooltip_list = g_list_append(tooltip_list, g_strdup(tooltip));
  action_list = g_list_append(action_list, g_strdup(action));
  state_list = g_list_append(state_list, g_strdup(state));

  g_object_set_data(G_OBJECT(button), "prop_icon", icon_list);
  g_object_set_data(G_OBJECT(button), "prop_label", label_list);
  g_object_set_data(G_OBJECT(button), "prop_tooltip", tooltip_list);
  g_object_set_data(G_OBJECT(button), "prop_action", action_list);
  g_object_set_data(G_OBJECT(button), "prop_state", state_list);
}

static void
append_prop_button(GtkWidget *hbox, GtkWidget *button)
{
  GList *prop_buttons;

  if (button) {
    gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 0);

    prop_buttons = g_object_get_data(G_OBJECT(hbox), OBJECT_DATA_PROP_BUTTONS);
    prop_buttons = g_list_append(prop_buttons, button);
    g_object_set_data(G_OBJECT(hbox), OBJECT_DATA_PROP_BUTTONS, prop_buttons);
  }
}

static void
append_tool_button(GtkWidget *hbox, GtkWidget *button)
{
  GList *tool_buttons;

  if (button) {
    gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, TRUE, 0);

    tool_buttons = g_object_get_data(G_OBJECT(hbox), OBJECT_DATA_TOOL_BUTTONS);
    tool_buttons = g_list_append(tool_buttons, button);
    g_object_set_data(G_OBJECT(hbox), OBJECT_DATA_TOOL_BUTTONS, tool_buttons);
  }
}


static gchar *
get_charset(gchar *line)
{
  gchar **tokens;
  gchar *charset = NULL;

  tokens = g_strsplit(line, "=", 0);
  if (tokens && tokens[0] && tokens[1] && !strcmp("charset", tokens[0]))
    charset = g_strdup(tokens[1]);
  g_strfreev(tokens);

  return charset;
}

static gchar *
convert_charset(const gchar *charset, const gchar *str)
{
  if (!charset)
    return NULL;

  return g_convert(str, strlen(str),
		   "UTF-8", charset,
		   NULL, /* gsize *bytes_read */
		   NULL, /* size *bytes_written */
		   NULL); /* GError **error */
}

static void
helper_toolbar_prop_list_update(GtkWidget *widget, gchar **lines)
{
  GtkWidget *button = NULL;
  guint i;
  gchar **cols;
  gchar *charset;
  const gchar *indication_id, *iconic_label, *label, *tooltip_str;
  const gchar *action_id, *is_selected;
  GList *prop_buttons, *tool_buttons;
  GtkSizeGroup *sg;
  char *display_time;
  gboolean is_hidden;
  GtkWidget *toplevel;

  if (prop_menu_showing)
    return;

  charset = get_charset(lines[1]);

  prop_buttons = g_object_get_data(G_OBJECT(widget), OBJECT_DATA_PROP_BUTTONS);
  tool_buttons = g_object_get_data(G_OBJECT(widget), OBJECT_DATA_TOOL_BUTTONS);
  sg  = g_object_get_data(G_OBJECT(widget), OBJECT_DATA_SIZE_GROUP);

  if (prop_buttons) {
    g_list_foreach(prop_buttons, prop_button_destroy, NULL);
    g_list_free(prop_buttons);
    g_object_set_data(G_OBJECT(widget), OBJECT_DATA_PROP_BUTTONS, NULL);
  }

  if (tool_buttons) {
    g_list_foreach(tool_buttons, tool_button_destroy, NULL);
    g_list_free(tool_buttons);
    g_object_set_data(G_OBJECT(widget), OBJECT_DATA_TOOL_BUTTONS, NULL);
  }

  display_time
        = uim_scm_c_symbol( uim_scm_symbol_value( "toolbar-display-time" ) );
  is_hidden = strcmp(display_time, "mode");
  for (i = 0; lines[i] && strcmp("", lines[i]); i++) {
    gchar *utf8_str = convert_charset(charset, lines[i]);

    if (utf8_str != NULL) {
      cols = g_strsplit(utf8_str, "\t", 0);
      g_free(utf8_str);
    } else {
      cols = g_strsplit(lines[i], "\t", 0);
    }

    if (cols && cols[0]) {
      if (!strcmp("branch", cols[0]) && has_n_strs(cols, 4)) {
	indication_id = cols[1];
	iconic_label  = safe_gettext(cols[2]);
	tooltip_str   = safe_gettext(cols[3]);
	button = prop_button_create(widget,
				    indication_id, iconic_label, tooltip_str);
	append_prop_button(widget, button);

        if (!is_hidden && (!strcmp(indication_id, "direct")
            || g_str_has_suffix(indication_id, "_direct"))) {
          is_hidden = TRUE;
        }
      } else if (!strcmp("leaf", cols[0]) && has_n_strs(cols, 7)) {
	indication_id = cols[1];
	iconic_label  = safe_gettext(cols[2]);
	label         = safe_gettext(cols[3]);
	tooltip_str   = safe_gettext(cols[4]);
	action_id     = cols[5];
	is_selected   = cols[6];
	prop_button_append_menu(button,
				indication_id, label, tooltip_str, action_id,
				is_selected);
      }
      g_strfreev(cols);
    }
  }
  toplevel = gtk_widget_get_toplevel(widget);
  is_hidden = (is_hidden && strcmp(display_time, "always"));
#if GTK_CHECK_VERSION(2, 18, 0)
  if (gtk_widget_get_visible(toplevel) == is_hidden) {
#else
  if (GTK_WIDGET_VISIBLE(toplevel) == is_hidden) {
#endif
    if (is_hidden) {
      gtk_widget_hide(toplevel);
    } else {
      gint x = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(toplevel),
                                                 "position_x"));
      gint y = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(toplevel),
                                                 "position_y"));
      gtk_window_move(GTK_WINDOW(toplevel), x, y);
      gtk_widget_show(toplevel);
    }
  }

  /* create tool buttons */
  /* FIXME! command menu and buttons should be customizable. */
  for (i = 0; i < command_entry_len; i++) {
    GtkWidget *tool_button;
    GtkWidget *img;

    if (!command_entry[i].show_button)
      continue;

    tool_button = gtk_button_new();

    set_button_style(tool_button,
                     GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget),
                                     OBJECT_DATA_TOOLBAR_TYPE)));

    g_object_set_data(G_OBJECT(tool_button), OBJECT_DATA_BUTTON_TYPE,
		      GINT_TO_POINTER(BUTTON_TOOL));
    g_object_set_data(G_OBJECT(tool_button), OBJECT_DATA_COMMAND,
		      (gpointer)command_entry[i].command);
    if (command_entry[i].icon)
      img = gtk_image_new_from_stock(command_entry[i].icon,
				     GTK_ICON_SIZE_MENU);
    else {
      img = gtk_label_new("");
      gtk_label_set_markup(GTK_LABEL(img), command_entry[i].label);
    }
    if (img)
      gtk_container_add(GTK_CONTAINER(tool_button), img);
    gtk_button_set_relief(GTK_BUTTON(tool_button), GTK_RELIEF_NONE);
    gtk_size_group_add_widget(sg, tool_button);
    g_signal_connect(G_OBJECT(tool_button), "button-press-event",
		     G_CALLBACK(button_pressed), widget);
    g_signal_connect(G_OBJECT(tool_button), "clicked",
		     G_CALLBACK(tool_button_clicked_cb), widget);

    /* tooltip */
    gtk_widget_set_tooltip_text(tool_button, _(command_entry[i].desc));

    append_tool_button(widget, tool_button);
  }

  gtk_widget_show_all(widget);
  g_free(charset);
}

static void
helper_toolbar_check_custom()
{
  guint i;

  for (i = 0; i < command_entry_len; i++)
    command_entry[i].show_button =
      uim_scm_symbol_value_bool(command_entry[i].custom_button_show_symbol);

  with_dark_bg =
    uim_scm_symbol_value_bool("toolbar-icon-for-dark-background?");
}

static void
helper_toolbar_parse_helper_str(GtkWidget *widget, gchar *str)
{
  gchar **lines;
  lines = g_strsplit(str, "\n", 0);

  if (lines && lines[0]) {
    if (!strcmp("prop_list_update", lines[0]))
      helper_toolbar_prop_list_update(widget, lines);
    else if (!strcmp("custom_reload_notify", lines[0])) {
      uim_prop_reload_configs();
      helper_toolbar_check_custom();
      reset_icon();
    }
    g_strfreev(lines);
  }
}

static gboolean
fd_read_cb(GIOChannel *channel, GIOCondition c, gpointer p)
{
  gchar *msg;
  int fd = g_io_channel_unix_get_fd(channel);
  GtkWidget *widget = GTK_WIDGET(p);

  uim_helper_read_proc(fd);

  while ((msg = uim_helper_get_message())) {
    helper_toolbar_parse_helper_str(widget, msg);
    free(msg);
  }

  return TRUE;
}

static void
helper_disconnect_cb(void)
{
  uim_fd = -1;
  g_source_remove(read_tag);
}

void
uim_toolbar_get_im_list(void)
{
  uim_helper_send_message(uim_fd, "im_list_get\n");
}

void
uim_toolbar_check_helper_connection(GtkWidget *widget)
{
  if (uim_fd < 0) {
    uim_fd = uim_helper_init_client_fd(helper_disconnect_cb);
    if (uim_fd > 0) {
      GIOChannel *channel;
      channel = g_io_channel_unix_new(uim_fd);
      read_tag = g_io_add_watch(channel, G_IO_IN | G_IO_HUP | G_IO_ERR,
				fd_read_cb, (gpointer)widget);
      g_io_channel_unref(channel);
    }
  }
}

static GtkWidget *
right_click_menu_create(void)
{
  GtkWidget *menu;
  GtkWidget *menu_item;
  GtkWidget *img;
  guint i;

  menu = gtk_menu_new();

  /* FIXME! command menu and buttons should be customizable. */
  for (i = 0; i < command_entry_len; i++) {
    menu_item = gtk_image_menu_item_new_with_label(_(command_entry[i].desc));

    if (command_entry[i].icon) {
      img = gtk_image_new_from_stock(command_entry[i].icon,
		      		     GTK_ICON_SIZE_MENU);
      if (img)
	gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_item), img);
    }

    gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_item);
    g_signal_connect(G_OBJECT(menu_item), "activate",
		     G_CALLBACK(right_click_menu_activated),
		     (gpointer)command_entry[i].command);
  }

  /* Add quit item */
  img = gtk_image_new_from_stock(GTK_STOCK_QUIT, GTK_ICON_SIZE_MENU);
  menu_item = gtk_image_menu_item_new_with_label(_("Quit this toolbar"));
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_item), img);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_item);
  g_signal_connect(G_OBJECT(menu_item), "activate",
		   G_CALLBACK(right_click_menu_quit_activated), NULL);

  gtk_widget_show_all(menu);

  return menu;
}

static gboolean
is_icon_registered(const gchar *name)
{
  GList *list;

  list = uim_icon_list;
  while (list) {
   if (!strcmp(list->data, name))
     return TRUE;
   list = list->next;
  }

  return FALSE;
}

static gboolean
register_icon(const gchar *name)
{
  GtkIconSet *icon_set;
  GdkPixbuf *pixbuf;
  GString *filename;
  struct stat st;

  g_return_val_if_fail(uim_factory, FALSE);

  if (is_icon_registered(name))
    return TRUE;

  filename = g_string_new(UIM_PIXMAPSDIR "/");
  g_string_append(filename, name);
  if (with_dark_bg) {
    g_string_append(filename, "_dark_background");
  }
  g_string_append(filename, ".png");

  if (with_dark_bg && stat(filename->str, &st) == -1) {
    g_string_free(filename, TRUE);
    filename = g_string_new(UIM_PIXMAPSDIR "/");
    g_string_append(filename, name);
    g_string_append(filename, ".png");
  }

  pixbuf = gdk_pixbuf_new_from_file(filename->str, NULL);
  if (!pixbuf) {
    g_string_free(filename, TRUE);
    return FALSE;
  }

  icon_set = gtk_icon_set_new_from_pixbuf(pixbuf);
  gtk_icon_factory_add(uim_factory, name, icon_set);

  uim_icon_list = g_list_append(uim_icon_list, g_strdup(name));

  g_string_free(filename, TRUE);
  gtk_icon_set_unref(icon_set);
  g_object_unref(G_OBJECT(pixbuf));

  return TRUE;
}

static void
init_icon(void)
{
  if (uim_factory)
    return;

  uim_factory = gtk_icon_factory_new();
  gtk_icon_factory_add_default(uim_factory);

  register_icon("im_switcher");
  register_icon("uim-icon");
  register_icon("uim-dict");
  register_icon("null");
}

static void
reset_icon(void)
{
  g_list_foreach(uim_icon_list, (GFunc)g_free, NULL);
  g_list_free(uim_icon_list);
  uim_icon_list = NULL;

  if (GTK_IS_ICON_FACTORY(uim_factory)) {
    gtk_icon_factory_remove_default(uim_factory);
    uim_factory = NULL;
    init_icon();
  }
}


static GtkWidget *
toolbar_new(gint type)
{
  GtkWidget *button;
  GtkWidget *hbox;
  GList *prop_buttons = NULL;
  GtkSizeGroup *sg;

  /*
   * Set uim-toolbar-save-default-im? #t in ~/.uim enable this if you'd like to
   * save default IM into ~/.uim.d/custom/custom-global.scm upon system global
   * IM switch.  However, using uim-custom consumes quite amount of memory, and
   * requires additional startup time.
   */
  if (uim_scm_symbol_value_bool("uim-toolbar-save-default-im?"))
    custom_enabled = (gboolean)uim_custom_enable();

  helper_toolbar_check_custom();
  init_icon();

  /* create widgets */
#if GTK_CHECK_VERSION(3, 2, 0)
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
#else
  hbox = gtk_hbox_new(FALSE, 0);
#endif

  im_menu = gtk_menu_new();
  prop_menu = gtk_menu_new();
  right_click_menu = right_click_menu_create();
  sg = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);

  /* prop menu button */
  button = button_create(hbox, sg, "uim-icon", " x", type);

  gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);

  prop_buttons = g_list_append(prop_buttons, button);

  g_object_set_data(G_OBJECT(hbox), OBJECT_DATA_PROP_BUTTONS, prop_buttons);
  g_object_set_data(G_OBJECT(hbox), OBJECT_DATA_SIZE_GROUP, sg);
  g_object_set_data(G_OBJECT(hbox), OBJECT_DATA_TOOLBAR_TYPE,
		    GINT_TO_POINTER(type));

  uim_fd = -1;

  if (type != TYPE_ICON) {
    /* delay initialization until getting "embedded" signal */
    uim_toolbar_check_helper_connection(hbox);
    uim_helper_client_get_prop_list();
    uim_toolbar_get_im_list();
  }

  return hbox;
}

GtkWidget *
uim_toolbar_standalone_new(void)
{
  return toolbar_new(TYPE_STANDALONE);
}

GtkWidget *
uim_toolbar_applet_new(void)
{
  return toolbar_new(TYPE_APPLET);
}

GtkWidget *
uim_toolbar_trayicon_new(void)
{
#if !GTK_CHECK_VERSION(2, 90, 0)
  gtk_rc_parse_string("\n"
		      "   style \"uim-systray-button-style\"\n"
		      "   {\n"
		      "      GtkWidget::focus-line-width=0\n"
		      "      GtkWidget::focus-padding=0\n"
		      "      ythickness=0\n"
		      "   }\n" "\n"
		      "    widget \"*.uim-systray-button\" style \"uim-systray-button-style\"\n"
		      "\n");
#endif

  return toolbar_new(TYPE_ICON);
}
