/*

  Copyright (c) 2004-2013 uim Project https://github.com/uim/uim

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

#include <locale.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <uim/uim-custom.h>
#include <uim/uim-scm.h>
#include "uim/gettext.h"


static unsigned int read_tag;
static int uim_fd; /* file descriptor to connect helper message bus */
static gchar *im_list_str_old; /* To compare new im_list_str */
static GtkWidget *switcher_tree_view;
static int custom_enabled;

static gboolean
reload_im_list(GtkWindow *window, gpointer user_data);
static void
parse_helper_str(const char *sent_str);
static void
parse_helper_str_im_list(const char *im_list_str_new);
static void
check_helper_connection(void);

static gint coverage;
GtkWidget *radio0, *radio1, *radio2;


/* TreeItem structure */
typedef struct _TreeItem TreeItem;
struct _TreeItem
{
  gchar    *im_name;
  gchar    *language;
  gchar    *description;
  gboolean usable;
};

enum
{
  NAME_COLUMN=0,
  LANG_COLUMN,
  DESC_COLUMN,
  NUM_COLUMNS
};

enum switcher_coverage {
  IMSW_COVERAGE_WHOLE_DESKTOP,
  IMSW_COVERAGE_THIS_APPLICATION_ONLY,
  IMSW_COVERAGE_THIS_TEXT_AREA_ONLY
};

/* Configration:
   - History of used input method (for sorting by most recently used) (list of input method name)
   - Grouped by language? (boolean)
   - Sorting way (input method, language, most recently used) (string)
 */

static char *
get_next_line(FILE *fp)
{
  char buf[1024];
  GString *line = g_string_new("");
  /* I don't want to depend on glib, but GString is too convenience... */

  while (fgets(buf, sizeof(buf), fp) != NULL) {
    g_string_append(line, buf);
    if (line->str[line->len - 1] == '\n') {
      return g_string_free(line, FALSE);
    }
  }
  return NULL;
}

static void
parse_config_line_history(const char *line)
{
  int i;
  char **splitted1 = g_strsplit(line, "=", -1);
  char **splitted2;
  if (splitted1 && splitted1[1] ) {
    splitted2 = g_strsplit(splitted1[1], ",", -1);
    g_strfreev(splitted1);
  } else {
    return;
  }
  for (i = 0 ;splitted2[i] != NULL; i++) {
  g_print("%s", splitted2[i]);
  }
    g_strfreev(splitted2);
}

static void
parse_config_line_grouped(const char *line)
{
  /* set global variable gboolean grouped */
}

static void
parse_config_line_sorting_way(const char *line)
{
  /* set global variable gchar * sorying_way */
}
static void
parse_config_line(const char *line)
{
  if (g_strrstr(line, "history=") == line) {
    parse_config_line_history(line);
  } else if (g_strrstr(line, "grouped=") == line) {
    parse_config_line_grouped(line);
  } else if (g_strrstr(line, "sorting_way=") == line) {
    parse_config_line_sorting_way(line);
  }
}

static void
load_configration(const char *filename)
{
  const char *f;
  char *line;
  FILE *fp;
  if (filename == NULL) {
    return;/* FIXME:later get correct file path here */
  } else {
    f= filename;
  }

  fp = fopen(f, "r");
  if (fp == NULL) {
    fprintf(stderr, "%s\n", strerror(errno));
    return;
  }

 /* XXX: I want you to write more beaftiful loop */
  while ((line = get_next_line(fp)) != NULL) {
    parse_config_line(line);
    free(line);
  }
  fclose(fp);

  /* open file and load config */
}

#if 0
static void
save_configration(const char *filename)
{
  /* open file and save config */
  /* make sure file permission, file owner, etc. */
}
#endif

/* Return value must be freed! */
static char *
get_selected_im_name(void)
{
  GtkTreeSelection *sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(switcher_tree_view));
  GtkTreeModel *model;
  GtkTreeIter iter;
  gchar *str_data;
  if (gtk_tree_selection_get_selected (sel, &model, &iter) == TRUE) {
    gtk_tree_model_get (model, &iter,
			NAME_COLUMN, &str_data,
			-1);
    return str_data;
  }
  return NULL;
}

static void
send_message_im_change(const gchar *type)
{
  GString *msg = g_string_new(type);
  gchar *im_name = get_selected_im_name();
  if (im_name == NULL) {
    g_string_free(msg, TRUE);
    return; /* Or should pop-up alert window here? */
  }
  check_helper_connection(); /* ensuring connected to message bus */
  g_string_append(msg, im_name);
  g_string_append(msg, "\n");
  g_free(im_name);
  uim_helper_send_message(uim_fd, msg->str);
  g_string_free(msg, TRUE);
}

static void
toggle_coverage(GtkToggleButton *togglebutton, gpointer user_data)
{
  if (gtk_toggle_button_get_active((GtkToggleButton *)radio0)) {
    coverage = IMSW_COVERAGE_WHOLE_DESKTOP;
  } else if (gtk_toggle_button_get_active((GtkToggleButton *)radio1)) {
    coverage = IMSW_COVERAGE_THIS_APPLICATION_ONLY;
  } else  if (gtk_toggle_button_get_active((GtkToggleButton *)radio2)) {
    coverage = IMSW_COVERAGE_THIS_TEXT_AREA_ONLY;
  }
}

static void
save_default_im()
{
  if (custom_enabled) {
    gchar *im_name = get_selected_im_name();

    uim_scm_callf("custom-set-value!",
		  "yy",
		  "custom-preserved-default-im-name",
		  im_name);
    uim_custom_save_custom("custom-preserved-default-im-name");
    g_free(im_name);
  }
}

static void
change_input_method(GtkButton *button, gpointer user_data)
{
  switch (coverage) {
  case IMSW_COVERAGE_WHOLE_DESKTOP:
    send_message_im_change("im_change_whole_desktop\n");
    save_default_im();
    break;
  case IMSW_COVERAGE_THIS_APPLICATION_ONLY:
    send_message_im_change("im_change_this_application_only\n");
    break;
  case IMSW_COVERAGE_THIS_TEXT_AREA_ONLY:
    send_message_im_change("im_change_this_text_area_only\n");
    break;
  }
}

static void
change_input_method_and_quit(GtkButton *button, gpointer user_data)
{
  change_input_method(button, user_data);
  gtk_main_quit();
}

static void
parse_arg(int argc, char *argv[])
{
  /* Doing nothing yet. */
}

static char *
get_error_msg(void)
{
  return "Dummy function";
}

static GtkWidget *
create_switcher_treeview(void)
{
  GtkTreeStore *tree_store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  tree_store = gtk_tree_store_new (NUM_COLUMNS,
				   G_TYPE_STRING,
				   G_TYPE_STRING,
				   G_TYPE_STRING);

  switcher_tree_view = gtk_tree_view_new();

  /* column 0 */
  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("InputMethodName"),
						    renderer,
						    "text", NAME_COLUMN,
						    (const gchar *)NULL);
  gtk_tree_view_column_set_sort_column_id(column, 0);
  gtk_tree_view_append_column(GTK_TREE_VIEW(switcher_tree_view), column);

  /* column 1 */
  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Language"),
						    renderer,
						    "text", LANG_COLUMN,
						    (const gchar *)NULL);
  gtk_tree_view_column_set_sort_column_id(column, 1);
  gtk_tree_view_append_column(GTK_TREE_VIEW(switcher_tree_view), column);

  /* column 2 */
  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Description"),
						    renderer,
						    "text", DESC_COLUMN,
						    (const gchar *)NULL);
  gtk_tree_view_column_set_sort_column_id(column, 2);
  gtk_tree_view_append_column(GTK_TREE_VIEW(switcher_tree_view), column);

  gtk_tree_view_set_model(GTK_TREE_VIEW(switcher_tree_view), GTK_TREE_MODEL(tree_store));

  g_object_unref (tree_store);
  gtk_tree_view_set_rules_hint (GTK_TREE_VIEW(switcher_tree_view), TRUE);
  /* expand all rows after the treeview widget has been realized */
  g_signal_connect (G_OBJECT(switcher_tree_view), "realize",
		    G_CALLBACK (gtk_tree_view_expand_all), NULL);

  return switcher_tree_view;
}

static int
create_switcher(void)
{
  GtkWidget *switcher_win;
  GtkWidget *scrolled_win; /* treeview container */
  GtkWidget *hbox, *vbox1, *vbox2, *vbox3;
  GtkWidget *setting_button_box;
  GtkWidget *button;
  GtkWidget *frame;
  GdkPixbuf *icon;

  switcher_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(switcher_win),
		       _("uim input method switcher"));

  icon = gdk_pixbuf_new_from_file(UIM_PIXMAPSDIR"/uim-icon.png", NULL);
  gtk_window_set_icon(GTK_WINDOW(switcher_win), icon);

  g_signal_connect(G_OBJECT(switcher_win), "destroy",
		   G_CALLBACK(gtk_main_quit), NULL);

  g_signal_connect(G_OBJECT(switcher_win), "focus-in-event",
		   G_CALLBACK(reload_im_list), NULL);

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
  vbox3 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
#else
  vbox1 = gtk_vbox_new(FALSE, 0);
  vbox3 = gtk_vbox_new(FALSE, 0);
  hbox = gtk_hbox_new(FALSE, 8);
#endif
  gtk_container_set_border_width(GTK_CONTAINER(vbox3), 4);

  scrolled_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolled_win),
				       GTK_SHADOW_ETCHED_IN);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win),
				 GTK_POLICY_AUTOMATIC,
				 GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start (GTK_BOX (hbox), scrolled_win, TRUE, TRUE, 0);

  gtk_container_add(GTK_CONTAINER(scrolled_win), create_switcher_treeview());

  gtk_box_pack_start (GTK_BOX (vbox1), hbox, TRUE, TRUE, 0);


  frame = gtk_frame_new(_("Effective coverage"));
  gtk_frame_set_label_align(GTK_FRAME(frame), 0.015, 0.5);

  gtk_box_pack_start(GTK_BOX(vbox3), frame, FALSE, FALSE, 6);
#if GTK_CHECK_VERSION(3, 2, 0)
  vbox2 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
#else
  vbox2 = gtk_vbox_new(FALSE, 8);
#endif
  gtk_container_set_border_width(GTK_CONTAINER(vbox2), 10);
  gtk_container_add(GTK_CONTAINER(frame), vbox2);

  radio0 = gtk_radio_button_new_with_label(NULL, _("whole desktop"));
  radio1 = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(radio0), _("current application only"));
  radio2 = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(radio0), _("current text area only"));

  gtk_box_pack_start(GTK_BOX(vbox2), radio0, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox2), radio1, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox2), radio2, FALSE, FALSE, 0);

  g_signal_connect(G_OBJECT(radio0), "toggled", G_CALLBACK(toggle_coverage), NULL);
  g_signal_connect(G_OBJECT(radio1), "toggled", G_CALLBACK(toggle_coverage), NULL);
  g_signal_connect(G_OBJECT(radio2), "toggled", G_CALLBACK(toggle_coverage), NULL);

  /* set radio0 (Change whole desktop) as default */
  gtk_toggle_button_set_active((GtkToggleButton *)radio0, TRUE);

#if GTK_CHECK_VERSION(3, 2, 0)
  setting_button_box = gtk_button_box_new(GTK_ORIENTATION_HORIZONTAL);
#else
  setting_button_box = gtk_hbutton_box_new();
#endif
  gtk_button_box_set_layout(GTK_BUTTON_BOX(setting_button_box), GTK_BUTTONBOX_END);
  gtk_box_set_spacing(GTK_BOX(setting_button_box), 8);

  /* Apply button */
  button = gtk_button_new_from_stock(GTK_STOCK_APPLY);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(change_input_method), NULL);
  gtk_box_pack_start(GTK_BOX(setting_button_box), button, TRUE, TRUE, 2);

  /* Cancel button */
  button = gtk_button_new_from_stock(GTK_STOCK_CLOSE);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(gtk_main_quit), NULL);
  gtk_box_pack_start(GTK_BOX(setting_button_box), button, TRUE, TRUE, 2);

  /* OK button */
  button = gtk_button_new_from_stock(GTK_STOCK_OK);
  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(change_input_method_and_quit), NULL);
  gtk_box_pack_start(GTK_BOX(setting_button_box), button, TRUE, TRUE, 2);

  gtk_box_pack_start(GTK_BOX(vbox3), setting_button_box, FALSE, FALSE, 8);
  gtk_box_pack_start(GTK_BOX(vbox1), vbox3, FALSE, FALSE, 0);
  gtk_container_add(GTK_CONTAINER(switcher_win), vbox1);

  {
    GdkScreen *scr = gtk_window_get_screen(GTK_WINDOW(switcher_win));
    gtk_window_set_default_size(GTK_WINDOW(switcher_win),
				gdk_screen_get_width(scr)  / 2,
				gdk_screen_get_height(scr) / 2);

    gtk_window_set_position(GTK_WINDOW(switcher_win),
			    GTK_WIN_POS_CENTER_ALWAYS);
  }
  gtk_widget_grab_focus(switcher_tree_view);
  gtk_widget_show_all(switcher_win);

  return 0;
}

static gboolean
reload_im_list(GtkWindow *window, gpointer user_data)
{
  check_helper_connection();
  uim_helper_send_message(uim_fd, "im_list_get\n");

  return FALSE;
}

static void
parse_helper_str(const char *sent_str)
{
  if (g_str_has_prefix(sent_str, "im_list") == TRUE) {
    parse_helper_str_im_list(sent_str);
  } else if (g_str_has_prefix(sent_str, "im_switcher_start") == TRUE) {
    uim_helper_send_message(uim_fd, "im_switcher_quit\n"); 
  } else if (g_str_has_prefix(sent_str, "im_switcher_quit") == TRUE) {
    gtk_main_quit();
  }
}

static char *
get_text(const char *str)
{
  if (strcmp("", str) == 0)
    return "-";
  else
    return gettext(str);
}

static void
parse_helper_str_im_list(const char *im_list_str_new)
{
  gchar **lines;
  int i = 2;
  gchar **info;
  GtkTreeStore *tree_store;
  GtkTreeIter iter;
  GtkTreePath *path = NULL;
  if (im_list_str_old && strcmp(im_list_str_new, im_list_str_old) == 0) {
    return; /* No need to update */
  }

  lines = g_strsplit(im_list_str_new, "\n", -1);
  tree_store = gtk_tree_store_new (NUM_COLUMNS,
				   G_TYPE_STRING,
				   G_TYPE_STRING,
				   G_TYPE_STRING);

  for (i=2; lines[i] != NULL; i++) {

    if (!lines[i] || strcmp(lines[i], "") == 0) {
      break;
    }
    info = g_strsplit(lines[i], "\t", -1);
    if (info && info[0] && info[1] && info[2]) {
      gtk_tree_store_append(tree_store, &iter, NULL/* parent iter */);
      gtk_tree_store_set(tree_store, &iter,
			 NAME_COLUMN, info[0],
			 LANG_COLUMN, get_text(info[1]),
			 DESC_COLUMN, get_text(info[2]),
			 -1);
      if (info[3] && (strcmp(info[3], "") != 0)) {
	path = gtk_tree_model_get_path(GTK_TREE_MODEL(tree_store),
				       &iter);

      }
    }
    g_strfreev(info);
  }
  gtk_tree_view_set_model(GTK_TREE_VIEW(switcher_tree_view),
			  GTK_TREE_MODEL(tree_store));

  if (path != NULL) {
    gtk_tree_view_set_cursor(GTK_TREE_VIEW(switcher_tree_view),
			     path, NULL, FALSE);
  }

  g_free(im_list_str_old); im_list_str_old = g_strdup(im_list_str_new);
  g_strfreev(lines);
}


static void
helper_disconnect_cb(void)
{
  uim_fd = -1;
  g_source_remove(read_tag);
}

static gboolean
fd_read_cb(GIOChannel *channel, GIOCondition c, gpointer p)
{
  char *tmp;
  int fd = g_io_channel_unix_get_fd(channel);

  uim_helper_read_proc(fd);
  while ((tmp = uim_helper_get_message())) {
    parse_helper_str(tmp);
    g_free(tmp); tmp = NULL;
  }
  return TRUE;
}


static void
check_helper_connection(void)
{
  if (uim_fd < 0) {
    uim_fd = uim_helper_init_client_fd(helper_disconnect_cb);
    if (uim_fd > 0) {
      GIOChannel *channel;
      channel = g_io_channel_unix_new(uim_fd);
      read_tag = g_io_add_watch(channel, G_IO_IN | G_IO_HUP | G_IO_ERR,
				fd_read_cb, NULL);
      g_io_channel_unref(channel);
    }
  }
}

int
main(int argc, char *argv[])
{
  gint result;
  setlocale(LC_ALL, "");
  bindtextdomain( PACKAGE, LOCALEDIR );
  textdomain( PACKAGE );
  bind_textdomain_codeset( PACKAGE, "UTF-8");
  parse_arg(argc, argv);

  /* connect to uim helper message bus */
  uim_fd = -1;
  check_helper_connection();

  /* To check if another uim-im-switcher exists */
  uim_helper_send_message(uim_fd, "im_switcher_start\n"); 

  /* To load input method list */
  uim_helper_send_message(uim_fd, "im_list_get\n");

  gtk_init(&argc, &argv);

  if (uim_init() < 0) {
    fprintf(stderr, "uim_init() failed.\n");
    exit(EXIT_FAILURE);
  }
  custom_enabled = uim_custom_enable();

  result = create_switcher();

  if (result == -1) {
    fprintf(stderr, "Error:%s\n", get_error_msg());
    exit(EXIT_FAILURE);
  }

  load_configration(NULL);
  gtk_main ();
  return 0;
}
