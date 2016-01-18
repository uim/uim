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

#include <stdlib.h>
# include <gtk/gtk.h>
#if GTK_CHECK_VERSION(2, 90, 0)
# include <gdk/gdkkeysyms-compat.h>
#else
# include <gdk/gdkkeysyms.h>
#endif

#include "gettext.h"

#include "word.h"
#include "anthy.h"
#include "word-list-view-gtk.h"
#include "word-list-win-gtk.h"
#include "word-win-gtk.h"

static void word_list_window_class_init    (WordListWindowClass *window);
static void word_list_window_init          (WordListWindow *window);
static void word_list_window_dispose       (GObject        *object);
static void word_list_window_set_sensitive (WordListWindow *window);

/* call back functions for ui manager */
static void add_widget_cb               (GtkUIManager   *merge,
					 GtkWidget      *widget,
					 GtkBox         *box);

/* call back functions for actions */
static void file_exit_action_cb         (GtkAction      *action);
static void edit_add_word_action_cb     (GtkAction      *action,
					 WordListWindow *window);
static void edit_remove_word_action_cb  (GtkAction      *action,
					 WordListWindow *window);
static void edit_edit_word_action_cb    (GtkAction      *action,
					 WordListWindow *window);
static void popup_menu_action_cb        (GtkAction      *action,
					 WordListWindow *window);
static void help_about_action_cb        (GtkAction      *action,
					 WordListWindow *window);
static void activate_radio_action(GtkAction *action, GtkRadioAction *current,
				  gpointer data);

/* call back functions for WordListView */
static gboolean word_list_button_press_cb  (GtkWidget      *widget,
					    GdkEventButton *event,
					    WordListWindow *window);
static gboolean word_list_row_activated_cb(GtkWidget *widget,
					   GtkTreePath *path,
					   GtkTreeViewColumn *column,
					   WordListWindow *window);
static gboolean word_list_key_press_cb     (GtkWidget      *widget,
					    GdkEventKey    *event,
					    WordListWindow *window);
static void word_list_selection_changed_cb (GtkTreeSelection *selection,
					    WordListWindow *window);

/* call back functions for WordWindow */
static void wordwin_response_cb            (WordWindow     *dialog,
					    WordListWindow *window);

static void dict_get_property(GObject *object, guint prop_id, GValue *value, GParamSpec *pspec);
static void dict_set_property(GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec);

GtkWindowClass *parent_class = NULL;
GdkEventButton *current_button_event = NULL;

/* Main menu*/
GtkActionEntry menu_action_entries[] = {
    { "DictionaryMenu", NULL, N_("_Dictionary") },
    { "SelectMenu", NULL, N_("_Select") },
    { "EditMenu",   NULL, N_("_Edit")   },
    { "OptionMenu", NULL, N_("_Option") },
    { "HelpMenu",   NULL, N_("_Help")   },
    { "Quit",       GTK_STOCK_QUIT,   N_("_Quit"),       "<control>Q",
      N_("Quit uim-dict"),            G_CALLBACK(file_exit_action_cb) },
    { "AddWord",    GTK_STOCK_ADD,    N_("A_dd..."),     "<control>A",
      N_("Add a new word"),           G_CALLBACK(edit_add_word_action_cb) },
    { "RemoveWord", GTK_STOCK_REMOVE, N_("_Remove..."),  "<control>D",
      N_("Remove the selected word"), G_CALLBACK(edit_remove_word_action_cb) },
    { "EditWord",   GTK_STOCK_DND,    N_("_Edit..."),    "<control>E",
      N_("Edit the selected word"),   G_CALLBACK(edit_edit_word_action_cb) },
    { "PopupMenu",  NULL,             N_("_Popup Menu"), NULL,
      N_("Show popup menu"),          G_CALLBACK(popup_menu_action_cb) },
    { "About",      NULL,             N_("_About"),      NULL,
      N_("About uim-dict"),           G_CALLBACK(help_about_action_cb) },
};
static guint n_menu_action_entries = G_N_ELEMENTS(menu_action_entries);

static GtkRadioActionEntry dictionary_entries[] = {
  { "Anthy", NULL,
    "_Anthy", NULL,
    "Anthy private dictionary", DICT_ENUM_DICTIONARY_TYPE_ANTHY },
  { "Canna", NULL,
    "_Canna", NULL,
    "Canna private dictionary", DICT_ENUM_DICTIONARY_TYPE_CANNA },
};
static guint n_dictionary_entries = G_N_ELEMENTS(dictionary_entries);

#define ACTIVATE_ACTION(window, action_name)				       \
{									       \
  GtkAction *action;							       \
  action = gtk_action_group_get_action((window)->action_group, (action_name)); \
  if (action) {								       \
    gtk_action_activate(action);					       \
  }									       \
}

#define SET_ACTION_SENSITIVE(window, action_name, sensitive)		       \
{									       \
  GtkAction *action;							       \
  action = gtk_action_group_get_action((window)->action_group, (action_name)); \
  if (action) {								       \
    g_object_set(action,						       \
		 "sensitive", (sensitive),				       \
		 NULL);							       \
  }									       \
}

GType word_list_window_get_type(void) {
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(WordListWindowClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc)word_list_window_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(WordWindow),
      0, /* n_preallocs */
      (GInstanceInitFunc)word_list_window_init /* instance_init */
    };
    type = g_type_register_static(GTK_TYPE_WINDOW,
				  "WordListWindow", &info, 0);
  }
  return type;
}

GType
dict_enum_dictionary_type_get_type(void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { DICT_ENUM_DICTIONARY_TYPE_ANTHY, "DICT_ENUM_DICTIONARY_TYPE_ANTHY", "anthy" },
      { DICT_ENUM_DICTIONARY_TYPE_CANNA, "DICT_ENUM_DICTIONARY_TYPE_CANNA", "canna" },
      { DICT_ENUM_DICTIONARY_TYPE_SKK, "DICT_ENUM_DICTIONARY_TYPE_SKK", "skk" },
      { DICT_ENUM_DICTIONARY_TYPE_PRIME, "DICT_ENUM_DICTIONARY_TYPE_PRIME", "prime" },
      { DICT_ENUM_DICTIONARY_TYPE_UNKOWN, "DICT_ENUM_DICTIONARY_TYPE_UNKOWN", "unknown" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static("DictEnumDictionaryType", values);
  }
  return etype;
}

static void
word_list_window_class_init (WordListWindowClass *klass)
{
  GObjectClass *object_class;

  parent_class = g_type_class_peek_parent(klass);
  object_class = (GObjectClass *) klass;

  object_class->dispose = word_list_window_dispose;

  object_class->get_property = dict_get_property;
  object_class->set_property = dict_set_property;

  g_object_class_install_property(object_class,
  				  PROP_DICTIONARY_TYPE,
				  g_param_spec_enum("dictionary-type",
				  		    _("dictionary type"),
						    _("dictionary type"),
						    DICT_TYPE_ENUM_DICTIONARY_TYPE,
						    DICT_ENUM_DICTIONARY_TYPE_ANTHY,
						    G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static gchar *
translate_func(const gchar *path, gpointer data)
{
  return _(path);
}

static void
warn_dict_open()
{
  GtkWidget *dialog;
  const gchar *message;

  message = N_("Couldn't open the dictionary.\n");
  dialog = gtk_message_dialog_new(NULL,
		  		  GTK_DIALOG_MODAL,
				  GTK_MESSAGE_WARNING,
				  GTK_BUTTONS_OK,
				  "%s", _(message));
  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(GTK_WIDGET(dialog));
}

static void
word_list_window_init (WordListWindow *window)
{
  GtkWidget *word_list, *vbox, *statusbar;
  GtkUIManager *ui;
  GtkActionGroup *actions;

  gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
  gtk_window_set_default_size(GTK_WINDOW(window), 600, 450);
  gtk_window_set_title(GTK_WINDOW(window), _("Edit the dictionary"));

#if GTK_CHECK_VERSION(3, 2, 0)
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
#else
  vbox = gtk_vbox_new(FALSE, 0);
#endif
  gtk_container_add(GTK_CONTAINER(window), vbox);
  gtk_widget_show(vbox);

  window->action_group = actions = gtk_action_group_new("Actions");
#if ENABLE_NLS
  gtk_action_group_set_translate_func(window->action_group,
				      translate_func, NULL, NULL);
#endif
  gtk_action_group_add_actions(actions, menu_action_entries,
			       n_menu_action_entries, window);

  gtk_action_group_add_radio_actions(actions, dictionary_entries,
				     n_dictionary_entries,
				     DICT_ENUM_DICTIONARY_TYPE_ANTHY,
				     G_CALLBACK(activate_radio_action),
				     window);

  window->ui_manager = ui = gtk_ui_manager_new();
  gtk_ui_manager_insert_action_group(ui, actions, 0);
  g_signal_connect(ui, "add_widget",
		   G_CALLBACK(add_widget_cb),
		   vbox);
  gtk_window_add_accel_group(GTK_WINDOW(window),
			     gtk_ui_manager_get_accel_group(ui));

  gtk_ui_manager_add_ui_from_file(ui,
				  UIM_DATADIR "/helperdata/uim-dict-ui.xml",
				  NULL);
  gtk_ui_manager_ensure_update(ui);

  window->word_list = word_list = word_list_view_new();
  word_list_view_set_visible_cclass_code_column(WORD_LIST_VIEW(word_list), TRUE);
  word_list_view_set_visible_freq_column(WORD_LIST_VIEW(word_list), TRUE);
  gtk_widget_show(word_list);
  gtk_box_pack_start(GTK_BOX(vbox), word_list, TRUE, TRUE, 0);

  g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN(window->word_list))),
		   "button-press-event",
		   G_CALLBACK(word_list_button_press_cb), window);
  g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN(window->word_list))),
		   "row-activated",
		   G_CALLBACK(word_list_row_activated_cb), window);
  g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN(window->word_list))),
		   "key-press-event",
		   G_CALLBACK(word_list_key_press_cb), window);
  g_signal_connect(G_OBJECT(WORD_LIST_VIEW(window->word_list)->selection),
		   "changed",
		   G_CALLBACK(word_list_selection_changed_cb), window);

  window->statusbar = statusbar = gtk_statusbar_new();
  gtk_box_pack_start(GTK_BOX(vbox), statusbar, FALSE, FALSE, 0);
  gtk_widget_show(statusbar);

  word_list_window_set_sensitive(window);
}

static void
word_list_window_dispose(GObject *object)
{
  WordListWindow *window = WORD_LIST_WINDOW(object);

  if (window->action_group) {
    g_object_unref(window->action_group);
    window->action_group = NULL;
  }

  if (window->ui_manager) {
    g_object_unref(window->ui_manager);
    window->ui_manager = NULL;
  }

  if (G_OBJECT_CLASS(parent_class)->dispose)
    G_OBJECT_CLASS(parent_class)->dispose(object);
}

static void
dict_get_property(GObject *object, guint prop_id, GValue *value,
		  GParamSpec *pspec)
{
  switch (prop_id) {
  case PROP_DICTIONARY_TYPE:
    g_value_set_enum(value, dict_get_dictionary_type(GTK_WIDGET(object)));
    break;
  default:
    break;
  }
}

static void
dict_set_property(GObject *object, guint prop_id, const GValue *value,
		  GParamSpec *pspec)
{
  WordListWindow *window = WORD_LIST_WINDOW(object);
  uim_dict *dict = NULL;
  gchar message[128];

  switch (prop_id) {
  case PROP_DICTIONARY_TYPE:
    window->dictionary_type = g_value_get_enum(value);

    ACTIVATE_ACTION(window, dictionary_entries[window->dictionary_type].name);

    switch (window->dictionary_type) {
    case DICT_ENUM_DICTIONARY_TYPE_ANTHY:
      dict = uim_dict_open(N_("Anthy private dictionary"));
      break;
    case DICT_ENUM_DICTIONARY_TYPE_CANNA:
      word_list_view_set_visible_freq_column(WORD_LIST_VIEW(window->word_list), FALSE);
      dict = uim_dict_open(N_("Canna private dictionary"));
      break;
    default:
      break;
    }
    if (!dict) {
      warn_dict_open();
      break;
    }
    word_list_view_set_dict(WORD_LIST_VIEW(window->word_list), dict);

    g_snprintf(message, sizeof(message), _("%s"), _(dict->identifier));
    gtk_statusbar_push(GTK_STATUSBAR(window->statusbar), 0, _(dict->identifier));
    break;
  default:
    break;
  }
}

DictEnumDictionaryType
dict_get_dictionary_type(GtkWidget *window)
{
  WordListWindow *w = (WordListWindow *)window;

  return w->dictionary_type;
}

GtkWidget *word_list_window_new(int type)
{
  return GTK_WIDGET(g_object_new(WORD_LIST_WINDOW_TYPE,
				 "dictionary-type", type, NULL));
}

static void
word_list_window_set_sensitive(WordListWindow *window)
{
  gboolean selected;
  WordListView *word_list;
  GtkTreeSelection *selection;

  word_list = WORD_LIST_VIEW(window->word_list);
  selection = word_list->selection;

  selected = gtk_tree_selection_get_selected(selection, NULL, NULL);

  if (selected) {
    SET_ACTION_SENSITIVE(window, "EditWord",   TRUE);
    SET_ACTION_SENSITIVE(window, "RemoveWord", TRUE);
  } else {
    SET_ACTION_SENSITIVE(window, "EditWord",   FALSE);
    SET_ACTION_SENSITIVE(window, "RemoveWord", FALSE);
  }
}


/*
 * call back functions for ui manager
 */
static void
add_widget_cb (GtkUIManager *merge, GtkWidget *widget, GtkBox *box)
{
  gtk_box_pack_start (box, widget, FALSE, FALSE, 0);
}


/*
 *  call back functions for actions
 */
static void
file_exit_action_cb(GtkAction *action)
{
  gtk_main_quit();
}

static void
edit_add_word_action_cb(GtkAction *action, WordListWindow *window)
{
  GtkWidget *w;

  w = word_window_new(WORD_WINDOW_MODE_ADD,
		      WORD_LIST_VIEW(window->word_list)->dict);
  gtk_grab_add(w);
  gtk_window_set_transient_for(GTK_WINDOW(w), GTK_WINDOW(window));
  gtk_window_set_position(GTK_WINDOW(w), GTK_WIN_POS_CENTER_ON_PARENT);
  g_signal_connect(G_OBJECT(w), "word-added",
		   G_CALLBACK(wordwin_response_cb), window);
  gtk_widget_show(w);
}

static void
remove_confirm_dialog_response_cb(GtkDialog *dialog, gint arg,
				  gboolean *ok)
{
  g_return_if_fail(ok);

  *ok = FALSE;

  switch (arg)
  {
  case GTK_RESPONSE_OK:
    *ok = TRUE;
    break;
  default:
    break;
  }
}

static void
edit_remove_word_action_cb(GtkAction *action, WordListWindow *window)
{
  GtkWidget *dialog;
  GList *node, *list;
  gboolean ok = FALSE;
  const gchar *message = _("Are you sure to remove seleted words?");

  list = word_list_view_get_selected_data_list(WORD_LIST_VIEW(window->word_list));
  if (!list) return;

  if (!g_list_next(list))
    message = _("Are you sure to remove the selected word?");

  dialog = gtk_message_dialog_new(NULL,
				  GTK_DIALOG_MODAL,
				  GTK_MESSAGE_QUESTION,
				  GTK_BUTTONS_OK_CANCEL,
				  "%s", message);
  gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(window));
  gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER_ON_PARENT);
  g_signal_connect(G_OBJECT(dialog), "response",
		   G_CALLBACK(remove_confirm_dialog_response_cb), &ok);
  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
  dialog = NULL;

  if (!ok) return;

  for (node = list; node; node = g_list_next(node)) {
    uim_word *w = node->data;
    int ret;

    ret = uim_dict_remove_word(WORD_LIST_VIEW(window->word_list)->dict, w);

    if (ret) {
      word_list_view_refresh(WORD_LIST_VIEW(window->word_list));
      dialog = gtk_message_dialog_new(NULL,
				      GTK_DIALOG_MODAL,
				      GTK_MESSAGE_INFO,
				      GTK_BUTTONS_CLOSE,
				      "%s", _("Word deletion succeded."));
    } else {
      dialog = gtk_message_dialog_new(NULL,
				      GTK_DIALOG_MODAL,
				      GTK_MESSAGE_ERROR,
				      GTK_BUTTONS_CLOSE,
				      "%s", _("Word deletion failed."));
    }
    gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(window));
    gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER_ON_PARENT);
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
  }
  g_list_free(list);
}

static void
edit_edit_word_action_cb(GtkAction *action, WordListWindow *window)
{
  GList *list;

  list = word_list_view_get_selected_data_list(WORD_LIST_VIEW(window->word_list));

  /* FIXME! it can edit only one word yet. */
  if (list) {
    GtkWidget *widget;

    widget = word_window_new(WORD_WINDOW_MODE_EDIT,
			     WORD_LIST_VIEW(window->word_list)->dict);
    gtk_grab_add(widget);
    gtk_window_set_transient_for(GTK_WINDOW(widget), GTK_WINDOW(window));
    gtk_window_set_position(GTK_WINDOW(widget), GTK_WIN_POS_CENTER_ON_PARENT);
    word_window_set_word(WORD_WINDOW(widget), list->data);
    g_signal_connect(G_OBJECT(widget), "word-added",
    		     G_CALLBACK(wordwin_response_cb), window);

    gtk_widget_show(widget);
  }
  g_list_free(list);
}

static void
popup_menu_action_cb(GtkAction *action, WordListWindow *window)
{
  GtkWidget *popup_menu;

  popup_menu = gtk_ui_manager_get_widget(window->ui_manager,
					 "/WordListPopup");
  g_return_if_fail(popup_menu);

  if (current_button_event) {
    gtk_menu_popup(GTK_MENU(popup_menu),
		   NULL, NULL,
		   NULL, NULL,
		   current_button_event->button,
		   current_button_event->time);
  } else {
    gtk_menu_popup(GTK_MENU(popup_menu), NULL, NULL,
		   NULL, NULL, 0, GDK_CURRENT_TIME);
  }
}

#if GTK_CHECK_VERSION(2, 6, 0) && !GTK_CHECK_VERSION(2, 18, 0)
static void
activate_url(GtkAboutDialog *about, const gchar *link, gpointer data)
{
  /* g_print("show url %s\n", link); */
}
#endif

static void
help_about_action_cb(GtkAction *action, WordListWindow *window)
{
  const gchar *name = N_("uim-dict");
#if GTK_CHECK_VERSION(2, 6, 0)
  GdkPixbuf *pixbuf, *transparent;
  const gchar *filename = UIM_PIXMAPSDIR "/uim-dict.png";
  const gchar *authors[] = {
    "Masahito Omote <omote@utyuuzin.net>",
    "Takuro Ashie",
    "Etsushi Kato",
    NULL
  };
  const gchar *copyright = N_(
    "Copyright (C) 2003-2004 Masahito Omote\n"
    "Copyright (C) 2004-2013 uim Project\n"
    "All rights reserved.");

  transparent = NULL;
  pixbuf = gdk_pixbuf_new_from_file(filename, NULL);
  if (pixbuf) {
    transparent = gdk_pixbuf_add_alpha(pixbuf, TRUE, 0xff, 0xff, 0xff);
    g_object_unref(pixbuf);
  }
  
#if !GTK_CHECK_VERSION(2, 18, 0)
  gtk_about_dialog_set_url_hook (activate_url, NULL, NULL);
#endif
  gtk_show_about_dialog (GTK_WINDOW(window),
			 "name", name,
			 "version", VERSION,
			 "copyright", copyright,
			 "website", "https://github.com/uim/uim",
			 "authors", authors,
			 "logo", transparent,
			 NULL);
  g_object_unref(transparent);
#else
  GtkWidget *about_dialog, *label1;
  const gchar *copyright = N_(
    "Copyright 2003-2004 Masahito Omote &lt;omote@utyuuzin.net&gt;\n"
    "Copyright 2004-2013 uim Project https://github.com/uim/uim\n"
    "All rights reserved.");
  gchar *about_name =
    g_strdup_printf("<span size=\"20000\">%s %s </span>\n\n<span size=\"14000\">%s </span>\n", _(name), VERSION, _(copyright));

  about_dialog = gtk_dialog_new_with_buttons(_("About uim-dict"), NULL,
					     GTK_DIALOG_MODAL,
					     GTK_STOCK_OK,
					     GTK_RESPONSE_ACCEPT, NULL);
  gtk_container_set_border_width(GTK_CONTAINER(about_dialog), 8);

  label1 = gtk_label_new(NULL);
  gtk_widget_show(label1);
  gtk_label_set_markup(GTK_LABEL(label1), about_name);
  g_free(about_name);
  gtk_box_pack_start(
      GTK_BOX(gtk_dialog_get_content_area(GTK_DIALOG(about_dialog))),
      label1, FALSE, FALSE, 0);

  gtk_window_set_transient_for(GTK_WINDOW(about_dialog),
			       GTK_WINDOW(window));
  gtk_window_set_position(GTK_WINDOW(about_dialog),
			  GTK_WIN_POS_CENTER_ON_PARENT);
  gtk_dialog_run(GTK_DIALOG(about_dialog));

  gtk_widget_destroy(about_dialog);
#endif
}

static void
activate_radio_action(GtkAction *action, GtkRadioAction *current,
		      gpointer data)
{
  WordListWindow *window;
  gboolean active;
  guint value;

  window = WORD_LIST_WINDOW(data);
  active = gtk_toggle_action_get_active(GTK_TOGGLE_ACTION(current));
  value = gtk_radio_action_get_current_value(GTK_RADIO_ACTION(current));

  if (active) {
    if (value != window->dictionary_type) {
      GtkWidget *newwin;

      newwin = word_list_window_new(value);

      /* quit */
      if (WORD_LIST_VIEW(WORD_LIST_WINDOW(newwin)->word_list)->dict == NULL) {
        gtk_main_quit();
      }

      g_signal_handlers_disconnect_by_func(G_OBJECT(window),
					   (gpointer)(uintptr_t)dict_window_destroy_cb,
					   NULL);
      gtk_widget_destroy(GTK_WIDGET(window));

      g_signal_connect(G_OBJECT(newwin), "destroy",
		       G_CALLBACK(dict_window_destroy_cb), NULL);
      gtk_widget_show(newwin);
    }
  }
}

/*
 * call back functions for WordListView
 */
static gboolean
word_list_button_press_cb(GtkWidget *widget, GdkEventButton *event,
			  WordListWindow *window)
{
  if (event->button == 2 && event->type == GDK_BUTTON_PRESS) {
    ACTIVATE_ACTION(window, "EditWord");
  } else if (event->button == 3) {
    current_button_event = event;
    ACTIVATE_ACTION(window, "PopupMenu");
    current_button_event = NULL;
  }

  return FALSE;
}

static gboolean
word_list_row_activated_cb(GtkWidget *widget, GtkTreePath *path,
			   GtkTreeViewColumn *column, WordListWindow *window)
{
    ACTIVATE_ACTION(window, "EditWord");

    return FALSE;
}

static gboolean
word_list_key_press_cb(GtkWidget *widget, GdkEventKey *event,
		       WordListWindow *window)
{
  switch (event->keyval) {
  case GDK_Delete:
  case GDK_BackSpace:
    ACTIVATE_ACTION(window, "RemoveWord");
    break;
  default:
    break;
  }

  return FALSE;
}

static void
word_list_selection_changed_cb(GtkTreeSelection *selection,
			       WordListWindow *window)
{
  word_list_window_set_sensitive(window);
}


/*
 * call back functions for WordWindow
 */
static void
wordwin_response_cb(WordWindow *dialog, WordListWindow *window)
{
  word_list_view_refresh(WORD_LIST_VIEW(window->word_list));
}
