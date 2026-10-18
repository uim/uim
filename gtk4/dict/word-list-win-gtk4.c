/*

  Copyright (c) 2004-2026 uim Project https://github.com/uim/uim

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
#include <gtk/gtk.h>

#include "gettext.h"

#include "word.h"
#include "anthy.h"
#include "word-list-view-gtk4.h"
#include "word-list-win-gtk4.h"
#include "word-win-gtk4.h"

static void word_list_window_class_init    (WordListWindowClass *window);
static void word_list_window_init          (WordListWindow *window);
static void word_list_window_dispose       (GObject        *object);
static void word_list_window_set_sensitive (WordListWindow *window);

static void build_action_group          (WordListWindow *window);
static void build_menu_bar              (WordListWindow *window, GtkBox *vbox);
static void build_toolbar               (WordListWindow *window, GtkBox *vbox);
static void build_popup_menu            (WordListWindow *window);

/* action callbacks (GSimpleAction, "activate") */
static void act_quit_cb                 (GSimpleAction *action, GVariant *param, gpointer data);
static void act_add_word_cb             (GSimpleAction *action, GVariant *param, gpointer data);
static void act_remove_word_cb          (GSimpleAction *action, GVariant *param, gpointer data);
static void act_edit_word_cb            (GSimpleAction *action, GVariant *param, gpointer data);
static void act_about_cb                (GSimpleAction *action, GVariant *param, gpointer data);
static void act_dictionary_type_change_state_cb (GSimpleAction *action, GVariant *value, gpointer data);

/* call back functions for WordListView */
static void     word_list_click_pressed_cb (GtkGestureClick *gesture, gint n_press,
					    gdouble x, gdouble y, WordListWindow *window);
static gboolean word_list_row_activated_cb (GtkTreeView *treeview, GtkTreePath *path,
					    GtkTreeViewColumn *column, WordListWindow *window);
static gboolean word_list_key_pressed_cb   (GtkEventControllerKey *controller, guint keyval,
					    guint keycode, GdkModifierType state,
					    WordListWindow *window);
static void word_list_selection_changed_cb (GtkTreeSelection *selection,
					    WordListWindow *window);

/* call back functions for WordWindow */
static void wordwin_response_cb            (WordWindow     *dialog,
					    WordListWindow *window);

static void dict_get_property(GObject *object, guint prop_id, GValue *value, GParamSpec *pspec);
static void dict_set_property(GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec);

GtkWindowClass *parent_class = NULL;

/*
 * "win"-scoped action names. These carry no engine name: the only
 * engine-specific bit is which of "anthy"/"canna" the dictionary-type
 * radio offers, gated by USE_ANTHY/USE_CANNA below, exactly mirroring
 * what the old uim-dict-ui.xml.in did with @UI_XML_*_START@ markers.
 */
#define ACTION_QUIT            "quit"
#define ACTION_ADD_WORD        "add-word"
#define ACTION_REMOVE_WORD     "remove-word"
#define ACTION_EDIT_WORD       "edit-word"
#define ACTION_ABOUT           "about"
#define ACTION_DICTIONARY_TYPE "dictionary-type"

#define ACTIVATE_ACTION(window, action_name)				       \
  g_action_group_activate_action(G_ACTION_GROUP((window)->actions),	       \
				 (action_name), NULL)

#define SET_ACTION_SENSITIVE(window, action_name, sensitive)		       \
{									       \
  GAction *action_ = g_action_map_lookup_action(G_ACTION_MAP((window)->actions), \
						(action_name));	       \
  if (action_)								       \
    g_simple_action_set_enabled(G_SIMPLE_ACTION(action_), (sensitive));      \
}

static const gchar *
dict_type_to_name(DictEnumDictionaryType type)
{
  switch (type) {
  case DICT_ENUM_DICTIONARY_TYPE_ANTHY: return "anthy";
  case DICT_ENUM_DICTIONARY_TYPE_CANNA: return "canna";
  default: return "anthy";
  }
}

static DictEnumDictionaryType
name_to_dict_type(const gchar *name)
{
  if (g_strcmp0(name, "canna") == 0)
    return DICT_ENUM_DICTIONARY_TYPE_CANNA;
  return DICT_ENUM_DICTIONARY_TYPE_ANTHY;
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
      sizeof(WordListWindow),
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

static void
warn_dict_open(void)
{
  GtkAlertDialog *alert;

  alert = gtk_alert_dialog_new("%s", _("Couldn't open the dictionary.\n"));
  gtk_alert_dialog_show(alert, NULL);
  g_object_unref(alert);
}

static void
build_action_group(WordListWindow *window)
{
  static const GActionEntry entries[] = {
    { ACTION_QUIT,        act_quit_cb,        NULL, NULL, NULL },
    { ACTION_ADD_WORD,    act_add_word_cb,    NULL, NULL, NULL },
    { ACTION_REMOVE_WORD, act_remove_word_cb, NULL, NULL, NULL },
    { ACTION_EDIT_WORD,   act_edit_word_cb,   NULL, NULL, NULL },
    { ACTION_ABOUT,       act_about_cb,       NULL, NULL, NULL },
  };
  GSimpleAction *dict_type_action;

  window->actions = g_simple_action_group_new();
  g_action_map_add_action_entries(G_ACTION_MAP(window->actions), entries,
				  G_N_ELEMENTS(entries), window);

  dict_type_action = g_simple_action_new_stateful(
      ACTION_DICTIONARY_TYPE, G_VARIANT_TYPE_STRING,
      g_variant_new_string(dict_type_to_name(window->dictionary_type)));
  g_signal_connect(dict_type_action, "change-state",
		   G_CALLBACK(act_dictionary_type_change_state_cb), window);
  g_action_map_add_action(G_ACTION_MAP(window->actions),
			  G_ACTION(dict_type_action));
  g_object_unref(dict_type_action);

  gtk_widget_insert_action_group(GTK_WIDGET(window), "win",
				 G_ACTION_GROUP(window->actions));
}

static void
build_menu_bar(WordListWindow *window, GtkBox *vbox)
{
  GMenu *menu_bar, *dict_menu, *select_menu, *edit_menu, *help_menu;
  GMenuItem *item;
  GtkWidget *menubar;

  menu_bar = g_menu_new();

  /* _Dictionary */
  dict_menu = g_menu_new();
  select_menu = g_menu_new();

#ifdef USE_ANTHY
  item = g_menu_item_new(_("_Anthy"), NULL);
  g_menu_item_set_action_and_target_value(item, "win." ACTION_DICTIONARY_TYPE,
					  g_variant_new_string("anthy"));
  g_menu_append_item(select_menu, item);
  g_object_unref(item);
#endif
#ifdef USE_CANNA
  item = g_menu_item_new(_("_Canna"), NULL);
  g_menu_item_set_action_and_target_value(item, "win." ACTION_DICTIONARY_TYPE,
					  g_variant_new_string("canna"));
  g_menu_append_item(select_menu, item);
  g_object_unref(item);
#endif

  g_menu_append_submenu(dict_menu, _("_Select"), G_MENU_MODEL(select_menu));
  g_object_unref(select_menu);
  g_menu_append(dict_menu, _("_Quit"), "win." ACTION_QUIT);
  g_menu_append_submenu(menu_bar, _("_Dictionary"), G_MENU_MODEL(dict_menu));
  g_object_unref(dict_menu);

  /* _Edit */
  edit_menu = g_menu_new();
  g_menu_append(edit_menu, _("A_dd..."), "win." ACTION_ADD_WORD);
  g_menu_append(edit_menu, _("_Edit..."), "win." ACTION_EDIT_WORD);
  g_menu_append(edit_menu, _("_Remove..."), "win." ACTION_REMOVE_WORD);
  g_menu_append_submenu(menu_bar, _("_Edit"), G_MENU_MODEL(edit_menu));
  g_object_unref(edit_menu);

  /* _Help */
  help_menu = g_menu_new();
  g_menu_append(help_menu, _("_About"), "win." ACTION_ABOUT);
  g_menu_append_submenu(menu_bar, _("_Help"), G_MENU_MODEL(help_menu));
  g_object_unref(help_menu);

  menubar = gtk_popover_menu_bar_new_from_model(G_MENU_MODEL(menu_bar));
  g_object_unref(menu_bar);
  gtk_box_append(vbox, menubar);
}

static void
build_toolbar(WordListWindow *window, GtkBox *vbox)
{
  GtkWidget *box, *button;

  box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 4);
  gtk_widget_set_margin_start(box, 4);
  gtk_widget_set_margin_end(box, 4);
  gtk_widget_set_margin_top(box, 4);
  gtk_widget_set_margin_bottom(box, 4);

  button = gtk_button_new_from_icon_name("list-add-symbolic");
  gtk_widget_set_tooltip_text(button, _("Add a new word"));
  gtk_actionable_set_action_name(GTK_ACTIONABLE(button), "win." ACTION_ADD_WORD);
  gtk_box_append(GTK_BOX(box), button);

  button = gtk_button_new_from_icon_name("document-edit-symbolic");
  gtk_widget_set_tooltip_text(button, _("Edit the selected word"));
  gtk_actionable_set_action_name(GTK_ACTIONABLE(button), "win." ACTION_EDIT_WORD);
  gtk_box_append(GTK_BOX(box), button);

  button = gtk_button_new_from_icon_name("list-remove-symbolic");
  gtk_widget_set_tooltip_text(button, _("Remove the selected word"));
  gtk_actionable_set_action_name(GTK_ACTIONABLE(button), "win." ACTION_REMOVE_WORD);
  gtk_box_append(GTK_BOX(box), button);

  gtk_box_append(vbox, box);
}

static void
build_popup_menu(WordListWindow *window)
{
  GMenu *popup;

  popup = g_menu_new();
  g_menu_append(popup, _("_Edit..."), "win." ACTION_EDIT_WORD);
  g_menu_append(popup, _("_Remove..."), "win." ACTION_REMOVE_WORD);

  window->popup_menu = gtk_popover_menu_new_from_model(G_MENU_MODEL(popup));
  g_object_unref(popup);

  /* Anchor the popover to word_list (our own plain GtkWidget, built
   * with a modern layout manager -- see word-list-view-gtk4.c) rather
   * than to its internal GtkTreeView: GtkTreeView keeps its own
   * private bookkeeping for headers/children and doesn't reliably
   * support an *extra* widget being attached to it via
   * gtk_widget_set_parent(), which was corrupting its CSS node
   * ordering ("gtk_css_node_insert_after" critical warning) as soon
   * as the popup menu was built. word_list_click_pressed_cb()
   * translates the click position into word_list's own coordinate
   * space accordingly. */
  gtk_widget_set_parent(window->popup_menu, window->word_list);
  gtk_popover_set_has_arrow(GTK_POPOVER(window->popup_menu), FALSE);
}

static void
word_list_window_init (WordListWindow *window)
{
  GtkWidget *word_list, *vbox, *statusbar;
  GtkEventController *key_controller;
  GtkGesture *click_gesture;

  gtk_window_set_default_size(GTK_WINDOW(window), 600, 450);
  gtk_window_set_title(GTK_WINDOW(window), _("Edit the dictionary"));

  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
  gtk_window_set_child(GTK_WINDOW(window), vbox);

  build_action_group(window);
  build_menu_bar(window, GTK_BOX(vbox));
  build_toolbar(window, GTK_BOX(vbox));

  window->word_list = word_list = word_list_view_new();
  word_list_view_set_visible_cclass_code_column(WORD_LIST_VIEW(word_list), TRUE);
  word_list_view_set_visible_freq_column(WORD_LIST_VIEW(word_list), TRUE);
  gtk_widget_set_vexpand(word_list, TRUE);
  gtk_box_append(GTK_BOX(vbox), word_list);

  build_popup_menu(window);

  click_gesture = gtk_gesture_click_new();
  gtk_gesture_single_set_button(GTK_GESTURE_SINGLE(click_gesture), 0);
  gtk_widget_add_controller(GTK_WIDGET(WORD_LIST_VIEW(window->word_list)->view),
			    GTK_EVENT_CONTROLLER(click_gesture));
  g_signal_connect(click_gesture, "pressed",
		   G_CALLBACK(word_list_click_pressed_cb), window);

  g_signal_connect(WORD_LIST_VIEW(window->word_list)->view,
		   "row-activated",
		   G_CALLBACK(word_list_row_activated_cb), window);

  key_controller = gtk_event_controller_key_new();
  gtk_widget_add_controller(GTK_WIDGET(WORD_LIST_VIEW(window->word_list)->view),
			    key_controller);
  g_signal_connect(key_controller, "key-pressed",
		   G_CALLBACK(word_list_key_pressed_cb), window);

  g_signal_connect(WORD_LIST_VIEW(window->word_list)->selection,
		   "changed",
		   G_CALLBACK(word_list_selection_changed_cb), window);

  window->statusbar = statusbar = gtk_statusbar_new();
  gtk_box_append(GTK_BOX(vbox), statusbar);

  word_list_window_set_sensitive(window);
}

static void
word_list_window_dispose(GObject *object)
{
  WordListWindow *window = WORD_LIST_WINDOW(object);

  if (window->popup_menu) {
    gtk_widget_unparent(window->popup_menu);
    window->popup_menu = NULL;
  }

  if (window->actions) {
    g_object_unref(window->actions);
    window->actions = NULL;
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
  GAction *dict_type_action;

  switch (prop_id) {
  case PROP_DICTIONARY_TYPE:
    window->dictionary_type = g_value_get_enum(value);

    dict_type_action = g_action_map_lookup_action(G_ACTION_MAP(window->actions),
						  ACTION_DICTIONARY_TYPE);
    if (dict_type_action)
      g_simple_action_set_state(G_SIMPLE_ACTION(dict_type_action),
				g_variant_new_string(dict_type_to_name(window->dictionary_type)));

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
    SET_ACTION_SENSITIVE(window, ACTION_EDIT_WORD,   TRUE);
    SET_ACTION_SENSITIVE(window, ACTION_REMOVE_WORD, TRUE);
  } else {
    SET_ACTION_SENSITIVE(window, ACTION_EDIT_WORD,   FALSE);
    SET_ACTION_SENSITIVE(window, ACTION_REMOVE_WORD, FALSE);
  }
}

/*
 *  action callbacks
 */
static void
act_quit_cb(GSimpleAction *action, GVariant *param, gpointer data)
{
  GApplication *app = g_application_get_default();

  if (app)
    g_application_quit(app);
}

static void
act_add_word_cb(GSimpleAction *action, GVariant *param, gpointer data)
{
  WordListWindow *window = WORD_LIST_WINDOW(data);
  GtkWidget *w;

  w = word_window_new(WORD_WINDOW_MODE_ADD,
		      WORD_LIST_VIEW(window->word_list)->dict);
  gtk_window_set_modal(GTK_WINDOW(w), TRUE);
  gtk_window_set_transient_for(GTK_WINDOW(w), GTK_WINDOW(window));
  g_signal_connect(w, "word-added",
		   G_CALLBACK(wordwin_response_cb), window);
  gtk_window_present(GTK_WINDOW(w));
}

typedef struct {
  WordListWindow *window;
  GList          *list;
} RemoveConfirmState;

static void
remove_confirm_dialog_cb(GObject *source, GAsyncResult *result, gpointer data)
{
  RemoveConfirmState *state = data;
  GError *error = NULL;
  int button, ok_count = 0, fail_count = 0;
  GList *node;

  button = gtk_alert_dialog_choose_finish(GTK_ALERT_DIALOG(source), result, &error);
  if (error) {
    g_error_free(error);
    g_list_free(state->list);
    g_free(state);
    return;
  }

  /* button 1 == "_Remove" (see edit_remove_word_action_cb) */
  if (button == 1) {
    for (node = state->list; node; node = g_list_next(node)) {
      uim_word *w = node->data;
      int ret = uim_dict_remove_word(WORD_LIST_VIEW(state->window->word_list)->dict, w);

      if (ret)
	ok_count++;
      else
	fail_count++;
    }
    if (ok_count)
      word_list_view_refresh(WORD_LIST_VIEW(state->window->word_list));

    if (fail_count == 0) {
      GtkAlertDialog *alert = gtk_alert_dialog_new("%s", _("Word deletion succeeded."));
      gtk_alert_dialog_show(alert, GTK_WINDOW(state->window));
      g_object_unref(alert);
    } else {
      GtkAlertDialog *alert = gtk_alert_dialog_new("%s", _("Word deletion failed."));
      gtk_alert_dialog_show(alert, GTK_WINDOW(state->window));
      g_object_unref(alert);
    }
  }

  g_list_free(state->list);
  g_free(state);
}

static void
act_remove_word_cb(GSimpleAction *action, GVariant *param, gpointer data)
{
  WordListWindow *window = WORD_LIST_WINDOW(data);
  GtkAlertDialog *alert;
  RemoveConfirmState *state;
  GList *list;
  const gchar *message = _("Are you sure to remove seleted words?");
  const char *buttons[] = { N_("_Cancel"), N_("_Remove"), NULL };

  list = word_list_view_get_selected_data_list(WORD_LIST_VIEW(window->word_list));
  if (!list) return;

  if (!g_list_next(list))
    message = _("Are you sure to remove the selected word?");

  state = g_new0(RemoveConfirmState, 1);
  state->window = window;
  state->list = list;

  alert = gtk_alert_dialog_new("%s", message);
  gtk_alert_dialog_set_buttons(alert, (const char * const *)buttons);
  gtk_alert_dialog_set_cancel_button(alert, 0);
  gtk_alert_dialog_set_default_button(alert, 0);
  gtk_alert_dialog_choose(alert, GTK_WINDOW(window), NULL,
			  remove_confirm_dialog_cb, state);
  g_object_unref(alert);
}

static void
act_edit_word_cb(GSimpleAction *action, GVariant *param, gpointer data)
{
  WordListWindow *window = WORD_LIST_WINDOW(data);
  GList *list;

  list = word_list_view_get_selected_data_list(WORD_LIST_VIEW(window->word_list));

  /* FIXME! it can edit only one word yet. */
  if (list) {
    GtkWidget *widget;

    widget = word_window_new(WORD_WINDOW_MODE_EDIT,
			     WORD_LIST_VIEW(window->word_list)->dict);
    gtk_window_set_modal(GTK_WINDOW(widget), TRUE);
    gtk_window_set_transient_for(GTK_WINDOW(widget), GTK_WINDOW(window));
    word_window_set_word(WORD_WINDOW(widget), list->data);
    g_signal_connect(widget, "word-added",
    		     G_CALLBACK(wordwin_response_cb), window);

    gtk_window_present(GTK_WINDOW(widget));
  }
  g_list_free(list);
}

static void
act_about_cb(GSimpleAction *action, GVariant *param, gpointer data)
{
  WordListWindow *window = WORD_LIST_WINDOW(data);
  GtkWidget *about;
  GdkPixbuf *pixbuf, *transparent;
  GdkTexture *logo = NULL;
  const gchar *filename = UIM_PIXMAPSDIR "/uim-dict.png";
  const gchar *authors[] = {
    "Masahito Omote <omote@utyuuzin.net>",
    "Takuro Ashie",
    "Etsushi Kato",
    NULL
  };
  const gchar *copyright = N_(
    "Copyright (C) 2003-2004 Masahito Omote\n"
    "Copyright (C) 2004-2026 uim Project\n"
    "All rights reserved.");

  pixbuf = gdk_pixbuf_new_from_file(filename, NULL);
  if (pixbuf) {
    transparent = gdk_pixbuf_add_alpha(pixbuf, TRUE, 0xff, 0xff, 0xff);
    g_object_unref(pixbuf);
    if (transparent) {
      logo = gdk_texture_new_for_pixbuf(transparent);
      g_object_unref(transparent);
    }
  }

  about = gtk_about_dialog_new();
  gtk_about_dialog_set_program_name(GTK_ABOUT_DIALOG(about), N_("uim-dict"));
  gtk_about_dialog_set_version(GTK_ABOUT_DIALOG(about), VERSION);
  gtk_about_dialog_set_copyright(GTK_ABOUT_DIALOG(about), _(copyright));
  gtk_about_dialog_set_website(GTK_ABOUT_DIALOG(about), "https://github.com/uim/uim");
  gtk_about_dialog_set_authors(GTK_ABOUT_DIALOG(about), authors);
  if (logo) {
    gtk_about_dialog_set_logo(GTK_ABOUT_DIALOG(about), GDK_PAINTABLE(logo));
    g_object_unref(logo);
  }

  gtk_window_set_transient_for(GTK_WINDOW(about), GTK_WINDOW(window));
  gtk_window_present(GTK_WINDOW(about));
}

static void
act_dictionary_type_change_state_cb(GSimpleAction *action, GVariant *value,
				    gpointer data)
{
  WordListWindow *window = WORD_LIST_WINDOW(data);
  DictEnumDictionaryType new_type = name_to_dict_type(g_variant_get_string(value, NULL));

  if (new_type != window->dictionary_type) {
    GtkWidget *newwin;

    newwin = word_list_window_new(new_type);

    /* quit if the new dictionary couldn't be opened */
    if (WORD_LIST_VIEW(WORD_LIST_WINDOW(newwin)->word_list)->dict == NULL) {
      GApplication *app = g_application_get_default();

      gtk_window_destroy(GTK_WINDOW(newwin));
      if (app)
	g_application_quit(app);
      return;
    }

    g_signal_handlers_disconnect_by_func(window,
					 (gpointer)dict_window_destroy_cb, NULL);
    gtk_window_destroy(GTK_WINDOW(window));

    g_signal_connect(newwin, "destroy",
		     G_CALLBACK(dict_window_destroy_cb), NULL);
    gtk_window_present(GTK_WINDOW(newwin));
  }

  g_simple_action_set_state(action, value);
}

/*
 * call back functions for WordListView
 */
static void
word_list_click_pressed_cb(GtkGestureClick *gesture, gint n_press,
			   gdouble x, gdouble y, WordListWindow *window)
{
  guint button = gtk_gesture_single_get_current_button(GTK_GESTURE_SINGLE(gesture));

  if (button == GDK_BUTTON_MIDDLE && n_press == 1) {
    ACTIVATE_ACTION(window, ACTION_EDIT_WORD);
  } else if (button == GDK_BUTTON_SECONDARY) {
    GtkWidget *view = GTK_WIDGET(WORD_LIST_VIEW(window->word_list)->view);
    double tx = x, ty = y;
    GdkRectangle rect;

    /* x/y arrive in the treeview's own coordinate space (the gesture
     * is attached to it); the popover is anchored to word_list
     * instead (see build_popup_menu()), so translate accordingly. */
    gtk_widget_translate_coordinates(view, window->word_list, x, y, &tx, &ty);
    rect = (GdkRectangle){ (int)tx, (int)ty, 1, 1 };

    gtk_popover_set_pointing_to(GTK_POPOVER(window->popup_menu), &rect);
    gtk_popover_popup(GTK_POPOVER(window->popup_menu));
  }
}

static gboolean
word_list_row_activated_cb(GtkTreeView *treeview, GtkTreePath *path,
			   GtkTreeViewColumn *column, WordListWindow *window)
{
  ACTIVATE_ACTION(window, ACTION_EDIT_WORD);

  return FALSE;
}

static gboolean
word_list_key_pressed_cb(GtkEventControllerKey *controller, guint keyval,
			 guint keycode, GdkModifierType state,
			 WordListWindow *window)
{
  switch (keyval) {
  case GDK_KEY_Delete:
  case GDK_KEY_BackSpace:
    ACTIVATE_ACTION(window, ACTION_REMOVE_WORD);
    return TRUE;
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
