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

/*
 * gtk+-immodule
 */
#include <config.h>

#include <gtk/gtk.h>
#include <gtk/gtkimmodule.h>
#include <gdk/gdkkeysyms.h>
#ifdef GDK_WINDOWING_X11
#include <gdk/gdkx.h>
#endif
#include <glib.h>

#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <locale.h>

#include "uim/uim.h"
#include "uim/uim-util.h"
#include "uim/uim-helper.h"
#include "uim/uim-im-switcher.h"
#include "uim/gettext.h"
#include "uim/uim-scm.h"
#include "uim/counted-init.h"

#include "gtk-im-uim.h"
#include "uim-cand-win-gtk.h"
#include "uim-cand-win-vertical-gtk.h"
#include "uim-cand-win-tbl-gtk.h"
#include "uim-cand-win-horizontal-gtk.h"
#include "caret-state-indicator.h"
#include "key-util-gtk.h"
#ifdef GDK_WINDOWING_X11
#include "compose.h"
#endif
#include "text-util.h"

/* exported symbols */
GtkIMContext *im_module_create(const gchar *context_id);
void im_module_list(const GtkIMContextInfo ***contexts, int *n_contexts);
void im_module_exit(void);
void im_module_init(GTypeModule *type_module);

#ifdef GDK_WINDOWING_X11
extern int compose_handle_key(GdkEventKey *key, IMUIMContext *uic);
#endif

#define NR_CANDIDATES 20
#define DEFAULT_SEPARATOR_STR "|"

struct preedit_segment {
  int attr;
  gchar *str;
};

static int im_uim_fd = -1;
static unsigned int read_tag;
#if IM_UIM_USE_SNOOPER
static guint snooper_id;
static gboolean snooper_installed = FALSE;
#elif IM_UIM_USE_TOPLEVEL
static GtkWidget *cur_toplevel;
static GtkWidget *grab_widget;
static gulong cur_key_press_handler_id;
static gulong cur_key_release_handler_id;
static GList *cwin_list;
#endif

static IMUIMContext context_list;
static IMUIMContext *focused_context = NULL;
static gboolean disable_focused_context = FALSE;

static GObjectClass *parent_class;

typedef struct _IMContextUIMClass
{
  GtkIMContextClass parent_class;
} IMContextUIMClass;


static void cand_select_cb(void *ptr, int index);
static void im_uim_class_init(GtkIMContextClass *class);
static void im_uim_class_finalize(GtkIMContextClass *class);
static void im_uim_init(IMUIMContext *uic);
static void switch_app_global_im_cb(void *ptr, const char *name);
static void switch_system_global_im_cb(void *ptr, const char *name);

#if IM_UIM_USE_SNOOPER
static gboolean key_snoop(GtkWidget *grab_widget, GdkEventKey *key, gpointer data);
#elif IM_UIM_USE_TOPLEVEL
static gboolean handle_key_on_toplevel(GtkWidget *widget, GdkEventKey *event, gpointer data);
#endif
#if IM_UIM_USE_DELAY
static void cand_delay_timer_remove(UIMCandWinGtk *cwin);
#endif
#if IM_UIM_USE_NEW_PAGE_HANDLING
static GSList *get_page_candidates(IMUIMContext *uic, guint page, guint nr, guint display_limit);
static void free_candidates(GSList *candidates);
#endif
static void send_im_list(void);
static UIMCandWinGtk *im_uim_create_cand_win_gtk(void);

static const GTypeInfo class_info = {
  sizeof(IMContextUIMClass),
  (GBaseInitFunc) NULL,
  (GBaseFinalizeFunc) NULL,
  (GClassInitFunc) im_uim_class_init,
  (GClassFinalizeFunc)im_uim_class_finalize,
  NULL, /* for class data */
  sizeof(IMUIMContext), /* size of instance */
  0,
  (GInstanceInitFunc) im_uim_init, /* constructor */
};

static GType type_im_uim = 0;

#define IM_UIM_CONTEXT(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj),type_im_uim,IMUIMContext))

static const GtkIMContextInfo im_uim_info = {
  "uim", /* id */
  "uim", /* human-readable name*/
  "uim", /* domain for gettext*/
  LOCALEDIR,
  "ja:ko:zh:*"
};

static const GtkIMContextInfo *im_uim_info_list = {
  &im_uim_info
};



/* gtk's string handling */

void
im_uim_commit_string(void *ptr, const char *str)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  uim_bool show_state;
  gint x, y;

  g_return_if_fail(str);
  g_signal_emit_by_name(uic, "commit", str);

  show_state = uim_scm_symbol_value_bool("bridge-show-input-state?");
  if (show_state && uic->win) {
    gdk_window_get_origin(uic->win, &x, &y);
    caret_state_indicator_update(uic->caret_state_indicator, x, y, NULL);
  }
}

static void
commit_cb(GtkIMContext *ic, const gchar *str, IMUIMContext *is)
{
  g_return_if_fail(str);
  g_signal_emit_by_name(is, "commit", str);
}

static gboolean
get_user_defined_color(PangoColor *color, const gchar *uim_symbol)
{
  gboolean parsed = FALSE;
  char *literal = uim_scm_symbol_value_str(uim_symbol);

  if (literal != NULL && literal[0] != '\0')
    parsed = pango_color_parse(color, literal);

  free(literal);

  return parsed;
}

static gchar *
get_preedit_segment(struct preedit_segment *ps, PangoAttrList *attrs,
		    gchar *str)
{
  PangoAttribute *attr;
  const gchar *segment_str = ps->str;
  gint len;

  if ((ps->attr & UPreeditAttr_Separator) && !strcmp(segment_str, ""))
    segment_str = DEFAULT_SEPARATOR_STR;

  if (attrs) {
    PangoColor color;
    int begin, end;

    begin = strlen(str);
    end = begin + strlen(segment_str);

    if (ps->attr & UPreeditAttr_UnderLine) {
      attr = pango_attr_underline_new(PANGO_UNDERLINE_SINGLE);
      attr->start_index = begin;
      attr->end_index = end;
      pango_attr_list_change(attrs, attr);
    }

    if (ps->attr & UPreeditAttr_Separator) {
      const gchar *separator_fg_symbol, *separator_bg_symbol;

      if (ps->attr & UPreeditAttr_Reverse) {
	separator_fg_symbol = "reversed-separator-foreground";
	separator_bg_symbol = "reversed-separator-background";
      } else {
	separator_fg_symbol = "separator-foreground";
	separator_bg_symbol = "separator-background";
      }

      if (get_user_defined_color(&color, separator_fg_symbol)) {
	attr = pango_attr_foreground_new(color.red, color.green, color.blue);
	attr->start_index = begin;
	attr->end_index = end;
	pango_attr_list_change(attrs, attr);
      }

      if (get_user_defined_color(&color, separator_bg_symbol)) {
	attr = pango_attr_background_new(color.red, color.green, color.blue);
	attr->start_index = begin;
	attr->end_index = end;
	pango_attr_list_change(attrs, attr);
      }
    } else if (ps->attr & UPreeditAttr_Reverse) {
      if (get_user_defined_color(&color, "reversed-preedit-foreground")
	  || pango_color_parse(&color, "#fff")) {
	attr = pango_attr_foreground_new(color.red, color.green, color.blue);
	attr->start_index = begin;
	attr->end_index = end;
	pango_attr_list_change(attrs, attr);
      }

      if (get_user_defined_color(&color, "reversed-preedit-background")
	  || pango_color_parse(&color, "#000")) {
	attr = pango_attr_background_new(color.red, color.green, color.blue);
	attr->start_index = begin;
	attr->end_index = end;
	pango_attr_list_change(attrs, attr);
      }
    }
  }

  len = strlen(str) + strlen(segment_str) + 1;
  str = (gchar *)g_realloc(str, len);
  g_strlcat(str, segment_str, len);

  return str;
}

/* only used when use_preedit == FALSE */
static void
show_preedit(GtkIMContext *ic, GtkWidget *preedit_label)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);
  GtkWidget *preedit_window;
  gchar *str;
  gint cursor_pos;
  PangoAttrList *attrs;

  preedit_window = gtk_widget_get_parent(preedit_label);

  gtk_im_context_get_preedit_string(ic, &str, &attrs, &cursor_pos);

  if (strlen(str) > 0) {
    gint x, y, w, h;
    PangoLayout *layout;

    gtk_label_set_text(GTK_LABEL(preedit_label), str);
    gtk_label_set_attributes(GTK_LABEL(preedit_label), attrs);

    gdk_window_get_origin(uic->win, &x, &y);

    gtk_window_move(GTK_WINDOW(preedit_window),
		    x + uic->preedit_pos.x,
		    y + uic->preedit_pos.y);

    layout = gtk_label_get_layout(GTK_LABEL(preedit_label));

    pango_layout_get_cursor_pos(layout, 0, NULL, NULL);

    pango_layout_get_pixel_size(layout, &w, &h);
    gtk_window_resize(GTK_WINDOW(preedit_window), w, h);

    gtk_widget_show(preedit_window);
  } else {
    gtk_label_set_text(GTK_LABEL(preedit_label), "");
    gtk_widget_hide(preedit_window);
    gtk_window_resize(GTK_WINDOW(preedit_window), 1, 1);
  }
  g_free(str);
  pango_attr_list_unref(attrs);
}



/* widget utilities */

#if IM_UIM_USE_TOPLEVEL
static void
remove_cur_toplevel()
{
#if GTK_CHECK_VERSION(2, 18, 0)
  if (cur_toplevel && gtk_widget_is_toplevel(cur_toplevel)) {
#else
  if (cur_toplevel && GTK_WIDGET_TOPLEVEL(cur_toplevel)) {
#endif
    if (cur_key_press_handler_id)
      g_signal_handler_disconnect(cur_toplevel, cur_key_press_handler_id);
    if (cur_key_release_handler_id)
      g_signal_handler_disconnect(cur_toplevel, cur_key_release_handler_id);
    cur_toplevel = NULL;
  }
}

static gboolean
cur_toplevel_deleted(GtkWidget *widget, gpointer data)
{
  cur_toplevel = NULL;

  return FALSE;
}

static void
update_cur_toplevel(IMUIMContext *uic)
{
  /* Don't set our candwin's text widget as cur_toplevel */
  if (uic->widget) {
    UIMCandWinGtk *cwin;
    GList *tmp_list;

    tmp_list = cwin_list;
    while (tmp_list) {
      cwin = tmp_list->data;
      if (cwin->sub_window.text_view &&
		      cwin->sub_window.text_view == uic->widget)
	  return;
      tmp_list = tmp_list->next;
    }
  }

  if (uic->widget) {
    GtkWidget *toplevel = gtk_widget_get_toplevel(uic->widget);
#if GTK_CHECK_VERSION(2, 18, 0)
    if (toplevel && gtk_widget_is_toplevel(toplevel)) {
#else
    if (toplevel && GTK_WIDGET_TOPLEVEL(toplevel)) {
#endif
      if (cur_toplevel != toplevel) {
	remove_cur_toplevel();
	cur_toplevel = toplevel;
	cur_key_press_handler_id = g_signal_connect(cur_toplevel,
			"key-press-event",
			G_CALLBACK(handle_key_on_toplevel), uic);
	cur_key_release_handler_id = g_signal_connect(cur_toplevel,
			"key-release-event",
			G_CALLBACK(handle_key_on_toplevel), uic);
	g_signal_connect(cur_toplevel,
			"delete_event",
			G_CALLBACK(cur_toplevel_deleted), NULL);
      }
    } else
      remove_cur_toplevel();
  } else
    remove_cur_toplevel();
}

static void
on_client_widget_hierarchy_changed(GtkWidget *widget, GtkWidget *old_toplevel, IMUIMContext *uic)
{
  update_cur_toplevel(uic);
}

static gboolean
on_client_widget_grab_notify(GtkWidget *widget, gboolean was_grabbed, IMUIMContext *uic)
{
  if (was_grabbed)
    grab_widget = NULL;
  else {
    grab_widget = gtk_grab_get_current();
    if (!grab_widget) {
      if (cur_toplevel && GTK_IS_WINDOW(cur_toplevel)) {
        GtkWindowGroup *group;
        GtkWindow *window;
	
        window = GTK_WINDOW(cur_toplevel);
        group = gtk_window_get_group(window);
#if GTK_CHECK_VERSION(2, 22, 0)
        grab_widget = gtk_window_group_get_current_grab(group);
#else
        if (group && group->grabs)
          grab_widget = GTK_WIDGET(group->grabs->data);
#endif
      }
    }
  }

  return FALSE;
}
#endif /* IM_UIM_USE_TOPLEVEL */

static GtkWidget *
widget_for_window(GdkWindow *window)
{
  while (window) {
    gpointer user_data;
    gdk_window_get_user_data(window, &user_data);
    if (user_data)
      return user_data;

    window = gdk_window_get_parent(window);
  }

  return NULL;
}

static void
update_client_widget(IMUIMContext *uic)
{
  GtkWidget *new_widget = widget_for_window(uic->win);

#if IM_UIM_USE_TOPLEVEL
  if (new_widget != uic->widget) {
    if (uic->widget) {
      g_signal_handlers_disconnect_by_func(uic->widget,
		      (gpointer)(uintptr_t)on_client_widget_hierarchy_changed, uic);
      g_signal_handlers_disconnect_by_func(uic->widget,
		      (gpointer)(uintptr_t)on_client_widget_grab_notify, uic);
    }
    uic->widget = new_widget;
    if (uic->widget) {
      g_signal_connect(uic->widget, "hierarchy-changed",
		      G_CALLBACK(on_client_widget_hierarchy_changed), uic);
      g_signal_connect(uic->widget, "grab-notify",
		      G_CALLBACK(on_client_widget_grab_notify), uic);
    }

    update_cur_toplevel(uic);
  }
#else /* IM_UIM_USE_TOPLEVEL */
  uic->widget = new_widget;
#endif
}



/* utility functions */

static int
preedit_strlen(IMUIMContext *uic)
{
  int i, len = 0;

  for (i = 0; i < uic->nr_psegs; i++)
    len += strlen(uic->pseg[i].str);

  return len;
}

static void
index_changed_cb(UIMCandWinGtk *cwin, IMUIMContext *uic)
{
  gint index;
#if IM_UIM_USE_NEW_PAGE_HANDLING
  guint new_page;
#endif

  g_return_if_fail(UIM_IS_CAND_WIN_GTK(cwin));

  index = uim_cand_win_gtk_get_index(cwin);
  uim_set_candidate_index(uic->uc, index);

#if IM_UIM_USE_NEW_PAGE_HANDLING
  new_page = uim_cand_win_gtk_query_new_page_by_cand_select(uic->cwin, index);

  if (!uic->cwin->stores->pdata[new_page]) {
    /* index_changed signal was triggered by prev/next page button on candwin
     * (not from uim (cand_select_cb(), cand_shift_page_cb()))
     */
    guint nr = uic->cwin->nr_candidates;
    guint display_limit = uic->cwin->display_limit;
    GSList *list = get_page_candidates(uic, new_page, nr, display_limit);
    uim_cand_win_gtk_set_page_candidates(uic->cwin, new_page, list);
    free_candidates(list);
  }
#endif /* IM_UIM_USE_NEW_PAGE_HANDLING */
}

static void
layout_candwin(IMUIMContext *uic)
{
#if GTK_CHECK_VERSION(2, 90, 0)
  gint x, y, width, height;
#else
  gint x, y, width, height, depth;
#endif

  g_return_if_fail(uic);

  if (uic->win && uic->cwin) {
#if GTK_CHECK_VERSION(2, 90, 0)
    gdk_window_get_geometry(uic->win, &x, &y, &width, &height);
#else
    gdk_window_get_geometry(uic->win, &x, &y, &width, &height, &depth);
#endif
    gdk_window_get_origin(uic->win, &x, &y);
    uim_cand_win_gtk_layout(uic->cwin, x, y, width, height);
  }
}

static GdkFilterReturn
toplevel_window_candidate_cb(GdkXEvent *xevent, GdkEvent *ev, gpointer data)
{
  IMUIMContext *uic = data;

  if (!uic)
    return GDK_FILTER_CONTINUE;

  if (uic->cwin_is_active)
    layout_candwin(uic);

  return GDK_FILTER_CONTINUE;
}

#if IM_UIM_USE_TOPLEVEL
static inline gboolean
event_key_equal(GdkEventKey *event1, GdkEventKey *event2)
{
  return (event1->type == event2->type &&
	  event1->window == event2->window &&
	  event1->send_event == event2->send_event &&
	  event1->time == event2->time &&
	  event1->state == event2->state &&
	  event1->keyval == event2->keyval &&
	  event1->length == event2->length &&
	  event1->string == event2->string &&
	  event1->hardware_keycode == event2->hardware_keycode &&
	  event1->group == event2->group);
}

static void
init_event_key_rec(GdkEventKey *event)
{
  event->type = -1;
  event->window = NULL;
  event->send_event = 0;
  event->time = 0;
  event->state = 0;
  event->keyval = 0;
  event->length = 0;
  event->string = NULL;
  event->hardware_keycode = 0;
  event->group = 0;
}

static inline void
store_event_key(GdkEventKey *dest, GdkEventKey *source)
{
  memcpy(dest, source, sizeof(GdkEventKey));
}
#endif

static GString *
get_caret_state_label_from_prop_list(const char *str)
{
  gchar **lines;
  GString *label;
  int i;

  label = g_string_new("");
  lines = g_strsplit(str, "\n", 0);
  for (i = 0; lines[i] && strcmp("", lines[i]); i++) {
    gchar **cols;

    cols = g_strsplit(lines[i], "\t", 0);
    if (cols && cols[0]) {
      if (!strcmp("branch", cols[0])) {
	gchar *iconic_label = cols[2];

	if (strcmp(label->str, ""))
	  g_string_append(label, "\t");
	g_string_append(label, iconic_label);
      }
    }
    g_strfreev(cols);
  }
  g_strfreev(lines);

  return label;
}



/* callback functions for libuim */

static void
clear_cb(void *ptr)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  int i;

  for (i = 0; i < uic->nr_psegs; i++)
    g_free(uic->pseg[i].str);
  free(uic->pseg);

  uic->pseg = NULL;
  uic->nr_psegs = 0;
}

static void
pushback_cb(void *ptr, int attr, const char *str)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  g_return_if_fail(str);

  if (!strcmp(str, "")
      && !(attr & (UPreeditAttr_Cursor | UPreeditAttr_Separator)))
    return;

  uic->pseg = realloc(uic->pseg,
		      sizeof(struct preedit_segment) * (uic->nr_psegs + 1));
  uic->pseg[uic->nr_psegs].str = g_strdup(str);
  uic->pseg[uic->nr_psegs].attr = attr;
  uic->nr_psegs++;
}

static void
update_cb(void *ptr)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  int preedit_len;

  g_return_if_fail(uic);

  preedit_len = preedit_strlen(uic);

  if (uic->prev_preedit_len == 0 && preedit_len)
    g_signal_emit_by_name(uic, "preedit_start");

  if (uic->prev_preedit_len || preedit_len)
    g_signal_emit_by_name(uic, "preedit_changed");

  if (uic->prev_preedit_len && preedit_len == 0)
    g_signal_emit_by_name(uic, "preedit_end");

  uic->prev_preedit_len = preedit_len;
}

static void
update_prop_list_cb(void *ptr, const char *str)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  GString *prop_list;
  uim_bool show_state;
  char *show_state_with;
  uim_bool show_state_mode;
  uim_bool show_state_mode_on;

  if (uic != focused_context || disable_focused_context)
    return;

  prop_list = g_string_new("");
  g_string_printf(prop_list, "prop_list_update\ncharset=UTF-8\n%s", str);

  uim_helper_send_message(im_uim_fd, prop_list->str);
  g_string_free(prop_list, TRUE);

  show_state = uim_scm_symbol_value_bool("bridge-show-input-state?");
  show_state_with = uim_scm_c_symbol(uim_scm_symbol_value("bridge-show-with?"));
  show_state_mode = (strcmp(show_state_with, "mode") == 0);
  show_state_mode_on = uim_scm_symbol_value_bool("bridge-show-input-state-mode-on?");

  if (uic->win) {
    if (show_state && !(show_state_mode && !show_state_mode_on)) {
      gint timeout;
      gint x, y;
      GString *label;

      gdk_window_get_origin(uic->win, &x, &y);
      label = get_caret_state_label_from_prop_list(str);
      caret_state_indicator_update(uic->caret_state_indicator, x, y, label->str);
      g_string_free(label, TRUE);
      if (strcmp(show_state_with, "time") == 0)
	timeout = uim_scm_symbol_value_int("bridge-show-input-state-time-length");
      else
	timeout = 0;

      if (timeout != 0)
	caret_state_indicator_set_timeout(uic->caret_state_indicator,
					timeout * 1000);
      gtk_widget_show_all(uic->caret_state_indicator);
    } else if (show_state_mode && !show_state_mode_on) {
      gtk_widget_hide(uic->caret_state_indicator);
    }
  }
  free(show_state_with);
}

#if IM_UIM_USE_NEW_PAGE_HANDLING
static GSList *
get_page_candidates(IMUIMContext *uic,
		    guint page,
		    guint nr,
		    guint display_limit)
{
  gint i, page_nr, start;
  GSList *list = NULL;

  start = page * display_limit;
  if (display_limit && (nr - start) > display_limit)
    page_nr = display_limit;
  else
    page_nr = nr - start;

  for (i = start; i < (start + page_nr); i++) {
    uim_candidate cand = uim_get_candidate(uic->uc, i,
		    display_limit ? (int)(i % display_limit) : i);
    list = g_slist_prepend(list, cand);
  }
  list = g_slist_reverse(list);

  return list;
}

static void
free_candidates(GSList *candidates)
{
  g_slist_foreach(candidates, (GFunc)uim_candidate_free, NULL);
  g_slist_free(candidates);
}
#endif /* IM_UIM_USE_NEW_PAGE_HANDLING */
 
static void
cand_activate_cb(void *ptr, int nr, int display_limit)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  GSList *list = NULL;
#if !IM_UIM_USE_NEW_PAGE_HANDLING
  uim_candidate cand;
  gint i;
#endif

#if IM_UIM_USE_DELAY
  cand_delay_timer_remove(uic->cwin);
#endif

  uic->cwin_is_active = TRUE;

#if !IM_UIM_USE_NEW_PAGE_HANDLING
  for (i = 0; i < nr; i++) {
    cand = uim_get_candidate(uic->uc, i, display_limit ? i % display_limit : i);
    list = g_slist_prepend(list, cand);
  }
  list = g_slist_reverse(list);

  uim_cand_win_gtk_set_candidates(uic->cwin, display_limit, list);

  g_slist_foreach(list, (GFunc)uim_candidate_free, NULL);
  g_slist_free(list);
#else
  list = get_page_candidates(uic, 0, nr, display_limit);

  uim_cand_win_gtk_set_nr_candidates(uic->cwin, nr, display_limit);
  uic->cwin->candidate_index = -1; /* Don't select any candidate at first */
  uim_cand_win_gtk_set_page_candidates(uic->cwin, 0, list);
  uim_cand_win_gtk_set_page(uic->cwin, 0);

  free_candidates(list);
#endif /* IM_UIM_USE_NEW_PAGE_HANDLING */

  layout_candwin(uic);
  gtk_widget_show(GTK_WIDGET(uic->cwin));

  if (uic->win) {
    GdkWindow *toplevel;

    toplevel = gdk_window_get_toplevel(uic->win);
    gdk_window_add_filter(toplevel, toplevel_window_candidate_cb, uic);
  }
}

#if IM_UIM_USE_DELAY
static gint
cand_activate_timeout(gpointer data)
{
  IMUIMContext *uic = (IMUIMContext *)data;
  int nr = -1, display_limit = -1, selected_index = -1;

  g_object_set_data(G_OBJECT(uic->cwin), "timeout-tag", GUINT_TO_POINTER(0));
  uim_delay_activating(uic->uc, &nr, &display_limit, &selected_index);
  if (nr > 0) {
    cand_activate_cb(uic, nr, display_limit);
    if (selected_index >= 0) {
      cand_select_cb(uic, selected_index);
    }
  }
  return FALSE;
}

static void
cand_delay_timer_remove(UIMCandWinGtk *cwin)
{
  guint tag = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(cwin), "timeout-tag"));
  if (tag > 0)
    g_source_remove(tag);
}

static void
cand_activate_with_delay_cb(void *ptr, int delay)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  guint tag;

  cand_delay_timer_remove(uic->cwin);
  if (delay > 0) {
    /* g_timeout_add_seconds() needs GLib 2.14 */
    tag = g_timeout_add(delay * 1000, cand_activate_timeout, (gpointer)uic);
    g_object_set_data(G_OBJECT(uic->cwin), "timeout-tag", GUINT_TO_POINTER(tag));
  } else {
    cand_activate_timeout(ptr);
  }
}
#endif /* IM_UIM_USE_DELAY */

static void
cand_select_cb(void *ptr, int index)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
#if IM_UIM_USE_NEW_PAGE_HANDLING
  guint new_page;
#endif

  layout_candwin(uic);
#if IM_UIM_USE_NEW_PAGE_HANDLING
  new_page = uim_cand_win_gtk_query_new_page_by_cand_select(uic->cwin, index);

  if (!uic->cwin->stores->pdata[new_page]) {
    guint nr = uic->cwin->nr_candidates;
    guint display_limit = uic->cwin->display_limit;
    GSList *list = get_page_candidates(uic, new_page, nr, display_limit);
    uim_cand_win_gtk_set_page_candidates(uic->cwin, new_page, list);
    free_candidates(list);
  }
#endif /* IM_UIM_USE_NEW_PAGE_HANDLING */
  g_signal_handlers_block_by_func(uic->cwin, (gpointer)(uintptr_t)index_changed_cb, uic);
  uim_cand_win_gtk_set_index(uic->cwin, index);
  g_signal_handlers_unblock_by_func(uic->cwin, (gpointer)(uintptr_t)index_changed_cb, uic);
}

static void
cand_shift_page_cb(void *ptr, int direction)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
#if IM_UIM_USE_NEW_PAGE_HANDLING
  guint new_page;
#endif

  layout_candwin(uic);

  g_signal_handlers_block_by_func(uic->cwin,
				  (gpointer)(uintptr_t)index_changed_cb, uic);
#if IM_UIM_USE_NEW_PAGE_HANDLING
  new_page = uim_cand_win_gtk_query_new_page_by_shift_page(uic->cwin,
							 direction);
  if (!uic->cwin->stores->pdata[new_page]) {
    guint nr = uic->cwin->nr_candidates;
    guint display_limit = uic->cwin->display_limit;
    GSList *list = get_page_candidates(uic, new_page, nr, display_limit);
    uim_cand_win_gtk_set_page_candidates(uic->cwin, new_page, list);
    free_candidates(list);
  }
#endif /* IM_UIM_USE_NEW_PAGE_HANDLING */
  uim_cand_win_gtk_shift_page(uic->cwin, direction);
  if (uic->cwin->candidate_index != -1)
    uim_set_candidate_index(uic->uc, uic->cwin->candidate_index);
  g_signal_handlers_unblock_by_func(uic->cwin,
				   (gpointer)(uintptr_t)index_changed_cb, uic);
}

static void
cand_deactivate_cb(void *ptr)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;

  uic->cwin_is_active = FALSE;

  if (uic->cwin) {
#if IM_UIM_USE_DELAY
    cand_delay_timer_remove(uic->cwin);
#endif
    gtk_widget_hide(GTK_WIDGET(uic->cwin));
    uim_cand_win_gtk_clear_candidates(uic->cwin);
  }

  if (uic->win) {
    GdkWindow *toplevel;

    toplevel = gdk_window_get_toplevel(uic->win);
    gdk_window_remove_filter(toplevel, toplevel_window_candidate_cb, uic);
  }
}

static void
configuration_changed_cb(void *ptr)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;

  if (focused_context == uic && !disable_focused_context)
    send_im_list();
}

static void
switch_app_global_im_cb(void *ptr, const char *name)
{
  IMUIMContext *uic, *cc;
  GString *im_name_sym;

  uic = (IMUIMContext *)ptr;
  im_name_sym = g_string_new(name);
  g_string_prepend_c(im_name_sym, '\'');

  for (cc = context_list.next; cc != &context_list; cc = cc->next) {
    if (cc != uic)
      uim_switch_im(cc->uc, name);
  }
  uim_prop_update_custom(uic->uc,
			 "custom-preserved-default-im-name", im_name_sym->str);
  g_string_free(im_name_sym, TRUE);
}

static void
switch_system_global_im_cb(void *ptr, const char *name)
{
  GString *msg;

  /* switch contexts of this process */
  switch_app_global_im_cb(ptr, name);

  /* Switch contexts of other processes. Bridges should not expect
   * that the helper-server reflect back the messaage to the
   * originating process.  -- YamaKen 2006-03-01 */
  msg = g_string_new("");
  g_string_printf(msg, "im_change_whole_desktop\n%s\n", name);
  uim_helper_send_message(im_uim_fd, msg->str);
  g_string_free(msg, TRUE);
}

static int
acquire_text_cb(void *ptr, enum UTextArea text_id, enum UTextOrigin origin,
		int former_req_len, int latter_req_len, char **former,
		char **latter)
{
  int err;
  IMUIMContext *uic = (IMUIMContext *)ptr;

  switch (text_id) {
  case UTextArea_Primary:
    err = im_uim_acquire_primary_text(uic, origin, former_req_len,
				      latter_req_len, former, latter);
    break;
  case UTextArea_Selection:
    err = im_uim_acquire_selection_text(uic, origin, former_req_len,
					latter_req_len, former, latter);
    break;
  case UTextArea_Clipboard:
    err = im_uim_acquire_clipboard_text(uic, origin, former_req_len,
					latter_req_len, former, latter);
    break;
  case UTextArea_Unspecified:
  default:
    err = -1;
  }

  return err;
}

static int
delete_text_cb(void *ptr, enum UTextArea text_id, enum UTextOrigin origin,
		int former_req_len, int latter_req_len)
{
  int err;
  IMUIMContext *uic = (IMUIMContext *)ptr;

  switch (text_id) {
  case UTextArea_Primary:
    err = im_uim_delete_primary_text(uic, origin, former_req_len,
				     latter_req_len);
    break;
  case UTextArea_Selection:
    err = im_uim_delete_selection_text(uic, origin, former_req_len,
				       latter_req_len);
    break;
  case UTextArea_Clipboard:
  case UTextArea_Unspecified:
  default:
    err = -1;
  }

  return err;
}

/* uim helper related */

static void
helper_disconnect_cb(void)
{
  im_uim_fd = -1;
  g_source_remove(read_tag);
}

static void
parse_helper_str_im_change(const char *str)
{
  IMUIMContext *cc;
  gchar **lines = g_strsplit(str, "\n", -1);
  gchar *im_name = lines[1];
  GString *im_name_sym = g_string_new(im_name);

  g_string_prepend_c(im_name_sym, '\'');

  if (g_str_has_prefix(str, "im_change_this_text_area_only") == TRUE) {
    if (focused_context && disable_focused_context == FALSE) {
      uim_switch_im(focused_context->uc, im_name);
      uim_prop_list_update(focused_context->uc);
    }
  } else if (g_str_has_prefix(str, "im_change_whole_desktop") == TRUE) {
    for (cc = context_list.next; cc != &context_list; cc = cc->next) {
      uim_switch_im(cc->uc, im_name);
      uim_prop_update_custom(cc->uc, "custom-preserved-default-im-name",
			     im_name_sym->str);
      if (focused_context && cc == focused_context)
	uim_prop_list_update(cc->uc);
    }
  } else if (g_str_has_prefix(str, "im_change_this_application_only") == TRUE) {
    if (focused_context && disable_focused_context == FALSE) {
      for (cc = context_list.next; cc != &context_list; cc = cc->next) {
	uim_switch_im(cc->uc, im_name);
	uim_prop_update_custom(cc->uc, "custom-preserved-default-im-name",
			       im_name_sym->str);
	if (cc == focused_context)
	  uim_prop_list_update(cc->uc);
      }
    }
  }
  g_strfreev(lines);
  g_string_free(im_name_sym, TRUE);
}

static void
send_im_list(void)
{
  int nr, i;
  GString *msg;
  const char *current_im_name;

  if (!focused_context)
    return;

  nr = uim_get_nr_im(focused_context->uc);
  current_im_name = uim_get_current_im_name(focused_context->uc);

  msg = g_string_new("im_list\ncharset=UTF-8\n");
  for (i = 0; i < nr; i++) {
    /*
     * Return value of uim_get_im_language() is an ISO 639-1
     * compatible language code such as "ja". Since it is unfriendly
     * for human reading, we convert it into friendly one by
     * uim_get_language_name_from_locale() here.
     */
    const char *name = uim_get_im_name(focused_context->uc, i);
    const char *langcode = uim_get_im_language(focused_context->uc, i);
    const char *lang = uim_get_language_name_from_locale(langcode);
    const char *short_desc = uim_get_im_short_desc(focused_context->uc, i);

    g_string_append(msg, name);
    g_string_append(msg, "\t");
    if (lang)
      g_string_append(msg, lang);
    g_string_append(msg, "\t");
    if (short_desc)
      g_string_append(msg, short_desc);
    g_string_append(msg, "\t");
    if (strcmp(name, current_im_name) == 0)
      g_string_append(msg, "selected");
    g_string_append(msg, "\n");
  }
  uim_helper_send_message(im_uim_fd, msg->str);
  g_string_free(msg, TRUE);
}

/* Copied from helper-common-gtk.c. Maybe we need common GTK+ utility file. */
static gchar *
get_charset(gchar *line)
{
  gchar **splitted = NULL;

  splitted = g_strsplit(line, "=", 0);

  if (splitted && splitted[0] && splitted[1]
      && strcmp("charset", splitted[0]) == 0) {
    gchar *charset = g_strdup(splitted[1]);
    g_strfreev(splitted);
    return charset;
  } else {
    g_strfreev(splitted);
    return NULL;
  }
}

static void
commit_string_from_other_process(const gchar *str)
{
  gchar **lines = g_strsplit(str, "\n", 0);
  gchar *commit_string;

  if (!lines || !lines[0] || !lines[1] || !lines[2])
    return; /* Message is broken, do nothing. */

  /*
   * If second line exists, we assume the first line as a charset
   * specifier.  This (rotten) convention is influenced by old design
   * mistake (character encoding was forgotten!).
   */
  if (strcmp(lines[2], "") != 0) {
    gchar *encoding, *commit_string_utf8;

    encoding = get_charset(lines[1]);
    commit_string = lines[2];
    commit_string_utf8 = g_convert(commit_string, strlen(commit_string),
				   "UTF-8", encoding,
				   NULL, /* gsize *bytes_read */
				   NULL, /* size *bytes_written */
				   NULL); /* GError **error */
    g_signal_emit_by_name(focused_context, "commit", commit_string_utf8);
    g_free(encoding);
    g_free(commit_string_utf8);
  } else {
    /* Assuming character encoding as UTF-8. */
    commit_string = lines[1];
    g_signal_emit_by_name(focused_context, "commit", commit_string);
  }

  g_strfreev(lines);
}

static void
update_candwin_pos_type()
{
  IMUIMContext *cc;

  for (cc = context_list.next; cc != &context_list; cc = cc->next) {
    if (cc->cwin)
      uim_cand_win_gtk_get_window_pos_type(cc->cwin);
  }
}

static void
update_candwin_style()
{
  IMUIMContext *cc;
  char *candwinprog; /* deprecated */

  candwinprog = uim_scm_symbol_value_str("uim-candwin-prog");
  /* don't update window style if deprecated uim-candwin-prog is set */
  if (candwinprog) {
    free(candwinprog);
    return;
  }

  for (cc = context_list.next; cc != &context_list; cc = cc->next) {
    if (cc->cwin) {
      g_signal_handlers_disconnect_by_func(cc->cwin,
		      (gpointer)(uintptr_t)index_changed_cb, cc);
#if IM_UIM_USE_DELAY
      cand_delay_timer_remove(cc->cwin);
#endif
      gtk_widget_destroy(GTK_WIDGET(cc->cwin));
#if IM_UIM_USE_TOPLEVEL
      cwin_list = g_list_remove(cwin_list, cc->cwin);
#endif
      cc->cwin = im_uim_create_cand_win_gtk();
#if IM_UIM_USE_TOPLEVEL
      cwin_list = g_list_append(cwin_list, cc->cwin);
#endif
      g_signal_connect(G_OBJECT(cc->cwin), "index-changed",
		       G_CALLBACK(index_changed_cb), cc);
    }
  }
}

static void
parse_helper_str(const char *str)
{
  gchar **lines;

  if (g_str_has_prefix(str, "im_change") == TRUE) {
    parse_helper_str_im_change(str);
  } else if (g_str_has_prefix(str, "prop_update_custom") == TRUE) {
    IMUIMContext *cc;

    lines = g_strsplit(str, "\n", 0);
    if (lines && lines[0] && lines[1] && lines[2]) {
      for (cc = context_list.next; cc != &context_list; cc = cc->next) {
	uim_prop_update_custom(cc->uc, lines[1], lines[2]);
	if (!strcmp(lines[1], "candidate-window-position"))
	  update_candwin_pos_type();
	if (!strcmp(lines[1], "candidate-window-style"))
	  update_candwin_style();
	break;  /* all custom variables are global */
      }
      g_strfreev(lines);
    }
  } else if (g_str_has_prefix(str, "custom_reload_notify") == TRUE) {
    uim_prop_reload_configs();
    update_candwin_pos_type();
    update_candwin_style();
  } else if (focused_context && !disable_focused_context) {
    if (g_str_has_prefix(str, "prop_list_get") == TRUE) {
      uim_prop_list_update(focused_context->uc);
    } else if (g_str_has_prefix(str, "prop_activate") == TRUE) {
      lines = g_strsplit(str, "\n", 0);
      if (lines && lines[0]) {
	uim_prop_activate(focused_context->uc, lines[1]);
	g_strfreev(lines);
      }
    } else if (g_str_has_prefix(str, "im_list_get") == TRUE) {
      send_im_list();
    } else if (g_str_has_prefix(str, "commit_string")) {
      commit_string_from_other_process(str);
    } else if (g_str_has_prefix(str, "focus_in") == TRUE) {
      disable_focused_context = TRUE;
      /*
       * We don't set "focused_context = NULL" here, because some
       * window managers have some focus related bugs??
       */
    }
  }
}

static gboolean
helper_read_cb(GIOChannel *channel, GIOCondition c, gpointer p)
{
  char *msg;
  int fd = g_io_channel_unix_get_fd(channel);

  uim_helper_read_proc(fd);
  while ((msg = uim_helper_get_message())) {
    parse_helper_str(msg);
    free(msg);
  }
  return TRUE;
}

static void
check_helper_connection()
{
  if (im_uim_fd < 0) {
    im_uim_fd = uim_helper_init_client_fd(helper_disconnect_cb);
    if (im_uim_fd >= 0) {
      GIOChannel *channel;
      channel = g_io_channel_unix_new(im_uim_fd);
      read_tag = g_io_add_watch(channel, G_IO_IN | G_IO_HUP | G_IO_ERR,
				helper_read_cb, NULL);
      g_io_channel_unref(channel);
    }
  }
}



/* class functions */

/*
 * filter key event handler
 *
 * uim uses key snooper or toplevel key event for IM.  So filter key
 * event is just for fallbacks.
 *
 */
static gboolean
im_uim_filter_keypress(GtkIMContext *ic, GdkEventKey *key)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);
  int rv;

#if IM_UIM_USE_SNOOPER
  if (!snooper_installed) {
#elif IM_UIM_USE_TOPLEVEL
  if (!cur_toplevel || (cur_toplevel && grab_widget) ||
		  !event_key_equal(&uic->event_rec, key)) {
#else
  if (TRUE) {
#endif
    int kv, mod;

    im_uim_convert_keyevent(key, &kv, &mod);

    if (key->type == GDK_KEY_RELEASE)
      rv = uim_release_key(uic->uc, kv, mod);
    else
      rv = uim_press_key(uic->uc, kv, mod);

    if (rv) {
#ifdef GDK_WINDOWING_X11
      rv = compose_handle_key(key, uic);
      if (rv)
#endif
        return gtk_im_context_filter_keypress(uic->slave, key);
    }

    return TRUE;
  }

#ifdef GDK_WINDOWING_X11
  rv = compose_handle_key(key, uic);
  if (rv)
    return gtk_im_context_filter_keypress(uic->slave, key);

  return TRUE;
#else
  return gtk_im_context_filter_keypress(uic->slave, key);
#endif
}

static void
im_uim_get_preedit_string(GtkIMContext *ic, gchar **str, PangoAttrList **attrs,
			  gint *cursor_pos)
{
  gchar *tmp;
  int i, pos = 0;
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);

  if (attrs)
    *attrs = pango_attr_list_new();

  tmp = g_strdup("");

  for (i = 0; i < uic->nr_psegs; i++) {
    if (uic->pseg[i].attr & UPreeditAttr_Cursor)
      pos = g_utf8_strlen(tmp, -1);

    if (attrs)
      tmp = get_preedit_segment(&uic->pseg[i], *attrs, tmp);
    else
      tmp = get_preedit_segment(&uic->pseg[i], NULL, tmp);
  }
  if (cursor_pos)
    *cursor_pos = pos;

  if (str)
    *str = tmp;
  else
    g_free(tmp);
}

static void
im_uim_set_cursor_location(GtkIMContext *ic, GdkRectangle *area)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);

  uic->preedit_pos = *area;
  uim_cand_win_gtk_set_cursor_location(uic->cwin, area);
  caret_state_indicator_set_cursor_location(uic->caret_state_indicator, area);

  if (uic->cwin_is_active)
    layout_candwin(uic);
}

static void
im_uim_focus_in(GtkIMContext *ic)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);
  IMUIMContext *cc;

  focused_context = uic;
  disable_focused_context = FALSE;

#if IM_UIM_USE_SNOOPER
  /* Using key snooper is not recommended */
  if (snooper_installed == FALSE) {
    snooper_id = gtk_key_snooper_install((GtkKeySnoopFunc)key_snoop, NULL);
    snooper_installed = TRUE;
  }
#elif IM_UIM_USE_TOPLEVEL
  update_cur_toplevel(uic);
#endif

  check_helper_connection();
  uim_helper_client_focus_in(uic->uc);
  uim_prop_list_update(uic->uc);

  for (cc = context_list.next; cc != &context_list; cc = cc->next) {
    if (cc != uic && cc->cwin)
      gtk_widget_hide(GTK_WIDGET(cc->cwin));
  }

  if (uic->cwin && uic->cwin_is_active)
    gtk_widget_show(GTK_WIDGET(uic->cwin));

  uim_focus_in_context(uic->uc);
}

static void
im_uim_focus_out(GtkIMContext *ic)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);

#if IM_UIM_USE_SNOOPER
  if (snooper_installed == TRUE) {
    gtk_key_snooper_remove(snooper_id);
    snooper_installed = FALSE;
  }
#elif IM_UIM_USE_TOPLEVEL
  remove_cur_toplevel();
#endif

  uim_focus_out_context(uic->uc);

  check_helper_connection();
  uim_helper_client_focus_out(uic->uc);

  if (uic->cwin)
    gtk_widget_hide(GTK_WIDGET(uic->cwin));

  gtk_widget_hide(uic->caret_state_indicator);
}

#define WORKAROUND_BROKEN_RESET_IN_GTK	1
static void
im_uim_reset(GtkIMContext *ic)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);
#if !defined(WORKAROUND_BROKEN_RESET_IN_GTK)
  uim_reset_context(uic->uc);
  clear_cb(uic);
  update_cb(uic);
#else
  if (uic == focused_context) {
    uim_focus_out_context(uic->uc);
    uim_focus_in_context(uic->uc);
  } else {
    uim_reset_context(uic->uc);
    clear_cb(uic);
    update_cb(uic);
  }
#endif
#ifdef GDK_WINDOWING_X11
  im_uim_compose_reset(uic->compose);
#endif
}

static void
im_uim_set_use_preedit(GtkIMContext *ic, gboolean use_preedit)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);
  GtkWidget *preedit_label = NULL;

  if (use_preedit == FALSE) {
    if (!uic->preedit_window) {
      uic->preedit_window = gtk_window_new(GTK_WINDOW_POPUP);
      preedit_label = gtk_label_new("");
      gtk_container_add(GTK_CONTAINER(uic->preedit_window), preedit_label);
      gtk_widget_show(preedit_label);
    }
    uic->preedit_handler_id =
      g_signal_connect(G_OBJECT(ic), "preedit-changed",
		       G_CALLBACK(show_preedit), preedit_label);
  } else {
    if (uic->preedit_handler_id) {
      g_signal_handler_disconnect(G_OBJECT(ic), uic->preedit_handler_id);
      uic->preedit_handler_id = 0;
    }
    if (uic->preedit_window) {
      gtk_widget_destroy(uic->preedit_window);
      uic->preedit_window = NULL;
    }
  }
}

static void
im_uim_set_client_window(GtkIMContext *ic, GdkWindow *w)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);

  if (w) {
    uic->win = w;
  } else {
    uic->win = NULL;
  }
  update_client_widget(uic);
}

static UIMCandWinGtk *
im_uim_create_cand_win_gtk()
{
  UIMCandWinGtk *cwin = NULL;
  char *candwinprog; /* deprecated */
  char *style;

  candwinprog = uim_scm_symbol_value_str("uim-candwin-prog");
  style= uim_scm_symbol_value_str("candidate-window-style");

  if (candwinprog) {
    if (!strncmp(candwinprog, "uim-candwin-tbl", 15))
      cwin = UIM_CAND_WIN_GTK(uim_cand_win_tbl_gtk_new());
    else if (!strncmp(candwinprog, "uim-candwin-horizontal", 22))
      cwin = UIM_CAND_WIN_GTK(uim_cand_win_horizontal_gtk_new());
  } else {
    if (style) {
      if (!strcmp(style, "table"))
        cwin = UIM_CAND_WIN_GTK(uim_cand_win_tbl_gtk_new());
      else if (!strcmp(style, "horizontal"))
        cwin = UIM_CAND_WIN_GTK(uim_cand_win_horizontal_gtk_new());
    }
  }
  free(candwinprog);
  free(style);

  if (!cwin)
    cwin = UIM_CAND_WIN_GTK(uim_cand_win_vertical_gtk_new());

  return cwin;
}

static void
im_uim_init(IMUIMContext *uic)
{
  uic->win = NULL;
  uic->widget = NULL;
#if IM_UIM_USE_TOPLEVEL
  init_event_key_rec(&uic->event_rec);
#endif
  uic->caret_state_indicator = NULL;
  uic->pseg = NULL;
  uic->nr_psegs = 0;
  uic->prev_preedit_len = 0;

  uic->cwin = im_uim_create_cand_win_gtk();
#if IM_UIM_USE_TOPLEVEL
  cwin_list = g_list_append(cwin_list, uic->cwin);
#endif
  uic->cwin_is_active = FALSE;
  uic->preedit_window = NULL;
  uic->preedit_handler_id = 0;

  g_signal_connect(G_OBJECT(uic->cwin), "index-changed",
		   G_CALLBACK(index_changed_cb), uic);
}

static void
im_uim_finalize(GObject *obj)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(obj);

  im_uim_set_client_window(GTK_IM_CONTEXT(uic), NULL);

  uic->next->prev = uic->prev;
  uic->prev->next = uic->next;

  if (uic->cwin) {
#if IM_UIM_USE_DELAY
    cand_delay_timer_remove(uic->cwin);
#endif
    gtk_widget_destroy(GTK_WIDGET(uic->cwin));
#if IM_UIM_USE_TOPLEVEL
    cwin_list = g_list_remove(cwin_list, uic->cwin);
#endif
    uic->cwin = NULL;
  }

  if (uic->caret_state_indicator) {
    guint tag = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(uic->caret_state_indicator), "timeout-tag"));
    if (tag > 0)
      g_source_remove(tag);
    gtk_widget_destroy(uic->caret_state_indicator);
    uic->caret_state_indicator = NULL;
  }

  if (uic->preedit_handler_id) {
    g_signal_handler_disconnect(obj, uic->preedit_handler_id);
    uic->preedit_handler_id = 0;
  }
  if (uic->preedit_window) {
    gtk_widget_destroy(uic->preedit_window);
    uic->preedit_window = NULL;
  }

  uim_release_context(uic->uc);

  g_signal_handlers_disconnect_by_func(uic->slave, (gpointer)(uintptr_t)commit_cb, uic);
  g_object_unref(uic->slave);
  parent_class->finalize(obj);

  if (uic == focused_context) {
    focused_context = NULL;
    disable_focused_context = TRUE;
  }
#ifdef GDK_WINDOWING_X11
  free(uic->compose);
#endif
}

static void
im_uim_class_init(GtkIMContextClass *class)
{
  GObjectClass *object_class = G_OBJECT_CLASS(class);

  parent_class = g_type_class_peek_parent(class);
  class->set_client_window = im_uim_set_client_window;
  class->filter_keypress = im_uim_filter_keypress;
  class->get_preedit_string = im_uim_get_preedit_string;
  class->set_cursor_location = im_uim_set_cursor_location;
  class->focus_in = im_uim_focus_in;
  class->focus_out = im_uim_focus_out;
  class->reset = im_uim_reset;
  class->set_use_preedit = im_uim_set_use_preedit;

  object_class->finalize = im_uim_finalize;
}

static void
im_uim_class_finalize(GtkIMContextClass *class)
{
}


GtkIMContext *
im_module_create(const gchar *context_id)
{
  GObject *obj;
  IMUIMContext *uic;
  const char *im_name;

  g_return_val_if_fail(context_id, NULL);
  g_return_val_if_fail(!strcmp(context_id, "uim"), NULL);

  obj = g_object_new(type_im_uim, NULL);
  uic = IM_UIM_CONTEXT(obj);

  if (!uic)
    return NULL;

  im_name = uim_get_default_im_name(setlocale(LC_CTYPE, NULL));
  uic->uc = uim_create_context(uic, "UTF-8",
			       NULL, im_name,
			       uim_iconv,
			       im_uim_commit_string);
  if (uic->uc == NULL) {
    parent_class->finalize(obj);
    return NULL;
  }

  check_helper_connection();

  uim_set_preedit_cb(uic->uc, clear_cb, pushback_cb, update_cb);
  uim_set_prop_list_update_cb(uic->uc, update_prop_list_cb);
  uim_set_candidate_selector_cb(uic->uc, cand_activate_cb, cand_select_cb,
				cand_shift_page_cb, cand_deactivate_cb);
  uim_set_configuration_changed_cb(uic->uc, configuration_changed_cb);
  uim_set_im_switch_request_cb(uic->uc,
			       switch_app_global_im_cb,
			       switch_system_global_im_cb);
  uim_set_text_acquisition_cb(uic->uc, acquire_text_cb, delete_text_cb);
#if IM_UIM_USE_DELAY
  uim_set_delay_candidate_selector_cb(uic->uc, cand_activate_with_delay_cb);
#endif

  uim_prop_list_update(uic->uc);

#ifdef GDK_WINDOWING_X11
  uic->compose = im_uim_compose_new();
#endif

  /* slave exists for using gtk+'s table based input method */
  uic->slave = g_object_new(GTK_TYPE_IM_CONTEXT_SIMPLE, NULL);
  g_signal_connect(G_OBJECT(uic->slave), "commit",
		   G_CALLBACK(commit_cb), uic);

  uic->caret_state_indicator = caret_state_indicator_new();

  uic->next = context_list.next;
  uic->prev = (IMUIMContext *)&context_list;
  context_list.next->prev = uic;
  context_list.next = uic;
  return GTK_IM_CONTEXT(uic);
}

void
im_module_list(const GtkIMContextInfo ***contexts,
	       int *n_contexts)
{
  *contexts = &im_uim_info_list;
  *n_contexts = 1;
}

#if IM_UIM_USE_SNOOPER
/* snooper is not recommended! */
static gboolean
key_snoop(GtkWidget *grab_widget, GdkEventKey *key, gpointer data)
{
  if (focused_context) {
    int rv, kv, mod;

    im_uim_convert_keyevent(key, &kv, &mod);

    if (key->type == GDK_KEY_RELEASE)
      rv = uim_release_key(focused_context->uc, kv, mod);
    else
      rv = uim_press_key(focused_context->uc, kv, mod);

    if (rv)
      return FALSE;
    return TRUE;
  }

  return FALSE;
}
#endif

#if IM_UIM_USE_TOPLEVEL
static gboolean
handle_key_on_toplevel(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
  IMUIMContext *uic = data;
  /* GtkWindow *window = GTK_WINDOW(widget); */

  if (focused_context == uic) {
    int rv, kv, mod;

    store_event_key(&uic->event_rec, event);
    im_uim_convert_keyevent(event, &kv, &mod);

    if (event->type == GDK_KEY_RELEASE)
      rv = uim_release_key(focused_context->uc, kv, mod);
    else
      rv = uim_press_key(focused_context->uc, kv, mod);

    if (rv)
      return FALSE;

#if !GTK_CHECK_VERSION(2, 90, 0)
    /* FIXME: Can't compile with GSEAL_ENABLE */
    if (GTK_IS_TEXT_VIEW(uic->widget))
      GTK_TEXT_VIEW(uic->widget)->need_im_reset = TRUE;
    else if (GTK_IS_ENTRY(uic->widget)) {
      /* FIXME: Can't compile with GSEAL_ENABLE */
      if (gtk_editable_get_editable(GTK_EDITABLE(uic->widget)))
        GTK_ENTRY(uic->widget)->need_im_reset = TRUE;
    }
#endif
    return TRUE;
  }

  return FALSE;
}
#endif

void
im_module_init(GTypeModule *type_module)
{
  if (uim_counted_init() == -1)
    return;

  context_list.next = (IMUIMContext *)&context_list;
  context_list.prev = (IMUIMContext *)&context_list;
  type_im_uim = g_type_module_register_type(type_module, GTK_TYPE_IM_CONTEXT,
					    "GtkIMContextUIM", &class_info, 0);
  uim_cand_win_gtk_register_type(type_module);

#if IM_UIM_USE_SNOOPER
  /* Using snooper is not recommended! */
  snooper_id = gtk_key_snooper_install((GtkKeySnoopFunc)key_snoop, NULL);
  snooper_installed = TRUE;
#endif

  im_uim_init_modifier_keys();
#ifdef GDK_WINDOWING_X11
  im_uim_create_compose_tree();
#endif
}

void
im_module_exit(void)
{
  if (im_uim_fd != -1)
    uim_helper_close_client_fd(im_uim_fd);

#if IM_UIM_USE_SNOOPER
  gtk_key_snooper_remove(snooper_id);
#endif
#ifdef GDK_WINDOWING_X11
  im_uim_release_compose_tree();
#endif
  uim_counted_quit();
}
