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
#include <gtk/gtk.h>
#include <gtk/gtkimcontext.h>
#include <gtk/gtkimmodule.h>
#include <gdk/gdkkeysyms.h>
#include <gdk/gdkx.h>
#include <glib/gprintf.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <locale.h>

#include "uim/uim.h"
#include "uim/uim-util.h"
#include "uim/uim-helper.h"
#include "uim/uim-im-switcher.h"
#include "uim/config.h"
#include "uim/gettext.h"
#include "uim/uim-compat-scm.h"
#include "uim-cand-win-gtk.h"
#include "caret-state-indicator.h"

/* exported symbols */
GtkIMContext *im_module_create(const gchar *context_id);
void im_module_list(const GtkIMContextInfo ***contexts, int *n_contexts);
void im_module_exit(void);
void im_module_init(GTypeModule *type_module);

/**/
#define NR_CANDIDATES 20
#define DEFAULT_SEPARATOR_STR "|"

static int im_uim_fd = -1;
static unsigned int read_tag;
static guint snooper_id;

struct preedit_segment {
  int attr;
  char *str;
};

typedef struct _IMUIMContext {
  /**/
  struct _GtkIMContext parent;
  struct _GtkIMContext *slave;
  uim_context uc;
  UIMCandWinGtk *cwin;
  gboolean cwin_is_active;
  int nr_psegs;
  struct preedit_segment *pseg;
  /**/
  GtkWidget *menu;
  GdkWindow *win;
  GdkWindow *toplevel;
  GtkWidget *caret_state_indicator;
  GdkRectangle preedit_pos; /* preedit_pos not always point the cursor location */
  /**/
  struct _IMUIMContext *prev, *next;
} IMUIMContext;

static IMUIMContext context_list;
static IMUIMContext *focused_context = NULL;
static gboolean disable_focused_context = FALSE;
static gboolean snooper_installed = FALSE;

static GObjectClass *parent_class;

typedef struct _IMContextUIMClass
{
  GtkIMContextClass parent_class;
} IMContextUIMClass;


static void im_uim_class_init(GtkIMContextClass *class);
static void im_uim_class_finalize(GtkIMContextClass *class);
static void im_uim_init(IMUIMContext *uic);

static void show_preedit(GtkIMContext *ic, GtkWidget *preedit_label);

static void im_uim_helper_disconnect_cb(void);
/* #ifndef GDK_DISABLE_DEPRECATED */
static gboolean helper_read_cb(GIOChannel *channel, GIOCondition c, gpointer p);
static GdkFilterReturn
toplevel_window_candidate_cb(GdkXEvent *xevent, GdkEvent *ev, gpointer data);

static void
im_uim_parse_helper_str(const char *str);

static gboolean get_user_defined_color(PangoColor *color, const gchar *uim_symbol);

static gboolean
uim_key_snoop(GtkWidget *grab_widget, GdkEventKey *key, gpointer data);

static const GTypeInfo class_info = {
  sizeof(IMContextUIMClass),
  (GBaseInitFunc) NULL,
  (GBaseFinalizeFunc) NULL,
  (GClassInitFunc) im_uim_class_init,
  (GClassFinalizeFunc)im_uim_class_finalize,
  NULL,/*for class data*/
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


static void
im_uim_commit_string(void *ptr, const char *str)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  uim_bool show_state;
  gint x, y;

  g_return_if_fail(str);
  g_signal_emit_by_name(uic, "commit", str);

  show_state = uim_scm_symbol_value_bool("bridge-show-input-state?");
  if (show_state == UIM_TRUE) {
    gdk_window_get_origin(uic->win, &x, &y);
    caret_state_indicator_update(uic->caret_state_indicator, x, y, NULL);
  }
}

static void
clear_cb(void *ptr)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  int i;
  for (i = 0; i < uic->nr_psegs; i++) {
    free(uic->pseg[i].str);
  }
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
		      sizeof(struct preedit_segment) *
		      (uic->nr_psegs + 1));
  uic->pseg[uic->nr_psegs].str = g_strdup(str);
  uic->pseg[uic->nr_psegs].attr = attr;
  uic->nr_psegs ++;
}

static void
update_cb(void *ptr)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  g_return_if_fail(uic);
  g_signal_emit_by_name(uic, "preedit_changed");
}

static int
convert_keyval(int key)
{
  switch (key) {
  case GDK_BackSpace: return UKey_Backspace;
  case GDK_Delete: return UKey_Delete;
  case GDK_Escape: return UKey_Escape;
  case GDK_Tab: return UKey_Tab;
  case GDK_Return: return UKey_Return;
  case GDK_Left: return UKey_Left;
  case GDK_Up: return UKey_Up;
  case GDK_Right: return UKey_Right;
  case GDK_Down: return UKey_Down;
  case GDK_Prior: return UKey_Prior;
  case GDK_Next: return UKey_Next;
  case GDK_Home: return UKey_Home;
  case GDK_End: return UKey_End;
  case GDK_Kanji:
  case GDK_Zenkaku_Hankaku: return UKey_Zenkaku_Hankaku;
  case GDK_Multi_key: return UKey_Multi_key;
  case GDK_Mode_switch: return UKey_Mode_switch;
  case GDK_Henkan_Mode: return UKey_Henkan_Mode;
  case GDK_Muhenkan: return UKey_Muhenkan;
  case GDK_Shift_L: return UKey_Shift_key;
  case GDK_Shift_R: return UKey_Shift_key;
  case GDK_Control_L: return UKey_Control_key;
  case GDK_Control_R: return UKey_Control_key;
  case GDK_Alt_L: return UKey_Alt_key;
  case GDK_Alt_R: return UKey_Alt_key;
  case GDK_Meta_L: return UKey_Meta_key;
  case GDK_Meta_R: return UKey_Meta_key;
  case GDK_Super_L: return UKey_Super_key;
  case GDK_Super_R: return UKey_Super_key;
  case GDK_Hyper_L: return UKey_Hyper_key;
  case GDK_Hyper_R: return UKey_Hyper_key;
  }

  if (key >= GDK_F1 && key <= GDK_F35) {
    return key - GDK_F1 + UKey_F1;
  }
  if (key >= GDK_KP_0 && key <= GDK_KP_9) {
    return key - GDK_KP_0 + UKey_0;
  }
  if (key < 256) {
    return key;
  }
  return UKey_Other;
}

static int
convert_modifier(int mod)
{
  int rv = 0;
  if (mod & GDK_SHIFT_MASK) {
    rv |= UMod_Shift;
  }
  if (mod & GDK_CONTROL_MASK) {
    rv |= UMod_Control;
  }
  if (mod & GDK_MOD1_MASK) {
    rv |= UMod_Alt;
  }
  if (mod & GDK_MOD3_MASK) {  /* assuming mod3 */
    rv |= UMod_Super;
  }
  if (mod & GDK_MOD4_MASK) {  /* assuming mod4 */
    rv |= UMod_Hyper;
  }
  return rv;
}


/*
 * KEY EVENT HANDLER
 */
static gboolean
filter_keypress(GtkIMContext *ic,
		GdkEventKey *key)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);

  /*** Hack for combination of xchat + GTK+ 2.6 ***/
  if (snooper_installed == FALSE) {
    int rv;
    int kv = convert_keyval(key->keyval);
    int mod = convert_modifier(key->state);

    if (key->type == GDK_KEY_RELEASE) {
      rv = uim_release_key(uic->uc, kv, mod);
    } else {
      rv = uim_press_key(uic->uc, kv, mod);
    }
    if (rv) {
      return gtk_im_context_filter_keypress(uic->slave, key);
    }
    return TRUE;
  }
  /*** Hack for combination of xchat + GTK+ 2.6 ***/

  return gtk_im_context_filter_keypress(uic->slave, key);
}

static gboolean
get_user_defined_color(PangoColor *color, const gchar *uim_symbol)
{
  gboolean parsed = FALSE;
  gchar *literal = uim_symbol_value_str(uim_symbol);
  if (literal != NULL && literal[0] != '\0') {
    parsed = pango_color_parse(color, literal);
  }
  g_free(literal);
  return parsed;
}

static char *
get_preedit_segment(struct preedit_segment *ps,
		    PangoAttrList *attrs,
		    char *str)
{
  PangoAttribute *attr;
  const gchar *segment_str = ps->str;
  if ((ps->attr & UPreeditAttr_Separator) && !strcmp(segment_str, "")) {
    segment_str = DEFAULT_SEPARATOR_STR;
  }
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
      if (get_user_defined_color(&color, separator_fg_symbol))
      {
	attr = pango_attr_foreground_new(color.red, color.green, color.blue);
	attr->start_index = begin;
	attr->end_index = end;
	pango_attr_list_change(attrs, attr);
      }

      if (get_user_defined_color(&color, separator_bg_symbol))
      {
	attr = pango_attr_background_new(color.red, color.green, color.blue);
	attr->start_index = begin;
	attr->end_index = end;
	pango_attr_list_change(attrs, attr);
      }
    } else if (ps->attr & UPreeditAttr_Reverse) {
      if (get_user_defined_color(&color, "reversed-preedit-foreground")
	  || pango_color_parse(&color, "#fff"))
      {
	attr = pango_attr_foreground_new(color.red, color.green, color.blue);
	attr->start_index = begin;
	attr->end_index = end;
	pango_attr_list_change(attrs, attr);
      }

      if (get_user_defined_color(&color, "reversed-preedit-background")
	  || pango_color_parse(&color, "#000"))
      {
	attr = pango_attr_background_new(color.red, color.green, color.blue);
	attr->start_index = begin;
	attr->end_index = end;
	pango_attr_list_change(attrs, attr);
      }
    }
  }
  str = (char *)realloc(str, strlen(str) + strlen(segment_str) + 1);
  strcat(str, segment_str);
  return str;
}


static void
im_uim_get_preedit_string(GtkIMContext *ic, gchar **str,
			  PangoAttrList **attrs,
			  gint *cursor_pos)
{
  char *tmp;
  int i, pos = 0;
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);

  if (attrs) {
    *attrs = pango_attr_list_new();
  }
  tmp = g_strdup("");
  for (i = 0; i < uic->nr_psegs; i++) {
    if (uic->pseg[i].attr & UPreeditAttr_Cursor) {
      pos = g_utf8_strlen(tmp, -1);
    }
    if (attrs) {
      tmp = get_preedit_segment(&uic->pseg[i], *attrs, tmp);
    } else {
      tmp = get_preedit_segment(&uic->pseg[i], NULL, tmp);
    }
  }
  if (cursor_pos) {
    *cursor_pos = pos;
  }
  if (str) {
    *str = tmp;
  } else {
    free(tmp);
  }
}

static void
im_uim_set_cursor_location(GtkIMContext *ic,
			   GdkRectangle *area)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);

  uic->preedit_pos = *area;
  uim_cand_win_gtk_set_cursor_location(uic->cwin, area);
  caret_state_indicator_set_cursor_location(uic->caret_state_indicator, area);
}


static void
im_uim_commit_cb(GtkIMContext *ic,
		 const gchar *str,
		 IMUIMContext *is)
{
  g_return_if_fail(str);
  g_signal_emit_by_name(is, "commit", str);
}

static void
check_helper_connection()
{
  if (im_uim_fd < 0) {
    im_uim_fd = uim_helper_init_client_fd(im_uim_helper_disconnect_cb);
    if (im_uim_fd >= 0) {
      GIOChannel *channel;
      channel = g_io_channel_unix_new(im_uim_fd);
      read_tag = g_io_add_watch(channel, G_IO_IN | G_IO_HUP | G_IO_ERR,
				helper_read_cb, NULL);
      g_io_channel_unref(channel);
    }
  }
}

static void
focus_in(GtkIMContext *ic)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);
  IMUIMContext *cc;

  focused_context = uic;
  disable_focused_context = FALSE;

  /* XXX:Use of key snooper is not recommended way!! */
  if (snooper_installed == FALSE) {
    snooper_id = gtk_key_snooper_install((GtkKeySnoopFunc)uim_key_snoop, NULL );
    snooper_installed = TRUE;
  }

  check_helper_connection();

  uim_helper_client_focus_in(uic->uc);

  uim_prop_list_update(uic->uc);
  uim_prop_label_update(uic->uc);

  for (cc = context_list.next; cc != &context_list; cc = cc->next) {
    if (cc != uic && cc->cwin) {
      gtk_widget_hide(GTK_WIDGET(cc->cwin));
    }
  }

  if (uic->cwin && uic->cwin_is_active) {
    gtk_widget_show(GTK_WIDGET(uic->cwin));
  }
}

static void
focus_out(GtkIMContext *ic)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);

  if (snooper_installed == TRUE) {
    gtk_key_snooper_remove(snooper_id);
    snooper_installed = FALSE;
  }

  check_helper_connection();

  uim_helper_client_focus_out(uic->uc);

  if (uic->cwin) {
    gtk_widget_hide(GTK_WIDGET(uic->cwin));
  }
  gtk_widget_hide(uic->caret_state_indicator);
}

static void
im_uim_reset(GtkIMContext *ic)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);
  uim_reset_context(uic->uc);
}

static void
set_use_preedit(GtkIMContext *ic, gboolean use_preedit)
{
  GtkWidget *preedit_window;
  GtkWidget *preedit_label;
  if (use_preedit == FALSE ) {
    preedit_window = gtk_window_new(GTK_WINDOW_POPUP);
    preedit_label = gtk_label_new("");
    gtk_container_add(GTK_CONTAINER(preedit_window), preedit_label);

    g_signal_connect(G_OBJECT(ic), "preedit-changed",
		     G_CALLBACK(show_preedit), preedit_label );
    gtk_widget_show_all(preedit_window);
  }
}


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
      gint x,y,w,h;
      PangoLayout *layout;

      gtk_label_set_text(GTK_LABEL(preedit_label), str);
      gtk_label_set_attributes(GTK_LABEL(preedit_label), attrs);

      gdk_window_get_origin(uic->win, &x, &y);

      gtk_window_move(GTK_WINDOW(preedit_window),
		      x + uic->preedit_pos.x,
		      y + uic->preedit_pos.y );

      layout = gtk_label_get_layout(GTK_LABEL(preedit_label));

      pango_layout_get_cursor_pos(layout, 0, NULL, NULL);

      pango_layout_get_pixel_size(layout, &w, &h);
      gtk_window_resize(GTK_WINDOW(preedit_window), w, h);

      gtk_widget_show(preedit_window);

  } else {
      gtk_label_set_text(GTK_LABEL(preedit_label), "");
      gtk_widget_hide(preedit_window);
      gtk_window_resize(GTK_WINDOW(preedit_window), 0, 0);
  }
  g_free(str);
  pango_attr_list_unref(attrs);

}

static GdkFilterReturn
toplevel_window_candidate_cb(GdkXEvent *xevent, GdkEvent *ev, gpointer data)
{
  IMUIMContext *uic = data;

  if (!uic)
    return GDK_FILTER_CONTINUE;

  if (uic->cwin && uic->cwin_is_active) {
    gint x, y, width, height, depth;

    gdk_window_get_geometry(uic->win, &x, &y, &width, &height, &depth);
    gdk_window_get_origin(uic->win, &x, &y);
    uim_cand_win_gtk_layout(uic->cwin, x, y, width, height);
  }
  return GDK_FILTER_CONTINUE;
}


static void
set_client_window(GtkIMContext *ic, GdkWindow *w)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(ic);

  if (w) {
    g_object_ref(w);
    uic->win = w;
  } else {
    if (uic->win)
      g_object_unref(uic->win);
    uic->win = NULL;
  }
}

static void
index_changed_cb(UIMCandWinGtk *cwin, IMUIMContext *uic)
{
  g_return_if_fail(UIM_IS_CAND_WIN_GTK(cwin));

  uim_set_candidate_index(uic->uc, uim_cand_win_gtk_get_index(cwin));
}

static void
im_uim_init(IMUIMContext *uic)
{
  uic->win = NULL;
  uic->menu = NULL;
  uic->pseg = 0;
  uic->nr_psegs = 0;

  uic->cwin = uim_cand_win_gtk_new();
  uic->cwin_is_active = FALSE;
  g_signal_connect(G_OBJECT(uic->cwin), "index-changed",
		   G_CALLBACK(index_changed_cb), uic);
}


/*
 * DESTRUCTOR
 */
static void
im_uim_finalize(GObject *obj)
{
  IMUIMContext *uic = IM_UIM_CONTEXT(obj);
  /* set_client_window(GTK_IM_CONTEXT(uic), NULL); */

  uic->next->prev = uic->prev;
  uic->prev->next = uic->next;

  if (uic->menu) {
    gtk_widget_destroy(uic->menu);
    uic->menu = NULL;
  }
  if (uic->cwin) {
    gtk_widget_destroy(GTK_WIDGET(uic->cwin));
    uic->cwin = NULL;
  }

  uim_release_context(uic->uc);

  g_signal_handlers_disconnect_by_func(uic->slave,
				       (gpointer)im_uim_commit_cb,
				       uic);
  g_object_unref(uic->slave);
  parent_class->finalize(obj);

  if (uic == focused_context) {
    focused_context = NULL;
    disable_focused_context = TRUE;
  }
}

static void
im_uim_class_init(GtkIMContextClass *class)
{
  GObjectClass *object_class = G_OBJECT_CLASS(class);
  parent_class = g_type_class_peek_parent(class);
  class->set_client_window = set_client_window;
  class->filter_keypress = filter_keypress;
  class->get_preedit_string = im_uim_get_preedit_string;
  class->set_cursor_location = im_uim_set_cursor_location;
  class->focus_in = focus_in;
  class->focus_out = focus_out;
  class->reset = im_uim_reset;
  class->set_use_preedit = set_use_preedit;

  object_class->finalize = im_uim_finalize;
}


static void
im_uim_class_finalize(GtkIMContextClass *class)
{

}

static void
update_prop_list_cb(void *ptr, const char *str)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  GString *tmp;

  if (uic != focused_context)
    return;

  tmp = g_string_new("");
  g_string_printf(tmp, "prop_list_update\ncharset=UTF-8\n%s", str);

  uim_helper_send_message(im_uim_fd, tmp->str);
  g_string_free(tmp, TRUE);
}

static void
update_prop_label_cb(void *ptr, const char *str)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  GString *tmp;
  gint x, y;
  uim_bool show_state;

  if (uic != focused_context)
    return;

  tmp = g_string_new("");
  g_string_printf(tmp, "prop_label_update\ncharset=UTF-8\n%s", str);

  uim_helper_send_message(im_uim_fd, tmp->str);
  g_string_free(tmp, TRUE);

  show_state = uim_scm_symbol_value_bool("bridge-show-input-state?");
  if (show_state == UIM_TRUE) {
    gint timeout;
    gdk_window_get_origin(uic->win, &x, &y);
    caret_state_indicator_update(uic->caret_state_indicator, x, y, str);
    timeout = uim_scm_symbol_value_int("bridge-show-input-state-time-length");
    if(timeout != 0)
      caret_state_indicator_set_timeout(uic->caret_state_indicator, timeout * 1000);
    gtk_widget_show_all(uic->caret_state_indicator);
  }
}

static void
im_uim_send_im_list(void)
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
    const char *name = uim_get_im_name(focused_context->uc, i);
    /* return value of uim_get_im_language() is an ISO 639-1
       compatible language code such as "ja". Since it is unfriendly
       for human reading, we convert it into friendly one by
       uim_get_language_name_from_locale() here */
    const char *langcode = uim_get_im_language(focused_context->uc, i);
    const char *lang = uim_get_language_name_from_locale(langcode);
    const char *short_desc = uim_get_im_short_desc(focused_context->uc, i);

    g_string_append(msg, name);
    g_string_append(msg, "\t");
    if (lang) g_string_append(msg, lang);
    g_string_append(msg, "\t");
    if (short_desc) g_string_append(msg, short_desc);
    g_string_append(msg, "\t");
    if (strcmp(name, current_im_name) == 0) {
      g_string_append(msg, "selected");
    }
    g_string_append(msg, "\n");
  }
  uim_helper_send_message(im_uim_fd, msg->str);
  g_string_free(msg, TRUE);
}

static void
cand_activate_cb(void *ptr, int nr, int display_limit)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  gint x, y, width, height, depth;
  GSList *list = NULL;
  uim_candidate cand;
  gint i;

  uic->cwin_is_active = TRUE;

  for (i = 0; i < nr; i++) {
    cand = uim_get_candidate(uic->uc, i, display_limit ? i % display_limit : i);
    list = g_slist_append(list, cand);
  }

  uim_cand_win_gtk_set_candidates(uic->cwin, display_limit, list);

  g_slist_foreach(list, (GFunc)uim_candidate_free, NULL);
  g_slist_free(list);

  gdk_window_get_geometry(uic->win, &x, &y, &width, &height, &depth);
  gdk_window_get_origin(uic->win, &x, &y);
  uim_cand_win_gtk_layout(uic->cwin, x, y, width, height);
  gtk_widget_show(GTK_WIDGET(uic->cwin));

  if (uic->win) {
    GdkWindow *toplevel;

    toplevel = gdk_window_get_toplevel(uic->win);
    gdk_window_add_filter(toplevel,
			  toplevel_window_candidate_cb,
			  uic);
  }
}



/* This function called by libuim.
   Selected by keyboard, etc. */
static void
cand_select_cb(void *ptr, int index)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  gint x, y, width, height, depth;

  gdk_window_get_geometry(uic->win, &x, &y, &width, &height, &depth);
  gdk_window_get_origin(uic->win, &x, &y);
  uim_cand_win_gtk_layout(uic->cwin, x, y, width, height);
  g_signal_handlers_block_by_func(uic->cwin, (gpointer)index_changed_cb, uic);
  uim_cand_win_gtk_set_index(uic->cwin, index);
  g_signal_handlers_unblock_by_func(uic->cwin, (gpointer)index_changed_cb, uic);
}

static void
cand_shift_page_cb(void *ptr, int direction)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;
  gint x, y, width, height, depth;

  gdk_window_get_geometry(uic->win, &x, &y, &width, &height, &depth);
  gdk_window_get_origin(uic->win, &x, &y);
  uim_cand_win_gtk_layout(uic->cwin, x, y, width, height);
  g_signal_handlers_block_by_func(uic->cwin, (gpointer)index_changed_cb, uic);
  uim_cand_win_gtk_shift_page(uic->cwin, direction);
  uim_set_candidate_index(uic->uc, uic->cwin->candidate_index);
  g_signal_handlers_unblock_by_func(uic->cwin, (gpointer)index_changed_cb, uic);
}

static void
cand_deactivate_cb(void *ptr)
{
  IMUIMContext *uic = (IMUIMContext *)ptr;

  uic->cwin_is_active = FALSE;

  if (uic->cwin) {
    gtk_widget_hide(GTK_WIDGET(uic->cwin));
    uim_cand_win_gtk_clear_candidates(uic->cwin);
  }
  if (uic->win) {
    GdkWindow *toplevel;

    toplevel = gdk_window_get_toplevel(uic->win);
    gdk_window_remove_filter(toplevel, toplevel_window_candidate_cb, uic);
  }
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

  /**/

  uim_set_preedit_cb(uic->uc, clear_cb,
		     pushback_cb, update_cb);
  uim_set_prop_list_update_cb(uic->uc,
			      update_prop_list_cb);
  uim_set_prop_label_update_cb(uic->uc,
			      update_prop_label_cb);
  uim_set_candidate_selector_cb(uic->uc,
				cand_activate_cb,
				cand_select_cb,
				cand_shift_page_cb,
				cand_deactivate_cb);

  uim_prop_list_update(uic->uc);


  /* slave exists for using gtk+'s table based input method */
  uic->slave = g_object_new(GTK_TYPE_IM_CONTEXT_SIMPLE,
			    NULL);
  g_signal_connect(G_OBJECT(uic->slave), "commit",
		   G_CALLBACK(im_uim_commit_cb), uic);
  
  uic->caret_state_indicator = caret_state_indicator_new();

  /**/
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

static void
im_uim_parse_helper_str_im_change(const char *str)
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
im_uim_parse_helper_str(const char *str)
{
  gchar **lines;

  if (g_str_has_prefix(str, "im_change") == TRUE) {
    im_uim_parse_helper_str_im_change(str);
  } else if (g_str_has_prefix(str, "prop_update_custom") == TRUE) {
    IMUIMContext *cc;
    lines = g_strsplit(str, "\n", 0);
    if (lines && lines[0] && lines[1] && lines[2]) {
      for (cc = context_list.next; cc != &context_list; cc = cc->next) {
	uim_prop_update_custom(cc->uc, lines[1], lines[2]);
	break;  /* all custom variables are global */
      }
      g_strfreev(lines);
    }
  } else if (focused_context && !disable_focused_context) {
    if (g_str_has_prefix(str, "prop_list_get") == TRUE) {
      uim_prop_list_update(focused_context->uc);
    } else if (g_str_has_prefix(str, "prop_label_get") == TRUE) {
      uim_prop_label_update(focused_context->uc);
    } else if (g_str_has_prefix(str, "prop_activate") == TRUE) {
      lines = g_strsplit(str, "\n", 0);
      if (lines && lines[0]) {
	uim_prop_activate(focused_context->uc, lines[1]);
	g_strfreev(lines);
      }
    } else if (g_str_has_prefix(str, "im_list_get") == TRUE) {
      im_uim_send_im_list();
    } else if (g_str_has_prefix(str, "commit_string")) {
      lines = g_strsplit(str, "\n", 0);
      if (lines && lines[0] && lines[1])
	g_signal_emit_by_name(focused_context, "commit", lines[1]);
    } else if (g_str_has_prefix(str, "focus_in") == TRUE) {
      disable_focused_context = TRUE;
      /* We shouldn't do "focused_context = NULL" here, because some
	 window manager has some focus related bugs. */
    }
  }
}

static gboolean
helper_read_cb(GIOChannel *channel, GIOCondition c, gpointer p)
{
  char *tmp;
  int fd = g_io_channel_unix_get_fd(channel);
  uim_helper_read_proc(fd);
  while ((tmp = uim_helper_get_message())) {
    im_uim_parse_helper_str(tmp);
    free(tmp);
  }
  return TRUE;
}

static void
im_uim_helper_disconnect_cb(void)
{
  im_uim_fd = -1;
  g_source_remove(read_tag);
}

/* XXX:This is not a recommended way!! */
static gboolean
uim_key_snoop(GtkWidget *grab_widget, GdkEventKey *key, gpointer data)
{
  if (focused_context) {
    int rv;
    int kv = convert_keyval(key->keyval);
    int mod = convert_modifier(key->state);

    if (key->type == GDK_KEY_RELEASE) {
      rv = uim_release_key(focused_context->uc, kv, mod);
    } else {
      rv = uim_press_key(focused_context->uc, kv, mod);
    }
    if (rv) {
      return FALSE;
    }
    return TRUE;
  }

  return FALSE;
}

void
im_module_init(GTypeModule *type_module)
{
  if (uim_init() == -1) {
    return;
  }

  context_list.next = (IMUIMContext *)&context_list;
  context_list.prev = (IMUIMContext *)&context_list;
  type_im_uim =
    g_type_module_register_type(type_module,
				GTK_TYPE_IM_CONTEXT,
				"GtkIMContextUIM",
				&class_info, 0);
  uim_cand_win_gtk_register_type(type_module);

  /* XXX:This is not recommended way!! */
  snooper_id = gtk_key_snooper_install((GtkKeySnoopFunc)uim_key_snoop, NULL );
  snooper_installed = TRUE;
}

void
im_module_exit(void)
{
  if (im_uim_fd != -1) {
    uim_helper_close_client_fd(im_uim_fd);
  }
  gtk_key_snooper_remove(snooper_id);
  uim_quit();
}
