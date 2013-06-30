/*

  Copyright (c) 2003-2013 uim Project http://code.google.com/p/uim/

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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <panel-applet.h>
#include <uim/uim.h>
#include <uim/gettext.h>

PanelApplet *uimapplet;

static void exec_switcher(GtkAction *action, gpointer data);
static void exec_pref(GtkAction *action, gpointer data);
static void exec_dic(GtkAction *action, gpointer data);
static void exec_pad(GtkAction *action, gpointer data);
static void exec_hand(GtkAction *action, gpointer data);
static void exec_help(GtkAction *action, gpointer data);
static void display_about_dialog(GtkAction *action, gpointer data);

extern GtkWidget *uim_toolbar_applet_new(void);
extern void uim_toolbar_launch_helper_application(const char *command);


static const GtkActionEntry uim_menu_actions[] = {
  {"Switcher", "im_switcher",
    N_("Switch input method"), NULL, NULL, G_CALLBACK(exec_switcher)},
  {"Pref", GTK_STOCK_PREFERENCES,
    N_("Preference"), NULL, NULL, G_CALLBACK(exec_pref)},
  {"Dic", "uim-dict",
    N_("Japanese dictionary editor"), NULL, NULL, G_CALLBACK(exec_dic)},
  {"Pad", GTK_STOCK_BOLD,
    N_("Input pad"), NULL, NULL, G_CALLBACK(exec_pad)},
  {"Hand", GTK_STOCK_EDIT,
    N_("Handwriting input pad"), NULL, NULL, G_CALLBACK(exec_hand)},
  {"Help", GTK_STOCK_HELP,
    N_("Help"), NULL, NULL, G_CALLBACK(exec_help)},
  {"About", GTK_STOCK_ABOUT,
    N_("About"), NULL, NULL, G_CALLBACK(display_about_dialog)}
};

static const char uim_menu_xml[] =
  "<menuitem action=\"Switcher\"/>"
  "<menuitem action=\"Pref\"/>"
  "<menuitem action=\"Dic\"/>"
  "<menuitem action=\"Pad\"/>"
  "<menuitem action=\"Hand\"/>"
  "<menuitem action=\"Help\"/>"
  "<menuitem action=\"About\"/>";

static void
exec_switcher(GtkAction *action, gpointer data)
{
  uim_toolbar_launch_helper_application("uim-im-switcher-gtk3");
}

static void
exec_pref(GtkAction *action, gpointer data)
{
  uim_toolbar_launch_helper_application("uim-pref-gtk3");
}

static void
exec_dic(GtkAction *action, gpointer data)
{
  uim_toolbar_launch_helper_application("uim-dict-gtk3");
}

static void
exec_pad(GtkAction *action, gpointer data)
{
  uim_toolbar_launch_helper_application("uim-input-pad-ja-gtk3");
}

static void
exec_hand(GtkAction *action, gpointer data)
{
  uim_toolbar_launch_helper_application("uim-tomoe-gtk");
}

static void
exec_help(GtkAction *uic, gpointer data)
{
  uim_toolbar_launch_helper_application("uim-help");
}

/* Just the about window... If it's already open, just focus it */
static void
display_about_dialog(GtkAction *action, gpointer data)
{
  GdkPixbuf *icon = NULL;
  const gchar *authors[] = {"uim Project", NULL};
  /* Feel free to put your names here translators */
  gchar *translators = _("TRANSLATORS");
  icon = gdk_pixbuf_new_from_file(UIM_PIXMAPSDIR "/uim-icon.png", NULL);

  gtk_show_about_dialog(NULL,
			   "program-name", _("uim Applet"),
			   "version", VERSION,
			   "copyright", "Copyright \xc2\xa9 2003-2013 uim Project.",
			   "comments", _("Applet for indicating uim's status"),
			   "authors", authors,
			   "translator-credits",
			       strcmp("TRANSLATORS", translators) ? translators
			   				      : NULL,
			   "icon", icon,
			   "logo", icon, NULL);

  if (icon) {
    g_object_unref(icon);
  }
}

static gboolean
uim_applet_new(PanelApplet *applet, const gchar *iid, gpointer data)
{
  GtkWidget *toolbar;
  GtkActionGroup *action_group;
  
  uimapplet = applet;

  if (strcmp(iid, "UimApplet") != 0)
    return FALSE;

  uim_init();

  toolbar = (GtkWidget*)uim_toolbar_applet_new();

  gtk_container_add(GTK_CONTAINER(applet), toolbar);

  gtk_widget_show_all(GTK_WIDGET(applet));

  action_group = gtk_action_group_new("uim Applet Actions");
  gtk_action_group_set_translation_domain(action_group, GETTEXT_PACKAGE);
  gtk_action_group_add_actions(action_group, uim_menu_actions,
      G_N_ELEMENTS(uim_menu_actions), toolbar);
  panel_applet_setup_menu(applet, uim_menu_xml, action_group);
#if LIBPANEL_APPLET_HAVE_SET_BACKGROUND_WIDGET
  panel_applet_set_background_widget(applet, GTK_WIDGET(applet));
#endif
  g_object_unref(action_group);

  return TRUE;
}



PANEL_APPLET_OUT_PROCESS_FACTORY("UimAppletFactory",
                            PANEL_TYPE_APPLET,
                            (PanelAppletFactoryCallback)uim_applet_new,
                            NULL)
