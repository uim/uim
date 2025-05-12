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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <locale.h>
#include <panel-applet.h>
#include <uim/uim.h>

PanelApplet *uimapplet;

static void exec_switcher(BonoboUIComponent *uic, gpointer data, const gchar *verbname);
static void exec_pref(BonoboUIComponent *uic, gpointer data, const gchar *verbname);
static void exec_dic(BonoboUIComponent *uic, gpointer data, const gchar *verbname);
static void exec_pad(BonoboUIComponent *uic, gpointer data, const gchar *verbname);
static void exec_hand(BonoboUIComponent *uic, gpointer data, const gchar *verbname);
static void exec_help(BonoboUIComponent *uic, gpointer data, const gchar *verbname);
static void display_about_dialog(BonoboUIComponent *uic, gpointer data, const gchar *verbname);

extern GtkWidget *uim_toolbar_applet_new(void);
extern void uim_toolbar_launch_helper_application(const char *command);


static const BonoboUIVerb uim_menu_verbs[] = {
  BONOBO_UI_VERB("UimExecSwitcher", exec_switcher),
  BONOBO_UI_VERB("UimExecPref", exec_pref),
  BONOBO_UI_VERB("UimExecDic", exec_dic),
  BONOBO_UI_VERB("UimExecPad", exec_pad),
  BONOBO_UI_VERB("UimExecHand", exec_hand),
  BONOBO_UI_VERB("UimExecHelp", exec_help),
  BONOBO_UI_VERB("UimAbout", display_about_dialog),
  BONOBO_UI_VERB_END
};


static const char uim_menu_xml[] =
  "<popup name=\"button3\">\n"
  "   <menuitem name=\"Switcher Item\" verb=\"UimExecSwitcher\" _label=\"Switch input method\"\n"
  "             pixtype=\"filename\" pixname=\""UIM_PIXMAPSDIR"/im_switcher.png\"/>\n"
  "   <menuitem name=\"Pref Item\" verb=\"UimExecPref\" _label=\"Preference\"\n"
  "             pixtype=\"stock\" pixname=\"preferences\"/>\n"
  "   <menuitem name=\"Dic Item\" verb=\"UimExecDic\" _label=\"Japanese dictionary editor\"\n"
  "             pixtype=\"filename\" pixname=\""UIM_PIXMAPSDIR"/uim-dict.png\"/>\n"
  "   <menuitem name=\"Pad Item\" verb=\"UimExecPad\" _label=\"Input pad\"\n"
  "             pixtype=\"stock\" pixname=\"bold\"/>\n"
  "   <menuitem name=\"Hand Item\" verb=\"UimExecHand\" _label=\"Handwriting input pad\"\n"
  "             pixtype=\"stock\" pixname=\"edit\"/>\n"
  "   <menuitem name=\"Help Item\" verb=\"UimExecHelp\" _label=\"Help\"\n"
  "             pixtype=\"stock\" pixname=\"help\"/>\n"
  "   <menuitem name=\"About Item\" verb=\"UimAbout\" _label=\"About\"\n"
  "             pixtype=\"stock\" pixname=\"gnome-stock-about\"/>\n"
  "</popup>\n";



static void
exec_switcher(BonoboUIComponent *uic, gpointer data, const gchar *verbname)
{
  uim_toolbar_launch_helper_application("uim-im-switcher-gtk");
}

static void
exec_pref(BonoboUIComponent *uic, gpointer data, const gchar *verbname)
{
  uim_toolbar_launch_helper_application("uim-pref-gtk");
}

static void
exec_dic(BonoboUIComponent *uic, gpointer data, const gchar *verbname)
{
  uim_toolbar_launch_helper_application("uim-dict-gtk");
}

static void
exec_pad(BonoboUIComponent *uic, gpointer data, const gchar *verbname)
{
  uim_toolbar_launch_helper_application("uim-input-pad-ja");
}

static void
exec_hand(BonoboUIComponent *uic, gpointer data, const gchar *verbname)
{
  uim_toolbar_launch_helper_application("uim-tomoe-gtk");
}

static void
exec_help(BonoboUIComponent *uic, gpointer data, const gchar *verbname)
{
  uim_toolbar_launch_helper_application("uim-help");
}

/* Just the about window... If it's already open, just focus it */
static void
display_about_dialog(BonoboUIComponent *uic, gpointer data,
		     const gchar *verbname)
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
  uimapplet = applet;

  if (strcmp(iid, "OAFIID:GNOME_UimApplet") != 0)
    return FALSE;

  uim_init();

  toolbar = (GtkWidget*)uim_toolbar_applet_new();

  gtk_container_add(GTK_CONTAINER(applet), toolbar);

  gtk_widget_show_all(GTK_WIDGET(applet));

  panel_applet_setup_menu(applet, uim_menu_xml, uim_menu_verbs, toolbar);
#if LIBPANEL_APPLET_HAVE_SET_BACKGROUND_WIDGET
  panel_applet_set_background_widget(applet, GTK_WIDGET(applet));
#endif

  return TRUE;
}



PANEL_APPLET_BONOBO_FACTORY("OAFIID:GNOME_UimApplet_Factory",
                            PANEL_TYPE_APPLET,
                            "uim Applet for GNOME",
                            "0",
                            (PanelAppletFactoryCallback)uim_applet_new,
                            NULL)
