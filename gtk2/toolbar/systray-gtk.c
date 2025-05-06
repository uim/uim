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
#include "uim/gettext.h"
#include "uim/uim.h"
#include "uim/uim-helper.h"
#include <gtk/gtk.h>
#include "eggtrayicon.h"

extern GtkWidget *uim_toolbar_trayicon_new(void);
extern void uim_toolbar_check_helper_connection(GtkWidget *widget);
extern void uim_toolbar_get_im_list(void);


static void
embedded_cb(GtkWidget *widget, gpointer user_data)
{
  uim_toolbar_check_helper_connection(user_data);
  uim_helper_client_get_prop_list();
  uim_toolbar_get_im_list();

  gtk_widget_show_all(user_data);
}

int
main(int argc, char *argv[])
{
  GtkWidget *icon;
  EggTrayIcon *tray;

  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8");

  uim_init();

  gtk_init(&argc, &argv);

  tray = egg_tray_icon_new("uim");
  gtk_window_set_wmclass(GTK_WINDOW(tray), "ibus-ui-gtk", "ibus-ui-gtk");

  icon = uim_toolbar_trayicon_new();
  g_signal_connect(G_OBJECT(tray), "embedded", G_CALLBACK(embedded_cb), icon);

  gtk_container_add(GTK_CONTAINER(tray), icon);
  gtk_widget_show(GTK_WIDGET(tray));

  gtk_main();

  uim_quit();
  return 0;
}
