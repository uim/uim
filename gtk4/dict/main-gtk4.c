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

  GTK 4 note: gtk_main()/gtk_main_quit() are gone; the event loop is now
  driven by GtkApplication. gtk_window_set_default_icon_list() is gone
  too, replaced by gtk_window_set_default_icon_name() (an icon-theme
  name) -- since the original shipped a file path rather than a theme
  name, we instead set the icon directly on each top-level window via
  gtk_window_set_icon_name()/a loaded texture is not necessary for a
  single-instance tool, so we keep it simple and set the icon per window.

*/

#include <config.h>

#include <gtk/gtk.h>

#include <locale.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>

#include "uim.h"
#include "uim-helper.h"
#include "gettext.h"

#include "word-win-gtk4.h"
#include "word-list-win-gtk4.h"
#include "word-list-view-gtk4.h"

static int ae_mode; /* add mode or edit mode */
static int g_startup_dictionary;

enum {
  MODE_EDIT,
  MODE_ADD,
  NR_MODE
};

static char *
get_error_msg(void)
{
  /* dummy */
  return NULL;
}

static void
parse_arg(int argc, char *argv[])
{
  int ch;

  ae_mode = MODE_EDIT;

  while ((ch = getopt(argc, argv, "aehi:")) != -1)
  {
    switch (ch) {
    case 'a':
      ae_mode = MODE_ADD;
      break;
    case 'e':
      ae_mode = MODE_EDIT;
      break;
    case 'i':
      if (!strcmp(optarg, "anthy"))
	g_startup_dictionary = DICT_ENUM_DICTIONARY_TYPE_ANTHY;
      else if (!strcmp(optarg, "canna"))
	g_startup_dictionary = DICT_ENUM_DICTIONARY_TYPE_CANNA;
      else if (!strcmp(optarg, "prime"))
	g_startup_dictionary = DICT_ENUM_DICTIONARY_TYPE_PRIME;
      else if (!strcmp(optarg, "skk"))
	g_startup_dictionary = DICT_ENUM_DICTIONARY_TYPE_SKK;
      else
	g_startup_dictionary = DICT_ENUM_DICTIONARY_TYPE_ANTHY;
      break;
    case 'h':
      fprintf(stderr, "Usage: uim-dict-gtk4 [OPTION...]\n");
      fprintf(stderr, "\n");
      fprintf(stderr, "Options:\n");
      fprintf(stderr, " -h            Show this help\n");
      fprintf(stderr, " -i [IM]       Open a dictionary for IM [anthy, canna]\n");
      fprintf(stderr, " -e            Start with editing mode (default)\n");
      fprintf(stderr, " -a            Start with adding mode\n");
      exit(1);
      break;
    default:
      ae_mode = MODE_EDIT;
      /* g_startup_dictionary = get_current_im(); */
    }
  }

  argv += optind;
  argc -= optind;
}

static void
setup_window_icon(GtkWindow *window)
{
  GdkPixbuf *pixbuf;

  pixbuf = gdk_pixbuf_new_from_file(UIM_PIXMAPSDIR "/uim-dict.png", NULL);
  if (pixbuf) {
    GdkTexture *texture = gdk_texture_new_for_pixbuf(pixbuf);

    g_object_unref(pixbuf);
    if (texture) {
      /* GtkWindow has no direct "set icon from texture" any more; an
       * icon name from the theme is the portable GTK 4 way. uim-dict
       * installs its own pixmap rather than a themed icon, so this is
       * best-effort only and silently does nothing if the name isn't
       * in the icon theme. */
      g_object_unref(texture);
    }
  }
  gtk_window_set_icon_name(window, "uim-dict");
}

static GtkWidget *
create_window_anthy(GtkApplication *app)
{
  GtkWidget *window;
  uim_dict *dict;

  if (ae_mode == MODE_EDIT) {
    window = word_list_window_new(DICT_ENUM_DICTIONARY_TYPE_ANTHY);
    if (WORD_LIST_VIEW(WORD_LIST_WINDOW(window)->word_list)->dict == NULL) {
      gtk_window_destroy(GTK_WINDOW(window));
      return NULL;
    }
  } else {
    dict = uim_dict_open(N_("Anthy private dictionary"));
    if (!dict)
      return NULL;
    window = word_window_new(WORD_WINDOW_MODE_ADD, dict);
  }

  gtk_window_set_application(GTK_WINDOW(window), app);
  setup_window_icon(GTK_WINDOW(window));
  gtk_window_present(GTK_WINDOW(window));

  return window;
}

static GtkWidget *
create_window_canna(GtkApplication *app)
{
  GtkWidget *window;
  uim_dict *dict;

  if (ae_mode == MODE_EDIT) {
    window = word_list_window_new(DICT_ENUM_DICTIONARY_TYPE_CANNA);
    if (WORD_LIST_VIEW(WORD_LIST_WINDOW(window)->word_list)->dict == NULL) {
      gtk_window_destroy(GTK_WINDOW(window));
      return NULL;
    }
  } else {
    dict = uim_dict_open(N_("Canna private dictionary"));
    if (!dict) {
      fprintf(stderr, "uim_dict_open() canna NULL\n");
      return NULL;
    }
    window = word_window_new(WORD_WINDOW_MODE_ADD, dict);
  }

  gtk_window_set_application(GTK_WINDOW(window), app);
  setup_window_icon(GTK_WINDOW(window));
  gtk_window_present(GTK_WINDOW(window));

  return window;
}

void
dict_window_destroy_cb(GtkWidget *widget, gpointer data)
{
  GApplication *app = g_application_get_default();

  if (app)
    g_application_quit(app);
}

static int
create_window(GtkApplication *app)
{
  GtkWidget *window = NULL;

  switch (g_startup_dictionary) {
  case DICT_ENUM_DICTIONARY_TYPE_ANTHY:
    window = create_window_anthy(app);
    break;
  case DICT_ENUM_DICTIONARY_TYPE_CANNA:
    window = create_window_canna(app);
    break;
  case DICT_ENUM_DICTIONARY_TYPE_PRIME:
    /* create_window_prime();*/
    break;
  case DICT_ENUM_DICTIONARY_TYPE_SKK:
    /* create_window_skk();*/
    break;
  default:
    return -1;
  }

  if (!window)
    return -1;

  g_signal_connect(window, "destroy",
		   G_CALLBACK(dict_window_destroy_cb), NULL);

  return 0;
}

static void
activate_cb(GtkApplication *app, gpointer user_data)
{
  gint result = create_window(app);

  if (result == -1) {
    g_printerr(_("Error:%s\n"), get_error_msg());
    exit(EXIT_FAILURE);
  }
}

int
main(int argc, char *argv[])
{
  GtkApplication *app;
  int status;

  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8");
  parse_arg(argc, argv);

  app = gtk_application_new("org.uim.Dict", G_APPLICATION_NON_UNIQUE);
  g_signal_connect(app, "activate", G_CALLBACK(activate_cb), NULL);

  /* uim-dict-gtk4 parses its own -a/-e/-i flags rather than GApplication's,
   * so command-line handling is left entirely to parse_arg() above. */
  status = g_application_run(G_APPLICATION(app), 0, NULL);
  g_object_unref(app);

  return status;
}
