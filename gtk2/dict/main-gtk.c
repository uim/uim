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
#include <errno.h>
#include <unistd.h>

#include "uim.h"
#include "uim-helper.h"
#include "gettext.h"

#include "word-win-gtk.h"
#include "word-list-win-gtk.h"
#include "word-list-view-gtk.h"

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
      fprintf(stderr, "Usage: uim-dict-gtk [OPTION...]\n");
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

static GtkWidget *
create_window_anthy(void)
{
  GtkWidget *window;
  uim_dict *dict;

  if (ae_mode == MODE_EDIT) {
    window = word_list_window_new(DICT_ENUM_DICTIONARY_TYPE_ANTHY);
    if (WORD_LIST_VIEW(WORD_LIST_WINDOW(window)->word_list)->dict == NULL)
      return NULL;
  } else {
    dict = uim_dict_open(N_("Anthy private dictionary"));
    if (!dict)
      return NULL;
    window = word_window_new(WORD_WINDOW_MODE_ADD, dict);
  }

  gtk_widget_show(window);

  return window;
}

static GtkWidget *
create_window_canna(void)
{
  GtkWidget *window;
  uim_dict *dict;

  if (ae_mode == MODE_EDIT) {
    window = word_list_window_new(DICT_ENUM_DICTIONARY_TYPE_CANNA);
    if (WORD_LIST_VIEW(WORD_LIST_WINDOW(window)->word_list)->dict == NULL)
      return NULL;
  } else {
    dict = uim_dict_open(N_("Canna private dictionary"));
    if (!dict) {
	    fprintf(stderr, "uim_dict_open() canna NULL\n");
      return NULL;
    }
    window = word_window_new(WORD_WINDOW_MODE_ADD, dict);
  }

  gtk_widget_show(window);

  return window;
}


void
dict_window_destroy_cb(GtkWidget *widget, gpointer data)
{
  gtk_main_quit();
}

static int
create_window(void)
{
  GtkWidget *window = NULL;

  switch (g_startup_dictionary) {
  case DICT_ENUM_DICTIONARY_TYPE_ANTHY:
    window = create_window_anthy();
    break;
  case DICT_ENUM_DICTIONARY_TYPE_CANNA:
    window = create_window_canna();
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

  g_signal_connect(G_OBJECT(window), "destroy",
		   G_CALLBACK(dict_window_destroy_cb), NULL);

  return 0;
}

static void
setup_default_icon()
{
  GdkPixbuf *pixbuf;

  pixbuf = gdk_pixbuf_new_from_file(UIM_PIXMAPSDIR "/uim-dict.png", NULL);
  if (pixbuf) {
    GList *list;

    list = NULL;
    list = g_list_append(list, pixbuf);
    gtk_window_set_default_icon_list(list);
    g_list_free(list);
    g_object_unref(pixbuf);
  }
}

int
main(int argc, char *argv[])
{
  gint result;

  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8");
  parse_arg(argc, argv);

  gtk_init(&argc, &argv);
  setup_default_icon();

  result = create_window();

  if (result == -1) {
    g_printerr(_("Error:%s\n"), get_error_msg());
    exit(EXIT_FAILURE);
  }

  gtk_main ();

  return 0;
}
