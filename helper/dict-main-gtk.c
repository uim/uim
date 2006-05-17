/*

  Copyright (c) 2004-2006 uim Project http://uim.freedesktop.org/

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

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include "uim/gettext.h"

#include "dict-word-win-gtk.h"
#include "dict-word-list-win-gtk.h"

static unsigned int read_tag;
static int uim_fd;  /* file descriptor to connect helper message bus */
static int ae_mode; /* add mode or edit mode */
static int input_method;

enum {
  MODE_EDIT,
  MODE_ADD,
  NR_MODE
};

/* should be implemeted as loadable module */
enum {
  IM_ANTHY,
  IM_CANNA,
  IM_SKK,
  IM_PRIME,
  NR_IM
};

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
    /* parse_helper_str(tmp); */
    g_free(tmp);
    tmp = NULL;
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

  while ((ch = getopt(argc, argv, "aei:")) != -1)
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
	input_method = IM_ANTHY;
      else if (!strcmp(optarg, "canna"))
	input_method = IM_CANNA;
      else if (!strcmp(optarg, "prime"))
	input_method = IM_PRIME;
      else if (!strcmp(optarg, "skk"))
	input_method = IM_SKK;
      else
	input_method = IM_ANTHY;
      break;
    default:
      ae_mode = MODE_EDIT;
      /* input_method = get_current_im(); */
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
    window = word_list_window_new();
  } else {
    dict = uim_dict_open(N_("Anthy private dictionary"));
    if (!dict)
      return NULL;
    window = word_window_new(WORD_WINDOW_MODE_ADD, dict);
    uim_dict_unref(dict);
  }

  gtk_widget_show(window);

  return window;
}

static void
window_destroy_cb(GtkWidget *widget, gpointer data)
{
  gtk_main_quit();
}

static int
create_window(void)
{
  GtkWidget *window = NULL;

  switch (input_method) {
  case IM_ANTHY:
    window = create_window_anthy();
    break;
  case IM_CANNA:
    /* window = create_window_canna(); */
    break;
  case IM_SKK:
    /* create_window_skk();*/
    break;
  case IM_PRIME:
    /* create_window_prime();*/
    break;
  default:
    return -1;
  }

  if (!window)
    return -1;

  g_signal_connect(G_OBJECT(window), "destroy",
		   G_CALLBACK(window_destroy_cb), NULL);

  return 0;
}

int
main(int argc, char *argv[])
{  
  gint result;
  setlocale(LC_ALL, "");
  gtk_set_locale();
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8"); 
  parse_arg(argc, argv);

  gtk_init(&argc, &argv);

  result = create_window();

  if (result == -1) {
    g_printerr(_("Error:%s\n"), get_error_msg());
    exit(EXIT_FAILURE);
  }

  /* connect to uim helper message bus */
  uim_fd = -1;
  check_helper_connection();  

  gtk_main ();

  return 0;
}
