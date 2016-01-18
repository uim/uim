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
#include <config.h>
#endif
#if (!defined(DEBUG) && !defined(NDEBUG))
#define NDEBUG
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include "uim-fep.h"
#include "callbacks.h"
#include "helper.h"
#include "str.h"
#include <uim/uim-helper.h>
#include <uim/uim-util.h>
#include <uim/uim-im-switcher.h>

int g_focus_in = FALSE;
int g_helper_fd = -1;

static void helper_disconnected_cb(void);
static void helper_handler_change_im(const char *str);
static void send_im_list(void);

void init_helper(void)
{
  g_helper_fd = uim_helper_init_client_fd(helper_disconnected_cb);
}

void quit_helper(void)
{
  if (g_helper_fd >= 0) {
    uim_helper_close_client_fd(g_helper_fd);
  }
}

static void helper_disconnected_cb(void)
{
  debug(("helper_disconnected_cb()\n"));
  g_helper_fd = -1;
}

void helper_handler(void)
{
  char *message;
  uim_helper_read_proc(g_helper_fd);

  while ((message = uim_helper_get_message()) != NULL) {

    if (str_has_prefix(message, "im_change")) {
      debug(("im_change\n"));
      helper_handler_change_im(message);

    } else if (str_has_prefix(message, "prop_update_custom")) {
      char *eol;
      debug(("prop_update_custom\n"));
      if ((eol = strchr(message, '\n')) != NULL) {
        char *sym = eol + 1;
        if ((eol = strchr(sym, '\n')) != NULL) {
          char *value = eol + 1;
          *eol = '\0';
          if ((eol = strchr(value, '\n')) != NULL) {
            *eol = '\0';
            uim_prop_update_custom(g_context, sym, value);
          }
        }
      }

    } else if (str_has_prefix(message, "custom_reload_notify")) {
      debug(("custom_reload_notify\n"));
      uim_prop_reload_configs();

    } else if (g_focus_in) {
      if (str_has_prefix(message, "prop_list_get")) {
        debug(("prop_list_get\n"));
        uim_prop_list_update(g_context);

      } else if (str_has_prefix(message, "prop_activate")) {
        char *eol;
        debug(("prop_activate\n"));
        if ((eol = strchr(message, '\n')) != NULL) {
          char *menucommand_name = eol + 1;
          if ((eol = strchr(menucommand_name, '\n')) != NULL) {
            *eol = '\0';
            uim_prop_activate(g_context, menucommand_name);
          }
        }

      } else if (str_has_prefix(message, "im_list_get")) {
        debug(("im_list_get\n"));
        send_im_list();

      } else if (str_has_prefix(message, "commit_string")) {
        char *eol;
        debug(("commit_string\n"));
        if ((eol = strchr(message, '\n')) != NULL) {
          char *charset = "UTF-8";

          if (str_has_prefix(eol + 1, "charset=")) {
            charset = eol + 1 + strlen("charset=");
            eol = strchr(charset, '\n');
            *eol = '\0';
          }

          if (eol != NULL) {
            char *commit_string = eol + 1;

            if ((eol = strchr(commit_string, '\n')) != NULL) {
              const char *commit_enc = get_enc();

              *eol = '\0';
              if (uim_iconv->is_convertible(commit_enc, charset)) {
                void *cd = uim_iconv->create(commit_enc, charset);
                commit_string = uim_iconv->convert(cd, commit_string);
                commit_cb(NULL, commit_string);
                free(commit_string);
                if (cd) {
                  uim_iconv->release(cd);
                }
              }
            }
          }
        }

      } else if (str_has_prefix(message, "focus_in")) {
        debug(("focus_in\n"));
        g_focus_in = FALSE;
        /* printf("focus_out\r\n"); */
      }
    }

    free(message);
  }
}

static void helper_handler_change_im(const char *str)
{
  if (str_has_prefix(str, "im_change_whole_desktop") ||
      (g_focus_in && (
        str_has_prefix(str, "im_change_this_text_area_only") ||
        str_has_prefix(str, "im_change_this_application_only")))) {
    char *eol;
    if ((eol = strchr(str, '\n')) != NULL) {
      char *imname = eol + 1;
      if ((eol = strchr(imname, '\n')) != NULL) {
        *eol = '\0';
        uim_switch_im(g_context, imname);
      }
    }
  }
}

static void send_im_list(void)
{
  int i;
  int nr_im = uim_get_nr_im(g_context);
  const char *current_im_name = uim_get_current_im_name(g_context);
  const char *enc = get_enc();
  char *message;
  char *oldmessage;

  uim_asprintf(&message, "im_list\ncharset=%s\n", enc);
  for (i = 0; i < nr_im; i++) {
    const char *name = uim_get_im_name(g_context, i);
    const char *langcode = uim_get_im_language(g_context, i);
    const char *lang = uim_get_language_name_from_locale(langcode);
    const char *short_desc = uim_get_im_short_desc(g_context, i);
    char *im_str;

    uim_asprintf(&im_str, "%s\t%s\t%s\t%s\n", name,
        (lang != NULL ? lang : ""),
        (short_desc != NULL ? short_desc : ""),
        (strcmp(name, current_im_name) == 0 ? "selected" : ""));

    oldmessage = message;
    uim_asprintf(&message, "%s%s", oldmessage, im_str);
    free(oldmessage);
    free(im_str);
  }
  uim_helper_send_message(g_helper_fd, message);
  free(message);
}

void focus_in(void)
{
  g_focus_in = TRUE;
  uim_helper_client_focus_in(g_context);
  uim_prop_list_update(g_context);
}
