/*

  Copyright (c) 2003-2010 uim Project http://code.google.com/p/uim/

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

/* FIXME! This is a ad-hoc solution to advance
   annotation related discussion. */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <eb/eb.h>
#include <eb/text.h>
#include <eb/font.h>
#include <eb/binary.h>
#include <eb/error.h>

#include "uim-eb.h"

#define MAX_HITS   10
#define MAX_TEXT   1000
#define MAX_LENGTH 10000

struct _uim_eb {
  EB_Book         book;
  EB_Subbook_Code subCodes[EB_MAX_SUBBOOKS];
  int             subCount;
};

static void go_text_eb (uim_eb *ueb,
			EB_Position position,
			GString *str);

static unsigned int eb_ref_count = 0;

uim_eb *
uim_eb_new (const char *bookpath)
{
  uim_eb *ueb;
  EB_Error_Code err;

  ueb = malloc(sizeof(uim_eb));
  eb_ref_count++;

  err = eb_initialize_library();
  if (err != EB_SUCCESS)
    fprintf(stderr, "failed to initialize EB library : error = %s\n",
	    eb_error_message(err));

  eb_initialize_book(&ueb->book);

  err = eb_bind(&ueb->book, bookpath);
  if (err != EB_SUCCESS) {
    fprintf(stderr, "wrong bookpath\n");
    free(ueb);
    return NULL;
  }

  err = eb_subbook_list(&ueb->book, ueb->subCodes, &ueb->subCount);
  if (err != EB_SUCCESS) {
    g_printerr("eb_subbook_list() failed\n");
    free(ueb);
    return NULL;
  }

  return ueb;
}


void
uim_eb_destroy (uim_eb *ueb)
{
  if (!ueb)
    return;

  eb_finalize_book(&ueb->book);

  eb_ref_count--;
  if (eb_ref_count == 0)
    eb_finalize_library();
  free(ueb);
}


gchar *
uim_eb_search_text (uim_eb *ueb, const gchar *text_utf8)
{
  gchar *text;
  int i;
  gsize bytes_read, bytes_written;
  GString *str;

  /* FIXME! check return value */
  text = g_convert(text_utf8, strlen(text_utf8),
		   "EUC-JP", "UTF-8",
		   &bytes_read, &bytes_written,
		   NULL);
  g_return_val_if_fail(text, FALSE);

  str = g_string_new("");

  for (i = 0; i < ueb->subCount; i++) {
    EB_Hit hits[MAX_HITS];
    int hitCount;
    int j;

    /* specify subbook */
    if (eb_set_subbook(&ueb->book, ueb->subCodes[i]) != EB_SUCCESS) {
      g_print("eb_set_subbook() failed\n"); continue;
    }

    eb_search_word(&ueb->book, text);
    eb_hit_list(&ueb->book, MAX_HITS, hits, &hitCount);
    for (j = 0; j < hitCount; j++) {
      /*EB_Position headp = hits[j].heading;*/
      EB_Position textp = hits[j].text;

      go_text_eb(ueb, textp, str);
      g_string_append(str, "\n");
    }
  }

  g_free(text);

  return g_string_free(str, FALSE);
}


static void
go_text_eb (uim_eb *ueb, EB_Position position, GString *str)
{
  EB_Hookset hookset;
  char text[MAX_TEXT + 1];
  ssize_t text_length;
  ssize_t bytes;
  int i;

  if (eb_seek_text(&ueb->book, &position) != EB_SUCCESS) {
    g_print("eb_seek_text error occurs");
    return;
  }

  eb_initialize_hookset(&hookset);
  for (i = 0; i < 1; i++) {
    gchar *text_utf8;
    gsize bytes_read, bytes_written;

    if (eb_read_text(&ueb->book, NULL, &hookset,
		     NULL, MAX_TEXT, text, &text_length) != EB_SUCCESS) {
      bytes = 0;
      g_print("eb_read_text : an error occurs.\n");
      return;
    }

    bytes += text_length;
    if (text_length < 1)
      break;

    /* FIXME! check return value */
    text_utf8 = g_convert(text, strlen(text),
			  "UTF-8", "EUC-JP",
			  &bytes_read, &bytes_written,
			  NULL);
    g_string_append(str, text_utf8);
    g_free(text_utf8);
  }
  eb_finalize_hookset(&hookset);
}
