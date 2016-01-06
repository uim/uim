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

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <iconv.h>
#include <gettext.h>
#include <eb/eb.h>
#include <eb/text.h>
#include <eb/font.h>
#include <eb/binary.h>
#include <eb/error.h>

#include "uim.h"
#include "uim-util.h"
#include "uim-notify.h"

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
			char **str,
			const char *enc);

static int
uim_eb_strappend(char **dest, const char *append, size_t append_len)
{
  if (*dest) {
    char *str;
    size_t dest_len = strlen(*dest);
    size_t len = dest_len + append_len;

    str = uim_realloc(*dest, len + 1);
    memcpy(&str[dest_len], append, append_len);
    str[len] = '\0';
    *dest = str;
  } else {
    char *str;

    str = uim_malloc(append_len + 1);
    memcpy(str, append, append_len);
    str[append_len] = '\0';
    *dest = str;
  }
  return 1;
}

void
uim_eb_open ()
{
  EB_Error_Code err;

  err = eb_initialize_library();
  if (err != EB_SUCCESS)
    uim_notify_fatal(_("eb: failed to initialize EB library : error = %s\n"),
	     eb_error_message(err));
}

void
uim_eb_close (void)
{
  eb_finalize_library();
}

uim_eb *
uim_eb_new (const char *bookpath)
{
  uim_eb *ueb;
  EB_Error_Code err;

  ueb = uim_malloc(sizeof(uim_eb));

  eb_initialize_book(&ueb->book);

  err = eb_bind(&ueb->book, bookpath);
  if (err != EB_SUCCESS) {
    uim_notify_fatal(N_("eb: wrong bookpath"));
    free(ueb);
    return NULL;
  }

  err = eb_subbook_list(&ueb->book, ueb->subCodes, &ueb->subCount);
  if (err != EB_SUCCESS) {
    uim_notify_fatal(N_("eb: eb_subbook_list() failed\n"));
    free(ueb);
    return NULL;
  }

  return ueb;
}

void
uim_eb_destroy (uim_eb *ueb)
{
  if (ueb)
    eb_finalize_book(&ueb->book);

  free(ueb);
  ueb = NULL;
}


char *
uim_eb_search_text (uim_eb *ueb, const char *key, const char *enc)
{
  char *text;
  int i;
  char *str = NULL;
  iconv_t cd;

  /* FIXME! check return value */

  cd = (iconv_t)uim_iconv->create("EUC-JP", enc);
  text = uim_iconv->convert(cd, key);
  uim_iconv->release(cd);

  if (!text)
	  return NULL;

  for (i = 0; i < ueb->subCount; i++) {
    EB_Hit hits[MAX_HITS];
    int hitCount;
    int j;

    /* specify subbook */
    if (eb_set_subbook(&ueb->book, ueb->subCodes[i]) != EB_SUCCESS) {
      uim_notify_fatal(N_("eb: eb_set_subbook() failed")); continue;
    }

    eb_search_word(&ueb->book, text);
    eb_hit_list(&ueb->book, MAX_HITS, hits, &hitCount);
    for (j = 0; j < hitCount; j++) {
      /*EB_Position headp = hits[j].heading;*/
      EB_Position textp = hits[j].text;

      go_text_eb(ueb, textp, &str, enc);
      uim_eb_strappend(&str, "\n", sizeof("\n"));
    }
  }

  free(text);

  return str;
}


static void
go_text_eb (uim_eb *ueb, EB_Position position, char **str, const char *enc)
{
  EB_Hookset hookset;
  char text[MAX_TEXT + 1];
  ssize_t text_length;
  ssize_t bytes;
  int i;

  if (eb_seek_text(&ueb->book, &position) != EB_SUCCESS) {
    uim_notify_fatal(N_("eb: eb_seek_text error occurs"));
    return;
  }

  eb_initialize_hookset(&hookset);
  for (i = 0; i < 1; i++) {
    char *local;
    iconv_t cd;

    if (eb_read_text(&ueb->book, NULL, &hookset,
		     NULL, MAX_TEXT, text, &text_length) != EB_SUCCESS) {
      bytes = 0;
      uim_notify_fatal(N_("eb_read_text : an error occurs"));
      return;
    }

    bytes += text_length;
    if (text_length < 1)
      break;

    /* FIXME! check return value */
    cd = (iconv_t)uim_iconv->create(enc, "EUC-JP");
    local = uim_iconv->convert(cd, text);
    uim_iconv->release(cd);

    uim_eb_strappend(str, local, strlen(local));

    free(local);
  }
  eb_finalize_hookset(&hookset);
}
