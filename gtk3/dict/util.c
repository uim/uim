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

#include "util.h"

gchar *charset_convert(const gchar *in,
		       const gchar *incode,
		       const gchar *outcode)
{
    gsize rbytes, wbytes;
    gchar *out = NULL;
    GError *error = NULL;

    g_return_val_if_fail(in, NULL);
    g_return_val_if_fail(incode && *incode, g_strdup(in));
    g_return_val_if_fail(outcode && *outcode, g_strdup(in));

    out = g_convert(in, -1, outcode, incode, &rbytes, &wbytes, &error);
    if (error != NULL) {
	g_printerr("g_convert failed: %s\nin: %s out: %s\n",
		   error->message, in, out);
	g_error_free(error);
    }

    return out;
}

gchar *utf8_convert(const gchar *in) {
    gsize rbytes, wbytes;
    gchar *out;
    GError *error = NULL;

    g_return_val_if_fail(in, NULL);

    out = g_locale_to_utf8(in, -1, &rbytes, &wbytes, &error);
    if (out == NULL && error != NULL) {
	if (g_utf8_validate(in, -1, NULL)) {
	    out = g_strdup(in);
	} else {
	    g_printerr("g_locale_to_utf8 failed: %s\n", error->message);
	    out = NULL;
	}
	g_error_free(error);
    }
    return out;
}

gchar *utf8_to_eucjp(const gchar *utf8) {
    gchar *eucjp = NULL;
    g_return_val_if_fail(utf8, NULL);

    if (g_utf8_validate(utf8, -1, NULL)) {
	eucjp = charset_convert(utf8, "UTF-8", "EUC-JP");
    }

    /* XXX: must prepare fallbacks */
    return eucjp;
}

gchar *eucjp_to_utf8(const gchar *eucjp) {
    gchar *utf8;
    g_return_val_if_fail(eucjp, NULL);

    /* XXX: must check wheter eucjp is really EUC-JP */
    utf8 = charset_convert(eucjp, "EUC-JP", "UTF-8");
    return utf8;
}
