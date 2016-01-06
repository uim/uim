/*
  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or
  without modification, are permitted provided that the
  following conditions are met:

  1. Redistributions of source code must retain the above
     copyright notice, this list of conditions and the
     following disclaimer.
  2. Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the
     following disclaimer in the documentation and/or other
     materials provided with the distribution.
  3. Neither the name of authors nor the names of its
     contributors may be used to endorse or promote products
     derived from this software without specific prior written
     permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "encoding.h"

im_encoding *im_enc_list_head = NULL, *im_enc_list_tail = NULL;

char default_encoding[] = "UTF-8";

/* search encoding entry */
im_encoding *
search_im_encoding(const char *im)
{
  im_encoding *im_enc = im_enc_list_head;

  debug_printf(DEBUG_NOTE, "search_im_encoding\n");

  while (im_enc) {
	if (strcmp(im_enc->im, im) == 0) {
	  debug_printf(DEBUG_NOTE, " found\n");
	  return im_enc;
	}
	im_enc = im_enc->next;
  }

  debug_printf(DEBUG_NOTE, " not found\n");
  return NULL;
}


im_encoding *
new_im_encoding(const char *im, const char *encoding)
{
  im_encoding *im_enc = uim_malloc(sizeof(im_encoding));

  debug_printf(DEBUG_NOTE, "new_im_encoding\n");

  im_enc->im = uim_strdup(im);

  if (encoding != NULL)
	im_enc->encoding = uim_strdup( encoding );
  else
	im_enc->encoding = NULL;

  im_enc->next = NULL;

  if (im_enc_list_tail == NULL) {
	im_enc_list_tail = im_enc;
	im_enc_list_head = im_enc;
  } else {
	im_enc_list_tail->next = im_enc;
	im_enc_list_tail = im_enc;
  }

  return im_enc;
}


/*
 *     Since 21.x or lower version of Emacs doesn't support UTF-8 well,
 *  uim-el-agent outputs strings which are encoded with each language's
 *  encoding.  The encoding of each IM are specified by Emacs.
 */
int
set_im_encoding(const char *im, const char *encoding)
{
  im_encoding *im_enc;

  if (im == NULL) return -1;

  im_enc = search_im_encoding(im);

  if (im_enc != NULL) {
	free(im_enc->encoding);

	if (encoding)
	  im_enc->encoding = uim_strdup(encoding);
	else
	  im_enc->encoding = NULL;

  } else {
	new_im_encoding(im, encoding);
  }

  return 1;
}


/* get encoding from im list */
const char *
get_im_encoding(const char *im)
{
  im_encoding *im_enc = search_im_encoding(im);

  debug_printf(DEBUG_NOTE, "get_im_encoding %s\n", im);

  if (im_enc && im_enc->encoding) {
	debug_printf(DEBUG_NOTE, " encoding = %s\n", im_enc->encoding);
	return im_enc->encoding;
  } else {
	/*return NULL;*/
	debug_printf(DEBUG_WARNING, " default encoding (%s)\n", default_encoding);
	return default_encoding;
  }
}
