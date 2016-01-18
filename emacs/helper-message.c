/*
  Copyright (c) 2006-2013 uim Project https://github.com/uim/uim

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

#include "helper-message.h"

#define helper_encode(src) ((src) == 57 ? 33 : ((src) + 0x23))
#define helper_decode(src) ((src) == 33 ? 57 : ((src) - 0x23))


/* encode src string to Emacs safe 7bit ASCII string */
char *
helper_message_encode(const char *src)
{
  int i, length = 0;
  unsigned char *p = (unsigned char *)src;
  char *outbuf, *ob;
  
  if (src) length = strlen(src);

  /* 24bit -> 32bit */
  outbuf = uim_malloc((length + 2) / 3 * 4 + 1);

  ob = outbuf;

  for (i = 0; i < length - 2; i += 3) {
	*ob = (char)helper_encode(*p >> 2);
	*(ob + 1) = (char)helper_encode(((*p & 0x3) << 4) |
									(*(p + 1) >> 4));
	*(ob + 2) = (char)helper_encode(((*(p + 1) & 0xf) << 2) |
									(*(p + 2) >> 6));
	*(ob + 3) = (char)helper_encode(*(p + 2) & 0x3f);
	ob += 4;
	p += 3;
  }

  if (length % 3 == 2) {
	*ob = (char)helper_encode(*p >> 2);
	*(ob + 1) = (char)helper_encode(((*p & 0x3) << 4) |
									(*(p + 1) >> 4));
	*(ob + 2) = (char)helper_encode((*(p + 1) & 0xf) << 2);
	*(ob + 3) = (char)helper_encode(0);
	ob += 4;
  } else if (length % 3 == 1) {
	*ob = (char)helper_encode(*p >> 2);
	*(ob + 1) = (char)helper_encode(((*p & 0x3) << 4));
	*(ob + 2) = (char)helper_encode(0);
	*(ob + 3) = (char)helper_encode(0);
	ob += 4;
  }

  *ob = '\0';

  return outbuf;
}


/* decode src string to 8bit string */
char *
helper_message_decode(const char *src)
{
  int i, length = 0;

  unsigned char *p = (unsigned char *)src;
  unsigned char p0, p1, p2, p3;
  char *outbuf, *ob;

  if (src) length = strlen(src);

  outbuf = uim_malloc(length / 4 * 3 + 1);

  ob = outbuf;

  for (i = 0; i < length; i += 4) {
	p0 = helper_decode(*p);
	p1 = helper_decode(*(p + 1));
	p2 = helper_decode(*(p + 2));
	p3 = helper_decode(*(p + 3));

	*ob = (char)((p0 << 2) | (p1 >> 4));
	*(ob + 1) = (char)((p1 << 4) | (p2 >> 2));
	*(ob + 2) = (char)((p2 << 6) | p3);
	ob += 3;
	p += 4;
  }

  *ob = '\0';

  return outbuf;
}


void
helper_send_message(const char *message)
{
  char *enc = helper_message_encode(message);
  a_printf("(h \"%s\") ", enc);
  free(enc);
}
