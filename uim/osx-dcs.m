/*

  Copyright (c) 2011-2013 uim Project https://github.com/uim/uim

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

#include <Cocoa/Cocoa.h>
#include <iconv.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-util.h"
#include "dynlib.h"


static uim_lisp
osx_dcs_search_text(uim_lisp text_, uim_lisp encoding_)
{
  char *str;

  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  const char *enc = NULLP(encoding_) ? "UTF-8" : REFER_C_STR(encoding_);
  iconv_t cd = (iconv_t)uim_iconv->create("UTF-8", enc);
  char *text = uim_iconv->convert(cd, REFER_C_STR(text_));
  uim_iconv->release(cd);

  if (!text)
    str = strdup("");
  else {
    NSString *key = [NSString stringWithUTF8String:text];
    NSString *value = (NSString *)DCSCopyTextDefinition(NULL,
					      (CFStringRef)key,
					      CFRangeMake(0, [key length]));
    const char *s = [value UTF8String];

    if (s) {
      cd = (iconv_t)uim_iconv->create(enc, "UTF-8");
      str = uim_iconv->convert(cd, s);
      uim_iconv->release(cd);
      CFRelease((CFStringRef)value);
    } else
      str = strdup("");
  }
  free(text);
  [pool release];

  return MAKE_STR_DIRECTLY(str);
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc2("osx-dcs-search-text", osx_dcs_search_text);
}

void
uim_plugin_instance_quit(void)
{
}
