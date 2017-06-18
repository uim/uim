/*
  Copyright (c) 2009-2013 uim Project https://github.com/uim/uim

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

#import <Cocoa/Cocoa.h>
#import <Growl/GrowlApplicationBridge.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "uim.h"  /* for uim_bool */
#include "uim-notify.h"
#include "gettext.h"

#define UGETTEXT(str)	(dgettext(GETTEXT_PACKAGE, (str)))

static uim_bool
uim_growl_notify(int prio, BOOL is_sticky, const char *body)
{
  char body_short[256];
  const char *str[3] = {"uim notify info", "dummy", "uim notify fatal"};
  const char *title[3] = {"Info", "dummy", "Fatal"};

  if (prio > 2 || prio < 0)
    return UIM_FALSE;

  strlcpy(body_short, UGETTEXT(body), sizeof(body_short));

  fprintf(stderr, "libuim: %s\n", UGETTEXT(body));

  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  [GrowlApplicationBridge
	  notifyWithTitle:[NSString stringWithCString:title[prio]]
	      description:[NSString stringWithUTF8String:body_short]
	 notificationName:[NSString stringWithCString:str[prio]]
		 iconData:nil
		 priority:prio
		 isSticky:is_sticky
	     clickContext:nil];
  [pool release];

  return UIM_TRUE;
}

/*
  interface
 */
static uim_notify_desc uim_notify_growl_desc = {
  "growl",
  "Output via growl",
};

const uim_notify_desc *
uim_notify_plugin_get_desc(void)
{
  return &uim_notify_growl_desc;
}

uim_bool
uim_notify_plugin_init(void)
{
  return UIM_TRUE;
}

void
uim_notify_plugin_quit(void)
{
  return;
}

uim_bool
uim_notify_plugin_info(const char *msg)
{
  return uim_growl_notify(0, NO, msg);
}

uim_bool
uim_notify_plugin_fatal(const char *msg)
{
  return uim_growl_notify(2, YES, msg);
}
