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
#include <stdarg.h>
#include <string.h>
#include <dcopobject.h>
#include <dcopclient.h>
#include <kapplication.h>
#include <knotifyclient.h>

#include "uim.h"  // for uim_bool
#include "uim-notify.h"
#include "gettext.h"

#define UGETTEXT(str)	(dgettext(GETTEXT_PACKAGE, (str)))

static uim_notify_desc uim_notify_knotify3_desc = {
  "knotify3",
  "Output via knotify",
};

const uim_notify_desc *
uim_notify_plugin_get_desc(void)
{
  return &uim_notify_knotify3_desc;
}

uim_bool
uim_notify_plugin_init()
{
  return UIM_TRUE;
}

void
uim_notify_plugin_quit()
{
  return;
}

static uim_bool
send_knotify(const char *eventstr, const char *msg, int level)
{
  char body[BUFSIZ];
  char body_short[256];
  QByteArray data;
  QDataStream arg(data, IO_WriteOnly);
  QString event(eventstr), fromApp("uim"), text, sound(""), file("");
  int present = KNotifyClient::Messagebox | level;

  snprintf(body, sizeof(body), "libuim: %s", UGETTEXT(msg));
  fprintf(stderr, "%s\n", UGETTEXT(msg));

  strlcpy(body_short, body, sizeof(body_short));
  text = body_short;

  if (!kapp->dcopClient()->attach()) {
    fprintf(stderr, "libuim: cannot connect DCOP\n");
    return UIM_FALSE;
  }
  arg << event << fromApp << text << sound << file << present << level;
  if (!kapp->dcopClient()->send("knotify", "Notify", "notify(QString,QString,QString,QString,QString,int,int)",
				data)) {
    fprintf(stderr, "libuim: cannot send message via DCOP\n");
    return UIM_FALSE;
  }
  return UIM_TRUE;
}

uim_bool
uim_notify_plugin_info(const char *msg)
{
  return send_knotify("Info", msg, KNotifyClient::Notification);
}

uim_bool
uim_notify_plugin_fatal(const char *msg)
{
  return send_knotify("Fatal", msg, KNotifyClient::Error);
}
