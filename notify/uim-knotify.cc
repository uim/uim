/*
  Copyright (c) 2003-2007 uim Project http://code.google.com/p/uim/

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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <kapplication.h>
#include <knotifyclient.h>

#include "config.h"
#include "knotify.h"

int uim_notify_init(void);
void uim_notify_quit(void);
int uim_notify_info(const char *, va_list);
int uim_notify_fatal(const char *, va_list);

static int inited = 0;
int
uim_notify_init()
{
  if (!inited) {
    int argc = 1;
    char *argv[1] = { "uim" };

    KApplication app(argc, argv, "uim"); // XXX
    inited = 1;
  }
  return 1;
}

void
uim_notify_quit()
{
  return;
}

int
uim_notify_info(const char *msg_fmt, va_list ap)
{
  char body[BUFSIZ];
  char body_short[256];
  char body_fmt[BUFSIZ];

  strlcpy(body_fmt, "uim: ", sizeof(body_fmt));
  strlcat(body_fmt, msg_fmt, sizeof(body_fmt));
  vsnprintf(body, sizeof(body), body_fmt, ap);
  strlcpy(body_short, body, sizeof(body_short));

  KNotifyClient::userEvent(body_short, KNotifyClient::Messagebox);
  kapp->processEvents();
}

int
uim_notify_fatal(const char *msg_fmt, va_list ap)
{
  char body[BUFSIZ];
  char body_short[256];
  char body_fmt[BUFSIZ];

  strlcpy(body_fmt, "uim: ", sizeof(body_fmt));
  strlcat(body_fmt, msg_fmt, sizeof(body_fmt));
  vsnprintf(body, sizeof(body), body_fmt, ap);
  strlcpy(body_short, body, sizeof(body_short));

  KNotifyClient::userEvent(body_short, KNotifyClient::Messagebox, KNotifyClient::Error);
  kapp->processEvents();
}
