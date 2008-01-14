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

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <glib.h>
#include <libnotify/notify.h>

#include "uim-notify.h"

#define UIM_ICON UIM_PIXMAPSDIR "/uim-icon.png"

static int
uim_libnotify_notify(int urgency, int timeout, const char *body)
{
  char body_short[256];
  NotifyNotification *notification;
  GError *error = NULL;
  gboolean ret;

  strlcpy(body_short, body, sizeof(body_short));

  fprintf(stderr, "uim: %s\n", body);

  if (!notify_is_initted()) {
    fprintf(stderr, "libnotify: libnotify is not initted\n");
    return 0;
  }

  notification = notify_notification_new("uim", body_short, UIM_ICON, NULL);

  if (!notification) {
    fprintf(stderr, "notify_notification_new: can not create notification object\n");
    return 0;
  }

  notify_notification_set_timeout(notification, timeout);

  notify_notification_set_urgency(notification, urgency);

  ret = notify_notification_show(notification, &error);
  if (error) {
    fprintf(stderr, "notify_notification_show: %s\n", error->message);
    return 0;
  }

  g_object_unref(G_OBJECT(notification));

  return 1;
}

/*
  interface
 */
static uim_notify_desc uim_notify_libnotify_desc = {
  "libnotify",
  "Output via libnotify",
};

const uim_notify_desc *
uim_notify_plugin_get_desc(void)
{
  return &uim_notify_libnotify_desc;
}

int
uim_notify_plugin_init(void)
{
  return notify_init("uim");
}

void
uim_notify_plugin_quit(void)
{
  if (notify_is_initted())
    notify_uninit();
}

int
uim_notify_plugin_info(const char *msg)
{
  return uim_libnotify_notify(NOTIFY_URGENCY_NORMAL, NOTIFY_EXPIRES_DEFAULT, msg);
}

int
uim_notify_plugin_fatal(const char *msg)
{
  return uim_libnotify_notify(NOTIFY_URGENCY_CRITICAL, NOTIFY_EXPIRES_NEVER, msg);
}
