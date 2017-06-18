/*
  Copyright (c) 2012-2013 uim Project https://github.com/uim/uim

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

#include <cstdio> // for fprintf

#include <QtGui/QApplication>

#include <knotification.h>

#include "qtgettext.h"
#include "uim.h"  // for uim_bool
#include "uim-notify.h"

//  interface
static uim_notify_desc uim_notify_knotify4_desc = {
    "knotify4",
    "Output via knotify",
};

KDE_EXPORT const uim_notify_desc *
uim_notify_plugin_get_desc(void)
{
    return &uim_notify_knotify4_desc;
}

KDE_EXPORT uim_bool
uim_notify_plugin_init(void)
{
    return UIM_TRUE;
}

KDE_EXPORT void
uim_notify_plugin_quit(void)
{
}

static uim_bool
send_knotify(const char *msg, KNotification::StandardEvent eventId)
{
    fprintf(stderr, "%s\n", dgettext(GETTEXT_PACKAGE, msg));

    if (QApplication::instance()) {
        KNotification::event(eventId, mygettext(msg),
            QPixmap(UIM_PIXMAPSDIR "/uim-icon.png"));
        return UIM_TRUE;
    }

    // fake arguments
    int argc = 1;
    char *arg = strdup("uim");
    char *argv[] = {arg};

    QApplication app(argc, argv);

    KNotification::event(eventId, mygettext(msg),
        QPixmap(UIM_PIXMAPSDIR "/uim-icon.png"));

    free(arg);

    return UIM_TRUE;
}

KDE_EXPORT uim_bool
uim_notify_plugin_info(const char *msg)
{
    return send_knotify(msg, KNotification::Notification);
}

KDE_EXPORT uim_bool
uim_notify_plugin_fatal(const char *msg)
{
    return send_knotify(msg, KNotification::Error);
}
