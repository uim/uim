/*

Copyright (c) 2003-2013 uim Project http://code.google.com/p/uim/

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

#include <qinputcontext.h>
#if UIM_QT_USE_JAPANESE_KANA_KEYBOARD_HACK
#include <qwindowdefs.h>
#endif

#include <locale.h>

#include "uim/uim.h"
#include "uim/uim-scm.h"
#include "uim/counted-init.h"

#include "plugin.h"
#include "quiminputcontext_with_slave.h"
#include "quiminfomanager.h"

#if UIM_QT_USE_JAPANESE_KANA_KEYBOARD_HACK
#include "uim/uim-x-util.h"
#endif

QUimInfoManager *UimInputContextPlugin::infoManager = NULL;


UimInputContextPlugin::UimInputContextPlugin()
{
        uimReady = false;
        uimInit();
}

UimInputContextPlugin::~UimInputContextPlugin()
{
        uimQuit();
}

QStringList
UimInputContextPlugin::keys() const
{
        QStringList imList;
        imList << "uim";
        return imList;
}

QInputContext *
UimInputContextPlugin::create( const QString &key )
{
        QString imname = QString::null;
        if ( QString::compare( key, "uim" ) == 0 )
        {
            imname = uim_get_default_im_name( setlocale( LC_CTYPE, NULL ) );
            QString lang = infoManager->imLang ( imname );
            QUimInputContext *uic = new QUimInputContextWithSlave( imname, lang );
            return uic;
        }

        return NULL;
}

QStringList
UimInputContextPlugin::languages( const QString &key )
{
        QStringList langs;
        if ( key == QString( "uim" ) ) {
            langs.push_back ( "ja" );
            langs.push_back ( "ko" );
            langs.push_back ( "zh" );
            langs.push_back ( "*" );
        }

        return langs;
}

QString
UimInputContextPlugin::displayName( const QString &key )
{
        return QString( key );
}

QString
UimInputContextPlugin::description( const QString &key )
{
        return displayName( key ) + ": the universal input method framework";
}

QUimInfoManager *
UimInputContextPlugin::getQUimInfoManager()
{
    return infoManager;
}

void
UimInputContextPlugin::uimInit()
{
    if ( !uim_counted_init() ) {
        if (!infoManager)
            infoManager = new QUimInfoManager();

	if (uim_scm_c_bool(uim_scm_callf("require-dynlib", "s", "xkb")))
	    uim_scm_callf("%xkb-set-display", "p", qt_xdisplay());

#if UIM_QT_USE_JAPANESE_KANA_KEYBOARD_HACK
	uim_x_kana_input_hack_init(qt_xdisplay());
#endif
        uimReady = true;
    }
}

void
UimInputContextPlugin::uimQuit()
{
    if ( uimReady )
    {
        uim_counted_quit();
        delete infoManager;
        uimReady = false;
    }
}

Q_EXPORT_PLUGIN( UimInputContextPlugin )
