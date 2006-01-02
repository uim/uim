/*

Copyright (c) 2003-2006 uim Project http://uim.freedesktop.org/

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
#include <qapplication.h>
#include <qinputcontextplugin.h>
#include <qinputcontext.h>

#include <uim/uim.h>

#include <locale.h>

#include "immodule-quiminputcontext_with_slave.h"

class UimInputContextPlugin : public QInputContextPlugin
{
public:
    UimInputContextPlugin() : QInputContextPlugin()
    {
        uimReady = false;
        uimInit();
    }

    ~UimInputContextPlugin()
    {
        uimQuit();
    }

    QStringList keys() const
    {
        QStringList imList;
        imList << "uim";
        return imList;
    }

    QInputContext *create( const QString &key )
    {
        QString imname = QString::null;
        if ( QString::compare( key, "uim" ) == 0 )
        {
            imname = uim_get_default_im_name( setlocale( LC_ALL, NULL ) );
            QStringList langs = languages( "uim" );
            QUimInputContext *uic = new QUimInputContextWithSlave( imname, langs[ 0 ] );
            return uic;
        }

        return NULL;
    }

    QStringList languages( const QString &key )
    {
        if ( key == QString( "uim" ) )
            return "ja:ko:zh:*";

        return QStringList();
    }

    QString displayName( const QString &key )
    {
        return QString( key ) + " (" + languages( key ) [ 0 ] + ")";
    }

    QString description( const QString &key )
    {
        return displayName( key ) + ": the universal input method framework";
    }

protected:
    void uimInit();
    void uimQuit();

    bool uimReady;
};

void UimInputContextPlugin::uimInit()
{
    if ( !uim_init() )
        uimReady = true;
}

void UimInputContextPlugin::uimQuit()
{
    if ( uimReady )
    {
        uim_quit();
        uimReady = false;
    }
}

Q_EXPORT_PLUGIN( UimInputContextPlugin )
