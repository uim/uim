/*

Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
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

#include "quiminputcontext_with_slave.h"

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
        return createImList();
    }

    QInputContext *create( const QString &key )
    {
        QString imname = QString::null;
        if ( QString::compare( key, "uim" ) == 0 )
            imname = uim_get_default_im_name( setlocale( LC_ALL, NULL ) );
        else
            imname = key.mid( 4 );

        QStringList langs = createLanguageList( key );
        QUimInputContext *uic = new QUimInputContextWithSlave( imname, langs[ 0 ] );

        return uic;
    }

    QStringList languages( const QString &key )
    {
        return createLanguageList( key );
    }

    QString displayName( const QString &key )
    {
        return QString( key ) + " (" + languages( key ) [ 0 ] + ")";
    }

    QString description( const QString &key )
    {
        return displayName( key ) + ": an input method provided via the uim input method framework";
    }

protected:
    void uimInit();
    void uimQuit();

    QStringList createImList() const;
    QStringList createLanguageList( const QString &key ) const;

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

QStringList UimInputContextPlugin::createImList() const
{
    QStringList lst;

    // default
    lst.append( "uim" );

    uim_context tmp_uc = uim_create_context( NULL, "UTF-8",
                         NULL, NULL, uim_iconv, NULL );
    int nr = uim_get_nr_im( tmp_uc );
    if ( uimReady )
    {
        for ( int i = 0; i < nr; i++ )
        {
            const char *name = uim_get_im_name( tmp_uc, i );
            QString qs( name );
            qs = "uim-" + qs;
            lst << qs;

#ifdef ENABLE_DEBUG
            qDebug( "name = %s", name );
#endif
        }
    }
    uim_release_context( tmp_uc );

    return lst;
}

QStringList UimInputContextPlugin::createLanguageList( const QString &key ) const
{
    if ( key == QString( "uim" ) )
        return "ja:ko:zh:*";

    uim_context tmp_uc = uim_create_context( NULL, "UTF-8",
                         NULL, NULL, uim_iconv, NULL );
    int nr = uim_get_nr_im( tmp_uc );
    if ( uimReady )
    {
        for ( int i = 0; i < nr; i++ )
        {
            const char *name = uim_get_im_name( tmp_uc, i );
            const char *lang = uim_get_im_language( tmp_uc, i );

            if ( key == QString( "uim-" ) + name )
            {
                // ":" separated languages for future extension
                QStringList langs = QStringList::split( ":", lang );
                return langs;
            }
        }
    }
    uim_release_context( tmp_uc );

    return QStringList();
}

Q_EXPORT_PLUGIN( UimInputContextPlugin )
