/*

  Copyright (c) 2004-2005 Kazuki Ohta <mover@hct.zaq.ne.jp>
  Copyright (c) 2005-2013 uim Project http://code.google.com/p/uim/

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

#include "plugin.h"

#include <clocale>

#include <QtCore/QStringList>
#ifdef Q_WS_X11
# include <QtGui/QX11Info>
#endif
#if QT_VERSION < 0x050000
# include <QtGui/QInputContext>
#else
# include <qpa/qplatforminputcontext.h>
#endif

#include "uim/uim.h"
#include "uim/uim-scm.h"
#include "uim/uim-x-util.h"
#include "uim/counted-init.h"

#include "quiminfomanager.h"
#if QT_VERSION >= 0x050000
# undef Bool
# undef CursorShape
# undef Expose
# undef FocusIn
# undef FocusOut
# undef FontChange
# undef KeyPress
# undef KeyRelease
# undef None
#endif
#include "quiminputcontext_with_slave.h"

#define UIM_QT_LIST_SUBIM_AS_QTIM 0

QUimInfoManager *UimInputContextPlugin::infoManager = 0;


UimInputContextPlugin::UimInputContextPlugin()
{
    uimReady = false;
    uimInit();
}

UimInputContextPlugin::~UimInputContextPlugin()
{
    uimQuit();
}

QStringList UimInputContextPlugin::keys() const
{
    return createImList();
}

#if QT_VERSION < 0x050000
QInputContext *UimInputContextPlugin::create( const QString & key )
#else
QPlatformInputContext *UimInputContextPlugin::create( const QString & key, const QStringList & paramList )
#endif
{
#if QT_VERSION >= 0x050000
    Q_UNUSED(paramList);
#endif
    QString imname;

#if UIM_QT_LIST_SUBIM_AS_QTIM
    if ( key.startsWith( QLatin1String( "uim-" ) ) )
        imname = key.mid( 4 );
    else
#endif
    if ( key == "uim" )
        imname = uim_get_default_im_name( setlocale( LC_CTYPE, 0 ) );

#if QT_VERSION < 0x050000
    QUimInputContext *uic = new QUimInputContext( imname.toUtf8().data() );
#else
    QUimPlatformInputContext *uic
        = new QUimPlatformInputContext( imname.toUtf8().data() );
#endif

    return uic;
}

#if QT_VERSION < 0x050000
QStringList UimInputContextPlugin::languages( const QString & key )
{
    return createLanguageList( key );
}

QString UimInputContextPlugin::displayName( const QString & key )
{
    return key;
}

QString UimInputContextPlugin::description( const QString & key )
{
    return displayName( key ) + ": an input method provided via the uim input method framework";
}
#endif

QUimInfoManager *
UimInputContextPlugin::getQUimInfoManager()
{
    return infoManager;
}

void UimInputContextPlugin::uimInit()
{
    if ( !uim_counted_init() ) {
        if (!infoManager)
            infoManager = new QUimInfoManager();

	if (uim_scm_c_bool(uim_scm_callf("require-dynlib", "s", "xkb")))
	    uim_scm_callf("%xkb-set-display", "p", QX11Info::display());

#if UIM_QT_USE_JAPANESE_KANA_KEYBOARD_HACK
        uim_x_kana_input_hack_init( QX11Info::display() );
#endif
        uimReady = true;
    }
}

void UimInputContextPlugin::uimQuit()
{
    if ( uimReady )
    {
        uim_counted_quit();
        delete infoManager;
        uimReady = false;
    }
}



QStringList UimInputContextPlugin::createImList() const
{
    QStringList lst;

    // default
    lst.append( "uim" );
#ifdef ENABLE_DEBUG
    qDebug( "name = uim" );
#endif

#if UIM_QT_LIST_SUBIM_AS_QTIM
    uim_context tmp_uc = uim_create_context( 0, "UTF-8",
                         0, 0, uim_iconv, 0 );
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
            qDebug( "name = %s", qs.toUtf8().data() );
#endif
        }
    }
    uim_release_context( tmp_uc );
#endif

    return lst;
}

#if QT_VERSION < 0x050000
QStringList UimInputContextPlugin::createLanguageList( const QString &key ) const
{
    if ( key == "uim" )
        return QStringList() << "ja" << "ko" << "zh" << "*";

#if UIM_QT_LIST_SUBIM_AS_QTIM
    uim_context tmp_uc = uim_create_context( 0, "UTF-8",
                         0, 0, uim_iconv, 0 );
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
                QStringList langs = QString( lang ).split( ':' );
                return langs;
            }
        }
    }
    uim_release_context( tmp_uc );
#endif

    return QStringList( "" );
}
#endif

#if QT_VERSION < 0x050000
Q_EXPORT_PLUGIN2( uiminputcontextplugin, UimInputContextPlugin )
#endif
