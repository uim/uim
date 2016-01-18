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
#include "qhelpermanager.h"

#include <QtCore/QSocketNotifier>
#include <QtCore/QStringList>
#include <QtCore/QTextCodec>

#include "uim/uim.h"
#include "uim/uim-helper.h"
#include "uim/uim-im-switcher.h"
#include "uim/uim-util.h"

#include "plugin.h"
#include "quiminfomanager.h"
#if QT_VERSION < 0x050000
# include "quiminputcontext.h"
#else
# include "quimplatforminputcontext.h"
#endif

static int im_uim_fd = 0;
static QSocketNotifier *notifier = 0;

#if QT_VERSION < 0x050000
extern QUimInputContext *focusedInputContext;
#else
extern QUimPlatformInputContext *focusedInputContext;
#endif
extern bool disableFocusedContext;

#if QT_VERSION < 0x050000
extern QList<QUimInputContext *> contextList;
#else
extern QList<QUimPlatformInputContext *> contextList;
#endif

QUimHelperManager::QUimHelperManager( QObject *parent )
        : QObject( parent )
{
    notifier = 0;
    im_uim_fd = -1;
}

QUimHelperManager::~QUimHelperManager()
{
    if ( im_uim_fd != -1 )
        uim_helper_close_client_fd( im_uim_fd );
}

void QUimHelperManager::checkHelperConnection()
{
    if ( im_uim_fd < 0 )
    {
        im_uim_fd = uim_helper_init_client_fd( QUimHelperManager::helper_disconnect_cb );

        if ( im_uim_fd >= 0 )
        {
            notifier = new QSocketNotifier( im_uim_fd, QSocketNotifier::Read );
            connect( notifier, SIGNAL( activated( int ) ),
                              this, SLOT( slotStdinActivated() ) );
        }
    }
}

void QUimHelperManager::slotStdinActivated()
{
    char *tmp;
    uim_helper_read_proc( im_uim_fd );
    while ((tmp = uim_helper_get_message()))
    {
        parseHelperStr(QString::fromUtf8(tmp));
        free(tmp);
    }
}

void QUimHelperManager::parseHelperStr( const QString &str )
{
    if ( focusedInputContext && !disableFocusedContext )
    {
        if ( str.startsWith( QLatin1String( "prop_list_get" ) ) )
            uim_prop_list_update( focusedInputContext->uimContext() );
        else if ( str.startsWith( QLatin1String( "prop_label_get" ) ) )
            uim_prop_label_update( focusedInputContext->uimContext() );
        else if ( str.startsWith( QLatin1String( "prop_activate" ) ) )
        {
            QStringList list = str.split( '\n' );
            uim_prop_activate( focusedInputContext->uimContext(),
                               list[ 1 ].toUtf8().data() );
        }
        else if ( str.startsWith( QLatin1String( "im_list_get" ) ) )
        {
            sendImList();
        }
        else if ( str.startsWith( QLatin1String( "commit_string" ) ) )
        {
            QStringList lines = str.split( '\n' );
            if ( !lines.isEmpty() && !lines[ 1 ].isEmpty() ) {
                QString commit_str;
                
                if ( lines[ 1 ].startsWith( QLatin1String( "charset" ) ) ) {
                    /* get charset */
                    QString charset = lines[ 1 ].split( '=' ) [ 1 ];

                    /* convert to unicode */
                    QTextCodec *codec
                        = QTextCodec::codecForName( charset.toLatin1() );
                    if ( codec && !lines[ 2 ].isEmpty() )
                        commit_str = codec->toUnicode( lines[ 2 ].toLatin1() );
                } else {
                    commit_str = lines[ 1 ];
                }

                focusedInputContext->commitString( commit_str );
            }
        }
        else if ( str.startsWith( QLatin1String( "focus_in" ) ) )
        {
            // We shouldn't do "focusedInputContext = NULL" here, because some
            // window manager has some focus related bugs.
            disableFocusedContext = true;
        }
    }

    /**
     * This part should be processed even if not focused
     */
    if ( str.startsWith( QLatin1String( "im_change" ) ) )
    {
        // for IM switcher
        parseHelperStrImChange( str );
    }
    else if ( str.startsWith( QLatin1String( "prop_update_custom" ) ) )
    {
        // for custom api
        QStringList list = str.split( '\n' );
        if ( !list.isEmpty() && !list[ 0 ].isEmpty() &&
                !list[ 1 ].isEmpty() && !list[ 2 ].isEmpty() )
        {
#if QT_VERSION < 0x050000
            QList<QUimInputContext *>::iterator it;
#else
            QList<QUimPlatformInputContext *>::iterator it;
#endif
            for ( it = contextList.begin(); it != contextList.end(); ++it )
            {
                uim_prop_update_custom( ( *it )->uimContext(),
                                        list[ 1 ].toUtf8().data(),
                                        list[ 2 ].toUtf8().data() );
                if ( list[ 1 ]
                        == QLatin1String( "candidate-window-position" ) )
                    ( *it )->updatePosition();
                if ( list[ 1 ]
                        == QLatin1String( "candidate-window-style" ) )
                    ( *it )->updateStyle();
                break;  /* all custom variables are global */
            }
        }
    }
    else if ( str.startsWith( QLatin1String( "custom_reload_notify" ) ) )
    {
        uim_prop_reload_configs();

        QUimInfoManager *infoManager =
            UimInputContextPlugin::getQUimInfoManager();
        infoManager->initUimInfo();

#if QT_VERSION < 0x050000
        QList<QUimInputContext *>::iterator it;
#else
        QList<QUimPlatformInputContext *>::iterator it;
#endif
        for ( it = contextList.begin(); it != contextList.end(); ++it ) {
            ( *it )->updatePosition();
            ( *it )->updateStyle();
        }
    }
}

void QUimHelperManager::parseHelperStrImChange( const QString &str )
{
    QStringList list = str.split( '\n' );
    QString im_name = list[ 1 ];
    QString im_name_sym = '\'' + im_name;

    if ( str.startsWith( QLatin1String( "im_change_this_text_area_only" ) ) )
    {
        if ( focusedInputContext )
        {
            uim_switch_im( focusedInputContext->uimContext(),
                           im_name.toUtf8().data() );
            uim_prop_list_update( focusedInputContext->uimContext() );
            focusedInputContext->updatePosition();
        }
    }
    else if ( str.startsWith( QLatin1String( "im_change_whole_desktop" ) ) )
    {
#if QT_VERSION < 0x050000
        QList<QUimInputContext *>::iterator it;
#else
        QList<QUimPlatformInputContext *>::iterator it;
#endif
        for ( it = contextList.begin(); it != contextList.end(); ++it )
        {
            uim_switch_im( ( *it )->uimContext(), im_name.toUtf8().data() );
            ( *it )->updatePosition();
            uim_prop_update_custom( ( *it )->uimContext(),
                                    "custom-preserved-default-im-name",
                                    im_name_sym.toUtf8().data() );
        }
    }
    else if ( str.startsWith( QLatin1String(
        "im_change_this_application_only" ) ) )
    {
        if ( focusedInputContext )
        {
#if QT_VERSION < 0x050000
            QList<QUimInputContext *>::iterator it;
#else
            QList<QUimPlatformInputContext *>::iterator it;
#endif
            for ( it = contextList.begin(); it != contextList.end(); ++it )
            {
                uim_switch_im( ( *it )->uimContext(), im_name.toUtf8().data() );
                ( *it )->updatePosition();
                uim_prop_update_custom( ( *it )->uimContext(),
                                        "custom-preserved-default-im-name",
                                        im_name_sym.toUtf8().data() );
            }
        }
    }
}

void QUimHelperManager::sendImList()
{
    if ( !focusedInputContext )
        return ;

    QString msg = "im_list\ncharset=UTF-8\n";
    const char* current_im_name = uim_get_current_im_name( focusedInputContext->uimContext() );

    QUimInfoManager *infoManager = UimInputContextPlugin::getQUimInfoManager();
    QList<uimInfo> info = infoManager->getUimInfo();
    QList<uimInfo>::iterator it;

    for ( it = info.begin(); it != info.end(); ++it )
    {
        QString leafstr;
        leafstr.sprintf( "%s\t%s\t%s\t",
                         ( *it ).name.toUtf8().data(),
                         uim_get_language_name_from_locale( ( *it ).lang.toUtf8().data() ),
                         ( *it).short_desc.toUtf8().data() );

        if ( QString::compare( ( *it ).name, current_im_name ) == 0 )
            leafstr.append( "selected" );

        leafstr.append( "\n" );

        msg += leafstr;
    }

    uim_helper_send_message( im_uim_fd, msg.toUtf8().data() );
}

void QUimHelperManager::send_im_change_whole_desktop( const char *name )
{
    QString msg;

    msg.sprintf("im_change_whole_desktop\n%s\n", name);
    uim_helper_send_message( im_uim_fd, msg.toUtf8().data() );
}

void QUimHelperManager::helper_disconnect_cb()
{
    im_uim_fd = -1;

    if ( notifier )
    {
        delete notifier;
        notifier = 0;
    }
}

void QUimHelperManager::update_prop_list_cb( void *ptr, const char *str )
{
#if QT_VERSION < 0x050000
    QUimInputContext *ic = static_cast<QUimInputContext*>( ptr );
#else
    QUimPlatformInputContext *ic = static_cast<QUimPlatformInputContext*>( ptr );
#endif

    if ( ic != focusedInputContext || disableFocusedContext )
        return;

    QString msg = "prop_list_update\ncharset=UTF-8\n";
    msg += QString::fromUtf8( str );

    uim_helper_send_message( im_uim_fd, msg.toUtf8().data() );

    ic->updateIndicator( msg );
}

void QUimHelperManager::update_prop_label_cb( void *ptr, const char *str )
{
#if QT_VERSION < 0x050000
    QUimInputContext *ic = static_cast<QUimInputContext*>( ptr );
#else
    QUimPlatformInputContext *ic = static_cast<QUimPlatformInputContext*>( ptr );
#endif
    if ( ic != focusedInputContext || disableFocusedContext )
        return;

    QString msg = "prop_label_update\ncharset=UTF-8\n";
    msg += QString::fromUtf8( str );

    uim_helper_send_message( im_uim_fd, msg.toUtf8().data() );
}
