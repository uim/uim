/*
Copyright (C) 2004 Kazuki Ohta <mover@hct.zaq.ne.jp>
*/
#include "qhelpermanager.h"

#include <qsocketnotifier.h>
#include <qstring.h>
#include <qstringlist.h>

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <uim/uim-im-switcher.h>

#include "quiminputcontext.h"

static int im_uim_fd = 0;
static QSocketNotifier *notifier = NULL;

extern QUimInputContext *focusedInputContext;
extern bool disableFocusedContext;

extern QList<QUimInputContext*> contextList;
extern QList<UIMInfo> uimInfo;

QUimHelperManager::QUimHelperManager( QObject *parent )
        : QObject( parent )
{
    notifier = NULL;
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
            QObject::connect( notifier, SIGNAL( activated( int ) ),
                              this, SLOT( slotStdinActivated( int ) ) );
        }
    }
}

void QUimHelperManager::slotStdinActivated( int /*socket*/ )
{
    QString tmp;

    uim_helper_read_proc( im_uim_fd );
    for ( ;; )
    {
        tmp = QString::fromUtf8( uim_helper_get_message() );

        if ( !tmp.isEmpty() )
            parseHelperStr( tmp );
        else
            break;
    }
}

void QUimHelperManager::parseHelperStr( const QString &str )
{
    if ( focusedInputContext && !disableFocusedContext )
    {
        if ( str.startsWith( "prop_list_get" ) )
            uim_prop_list_update( focusedInputContext->uimContext() );
        else if ( str.startsWith( "prop_label_get" ) )
            uim_prop_label_update( focusedInputContext->uimContext() );
        else if ( str.startsWith( "prop_activate" ) )
        {
            QStringList list = str.split( "\n" );
            uim_prop_activate( focusedInputContext->uimContext(), ( const char* ) list[ 1 ] );
        }
        else if ( str.startsWith( "im_list_get" ) )
        {
            sendImList();
        }
        else if ( str.startsWith( "focus_in" ) )
        {
            // We shouldn't do "focusedInputContext = NULL" here, because some
            // window manager has some focus related bugs.
            disableFocusedContext = true;
        }
    }

    /**
     * This part should be processed even if not focused
     */
    if ( str.startsWith( "im_change" ) )
    {
        // for IM switcher
        parseHelperStrImChange( str );
    }
    else if ( str.startsWith( "prop_update_custom" ) )
    {
        // for custom api
        QStringList list = str.split( "\n" );
        if ( !list.isEmpty() && !list[ 0 ].isEmpty() &&
                !list[ 1 ].isEmpty() && !list[ 2 ].isEmpty() )
        {
            for ( int i = 0; i < contextList.size(); ++i )
            {
                uim_prop_update_custom( contextList.at( i ) ->uimContext(), list[ 1 ], list[ 2 ] );
                break;  /* all custom variables are global */
            }
        }
    }
}


void QUimHelperManager::parseHelperStrImChange( const QString &str )
{
    QStringList list = str.split( "\n" );
    QString im_name = list[ 1 ];

    if ( str.startsWith( "im_change_this_text_area_only" ) )
    {
        if ( focusedInputContext )
        {
            uim_switch_im( focusedInputContext->uimContext(), ( const char* ) im_name );
            uim_prop_list_update( focusedInputContext->uimContext() );
            focusedInputContext->readIMConf();
        }
    }
    else if ( str.startsWith( "im_change_whole_desktop" ) )
    {
        for ( int i = 0; i < contextList.size(); ++i )
        {
            QUimInputContext *uic = contextList.at( i );
            uim_switch_im( uic->uimContext(), ( const char* ) im_name );
            uic->readIMConf();
        }

    }
    else if ( str.startsWith( "im_change_this_application_only" ) )
    {
        if ( focusedInputContext )
        {
            for ( int i = 0; i < contextList.size(); ++i )
            {
                QUimInputContext *uic = contextList.at( i );
                uim_switch_im( uic->uimContext(), ( const char* ) im_name );
                uic->readIMConf();
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

    QList<UIMInfo>::iterator it;
    for ( it = uimInfo.begin(); it != uimInfo.end(); ++it )
    {
        QString leafstr;
        leafstr.sprintf( "%s\t%s\t%s\t",
                         ( *it ).name,
                         ( *it ).lang,
                         ( *it ).short_desc );

        if ( QString::compare( ( *it ).name, current_im_name ) == 0 )
            leafstr.append( "selected" );

        leafstr.append( "\n" );

        msg += leafstr;
    }

    uim_helper_send_message( im_uim_fd, ( const char* ) msg.utf8() );
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
    QString msg = "prop_list_update\ncharset=UTF-8\n";
    msg += QString::fromUtf8( str );

    uim_helper_send_message( im_uim_fd, ( const char* ) msg.utf8() );
}

void QUimHelperManager::update_prop_label_cb( void *ptr, const char *str )
{
    QString msg = "prop_label_update\ncharset=UTF-8\n";
    msg += QString::fromUtf8( str );

    uim_helper_send_message( im_uim_fd, ( const char* ) msg.utf8() );
}
