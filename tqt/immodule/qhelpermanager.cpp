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

#include <tqsocketnotifier.h>
#include <tqstring.h>
#include <tqstringlist.h>
#include <tqtextcodec.h>

#include "uim/uim.h"
#include "uim/uim-util.h"
#include "uim/uim-helper.h"
#include "uim/uim-im-switcher.h"

#include "qhelpermanager.h"
#include "quiminputcontext.h"
#include "quiminfomanager.h"
#include "plugin.h"

static int im_uim_fd = 0;
static TQSocketNotifier *notifier = NULL;

extern QUimInputContext *focusedInputContext;
extern bool disableFocusedContext;

extern TQPtrList<QUimInputContext> contextList;

QUimHelperManager::QUimHelperManager( TQObject *parent, const char *name )
        : TQObject( parent, name )
{
    notifier = NULL;
    im_uim_fd = -1;
}

QUimHelperManager::~QUimHelperManager()
{
    if ( im_uim_fd != -1 )
        uim_helper_close_client_fd( im_uim_fd );
}

void QUimHelperManager::checkHelperConnection(uim_context uc)
{
    if ( im_uim_fd < 0 )
    {
        im_uim_fd = uim_helper_init_client_fd( QUimHelperManager::helper_disconnect_cb );

        if ( im_uim_fd >= 0 )
        {
            notifier = new TQSocketNotifier( im_uim_fd, TQSocketNotifier::Read );
            TQObject::connect( notifier, TQ_SIGNAL( activated( int ) ),
                              this, TQ_SLOT( slotStdinActivated( int ) ) );
            uim_set_uim_fd(uc, im_uim_fd);
        }
    }
}

void QUimHelperManager::slotStdinActivated( int /*socket*/ )
{
    char* tmp;

    uim_helper_read_proc( im_uim_fd );
    while ( ( tmp = uim_helper_get_message() ) )
    {
        parseHelperStr( TQString::fromUtf8( tmp ) );
        free( tmp );
    }
}

void QUimHelperManager::parseHelperStr( const TQString &str )
{
    if ( focusedInputContext && !disableFocusedContext )
    {
        if ( str.startsWith( "prop_list_get" ) )
            uim_prop_list_update( focusedInputContext->uimContext() );
        else if ( str.startsWith( "prop_label_get" ) )
            uim_prop_label_update( focusedInputContext->uimContext() );
        else if ( str.startsWith( "prop_activate" ) )
        {
            TQStringList lines = TQStringList::split( "\n", str );
            uim_prop_activate( focusedInputContext->uimContext(), lines[ 1 ].local8Bit() );
        }
        else if ( str.startsWith( "im_list_get" ) )
        {
            sendImList();
        }
        else if ( str.startsWith( "commit_string" ) )
        {
            TQStringList lines = TQStringList::split( "\n", str );
            if ( !lines.isEmpty() && !lines[ 1 ].isEmpty() ) {
                TQString commit_str = TQString::null;

                if ( lines[ 1 ].startsWith( "charset" ) ) {
                    /* get charset */
                    TQString charset = TQStringList::split( "=", lines[ 1 ] ) [ 1 ];

                    /* convert to unicode */
                    TQTextCodec *codec = TQTextCodec::codecForName( charset.local8Bit() );
                    if ( codec && !lines[ 2 ].isEmpty() )
                        commit_str = codec->toUnicode( lines[ 2 ].local8Bit() );
                } else {
                    commit_str = lines[ 1 ];
                }

                focusedInputContext->commitString( commit_str );
            }
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
        QUimInputContext * cc;
        TQStringList list = TQStringList::split( "\n", str );
        if ( !list.isEmpty() && !list[ 0 ].isEmpty() &&
                !list[ 1 ].isEmpty() && !list[ 2 ].isEmpty() )
        {
            for ( cc = contextList.first(); cc; cc = contextList.next() )
            {
                uim_prop_update_custom( cc->uimContext(), list[ 1 ].utf8(), list[ 2 ].utf8() );
                break;  /* all custom variables are global */
            }
        }
    }
    else if ( str.startsWith( "custom_reload_notify" ) )
    {
        uim_prop_reload_configs();

        QUimInfoManager *infoManager =
            UimInputContextPlugin::getQUimInfoManager();
	infoManager->initUimInfo();
    }
}

void QUimHelperManager::parseHelperStrImChange( const TQString &str )
{
    QUimInputContext * cc;
    TQStringList list = TQStringList::split( "\n", str );
    TQString im_name = list[ 1 ];
    TQString im_name_sym = "'" + im_name;

    if ( str.startsWith( "im_change_this_text_area_only" ) )
    {
        if ( focusedInputContext )
        {
            uim_switch_im( focusedInputContext->uimContext(), im_name.utf8() );
            uim_prop_list_update( focusedInputContext->uimContext() );
            focusedInputContext->readIMConf();
        }
    }
    else if ( str.startsWith( "im_change_whole_desktop" ) )
    {
        for ( cc = contextList.first(); cc; cc = contextList.next() )
        {
            uim_switch_im( cc->uimContext(), im_name.utf8() );
            cc->readIMConf();
            uim_prop_update_custom( cc->uimContext(),
	                            "custom-preserved-default-im-name",
				    im_name_sym.utf8() );
        }
    }
    else if ( str.startsWith( "im_change_this_application_only" ) )
    {
        if ( focusedInputContext )
        {
            for ( cc = contextList.first(); cc; cc = contextList.next() )
            {
                uim_switch_im( cc->uimContext(), im_name.utf8() );
                cc->readIMConf();
                uim_prop_update_custom( cc->uimContext(),
                                        "custom-preserved-default-im-name",
                                        im_name_sym.utf8() );
            }
        }
    }
}

void QUimHelperManager::sendImList()
{
    if ( !focusedInputContext )
        return ;

    TQString msg = "im_list\ncharset=UTF-8\n";
    const char* current_im_name = uim_get_current_im_name( focusedInputContext->uimContext() );

    QUimInfoManager *infoManager = UimInputContextPlugin::getQUimInfoManager();
    TQValueList<uimInfo> info = infoManager->getUimInfo();
    TQValueList<uimInfo>::iterator it;

    for ( it = info.begin(); it != info.end(); ++it )
    {
        TQString leafstr;
        leafstr.sprintf( "%s\t%s\t%s\t",
                         ( *it ).name.utf8().data(),
                         uim_get_language_name_from_locale( ( *it ).lang.utf8() ),
                         ( *it).short_desc.utf8().data() );

        if ( TQString::compare( ( *it ).name, current_im_name ) == 0 )
            leafstr.append( "selected" );

        leafstr.append( "\n" );

        msg += leafstr;
    }

    uim_helper_send_message( im_uim_fd, ( const char* ) msg.utf8() );
}

void QUimHelperManager::send_im_change_whole_desktop( const char *name )
{
    TQString msg;

    msg.sprintf("im_change_whole_desktop\n%s\n", name);
    uim_helper_send_message( im_uim_fd, msg.utf8() );
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
    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    if ( ic != focusedInputContext || disableFocusedContext )
        return;

    TQString msg = "prop_list_update\ncharset=UTF-8\n";
    msg += TQString::fromUtf8( str );

    uim_helper_send_message( im_uim_fd, ( const char* ) msg.utf8() );
}

void QUimHelperManager::update_prop_label_cb( void *ptr, const char *str )
{
    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    if ( ic != focusedInputContext || disableFocusedContext )
        return;

    TQString msg = "prop_label_update\ncharset=UTF-8\n";
    msg += TQString::fromUtf8( str );

    uim_helper_send_message( im_uim_fd, ( const char* ) msg.utf8() );
}

#include "qhelpermanager.moc"
