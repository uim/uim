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
#include <config.h>

#include "toolbar-common-uimstateindicator.h"

#include <qsocketnotifier.h>
#include <qstring.h>
#include <qtextcodec.h>
#include <qstringlist.h>
#include <qpoint.h>
#include <qtooltip.h>

#include <string.h>
#include <stdlib.h>

static const QString ICONDIR = UIM_PIXMAPSDIR;
static int uim_fd;
static QHelperToolbarButton *fallbackButton = NULL;
static QSocketNotifier *notifier = NULL;

UimStateIndicator::UimStateIndicator( QWidget *parent, const char *name, WFlags f )
        : QHBox( parent, name, f )
{
    if ( !fallbackButton )
    {
        fallbackButton = new QHelperToolbarButton( this );
        fallbackButton->setText( "?" );
        fallbackButton->show();
    }

    buttons.setAutoDelete( true );
    buttons.clear();

    uim_fd = -1;
    checkHelperConnection();
    uim_helper_client_get_prop_list();
}


UimStateIndicator::~UimStateIndicator()
{
    if ( notifier )
        delete notifier;
    notifier = NULL;

    buttons.clear();
}

int UimStateIndicator::getNumButtons()
{
    return buttons.count();
}

void UimStateIndicator::checkHelperConnection()
{
    if ( uim_fd < 0 )
    {
        uim_fd = uim_helper_init_client_fd( helper_disconnect_cb );
        if ( uim_fd > 0 )
        {
            if ( notifier )
                delete notifier;
            notifier = new QSocketNotifier( uim_fd, QSocketNotifier::Read );
            QObject::connect( notifier, SIGNAL( activated( int ) ),
                              this, SLOT( slotStdinActivated( int ) ) );
        }
    }
}
void UimStateIndicator::parseHelperStr( const QString& str )
{
    const QStringList lines = QStringList::split( "\n", str );
    if ( !lines.isEmpty() && !lines[ 0 ].isEmpty() )
    {
        if ( lines[ 0 ] == "prop_list_update" )
            propListUpdate( lines );
        else if ( lines[ 0 ] == "prop_label_update" )
            propLabelUpdate( lines );
    }
}

void UimStateIndicator::propListUpdate( const QStringList& lines )
{
    if ( !buttons.isEmpty() )
        buttons.clear();

    QHelperPopupMenu *popupMenu = NULL;

    QStringList::ConstIterator it = lines.begin();
    const QStringList::ConstIterator end = lines.end();
    for ( ; it != end; ++it )
    {
        const QStringList fields = QStringList::split( "\t", ( *it ) );

        if ( !fields.isEmpty() && !fields[ 0 ].isEmpty() )
        {
            if ( fields[ 0 ].startsWith( "branch" ) )
            {
                if ( fallbackButton )
                {
                    delete fallbackButton;
                    fallbackButton = NULL;
                }
                // create popup
                popupMenu = new QHelperPopupMenu( 0 );
                popupMenu->setCheckable( true );

                // create button
                QHelperToolbarButton *button = new QHelperToolbarButton( this );
                QPixmap icon = QPixmap(ICONDIR + "/" + fields[1] + ".png");
                if (!icon.isNull())
                    button->setPixmap(icon);
                else
                    button->setText( fields[ 2 ] );
                QToolTip::add( button, fields[ 3 ] );
                button->setPopup( popupMenu );
                button->setPopupDelay( 50 );
                button->show();

                buttons.append( button );
            }
            else if ( fields[ 0 ].startsWith( "leaf" ) )
            {
                if ( popupMenu
                        && !fields[ 1 ].isEmpty()
                        && !fields[ 3 ].isEmpty()
                        && !fields[ 4 ].isEmpty()
                        && !fields[ 5 ].isEmpty() )
                {
                    int id = popupMenu->insertHelperItem( fields[1], fields[ 3 ], fields[ 4 ], fields[ 5 ] );
                    // check the item which is now used
                    if ( !fields[ 6 ].isEmpty() && fields[ 6 ] == "*" )
                        popupMenu->setItemChecked( id, true );
                }
            }
        }
    }
}

void UimStateIndicator::propLabelUpdate( const QStringList& lines )
{
    unsigned int i = 0;
    while ( !lines[ i ].isEmpty() )
        i++;

    if ( buttons.isEmpty() || buttons.count() != i - 2 )
    {
        uim_helper_client_get_prop_list();
        return ;
    }

    i = 1;
    while ( !lines[ i ].isEmpty() )
    {
        const QStringList fields = QStringList::split( "\t", lines[ i ] );
        if ( !fields.isEmpty() && !fields[ 0 ].isEmpty() && !fields[ 1 ].isEmpty() )
        {
            // set button label
            buttons.at( i - 2 ) ->setText( fields[ 0 ] );
            // set tooltip
            QToolTip::add( buttons.at( i - 2 ), fields[ 1 ] );
        }

        i++;
    }
}

void UimStateIndicator::helper_disconnect_cb()
{
    uim_fd = -1;
    QObject::disconnect( notifier, SIGNAL( activated( int ) ), 0, 0 );
}

void UimStateIndicator::slotStdinActivated( int /*socket*/ )
{
    uim_helper_read_proc( uim_fd );

    QString tmp = QString::null;
    char *s;
    while ( ( s = uim_helper_get_message() ) )
    {
        const QStringList lines = QStringList::split( "\n", QString( s ) );
        if ( !lines.isEmpty() && !lines[ 1 ].isEmpty() && lines[ 1 ].startsWith( "charset" ) )
        {
            /* get charset */
            QString charset = QStringList::split( "=", lines[ 1 ] ) [ 1 ];

            /* convert to unicode */
            QTextCodec *codec = QTextCodec::codecForName( charset );
            tmp = codec->toUnicode( s );
        }
        else
        {
            /* no charset */
            tmp = s;
        }

        parseHelperStr( tmp );
	free( s );
    }
}


/**/

QHelperPopupMenu::QHelperPopupMenu( QWidget *parent, const char *name )
    : QPopupMenu( parent, name )
{
    msgDict.setAutoDelete( true );
    msgDict.clear();
}

QHelperPopupMenu::~QHelperPopupMenu()
{
    msgDict.clear();
}

int QHelperPopupMenu::insertHelperItem( const QString &indicationIdStr,
                                        const QString &menulabelStr,
                                        const QString &menutooltipStr,
                                        const QString &menucommandStr )
{
    int id;
    QPixmap icon = QPixmap(ICONDIR + "/" + indicationIdStr + ".png");

    if (!icon.isNull())
        id = insertItem( icon, menulabelStr, this, SLOT( slotMenuActivated( int ) ) );
    else
        id = insertItem( menulabelStr, this, SLOT( slotMenuActivated( int ) ) );

    setWhatsThis( id, menutooltipStr );
    msgDict.insert( id, new QString( menucommandStr ) );

    return id;
}

void QHelperPopupMenu::slotMenuActivated( int id )
{
    QString msg = *msgDict.find( id );
    msg.prepend( "prop_activate\n" );
    msg.append( "\n" );
    uim_helper_send_message( uim_fd, ( const char* ) msg );
}

#include "toolbar-common-uimstateindicator.moc"
