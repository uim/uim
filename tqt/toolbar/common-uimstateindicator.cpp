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

#include "common-uimstateindicator.h"
#include <uim/uim-scm.h>

#include <tqsocketnotifier.h>
#include <tqstring.h>
#include <tqtextcodec.h>
#include <tqstringlist.h>
#include <tqpoint.h>
#include <tqtooltip.h>
#include <tqimage.h>
#include <tqfileinfo.h>

#include <cstring>
#include <cstdlib>

static const TQString ICONDIR = UIM_PIXMAPSDIR;
static int uim_fd;
static QHelperToolbarButton *fallbackButton = NULL;
static TQSocketNotifier *notifier = NULL;

UimStateIndicator::UimStateIndicator( TQWidget *parent, const char *name, WFlags f )
        : TQHBox( parent, name, f )
{
    if ( !fallbackButton )
    {
        fallbackButton = new QHelperToolbarButton( this );
        TQPixmap icon = TQPixmap( ICONDIR + "/" + "uim-icon.png" );
        if ( !icon.isNull() ) {
            TQImage image = icon.convertToImage();
            TQPixmap scaledIcon = image.smoothScale( ICON_SIZE, ICON_SIZE );
            fallbackButton->setPixmap( scaledIcon );
        } else {
            fallbackButton->setText( "?" );
        }
        fallbackButton->show();
    }

    buttons.setAutoDelete( true );
    buttons.clear();

    uim_fd = -1;
    checkHelperConnection();
    uim_helper_client_get_prop_list();
    popupMenuShowing = false;
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
            notifier = new TQSocketNotifier( uim_fd, TQSocketNotifier::Read );
            TQObject::connect( notifier, TQ_SIGNAL( activated( int ) ),
                              this, TQ_SLOT( slotStdinActivated( int ) ) );
        }
    }
}
void UimStateIndicator::parseHelperStr( const TQString& str )
{
    const TQStringList lines = TQStringList::split( "\n", str );
    if ( !lines.isEmpty() && !lines[ 0 ].isEmpty() )
    {
        if ( lines[ 0 ] == "prop_list_update" )
            propListUpdate( lines );
        else if ( lines[ 0 ] == "custom_reload_notify" )
            uim_prop_reload_configs();
    }
}

void UimStateIndicator::propListUpdate( const TQStringList& lines )
{
    TQPtrList<QHelperToolbarButton> tmp_button_list;
    QHelperToolbarButton *old_button;
    QHelperPopupMenu *popupMenu = NULL;
    bool size_changed = false;

    if (popupMenuShowing)
        return;

    tmp_button_list = buttons;
    old_button = tmp_button_list.first();

    TQStringList::ConstIterator it = lines.begin();
    const TQStringList::ConstIterator end = lines.end();
    for ( ; it != end; ++it )
    {
        const TQStringList fields = TQStringList::split( "\t", ( *it ) );

        if ( !fields.isEmpty() && !fields[ 0 ].isEmpty() )
        {
            if ( fields[ 0 ].startsWith( "branch" ) )
            {
                if ( fallbackButton )
                {
                    delete fallbackButton;
                    fallbackButton = NULL;
                }

                // create button
                QHelperToolbarButton *button;
                if (old_button) {
                    button = old_button;
                    delete button->popup();
                } else {
                    button = new QHelperToolbarButton( this );
                    buttons.append( button );
                    size_changed = true;
                }
                uim_bool isDarkBg = uim_scm_symbol_value_bool("toolbar-icon-for-dark-background?");
                const TQString append = isDarkBg ? "_dark_background" : "";
                TQString fileName = ICONDIR + "/" + fields[1] + append + ".png";
                TQFileInfo fileInfo( fileName );
                if ( isDarkBg && !fileInfo.isReadable() )
		{
                    fileName = ICONDIR + "/" + fields[1] + ".png";
                }
                TQPixmap icon = TQPixmap( fileName );
                if (!icon.isNull()) {
                    TQImage image = icon.convertToImage();
                    TQPixmap scaledIcon = image.smoothScale( ICON_SIZE,
                                                             ICON_SIZE );
                    button->setPixmap( scaledIcon );
                } else {
                    button->setText( fields[ 2 ] );
                }
                TQToolTip::add( button, fields[ 3 ] );

                // create popup
                popupMenu = new QHelperPopupMenu( button );
                popupMenu->setCheckable( true );
                connect( popupMenu, TQ_SIGNAL( aboutToShow() ), this, TQ_SLOT( slotPopupMenuAboutToShow() ) );
                connect( popupMenu, TQ_SIGNAL( aboutToHide() ), this, TQ_SLOT( slotPopupMenuAboutToHide() ) );
                button->setPopup( popupMenu );
                button->setPopupDelay( 50 );

                button->show();

                old_button = tmp_button_list.next();
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

    if (old_button)
        size_changed = true;

    while (old_button) {
        QHelperToolbarButton *next;

        next = tmp_button_list.next();
        buttons.remove(old_button);
        old_button = next;
    }

    if (size_changed)
        emit indicatorResized();

    this->parentWidget()->show();
}

void UimStateIndicator::helper_disconnect_cb()
{
    uim_fd = -1;
    TQObject::disconnect( notifier, TQ_SIGNAL( activated( int ) ), 0, 0 );
}

void UimStateIndicator::slotStdinActivated( int /*socket*/ )
{
    uim_helper_read_proc( uim_fd );

    TQString tmp = TQString::null;
    char *s;
    while ( ( s = uim_helper_get_message() ) )
    {
        const TQStringList lines = TQStringList::split( "\n", TQString( s ) );
        if ( !lines.isEmpty() && !lines[ 1 ].isEmpty() && lines[ 1 ].startsWith( "charset" ) )
        {
            /* get charset */
            TQString charset = TQStringList::split( "=", lines[ 1 ] ) [ 1 ];

            /* convert to unicode */
            TQTextCodec *codec = TQTextCodec::codecForName( charset.local8Bit() );
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

void UimStateIndicator::slotPopupMenuAboutToShow()
{
    popupMenuShowing = true;
}

void UimStateIndicator::slotPopupMenuAboutToHide()
{
    popupMenuShowing = false;
}

/**/

QHelperPopupMenu::QHelperPopupMenu( TQWidget *parent, const char *name )
    : TQPopupMenu( parent, name )
{
    msgDict.setAutoDelete( true );
    msgDict.clear();
}

QHelperPopupMenu::~QHelperPopupMenu()
{
    msgDict.clear();
}

int QHelperPopupMenu::insertHelperItem( const TQString &indicationIdStr,
                                        const TQString &menulabelStr,
                                        const TQString &menutooltipStr,
                                        const TQString &menucommandStr )
{
    int id;
    uim_bool isDarkBg =
        uim_scm_symbol_value_bool("toolbar-icon-for-dark-background?");
    const TQString append = isDarkBg ? "_dark_background" : "";
    TQString fileName = ICONDIR + "/" + indicationIdStr + append + ".png";
    TQFileInfo fileInfo( fileName );
    if ( isDarkBg && !fileInfo.isReadable() )
    {
        fileName = ICONDIR + "/" + indicationIdStr + ".png";
    }
    TQPixmap icon = TQPixmap( fileName );

    if (!icon.isNull()) {
        TQImage image = icon.convertToImage();
        TQPixmap scaledIcon = image.smoothScale( ICON_SIZE, ICON_SIZE );
        id = insertItem( scaledIcon, menulabelStr, this,
                         TQ_SLOT( slotMenuActivated( int ) ) );
    } else {
        id = insertItem( menulabelStr, this, TQ_SLOT( slotMenuActivated( int ) ) );
    }

    setWhatsThis( id, menutooltipStr );
    msgDict.insert( id, new TQString( menucommandStr ) );

    return id;
}

void QHelperPopupMenu::slotMenuActivated( int id )
{
    TQString msg = *msgDict.find( id );
    msg.prepend( "prop_activate\n" );
    msg.append( "\n" );
    uim_helper_send_message( uim_fd, msg.utf8() );
}

#include "common-uimstateindicator.moc"
