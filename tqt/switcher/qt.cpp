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

#include "qt.h"

#include <tqapplication.h>
#include <tqlistview.h>
#include <tqvbuttongroup.h>
#include <tqradiobutton.h>
#include <tqsocketnotifier.h>
#include <tqpushbutton.h>
#include <tqtextcodec.h>
#include <tqevent.h>
#include <tqlayout.h>
#include <tqsizepolicy.h>

#include <cstdlib>
#include <clocale>

#include <uim/uim-scm.h>
#include <uim/uim-custom.h>
#include "qtgettext.h"

#define NAME_COLUMN 0

static int uim_fd;
static bool customEnabled;
static TQSocketNotifier *notifier = NULL;

int main( int argc, char **argv )
{
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    bind_textdomain_codeset(PACKAGE, "UTF-8"); // ensure code encoding is UTF8-

    setenv("XMODIFIERS", "@im=none", 1);

    TQApplication a( argc, argv );

    UimImSwitcher switcher;
    switcher.resize( 550, 400 );
    switcher.setCaption( _( "uim input method switcher" ) );
    a.setMainWidget( &switcher );
    switcher.show();

    return a.exec();
}


UimImSwitcher::UimImSwitcher( TQWidget *parent, const char *name )
        : TQDialog( parent, name )
{
    /* connect to uim helper message bus */
    uim_fd = -1;
    checkHelperConnection();

    /* to check if another uim-im-switcher exists */
    uim_helper_send_message( uim_fd, "im_switcher_start\n" );

    /* to load input method list */
    uim_helper_send_message( uim_fd, "im_list_get\n" );

    uim_init();
    customEnabled = uim_custom_enable();

    /* create GUI */
    createGUI();
}

UimImSwitcher::~UimImSwitcher()
{
}

void UimImSwitcher::createGUI()
{
    /* im list view */
    listview = new TQListView( this );
    listview->setSelectionMode( TQListView::Single );
    listview->setAllColumnsShowFocus( true );
    listview->addColumn( _( "InputMethodName" ) );
    listview->addColumn( _( "Language" ) );
    listview->addColumn( _( "Description" ) );

    /* radio buttons for the switcher coverage */
    TQRadioButton *button;
    vbGroup = new TQVButtonGroup( _( "Effective coverage" ), this );
    button = new TQRadioButton( _( "whole desktop" ), vbGroup );
    vbGroup->insert( button, ID_CHANGE_WHOLE_DESKTOP );
    button->setChecked( true ); // default is "whole desktop"
    button = new TQRadioButton( _( "current application only" ), vbGroup );
    vbGroup->insert( button, ID_CHANGE_THIS_APPLICATION_ONLY );
    button = new TQRadioButton( _( "current text area only" ), vbGroup );
    vbGroup->insert( button, ID_CHANGE_THIS_TEXT_AREA_ONLY );

    /* cancel, apply & ok button */
    okButton = new TQPushButton( this );
    okButton->setText( _( "OK" ) );
    okButton->setSizePolicy( TQSizePolicy::Expanding, TQSizePolicy::Fixed );
    TQObject::connect( okButton, TQ_SIGNAL( clicked() ),
                      this, TQ_SLOT( slotChangeInputMethodAndQuit() ) );
    applyButton = new TQPushButton( this );
    applyButton->setText( _( "Apply" ) );
    applyButton->setSizePolicy( TQSizePolicy::Expanding, TQSizePolicy::Fixed );
    TQObject::connect( applyButton, TQ_SIGNAL( clicked() ),
                      this, TQ_SLOT( slotChangeInputMethod() ) );
    cancelButton = new TQPushButton( this );
    cancelButton->setText( _( "Cancel" ) );
    cancelButton->setSizePolicy( TQSizePolicy::Expanding, TQSizePolicy::Fixed );
    TQObject::connect( cancelButton, TQ_SIGNAL( clicked() ),
                      tqApp, TQ_SLOT( quit() ) );
    TQHBoxLayout *buttonLayout = new TQHBoxLayout;
    buttonLayout->addStretch( 0 );
    buttonLayout->addWidget( okButton );
    buttonLayout->addWidget( applyButton );
    buttonLayout->addWidget( cancelButton );

    // main layout
    TQVBoxLayout *mainLayout = new TQVBoxLayout( this );
    mainLayout->setMargin( 6 );
    mainLayout->setSpacing( 6 );
    mainLayout->addWidget( listview );
    mainLayout->addWidget( vbGroup );
    mainLayout->addLayout( buttonLayout );
}


void UimImSwitcher::checkHelperConnection()
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

void UimImSwitcher::helper_disconnect_cb()
{
    uim_fd = -1;
    TQObject::disconnect( notifier, TQ_SIGNAL( activated( int ) ), 0, 0 );
}

void UimImSwitcher::slotChangeInputMethodAndQuit()
{
    slotChangeInputMethod();
    tqApp->quit();
}

void UimImSwitcher::slotChangeInputMethod()
{

    switch ( vbGroup->selectedId() )
    {
    case ID_CHANGE_WHOLE_DESKTOP:
        sendMessageImChange( "im_change_whole_desktop\n" );
        saveDefaultIm();
        break;
    case ID_CHANGE_THIS_APPLICATION_ONLY:
        sendMessageImChange( "im_change_this_application_only\n" );
        break;
    case ID_CHANGE_THIS_TEXT_AREA_ONLY:
        sendMessageImChange( "im_change_this_text_area_only\n" );
        break;
    default:
        break;
    }
}

void UimImSwitcher::sendMessageImChange( const TQString &change_type )
{
    TQString imName = selectedImName();
    if ( imName.isEmpty() )
        return ;

    /* ensuring connected to message bus */
    checkHelperConnection();

    TQString msg = TQString::null;
    msg.append( change_type );
    msg.append( imName );
    msg.append( "\n" );

    uim_helper_send_message( uim_fd, ( const char* ) msg.utf8() );
}

void UimImSwitcher::saveDefaultIm()
{
    if ( customEnabled )
    {
        TQString imName = selectedImName();
        if ( imName.isEmpty() )
            return ;

        uim_scm_callf( "custom-set-value!",
                       "yy",
                       "custom-preserved-default-im-name",
                       ( const char* ) imName.utf8() );
        uim_custom_save_custom( "custom-preserved-default-im-name" );
    }
}

TQString UimImSwitcher::selectedImName() const
{
    TQListViewItem * selectedItem = listview->selectedItem();
    if ( selectedItem )
    {
        return selectedItem->text( NAME_COLUMN );
    }

    return TQString::null;
}

void UimImSwitcher::slotStdinActivated( int /*socket*/ )
{
    uim_helper_read_proc( uim_fd );

    TQString msg = TQString::null;
    char *s;
    while ( ( s = uim_helper_get_message() ) )
    {
        const TQStringList lines = TQStringList::split( "\n", TQString( s ) );
        if ( !lines.isEmpty() && !lines[ 1 ].isEmpty() && lines[ 1 ].startsWith( "charset" ) )
        {
            /* get charset */
            const TQString charset = TQStringList::split( "=", lines[ 1 ] ) [ 1 ];

            /* convert to unicode */
            TQTextCodec *codec = TQTextCodec::codecForName( charset.local8Bit() );
            msg = codec->toUnicode( s );
        }
        else
        {
            /* no charset */
            msg = s;
        }

        if ( msg.startsWith( "focus_in" ) )
            reloadImList();
        else if ( msg.startsWith( "im_list" ) )
            parseHelperStrImList( msg );
        else if ( msg.startsWith( "im_switcher_start" ) )
            uim_helper_send_message( uim_fd,  "im_switcher_quit\n" );
        else if ( msg.startsWith( "im_switcher_quit" ) )
            tqApp->quit();
    }
}


void UimImSwitcher::parseHelperStrImList( const TQString &message )
{
    /* delete old items */
    listview->clear();

    const TQStringList lines = TQStringList::split( "\n", message );
    for ( unsigned int i = 2; i < lines.count(); i++ )
    {
        const TQStringList iminfoList = TQStringList::split( "\t", lines[ i ], true );

        if ( !iminfoList.isEmpty()
                && !iminfoList[ 0 ].isEmpty()
		// Language of IM with any locale is set as "".
                // && !iminfoList[ 1 ].isEmpty()
                && !iminfoList[ 2 ].isEmpty() )
        {
	    TQString lang, short_desc;

	    if (iminfoList[1].isEmpty())
		lang = TQString("-");
	    else
		lang = TQString::fromUtf8(gettext(iminfoList[1].utf8()));
	    short_desc = TQString::fromUtf8(gettext(iminfoList[2].utf8()));

            // add new item to listview
            TQListViewItem * item = new TQListViewItem( listview, iminfoList[ 0 ], lang, short_desc );

            if ( !iminfoList[ 3 ].isEmpty() && TQString::compare( iminfoList[ 3 ], "selected" ) == 0 )
                listview->setSelected( item, true );
        }
    }
}

void UimImSwitcher::reloadImList()
{
    checkHelperConnection();

    /* send request to get im list */
    uim_helper_send_message( uim_fd, "im_list_get\n" );
}

#include "qt.moc"
