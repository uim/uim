/*

 Copyright (c) 2003,2004,2005 uim Project http://uim.freedesktop.org/

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
#include "switcher-qt.h"

#include <qapplication.h>
#include <qlistview.h>
#include <qvbuttongroup.h>
#include <qradiobutton.h>
#include <qsocketnotifier.h>
#include <qtoolbutton.h>
#include <qtextcodec.h>
#include <qevent.h>
#include <qlayout.h>
#include <qsizepolicy.h>

#define i18n(string) (string)

#define NAME_COLUMN 0

static int uim_fd;
static QSocketNotifier *notifier = NULL;

int main( int argc, char **argv )
{
    QApplication a( argc, argv );

    UimImSwitcher switcher;
    switcher.resize( 550, 400 );
    switcher.setCaption( i18n( "uim input method switcher" ) );
    a.setMainWidget( &switcher );
    switcher.show();

    return a.exec();
}


UimImSwitcher::UimImSwitcher( QWidget *parent, const char *name )
        : QDialog( parent, name )
{
    /* connect to uim helper message bus */
    uim_fd = -1;
    checkHelperConnection();

    /* to load input method list */
    uim_helper_send_message( uim_fd, "im_list_get\n" );

    /* create GUI */
    createGUI();
}

UimImSwitcher::~UimImSwitcher()
{
}

void UimImSwitcher::createGUI()
{
    /* im list view */
    listview = new QListView( this );
    listview->setSelectionMode( QListView::Single );
    listview->addColumn( i18n( "InputMethodName" ) );
    listview->addColumn( i18n( "Language" ) );
    listview->addColumn( i18n( "Description" ) );

    /* radio buttons for changing way*/
    QRadioButton *button;
    vbGroup = new QVButtonGroup( i18n( "Changing way" ), this );
    button = new QRadioButton( i18n( "Change whole desktop" ), vbGroup );
    vbGroup->insert( button, ID_CHANGE_WHOLE_DESKTOP );
    button->setChecked( TRUE ); // default is "Change whole desktop"
    button = new QRadioButton( i18n( "Change this application only" ), vbGroup );
    vbGroup->insert( button, ID_CHANGE_THIS_APPLICATION_ONLY );
    button = new QRadioButton( i18n( "Change this text area only" ), vbGroup );
    vbGroup->insert( button, ID_CHANGE_THIS_TEXT_AREA_ONLY );

    /* cancel & ok button */
    okButton = new QToolButton( this );
    okButton->setText( i18n( "OK" ) );
    okButton->setSizePolicy( QSizePolicy::Expanding, QSizePolicy::Fixed );
    QObject::connect( okButton, SIGNAL( clicked() ),
                      this, SLOT( slotChangeInputMethod() ) );
    cancelButton = new QToolButton( this );
    cancelButton->setText( i18n( "Cancel" ) );
    cancelButton->setSizePolicy( QSizePolicy::Expanding, QSizePolicy::Fixed );
    QObject::connect( cancelButton, SIGNAL( clicked() ),
                      qApp, SLOT( quit() ) );
    QHBoxLayout *buttonLayout = new QHBoxLayout;
    buttonLayout->addStretch( 0 );
    buttonLayout->addWidget( okButton );
    buttonLayout->addWidget( cancelButton );

    // main layout
    QVBoxLayout *mainLayout = new QVBoxLayout( this );
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
            notifier = new QSocketNotifier( uim_fd, QSocketNotifier::Read );
            QObject::connect( notifier, SIGNAL( activated( int ) ),
                              this, SLOT( slotStdinActivated( int ) ) );
        }
    }
}

void UimImSwitcher::helper_disconnect_cb()
{
    uim_fd = -1;
    QObject::disconnect( notifier, SIGNAL( activated( int ) ), 0, 0 );
}

void UimImSwitcher::slotChangeInputMethod()
{

    switch ( vbGroup->selectedId() )
    {
    case ID_CHANGE_WHOLE_DESKTOP:
        sendMessageImChange( "im_change_whole_desktop\n" );
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

    qApp->quit();
}

void UimImSwitcher::sendMessageImChange( const QString &change_type )
{
    QString imName = selectedImName();
    if ( imName.isEmpty() )
        return ;

    /* ensuring connected to message bus */
    checkHelperConnection();

    QString msg = QString::null;
    msg.append( change_type );
    msg.append( imName );
    msg.append( "\n" );

    uim_helper_send_message( uim_fd, ( const char* ) msg.utf8() );
}

QString UimImSwitcher::selectedImName() const
{
    QListViewItem * selectedItem = listview->selectedItem();
    if ( selectedItem )
    {
        return selectedItem->text( NAME_COLUMN );
    }

    return QString::null;
}

void UimImSwitcher::slotStdinActivated( int /*socket*/ )
{
    uim_helper_read_proc( uim_fd );

    QString msg = QString::null;
    char *s;
    while ( ( s = uim_helper_get_message() ) )
    {
        const QStringList lines = QStringList::split( "\n", QString( s ) );
        if ( !lines.isEmpty() && !lines[ 1 ].isEmpty() && lines[ 1 ].startsWith( "charset" ) )
        {
            /* get charset */
            const QString charset = QStringList::split( "=", lines[ 1 ] ) [ 1 ];

            /* convert to unicode */
            QTextCodec *codec = QTextCodec::codecForName( charset );
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
    }
}


void UimImSwitcher::parseHelperStrImList( const QString &message )
{
    /* delete old items */
    listview->clear();

    const QStringList lines = QStringList::split( "\n", message );
    for ( unsigned int i = 2; i < lines.count(); i++ )
    {
        const QStringList iminfoList = QStringList::split( "\t", lines[ i ] );

        if ( !iminfoList.isEmpty()
                && !iminfoList[ 0 ].isEmpty()
                && !iminfoList[ 1 ].isEmpty()
                && !iminfoList[ 2 ].isEmpty() )
        {
            // add new item to listview
            QListViewItem * item = new QListViewItem( listview, iminfoList[ 0 ], iminfoList[ 1 ], iminfoList[ 2 ] );

            if ( !iminfoList[ 3 ].isEmpty() && QString::compare( iminfoList[ 3 ], "selected" ) == 0 )
                listview->setSelected( item, TRUE );
        }
    }
}

void UimImSwitcher::reloadImList()
{
    checkHelperConnection();

    /* send request to get im list */
    uim_helper_send_message( uim_fd, "im_list_get\n" );
}

#include "switcher-qt.moc"
