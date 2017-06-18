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

#include "qt4.h"

#include <QtCore/QEvent>
#include <QtCore/QSocketNotifier>
#include <QtCore/QTextCodec>
#if QT_VERSION < 0x050000
# include <QtGui/QApplication>
# include <QtGui/QGroupBox>
# include <QtGui/QHBoxLayout>
# include <QtGui/QHeaderView>
# include <QtGui/QLayout>
# include <QtGui/QPushButton>
# include <QtGui/QRadioButton>
# include <QtGui/QSizePolicy>
# include <QtGui/QTableWidget>
# include <QtGui/QVBoxLayout>
#else
# include <QtWidgets/QApplication>
# include <QtWidgets/QGroupBox>
# include <QtWidgets/QHBoxLayout>
# include <QtWidgets/QHeaderView>
# include <QtWidgets/QPushButton>
# include <QtWidgets/QRadioButton>
# include <QtWidgets/QTableWidget>
# include <QtWidgets/QVBoxLayout>
#endif

#include <cstdlib>
#include <clocale>

#include <uim/uim-scm.h>
#include <uim/uim-custom.h>
#include "qtgettext.h"

static const int NAME_COLUMN = 0;

static int uim_fd;
static bool customEnabled;
static QSocketNotifier *notifier = 0;

int main( int argc, char **argv )
{
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    bind_textdomain_codeset(PACKAGE, "UTF-8"); // ensure code encoding is UTF8-

    setenv("XMODIFIERS", "@im=none", 1);
    
    QApplication a( argc, argv );

    UimImSwitcher switcher;
    switcher.setWindowIcon( QIcon( UIM_PIXMAPSDIR "/uim-icon.png" ) );
    switcher.resize( 550, 400 );
    switcher.setWindowTitle( _( "uim input method switcher" ) );
    switcher.show();

    return a.exec();
}


UimImSwitcher::UimImSwitcher( QWidget *parent )
        : QDialog( parent )
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
    listview = new QTableWidget( this );
    listview->setSelectionMode( QAbstractItemView::SingleSelection );
    listview->setSelectionBehavior( QAbstractItemView::SelectRows );
    listview->setColumnCount( 3 );
    listview->verticalHeader()->setVisible( false );
    listview->setHorizontalHeaderLabels( QStringList()
        << _( "InputMethodName" ) << _( "Language" ) << _( "Description" ) );
    listview->setShowGrid( false );
    listview->horizontalHeader()->setStretchLastSection( true );
    listview->setAttribute( Qt::WA_InputMethodEnabled, false );

    /* radio buttons for the switcher coverage */
    wholeButton = new QRadioButton( _( "whole desktop" ) );
    applicationButton = new QRadioButton( _( "current application only" ) );
    textButton = new QRadioButton( _( "current text area only" ) );
    wholeButton->setChecked( true ); // default is "whole desktop"

    QVBoxLayout *vbox = new QVBoxLayout;
    vbox->addWidget( wholeButton );
    vbox->addWidget( applicationButton );
    vbox->addWidget( textButton );

    QGroupBox *groupBox = new QGroupBox(  _( "Effective coverage" ) );
    groupBox->setLayout( vbox );

    /* cancel, apply & ok button */
    QPushButton *okButton = new QPushButton( this );
    okButton->setText( _( "OK" ) );
    connect( okButton, SIGNAL( clicked() ),
                      this, SLOT( slotChangeInputMethodAndQuit() ) );

    QPushButton *applyButton = new QPushButton( this );
    applyButton->setText( _( "Apply" ) );
    connect( applyButton, SIGNAL( clicked() ),
                      this, SLOT( slotChangeInputMethod() ) );

    QPushButton *cancelButton = new QPushButton( this );
    cancelButton->setText( _( "Cancel" ) );
    connect( cancelButton, SIGNAL( clicked() ),
                      QApplication::instance(), SLOT( quit() ) );
    QHBoxLayout *buttonLayout = new QHBoxLayout;
    buttonLayout->addStretch( 0 );
    buttonLayout->addWidget( okButton );
    buttonLayout->addWidget( applyButton );
    buttonLayout->addWidget( cancelButton );

    // main layout
    QVBoxLayout *mainLayout = new QVBoxLayout( this );
    mainLayout->setMargin( 6 );
    mainLayout->setSpacing( 6 );
    mainLayout->addWidget( listview );
    mainLayout->addWidget( groupBox );
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
            connect( notifier, SIGNAL( activated( int ) ),
                              this, SLOT( slotStdinActivated() ) );
        }
    }
}

void UimImSwitcher::helper_disconnect_cb()
{
    uim_fd = -1;
    disconnect( notifier, SIGNAL( activated( int ) ), 0, 0 );
}

void UimImSwitcher::slotChangeInputMethodAndQuit()
{
    slotChangeInputMethod();
    QApplication::instance()->quit();
}

void UimImSwitcher::slotChangeInputMethod()
{
    if ( wholeButton->isChecked() )
    {
        sendMessageImChange( "im_change_whole_desktop\n" );
        saveDefaultIm();
    }
    else if ( applicationButton->isChecked() )
        sendMessageImChange( "im_change_this_application_only\n" );
    else if ( textButton->isChecked() )
        sendMessageImChange( "im_change_this_text_area_only\n" );
}

void UimImSwitcher::sendMessageImChange( const QString &change_type )
{
    QString imName = selectedImName();
    if ( imName.isEmpty() )
        return ;

    /* ensuring connected to message bus */
    checkHelperConnection();

    QString msg;
    msg.append( change_type );
    msg.append( imName );
    msg.append( "\n" );

    uim_helper_send_message( uim_fd, msg.toUtf8().data() );
}

void UimImSwitcher::saveDefaultIm()
{
    if ( customEnabled )
    {
        QString imName = selectedImName();
        if ( imName.isEmpty() )
            return ;

        uim_scm_callf( "custom-set-value!",
                       "yy",
                       "custom-preserved-default-im-name",
                       imName.toUtf8().data() );
        uim_custom_save_custom( "custom-preserved-default-im-name" );
    }
}
 
QString UimImSwitcher::selectedImName() const
{
    QList<QTableWidgetItem *> selectedItems = listview->selectedItems();
    if ( !selectedItems.isEmpty() )
    {
        return listview->item( selectedItems[ 0 ]->row(), NAME_COLUMN )->text();
    }

    return QString();
}

void UimImSwitcher::slotStdinActivated()
{
    uim_helper_read_proc( uim_fd );

    QString msg;
    char *s;
    while ( ( s = uim_helper_get_message() ) )
    {
        const QStringList lines = QString( s ).split( '\n',
            QString::SkipEmptyParts );
        if ( !lines.isEmpty() && lines.count() > 1
            && lines[ 1 ].startsWith( QLatin1String( "charset" ) ) )
        {
            /* get charset */
            const QString charset
                = lines[ 1 ].split( '=', QString::SkipEmptyParts ) [ 1 ];

            /* convert to unicode */
            QTextCodec *codec
                = QTextCodec::codecForName( QByteArray( charset.toLatin1() ) );
            msg = codec->toUnicode( s );
        }
        else
        {
            /* no charset */
            msg = s;
        }

        if ( msg.startsWith( QLatin1String( "focus_in" ) ) )
            reloadImList();
        else if ( msg.startsWith( QLatin1String( "im_list" ) ) )
            parseHelperStrImList( msg );
        else if ( msg.startsWith( QLatin1String( "im_switcher_start" ) ) )
            uim_helper_send_message( uim_fd,  "im_switcher_quit\n" );
        else if ( msg.startsWith( QLatin1String( "im_switcher_quit" ) ) )
            QApplication::instance()->quit();
    }
}


void UimImSwitcher::parseHelperStrImList( const QString &message )
{
    /* delete old items */
    listview->setRowCount( 0 );
    listview->clearContents();

    const QStringList lines = message.split( '\n', QString::SkipEmptyParts );
    for ( int i = 2; i < lines.count(); i++ )
    {
        const QStringList iminfoList = lines[ i ].split( '\t' );

        if ( !iminfoList.isEmpty()
                && !iminfoList[ 0 ].isEmpty()
                // Language of IM with any locale is set as "".
                // && !iminfoList[ 1 ].isEmpty()
                && !iminfoList[ 2 ].isEmpty() )
        {
            QString lang, short_desc;

            if (iminfoList[1].isEmpty())
                lang = QString("-");
            else
                lang = mygettext(iminfoList[1].toUtf8().data());
            short_desc = mygettext(iminfoList[2].toUtf8().data());

            // add new item to listview
            int row = listview->rowCount();
            listview->setRowCount( row + 1 );
            QTableWidgetItem *imItem = new QTableWidgetItem( iminfoList[ 0 ] );
            imItem->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );
            listview->setItem( row, 0, imItem );

            QTableWidgetItem *langItem = new QTableWidgetItem( lang );
            langItem->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );
            listview->setItem( row, 1, langItem );

            QTableWidgetItem *descItem = new QTableWidgetItem( short_desc );
            descItem->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );
            listview->setItem( row, 2, descItem );

            if ( iminfoList.count() > 3
                    && iminfoList[ 3 ] == QLatin1String( "selected" ) )
                for ( int j = 0; j < listview->columnCount(); j++ )
                    listview->item( row, j )->setSelected( true );

            listview->setRowHeight(
                row, QFontMetrics( listview->font() ).height() + 2 );
        }
    }
    listview->sortItems( 0 );
}

void UimImSwitcher::reloadImList()
{
    checkHelperConnection();

    /* send request to get im list */
    uim_helper_send_message( uim_fd, "im_list_get\n" );
}
