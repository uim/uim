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
#include "qt4.h"
#include "bushuviewwidget.h"
#include "unicodeviewwidget.h"

#include <QtCore/QSettings>
#include <QtGui/QFont>
#if QT_VERSION < 0x050000
# include <QtGui/QApplication>
# include <QtGui/QComboBox>
# include <QtGui/QFontDialog>
# include <QtGui/QHBoxLayout>
# include <QtGui/QLabel>
# include <QtGui/QLineEdit>
# include <QtGui/QPushButton>
# include <QtGui/QSizePolicy>
# include <QtGui/QVBoxLayout>
# include <QtGui/QStackedWidget>
#else
# include <QtWidgets/QApplication>
# include <QtWidgets/QComboBox>
# include <QtWidgets/QFontDialog>
# include <QtWidgets/QHBoxLayout>
# include <QtWidgets/QLabel>
# include <QtWidgets/QLineEdit>
# include <QtWidgets/QPushButton>
# include <QtWidgets/QSizePolicy>
# include <QtWidgets/QVBoxLayout>
# include <QtWidgets/QStackedWidget>

#endif

#ifdef Q_WS_X11
#include <QtGui/QX11Info>

#include <X11/Xutil.h>
#endif

#include <clocale>

#include <uim/uim.h>
#include <uim/uim-helper.h>

#include "qtgettext.h"

static int uim_fd = -1;

int main( int argc, char *argv[] )
{

    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    bind_textdomain_codeset(PACKAGE, "UTF-8");

    QApplication a( argc, argv );

    QCoreApplication::setOrganizationName( "uim" );
    QCoreApplication::setApplicationName( "uim" );

    KUimCharDict::Mode m = KUimCharDict::UNKNOWN;
    for ( int i = 0; i < argc; i++ )
    {
        QString arg( argv[ i ] );
        if ( !arg.isEmpty() && arg.startsWith( QLatin1String( "-mode=" ) ) )
        {
            QStringList list = arg.split( '=', QString::SkipEmptyParts );
            if ( list.count() < 2 || list[ 1 ].isEmpty() )
                continue;
            QString mode = list[ 1 ];

            if ( mode == "BUSHU" )
                m = KUimCharDict::BUSHU;
            else if ( mode == "UNICODE" )
                m = KUimCharDict::UNICODE;
        }
    }

    KUimCharDict cdict;
    cdict.changeMode( m );
    cdict.setWindowIcon( QIcon( UIM_PIXMAPSDIR "/uim-icon.png" ) );
    cdict.resize( 600, 400 );
    cdict.show();

    return a.exec();
}

KUimCharDict::KUimCharDict( QWidget *parent )
        : QWidget( parent )
{
#ifdef Q_WS_X11
    // Don't give input focus to this window.
    XWMHints *wmhints = XGetWMHints( QX11Info::display(), winId() );
    if ( !wmhints )
        wmhints = XAllocWMHints();
    wmhints->flags = InputHint;
    wmhints->input = False;
    XSetWMHints( QX11Info::display(), winId(), wmhints );
    XFree( wmhints );
#endif

    setupWidgets();

    readConfig();

    uim_fd = uim_helper_init_client_fd( 0 );
}

KUimCharDict::~KUimCharDict()
{
    writeConfig();

    uim_helper_close_client_fd( uim_fd );
}

void KUimCharDict::setupWidgets()
{
    QVBoxLayout * layout = new QVBoxLayout( this );
    layout->setMargin( 4 );
    layout->setSpacing( 6 );

    QWidget *upperHWidget = new QWidget( this );
    upperHWidget->setSizePolicy( QSizePolicy::Minimum, QSizePolicy::Maximum );

    QLabel *modeLabel = new QLabel( _( "Mode:" ), upperHWidget );
    m_modeCombo = new QComboBox( upperHWidget );
    m_modeCombo->addItem( _( "Bushu Search" ) );
    m_modeCombo->addItem( _( "Unicode Search" ) );
    modeLabel->setBuddy( m_modeCombo );

    m_fontselButton = new QPushButton( upperHWidget );
    m_fontselButton->setText( _( "Select Font" ) );
    connect( m_fontselButton, SIGNAL( clicked() ),
                      this, SLOT( slotSelectFont() ) );

    QLabel *charLabel = new QLabel( _( "Chars:" ), upperHWidget );
    m_charLineEdit = new QLineEdit( upperHWidget );
    charLabel->setBuddy( m_charLineEdit );

    QPushButton *clearButton = new QPushButton( _( "Clear" ), upperHWidget );
    connect( clearButton, SIGNAL( clicked() ),
        m_charLineEdit, SLOT( clear() ) );

    QHBoxLayout *upperHLayout = new QHBoxLayout( upperHWidget );
    upperHLayout->setSpacing( 4 );
    upperHLayout->addWidget( modeLabel );
    upperHLayout->addWidget( m_modeCombo );
    upperHLayout->addWidget( m_fontselButton );
    upperHLayout->addSpacing( 11 );
    upperHLayout->addWidget( charLabel );
    upperHLayout->addWidget( m_charLineEdit );
    upperHLayout->addWidget( clearButton );

    m_widgetStack = new QStackedWidget( this );
    m_widgetStack->setSizePolicy( QSizePolicy::Minimum, QSizePolicy::Minimum );

    m_bushuView = new BushuViewWidget( this );
    connect( m_bushuView, SIGNAL( charSelected( const QString & ) ),
                      this, SLOT( slotCharSelected( const QString & ) ) );
    m_bushuView->hide();
    m_widgetStack->addWidget( m_bushuView );

    m_unicodeView = new UnicodeViewWidget( this );
    connect( m_unicodeView, SIGNAL( charSelected( const QString & ) ),
                      this, SLOT( slotCharSelected( const QString & ) ) );
    m_unicodeView->hide();
    m_widgetStack->addWidget( m_unicodeView );

    connect( m_modeCombo, SIGNAL( activated( int ) ),
                      m_widgetStack, SLOT( setCurrentIndex( int ) ) );

    QFrame *separator = new QFrame( this );
    separator->setFrameShape( QFrame::HLine );
    separator->setFrameShadow( QFrame::Sunken );
    layout->setMargin( 0 );
    layout->addWidget( upperHWidget );
    layout->addWidget( separator );
    layout->addWidget( m_widgetStack );
}

void KUimCharDict::writeConfig()
{
    QSettings settings;

    // font
    settings.setValue( "/uim-kdehelper/chardict/font", m_fontselButton->font().toString() );
}
void KUimCharDict::readConfig()
{
    QSettings settings;
    QString str;

    // font
    str = settings.value( "/uim-kdehelper/chardict/font" ).toString();
    if ( !str.isEmpty() )
    {
        QFont font;
        font.fromString( str );

        setCharDictFont( font );
    } else {
        setCharDictFont( font() );        
    }
}

void KUimCharDict::setCharDictFont( const QFont &font )
{
    // button
    m_fontselButton->setFont( font );
    // bushu
    m_bushuView->setCharFont( font );
    // unicode
    m_unicodeView->setCharFont( font );
}


void KUimCharDict::changeMode( int mode )
{
    if ( mode < 0 || mode > m_widgetStack->count() - 1 )
    {
        qDebug( "Unknown Mode" );
        return;
    }
    m_widgetStack->setCurrentIndex( mode );
    m_modeCombo->setCurrentIndex( mode );
}

void KUimCharDict::slotSelectFont()
{
    bool ok;
    QFont font = QFontDialog::getFont( &ok, m_fontselButton->font(), this );
    if ( ok )
    {
        // font is set to the font the user selected
        setCharDictFont( font );
    }
}

void KUimCharDict::slotCharSelected( const QString &c )
{
    m_charLineEdit->setText( m_charLineEdit->text() + c );
    uim_helper_send_message( uim_fd,
        ( "commit_string\n" + c + '\n' ).toUtf8().data() );
}
