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
#include "qt.h"
#include "bushuviewwidget.h"
#include "unicodeviewwidget.h"

#include <tqapplication.h>
#include <tqlayout.h>
#include <tqlabel.h>
#include <tqhbox.h>
#include <tqframe.h>
#include <tqsizepolicy.h>
#include <tqtoolbutton.h>
#include <tqfont.h>
#include <tqfontdialog.h>
#include <tqvbox.h>
#include <tqsettings.h>

#ifdef TQ_WS_X11
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

    TQApplication a( argc, argv );

    KUimCharDict::Mode m = KUimCharDict::UNKNOWN;
    for ( int i = 0; i < argc; i++ )
    {
        TQString arg( argv[ i ] );
        if ( !arg.isEmpty() && arg.startsWith( "-mode=" ) )
        {
            TQString mode = TQStringList::split( "=", arg ) [ 1 ];
            if ( mode.isEmpty() )
                continue;

            if ( mode == "BUSHU" )
                m = KUimCharDict::BUSHU;
            else if ( mode == "UNICODE" )
                m = KUimCharDict::UNICODE;
        }
    }

    KUimCharDict cdict;
    cdict.changeMode( m );
    cdict.resize( 600, 400 );
    cdict.setIcon( TQPixmap::fromMimeSource( UIM_PIXMAPSDIR "/uim-icon.png" ) );
    cdict.show();

    a.setMainWidget( &cdict );

    return a.exec();
}

KUimCharDict::KUimCharDict( TQWidget *parent, const char *name )
        : TQWidget( parent, name )
{
#ifdef TQ_WS_X11
    // Don't give input focus to this window.
    XWMHints *wmhints = XGetWMHints( x11Display(), winId() );
    if ( !wmhints )
        wmhints = XAllocWMHints();
    wmhints->flags = InputHint;
    wmhints->input = False;
    XSetWMHints( x11Display(), winId(), wmhints );
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
    TQVBoxLayout * layout = new TQVBoxLayout( this );
    layout->setMargin( 4 );
    layout->setSpacing( 6 );

    TQWidget *upperHWidget = new TQWidget( this );
    upperHWidget->setSizePolicy( TQSizePolicy::Minimum, TQSizePolicy::Maximum );

    TQLabel *modeLabel = new TQLabel( _( "Mode:" ), upperHWidget );
    m_modeCombo = new TQComboBox( upperHWidget );
    m_modeCombo->insertItem( _( "Bushu Search" ) );
    m_modeCombo->insertItem( _( "Unicode Search" ) );
    TQObject::connect( m_modeCombo, TQ_SIGNAL( activated( int ) ),
                      this, TQ_SLOT( changeMode( int ) ) );
    modeLabel->setBuddy( m_modeCombo );

    m_fontselButton = new TQPushButton( upperHWidget );
    m_fontselButton->setText( _( "Select Font" ) );
    TQObject::connect( m_fontselButton, TQ_SIGNAL( clicked() ),
                      this, TQ_SLOT( slotSelectFont() ) );

    TQLabel *charLabel = new TQLabel( _( "Chars:" ), upperHWidget );
    m_charLineEdit = new TQLineEdit( upperHWidget );
    charLabel->setBuddy( m_charLineEdit );

    TQPushButton *clearButton = new TQPushButton( _( "Clear" ), upperHWidget );
    TQObject::connect( clearButton, TQ_SIGNAL( clicked() ),
        m_charLineEdit, TQ_SLOT( clear() ) );

    TQHBoxLayout *upperHLayout = new TQHBoxLayout( upperHWidget );
    upperHLayout->setSpacing( 4 );
    upperHLayout->addWidget( modeLabel );
    upperHLayout->addWidget( m_modeCombo );
    upperHLayout->addWidget( m_fontselButton );
    upperHLayout->addSpacing( 11 );
    upperHLayout->addWidget( charLabel );
    upperHLayout->addWidget( m_charLineEdit );
    upperHLayout->addWidget( clearButton );

    m_widgetStack = new TQWidgetStack( this );
    m_widgetStack->setSizePolicy( TQSizePolicy::Minimum, TQSizePolicy::Minimum );

    m_bushuView = new BushuViewWidget( this );
    TQObject::connect( m_bushuView, TQ_SIGNAL( charSelected( const TQString & ) ),
                      this, TQ_SLOT( slotCharSelected( const TQString & ) ) );
    m_bushuView->hide();
    m_widgetStack->addWidget( m_bushuView, BUSHU );

    m_unicodeView = new UnicodeViewWidget( this );
    TQObject::connect( m_unicodeView, TQ_SIGNAL( charSelected( const TQString & ) ),
                      this, TQ_SLOT( slotCharSelected( const TQString & ) ) );
    m_unicodeView->hide();
    m_widgetStack->addWidget( m_unicodeView, UNICODE );

    TQFrame *separator = new TQFrame( this );
    separator->setFrameShape( TQFrame::HLine );
    separator->setFrameShadow( TQFrame::Sunken );

    layout->addWidget( upperHWidget );
    layout->addWidget( separator );
    layout->addWidget( m_widgetStack );
}

void KUimCharDict::writeConfig()
{
    TQSettings settings;

    // font
    settings.writeEntry( "/uim-kdehelper/chardict/font", m_fontselButton->font().toString() );
}
void KUimCharDict::readConfig()
{
    TQSettings settings;
    TQString str;

    // font
    str = settings.readEntry( "/uim-kdehelper/chardict/font" );
    if ( !str.isEmpty() )
    {
        TQFont font;
        font.fromString( str );

        setCharDictFont( font );
    } else {
        setCharDictFont( font() );
    }
}

void KUimCharDict::setCharDictFont( const TQFont &font )
{
    // button
    m_fontselButton->setFont( font );
    // bushu
    m_bushuView->setFont( font );
    // unicode
    m_unicodeView->setFont( font );
}


void KUimCharDict::changeMode( int mode )
{
    // not implemented yet
    switch ( mode )
    {
    case BUSHU:
        m_widgetStack->raiseWidget( BUSHU );
        break;
    case UNICODE:
        m_widgetStack->raiseWidget( UNICODE );
        break;
    default:
        tqDebug( "Unknown Mode" );
        break;
    }
}

void KUimCharDict::slotSelectFont()
{
    bool ok;
    TQFont font = TQFontDialog::getFont( &ok, m_fontselButton->font(), this );
    if ( ok )
    {
        // font is set to the font the user selected
        setCharDictFont( font );
    }
}

void KUimCharDict::slotCharSelected( const TQString &c )
{
    m_charLineEdit->setText( m_charLineEdit->text() + c );
    uim_helper_send_message( uim_fd,
        ( "commit_string\n" + c + '\n' ).utf8().data() );
}

#include "qt.moc"
