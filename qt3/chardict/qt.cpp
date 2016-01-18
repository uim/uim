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
#include "kseparator.h"

#include <qapplication.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qhbox.h>
#include <qframe.h>
#include <qsizepolicy.h>
#include <qtoolbutton.h>
#include <qfont.h>
#include <qfontdialog.h>
#include <qvbox.h>
#include <qsettings.h>

#include <locale.h>

#include "qtgettext.h"

int main( int argc, char *argv[] )
{

    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    bind_textdomain_codeset(PACKAGE, "UTF-8");

    QApplication a( argc, argv );

    KUimCharDict::Mode m = KUimCharDict::UNKNOWN;
    for ( int i = 0; i < argc; i++ )
    {
        QString arg( argv[ i ] );
        if ( !arg.isEmpty() && arg.startsWith( "-mode=" ) )
        {
            QString mode = QStringList::split( "=", arg ) [ 1 ];
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
    cdict.show();

    a.setMainWidget( &cdict );

    return a.exec();
}

KUimCharDict::KUimCharDict( QWidget *parent, const char *name )
        : QWidget( parent, name )
{
    setupWidgets();

    readConfig();
}

KUimCharDict::~KUimCharDict()
{
    writeConfig();
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
    m_modeCombo->insertItem( _( "Bushu Search" ) );
    m_modeCombo->insertItem( _( "Unicode Search" ) );
    QObject::connect( m_modeCombo, SIGNAL( activated( int ) ),
                      this, SLOT( changeMode( int ) ) );
    modeLabel->setBuddy( m_modeCombo );

    m_fontselButton = new QPushButton( upperHWidget );
    m_fontselButton->setText( _( "Select Font" ) );
    QObject::connect( m_fontselButton, SIGNAL( clicked() ),
                      this, SLOT( slotSelectFont() ) );

    QLabel *charLabel = new QLabel( _( "Chars:" ), upperHWidget );
    m_charLineEdit = new QLineEdit( upperHWidget );
    charLabel->setBuddy( m_charLineEdit );

    QHBoxLayout *upperHLayout = new QHBoxLayout( upperHWidget );
    upperHLayout->setSpacing( 4 );
    upperHLayout->addWidget( modeLabel );
    upperHLayout->addWidget( m_modeCombo );
    upperHLayout->addWidget( m_fontselButton );
    upperHLayout->addSpacing( 11 );
    upperHLayout->addWidget( charLabel );
    upperHLayout->addWidget( m_charLineEdit );

    m_widgetStack = new QWidgetStack( this );
    m_widgetStack->setSizePolicy( QSizePolicy::Minimum, QSizePolicy::Minimum );

    m_bushuView = new BushuViewWidget( this );
    QObject::connect( m_bushuView, SIGNAL( charSelected( const QString & ) ),
                      this, SLOT( slotCharSelected( const QString & ) ) );
    m_bushuView->hide();
    m_widgetStack->addWidget( m_bushuView, BUSHU );

    m_unicodeView = new UnicodeViewWidget( this );
    QObject::connect( m_unicodeView, SIGNAL( charSelected( const QString & ) ),
                      this, SLOT( slotCharSelected( const QString & ) ) );
    m_unicodeView->hide();
    m_widgetStack->addWidget( m_unicodeView, UNICODE );

    layout->addWidget( upperHWidget );
    layout->addWidget( new KSeparator( this ) );
    layout->addWidget( m_widgetStack );
}

void KUimCharDict::writeConfig()
{
    QSettings settings;

    // font
    settings.writeEntry( "/uim-kdehelper/chardict/font", m_fontselButton->font().toString() );
}
void KUimCharDict::readConfig()
{
    QSettings settings;
    QString str;

    // font
    str = settings.readEntry( "/uim-kdehelper/chardict/font" );
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
        qDebug( "Unknown Mode" );
        break;
    }
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
}

#include "qt.moc"
