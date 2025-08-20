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
#include "bushuviewwidget.h"
#include "chargridview.h"

#include <tqlayout.h>
#include <tqvbox.h>
#include <tqhbox.h>
#include <tqlabel.h>
#include <tqfile.h>
#include <tqtextstream.h>
#include <tqsettings.h>
#include <tqheader.h>
#include <tqpainter.h>
#include <tqdrawutil.h>
#include <tqtextcodec.h>

#include "qtgettext.h"

#define BUSHUDICT_ENCODING	"EUC-JP"

BushuViewWidget::BushuViewWidget( TQWidget *parent, const char *name )
        : CharDictViewBase( parent, name )
{
    setupWidgets();
    readDict();

    readConfig();
}

BushuViewWidget::~BushuViewWidget()
{
    writeConfig();
}

void BushuViewWidget::setupWidgets()
{
    m_mainSplitter = new TQSplitter( this );

    TQVBox *leftVBox = new TQVBox( m_mainSplitter );
    TQLabel *bushuLabel = new TQLabel( leftVBox );
    bushuLabel->setText( _( "Bushu List" ) );
    bushuLabel->setAlignment( TQt::AlignHCenter );
    m_bushuListView = new TQListView( leftVBox );
    m_bushuListView->setSorting( -1 );
    m_bushuListView->setSelectionMode( TQListView::Single );
    m_bushuListView->addColumn( "0" );
    m_bushuListView->header() ->setStretchEnabled( true, 0 );
    m_bushuListView->header() ->hide();
    m_bushuListView->setColumnWidthMode( 1, TQListView::Maximum );
    m_bushuListView->setHScrollBarMode( TQScrollView::AlwaysOff );
    m_bushuListView->setAllColumnsShowFocus( true );
    TQObject::connect( m_bushuListView, TQ_SIGNAL( selectionChanged( TQListViewItem * ) ),
                      this, TQ_SLOT( slotBushuSelected( TQListViewItem * ) ) );

    m_charGridView = new CharGridView( 10, 0, m_mainSplitter );
    TQObject::connect( m_charGridView, TQ_SIGNAL( charSelected( const TQString & ) ),
                      this, TQ_SIGNAL( charSelected( const TQString & ) ) );

    // main layout
    TQHBoxLayout* layout = new TQHBoxLayout( this );
    layout->addWidget( m_mainSplitter );
}

void BushuViewWidget::readDict()
{
    TQFile file( BUSHUDICT );
    if ( file.open( IO_ReadOnly ) )
    {
        TQTextStream stream( &file );
        stream.setCodec(TQTextCodec::codecForName(BUSHUDICT_ENCODING));
        TQString line;
        while ( !stream.atEnd() )
        {
            TQString bushuName = TQStringList::split( " ", stream.readLine()  ) [ 0 ];

            // insert last
            TQListViewItem *lastItem = m_bushuListView->lastItem();
            if( lastItem )
            {
                new TQListViewItem( m_bushuListView, lastItem, bushuName );
            }
            else
            {
                new TQListViewItem( m_bushuListView, bushuName );
            }

        }
        file.close();
    }
}

void BushuViewWidget::slotBushuSelected( TQListViewItem *item )
{
    if ( !item )
        return ;

    TQString selectedBushuName = item->text( 0 );
    if ( selectedBushuName.isEmpty() )
        return ;

    TQFile file( BUSHUDICT );
    if ( file.open( IO_ReadOnly ) )
    {
        TQTextStream stream( &file );
        stream.setCodec(TQTextCodec::codecForName(BUSHUDICT_ENCODING));
        TQString line;

        // search selected bushu line by line
        while ( !stream.atEnd() )
        {
            TQStringList chars = TQStringList::split( " ",  stream.readLine()  );
            TQString bushuName = chars[ 0 ];
            if ( selectedBushuName == bushuName )
            {
                // Display Characters
                chars.remove( bushuName );
                m_charGridView->setCharacters( chars );
            }
        }
        file.close();
    }
}

void BushuViewWidget::writeConfig()
{
    TQSettings settings;

    // splitter
    TQString str;
    TQTextOStream out( &str );
    out << *m_mainSplitter;
    settings.writeEntry( "/uim-kdehelper/chardict/bushuview/splitter", str );
}

void BushuViewWidget::readConfig()
{
    TQSettings settings;
    TQString str;

    // splitter
    str = settings.readEntry( "/uim-kdehelper/chardict/bushuview/splitter" );
    if ( !str.isEmpty() )
    {
        TQTextIStream in( &str );
        in >> *m_mainSplitter;
    }
}

void BushuViewWidget::setFont( const TQFont &font )
{
    m_charGridView->setFont( font );
}

#include "bushuviewwidget.moc"
