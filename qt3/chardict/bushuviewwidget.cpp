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

#include <qlayout.h>
#include <qvbox.h>
#include <qhbox.h>
#include <qlabel.h>
#include <qfile.h>
#include <qtextstream.h>
#include <qsettings.h>
#include <qheader.h>
#include <qpainter.h>
#include <qdrawutil.h>
#include <qtextcodec.h>

#include "qtgettext.h"

#define BUSHUDICT_ENCODING	"EUC-JP"

BushuViewWidget::BushuViewWidget( QWidget *parent, const char *name )
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
    m_mainSplitter = new QSplitter( this );

    QVBox *leftVBox = new QVBox( m_mainSplitter );
    QLabel *bushuLabel = new QLabel( leftVBox );
    bushuLabel->setText( _( "Bushu List" ) );
    bushuLabel->setAlignment( Qt::AlignHCenter );
    m_bushuListView = new QListView( leftVBox );
    m_bushuListView->setSorting( -1 );
    m_bushuListView->setSelectionMode( QListView::Single );
    m_bushuListView->addColumn( "0" );
    m_bushuListView->header() ->setStretchEnabled( true, 0 );
    m_bushuListView->header() ->hide();
    m_bushuListView->setColumnWidthMode( 1, QListView::Maximum );
    m_bushuListView->setHScrollBarMode( QScrollView::AlwaysOff );
    m_bushuListView->setAllColumnsShowFocus( true );
    QObject::connect( m_bushuListView, SIGNAL( selectionChanged( QListViewItem * ) ),
                      this, SLOT( slotBushuSelected( QListViewItem * ) ) );

    m_charGridView = new CharGridView( 10, 0, m_mainSplitter );
    QObject::connect( m_charGridView, SIGNAL( charSelected( const QString & ) ),
                      this, SIGNAL( charSelected( const QString & ) ) );

    // main layout
    QHBoxLayout* layout = new QHBoxLayout( this );
    layout->addWidget( m_mainSplitter );
}

void BushuViewWidget::readDict()
{
    QFile file( BUSHUDICT );
    if ( file.open( IO_ReadOnly ) )
    {
        QTextStream stream( &file );
        stream.setCodec(QTextCodec::codecForName(BUSHUDICT_ENCODING));
        QString line;
        while ( !stream.atEnd() )
        {
            QString bushuName = QStringList::split( " ", stream.readLine()  ) [ 0 ];

            // insert last
            QListViewItem *lastItem = m_bushuListView->lastItem();
            if( lastItem )
            {
                new QListViewItem( m_bushuListView, lastItem, bushuName );
            }
            else
            {
                new QListViewItem( m_bushuListView, bushuName );
            }
                
        }
        file.close();
    }
}

void BushuViewWidget::slotBushuSelected( QListViewItem *item )
{
    if ( !item )
        return ;

    QString selectedBushuName = item->text( 0 );
    if ( selectedBushuName.isEmpty() )
        return ;

    QFile file( BUSHUDICT );
    if ( file.open( IO_ReadOnly ) )
    {
        QTextStream stream( &file );
        stream.setCodec(QTextCodec::codecForName(BUSHUDICT_ENCODING));
        QString line;

        // search selected bushu line by line
        while ( !stream.atEnd() )
        {
            QStringList chars = QStringList::split( " ",  stream.readLine()  );
            QString bushuName = chars[ 0 ];
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
    QSettings settings;

    // splitter
    QString str;
    QTextOStream out( &str );
    out << *m_mainSplitter;
    settings.writeEntry( "/uim-kdehelper/chardict/bushuview/splitter", str );
}

void BushuViewWidget::readConfig()
{
    QSettings settings;
    QString str;

    // splitter
    str = settings.readEntry( "/uim-kdehelper/chardict/bushuview/splitter" );
    if ( !str.isEmpty() )
    {
        QTextIStream in( &str );
        in >> *m_mainSplitter;
    }
}

void BushuViewWidget::setFont( const QFont &font )
{
    m_charGridView->setFont( font );
}

#include "bushuviewwidget.moc"
