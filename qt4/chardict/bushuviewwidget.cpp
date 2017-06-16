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
#include "qtgettext.h"

#include <config.h>

#include <QtCore/QFile>
#include <QtCore/QSettings>
#include <QtCore/QTextCodec>
#include <QtCore/QTextStream>
#if QT_VERSION < 0x050000
# include <QtGui/QFrame>
# include <QtGui/QHBoxLayout>
# include <QtGui/QLabel>
# include <QtGui/QListWidget>
# include <QtGui/QListWidgetItem>
# include <QtGui/QSplitter>
# include <QtGui/QVBoxLayout>
#else
# include <QtWidgets/QFrame>
# include <QtWidgets/QHBoxLayout>
# include <QtWidgets/QLabel>
# include <QtWidgets/QListWidget>
# include <QtWidgets/QListWidgetItem>
# include <QtWidgets/QSplitter>
# include <QtWidgets/QVBoxLayout>
#endif

static const char BUSHUDICT[] = DATADIR "/uim/helperdata/bushu.t";
static const char BUSHUDICT_ENCODING[] = "EUC-JP";

BushuViewWidget::BushuViewWidget( QWidget *parent )
        : CharDictViewBase( parent )
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

    QFrame *leftVBox = new QFrame( m_mainSplitter );
    QLabel *bushuLabel = new QLabel;
    bushuLabel->setText( _( "Bushu List" ) );
    bushuLabel->setAlignment( Qt::AlignHCenter );
    m_bushuListView = new QListWidget;
    m_bushuListView->setSelectionMode( QAbstractItemView::SingleSelection );
    m_bushuListView->setHorizontalScrollBarPolicy( Qt::ScrollBarAlwaysOff );
    connect( m_bushuListView, SIGNAL( itemSelectionChanged() ),
                      this, SLOT( slotBushuSelected() ) );

    m_charGridView = new CharGridView( 10, 0, m_mainSplitter );
    connect( m_charGridView, SIGNAL( charSelected( const QString & ) ),
                      this, SIGNAL( charSelected( const QString & ) ) );

    QVBoxLayout *vLayout = new QVBoxLayout;
    vLayout->setMargin( 0 );
    vLayout->addWidget( bushuLabel );
    vLayout->addWidget( m_bushuListView );

    leftVBox->setLayout( vLayout );

    // main layout
    QHBoxLayout* layout = new QHBoxLayout( this );
    layout->addWidget( m_mainSplitter );
}

void BushuViewWidget::readDict()
{
    QFile file( BUSHUDICT );
    if ( file.open( QIODevice::ReadOnly ) )
    {
        QTextStream stream( &file );
        stream.setCodec(QTextCodec::codecForName(BUSHUDICT_ENCODING));
        QString line;
        while ( !stream.atEnd() )
        {
            QString bushuName
                = stream.readLine().split( ' ', QString::SkipEmptyParts ) [ 0 ];

            // insert last
            m_bushuListView->addItem( bushuName );
        }
        file.close();
    }
}

void BushuViewWidget::slotBushuSelected()
{
    QList<QListWidgetItem *> items = m_bushuListView->selectedItems();
    if ( items.isEmpty() )
        return ;

    QString selectedBushuName = items[ 0 ]->text();
    if ( selectedBushuName.isEmpty() )
        return ;

    QFile file( BUSHUDICT );
    if ( file.open( QIODevice::ReadOnly ) )
    {
        QTextStream stream( &file );
        stream.setCodec(QTextCodec::codecForName(BUSHUDICT_ENCODING));
        QString line;

        // search selected bushu line by line
        while ( !stream.atEnd() )
        {
            QStringList chars
                = stream.readLine().split( ' ', QString::SkipEmptyParts );
            QString bushuName = chars[ 0 ];
            if ( selectedBushuName == bushuName )
            {
                // Display Characters
                chars.removeAll( bushuName );
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
    QTextStream out( &str );
    out << *m_mainSplitter;
    settings.setValue( "/uim-kdehelper/chardict/bushuview/splitter", str );
}

void BushuViewWidget::readConfig()
{
    QSettings settings;
    QString str;

    // splitter
    str = settings.value(
        "/uim-kdehelper/chardict/bushuview/splitter" ).toString();
    if ( !str.isEmpty() )
    {
        QTextStream in( &str );
        in >> *m_mainSplitter;
    }
}

void BushuViewWidget::setCharFont( const QFont &font )
{
    m_charGridView->setCharFont( font );
}
