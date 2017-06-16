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
#include "chargridview.h"

#include <QtGui/QFont>
#include <QtGui/QMouseEvent>
#include <QtGui/QResizeEvent>
#if QT_VERSION < 0x050000
# include <QtGui/QHeaderView>
# include <QtGui/QScrollBar>
#else
# include <QtWidgets/QHeaderView>
# include <QtWidgets/QScrollBar>
#endif

static const int COLS = 10;

CharGridView::CharGridView( int x, int y, QWidget *parent )
        : QTableWidget( parent )
{
    setColumnCount( x );
    setRowCount( y );
    QHeaderView *header = horizontalHeader();
    header->setVisible( false );
    for ( int i = 0; i < x; i++)
    {
#if QT_VERSION < 0x050000
        header->setResizeMode( i, QHeaderView::Fixed );
#else
        header->setSectionResizeMode( i, QHeaderView::Fixed );
#endif
        header->resizeSection( i, 30 );
    }
    header = verticalHeader();
    header->setVisible( false );
    for ( int i = 0; i < y; i++)
    {
#if QT_VERSION < 0x050000
        header->setResizeMode( i, QHeaderView::Fixed );
#else
        header->setSectionResizeMode( i, QHeaderView::Fixed );
#endif
        header->resizeSection( i, 30 );
    }
    setSelectionBehavior( QAbstractItemView::SelectItems );
    setSelectionMode( QAbstractItemView::SingleSelection );

    setHorizontalScrollBarPolicy( Qt::ScrollBarAlwaysOff );


    setFrameStyle( QFrame::NoFrame );
    show();
}

CharGridView::~CharGridView()
{}

void CharGridView::mousePressEvent( QMouseEvent * e )
{
    if ( e->button() != Qt::LeftButton )
        return;
    QTableWidget::mousePressEvent( e );
}

void CharGridView::mouseReleaseEvent( QMouseEvent * e )
{
    if ( e->button() != Qt::LeftButton )
        return;

    QTableWidgetItem *item = itemAt( e->pos() );
    emit charSelected( item ? item->text() : QString() );
}

QSize CharGridView::sizeHint() const
{
    return QSize( columnCount() * horizontalHeader()->sectionSize( 0 ),
        rowCount() * verticalHeader()->sectionSize( 0 ) );
}

void CharGridView::setCharFont( const QFont &font )
{
    setFont( font );
    for ( int i = 0; i < rowCount(); i++)
    {
        for ( int j = 0; j < columnCount(); j++)
        {
            QTableWidgetItem *cell = item( i, j );
            if ( cell )
                cell->setFont( font );
        }
    }
}

void CharGridView::resizeEvent( QResizeEvent *e )
{
    Q_UNUSED( e )
    updateCharGridView();
}

void CharGridView::setCharacters( const QStringList &charList )
{
    int total = charList.count();

    int cols = COLS;
    int rows = total / COLS;
    if ( total % COLS > 0 )
    {
        rows++;
    }
    int prevCols = columnCount();
    int prevRows = rowCount();
    setColumnCount( cols );
    setRowCount( rows );
    QHeaderView *header = horizontalHeader();
    for ( int i = prevCols + 1; i < cols; i++)
    {
#if QT_VERSION < 0x050000
        header->setResizeMode( i, QHeaderView::Fixed );
#else
        header->setSectionResizeMode( i, QHeaderView::Fixed );
#endif
        header->resizeSection( i, header->sectionSize( 0 ) );
    }
    header = verticalHeader();
    for ( int i = prevRows + 1; i < rows; i++)
    {
#if QT_VERSION < 0x050000
        header->setResizeMode( i, QHeaderView::Fixed );
#else
        header->setSectionResizeMode( i, QHeaderView::Fixed );
#endif
        header->resizeSection( i, header->sectionSize( 0 ) );
    }
    for ( int i = 0; i < rows; i++)
    {
        for ( int j = 0; j < cols; j++)
        {
            int index = i * COLS + j;
            QString str = index < total ? charList.at( index ) : QString();
            QTableWidgetItem *cell = item( i, j );
            if ( !cell )
            {
                cell = new QTableWidgetItem;
                cell->setTextAlignment( Qt::AlignCenter );
                cell->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );
                cell->setFont( font() );
                setItem( i, j, cell );
            }
            cell->setText( str );
        }
    }

    updateCharGridView();
}

void CharGridView::updateCharGridView()
{
    int cellsize = 0;
    QScrollBar *vScrollBar = verticalScrollBar();
    /**
     * 2004-12-06 Kazuki Ohta <mover@hct.zaq.ne.jp>
     * FIXME:
     * The timing vScrollBar is shown is tricky.
     * So this code doesn't work properly.
     * hmm..
     */
    /*
    if( vScrollBar->isShown() )
    {
        qDebug("vScrollBar->isShown() = true");
        cellsize = (width() - vScrollBar->minimumWidth())/numCols();
    }
    else
    {
        qDebug("vScrollBar->isShown() = false");
        cellsize = width()/numCols();
    }
    */ 
    // adhoc code
    // but minimumWidth() is always 0 ?
    cellsize = ( width() - vScrollBar->minimumWidth() ) / columnCount();

    QHeaderView *header = horizontalHeader();
    for ( int i = 0; i < columnCount(); i++)
        header->resizeSection( i, cellsize );
    header = verticalHeader();
    for ( int i = 0; i < rowCount(); i++)
        header->resizeSection( i, cellsize );
}
