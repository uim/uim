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

#include <qpainter.h>
#include <qpoint.h>
#include <qbrush.h>
#include <qdrawutil.h>

static const int COLS = 10;

CharGridView::CharGridView( int x, int y, QWidget *parent, const char *name )
        : QGridView( parent, name )
{
    setNumCols( x );
    setNumRows( y );
    setCellWidth( 30 );
    setCellHeight( 30 );

    setHScrollBarMode( QScrollView::AlwaysOff );

    m_activeCell.setX( -1 );
    m_activeCell.setY( -1 );

    m_font = font();

    setFrameStyle( QFrame::NoFrame );
    show();
}
CharGridView::~CharGridView()
{}
void CharGridView::paintCell( QPainter * painter, int y, int x )
{
    // set font
    if ( m_font != font() )
        painter->setFont( m_font );

    bool isActiveCell = ( ( m_activeCell.x() == x ) && ( m_activeCell.y() == y ) );
    if ( isActiveCell )
    {
        // save the painter's state to the stack and swap back/fore ground colors
        painter->save();
        QColor tmp( painter->pen().color() );
        painter->setPen( painter->backgroundColor() );
        painter->setBackgroundColor( tmp );
    }

    QBrush bBrush( colorGroup().base() );
    QBrush tBrush( colorGroup().text() );
    if ( isActiveCell )
        qDrawShadePanel( painter, 0, 0, cellWidth(), cellHeight(), colorGroup(), false, 2, &tBrush ); //&(painter->brush()));
    else
        qDrawPlainRect( painter, 0, 0, cellWidth(), cellHeight(), colorGroup().foreground(), 1, &bBrush ); //&(painter->brush()));

    QString c = coordsToChar( x, y );
    if ( !c.isEmpty() )
        painter->drawText( 0, 0, cellWidth(), cellHeight(), AlignCenter, c );

    // restore state
    if ( isActiveCell )
        painter->restore();
}

void CharGridView::contentsMousePressEvent( QMouseEvent * e )
{
    if ( e->button() != LeftButton )
        return ;

    int y = e->pos().y();
    int x = e->pos().x();

    int row = rowAt( y );
    int col = columnAt( x );

    if ( ( m_activeCell.y() != row ) || ( m_activeCell.x() != col ) )
    {
        int oldactivecelly = m_activeCell.y();
        int oldactivecellx = m_activeCell.x();
        m_activeCell.setY( row );
        m_activeCell.setX( col );
        updateCell( oldactivecelly, oldactivecellx );
        updateCell( m_activeCell.y(), m_activeCell.x() );
    }
}

void CharGridView::contentsMouseReleaseEvent( QMouseEvent * e )
{
    if ( e->button() != LeftButton )
        return ;

    emit charSelected( coordsToChar( m_activeCell.x(), m_activeCell.y() ) );
}
QString CharGridView::coordsToChar( int x, int y )
{
    if ( x < 0 || x > numCols() || y < 0 || y > numRows() )
        debug( "coordsToIndex: invalid coords(%d, %d)\n", x, y );

    unsigned int index = y * numCols() + x;
    if ( index < m_charList.count() )
        return m_charList[ index ];
    else
        return QString::null;
}

QSize CharGridView::sizeHint( void ) const
{
    return QSize( numCols() * cellWidth(), numRows() * cellHeight() );
}

void CharGridView::resizeEvent( QResizeEvent * /* e */ )
{
    updateCharGridView();
}

void CharGridView::setCharacters( const QStringList &charList )
{
    // default position
    m_activeCell.setX( -1 );
    m_activeCell.setY( -1 );

    m_charList.clear();
    m_charList = charList;

    int total = m_charList.count();

    int cols = COLS;
    int rows = total / COLS;
    if ( total % COLS > 0 )
    {
        rows++;
    }

    setNumCols( cols );
    setNumRows( rows );

    updateCharGridView();

    // repaint all cells
    for ( int i = 0; i < numRows(); i++ )
        for ( int j = 0; j < numCols(); j++ )
            repaintCell( i, j, true );
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
    cellsize = ( width() - vScrollBar->minimumWidth() ) / numCols();

    setCellWidth( cellsize );
    setCellHeight( cellsize );
}

#include "chargridview.moc"
