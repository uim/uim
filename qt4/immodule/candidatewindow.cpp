/*

Copyright (c) 2003-2011 uim Project http://code.google.com/p/uim/

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
#include "candidatewindow.h"

#include <QtGui/QFontMetrics>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QVBoxLayout>

#include <uim/uim-scm.h>

#include "quiminputcontext.h"
#include "subwindow.h"

static const int MIN_CAND_WIDTH = 80;

static const int HEADING_COLUMN = 0;
static const int CANDIDATE_COLUMN = 1;
static const int ANNOTATION_COLUMN = 2;

CandidateWindow::CandidateWindow( QWidget *parent, bool vertical )
: AbstractCandidateWindow( parent ), subWin( 0 ),
    hasAnnotation( uim_scm_symbol_value_bool( "enable-annotation?" ) ),
    isVertical( vertical )
{
    //setup CandidateList
    cList = new CandidateListView( 0, isVertical );
    cList->setSelectionMode( QAbstractItemView::SingleSelection );
    cList->setSelectionBehavior( isVertical
        ? QAbstractItemView::SelectRows : QAbstractItemView::SelectColumns );
    cList->setMinimumWidth( MIN_CAND_WIDTH );
    if ( isVertical )
        // the last column is dummy for adjusting size.
        cList->setColumnCount( hasAnnotation ? 4 : 3 );
    else
        cList->setRowCount( 2 );
    cList->horizontalHeader()->setResizeMode( QHeaderView::ResizeToContents );
    cList->horizontalHeader()->setStretchLastSection( true );
    if ( !isVertical ) {
        cList->verticalHeader()
            ->setResizeMode( QHeaderView::ResizeToContents );
        cList->verticalHeader()->setStretchLastSection( true );
    }
    cList->horizontalHeader()->hide();
    cList->verticalHeader()->hide();
    cList->setHorizontalScrollBarPolicy( Qt::ScrollBarAlwaysOff );
    cList->setVerticalScrollBarPolicy( Qt::ScrollBarAlwaysOff );
    cList->setAutoScroll( false );
    cList->setShowGrid( false );
    connect( cList, SIGNAL( cellClicked( int, int ) ),
          this , SLOT( slotCandidateSelected( int, int ) ) );
    connect( cList, SIGNAL( itemSelectionChanged() ),
          this , SLOT( slotHookSubwindow() ) );

    QVBoxLayout *layout = new QVBoxLayout;
    layout->setMargin( 0 );
    layout->setSpacing( 0 );
    layout->addWidget( cList );
    layout->addWidget( numLabel );
    setLayout( layout );
}

void CandidateWindow::activateCandwin( int dLimit )
{
    AbstractCandidateWindow::activateCandwin( dLimit );

    if ( !subWin )
        subWin = new SubWindow( this );
}

#if UIM_QT_USE_NEW_PAGE_HANDLING
void CandidateWindow::setNrCandidates( int nrCands, int dLimit )
{
    AbstractCandidateWindow::setNrCandidates( nrCands, dLimit );

    if ( !subWin )
        subWin = new SubWindow( this );
}
#endif /* UIM_QT_USE_NEW_PAGE_HANDLING */

void CandidateWindow::updateView( int newpage, int ncandidates )
{
    cList->clearContents();
    annotations.clear();

    if ( isVertical )
        cList->setRowCount( ncandidates );
    else 
        // the last column is dummy for adjusting size.
        cList->setColumnCount( ncandidates + 1 );
    for ( int i = 0; i < ncandidates ; i++ ) {
        uim_candidate cand = stores[ displayLimit * newpage + i ];
        QString headString
            = QString::fromUtf8( uim_candidate_get_heading_label( cand ) );
        QString candString
            = QString::fromUtf8( uim_candidate_get_cand_str( cand ) );
        QString annotationString;
        if ( hasAnnotation ) {
            annotationString
                = QString::fromUtf8( uim_candidate_get_annotation_str( cand ) );
            annotations.append( annotationString );
        }

        // insert new item to the candidate list
        if ( isVertical ) {
            QTableWidgetItem *headItem = new QTableWidgetItem;
            headItem->setText( headString );
            headItem->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );

            QTableWidgetItem *candItem = new QTableWidgetItem;
            candItem->setText( candString );
            candItem->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );

            cList->setItem( i, HEADING_COLUMN, headItem );
            cList->setItem( i, CANDIDATE_COLUMN, candItem );

            if ( hasAnnotation ) {
                QTableWidgetItem *annotationItem = new QTableWidgetItem;
                annotationItem->setFlags(
                    Qt::ItemIsSelectable | Qt::ItemIsEnabled );
                if ( !annotationString.isEmpty() )
                    annotationItem->setText( "..." );

                cList->setItem( i, ANNOTATION_COLUMN, annotationItem );
            }
            cList->setRowHeight( i,
                QFontMetrics( cList->font() ).height() + 2 );
        } else {
            QTableWidgetItem *candItem = new QTableWidgetItem;
            candItem->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );

            QString candText = headString + ": " + candString;
            if ( hasAnnotation && !annotationString.isEmpty() )
                candText += "...";
            candItem->setText( candText );

            cList->setItem( 0, i, candItem );
        }
    }
    if ( !isVertical )
        cList->setRowHeight( 0, QFontMetrics( cList->font() ).height() + 2 );
}

void CandidateWindow::updateSize()
{
    // size adjustment
    cList->updateGeometry();
    setFixedSize(sizeHint());
}

void CandidateWindow::setIndex( int totalindex )
{
    AbstractCandidateWindow::setIndex( totalindex );

    // select item
    if ( candidateIndex >= 0 ) {
        int pos = totalindex;
        if ( displayLimit )
            pos = candidateIndex % displayLimit;

        int row;
        int column;
        if ( isVertical ) {
            row = pos;
            column = 0;
        } else {
            row = 0;
            column = pos;
        }
        if ( cList->item( row, column )
            && !cList->item( row, column )->isSelected() ) {
            cList->clearSelection();
            if ( isVertical )
                cList->selectRow( pos );
            else
                cList->selectColumn( pos );
        }
    } else {
        cList->clearSelection();
    }

    updateLabel();
}

void CandidateWindow::slotCandidateSelected( int row, int column )
{
    candidateIndex = ( pageIndex * displayLimit )
        + ( isVertical ? row : column );
    if ( ic && ic->uimContext() )
        uim_set_candidate_index( ic->uimContext(), candidateIndex );
    updateLabel();
}

void CandidateWindow::shiftPage( bool forward )
{
    AbstractCandidateWindow::shiftPage( forward );
    if ( candidateIndex != -1 ) {
        cList->clearSelection();
        int idx = displayLimit ? candidateIndex % displayLimit : candidateIndex;
        if ( isVertical )
            cList->selectRow( idx );
        else
            cList->selectColumn( idx );
    }
}


#if UIM_QT_USE_NEW_PAGE_HANDLING
#endif /* UIM_QT_USE_NEW_PAGE_HANDLING */

void CandidateWindow::slotHookSubwindow()
{
    if ( !hasAnnotation || !subWin )
        return;

    QList<QTableWidgetItem *> list = cList->selectedItems();
    if ( list.isEmpty() )
        return;

    // cancel previous hook
    subWin->cancelHook();

    // hook annotation
    QString annotationString
        = annotations.at( isVertical ? list[0]->row() : list[0]->column() );
    if ( !annotationString.isEmpty() ) {
        subWin->layoutWindow( frameGeometry() );
        subWin->hookPopup( annotationString );
    }
}

// Moving and Resizing affects the position of Subwindow
void CandidateWindow::moveEvent( QMoveEvent *e )
{
    // move subwindow
    if ( subWin )
        subWin->layoutWindow( QRect( e->pos(), size() ) );
}

void CandidateWindow::resizeEvent( QResizeEvent *e )
{
    // move subwindow
    if ( subWin )
        subWin->layoutWindow( QRect( pos(), e->size() ) );
}

void CandidateWindow::hideEvent( QHideEvent *event )
{
    QFrame::hideEvent( event );
    if ( subWin )
        subWin->cancelHook();
}

QSize CandidateWindow::sizeHint() const
{
    QSize cListSizeHint = cList->sizeHint();

    // According to the Qt4 documentation on the QFrame class,
    // the frame width is 1 pixel.
    int frame = 1 * 2;
    int width = cListSizeHint.width() + frame;
    int height = cListSizeHint.height() + numLabel->height() + frame;

    return QSize( width, height );
}

QSize CandidateListView::sizeHint() const
{
    // frame width
    int frame = style()->pixelMetric( QStyle::PM_DefaultFrameWidth ) * 2;

    // the size of the dummy row should be 0.
    const int rowNum = isVertical ? rowCount() : rowCount() - 1;
    if ( rowNum == 0 ) {
        return QSize( MIN_CAND_WIDTH, frame );
    }
    int width = frame;
    // the size of the dummy column should be 0.
    for ( int i = 0; i < columnCount() - 1; i++ )
        width += columnWidth( i );

    return QSize( width, rowHeight( 0 ) * rowNum + frame );
}
