/*

 Copyright (c) 2003-2009 uim Project http://code.google.com/p/uim/

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

#include <clocale>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>

#include <QtCore/QSocketNotifier>
#include <QtCore/QStringList>
#include <QtCore/QTextCodec>
#include <QtGui/QApplication>
#include <QtGui/QDesktopWidget>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QTableWidget>
#include <QtGui/QVBoxLayout>

#include <uim/uim.h>
#include <uim/uim-helper.h>

#include "qtgettext.h"

static const int NR_CANDIDATES = 10;
static const int MIN_CAND_WIDTH = 80;

static const int HEADING_COLUMN = 0;
static const int CANDIDATE_COLUMN = 1;

const Qt::WFlags candidateFlag = ( Qt::Window
                                   | Qt::WindowStaysOnTopHint
                                   | Qt::FramelessWindowHint
                                   | Qt::Tool
#if defined(Q_WS_X11)
                                   | Qt::X11BypassWindowManagerHint
#endif
                                 );
static QSocketNotifier *notifier = 0;

CandidateWindow::CandidateWindow( QWidget *parent )
        : QFrame( parent, candidateFlag )
{
    setFrameStyle( Raised | NoFrame );
    setFocusPolicy( Qt::NoFocus );

    //setup CandidateList
    cList = new QTableWidget;
    cList->setSelectionMode( QAbstractItemView::SingleSelection );
    cList->setSelectionBehavior( QAbstractItemView::SelectRows );
    // the last column is dummy for adjusting size.
    cList->setColumnCount( 3 );
    cList->horizontalHeader()->setResizeMode( QHeaderView::ResizeToContents );
    cList->horizontalHeader()->setStretchLastSection( true );
    cList->horizontalHeader()->hide();
    cList->verticalHeader()->hide();
    cList->setHorizontalScrollBarPolicy( Qt::ScrollBarAlwaysOff );
    cList->setVerticalScrollBarPolicy( Qt::ScrollBarAlwaysOff );
    cList->setShowGrid( false );
    cList->setMinimumWidth( MIN_CAND_WIDTH );
    connect( cList, SIGNAL( cellClicked( int, int ) ),
          this , SLOT( slotCandidateSelected( int ) ) );

    //setup NumberLabel
    numLabel = new QLabel;
    numLabel->setFocusPolicy( Qt::NoFocus );

    nrCandidates = 0;
    candidateIndex = 0;
    displayLimit = NR_CANDIDATES;
    pageIndex = -1;

    isActive = false;

    notifier = new QSocketNotifier( 0, QSocketNotifier::Read );
    connect( notifier, SIGNAL( activated( int ) ),
                      this, SLOT( slotStdinActivated( int ) ) );
    hide();

    QVBoxLayout *layout = new QVBoxLayout;
    layout->setMargin( 0 );
    layout->setSpacing( 0 );
    layout->addWidget( cList );
    layout->addWidget( numLabel );
    setLayout( layout );
}

CandidateWindow::~CandidateWindow()
{
    if ( !stores.isEmpty() )
        stores.clear();
}

void CandidateWindow::activateCand( const QStringList &list )
{
#if defined(ENABLE_DEBUG)
    qDebug( "uim-candwin-qt4: activateCand()" );
#endif
    /**
     * format: activate\ncharset=$charset\ndisplay_limit=$value\nhead1\tcand1\nhead2\tcand2\nhead3\tcand3\n
     */

    // remove old data
    cList->clearContents();
    cList->setRowCount( 0 );
    stores.clear();

    // get charset and create codec
    QTextCodec *codec = 0;
    if ( !list[ 1 ].isEmpty()
        && list[ 1 ].startsWith( QLatin1String( "charset" ) ) )
    {
        const QStringList l = list[ 1 ].split( '=', QString::SkipEmptyParts );
        codec = QTextCodec::codecForName( l[ 1 ].toAscii() );
    }

    // get display_limit
    if ( !list[ 2 ].isEmpty()
        && list[ 2 ].startsWith( QLatin1String( "display_limit" ) ) )
    {
        const QStringList l = list[ 2 ].split( '=', QString::SkipEmptyParts );
        displayLimit = l[ 1 ].toInt();
    }

    for ( int i = 3; !list[ i ].isNull(); i++ )
    {
        // case list[i] = ""
        if ( list[ i ].isEmpty() )
            break;

        // split heading_label and cand_str
        QStringList l = list[ i ].split( '\t' );

        // store data
        CandData d;
        QString headString;
        if ( codec )
            headString = codec->toUnicode( l[ 0 ].toAscii() );
        else
            headString = l [ 0 ];

        d.label = headString;

        // XXX Current prime (0.4.6) may return candidate string
        // containing "\t", and we can't handle annotation in another
        // window yet.
        l.pop_front();
        QString candString = l.join( "\t" );

        if ( codec )
            d.str = codec->toUnicode( candString.toAscii() );
        else
            d.str = candString;

        stores.append( d );
    }

    // set default value
    candidateIndex = -1;
    nrCandidates = stores.count();

    // shift to default page
    setPage( 0 );

    adjustCandidateWindowSize();
    show();

    isActive = true;
}
void CandidateWindow::selectCand( const QStringList &list )
{
#if defined(ENABLE_DEBUG)
    qDebug( "uim-candwin-qt4: selectCand()" );
#endif
    const int index = list[ 1 ].toInt();
    needHighlight = (list[ 2 ].toInt() == 1);
    setIndex( index );

    updateLabel();
}

void CandidateWindow::moveCand( const QStringList &list )
{
#if defined(ENABLE_DEBUG)
    qDebug( "uim-candwin-qt4: moveCand()" );
#endif
    if ( list[ 1 ].isEmpty() || list[ 2 ].isEmpty() )
        return ;

    const int topwin_x = list[ 1 ].toInt();
    const int topwin_y = list[ 2 ].toInt();
    const int cw_wi = width();
    const int cw_he = height();
    const int sc_wi = QApplication::desktop() ->screenGeometry().width();
    const int sc_he = QApplication::desktop() ->screenGeometry().height();

    int x, y;
    if ( sc_wi < topwin_x + cw_wi )
    {
        x = topwin_x - cw_wi;
    }
    else
    {
        x = topwin_x;
    }

    if ( sc_he < topwin_y + cw_he )
    {
        /* FIXME : How can I determine the preedit height? */
        y = topwin_y - cw_he - 20;
    }
    else
    {
        y = topwin_y;
    }

    move( x, y );
}

void CandidateWindow::showCand()
{
#if defined(ENABLE_DEBUG)
    qDebug( "uim-candwin-qt4: showCand()" );
#endif
    if ( isActive )
        show();
}
void CandidateWindow::deactivateCand()
{
#if defined(ENABLE_DEBUG)
    qDebug( "uim-candwin-qt4: deactivateCand()" );
#endif
    hide();
    isActive = false;
}
void CandidateWindow::setNrCandidates( const QStringList &list )
{
#if defined(ENABLE_DEBUG)
    qDebug( "uim-candwin-qt: setNrCandidates()" );
#endif
    if ( list[ 1 ].isEmpty() || list[ 2 ].isEmpty() )
        return ;

    // remove old data
    cList->clearContents();
    cList->setRowCount( 0 );
    stores.clear();

    // set default value
    candidateIndex = -1;
    nrCandidates = list[ 1 ].toInt();
    displayLimit = list[ 2 ].toInt();
    needHighlight = false;
    isActive = true;

    // setup dummy stores
    for ( int i = 0; i < nrCandidates; i++ ) {
        CandData d;
        stores.append( d );
    }
}
void CandidateWindow::setPageCandidates( const QStringList &list )
{
#if defined(ENABLE_DEBUG)
    qDebug( "uim-candwin-qt: setPageCandidates()" );
#endif
    /**
     * format: set_page_candidates\ncharset=$charset\npage=$value\nhead1\tcand1\nhead2\tcand2\nhead3\tcand3\n
     */

    int page = 0;

    // get charset and create codec
    QTextCodec *codec = 0;
    if ( !list[ 1 ].isEmpty()
        && list[ 1 ].startsWith( QLatin1String( "charset" ) ) )
    {
        const QStringList l = list[ 1 ].split( '=', QString::SkipEmptyParts );
        codec = QTextCodec::codecForName( l[ 1 ].toAscii() );
    }

    // get page
    if ( !list[ 2 ].isEmpty()
        && list[ 2 ].startsWith( QLatin1String( "page" ) ) )
    {
        const QStringList l = list[ 2 ].split( '=', QString::SkipEmptyParts );
        page = l[ 1 ].toInt();
    }

    int len = list.count();
    for ( int i = 3; i < len; i++ )
    {
        // case list[i] = ""
        if ( list[ i ].isEmpty() )
            break;

        // split heading_label and cand_str
        QStringList l = list [ i ].split( '\t' );

        // store data
        CandData &d = stores[page * displayLimit + i - 3];
        QString headString;
        if ( codec )
            headString = codec->toUnicode( l [ 0 ].toAscii() );
        else
            headString = l [ 0 ];

        d.label = headString;

        // XXX Current prime (0.4.6) may return candidate string
        // containing "\t", and we can't handle annotation in another
        // window yet.
        l.pop_front();
        QString candString = l.join( "\t" );

        if ( codec )
            d.str = codec->toUnicode( candString.toAscii() );
        else
            d.str = candString;
    }
}
void CandidateWindow::showPage( const QStringList &list )
{
#if defined(ENABLE_DEBUG)
    qDebug( "uim-candwin-qt: showPage()" );
#endif
    const int page = list[ 1 ].toInt();

    setPage( page );
    adjustCandidateWindowSize();
    show();
}
void CandidateWindow::slotStdinActivated( int fd )
{
    char buf[ 4096 ];
    char *read_buf = strdup( "" );
    int n;

    while (uim_helper_fd_readable( fd ) > 0) {
        n = read( fd, buf, 4096 - 1 );
        if ( n == 0 )
        {
            ::close( fd );
            exit( 1 );
        }
        if ( n == -1 )
            return ;
        buf[ n ] = '\0';
        read_buf = (char *)realloc( read_buf, strlen( read_buf ) + n + 1 );
        strcat( read_buf, buf );
    }

    QStringList msgList
        = QString( read_buf ).split( "\n\n", QString::SkipEmptyParts );

    QStringList::Iterator it = msgList.begin();
    const QStringList::Iterator end = msgList.end();
    for ( ; it != end; ++it )
        strParse( ( *it ) );
    free( read_buf );
}

void CandidateWindow::strParse( const QString& str )
{
#if defined(ENABLE_DEBUG)
    qDebug( "str = %s", str.toLocal8Bit().constData() );
#endif
    QStringList list = str.split( '\n', QString::SkipEmptyParts );

    QStringList::Iterator it = list.begin();
    const QStringList::Iterator end = list.end();
    for ( ; it != end; ++it )
    {
        if ( QString::compare( "activate", ( *it ) ) == 0 )
            activateCand( list );
        else if ( QString::compare( "select", ( *it ) ) == 0 )
            selectCand( list );
        else if ( QString::compare( "show", ( *it ) ) == 0 )
            showCand();
        else if ( QString::compare( "hide", ( *it ) ) == 0 )
            hide();
        else if ( QString::compare( "move", ( *it ) ) == 0 )
            moveCand( list );
        else if ( QString::compare( "deactivate", ( *it ) ) == 0 )
            deactivateCand();
        else if ( QString::compare( "set_nr_candidates", ( *it ) ) == 0 )
            setNrCandidates( list );
        else if ( QString::compare( "set_page_candidates", ( *it ) ) == 0 )
            setPageCandidates( list );
        else if ( QString::compare( "show_page", ( *it ) ) == 0 )
            showPage( list );
    }
}

void CandidateWindow::slotCandidateSelected( int row )
{
    cList->scrollToItem( cList->item( row, 0 ) );
    candidateIndex = ( pageIndex * displayLimit ) + row;

    // write message
    fprintf( stdout, "index\n" );
    fprintf( stdout, "%d\n\n", candidateIndex );
    fflush( stdout );

    updateLabel();
}

void CandidateWindow::adjustCandidateWindowSize()
{
#if defined(ENABLE_DEBUG)
    qDebug( "adjustCandidateWindowSize()" );
#endif
    // frame width
    // According to the Qt4 documentation on the QFrame class,
    // the frame width is 1 pixel.
    int frame = 1 * 2
        + cList->style()->pixelMetric( QStyle::PM_DefaultFrameWidth ) * 2;

    const int rowNum = cList->rowCount();
    if ( rowNum == 0 ) {
        resize( MIN_CAND_WIDTH, numLabel->height() + frame );
        return;
    }
    int width = frame;
    // the size of the dummy column should be 0.
    for ( int i = 0; i < cList->columnCount() - 1; i++ )
        width += cList->columnWidth( i );

    resize( width, cList->rowHeight( 0 ) * rowNum + numLabel->height()
        + frame );
}

void CandidateWindow::setPage( int page )
{
    // clear items
    cList->clearContents();

    // calculate page
    int newpage, lastpage;
    if ( displayLimit )
        lastpage = nrCandidates / displayLimit;
    else
        lastpage = 0;
    
    if ( page < 0 )
    {
        newpage = lastpage;
    }
    else if ( page > lastpage )
    {
        newpage = 0;
    }
    else
    {
        newpage = page;
    }

    pageIndex = newpage;

    // calculate index
    int newindex;
    if ( displayLimit )
    {
        if ( candidateIndex >= 0 )
            newindex = ( newpage * displayLimit ) + ( candidateIndex % displayLimit );
        else
            newindex = -1;
    }
    else
    {
        newindex = candidateIndex;
    }

    if ( newindex >= nrCandidates )
        newindex = nrCandidates - 1;

    // set cand items
    //
    // If we switch to last page, the number of items to be added
    // is lower than displayLimit.
    //
    // ex. if nrCandidate==14 and displayLimit==10, the number of
    //     last page's item==4
    int ncandidates = displayLimit;
    if ( newpage == lastpage )
        ncandidates = nrCandidates - displayLimit * lastpage;
    cList->setRowCount( ncandidates );
    for ( int i = 0; i < ncandidates ; i++ )
    {
        QString headString = stores[ displayLimit * newpage + i ].label;
        QString candString = stores[ displayLimit * newpage + i ].str;

        // insert new item to the candidate list
        QTableWidgetItem *headItem = new QTableWidgetItem;
        headItem->setText( headString );
        headItem->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );
        QTableWidgetItem *candItem = new QTableWidgetItem;
        candItem->setText( candString );
        candItem->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );
        cList->setItem( i, HEADING_COLUMN, headItem );
        cList->setItem( i, CANDIDATE_COLUMN, candItem );
        cList->setRowHeight( i, QFontMetrics( cList->font() ).height() + 2 );
    }

    // set index
    if ( newindex != candidateIndex )
        setIndex( newindex );
    else
        updateLabel();

    // set candwin size
    adjustCandidateWindowSize();
}

void CandidateWindow::setIndex( int index )
{
#if defined(ENABLE_DEBUG)
    qDebug( "setIndex : index = %d", index );
#endif
    // validity check
    if ( index < 0 )
        candidateIndex = nrCandidates - 1;
    else if ( index >= nrCandidates )
        candidateIndex = 0;
    else
        candidateIndex = index;

    // set page
    int newpage = 0;
    if ( displayLimit )
        newpage = candidateIndex / displayLimit;
    if ( pageIndex != newpage )
        setPage( newpage );

    // select item
    if ( candidateIndex >= 0 && needHighlight )
    {
        int pos = index;
        if ( displayLimit )
            pos = candidateIndex % displayLimit;

        if ( cList->item( pos, 0 ) && !cList->item( pos, 0 )->isSelected() )
        {
            cList->clearSelection();
            cList->selectRow( pos );
            cList->scrollToItem( cList->item( pos, 0 ) );
        }
    }
    else
    {
        cList->clearSelection();
    }

    updateLabel();
}

void CandidateWindow::updateLabel()
{
    QString indexString;
    if ( candidateIndex >= 0 && needHighlight )
        indexString = QString::number( candidateIndex + 1 ) + " / " + QString::number( nrCandidates );
    else
        indexString = "- / " + QString::number( nrCandidates );

    numLabel->setText( indexString );
}

int main( int argc, char *argv[] )
{
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    bind_textdomain_codeset(PACKAGE, "UTF-8"); // ensure code encoding is UTF8-
    
    QApplication a( argc, argv );

    CandidateWindow b;

    return a.exec();
}
