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
#include <config.h>

#include <qapplication.h>
#include <qlabel.h>
#include <qwidget.h>
#include <qheader.h>
#include <qsocketnotifier.h>
#include <qstringlist.h>
#include <qtextcodec.h>
#include <qrect.h>

#include <locale.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>


#include "qtgettext.h"
#include "qt.h"

static const int NR_CANDIDATES = 10;
static const int MIN_CAND_WIDTH = 80;

const Qt::WFlags candidateFlag = ( Qt::WType_TopLevel
                                   | Qt::WStyle_Customize
                                   | Qt::WStyle_StaysOnTop
                                   | Qt::WStyle_NoBorder
                                   | Qt::WStyle_Tool
#if defined(Q_WS_X11)
                                   | Qt::WX11BypassWM
#endif
                                 );
static QSocketNotifier *notifier = NULL;

CandidateWindow::CandidateWindow( QWidget *parent, const char * name )
        : QVBox( parent, name, candidateFlag )
{
    setFrameStyle( Raised | NoFrame );
    setFocusPolicy( QWidget::NoFocus );

    //setup CandidateList
    cList = new CandidateListView( this, "candidateListView" );
    cList->setSorting( -1 );
    cList->setSelectionMode( QListView::Single );
    cList->addColumn( "1" );
    cList->setColumnWidthMode( 0, QListView::Maximum );
    cList->addColumn( "2" );
    cList->setColumnWidthMode( 1, QListView::Maximum );
    cList->header() ->hide();
    cList->setVScrollBarMode( QScrollView::AlwaysOff );
    cList->setHScrollBarMode( QScrollView::AlwaysOff );
    cList->setAllColumnsShowFocus( true );
    QObject::connect( cList, SIGNAL( clicked( QListViewItem * ) ),
                      this , SLOT( slotCandidateSelected( QListViewItem * ) ) );

    //setup NumberLabel
    numLabel = new QLabel( this, "candidateLabel" );
    numLabel->setFocusPolicy( QWidget::NoFocus );

    nrCandidates = 0;
    candidateIndex = 0;
    displayLimit = NR_CANDIDATES;
    pageIndex = -1;

    isActive = false;

    notifier = new QSocketNotifier( 0, QSocketNotifier::Read );
    QObject::connect( notifier, SIGNAL( activated( int ) ),
                      this, SLOT( slotStdinActivated( int ) ) );
    hide();
}

CandidateWindow::~CandidateWindow()
{
    if ( !stores.isEmpty() )
        stores.clear();
}

void CandidateWindow::activateCand( const QStringList &list )
{
#if defined(ENABLE_DEBUG)
    qDebug( "uim-candwin-qt: activateCand()" );
#endif
    /**
     * format: activate\fcharset=$charset\fdisplay_limit=$value\fhead1\acand1\aannot1\fhead2\acand2\aannot2\fhead3\acand3\aannot3\f
     */

    // remove old data
    cList->clear();
    stores.clear();

    // get charset and create codec
    QTextCodec *codec = NULL;
    if ( !list[ 1 ].isEmpty() && list[ 1 ].startsWith( "charset" ) )
    {
        const QStringList l = QStringList::split( "=", list[ 1 ] );
        codec = QTextCodec::codecForName( l[ 1 ] );
    }

    // get display_limit
    if ( !list[ 2 ].isEmpty() && list[ 2 ].startsWith( "display_limit" ) )
    {
        const QStringList l = QStringList::split( "=", list[ 2 ] );
        displayLimit = l[ 1 ].toInt();
    }

    for ( int i = 3; !list[ i ].isNull(); i++ )
    {
        // case list[i] = ""
        if ( list[ i ].isEmpty() )
            break;

        // split heading_label and cand_str
        QStringList l = QStringList::split( "\a", list [ i ], true );

        // store data
        CandData d;
        QString headString;
        if ( codec )
            headString = codec->toUnicode( l [ 0 ] );
        else
            headString = l [ 0 ];

        d.label = headString;

	l.pop_front();
	QString candString = l [ 0 ];

        if ( codec )
            d.str = codec->toUnicode( candString );
        else
            d.str = candString;

	l.pop_front();
	QString annotString = l [ 0 ];

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
    qDebug( "uim-candwin-qt: selectCand()" );
#endif
    const int index = list[ 1 ].toInt();
    needHilite = (list[ 2 ].toInt() == 1) ? TRUE : FALSE;
    setIndex( index );

    updateLabel();
}

void CandidateWindow::moveCand( const QStringList &list )
{
#if defined(ENABLE_DEBUG)
    qDebug( "uim-candwin-qt: moveCand()" );
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
    qDebug( "uim-candwin-qt: showCand()" );
#endif
    if ( isActive )
        show();
}
void CandidateWindow::deactivateCand()
{
#if defined(ENABLE_DEBUG)
    qDebug( "uim-candwin-qt: deactivateCand()" );
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
    cList->clear();
    stores.clear();

    // set default value
    candidateIndex = -1;
    nrCandidates = list[ 1 ].toInt();
    displayLimit = list[ 2 ].toInt();
    needHilite = false;
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
     * format: set_page_candidates\fcharset=$charset\fpage=$value\fhead1\acand1\aannot1\fhead2\acand2\aannot2\fhead3\acand3\aannot3\f
     */

    int page = 0;

    // get charset and create codec
    QTextCodec *codec = NULL;
    if ( !list[ 1 ].isEmpty() && list[ 1 ].startsWith( "charset" ) )
    {
        const QStringList l = QStringList::split( "=", list[ 1 ] );
        codec = QTextCodec::codecForName( l[ 1 ] );
    }

    // get page
    if ( !list[ 2 ].isEmpty() && list[ 2 ].startsWith( "page" ) )
    {
        const QStringList l = QStringList::split( "=", list[ 2 ] );
        page = l[ 1 ].toInt();
    }

    for ( int i = 3; !list[ i ].isNull(); i++ )
    {
        // case list[i] = ""
        if ( list[ i ].isEmpty() )
            break;

        // split heading_label and cand_str
        QStringList l = QStringList::split( "\a", list [ i ], true );

        // store data
        CandData &d = stores[page * displayLimit + i - 3];
        QString headString;
        if ( codec )
            headString = codec->toUnicode( l [ 0 ] );
        else
            headString = l [ 0 ];

        d.label = headString;

	l.pop_front();
	QString candString = l [ 0 ];

        if ( codec )
            d.str = codec->toUnicode( candString );
        else
            d.str = candString;

	l.pop_front();
	QString annotString = l [ 0 ];
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
            close( fd );
            exit( 1 );
        }
        if ( n == -1 )
            return ;
        buf[ n ] = '\0';
	read_buf = (char *)realloc( read_buf, strlen( read_buf ) + n + 1 );
	strcat( read_buf, buf );
    }

    QStringList msgList = QStringList::split( "\f\f", QString( read_buf ) );

    QStringList::Iterator it = msgList.begin();
    const QStringList::Iterator end = msgList.end();
    for ( ; it != end; ++it )
        strParse( ( *it ) );
    free( read_buf );
}

void CandidateWindow::strParse( const QString& str )
{
#if defined(ENABLE_DEBUG)
    qDebug( "str = %s", ( const char* ) str.local8Bit() );
#endif
    QStringList list = QStringList::split( "\f", str );

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

void CandidateWindow::slotCandidateSelected( QListViewItem * item )
{
    candidateIndex = ( pageIndex * displayLimit ) + cList->itemIndex( item );

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
    int width = 0;
    int height = 0;
    QListViewItem *item = cList->firstChild();
    if ( item )
        height = item->height() * ( cList->childCount() + 1 );

    // 2004-08-02 Kazuki Ohta <mover@hct.zaq.ne.jp>
    // FIXME!:
    //    There may be more proper way. Now width is adjusted by indeterminal 3 spaces.
    //    Using QWidget::adjustSize() seems not to work properly...
    unsigned int maxCharIndex = 0, maxCharCount = 0;
    for ( int i = 0; i < cList->childCount(); i++ )
    {
        if ( maxCharCount < cList->itemAtIndex( i ) ->text( 1 ).length() )
        {
            maxCharIndex = i;
            maxCharCount = cList->itemAtIndex( i ) ->text( 1 ).length();
        }
    }
    QFontMetrics fm( cList->font() );
    width = fm.width( cList->itemAtIndex( maxCharIndex ) ->text( 0 ) + "   " + cList->itemAtIndex( maxCharIndex ) ->text( 1 ) );
    if ( width < MIN_CAND_WIDTH )
        width = MIN_CAND_WIDTH;

    resize( width, height );
}

void CandidateWindow::setPage( int page )
{
    // clear items
    cList->clear();

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
    for ( int i = ncandidates - 1; i >=0 ; i-- )
    {
        QString headString = stores[ displayLimit * newpage + i ].label;
        QString candString = stores[ displayLimit * newpage + i ].str;

        // insert new item to the candidate list
        new QListViewItem( cList, headString, candString );
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
        newpage = ( int ) candidateIndex / displayLimit;
    if ( pageIndex != newpage )
        setPage( newpage );

    // select item
    if ( candidateIndex >= 0 && needHilite )
    {
        int pos = index;
        if ( displayLimit )
            pos = candidateIndex % displayLimit;

        if ( cList->itemAtIndex( pos ) && ! ( cList->itemAtIndex( pos ) ->isSelected() ) )
            cList->setSelected( cList->itemAtIndex( pos ), true );
    }
    else
    {
        cList->clearSelection();
    }

    updateLabel();
}

void CandidateWindow::updateLabel()
{
    QString indexString = QString::null;
    if ( candidateIndex >= 0 && needHilite )
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
    a.setMainWidget( &b );

    return a.exec();
}

#include "qt.moc"
