/*

 Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

 THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 SUCH DAMAGE.

*/
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
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "uim/config.h"

#include "qtgettext.h"
#include "candwin-qt.h"

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
    cList->setSorting( 0 );
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
    qDebug( "uim-helper-candwin-qt: activateCand()" );

    /**
     * format: activate\ncharset=$charset\ndisplay_limit=$value\nhead1\tcand1\nhead2\tcand2\nhead3\tcand3\n
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
        QStringList l = QStringList::split( "\t", list [ i ], true );

        // store data
        CandData d;
        QString headString;
        if ( codec )
            headString = codec->toUnicode( l [ 0 ] );
        else
            headString = l [ 0 ];

        if ( ( headString.toInt() < 10 && headString.toInt() + displayLimit > 10 )
                || ( headString.toInt() < 100 && headString.toInt() + displayLimit > 100 ) )
            headString.prepend( "0" );

        d.label = headString;

	// XXX Current prime (0.4.6) may return candidate string
	// containing "\t", and we can't handle annotation in another
	// window yet.
	l.pop_front();
	QString candString = l.join( "\t" );

        if ( codec )
            d.str = codec->toUnicode( candString );
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
    qDebug( "uim-helper-candwin-qt: selectCand()" );

    const int index = list[ 1 ].toInt();
    setIndex( index );

    updateLabel();
}

void CandidateWindow::moveCand( const QStringList &list )
{
    qDebug( "uim-helper-candwin-qt: moveCand()" );

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
    qDebug( "uim-helper-candwin-qt: showCand()" );

    if ( isActive )
        show();
}
void CandidateWindow::deactivateCand()
{
    qDebug( "uim-helper-candwin-qt: deactivateCand()" );

    hide();
    isActive = false;
}
void CandidateWindow::slotStdinActivated( int fd )
{
    char buf[ 1024 ];
    int n;
    n = read( fd, buf, 1024 - 1 );
    if ( n == 0 )
    {
        close( fd );
        exit( -1 );
    }
    if ( n == -1 )
        return ;
    buf[ n ] = '\0';

    QStringList msgList = QStringList::split( "\n\n", QString( buf ) );

    QStringList::Iterator it = msgList.begin();
    const QStringList::Iterator end = msgList.end();
    for ( ; it != end; ++it )
        strParse( ( *it ) );
}

void CandidateWindow::strParse( const QString& str )
{
    qDebug( "str = %s", ( const char* ) str.local8Bit() );

    QStringList list = QStringList::split( "\n", str );

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
    qDebug( "adjustCandidateWindowSize()" );

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
    lastpage = nrCandidates / displayLimit;
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
            newindex = newpage * displayLimit;
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
    for ( int i = 0; i < ncandidates; i++ )
    {
        QString headString = stores[ displayLimit * newpage + i ].label;
        QString candString = stores[ displayLimit * newpage + i ].str;

        // insert new item to the candidate list
        new QListViewItem( cList, headString, candString );
    }

    // set index
    if ( newindex != candidateIndex )
        setIndex( newindex );

    // set candwin size
    adjustCandidateWindowSize();
}

void CandidateWindow::setIndex( int index )
{
    qDebug( "setIndex : index = %d", index );

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
    if ( candidateIndex >= 0 )
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
    if ( candidateIndex >= 0 )
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

#include "candwin-qt.moc"
