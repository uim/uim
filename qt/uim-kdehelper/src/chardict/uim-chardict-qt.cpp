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
#include <qlayout.h>
#include <qlabel.h>
#include <qhbox.h>
#include <qframe.h>
#include <qdrawutil.h>
#include <qcolor.h>
#include <qtextedit.h>
#include <qsizepolicy.h>
#include <qfile.h>
#include <qtextstream.h>
#include <qheader.h>
#include <qscrollbar.h>

#include <klocale.h>

#include "uim-chardict-qt.h"

static const int COLS = 10;

int main( int argc, char *argv[] )
{
    QApplication a( argc, argv );

    KUimCharDict cdict;
    cdict.show();

    a.setMainWidget( &cdict );

    qDebug("dict = %s\n", (const char*)QString(BUSHUDICT));

    return a.exec();
}

KUimCharDict::KUimCharDict( QWidget *parent, const char *name )
    : QWidget( parent, name )
{
    setupWidgets();
}

KUimCharDict::~KUimCharDict()
{

}

void KUimCharDict::setupWidgets()
{
    QVBoxLayout *layout = new QVBoxLayout( this );
    layout->setMargin( 4 );
    layout->setSpacing( 6 );

    QWidget *upperHWidget = new QWidget( this );
    upperHWidget->setSizePolicy( QSizePolicy::Minimum, QSizePolicy::Maximum );

    QLabel *modeLabel = new QLabel( i18n("Mode:"), upperHWidget );
    m_modeCombo = new QComboBox( upperHWidget );
    m_modeCombo->insertItem( i18n("Bushu Search") );
    QObject::connect( m_modeCombo, SIGNAL(activated(int)),
                      this, SLOT(changeMode(int)) );
    modeLabel->setBuddy( m_modeCombo );

    QLabel *charLabel = new QLabel( i18n("Chars:"), upperHWidget );
    m_charLineEdit = new QLineEdit( upperHWidget );
    charLabel->setBuddy( m_charLineEdit );
    
    QHBoxLayout *upperHLayout = new QHBoxLayout( upperHWidget );
    upperHLayout->setSpacing( 4 );
    upperHLayout->addWidget( modeLabel );
    upperHLayout->addWidget( m_modeCombo );
    upperHLayout->addWidget( charLabel );
    upperHLayout->addWidget( m_charLineEdit );

    m_widgetStack = new QWidgetStack( this );
    m_widgetStack->setSizePolicy( QSizePolicy::Minimum, QSizePolicy::Minimum );

    m_bushuView = new BushuViewWidget( this );
    QObject::connect( m_bushuView, SIGNAL(charSelected(const QString &)),
                      this, SLOT(slotCharSelected(const QString &)) );
    m_bushuView->hide();
    m_widgetStack->addWidget( m_bushuView, BUSHU );
    m_widgetStack->raiseWidget( BUSHU );

    layout->addWidget( upperHWidget );
    layout->addWidget( m_widgetStack );
}

void KUimCharDict::changeMode( int mode )
{
    // not implemented yet
    switch (mode) {
    case BUSHU:
        
        break;
    }
}

void KUimCharDict::slotCharSelected( const QString &c )
{
    m_charLineEdit->setText( m_charLineEdit->text() + c );
}


//----------------
BushuViewWidget::BushuViewWidget( QWidget *parent, const char *name )
    : QWidget( parent, name )    
{
    setupWidgets();
    readDict();
}

BushuViewWidget::~BushuViewWidget()
{
    
}

void BushuViewWidget::setupWidgets()
{
    QHBoxLayout* layout = new QHBoxLayout( this );

    m_mainSplitter = new QSplitter( this );

    m_bushuListView = new QListView( m_mainSplitter );
    m_bushuListView->setSorting( 0 );
    m_bushuListView->setSelectionMode( QListView::Single );
    m_bushuListView->addColumn( "1" );
    m_bushuListView->setColumnWidthMode( 1, QListView::Maximum );
    m_bushuListView->header() ->hide();
    m_bushuListView->setHScrollBarMode( QScrollView::AlwaysOff );
    m_bushuListView->setAllColumnsShowFocus( true );
    QObject::connect( m_bushuListView, SIGNAL(clicked(QListViewItem *)),
                      this, SLOT(slotBushuSelected(QListViewItem *)) );

    m_charView = new CharView( COLS, 0, m_mainSplitter );
    QObject::connect( m_charView, SIGNAL(charSelected(const QString &)),
                      this, SIGNAL(charSelected(const QString &)) );

    layout->addWidget( m_mainSplitter );
}

void BushuViewWidget::readDict()
{
    QFile file( BUSHUDICT );
    if ( file.open( IO_ReadOnly ) ) {
        QTextStream stream( &file );
        QString line;
        while ( !stream.atEnd() ) {
            QString bushuName = QStringList::split( " ", stream.readLine() )[0];
            new QListViewItem( m_bushuListView, bushuName );
        }
        file.close();
    }
}

void BushuViewWidget::slotBushuSelected(QListViewItem *item)
{
    if( !item )
        return;
    
    QString selectedBushuName = item->text( 0 );
    if( selectedBushuName.isEmpty() )
        return;
    
    QFile file( BUSHUDICT );
    if ( file.open( IO_ReadOnly ) ) {
        QTextStream stream( &file );
        QString line;
        
        // search selected bushu line by line
        while ( !stream.atEnd() ) {
            QStringList chars = QStringList::split( " ", stream.readLine() );
            QString bushuName = chars[0];
            if( selectedBushuName == bushuName )
            {
                // Display Characters
                chars.remove( bushuName );
                m_charView->setCharacters( chars );
            }
        }
        file.close();
    }
}

//-------------------
CharView::CharView( int x, int y, QWidget *parent, const char *name )
    : QGridView( parent, name )
{
    setNumCols( x );
    setNumRows( y );
    setCellWidth( 30 );
    setCellHeight( 30 );

    setHScrollBarMode( QScrollView::AlwaysOff );

    m_activeCell.setX(-1);
    m_activeCell.setY(-1);

    setFrameStyle( QFrame::NoFrame );
    show();
}
CharView::~CharView()
{
    
}
void CharView::paintCell(QPainter * painter, int y, int x)
{    
    bool isActiveCell = ((m_activeCell.x() == x) && (m_activeCell.y() == y));
    if( isActiveCell ) {
        // save the painter's state to the stack and swap back/fore ground colors
        painter->save();
        QColor tmp( painter->pen().color() );
        painter->setPen( painter->backgroundColor() );
        painter->setBackgroundColor( tmp );
    }

    QBrush bBrush( colorGroup().base() );
    QBrush tBrush( colorGroup().text() );
    if( isActiveCell )
        qDrawShadePanel(painter, 0, 0, cellWidth(), cellHeight(), colorGroup(), false, 2, &tBrush);//&(painter->brush()));
    else
        qDrawPlainRect(painter, 0, 0, cellWidth(), cellHeight(), colorGroup().foreground(), 1, &bBrush);//&(painter->brush()));

    QString c = coordsToChar( x, y );
    if( !c.isEmpty() )
        painter->drawText(0, 0, cellWidth(), cellHeight(), AlignCenter, c);

    // restore state
    if( isActiveCell )
        painter->restore();
}

void CharView::contentsMousePressEvent(QMouseEvent * e)
{
    if (e->button() != LeftButton)
        return;

    int y = e->pos().y();
    int x = e->pos().x();

    int row = rowAt( y );
    int col = columnAt( x );

    qDebug("(row, col) = (%d, %d)", row, col);
    
    if ((m_activeCell.y() != row) || (m_activeCell.x() != col)) {
        int oldactivecelly = m_activeCell.y();
        int oldactivecellx = m_activeCell.x();
        m_activeCell.setY(row);
        m_activeCell.setX(col);
        updateCell(oldactivecelly, oldactivecellx);
        updateCell(m_activeCell.y(), m_activeCell.x());
    }
}

void CharView::contentsMouseReleaseEvent(QMouseEvent * e)
{
    if (e->button() != LeftButton)
        return;

    emit charSelected(coordsToChar(m_activeCell.x(), m_activeCell.y()));
}
QString CharView::coordsToChar( int x, int y )
{
    if (x < 0 || x > numCols() || y < 0 || y > numRows())
        debug("coordsToIndex: invalid coords(%d, %d)\n", x, y);

    unsigned int index = y * numCols() + x;
    if ( index < m_charList.count() )
        return m_charList[index];
    else    
        return QString::null;
}

QSize CharView::sizeHint(void) const
{
    return QSize(numCols() * cellWidth(), numRows() * cellHeight());
}

void CharView::resizeEvent(QResizeEvent * e)
{
    updateCharView();
}

void CharView::setCharacters( const QStringList &charList )
{
    // default position
    m_activeCell.setX( -1 );
    m_activeCell.setY( -1 );
    
    m_charList.clear();
    m_charList = charList;
    
    int total = m_charList.count();
    
    int cols = COLS;
    int rows = (total - total%COLS)/COLS + 1;
    setNumCols( cols );
    setNumRows( rows );

    updateCharView();

    // repaint all cells
    for( int i = 0; i < numRows(); i++ )
        for( int j = 0; j < numCols(); j++ )
            repaintCell( i, j, true );
}
void CharView::updateCharView()
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
    cellsize = (width() - vScrollBar->minimumWidth())/numCols();

    setCellWidth(cellsize);
    setCellHeight(cellsize);    
}
