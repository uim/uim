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

#include "standalone-qt.h"
#include "common-quimhelpertoolbar.h"
#include "common-uimstateindicator.h"

#include <qapplication.h>
#include <qpoint.h>
#include <qhbox.h>
#include <qstyle.h>
#include <qcursor.h>

#include <locale.h>

#include "uim/uim.h"
#include "qtgettext.h"

#define TOOLBAR_MARGIN_SIZE	2

UimStandaloneToolbar::UimStandaloneToolbar( QWidget *parent, const char *name )
    : QHBox( parent, name, Qt::WStyle_NoBorder | Qt::WX11BypassWM )
{
    uim_init();

    adjustSize();
    UimToolbarDraggingHandler *h = new UimToolbarDraggingHandler( this );
    h->adjustSize();
    h->show();
    QObject::connect( h, SIGNAL( handleDoubleClicked() ),
                      this, SLOT( slotToolbarDoubleClicked() ) );

    
    toolbar = new QUimHelperToolbar( this );
    toolbar->adjustSize();
    toolbar->show();
    QObject::connect( toolbar, SIGNAL( toolbarResized() ), this, SLOT( slotToolbarResized() ) );
    toolbar->setMargin(TOOLBAR_MARGIN_SIZE);

    // Move
    int panelHeight = 64; // FIXME!
    int screenwidth = QApplication::desktop() ->screenGeometry().width();
    int screenheight = QApplication::desktop() ->screenGeometry().height();
    QPoint p( screenwidth - width() - panelHeight, screenheight - height() - panelHeight );
    move( p );

    // Enable Dragging Feature
    QObject::connect( h, SIGNAL( moveTo( const QPoint & ) ),
                      this, SLOT( move( const QPoint & ) ) );

    // Quit
    QObject::connect( toolbar, SIGNAL( quitToolbar() ),
                      qApp, SLOT( quit() ) );

    show();
}
UimStandaloneToolbar::~UimStandaloneToolbar()
{
    uim_quit();
}

void
UimStandaloneToolbar::slotToolbarResized()
{
    adjustSize();
}

void
UimStandaloneToolbar::slotToolbarDoubleClicked()
{
    if (toolbar->isVisible())
      toolbar->hide();
    else
      toolbar->show();
    adjustSize();
}

UimToolbarDraggingHandler::UimToolbarDraggingHandler( QWidget *parent,
        const char* name )
        : QFrame( parent, name ),
        isDragging( false )
{
    setFrameStyle( NoFrame );

    setBackgroundMode( parent->backgroundMode() );
    setBackgroundOrigin( ParentOrigin );

    setFixedWidth( 10 );
}

void UimToolbarDraggingHandler::drawContents( QPainter* p )
{
    const QStyle::SFlags flags = QStyle::Style_Default | QStyle::Style_Horizontal;
    style().drawPrimitive( QStyle::PE_DockWindowSeparator, p,
                           contentsRect(), colorGroup(), flags );
}

QSize UimToolbarDraggingHandler::sizeHint() const
{
    int width, height;
    
    width = style().pixelMetric( QStyle::PM_DockWindowSeparatorExtent, this );
    height = BUTTON_SIZE + TOOLBAR_MARGIN_SIZE * 2;

    return QSize( width, height );
}

QSizePolicy UimToolbarDraggingHandler::sizePolicy() const
{
    return QSizePolicy( QSizePolicy::Minimum, QSizePolicy::Minimum );
}

void UimToolbarDraggingHandler::mousePressEvent( QMouseEvent * /* e */ )
{
    isDragging = true;
    grabMouse( QCursor( Qt::SizeAllCursor) );

    offsetX = QCursor::pos().x() - this->parentWidget()->x();
    offsetY = QCursor::pos().y() - this->parentWidget()->y();
}

void UimToolbarDraggingHandler::mouseReleaseEvent( QMouseEvent * /* e */ )
{
    isDragging = false;
    releaseMouse();
}

void UimToolbarDraggingHandler::mouseMoveEvent( QMouseEvent * /* e */ )
{
    if ( isDragging ) {
        QPoint pos = QCursor::pos();
        pos -= QPoint(offsetX, offsetY);
        emit moveTo( pos );
    }
}

void UimToolbarDraggingHandler::mouseDoubleClickEvent( QMouseEvent * /* e */ )
{
    isDragging = false;
    emit handleDoubleClicked();
}

int main( int argc, char *argv[] )
{
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    bind_textdomain_codeset(PACKAGE, "UTF-8"); // ensure code encoding is UTF8-
    
    QApplication a( argc, argv );
    UimStandaloneToolbar *toolbar = new UimStandaloneToolbar( 0, 0 );
    toolbar->show();
    a.setMainWidget( toolbar );

    return a.exec();
}

#include "standalone-qt.moc"
