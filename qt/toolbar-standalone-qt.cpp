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
#include "toolbar-standalone-qt.h"
#include "toolbar-common-quimhelpertoolbar.h"

#include <qapplication.h>
#include <qpoint.h>
#include <qhbox.h>
#include <qstyle.h>
#include <qcursor.h>

#include <locale.h>

#include "uim/config.h"
#include "qtgettext.h"

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
    style().drawPrimitive( QStyle::PE_Splitter, p,
                           contentsRect(), colorGroup(), flags );
}

QSize UimToolbarDraggingHandler::sizeHint() const
{
    const int dim = style().pixelMetric( QStyle::PM_DockWindowSeparatorExtent, this );
    return QSize( dim, 0 );
}

QSizePolicy UimToolbarDraggingHandler::sizePolicy() const
{
    return QSizePolicy( QSizePolicy::Minimum, QSizePolicy::Minimum );
}

void UimToolbarDraggingHandler::mousePressEvent( QMouseEvent * e )
{
    isDragging = true;
}

void UimToolbarDraggingHandler::mouseReleaseEvent( QMouseEvent * e )
{
    isDragging = false;
}

void UimToolbarDraggingHandler::mouseMoveEvent( QMouseEvent * e )
{
    if ( isDragging )
        emit moveTo( QCursor::pos() );
}

int main( int argc, char *argv[] )
{
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    bind_textdomain_codeset(PACKAGE, "UTF-8"); // ensure code encoding is UTF8-
    
    QApplication a( argc, argv );

    QHBox toolbar( 0, 0, Qt::WStyle_NoBorder | Qt::WX11BypassWM );
    toolbar.adjustSize();
    UimToolbarDraggingHandler h( &toolbar );
    h.adjustSize();
    QUimHelperToolbar b( &toolbar );
    b.adjustSize();

    a.setMainWidget( &toolbar );

    // Move : FIXME!
    int panelHeight = 64; /* FIXME! */
    int screenwidth = QApplication::desktop() ->screenGeometry().width();
    int screenheight = QApplication::desktop() ->screenGeometry().height();
    QPoint p( screenwidth - toolbar.width() - panelHeight, screenheight - toolbar.height() - panelHeight );
    toolbar.move( p );

    // Enable Dragging Feature
    QObject::connect( &h, SIGNAL( moveTo( const QPoint & ) ),
                      &toolbar, SLOT( move( const QPoint & ) ) );

    // Show
    toolbar.show();
    return a.exec();
}

#include "toolbar-standalone-qt.moc"
