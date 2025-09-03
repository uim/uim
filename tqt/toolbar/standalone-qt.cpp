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

#include <tqapplication.h>
#include <tqpoint.h>
#include <tqhbox.h>
#include <tqstyle.h>
#include <tqcursor.h>

#include <clocale>

#include "uim/uim.h"
#include "qtgettext.h"

#define TOOLBAR_MARGIN_SIZE	2

UimStandaloneToolbar::UimStandaloneToolbar( TQWidget *parent, const char *name )
    : TQHBox( parent, name, TQt::WStyle_Tool | TQt::WStyle_NoBorder | TQt::WX11BypassWM )
{
    uim_init();

    UimToolbarDraggingHandler *h = new UimToolbarDraggingHandler( this );
    TQObject::connect( h, TQ_SIGNAL( handleDoubleClicked() ),
                      this, TQ_SLOT( slotToolbarDoubleClicked() ) );


    toolbar = new QUimHelperToolbar( this );
    TQObject::connect( toolbar, TQ_SIGNAL( toolbarResized() ), this, TQ_SLOT( slotToolbarResized() ) );
    toolbar->setMargin(TOOLBAR_MARGIN_SIZE);

    // Enable Dragging Feature
    TQObject::connect( h, TQ_SIGNAL( moveTo( const TQPoint & ) ),
                      this, TQ_SLOT( move( const TQPoint & ) ) );

    // Quit
    TQObject::connect( toolbar, TQ_SIGNAL( quitToolbar() ),
                      tqApp, TQ_SLOT( quit() ) );
}
UimStandaloneToolbar::~UimStandaloneToolbar()
{
    uim_quit();
}

void
UimStandaloneToolbar::polish()
{
    TQHBox::polish();

    adjustSize();

    int screenwidth = TQApplication::desktop() ->availableGeometry().width();
    int screenheight = TQApplication::desktop() ->availableGeometry().height();
    TQPoint p( screenwidth - width(), screenheight - height() );
    move( p );
}

void
UimStandaloneToolbar::slotToolbarResized()
{
    adjustSize();
    int screenwidth =  TQApplication::desktop()->availableGeometry().width();
    if ( pos().x() + width() > screenwidth ) {
	move( screenwidth - width(), pos().y() );
    }
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

UimToolbarDraggingHandler::UimToolbarDraggingHandler( TQWidget *parent,
        const char* name )
        : TQFrame( parent, name ),
        isDragging( false )
{
    setFrameStyle( NoFrame );

    setBackgroundMode( parent->backgroundMode() );
    setBackgroundOrigin( ParentOrigin );

    setFixedWidth( 10 );
}

void UimToolbarDraggingHandler::drawContents( TQPainter* p )
{
    const TQStyle::SFlags flags = TQStyle::Style_Default | TQStyle::Style_Horizontal;
    style().drawPrimitive( TQStyle::PE_DockWindowSeparator, p,
                           contentsRect(), colorGroup(), flags );
}

TQSize UimToolbarDraggingHandler::sizeHint() const
{
    int width, height;

    width = style().pixelMetric( TQStyle::PM_DockWindowSeparatorExtent, this );
    height = BUTTON_SIZE + TOOLBAR_MARGIN_SIZE * 2;

    return TQSize( width, height );
}

TQSizePolicy UimToolbarDraggingHandler::sizePolicy() const
{
    return TQSizePolicy( TQSizePolicy::Minimum, TQSizePolicy::Minimum );
}

void UimToolbarDraggingHandler::mousePressEvent( TQMouseEvent * /* e */ )
{
    isDragging = true;
    grabMouse( TQCursor( TQt::SizeAllCursor) );

    offsetX = TQCursor::pos().x() - this->parentWidget()->x();
    offsetY = TQCursor::pos().y() - this->parentWidget()->y();
}

void UimToolbarDraggingHandler::mouseReleaseEvent( TQMouseEvent * /* e */ )
{
    isDragging = false;
    releaseMouse();
}

void UimToolbarDraggingHandler::mouseMoveEvent( TQMouseEvent * /* e */ )
{
    if ( isDragging ) {
        TQPoint pos = TQCursor::pos();
        pos -= TQPoint(offsetX, offsetY);
        emit moveTo( pos );
    }
}

void UimToolbarDraggingHandler::mouseDoubleClickEvent( TQMouseEvent * /* e */ )
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

    TQApplication a( argc, argv );
    UimStandaloneToolbar *toolbar = new UimStandaloneToolbar( 0, 0 );
    toolbar->show();
    a.setMainWidget( toolbar );

    return a.exec();
}

#include "standalone-qt.moc"
