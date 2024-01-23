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

#include <tqlabel.h>
#include <tqtextbrowser.h>
#include <tqtimer.h>
#include <tqapplication.h>
#include <tqrect.h>
#include <tqpoint.h>

#include "subwindow.h"

const TQt::WFlags subwindowFlag = ( TQt::WType_TopLevel
                                   | TQt::WStyle_Customize
                                   | TQt::WStyle_StaysOnTop
                                   | TQt::WStyle_NoBorder
                                   | TQt::WStyle_Tool
#if defined(TQ_WS_X11)
                                   | TQt::WX11BypassWM
#endif
                                 );

static const int TIMER_INTERVAL = 1000; // 1000ms = 1second

SubWindow::SubWindow( TQWidget *parent, const char *name )
        : TQVBox( parent, name, subwindowFlag )
{
    m_titleLabel = new TQLabel( this );
    m_titleLabel->setAlignment( TQt::AlignHCenter );
    m_titleLabel->setPaletteBackgroundColor( TQt::darkGray );
    m_titleLabel->setPaletteForegroundColor( TQt::white );

    m_contentsEdit = new TQTextBrowser( this );

    m_hookTimer = new TQTimer( this );
    connect( m_hookTimer, TQ_SIGNAL( timeout() ), this, TQ_SLOT( timerDone() ) );

    hide();
}

SubWindow::~SubWindow()
{}

void SubWindow::hookPopup( const TQString &title, const TQString contents )
{
    // stop now running timer
    if ( m_hookTimer->isActive() )
        m_hookTimer->stop();

    m_titleLabel->setText( title );
    m_contentsEdit->setText( contents );

    m_hookTimer->start( TIMER_INTERVAL, true );
}

void SubWindow::popup()
{
    raise();
    show();
}

void SubWindow::cancelHook()
{
    m_hookTimer->stop();
    hide();
}

void SubWindow::timerDone()
{
    popup();
}

void SubWindow::layoutWindow( int x, int y )
{
    TQRect focusRect = TQRect( TQPoint( x, y ), frameSize() );
    TQRect screenRect = TQRect( 0, 0,
                              TQApplication::desktop() ->screenGeometry().width(),
                              TQApplication::desktop() ->screenGeometry().height() );

    TQPoint p = forceInside( screenRect, focusRect );
    move( p );
}

TQPoint SubWindow::forceInside( const TQRect &enclosure, const TQRect &prisoner )
{
    int new_x, new_y;

    new_x = TQMIN( enclosure.right(), prisoner.right() ) - prisoner.width() + 1;
    new_x = TQMAX( enclosure.left(), new_x );
    new_y = TQMIN( enclosure.bottom(), prisoner.bottom() ) - prisoner.height() + 1;
    new_y = TQMAX( enclosure.top(), new_y );

    return TQPoint( new_x, new_y );
}

#include "subwindow.moc"
