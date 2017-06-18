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

#include <qlabel.h>
#include <qtextbrowser.h>
#include <qtimer.h>
#include <qapplication.h>
#include <qrect.h>
#include <qpoint.h>

#include "subwindow.h"

const Qt::WFlags subwindowFlag = ( Qt::WType_TopLevel
                                   | Qt::WStyle_Customize
                                   | Qt::WStyle_StaysOnTop
                                   | Qt::WStyle_NoBorder
                                   | Qt::WStyle_Tool
#if defined(Q_WS_X11)
                                   | Qt::WX11BypassWM
#endif
                                 );

static const int TIMER_INTERVAL = 1000; // 1000ms = 1second

SubWindow::SubWindow( QWidget *parent, const char *name )
        : QVBox( parent, name, subwindowFlag )
{
    m_titleLabel = new QLabel( this );
    m_titleLabel->setAlignment( Qt::AlignHCenter );
    m_titleLabel->setPaletteBackgroundColor( Qt::darkGray );
    m_titleLabel->setPaletteForegroundColor( Qt::white );

    m_contentsEdit = new QTextBrowser( this );

    m_hookTimer = new QTimer( this );
    connect( m_hookTimer, SIGNAL( timeout() ), this, SLOT( timerDone() ) );

    hide();
}

SubWindow::~SubWindow()
{}

void SubWindow::hookPopup( const QString &title, const QString contents )
{
    // stop now running timer
    if ( m_hookTimer->isActive() )
        m_hookTimer->stop();

    m_titleLabel->setText( title );
    m_contentsEdit->setText( contents );

    m_hookTimer->start( TIMER_INTERVAL, TRUE );
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
    QRect focusRect = QRect( QPoint( x, y ), frameSize() );
    QRect screenRect = QRect( 0, 0,
                              QApplication::desktop() ->screenGeometry().width(),
                              QApplication::desktop() ->screenGeometry().height() );

    QPoint p = forceInside( screenRect, focusRect );
    move( p );
}

QPoint SubWindow::forceInside( const QRect &enclosure, const QRect &prisoner )
{
    int new_x, new_y;

    new_x = QMIN( enclosure.right(), prisoner.right() ) - prisoner.width() + 1;
    new_x = QMAX( enclosure.left(), new_x );
    new_y = QMIN( enclosure.bottom(), prisoner.bottom() ) - prisoner.height() + 1;
    new_y = QMAX( enclosure.top(), new_y );

    return QPoint( new_x, new_y );
}

#include "subwindow.moc"
