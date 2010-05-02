/*

Copyright (c) 2003-2010 uim Project http://code.google.com/p/uim/

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
#include "subwindow.h"

#include <QtCore/QPoint>
#include <QtCore/QRect>
#include <QtGui/QApplication>
#include <QtGui/QDesktopWidget>
#include <QtGui/QTextBrowser>
#include <QtGui/QVBoxLayout>


const Qt::WFlags subwindowFlag = ( Qt::Window
                                   | Qt::WindowStaysOnTopHint
                                   | Qt::FramelessWindowHint
                                   | Qt::Tool
#if defined(Q_WS_X11)
                                   | Qt::X11BypassWindowManagerHint
#endif
                                 );

static const int TIMER_INTERVAL = 1000; // 1000ms = 1second

SubWindow::SubWindow( QWidget *parent )
        : QFrame( parent, subwindowFlag )
{
    m_contentsEdit = new QTextBrowser( this );

    m_hookTimer = new QTimer( this );
    connect( m_hookTimer, SIGNAL( timeout() ), this, SLOT( timerDone() ) );

    QVBoxLayout *layout = new QVBoxLayout;
    layout->setMargin( 0 );
    layout->addWidget( m_contentsEdit );
    setLayout( layout );

    hide();
}

SubWindow::~SubWindow()
{}

void SubWindow::hookPopup( const QString &contents )
{
    // stop now running timer
    if ( m_hookTimer->isActive() )
        m_hookTimer->stop();

    m_contentsEdit->setText( contents );

    m_hookTimer->setSingleShot( true );
    m_hookTimer->start( TIMER_INTERVAL );
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

void SubWindow::layoutWindow( const QRect &rect )
{
    const QRect screenRect = QApplication::desktop()->screenGeometry();

    const int w = width();
    const int candX = rect.x();
    int destX = candX + rect.width();
    if ( destX + w > screenRect.width() )
        destX = candX - w;

    const int h = height();
    const int screenH = screenRect.height();
    int destY = rect.y();
    if ( destY + h > screenH )
        destY = screenH - h;

    move( destX, destY );
}
