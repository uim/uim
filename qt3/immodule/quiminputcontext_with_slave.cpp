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

#include <qinputcontextfactory.h>

#include "uim/uim.h"

#include "quiminputcontext.h"
#include "quiminputcontext_with_slave.h"

QUimInputContextWithSlave::QUimInputContextWithSlave( const char *imname, const char *lang )
        : QUimInputContext( imname, lang )
{
    slave = QInputContextFactory::create( "simple", NULL );
    if ( slave )
    {
        insertChild( slave );

        QObject::connect( slave, SIGNAL( imEventGenerated( QObject *, QIMEvent * ) ),
                          this, SIGNAL( imEventGenerated( QObject *, QIMEvent * ) ) );
        QObject::connect( slave, SIGNAL( deletionRequested() ),
                          this, SLOT( destroyInputContext() ) );
    }
}

QUimInputContextWithSlave::~QUimInputContextWithSlave()
{
}

void QUimInputContextWithSlave::setFocus()
{
    QUimInputContext::setFocus();

    if ( slave )
    {
        slave->setFocus();
        slave->setFocusWidget( focusWidget() );
    }
}

void QUimInputContextWithSlave::unsetFocus()
{
    QUimInputContext::unsetFocus();

    if ( slave )
        slave->unsetFocus();
}

#if defined(Q_WS_X11)
void QUimInputContextWithSlave::setFocusWidget( QWidget *w )
{
    QUimInputContext::setFocusWidget( w );

    if ( slave )
        slave->setFocusWidget( w );
}

void QUimInputContextWithSlave::setHolderWidget( QWidget *w )
{
    QUimInputContext::setHolderWidget( w );

    if ( slave )
        slave->setHolderWidget( w );
}
#endif

bool QUimInputContextWithSlave::filterEvent( const QEvent *event )
{
    if ( ! QUimInputContext::filterEvent( event ) ) {
      if ( ! isComposing() && slave && slave->filterEvent( event ) )
        return true;
      else
        return false;
    }

    return true;
}

void QUimInputContextWithSlave::destroyInputContext()
{
    if ( slave )
    {
        // slave->reset() may not properly work in the case, so we
        // manually resets the composing state of text widget
        if ( slave->focusWidget() )
        {
            emit imEventGenerated( slave->focusWidget(), new QIMEvent( QEvent::IMEnd, QString::null, -1 ) );
        }
        slave->deleteLater();
        slave = 0;
    }
}

#include "quiminputcontext_with_slave.moc"
