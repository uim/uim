/*

  Copyright (c) 2012 uim Project http://code.google.com/p/uim/

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
#include "quimplatforminputcontext.h"

QUimPlatformInputContext *focusedInputContext = 0;
bool disableFocusedContext = false;

QList<QUimPlatformInputContext*> contextList;

#include <QtCore/QRectF>
#include <QtCore/QLocale>

#define ENABLE_DEBUG

QUimPlatformInputContext::QUimPlatformInputContext( const char *imname )
{
#ifdef ENABLE_DEBUG
    qDebug( "QUimPlatformInputContext()" );
#endif
    Q_UNUSED(imname)
}

QUimPlatformInputContext::~QUimPlatformInputContext()
{
#ifdef ENABLE_DEBUG
    qDebug( "~QUimPlatformInputContext()" );
#endif
}

void QUimPlatformInputContext::commit()
{
#ifdef ENABLE_DEBUG
    qDebug( "commit()" );
#endif
}

bool QUimPlatformInputContext::filterEvent(const QEvent *event)
{
#ifdef ENABLE_DEBUG
    qDebug( "filterEvent()" );
#endif
    Q_UNUSED(event)
    return false;
}

void QUimPlatformInputContext::hideInputPanel()
{
#ifdef ENABLE_DEBUG
    qDebug( "hideInputPanel()" );
#endif
}

Qt::LayoutDirection QUimPlatformInputContext::inputDirection() const
{
#ifdef ENABLE_DEBUG
    qDebug( "inputDirection()" );
#endif
    return Qt::LayoutDirectionAuto;
}

void QUimPlatformInputContext::invokeAction(QInputMethod::Action action,
    int cursorPosition)
{
#ifdef ENABLE_DEBUG
    qDebug( "invokeAction()" );
#endif
    Q_UNUSED(action)
    Q_UNUSED(cursorPosition)
}

bool QUimPlatformInputContext::isAnimating() const
{
#ifdef ENABLE_DEBUG
    qDebug( "isAnimating()" );
#endif
    return false;
}

bool QUimPlatformInputContext::isInputPanelVisible() const
{
#ifdef ENABLE_DEBUG
    qDebug( "isInputPanelVisible()" );
#endif
    return false;
}

bool QUimPlatformInputContext::isValid() const
{
#ifdef ENABLE_DEBUG
    qDebug( "isValid()" );
#endif
    return true;
}

QRectF QUimPlatformInputContext::keyboardRect() const
{
#ifdef ENABLE_DEBUG
    qDebug( "keyboardRect()" );
#endif
    return QRectF();
}

QLocale QUimPlatformInputContext::locale() const
{
#ifdef ENABLE_DEBUG
    qDebug( "locale()" );
#endif
    return QLocale();
}

void QUimPlatformInputContext::reset()
{
#ifdef ENABLE_DEBUG
    qDebug( "reset()" );
#endif
}

void QUimPlatformInputContext::showInputPanel()
{
#ifdef ENABLE_DEBUG
    qDebug( "showInputPanel()" );
#endif
}

void QUimPlatformInputContext::update(Qt::InputMethodQueries)
{
#ifdef ENABLE_DEBUG
    qDebug( "update()" );
#endif
}
