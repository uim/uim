/*

 Copyright (c) 2003-2009 uim Project http://code.google.com/p/uim/

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
#ifndef UIM_QT4_STANDALONE_QT4_H
#define UIM_QT4_STANDALONE_QT4_H

#include <QtGui/QFrame>
#include <QtGui/QMouseEvent>

class QUimHelperToolbar;

class QPoint;
class QMouseEvent;

class UimStandaloneToolbar : public QFrame
{
    Q_OBJECT

public:
    UimStandaloneToolbar( QWidget *paret = 0 );
    ~UimStandaloneToolbar();

protected slots:
    void slotToolbarResized();
    void slotToolbarDoubleClicked();

private slots:
    void moveTo(const QPoint &point);

private:
    QUimHelperToolbar *toolbar;
};

class UimToolbarDraggingHandler : public QFrame
{
    Q_OBJECT
    
public:
    explicit UimToolbarDraggingHandler( QWidget *parent );

    QSize sizeHint() const;
    QSizePolicy sizePolicy() const;

signals:
    void moveTo( const QPoint & );
    void handleDoubleClicked();

protected:
    void drawContents( QPainter* );

    void mousePressEvent ( QMouseEvent * e );
    void mouseReleaseEvent ( QMouseEvent * e );
    void mouseMoveEvent ( QMouseEvent * e );
    void mouseDoubleClickEvent ( QMouseEvent * e );

private:
    bool isDragging;
    int offsetX;
    int offsetY;
};

#endif  /* UIM_QT4_STANDALONE_QT4_H */
