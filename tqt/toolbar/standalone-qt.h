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
#ifndef UIM_TQT_TOOLBAR_DRAGGING_HANDLER_H
#define UIM_TQT_TOOLBAR_DRAGGING_HANDLER_H

#include <tqframe.h>
#include <tqevent.h>
#include <tqpoint.h>
#include <tqhbox.h>

class QUimHelperToolbar;
class UimStandaloneToolbar : public TQHBox
{
    TQ_OBJECT

public:
    UimStandaloneToolbar( TQWidget *paret = 0, const char *name = 0 );
    ~UimStandaloneToolbar();

public slots:
    virtual void polish();

protected slots:
    void slotToolbarResized();
    void slotToolbarDoubleClicked();

private:
    QUimHelperToolbar *toolbar;
};

class UimToolbarDraggingHandler : public TQFrame
{
    TQ_OBJECT

public:
    UimToolbarDraggingHandler( TQWidget *parent, const char *name = 0 );

    TQSize sizeHint() const;
    TQSizePolicy sizePolicy() const;

signals:
    void moveTo( const TQPoint & );
    void handleDoubleClicked();

protected:
    void drawContents( TQPainter* );

    void mousePressEvent ( TQMouseEvent * e );
    void mouseReleaseEvent ( TQMouseEvent * e );
    void mouseMoveEvent ( TQMouseEvent * e );
    void mouseDoubleClickEvent ( TQMouseEvent * e );

private:
    bool isDragging;
    int offsetX;
    int offsetY;
};

#endif  /* UIM_TQT_TOOLBAR_DRAGGING_HANDLER_H */
