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
#ifndef UIM_QT4_TOOLBAR_COMMON_QUIMHELPERTOOLBAR_H
#define UIM_QT4_TOOLBAR_COMMON_QUIMHELPERTOOLBAR_H

#include <QtGui/QPixmap>
#if QT_VERSION < 0x050000
# include <QtGui/QFrame>
#else
# include <QtWidgets/QFrame>
#endif

class UimStateIndicator;

class QContextMenuEvent;
class QHBoxLayout;
class QMenu;

class QUimHelperToolbar : public QFrame
{
    Q_OBJECT

public:
    explicit QUimHelperToolbar( QWidget *parent = 0, bool isApplet = false );
    ~QUimHelperToolbar();

    void setMargin(int margin);

public slots:    
    void slotExecPref();
    QMenu *contextMenu();

protected:
    // right click
    virtual void contextMenuEvent ( QContextMenuEvent * e );

protected:
    void addExecImSwitcherButton();
    void addExecPrefButton();
    void addExecDictButton();
    void addExecInputPadButton();
    void addExecHandwritingInputPadButton();
    void addExecHelpButton();    

    int getNumButtons();
protected slots:
    void slotExecImSwitcher();
    void slotExecDict();
    void slotExecInputPad();
    void slotExecHandwritingInputPad();
    void slotExecHelp();    

    void slotIndicatorResized();
signals:
    void quitToolbar();
    void toolbarResized();
    void menuRequested( QMenu *menu );

protected:
    UimStateIndicator *m_indicator;
    QPixmap m_swicon;
    QPixmap m_preficon;
    QPixmap m_dicticon;
    QPixmap m_padicon;
    QPixmap m_handicon;
    QPixmap m_helpicon;
    QMenu *m_contextMenu;
    int m_nr_exec_buttons;

private:
    QHBoxLayout *m_layout;
};


#endif
