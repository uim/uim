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
#ifndef UIM_QT4_TOOLBAR_COMMON_UIM_STATE_INDICATOR_H
#define UIM_QT4_TOOLBAR_COMMON_UIM_STATE_INDICATOR_H

#include <QtCore/QList>
#include <QtCore/QMultiHash>
#if QT_VERSION < 0x050000
# include <QtGui/QMenu>
# include <QtGui/QToolButton>
# include <QtGui/QFrame>
#else
# include <QtWidgets/QMenu>
# include <QtWidgets/QToolButton>
# include <QtWidgets/QFrame>
#endif

#include <uim/uim.h>
#include <uim/uim-helper.h>

const int BUTTON_SIZE = 26;
const int ICON_SIZE = 16;

class QHelperToolbarButton;
class QHelperPopupMenu;

class QHBoxLayout;

class UimStateIndicator : public QFrame
{
    Q_OBJECT

public:
    explicit UimStateIndicator( QWidget *parent = 0 );
    ~UimStateIndicator();

    int getNumButtons();

protected:
    void checkHelperConnection();

    void parseHelperStr( const QString& str );
    void propListUpdate( const QStringList& lines );

    static void helper_disconnect_cb();

signals:
    void indicatorResized();
    void menuRequested( QMenu *menu );

public slots:
    void slotStdinActivated();

private slots:
    void slotPopupMenuAboutToShow();
    void slotPopupMenuAboutToHide();

protected:
    QList<QHelperToolbarButton *> buttons;
    bool popupMenuShowing;

private:
    void clearButtons();

    QHBoxLayout *m_layout;
    QHash<int, QAction*> actionHash;
};

class QHelperToolbarButton : public QToolButton
{
    Q_OBJECT
public:
    explicit QHelperToolbarButton( QWidget *parent = 0 );

    QSize sizeHint() const;

signals:
    void menuRequested( QMenu *menu );

private:
    void mousePressEvent( QMouseEvent *event );
};

class QHelperPopupMenu : public QMenu
{
    Q_OBJECT

public:
    explicit QHelperPopupMenu( QWidget *parent = 0 );
    ~QHelperPopupMenu();

    QAction *insertHelperItem( const QString &indicationIdStr,
                          const QString &menulabelStr,
                          const QString &menutooltipStr,
                          const QString &menucommandStr );

public slots:
    void slotMenuActivated( QAction *action );

protected:
    QMultiHash<QAction *, QString> msgDict;
};


#endif /* Not def: UIM_QT4_TOOLBAR_COMMON_UIM_STATE_INDICATOR_H */
