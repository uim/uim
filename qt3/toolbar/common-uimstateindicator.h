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
#ifndef UIM_QT_TOOLBAR_COMMON_UIM_STATE_INDICATOR_H
#define UIM_QT_TOOLBAR_COMMON_UIM_STATE_INDICATOR_H

#include <qhbox.h>
#include <qintdict.h>
#include <qpopupmenu.h>
#include <qtoolbutton.h>

#include <uim/uim.h>
#include <uim/uim-helper.h>

#define BUTTON_SIZE 26
#define ICON_SIZE	16

class QHelperToolbarButton;
class QHelperPopupMenu;

class UimStateIndicator : public QHBox
{
    Q_OBJECT

public:
    UimStateIndicator( QWidget *parent = 0, const char *name = 0, WFlags f = 0 );
    ~UimStateIndicator();

    int getNumButtons();

protected:
    void checkHelperConnection();

    void parseHelperStr( const QString& str );
    void propListUpdate( const QStringList& lines );

    static void helper_disconnect_cb();

signals:
    void indicatorResized();

public slots:
    void slotStdinActivated( int socket );

private slots:
    void slotPopupMenuAboutToShow();
    void slotPopupMenuAboutToHide();

protected:
    QPtrList<QHelperToolbarButton> buttons;
    bool popupMenuShowing;
};

class QHelperToolbarButton : public QToolButton
{
public:
    QHelperToolbarButton( QWidget *parent = 0, const char *name = 0 )
        : QToolButton( parent, name ){ setAutoRaise( TRUE ); }

    QSize sizeHint() const
    {
        return QSize( BUTTON_SIZE, BUTTON_SIZE );
    }
};

class QHelperPopupMenu : public QPopupMenu
{
    Q_OBJECT

public:
    QHelperPopupMenu( QWidget *parent = 0, const char *name = 0 );
    ~QHelperPopupMenu();

    int insertHelperItem( const QString &indicationIdStr,
                          const QString &menulabelStr,
                          const QString &menutooltipStr,
                          const QString &menucommandStr );

public slots:
    void slotMenuActivated( int id );

protected:
    QIntDict<QString> msgDict;
};


#endif /* Not def: UIM_QT_TOOLBAR_COMMON_UIM_STATE_INDICATOR_H */
