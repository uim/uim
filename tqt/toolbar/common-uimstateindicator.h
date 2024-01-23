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
#ifndef UIM_TQT_TOOLBAR_COMMON_UIM_STATE_INDICATOR_H
#define UIM_TQT_TOOLBAR_COMMON_UIM_STATE_INDICATOR_H

#include <tqhbox.h>
#include <tqintdict.h>
#include <tqpopupmenu.h>
#include <tqtoolbutton.h>

#include <uim/uim.h>
#include <uim/uim-helper.h>

#define BUTTON_SIZE 26
#define ICON_SIZE	16

class QHelperToolbarButton;
class QHelperPopupMenu;

class UimStateIndicator : public TQHBox
{
    TQ_OBJECT

public:
    UimStateIndicator( TQWidget *parent = 0, const char *name = 0, WFlags f = 0 );
    ~UimStateIndicator();

    int getNumButtons();

protected:
    void checkHelperConnection();

    void parseHelperStr( const TQString& str );
    void propListUpdate( const TQStringList& lines );

    static void helper_disconnect_cb();

signals:
    void indicatorResized();

public slots:
    void slotStdinActivated( int socket );

private slots:
    void slotPopupMenuAboutToShow();
    void slotPopupMenuAboutToHide();

protected:
    TQPtrList<QHelperToolbarButton> buttons;
    bool popupMenuShowing;
};

class QHelperToolbarButton : public TQToolButton
{
public:
    QHelperToolbarButton( TQWidget *parent = 0, const char *name = 0 )
        : TQToolButton( parent, name ){ setAutoRaise( true ); }

    TQSize sizeHint() const
    {
        return TQSize( BUTTON_SIZE, BUTTON_SIZE );
    }
};

class QHelperPopupMenu : public TQPopupMenu
{
    TQ_OBJECT

public:
    QHelperPopupMenu( TQWidget *parent = 0, const char *name = 0 );
    ~QHelperPopupMenu();

    int insertHelperItem( const TQString &indicationIdStr,
                          const TQString &menulabelStr,
                          const TQString &menutooltipStr,
                          const TQString &menucommandStr );

public slots:
    void slotMenuActivated( int id );

protected:
    TQIntDict<TQString> msgDict;
};


#endif /* Not def: UIM_TQT_TOOLBAR_COMMON_UIM_STATE_INDICATOR_H */
