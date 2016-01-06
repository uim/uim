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
#ifndef UIM_QT_IM_SWITCHER_QT_H
#define UIM_QT_IM_SWITCHER_QT_H

#include <uim/uim.h>
#include <uim/uim-helper.h>

#include <qdialog.h>

class QListView;
class QPushButton;
class QButtonGroup;
class QFocusEvent;

class UimImSwitcher : public QDialog
{
    Q_OBJECT

public:
    UimImSwitcher( QWidget *parent = 0, const char *name = 0 );
    ~UimImSwitcher();

    enum ID_TYPE {
        ID_CHANGE_WHOLE_DESKTOP,
        ID_CHANGE_THIS_APPLICATION_ONLY,
        ID_CHANGE_THIS_TEXT_AREA_ONLY,
        ID_NONE
    };

protected:
    void createGUI();

    void sendMessageImChange( const QString &change_type );
    void saveDefaultIm();
    QString selectedImName() const;

    void checkHelperConnection();
    static void helper_disconnect_cb();

    void parseHelperStrImList( const QString &message );

    void reloadImList();

protected slots:
    void slotStdinActivated( int socket );
    void slotChangeInputMethod();

protected:
    QListView *listview;
    QButtonGroup *vbGroup;
    QPushButton *okButton;
    QPushButton *cancelButton;
};

#endif /* Not def: UIM_QT_IM_SWITCHER_QT_H */
