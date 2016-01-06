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
#ifndef UIM_QT4_IM_SWITCHER_QT_H
#define UIM_QT4_IM_SWITCHER_QT_H

#include <uim/uim.h>
#include <uim/uim-helper.h>

#include <QtGui/QFocusEvent>
#if QT_VERSION < 0x050000
# include <QtGui/QDialog>
#else
# include <QtWidgets/QDialog>
#endif

class QFocusEvent;
class QPushButton;
class QRadioButton;
class QTableWidget;

class UimImSwitcher : public QDialog
{
    Q_OBJECT

public:
    explicit UimImSwitcher( QWidget *parent = 0 );
    ~UimImSwitcher();

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
    void slotStdinActivated();
    void slotChangeInputMethodAndQuit();
    void slotChangeInputMethod();

protected:
    QTableWidget *listview;
    QRadioButton *wholeButton;
    QRadioButton *applicationButton;
    QRadioButton *textButton;
};

#endif /* Not def: UIM_QT_IM_SWITCHER_QT_H */
