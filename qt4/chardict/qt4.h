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
#ifndef UIM_QT4_CHARDICT_CHARDICT_QT_H
#define UIM_QT4_CHARDICT_CHARDICT_QT_H

#include <QtCore/QtGlobal>
#if QT_VERSION < 0x050000
# include <QtGui/QWidget>
#else
# include <QtWidgets/QWidget>
#endif

class QComboBox;
class QLineEdit;
class QPushButton;
class QStackedWidget;
class QString;

class BushuViewWidget;
class UnicodeViewWidget;

class KUimCharDict : public QWidget
{
    Q_OBJECT

public:
    explicit KUimCharDict( QWidget *parent = 0 );
    ~KUimCharDict();

    enum Mode {
        BUSHU = 0,
        UNICODE = 1,
        UNKNOWN = 20
    };

protected:
    void setupWidgets();
    void setupBushuWidgets();

    void writeConfig();
    void readConfig();

    void setCharDictFont( const QFont &font );

public slots:
    void changeMode( int mode );

protected slots:
    void slotSelectFont();
    void slotCharSelected( const QString &c );

protected:
    QComboBox *m_modeCombo;
    QPushButton *m_fontselButton;
    QLineEdit *m_charLineEdit;

    QStackedWidget *m_widgetStack;
    BushuViewWidget *m_bushuView;
    UnicodeViewWidget *m_unicodeView;
};

class CharDictViewBase : public QWidget
{
    Q_OBJECT

public:
    explicit CharDictViewBase( QWidget *parent = 0 )
            : QWidget( parent ) {}

    virtual void setCharFont( const QFont &font ) = 0;

signals:
    void charSelected( const QString & );
};

#endif /* Not def: UIM_QT4_CHARDICT_CHARDICT_QT_H */
