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
#ifndef UIM_TQT_CHARDICT_CHARDICT_QT_H
#define UIM_TQT_CHARDICT_CHARDICT_QT_H

#include <tqstring.h>
#include <tqwidget.h>
#include <tqcombobox.h>
#include <tqlineedit.h>
#include <tqwidgetstack.h>
#include <tqscrollview.h>
#include <tqgridview.h>
#include <tqstringlist.h>
#include <tqpushbutton.h>

class BushuViewWidget;
class UnicodeViewWidget;

class KUimCharDict : public TQWidget
{
    TQ_OBJECT

public:
    KUimCharDict( TQWidget *parent = 0, const char *name = 0 );
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

    void setCharDictFont( const TQFont &font );

public slots:
    void changeMode( int mode );

protected slots:
    void slotSelectFont();
    void slotCharSelected( const TQString &c );

protected:
    TQComboBox *m_modeCombo;
    TQPushButton *m_fontselButton;
    TQLineEdit *m_charLineEdit;

    TQWidgetStack *m_widgetStack;
    BushuViewWidget *m_bushuView;
    UnicodeViewWidget *m_unicodeView;
};

class CharDictViewBase : public TQWidget
{
    TQ_OBJECT

public:
    CharDictViewBase( TQWidget *parent = 0, const char *name = 0 )
            : TQWidget( parent, name ) {}

    virtual void setFont( const TQFont &font ) = 0;

signals:
    void charSelected( const TQString & );
};

#endif /* Not def: UIM_TQT_CHARDICT_CHARDICT_QT_H */
