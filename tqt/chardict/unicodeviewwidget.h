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
#ifndef UIM_TQT_CHARDICT_UNICODE_VIEW_WIDGET_H
#define UIM_TQT_CHARDICT_UNICODE_VIEW_WIDGET_H

#include "qt.h"

#include <tqlistview.h>
#include <tqwidget.h>
#include <tqsplitter.h>
#include <tqmap.h>
#include <tqfont.h>

class CharGridView;
class UnicodeBlock;

class UnicodeViewWidget : public CharDictViewBase
{
    TQ_OBJECT

public:
    UnicodeViewWidget( TQWidget *parent = 0, const char *name = 0 );
    ~UnicodeViewWidget();

    void setFont( const TQFont &font );

protected:
    void setupWidgets();

    void writeConfig();
    void readConfig();

protected slots:
    void slotUnicodeBlockSelected( TQListViewItem * );

protected:
    TQMap<TQListViewItem *, UnicodeBlock*> uBlockMap;

    TQSplitter *m_mainSplitter;
    TQListView *m_unicodeBlockListView;
    CharGridView *m_charGridView;
};

#endif /* Not def: UIM_TQT_CHARDICT_UNICODE_VIEW_WIDGET_H */
