/*

 Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

 THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 SUCH DAMAGE.

*/
#ifndef _UIM_CHARDICT_QT_H_
#define _UIM_CHARDICT_QT_H_

#include <qstring.h>
#include <qwidget.h>
#include <qcombobox.h>
#include <qlineedit.h>
#include <qwidgetstack.h>
#include <qsplitter.h>
#include <qscrollview.h>
#include <qgridview.h>
#include <qstringlist.h>
#include <qpoint.h>
#include <qpainter.h>
#include <qsize.h>
#include <qlistview.h>

class BushuViewWidget;
class CharView;

class KUimCharDict : public QWidget
{
    Q_OBJECT

public:
    KUimCharDict( QWidget *parent = 0, const char *name = 0 );
    ~KUimCharDict();

    enum Mode {
        BUSHU = 0
    };

protected:
    void setupWidgets();
    void setupBushuWidgets();

protected slots:
    void changeMode( int mode );
    void slotCharSelected( const QString &c);

protected:
    QComboBox *m_modeCombo;
    QLineEdit *m_charLineEdit;
    QWidgetStack *m_widgetStack;
    BushuViewWidget *m_bushuView;
};

class BushuViewWidget : public QWidget {
    Q_OBJECT

public:
    BushuViewWidget( QWidget *parent = 0, const char *name = 0 );
    ~BushuViewWidget();

protected:
    void setupWidgets();
    void readDict();

protected slots:
    void slotBushuSelected(QListViewItem *);

signals:
    void charSelected( const QString & );

protected:
    QSplitter *m_mainSplitter;
    QListView *m_bushuListView;
    CharView *m_charView;
};

class CharView : public QGridView
{
    Q_OBJECT

public:
    CharView( int x, int y, QWidget *parent = 0, const char *name = 0 );
    ~CharView();

    void setCharacters( const QStringList &charList );
    virtual QSize sizeHint(void) const;
    
protected:
    virtual void paintCell(QPainter * painter, int y, int x);
    virtual void resizeEvent(QResizeEvent * e);
    virtual void contentsMousePressEvent(QMouseEvent * e);
    virtual void contentsMouseReleaseEvent(QMouseEvent * e);

    void updateCharView();

protected slots:
    QString coordsToChar( int x, int y );

signals:
    void charSelected( const QString &);

protected:
    QPoint m_activeCell;
    QStringList m_charList;
};
#endif /* Not def: _UIM_CHARDICT_QT_H_ */
