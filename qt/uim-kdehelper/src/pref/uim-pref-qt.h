/*

 Copyright (c) 2003,2004,2005 uim Project http://uim.freedesktop.org/

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
#ifndef _UIM_PREF_QT_H_
#define _UIM_PREF_QT_H_

#include <qdialog.h>
#include <qdict.h>
#include <qlistview.h>
#include <qwidgetstack.h>
#include <qcheckbox.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qpushbutton.h>
#include <qvbox.h>
#include <qhbox.h>

#include <uim/uim.h>
#include <uim/uim-custom.h>

class UimPrefDialog : public QDialog
{
    Q_OBJECT

public:
    UimPrefDialog( QWidget *parent = 0, const char *name = 0 );
    ~UimPrefDialog();

protected:
    void setupWidgets();
    void createMainWidgets();
    void createGroupWidgets();
    QWidget* createGroupWidget( const char *grpname );

    void addCustom( QVBox *vbox, const char *custom_sym );
    void addCustomTypeBool( QVBox *vbox, struct uim_custom *custom );
    void addCustomTypeInteger( QVBox *vbox, struct uim_custom *custom );
    void addCustomTypeString( QVBox *vbox, struct uim_custom *custom );
    void addCustomTypePathname( QVBox *vbox, struct uim_custom *custom );
    void addCustomTypeChoice( QVBox *vbox, struct uim_custom *custom );
    void addCustomTypeOrderedList( QVBox *vbox, struct uim_custom *custom );
    void addCustomTypeKey( QVBox *vbox, struct uim_custom *custom );

    void confirmChange();

protected slots:
    void slotApply();
    void slotOK();
    void slotCancel();

    void slotSelectionChanged( QListViewItem * );
    void slotCustomValueChanged();

private:
    bool m_isValueChanged;    
    
    QDict<QWidget>  m_groupWidgetsDict;
    QString m_currentPageName;

    QListView *m_groupListView;
    QWidgetStack *m_groupWidgetStack;
};

//---------------------------------------------------------------------------------
class QConfirmDialog : public QDialog {
    Q_OBJECT

public:
    QConfirmDialog( const QString &msg, QWidget *parent = 0, const char *name = 0 );
};



#endif /* Not def: _UIM_PREF_QT_H_ */
