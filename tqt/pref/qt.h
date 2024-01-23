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
#ifndef UIM_TQT_PREF_QT_H
#define UIM_TQT_PREF_QT_H

#include <tqdialog.h>
#include <tqdict.h>
#include <tqlistview.h>
#include <tqwidgetstack.h>
#include <tqlayout.h>
#include <tqlabel.h>
#include <tqpushbutton.h>
#include <tqvbox.h>
#include <tqhbox.h>
#include <tqvgroupbox.h>
#include <tqmap.h>
#include <tqptrlist.h>
#include <tqsplitter.h>

#include <uim/uim.h>
#include <uim/uim-custom.h>


class UimCustomItemIface;

class UimPrefDialog : public TQDialog
{
    TQ_OBJECT

public:
    UimPrefDialog( TQWidget *parent = 0, const char *name = 0 );
    ~UimPrefDialog();

protected:
    void checkDotUimFile();

    void setupWidgets();
    void createMainWidgets();
    void createGroupWidgets();

    void confirmChange();
    void confirmQuit();

protected slots:
    void slotSetDefault();
    void slotApply();
    void slotOK();
    void slotCancel();

    void slotSelectionChanged( TQListViewItem * );
    void slotCustomValueChanged();

private:
    bool m_isValueChanged;

    TQDict<TQWidget>  m_groupWidgetsDict;
    TQString m_currentPageName;

    TQListView *m_groupListView;
    TQWidgetStack *m_groupWidgetStack;

    TQPushButton *m_applyButton;
};

//---------------------------------------------------------------------------------
class QConfirmDialog : public TQDialog {
    TQ_OBJECT

public:
    QConfirmDialog( const TQString &msg, const TQString &confname, TQWidget *parent, const char *name = 0 );

protected:
    void setupWidgets( const TQString& msg );

protected slots:
    void showOnStart( bool isShowOnStart );

protected:
    TQString m_confname;
};

//---------------------------------------------------------------------------------
class GroupPageWidget : public TQWidget {
    TQ_OBJECT

public:
    GroupPageWidget( TQWidget *parent, const char *group_name );

    void setDefault();
    void setupWidgets();

protected:

    UimCustomItemIface *addCustom( TQVGroupBox *vbox, const char *custom_sym );
    UimCustomItemIface *addCustomTypeBool( TQVGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypeInteger( TQVGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypeString( TQVGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypePathname( TQVGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypeChoice( TQVGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypeOrderedList( TQVGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypeKey( TQVGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypeTable( TQVGroupBox *vbox, struct uim_custom *custom );

protected slots:
    void slotCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();

protected:
    TQPtrList<UimCustomItemIface> m_customIfaceList;
    TQString m_group_sym;
    bool m_widget_created;
};

#endif /* Not def: UIM_TQT_PREF_QT_H */
