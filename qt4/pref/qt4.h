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
#ifndef UIM_QT4_PREF_QT_H
#define UIM_QT4_PREF_QT_H

#include <QtCore/QHash>
#include <QtCore/QList>
#if QT_VERSION < 0x050000
# include <QtGui/QDialog>
#else
# include <QtWidgets/QDialog>
#endif

#include <uim/uim.h>
#include <uim/uim-custom.h>

class QGroupBox;
class QPushButton;
class QStackedWidget;
class QTreeWidget;
class QTreeWidgetItem;
class QScrollArea;

class UimCustomItemIface;

class UimPrefDialog : public QDialog
{
    Q_OBJECT

public:
    explicit UimPrefDialog( QWidget *parent = 0 );
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

    void slotItemSelectionChanged();
    void slotCustomValueChanged();

private:
    bool m_isValueChanged;    
    
    QHash<QString, QWidget*>  m_groupWidgetsDict;
    QString m_currentPageName;

    QTreeWidget *m_groupListView;
    QStackedWidget *m_groupWidgetStack;
    QScrollArea *m_rightSideWidget;

    QPushButton *m_applyButton;
};

//---------------------------------------------------------------------------------
class QConfirmDialog : public QDialog {
    Q_OBJECT

public:
    QConfirmDialog( const QString &msg, const QString &confname, QWidget *parent );

protected:
    void setupWidgets( const QString& msg );

protected slots:
    void showOnStart( bool isShowOnStart );

protected:
    QString m_confname;
};

//---------------------------------------------------------------------------------
class GroupPageWidget : public QWidget {
    Q_OBJECT
    
public:
    GroupPageWidget( QWidget *parent, const char *group_name );

    void setDefault();
    void setupWidgets();

protected:

    UimCustomItemIface *addCustom( QGroupBox *vbox, const char *custom_sym );
    UimCustomItemIface *addCustomTypeBool( QGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypeInteger( QGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypeString( QGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypePathname( QGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypeChoice( QGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypeOrderedList( QGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypeKey( QGroupBox *vbox, struct uim_custom *custom );
    UimCustomItemIface *addCustomTypeTable( QGroupBox *vbox, struct uim_custom *custom );

protected slots:
    void slotCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();

protected:
    QList<UimCustomItemIface *> m_customIfaceList;
    QString m_group_sym;
    bool m_widget_created;
};

#endif /* Not def: UIM_QT4_PREF_QT_H */
