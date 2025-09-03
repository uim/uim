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
#ifndef UIM_TQT_PREF_CUSTOMWIDGETS_H
#define UIM_TQT_PREF_CUSTOMWIDGETS_H

#include <uim/uim.h>
#include <uim/uim-custom.h>

#include <tqvbox.h>
#include <tqhbox.h>
#include <tqlayout.h>
#include <tqlabel.h>
#include <tqcheckbox.h>
#include <tqtoolbutton.h>
#include <tqpushbutton.h>
#include <tqapplication.h>
#include <tqsplitter.h>
#include <tqlistview.h>
#include <tqvbox.h>
#include <tqspinbox.h>
#include <tqhbox.h>
#include <tqlabel.h>
#include <tqlineedit.h>
#include <tqfiledialog.h>
#include <tqcombobox.h>
#include <tqptrlist.h>
#include <tqevent.h>
#include <tqtable.h>

#include "olisteditformbase.h"
#include "keyeditformbase.h"

class UimCustomItemIface
{
public:
    UimCustomItemIface( struct uim_custom *c = NULL )
    {
        m_custom = c;

        // callback
        uim_custom_cb_add( m_custom->symbol, this, UimCustomItemIface::update_cb );
    }
    virtual ~UimCustomItemIface()
    {
        if( m_custom ) uim_custom_free( m_custom );
    }

    /* Custom Update Callback */
    static void update_cb( void *ptr, const char *custom_sym )
    {
        UimCustomItemIface *iface = (UimCustomItemIface*)ptr;
        iface->updateItem( custom_sym );
        iface->update();
    }
    virtual void update() = 0;

    /* Set to default */
    virtual void setDefault() = 0;

protected:
    void setCustom( struct uim_custom *custom )
    {
        uim_bool rv = uim_custom_set( custom );
        if( rv )
            currentCustomValueChanged();
        else
            tqFatal( "Failed to set value for \"%s\".", custom->symbol );
    }
    void updateItem( const char *custom_sym )
    {
        // remove current custom
        if( m_custom ) uim_custom_free( m_custom );
        // set new item
        m_custom = uim_custom_get( custom_sym );
    }

    virtual void currentCustomValueChanged() = 0;

protected:
    struct uim_custom *m_custom;
};

//----------------------------------------------------------------------------------------
class CustomCheckBox : public TQCheckBox, public UimCustomItemIface
{
    TQ_OBJECT

public:
    CustomCheckBox( struct uim_custom *c, TQWidget *parent, const char *name = 0);

    virtual void update();
    virtual void setDefault();
protected slots:
    void slotCustomToggled( bool check );
protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

//----------------------------------------------------------------------------------------
class CustomSpinBox : public TQSpinBox, public UimCustomItemIface
{
    TQ_OBJECT

public:
    CustomSpinBox( struct uim_custom *c, TQWidget *parent, const char *name = 0 );

    virtual void update();
    virtual void setDefault();
public slots:
    void slotCustomValueChanged( int value );
protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

//----------------------------------------------------------------------------------------
class CustomLineEdit : public TQLineEdit, public UimCustomItemIface
{
    TQ_OBJECT

public:
    CustomLineEdit( struct uim_custom *c, TQWidget *parent, const char *name = 0 );

    virtual void update();
    virtual void setDefault();
protected slots:
    void slotCustomTextChanged( const TQString &text );
protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

//----------------------------------------------------------------------------------------
class CustomPathnameEdit : public TQHBox, public UimCustomItemIface
{
    TQ_OBJECT

public:
    CustomPathnameEdit( struct uim_custom *c, TQWidget *parent, const char *name = 0 );

    virtual void update();
    virtual void setDefault();
protected slots:
    void slotPathnameButtonClicked();
    void slotCustomTextChanged( const TQString & text );
    void slotFileDialogFilterSelected( const TQString & text );
private:
    TQLineEdit *m_lineEdit;
    TQPushButton *m_fileButton;
    TQFileDialog *m_fileDialog;
protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

//----------------------------------------------------------------------------------------
class CustomChoiceCombo : public TQComboBox, public UimCustomItemIface
{
    TQ_OBJECT

public:
    CustomChoiceCombo( struct uim_custom *c, TQWidget *parent, const char *name = 0 );

    virtual void update();
    virtual void setDefault();
protected slots:
    void slotActivated( int index );
protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

//----------------------------------------------------------------------------------------
class CustomOrderedListEdit : public TQHBox, public UimCustomItemIface
{
    TQ_OBJECT

public:
    CustomOrderedListEdit( struct uim_custom *c, TQWidget *parent, const char *name = 0 );

    virtual void update();
    virtual void setDefault();
protected slots:
    void slotEditButtonClicked();
private:
    TQLineEdit *m_lineEdit;
    TQPushButton *m_editButton;

    TQPtrList<struct uim_custom_choice> m_validItemList;
    TQPtrList<struct uim_custom_choice> m_itemList;
protected:
    void updateText();
    void initPtrList();
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

class OListEditForm : public OListEditFormBase {
    TQ_OBJECT

public:
    OListEditForm( TQWidget *parent = 0, const char *name = 0 );

    void addCheckItem( bool isActive, const TQString &str );
    TQStringList activeItemLabels() const;
protected slots:
    void upItem();
    void downItem();
};

//----------------------------------------------------------------------------------------
class CustomKeyEdit : public TQHBox, public UimCustomItemIface
{
    TQ_OBJECT

public:
    CustomKeyEdit( struct uim_custom *c, TQWidget *parent, const char *name = 0 );

    virtual void update();
    virtual void setDefault();
protected:
    void updateText();
protected slots:
    void slotKeyButtonClicked();
private:
    TQLineEdit *m_lineEdit;
    TQPushButton *m_editButton;
protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

class KeyEditForm : public KeyEditFormBase {
    TQ_OBJECT

public:
    KeyEditForm( TQWidget *parent = 0, const char *name = 0 );

    void addKeyItem( const TQString &str );
    const TQStringList getKeyStrList();
protected slots:
    void slotAddClicked();
    void slotRemoveClicked();
    void slotEditClicked();
    void slotSelectionChanged( TQListViewItem * );
};

class KeyGrabDialog : public TQDialog {
    TQ_OBJECT

public:
    KeyGrabDialog( TQWidget *parent = 0, const char *name = 0 );

    TQString getKeyStr() const { return m_keystr; }

    virtual void keyPressEvent( TQKeyEvent *e );
    virtual void keyReleaseEvent( TQKeyEvent *e );

protected:
    void setKeyStr();

protected:
    int pressed_keyval;
    ButtonState pressed_keystate;
    TQChar pressed_unichar;
    TQString m_keystr;
};

//----------------------------------------------------------------------------------------
class CustomTable : public TQFrame, public UimCustomItemIface
{
    TQ_OBJECT

public:
    CustomTable( struct uim_custom *c, TQWidget *parent );

    virtual void update();
    virtual void setDefault();
private slots:
    void slotEditButtonClicked();
protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};


class TableEditForm : public TQDialog
{
    TQ_OBJECT

public:
    TableEditForm( TQWidget *parent );

    void setTable( char ***custom_table );
    char ***table() const;

    void setTableHeaderItem( const TQString &item, int column );

private:
    TQTable *m_table;
    TQPushButton *m_removeButton;
    TQPushButton *m_upButton;
    TQPushButton *m_downButton;
    char ***m_customTable;
private slots:
    void slotCurrentChanged( int row, int col );
    void slotAddClicked();
    void slotRemoveClicked();
    void slotUpClicked();
    void slotDownClicked();
};

#endif /* Not def: UIM_TQT_PREF_CUSTOMWIDGETS_H */
