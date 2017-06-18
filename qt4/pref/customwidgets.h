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
#ifndef UIM_QT4_PREF_CUSTOMWIDGETS_H
#define UIM_QT4_PREF_CUSTOMWIDGETS_H

#include <uim/uim.h>
#include <uim/uim-custom.h>

#include <QtCore/QList>
#include <QtGui/QKeyEvent>
#if QT_VERSION < 0x050000
# include <QtGui/QCheckBox>
# include <QtGui/QComboBox>
# include <QtGui/QFrame>
# include <QtGui/QLineEdit>
# include <QtGui/QSpinBox>
#else
# include <QtWidgets/QCheckBox>
# include <QtWidgets/QComboBox>
# include <QtWidgets/QFrame>
# include <QtWidgets/QLineEdit>
# include <QtWidgets/QSpinBox>
#endif

#include "keyeditformbase.h"
#include "olisteditformbase.h"

class QListViewItem;
class QFileDialog;
class QPushButton;
class QTableWidget;

class UimCustomItemIface
{
public:
    explicit UimCustomItemIface( struct uim_custom *c = 0 )
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
        UimCustomItemIface *iface = static_cast<UimCustomItemIface*>( ptr );
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
            qFatal( "Failed to set value for \"%s\".", custom->symbol );
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
class CustomCheckBox : public QCheckBox, public UimCustomItemIface
{
    Q_OBJECT

public:
    explicit CustomCheckBox( struct uim_custom *c, QWidget *parent = 0 );

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
class CustomSpinBox : public QSpinBox, public UimCustomItemIface
{
    Q_OBJECT

public:
    CustomSpinBox( struct uim_custom *c, QWidget *parent );
    
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
class CustomLineEdit : public QLineEdit, public UimCustomItemIface
{
    Q_OBJECT

public:
    CustomLineEdit( struct uim_custom *c, QWidget *parent );

    virtual void update();
    virtual void setDefault();
protected slots:
    void slotCustomTextChanged( const QString &text );
protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }    
signals:
    void customValueChanged();
};

//----------------------------------------------------------------------------------------
class CustomPathnameEdit : public QFrame, public UimCustomItemIface
{
    Q_OBJECT

public:
    CustomPathnameEdit( struct uim_custom *c, QWidget *parent );

    virtual void update();
    virtual void setDefault();
protected slots:
    void slotPathnameButtonClicked();
    void slotCustomTextChanged( const QString & text );
private:
    QLineEdit *m_lineEdit;
    QPushButton *m_fileButton;
    QFileDialog *m_fileDialog;
protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

//----------------------------------------------------------------------------------------
class CustomChoiceCombo : public QComboBox, public UimCustomItemIface
{
    Q_OBJECT

public:
    CustomChoiceCombo( struct uim_custom *c, QWidget *parent );

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
class CustomOrderedListEdit : public QFrame, public UimCustomItemIface
{
    Q_OBJECT

public:
    CustomOrderedListEdit( struct uim_custom *c, QWidget *parent );
    
    virtual void update();
    virtual void setDefault();
protected slots:
    void slotEditButtonClicked();
private:
    QLineEdit *m_lineEdit;
    QPushButton *m_editButton;

    QList<struct uim_custom_choice *> m_validItemList;
    QList<struct uim_custom_choice *> m_itemList;
protected:
    void updateText();
    void initPtrList();
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

class OListEditForm : public OListEditFormBase {
    Q_OBJECT

public:
    explicit OListEditForm( QWidget *parent = 0 );
    virtual ~OListEditForm();    

    void addCheckItem( bool isActive, const QString &str );
    QStringList activeItemLabels() const;
protected slots:
    void upItem();
    void downItem();
};

//----------------------------------------------------------------------------------------
class CustomKeyEdit : public QFrame, public UimCustomItemIface
{
    Q_OBJECT

public:
    CustomKeyEdit( struct uim_custom *c, QWidget *parent );

    virtual void update();
    virtual void setDefault();
protected:
    void updateText();
protected slots:
    void slotKeyButtonClicked();
private:
    QLineEdit *m_lineEdit;
    QPushButton *m_editButton;
protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

class KeyEditForm : public KeyEditFormBase  {
    Q_OBJECT

public:
    explicit KeyEditForm( QWidget *parent = 0 );
    virtual ~KeyEditForm();

    void addKeyItem( const QString &str );
    const QStringList getKeyStrList();
protected slots:
    void slotAddClicked();
    void slotRemoveClicked();
    void slotEditClicked();
    void slotItemSelectionChanged();
};

class KeyGrabDialog : public QDialog {
    Q_OBJECT

public:
    explicit KeyGrabDialog( QWidget *parent = 0 );

    QString getKeyStr() const { return m_keystr; }

    virtual void keyPressEvent( QKeyEvent *e );
    virtual void keyReleaseEvent( QKeyEvent *e );

protected:
    void setKeyStr();

protected:
    int pressed_keyval;
    Qt::KeyboardModifiers pressed_keystate;
    QChar pressed_unichar;
    QString m_keystr; 
};

//----------------------------------------------------------------------------------------
class CustomTable : public QFrame, public UimCustomItemIface
{
    Q_OBJECT

public:
    CustomTable( struct uim_custom *c, QWidget *parent );

    virtual void update();
    virtual void setDefault();
private slots:
    void slotEditButtonClicked();
protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};


class TableEditForm : public QDialog
{
    Q_OBJECT

public:
    TableEditForm( QWidget *parent );

    void setTable( char ***custom_table );
    char ***table() const;

    void setTableHeaderItem( const QString &item, int column );

private:
    QTableWidget *m_table;
    QPushButton *m_removeButton;
    QPushButton *m_upButton;
    QPushButton *m_downButton;
    char ***m_customTable;
private slots:
    void slotItemSelectionChanged();
    void slotAddClicked();
    void slotRemoveClicked();
    void slotUpClicked();
    void slotDownClicked();
};

#endif /* Not def: UIM_QT4_PREF_CUSTOMWIDGETS_H */
