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
#ifndef _CUSTOMWIDGETS_H_
#define _CUSTOMWIDGETS_H_

#include <uim/uim.h>
#include <uim/uim-custom.h>

#include <qvbox.h>
#include <qhbox.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qcheckbox.h>
#include <qtoolbutton.h>
#include <qpushbutton.h>
#include <qapplication.h>
#include <qsplitter.h>
#include <qlistview.h>
#include <qvbox.h>
#include <qspinbox.h>
#include <qhbox.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <qfiledialog.h>
#include <qcombobox.h>

class UimCustomItemIface
{
public:
    UimCustomItemIface( struct uim_custom *c = NULL )
    {
        m_custom = c;
    }
    virtual ~UimCustomItemIface()
    {
        if( m_custom ) uim_custom_free( m_custom );
    }

protected:
    void setCustom( struct uim_custom *custom )
    {
        uim_bool rv = uim_custom_set( custom );
        if( rv )
            currentCustomValueChanged();
        else
            qFatal( "Failed to set value for \"%s\".", custom->symbol );
    }

    virtual void currentCustomValueChanged() = 0;

protected:
    struct uim_custom *m_custom;
};

class CustomCheckBox : public QCheckBox, public UimCustomItemIface
{
    Q_OBJECT

public:
    CustomCheckBox( struct uim_custom *c, QWidget *parent, const char *name = 0 )
        : QCheckBox( parent, name ),
          UimCustomItemIface( c )
    {
        QObject::connect( this, SIGNAL(toggled(bool)),
                          this, SLOT(slotCustomToggled(bool)) );
    }

protected slots:
    void slotCustomToggled( bool check )
    {
        Q_ASSERT( m_custom->type == UCustom_Bool );

        m_custom->value->as_bool = check;
        setCustom( m_custom );
    }

protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

class CustomSpinBox : public QSpinBox, public UimCustomItemIface
{
    Q_OBJECT

public:
    CustomSpinBox( struct uim_custom *c, QWidget *parent, const char *name = 0 )
        : QSpinBox( parent, name ),
          UimCustomItemIface( c )
    {
        QObject::connect( this, SIGNAL(valueChanged(int)),
                          this, SLOT(slotCustomValueChanged(int)) );
    }

public slots:
    void slotCustomValueChanged( int value )
    {
        Q_ASSERT( m_custom->type == UCustom_Int );

        m_custom->value->as_int = value;
        setCustom( m_custom );
    }

protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }    
signals:
    void customValueChanged();
};

class CustomLineEdit : public QLineEdit, public UimCustomItemIface
{
    Q_OBJECT

public:
    CustomLineEdit( struct uim_custom *c, QWidget *parent, const char *name = 0 )
        : QLineEdit( parent, name ),
          UimCustomItemIface( c )
    {
        QObject::connect( this, SIGNAL(textChanged(const QString&)),
                          this, SLOT(slotCustomTextChanged(const QString&)) );
    }

public slots:
    void slotCustomTextChanged( const QString &text )
    {
        Q_ASSERT( m_custom->type == UCustom_Str );

        free( m_custom->value->as_str );
        m_custom->value->as_str = strdup( (const char*)text.utf8() );

        setCustom( m_custom );
    }

protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }    
signals:
    void customValueChanged();
};

class CustomPathnameEdit : public QHBox, public UimCustomItemIface
{
    Q_OBJECT

public:
    CustomPathnameEdit( struct uim_custom *c, QWidget *parent, const char *name = 0 )
        : QHBox( parent, name ),
          UimCustomItemIface( c )
    {
        setSpacing( 6 );
        m_lineEdit = new QLineEdit( this );
        m_lineEdit->setText( m_custom->value->as_pathname );
        QObject::connect( m_lineEdit, SIGNAL(textChanged(const QString &)),
                          this, SLOT(slotCustomTextChanged(const QString &)) );
        QToolButton *m_fileButton = new QToolButton( this );
        m_fileButton->setText( "File" );
        QObject::connect( m_fileButton, SIGNAL(clicked()),
                          this, SLOT(slotPathnameButtonClicked()) );
    }

    void setText( const QString & str )
    {
        m_lineEdit->setText( str );
    }

protected slots:
    void slotPathnameButtonClicked()
    {
        QFileDialog* fd = new QFileDialog( this, "file dialog" );
        fd->setMode( QFileDialog::Directory );
        if ( fd->exec() == QDialog::Accepted )
        {
            QString fileName = fd->selectedFile();
            m_lineEdit->setText( fileName );
        }
    }

    void slotCustomTextChanged( const QString & text )
    {
        Q_ASSERT( m_custom->type == UCustom_Pathname );

        free( m_custom->value->as_pathname );
        m_custom->value->as_pathname = strdup( (const char*)text.utf8() );

        setCustom( m_custom );
    }

private:
    QLineEdit *m_lineEdit;
    QToolButton *m_filebutton;

protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

class CustomChoiceCombo : public QComboBox, public UimCustomItemIface
{
    Q_OBJECT

public:
    CustomChoiceCombo( struct uim_custom *c, QWidget *parent, const char *name = 0 )
        : QComboBox( parent, name ),
          UimCustomItemIface( c )
    {
        QObject::connect( this, SIGNAL(highlighted(int)),
                          this, SLOT(slotHighlighted(int)) );
    }

public slots:
    void slotHighlighted( int index )
    {
        Q_ASSERT( m_custom->type == UCustom_Choice );

        struct uim_custom_choice **valid_items = m_custom->range->as_choice.valid_items;
        struct uim_custom_choice *choice = NULL;
        if( valid_items )
        {
            for( int i = 0; valid_items[i]; i++ )
            {
                if( i == index )
                    choice = valid_items[i];
            }
        }

        free( m_custom->value->as_choice->symbol );
        free( m_custom->value->as_choice->label );
        free( m_custom->value->as_choice->desc );

        m_custom->value->as_choice->symbol = strdup( choice->symbol );
        m_custom->value->as_choice->label  = strdup( choice->label );
        m_custom->value->as_choice->desc   = strdup( choice->desc );

        setCustom( m_custom );
    }

protected:
    void currentCustomValueChanged(){ emit customValueChanged(); }
signals:
    void customValueChanged();
};

#endif /* Not def: _CUSTOMWIDGETS_H_ */
