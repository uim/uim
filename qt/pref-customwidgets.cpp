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
#include "pref-customwidgets.h"

#include <stdlib.h>
#include <ctype.h>

#include "qtgettext.h"

#define _FU8(String) QString::fromUtf8(String)

#define DEBUG_KEY_EDIT 0

CustomCheckBox::CustomCheckBox( struct uim_custom *c, QWidget *parent, const char *name )
    : QCheckBox( parent, name ),
      UimCustomItemIface( c )
{
    QObject::connect( this, SIGNAL(toggled(bool)),
                      this, SLOT(slotCustomToggled(bool)) );

    update();
}

void CustomCheckBox::update()
{
    if( !m_custom || m_custom->type != UCustom_Bool )
        return;

    setText( _FU8(m_custom->label) );
    setChecked( m_custom->value->as_bool );

    setEnabled( m_custom->is_active );
}

void CustomCheckBox::setDefault()
{
    m_custom->value->as_bool = m_custom->default_value->as_bool;

    setCustom( m_custom );
    update();
}

void CustomCheckBox::slotCustomToggled( bool check )
{
    Q_ASSERT( m_custom->type == UCustom_Bool );

    m_custom->value->as_bool = check;
    setCustom( m_custom );
}

//----------------------------------------------------------------------------------------
CustomSpinBox::CustomSpinBox( struct uim_custom *c, QWidget *parent, const char *name)
    : QSpinBox( parent, name ),
      UimCustomItemIface( c )
{
    QObject::connect( this, SIGNAL(valueChanged(int)),
                      this, SLOT(slotCustomValueChanged(int)) );
    update();
}

void CustomSpinBox::update()
{
    if( !m_custom || m_custom->type != UCustom_Int )
        return;

    setValue( m_custom->value->as_int );
    setMinValue( m_custom->range->as_int.min );
    setMaxValue( m_custom->range->as_int.max );

    /* sync with Label */
    parentWidget()->setEnabled( m_custom->is_active );
}

void CustomSpinBox::setDefault()
{
    m_custom->value->as_int = m_custom->default_value->as_int;

    setCustom( m_custom );
    update();
}

void CustomSpinBox::slotCustomValueChanged( int value )
{
    Q_ASSERT( m_custom->type == UCustom_Int );

    m_custom->value->as_int = value;
    setCustom( m_custom );
}

//----------------------------------------------------------------------------------------
CustomLineEdit::CustomLineEdit( struct uim_custom *c, QWidget *parent, const char *name)
    : QLineEdit( parent, name ),
      UimCustomItemIface( c )
{
    QObject::connect( this, SIGNAL(textChanged(const QString&)),
                      this, SLOT(slotCustomTextChanged(const QString&)) );

    update();
}

void CustomLineEdit::update()
{
    if( !m_custom || m_custom->type != UCustom_Str )
        return;

    setText( _FU8(m_custom->value->as_str) );

    /* sync with Label */
    parentWidget()->setEnabled( m_custom->is_active );
}

void CustomLineEdit::setDefault()
{
    free( m_custom->value->as_str );
    m_custom->value->as_str = strdup( m_custom->default_value->as_str );

    setCustom( m_custom );
    update();
}

void CustomLineEdit::slotCustomTextChanged( const QString &text )
{
    Q_ASSERT( m_custom->type == UCustom_Str );

    free( m_custom->value->as_str );
    m_custom->value->as_str = strdup( (const char*)text.utf8() );

    setCustom( m_custom );
}

//----------------------------------------------------------------------------------------
CustomPathnameEdit::CustomPathnameEdit( struct uim_custom *c, QWidget *parent, const char *name)
    : QHBox( parent, name ),
      UimCustomItemIface( c )
{
    setSpacing( 3 );
    m_lineEdit = new QLineEdit( this );
    QObject::connect( m_lineEdit, SIGNAL(textChanged(const QString &)),
                      this, SLOT(slotCustomTextChanged(const QString &)) );

    m_fileButton = new QToolButton( this );
    m_fileButton->setText( _("File") );
    QObject::connect( m_fileButton, SIGNAL(clicked()),
                      this, SLOT(slotPathnameButtonClicked()) );

    update();
}

void CustomPathnameEdit::update()
{
    if( !m_custom || m_custom->type != UCustom_Pathname )
        return;

    m_lineEdit->setText( _FU8(m_custom->value->as_pathname) );

    /* sync with Label */
    parentWidget()->setEnabled( m_custom->is_active );
}

void CustomPathnameEdit::setDefault()
{
    free( m_custom->value->as_pathname );
    m_custom->value->as_pathname = strdup( m_custom->default_value->as_pathname );

    setCustom( m_custom );
    update();
}

void CustomPathnameEdit::slotPathnameButtonClicked()
{
    QFileDialog* fd = new QFileDialog( this, "file dialog" );
    fd->setMode( QFileDialog::Directory );
    if ( fd->exec() == QDialog::Accepted )
    {
        QString fileName = fd->selectedFile();
        m_lineEdit->setText( fileName );
    }
    delete fd;
}

void CustomPathnameEdit::slotCustomTextChanged( const QString & text )
{
    Q_ASSERT( m_custom->type == UCustom_Pathname );

    free( m_custom->value->as_pathname );
    m_custom->value->as_pathname = strdup( (const char*)text.utf8() );

    setCustom( m_custom );
}

//----------------------------------------------------------------------------------------
CustomChoiceCombo::CustomChoiceCombo( struct uim_custom *c, QWidget *parent, const char *name)
    : QComboBox( parent, name ),
      UimCustomItemIface( c )
{
    QObject::connect( this, SIGNAL(activated(int)),
                      this, SLOT(slotActivated(int)) );

    update();
}

void CustomChoiceCombo::update()
{
    if( !m_custom || m_custom->type != UCustom_Choice )
        return;

    clear();
    char *default_symbol = m_custom->value->as_choice->symbol;
    int default_index = -1;
    int index = 0;
    struct uim_custom_choice **item = m_custom->range->as_choice.valid_items;
    while( *item )
    {
        int count = this->count();
        insertItem( _FU8((*item)->label), count ); // insert item at last

        if( QString::compare( default_symbol, (*item)->symbol ) == 0 )
            default_index = index;

        index++;
        item++;
    }
    setCurrentItem( default_index );

    /* sync with Label */
    parentWidget()->setEnabled( m_custom->is_active );
}

void CustomChoiceCombo::setDefault()
{
    free( m_custom->value->as_choice->symbol );
    free( m_custom->value->as_choice->label );
    free( m_custom->value->as_choice->desc );

    m_custom->value->as_choice->symbol = strdup( m_custom->default_value->as_choice->symbol );
    m_custom->value->as_choice->label  = strdup( m_custom->default_value->as_choice->label );
    m_custom->value->as_choice->desc   = strdup( m_custom->default_value->as_choice->desc );

    setCustom( m_custom );
    update();
}

void CustomChoiceCombo::slotActivated( int index )
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

//----------------------------------------------------------------------------------------
CustomOrderedListEdit::CustomOrderedListEdit( struct uim_custom *c, QWidget *parent, const char *name )
    : QHBox( parent, name ),
      UimCustomItemIface( c )
{
    setSpacing( 3 );

    m_lineEdit = new QLineEdit( this );
    m_lineEdit->setReadOnly( true );

    m_editButton = new QToolButton( this );
    m_editButton->setText( _("Edit") );
    QObject::connect( m_editButton, SIGNAL(clicked()),
                      this, SLOT(slotEditButtonClicked()) );

    update();
}

void CustomOrderedListEdit::update()
{
    if( !m_custom || m_custom->type != UCustom_OrderedList )
        return;

    updateText();

    /* sync with Label */
    parentWidget()->setEnabled( m_custom->is_active );
}

void CustomOrderedListEdit::setDefault()
{
    /* free old items */
    int num = 0;
    for( num = 0; m_custom->value->as_olist[num]; num++ )
        ;

    for( int i = 0; i < num; i++ )
    {
        free( m_custom->value->as_olist[i]->symbol );
        free( m_custom->value->as_olist[i]->label );
        free( m_custom->value->as_olist[i]->desc );
        free( m_custom->value->as_olist[i] );
    }

    /* copy default_value to value */
    int default_num = 0;
    for( default_num = 0; m_custom->default_value->as_olist[default_num]; default_num++ )
        ;

    m_custom->value->as_olist = (struct uim_custom_choice **)realloc( m_custom->value->as_olist,
                                                                      sizeof(struct uim_custom_choice *) * (default_num + 1) );

    for( int i = 0; i < default_num; i++ )
    {
        struct uim_custom_choice *default_item = m_custom->default_value->as_olist[i];
        struct uim_custom_choice *item = (struct uim_custom_choice *)malloc(sizeof(struct uim_custom_choice));

        item->symbol = default_item->symbol ? strdup(default_item->symbol) : NULL;
        item->label  = default_item->label  ? strdup(default_item->label)  : NULL;
        item->desc   = default_item->desc   ? strdup(default_item->desc)   : NULL;

        m_custom->value->as_olist[i] = item;
    }
    m_custom->value->as_olist[default_num] = NULL; /* NULL-terminated */

    setCustom( m_custom );
    initPtrList();
    update();
}

void CustomOrderedListEdit::initPtrList()
{
    m_itemList.clear();
    m_validItemList.clear();

    if( m_custom->value->as_olist )
    {
        struct uim_custom_choice *item = NULL;
        int i = 0;
        for( item = m_custom->value->as_olist[0], i = 0;
             item;
             item = m_custom->value->as_olist[++i] )
        {
            m_itemList.append( item );
        }
    }

    if( m_custom->value->as_olist && m_custom->range->as_olist.valid_items )
    {
        struct uim_custom_choice *item = NULL;
        int i = 0;
        for( item = m_custom->range->as_olist.valid_items[0], i = 0;
             item;
             item = m_custom->range->as_olist.valid_items[++i] )
        {
            m_validItemList.append( item );
        }
    }
}

void CustomOrderedListEdit::slotEditButtonClicked()
{
    OListEditForm *d = new OListEditForm( this );
    initPtrList();

    /*
     * Adding Enabled Items
     */
    for( struct uim_custom_choice *item = m_itemList.first();
         item;
         item = m_itemList.next() )
    {
        d->addCheckItem( true, _FU8(item->label) );
    }
    /*
     * Adding Disabled Items
     */
    for( struct uim_custom_choice *valid_item = m_validItemList.first();
         valid_item;
         valid_item = m_validItemList.next() )
    {
        /* Exclude Enabled Item */
        bool isActive = false;
        for( struct uim_custom_choice *item = m_itemList.first();
             item;
             item = m_itemList.next() )
        {
            if( QString::compare( valid_item->symbol, item->symbol ) == 0 )
            {
                isActive = true;
                break;
            }
        }

        if( isActive == false )
        {
            d->addCheckItem( false, _FU8(valid_item->label) );
        }
    }

    /* Exec Dialog */
    if( d->exec() == OListEditForm::Accepted )
    {
        /* search active item's ptr, realloc it, and store in activeItemList */
        QPtrList<struct uim_custom_choice> activeItemList;
        activeItemList.setAutoDelete( false );

        QStringList activeItemLabelList = d->activeItemLabels();
        for( unsigned int i = 0; i < activeItemLabelList.count(); i++ )
        {
            struct uim_custom_choice *item = NULL;
            int j = 0;
            for( item = m_custom->range->as_olist.valid_items[0], j = 0;
                 item;
                 item = m_custom->range->as_olist.valid_items[++j] )
            {
                if( QString::compare( activeItemLabelList[i], _FU8(item->label) ) == 0 )
                {
                    /* allocate new struct because we will free the old struct */
                    struct uim_custom_choice *activeItem = (struct uim_custom_choice *)malloc(sizeof(struct uim_custom_choice));
                    activeItem->symbol = item->symbol ? strdup(item->symbol) : NULL;
                    activeItem->label  = item->label  ? strdup(item->label)  : NULL;
                    activeItem->desc   = item->desc   ? strdup(item->desc)   : NULL;
                    activeItemList.append( activeItem );
                    break;
                }
            }
        }

        /* free old items */
        for( unsigned int i = 0; i < m_itemList.count(); i++ )
        {
            free( m_custom->value->as_olist[i]->symbol );
            free( m_custom->value->as_olist[i]->label );
            free( m_custom->value->as_olist[i]->desc );
            free( m_custom->value->as_olist[i] );
        }

        /* create null-terminated new olist */
        m_custom->value->as_olist = (struct uim_custom_choice **)realloc( m_custom->value->as_olist,
                                                                         sizeof(struct uim_custom_choice *) * (activeItemList.count() + 1) );
        for( unsigned int i = 0; i < activeItemList.count(); i++ )
        {
            m_custom->value->as_olist[i] = activeItemList.at(i);
        }
        m_custom->value->as_olist[activeItemList.count()] = NULL;

        /* save */
        setCustom( m_custom );

        /* reload */
        update();
    }

    delete d;
}

void CustomOrderedListEdit::updateText()
{
    QString str = QString::null;
    if( m_custom->value->as_olist )
    {
        struct uim_custom_choice *item = NULL;
        int i = 0;
        for( item = m_custom->value->as_olist[0], i = 0;
             item;
             item = m_custom->value->as_olist[++i] )
        {
            if( i != 0 )
                str.append(", ");
            str.append( _FU8(item->label) );
        }
    }
    m_lineEdit->setText( str );
}

OListEditForm::OListEditForm( QWidget *parent, const char *name )
    : OListEditFormBase( parent, name )
{
    m_listView->setSorting( -1 );

    QObject::connect( m_upButton, SIGNAL(clicked()),
                      this, SLOT(upItem()) );
    QObject::connect( m_downButton, SIGNAL(clicked()),
                      this, SLOT(downItem()) );
}

void OListEditForm::addCheckItem( bool isActive, const QString &str )
{
    QCheckListItem *item = NULL;
    QListViewItem *lastItem = m_listView->lastItem();
    if( lastItem )
        item = new QCheckListItem( m_listView, lastItem, str, QCheckListItem::CheckBox );
    else
        item = new QCheckListItem( m_listView, str, QCheckListItem::CheckBox );

    if( item )
        item->setOn( isActive );
}

void OListEditForm::upItem()
{
    QListViewItem *selectedItem = m_listView->selectedItem();
    if( selectedItem )
    {
        QListViewItem *previousItem = NULL;
        for( QListViewItem *item = m_listView->firstChild(); item; item = item->nextSibling() )
        {
            if( item->nextSibling() == selectedItem )
                previousItem = item;
        }

        if( previousItem )
            previousItem->moveItem( selectedItem );
    }
}

void OListEditForm::downItem()
{
    QListViewItem *selectedItem = m_listView->selectedItem();
    if( selectedItem )
    {
        QListViewItem *nextItem = selectedItem->nextSibling();
        if( nextItem )
            selectedItem->moveItem( nextItem );
    }
}

QStringList OListEditForm::activeItemLabels() const
{
    QStringList activeItemLabelList;
    for( QCheckListItem *item = (QCheckListItem*)m_listView->firstChild();
         item;
         item = (QCheckListItem*)item->nextSibling() )
    {
        if( item->isOn() )
        {
            activeItemLabelList << item->text( 0 );
        }
    }
    return activeItemLabelList;
}

//----------------------------------------------------------------------------------------
CustomKeyEdit::CustomKeyEdit( struct uim_custom *c, QWidget *parent, const char *name )
    : QHBox( parent, name ),
      UimCustomItemIface( c )
{
    setSpacing( 3 );
    m_lineEdit = new QLineEdit( this );
    m_lineEdit->setReadOnly( true );

    m_editButton = new QToolButton( this );
    m_editButton->setText( _("Edit") );
    QObject::connect( m_editButton, SIGNAL(clicked()),
                      this, SLOT(slotKeyButtonClicked()) );

    update();
}

void CustomKeyEdit::update()
{
    if( !m_custom || m_custom->type != UCustom_Key )
        return;

    updateText();

    /* sync with Label */
    parentWidget()->setEnabled( m_custom->is_active );
}

void CustomKeyEdit::updateText()
{
    QString str = QString::null;
    if (m_custom->value->as_key) {
        struct uim_custom_key *key = NULL;
        int i = 0;
        for (key = m_custom->value->as_key[0], i = 0;
             key;
             key = m_custom->value->as_key[++i])
        {
            if( i != 0 )
                str.append(", ");
            str.append( key->literal );
        }
    } else {
        /* error message */
    }
    m_lineEdit->setText( str );
}


void CustomKeyEdit::setDefault()
{
    /* free old items */
    int num = 0;
    for( num = 0; m_custom->value->as_key[num]; num++ )
        ;

    for( int i = 0; i < num; i++ )
    {
        free( m_custom->value->as_key[i]->literal );
        free( m_custom->value->as_key[i]->label );
        free( m_custom->value->as_key[i]->desc );
        free( m_custom->value->as_key[i] );
    }

    /* copy default_value to value */
    int default_num = 0;
    for( default_num = 0; m_custom->default_value->as_key[default_num]; default_num++ )
        ;

    m_custom->value->as_key = (struct uim_custom_key **)realloc( m_custom->value->as_key,
                                                                 sizeof(struct uim_custom_key *) * (default_num + 1) );

    for( int i = 0; i < default_num; i++ )
    {
        struct uim_custom_key *default_item = m_custom->default_value->as_key[i];
        struct uim_custom_key *item = (struct uim_custom_key *)malloc(sizeof(struct uim_custom_key));

        item->type        = default_item->type;
        item->editor_type = default_item->editor_type;
        item->literal     = default_item->literal ? strdup(default_item->literal) : NULL;
        item->label       = default_item->label   ? strdup(default_item->label)   : NULL;
        item->desc        = default_item->desc    ? strdup(default_item->desc)    : NULL;

        m_custom->value->as_key[i] = item;
    }
    m_custom->value->as_key[default_num] = NULL; /* NULL-terminated */

    setCustom( m_custom );
    update();
}

void CustomKeyEdit::slotKeyButtonClicked()
{
    KeyEditForm *d = new KeyEditForm( this );

    /* add items */
    QString str = QString::null;
    if (m_custom->value->as_key) {
        struct uim_custom_key *key = NULL;
        int i = 0;
        for (key = m_custom->value->as_key[0], i = 0;
             key;
             key = m_custom->value->as_key[++i])
        {
            d->addKeyItem( key->literal );
        }
    }

    if( d->exec() == KeyEditForm::Accepted )
    {
        const QStringList keyStrList = d->getKeyStrList();
        if( keyStrList.isEmpty() )
            return;

        /* free old items */
        int num = 0;
        for( num = 0; m_custom->value->as_key[num]; num++ )
            ;

        for( int i = 0; i < num; i++ )
        {
            free( m_custom->value->as_key[i]->literal );
            free( m_custom->value->as_key[i]->label );
            free( m_custom->value->as_key[i]->desc );
            free( m_custom->value->as_key[i] );
        }


        /* add new items */
        num = keyStrList.count();
        m_custom->value->as_key = (struct uim_custom_key **)realloc( m_custom->value->as_key,
                                                                     sizeof(struct uim_custom_key *) * (num + 1) );

        for( int i = 0; i < num; i++ )
        {
            const char *keystr = (const char *)keyStrList[i].ascii();

            struct uim_custom_key *item = (struct uim_custom_key *)malloc(sizeof(struct uim_custom_key));
            item->type        = UCustomKey_Regular;
            item->editor_type = UCustomKeyEditor_Basic;
            item->literal     = strdup( keystr );
            item->label       = strdup( "" );
            item->desc        = strdup( "" );

            m_custom->value->as_key[i] = item;
        }
        m_custom->value->as_key[num] = NULL;

        setCustom( m_custom );
        update();
    }

    delete d;
}

KeyEditForm::KeyEditForm( QWidget *parent, const char *name )
    : KeyEditFormBase( parent, name )
{
    m_listView->setSorting( -1 );
    m_removeButton->setEnabled( false );
    m_editButton->setEnabled( false );

    QObject::connect( m_addButton, SIGNAL(clicked()),
                      this, SLOT(slotAddClicked()) );
    QObject::connect( m_removeButton, SIGNAL(clicked()),
                      this, SLOT(slotRemoveClicked()) );
    QObject::connect( m_editButton, SIGNAL(clicked()),
                      this, SLOT(slotEditClicked()) );

    QObject::connect( m_listView, SIGNAL(selectionChanged(QListViewItem *)),
                      this, SLOT(slotSelectionChanged(QListViewItem*)) );
}

void KeyEditForm::addKeyItem( const QString &str )
{
    QListViewItem *lastItem = m_listView->lastItem();
    if( lastItem )
        new QListViewItem( m_listView, lastItem, str );
    else
        new QListViewItem( m_listView, str );
}

const QStringList KeyEditForm::getKeyStrList()
{
    QStringList keyStrList;
    for( QListViewItem *item = m_listView->firstChild();
         item;
         item = item->nextSibling() )
    {
        keyStrList << item->text( 0 );
    }

    return keyStrList;
}

void KeyEditForm::slotAddClicked()
{
    KeyGrabForm *d = new KeyGrabForm( this );
    if( d->exec() == KeyGrabForm::Accepted )
    {
        QString keystr = d->getKeyStr();
        if( !keystr.isEmpty() )
        {
            addKeyItem( keystr );
        }
    }
    delete d;
}

void KeyEditForm::slotRemoveClicked()
{
    QListViewItem *selectedItem = m_listView->selectedItem();
    if( selectedItem )
    {
        m_listView->takeItem( selectedItem );
    }
}

void KeyEditForm::slotEditClicked()
{
    QListViewItem *selectedItem = m_listView->selectedItem();
    if( selectedItem )
    {
        KeyGrabForm *d = new KeyGrabForm( this );
        if( d->exec() == KeyGrabForm::Accepted )
        {
            QString keystr = d->getKeyStr();
            if( !keystr.isEmpty() )
            {
                selectedItem->setText( 0, keystr );
            }
        }
        delete d;
    }
}

void KeyEditForm::slotSelectionChanged( QListViewItem *item )
{
    if( item )
    {
        m_removeButton->setEnabled( true );
        m_editButton->setEnabled( true );
    }
    else
    {
        m_removeButton->setEnabled( false );
        m_editButton->setEnabled( false );
    }
}

KeyGrabForm::KeyGrabForm( QWidget *parent, const char *name )
    : KeyGrabFormBase( parent, name )
{
    m_shiftCheckBox->installEventFilter( this );
    m_controlCheckBox->installEventFilter( this );
    m_altCheckBox->installEventFilter( this );
    m_keyLineEdit->installEventFilter( this );
    m_okButton->installEventFilter( this );
    m_cancelButton->installEventFilter( this );

    m_keyLineEdit->setInputMethodEnabled( false );
}

void KeyGrabForm::keyPressEvent( QKeyEvent *e )
{
#if DEBUG_KEY_EDIT
    qDebug( "key press!!! - %d:%d", e->key(), e->stateAfter() );
#endif

    int qkey = e->key();
    QString keystr = "";
    /*
     * Ignore Shift modifier for printable char keys for
     * easy-to-recognize key configuration.  uim-custom performs
     * implicit shift key encoding/decoding appropriately.
     */
    if ( ((qkey >= 256 || !isgraph(qkey))) && (e->stateAfter() & Qt::ShiftButton) )
    {
        if( qkey != Qt::Key_Shift )
            keystr.append( "<Shift>" );
        m_shiftCheckBox->setChecked( true );
    }
    else
    {
        m_shiftCheckBox->setChecked( false );
    }

    if ( e->stateAfter() & Qt::ControlButton )
    {
        if( qkey != Qt::Key_Control )
            keystr.append( "<Control>" );
        m_controlCheckBox->setChecked( true );
    }
    else
    {
        m_controlCheckBox->setChecked( false );
    }

    if ( e->stateAfter() & Qt::AltButton )
    {
        if( qkey != Qt::Key_Alt )
            keystr.append( "<Alt>" );
        m_altCheckBox->setChecked( true );
    }
    else
    {
        m_altCheckBox->setChecked( false );
    }

    QString editString = "";
    switch( qkey )
    {
        /* normal keys */
    case Qt::Key_Space:
        /*
         * "space" is not proper uim keysym and only exists for user
         * convenience. It is converted to " " by uim-custom
         */
        editString.append( "space" );
        break;
    case Qt::Key_BackSpace:
        editString.append( "backspace" );
        break;
    case Qt::Key_Delete:
        editString.append( "delete" );
        break;
    case Qt::Key_Insert:
        editString.append( "insert" );
        break;
    case Qt::Key_Escape:
        editString.append( "escape" );
        break;
    case Qt::Key_Tab:
        editString.append( "tab" );
        break;
    case Qt::Key_Return:
        editString.append( "return" );
        break;
    case Qt::Key_Left:
        editString.append( "left" );
        break;
    case Qt::Key_Up:
        editString.append( "up" );
        break;
    case Qt::Key_Right:
        editString.append( "right" );
        break;
    case Qt::Key_Down:
        editString.append( "right" );
        break;
    case Qt::Key_Prior:
        editString.append( "prior" );
        break;
    case Qt::Key_Next:
        editString.append( "next" );
        break;
    case Qt::Key_Home:
        editString.append( "home" );
        break;
    case Qt::Key_End:
        editString.append( "end" );
        break;
#ifdef QT_IMMODULE
    case Qt::Key_Kanji:
    case Qt::Key_Zenkaku_Hankaku:
        editString.append( "zenkaku-hankaku" );
        break;
    case Qt::Key_Multi_key:
        editString.append( "Multi_key" );
        break;
    case Qt::Key_Mode_switch:
        editString.append( "Mode_switch" );
        break;
    case Qt::Key_Henkan:
        editString.append( "Henkan_Mode" );
        break;
    case Qt::Key_Muhenkan:
        editString.append( "Muhenkan" );
        break;
#endif /* Def: QT_IMMODULE */
    case Qt::Key_Shift:
        m_shiftCheckBox->setChecked( true );
        editString.append( "Shift_key" );
        break;
    case Qt::Key_Control:
        m_controlCheckBox->setChecked( true );
        editString.append( "Control_key" );
        break;
    case Qt::Key_Alt:
        m_altCheckBox->setChecked( true );
        editString.append( "Alt_key" );
        break;
    case Qt::Key_Meta:
        editString.append( "Meta_key" );
        break;
    case Qt::Key_Super_L:
    case Qt::Key_Super_R:
        editString.append( "Super_key" );
        break;
    case Qt::Key_Hyper_L:
    case Qt::Key_Hyper_R:
        editString.append( "Hyper_key" );
        break;
    default:
        if( Qt::Key_F1 <= qkey && qkey <= Qt::Key_F35 ) {
            int n = qkey - Qt::Key_F1 + 1;
            editString.append( "F" );
            editString.append( QString::number( n ) );
        }
        else if( isascii( qkey ) )
        {
            QString ch = QChar( qkey );
            if( e->stateAfter() & Qt::ShiftButton )
            {
                ch = ch.upper();
            }
            else
            {
                ch = ch.lower();
            }
            editString.append( ch );
        }
        else
        {
            /* unknown key */
            qDebug( "unknown key" );
            return;
        }
    }

    if( qkey == Qt::Key_Shift || qkey == Qt::Key_Control || qkey == Qt::Key_Alt )
        m_keyLineEdit->setText( "" );
    else
        m_keyLineEdit->setText( editString );


    keystr.append( editString );
#if DEBUG_KEY_EDIT
    qDebug( "keystr = %s", (const char *)keystr.local8Bit() );
#endif

    m_keystr = keystr;
}

bool KeyGrabForm::eventFilter( QObject * watched, QEvent * e )
{
    if( e->type() == QEvent::KeyPress )
    {
        keyPressEvent( (QKeyEvent*)e );
        return true;
    }

    return false;
}


#include "pref-customwidgets.moc"
