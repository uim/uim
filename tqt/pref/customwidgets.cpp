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
#include "customwidgets.h"

#include <cstdlib>
#include <cctype>

#include "qtgettext.h"

#define _FU8(String) TQString::fromUtf8(String)

#define DEBUG_KEY_EDIT 0
static TQString unicodeKeyToSymStr( TQChar c );

CustomCheckBox::CustomCheckBox( struct uim_custom *c, TQWidget *parent, const char *name )
    : TQCheckBox( parent, name ),
      UimCustomItemIface( c )
{
    TQObject::connect( this, TQ_SIGNAL(toggled(bool)),
                      this, TQ_SLOT(slotCustomToggled(bool)) );

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
CustomSpinBox::CustomSpinBox( struct uim_custom *c, TQWidget *parent, const char *name)
    : TQSpinBox(c->range->as_int.min, c->range->as_int.max, 1, parent, name ),
      UimCustomItemIface( c )
{
    TQObject::connect( this, TQ_SIGNAL(valueChanged(int)),
                      this, TQ_SLOT(slotCustomValueChanged(int)) );
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
CustomLineEdit::CustomLineEdit( struct uim_custom *c, TQWidget *parent, const char *name)
    : TQLineEdit( parent, name ),
      UimCustomItemIface( c )
{
    TQObject::connect( this, TQ_SIGNAL(textChanged(const TQString&)),
                      this, TQ_SLOT(slotCustomTextChanged(const TQString&)) );

    setInputMethodEnabled( false );
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

void CustomLineEdit::slotCustomTextChanged( const TQString &text )
{
    Q_ASSERT( m_custom->type == UCustom_Str );

    free( m_custom->value->as_str );
    m_custom->value->as_str = strdup( (const char*)text.utf8() );

    setCustom( m_custom );
}

//----------------------------------------------------------------------------------------
CustomPathnameEdit::CustomPathnameEdit( struct uim_custom *c, TQWidget *parent, const char *name)
    : TQHBox( parent, name ),
      UimCustomItemIface( c )
{
    const char *button;

    setSpacing( 3 );
    m_lineEdit = new TQLineEdit( this );
    TQObject::connect( m_lineEdit, TQ_SIGNAL(textChanged(const TQString &)),
                      this, TQ_SLOT(slotCustomTextChanged(const TQString &)) );

    m_fileButton = new TQPushButton( this );
    /* Since both pathname type opens the file dialog to select an item rather
     * than open it, the label should always be "Select..." here.  The type is
     * obvious for uses even if the button label does not indicate
     * it. Information about the action the button causes is more important.
     *   -- YamaKen 2006-01-21 */
    switch (m_custom->value->as_pathname->type) {
    case UCustomPathnameType_Directory:
        button = N_( "Select..." );
        break;
    case UCustomPathnameType_RegularFile:
    default:
        button = N_( "Select..." );
        break;
    }
    m_fileButton->setText( mygettext(button) );
    TQObject::connect( m_fileButton, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(slotPathnameButtonClicked()) );

    update();
}

void CustomPathnameEdit::update()
{
    if( !m_custom || m_custom->type != UCustom_Pathname )
        return;

    m_lineEdit->setText( _FU8(m_custom->value->as_pathname->str) );

    /* sync with Label */
    parentWidget()->setEnabled( m_custom->is_active );
}

void CustomPathnameEdit::setDefault()
{
    free( m_custom->value->as_pathname->str );
    m_custom->value->as_pathname->str = strdup( m_custom->default_value->as_pathname->str );
    m_custom->value->as_pathname->type = m_custom->default_value->as_pathname->type;

    setCustom( m_custom );
    update();
}

void CustomPathnameEdit::slotPathnameButtonClicked()
{
    m_fileDialog = new TQFileDialog( this, "file dialog" );
    TQObject::connect( m_fileDialog, TQ_SIGNAL(filterSelected(const TQString&)),
                      this, TQ_SLOT(slotFileDialogFilterSelected(const TQString&)) );

    switch (m_custom->value->as_pathname->type) {
    case UCustomPathnameType_Directory:
        m_fileDialog->setMode( TQFileDialog::Directory );
        break;
    case UCustomPathnameType_RegularFile:
    default:
         m_fileDialog->setMode( TQFileDialog::ExistingFile );
         break;
    }
    if ( m_fileDialog->exec() == TQDialog::Accepted )
    {
        TQString fileName = m_fileDialog->selectedFile();
        m_lineEdit->setText( fileName );
    }
    delete m_fileDialog;
}

void CustomPathnameEdit::slotCustomTextChanged( const TQString & text )
{
    Q_ASSERT( m_custom->type == UCustom_Pathname );

    free( m_custom->value->as_pathname->str );
    m_custom->value->as_pathname->str = strdup( (const char*)text.utf8() );

    setCustom( m_custom );
}

void CustomPathnameEdit::slotFileDialogFilterSelected( const TQString & text )
{
    if (text == "Directories")
        m_fileDialog->setMode( TQFileDialog::Directory );
    else
        m_fileDialog->setMode( TQFileDialog::ExistingFile );
}

//----------------------------------------------------------------------------------------
CustomChoiceCombo::CustomChoiceCombo( struct uim_custom *c, TQWidget *parent, const char *name)
    : TQComboBox( false, parent, name ),
      UimCustomItemIface( c )
{
    TQObject::connect( this, TQ_SIGNAL(activated(int)),
                      this, TQ_SLOT(slotActivated(int)) );

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

        if( TQString::compare( default_symbol, (*item)->symbol ) == 0 )
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

    if ( choice ) {
        m_custom->value->as_choice->symbol = strdup( choice->symbol );
        m_custom->value->as_choice->label  = strdup( choice->label );
        m_custom->value->as_choice->desc   = strdup( choice->desc );
    } else {
        m_custom->value->as_choice->symbol = strdup( "" );
        m_custom->value->as_choice->label  = strdup( "" );
        m_custom->value->as_choice->desc   = strdup( "" );
    }

    setCustom( m_custom );
}

//----------------------------------------------------------------------------------------
CustomOrderedListEdit::CustomOrderedListEdit( struct uim_custom *c, TQWidget *parent, const char *name )
    : TQHBox( parent, name ),
      UimCustomItemIface( c )
{
    setSpacing( 3 );

    m_lineEdit = new TQLineEdit( this );
    m_lineEdit->setInputMethodEnabled( false );
    m_lineEdit->setReadOnly( true );

    m_editButton = new TQPushButton( this );
    m_editButton->setText( _("Edit") );
    TQObject::connect( m_editButton, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(slotEditButtonClicked()) );

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
    d->setCaption( _FU8( m_custom->label ) );
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
            if( TQString::compare( valid_item->symbol, item->symbol ) == 0 )
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
        TQPtrList<struct uim_custom_choice> activeItemList;
        activeItemList.setAutoDelete( false );

        TQStringList activeItemLabelList = d->activeItemLabels();
        for( unsigned int i = 0; i < activeItemLabelList.count(); i++ )
        {
            struct uim_custom_choice *item = NULL;
            int j = 0;
            for( item = m_custom->range->as_olist.valid_items[0], j = 0;
                 item;
                 item = m_custom->range->as_olist.valid_items[++j] )
            {
                if( TQString::compare( activeItemLabelList[i], _FU8(item->label) ) == 0 )
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
    TQString str = TQString::null;
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

OListEditForm::OListEditForm( TQWidget *parent, const char *name )
    : OListEditFormBase( parent, name )
{
    m_listView->setSorting( -1 );

    TQObject::connect( m_upButton, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(upItem()) );
    TQObject::connect( m_downButton, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(downItem()) );
}

void OListEditForm::addCheckItem( bool isActive, const TQString &str )
{
    TQCheckListItem *item = NULL;
    TQListViewItem *lastItem = m_listView->lastItem();
    if( lastItem )
        item = new TQCheckListItem( m_listView, lastItem, str, TQCheckListItem::CheckBox );
    else
        item = new TQCheckListItem( m_listView, str, TQCheckListItem::CheckBox );

    if( item )
        item->setOn( isActive );
}

void OListEditForm::upItem()
{
    TQListViewItem *selectedItem = m_listView->selectedItem();
    if( selectedItem )
    {
        TQListViewItem *previousItem = NULL;
        for( TQListViewItem *item = m_listView->firstChild(); item; item = item->nextSibling() )
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
    TQListViewItem *selectedItem = m_listView->selectedItem();
    if( selectedItem )
    {
        TQListViewItem *nextItem = selectedItem->nextSibling();
        if( nextItem )
            selectedItem->moveItem( nextItem );
    }
}

TQStringList OListEditForm::activeItemLabels() const
{
    TQStringList activeItemLabelList;
    for( TQCheckListItem *item = (TQCheckListItem*)m_listView->firstChild();
         item;
         item = (TQCheckListItem*)item->nextSibling() )
    {
        if( item->isOn() )
        {
            activeItemLabelList << item->text( 0 );
        }
    }
    return activeItemLabelList;
}

//----------------------------------------------------------------------------------------
CustomKeyEdit::CustomKeyEdit( struct uim_custom *c, TQWidget *parent, const char *name )
    : TQHBox( parent, name ),
      UimCustomItemIface( c )
{
    setSpacing( 3 );
    m_lineEdit = new TQLineEdit( this );
    m_lineEdit->setInputMethodEnabled( false );
    m_lineEdit->setReadOnly( true );

    m_editButton = new TQPushButton( this );
    m_editButton->setText( _("Edit") );
    TQObject::connect( m_editButton, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(slotKeyButtonClicked()) );

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
    TQString str = TQString::null;
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
    d->setCaption( _( "%1 - key configuration" ).arg( _FU8( m_custom->label ) ) );

    /* add items */
    TQString str = TQString::null;
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
        const TQStringList keyStrList = d->getKeyStrList();

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

KeyEditForm::KeyEditForm( TQWidget *parent, const char *name )
    : KeyEditFormBase( parent, name )
{
    m_listView->setSorting( -1 );
    m_removeButton->setEnabled( false );
    m_editButton->setEnabled( false );

    TQObject::connect( m_addButton, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(slotAddClicked()) );
    TQObject::connect( m_removeButton, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(slotRemoveClicked()) );
    TQObject::connect( m_editButton, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(slotEditClicked()) );

    TQObject::connect( m_listView, TQ_SIGNAL(selectionChanged(TQListViewItem *)),
                      this, TQ_SLOT(slotSelectionChanged(TQListViewItem*)) );
}

void KeyEditForm::addKeyItem( const TQString &str )
{
    TQListViewItem *lastItem = m_listView->lastItem();
    if( lastItem )
        new TQListViewItem( m_listView, lastItem, str );
    else
        new TQListViewItem( m_listView, str );
}

const TQStringList KeyEditForm::getKeyStrList()
{
    TQStringList keyStrList;
    for( TQListViewItem *item = m_listView->firstChild();
         item;
         item = item->nextSibling() )
    {
        keyStrList << item->text( 0 );
    }

    return keyStrList;
}

void KeyEditForm::slotAddClicked()
{
    KeyGrabDialog *d = new KeyGrabDialog( this );
    if( d->exec() == KeyGrabDialog::Accepted )
    {
        TQString keystr = d->getKeyStr();
        if( !keystr.isEmpty() )
        {
            addKeyItem( keystr );
        }
    }
    delete d;
}

void KeyEditForm::slotRemoveClicked()
{
    TQListViewItem *selectedItem = m_listView->selectedItem();
    if( selectedItem )
    {
        m_listView->takeItem( selectedItem );
    }
}

void KeyEditForm::slotEditClicked()
{
    TQListViewItem *selectedItem = m_listView->selectedItem();
    if( selectedItem )
    {
        KeyGrabDialog *d = new KeyGrabDialog( this );
        if( d->exec() == KeyGrabDialog::Accepted )
        {
            TQString keystr = d->getKeyStr();
            if( !keystr.isEmpty() )
            {
                selectedItem->setText( 0, keystr );
            }
        }
        delete d;
    }
}

void KeyEditForm::slotSelectionChanged( TQListViewItem *item )
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

KeyGrabDialog::KeyGrabDialog( TQWidget *parent, const char *name )
    : TQDialog( parent, name ),
      pressed_keyval( 0 ),
      pressed_keystate( TQt::NoButton ),
      pressed_unichar ( 0 ),
      m_keystr( 0 )
{
    TQVBoxLayout *vboxLayout = new TQVBoxLayout( this );
    TQLabel *l = new TQLabel( _("Press keys to grab (e.g. <Control>a)"), this );
    vboxLayout->addWidget( l );

    setCaption( _("Key Grab Dialog") );
}

void KeyGrabDialog::keyPressEvent( TQKeyEvent *e )
{
    pressed_keyval = e->key();
    pressed_keystate = e->state();
    pressed_unichar = e->text().at(0);
}

void KeyGrabDialog::keyReleaseEvent( TQKeyEvent * /* e */ )
{
    // create keystr
    setKeyStr();

    // end this dialog
    accept();
}

void KeyGrabDialog::setKeyStr()
{
    TQString keystr = TQString::null;
    int keyval = pressed_keyval;
    ButtonState mod = pressed_keystate;

    /*
     * Ignore Shift modifier for printable char keys for
     * easy-to-recognize key configuration.  uim-custom performs
     * implicit shift key encoding/decoding appropriately.
     */
    if( ((keyval >= 256) || !isgraph(keyval)) &&
        (mod & TQt::ShiftButton) )
        keystr += "<Shift>";
    if( mod & TQt::ControlButton )
        keystr += "<Control>";
    if( mod & TQt::AltButton )
        keystr += "<Alt>";
    if( mod & TQt::MetaButton )
        keystr += "<Meta>";

    switch( keyval ) {
    case TQt::Key_Space:
        keystr += "space";
        break;
    case TQt::Key_BackSpace:
        keystr += "backspace";
        break;
    case TQt::Key_Delete:
        keystr += "delete";
        break;
    case TQt::Key_Insert:
        keystr += "insert";
        break;
    case TQt::Key_Escape:
        keystr += "escape";
        break;
    case TQt::Key_Tab:
        keystr += "tab";
        break;
    case TQt::Key_Return:
        keystr += "return";
        break;
    case TQt::Key_Left:
        keystr += "left";
        break;
    case TQt::Key_Up:
        keystr += "up";
        break;
    case TQt::Key_Right:
        keystr += "right";
        break;
    case TQt::Key_Down:
        keystr += "down";
        break;
    case TQt::Key_Prior:
        keystr += "prior";
        break;
    case TQt::Key_Next:
        keystr += "next";
        break;
    case TQt::Key_Home:
        keystr += "home";
        break;
    case TQt::Key_End:
        keystr += "end";
        break;
#ifdef TQT_IMMODULE
    case TQt::Key_Multi_key:
        keystr += "Multi_key";
        break;
    case TQt::Key_Codeinput:
        keystr += "codeinput";
        break;
    case TQt::Key_SingleCandidate:
        keystr += "single-candidate";
        break;
    case TQt::Key_MultipleCandidate:
        keystr += "multiple-candidate";
        break;
    case TQt::Key_PreviousCandidate:
        keystr += "previous-candidate";
        break;
    case TQt::Key_Mode_switch:
        keystr += "Mode_switch";
        break;
    case TQt::Key_Kanji:
        keystr += "Kanji";
        break;
    case TQt::Key_Muhenkan:
        keystr += "Muhenkan";
        break;
    case TQt::Key_Henkan:
        keystr += "Henkan_Mode";
        break;
    case TQt::Key_Romaji:
        keystr += "romaji";
        break;
    case TQt::Key_Hiragana:
        keystr += "hiragana";
        break;
    case TQt::Key_Katakana:
        keystr += "katakana";
        break;
    case TQt::Key_Hiragana_Katakana:
        keystr += "hiragana-katakana";
        break;
    case TQt::Key_Zenkaku:
        keystr += "zenkaku";
        break;
    case TQt::Key_Hankaku:
        keystr += "hankaku";
        break;
    case TQt::Key_Zenkaku_Hankaku:
        keystr += "zenkaku-hankaku";
        break;
    case TQt::Key_Touroku:
        keystr += "touroku";
        break;
    case TQt::Key_Massyo:
        keystr += "massyo";
        break;
    case TQt::Key_Kana_Lock:
        keystr += "kana-lock";
        break;
    case TQt::Key_Kana_Shift:
        keystr += "kana-shift";
        break;
    case TQt::Key_Eisu_Shift:
        keystr += "eisu-shift";
        break;
    case TQt::Key_Eisu_toggle:
        keystr += "eisu-toggle";
        break;
    case TQt::Key_Hangul:
        keystr += "hangul";
        break;
    case TQt::Key_Hangul_Start:
        keystr += "hangul-start";
        break;
    case TQt::Key_Hangul_End:
        keystr += "hangul-end";
        break;
    case TQt::Key_Hangul_Hanja:
        keystr += "hangul-hanja";
        break;
    case TQt::Key_Hangul_Jamo:
        keystr += "hangul-jamo";
        break;
    case TQt::Key_Hangul_Romaja:
        keystr += "hangul-romaja";
        break;
    case TQt::Key_Hangul_Jeonja:
        keystr += "hangul-jeonja";
        break;
    case TQt::Key_Hangul_Banja:
        keystr += "hangul-banja";
        break;
    case TQt::Key_Hangul_PreHanja:
        keystr += "hangul-prehanja";
        break;
    case TQt::Key_Hangul_PostHanja:
        keystr += "hangul-prosthanja";
        break;
    case TQt::Key_Hangul_Special:
        keystr += "hangul-special";
        break;
    case TQt::Key_Dead_Grave:
        keystr += "dead-grave";
        break;
    case TQt::Key_Dead_Acute:
        keystr += "dead-acute";
        break;
    case TQt::Key_Dead_Circumflex:
        keystr += "dead-circumflex";
        break;
    case TQt::Key_Dead_Tilde:
        keystr += "dead-tilde";
        break;
    case TQt::Key_Dead_Macron:
        keystr += "dead-macron";
        break;
    case TQt::Key_Dead_Breve:
        keystr += "dead-breve";
        break;
    case TQt::Key_Dead_Abovedot:
        keystr += "dead-abovedot";
        break;
    case TQt::Key_Dead_Diaeresis:
        keystr += "dead-diaeresis";
        break;
    case TQt::Key_Dead_Abovering:
        keystr += "dead-abovering";
        break;
    case TQt::Key_Dead_Doubleacute:
        keystr += "dead-doubleacute";
        break;
    case TQt::Key_Dead_Caron:
        keystr += "dead-caron";
        break;
    case TQt::Key_Dead_Cedilla:
        keystr += "dead-cedilla";
        break;
    case TQt::Key_Dead_Ogonek:
        keystr += "dead-ogonek";
        break;
    case TQt::Key_Dead_Iota:
        keystr += "dead-iota";
        break;
    case TQt::Key_Dead_Voiced_Sound:
        keystr += "dead-voiced-sound";
        break;
    case TQt::Key_Dead_Semivoiced_Sound:
        keystr += "dead-semivoiced-sound";
        break;
    case TQt::Key_Dead_Belowdot:
        keystr += "dead-belowdot";
        break;
    case TQt::Key_Dead_Hook:
        keystr += "dead-hook";
        break;
    case TQt::Key_Dead_Horn:
        keystr += "dead-horn";
        break;
#endif /* Def: TQT_IMMODULE */
    case TQt::Key_Shift:
        keystr += "Shift_key";
        break;
    case TQt::Key_Control:
        keystr += "Control_key";
        break;
    case TQt::Key_Alt:
        keystr += "Alt_key";
        break;
    case TQt::Key_Meta:
        keystr += "Meta_key";
        break;
    case TQt::Key_Super_L:
    case TQt::Key_Super_R:
        keystr += "Super_key";
        break;
    case TQt::Key_Hyper_L:
    case TQt::Key_Hyper_R:
        keystr += "Hyper_key";
        break;
    case TQt::Key_CapsLock:
        keystr += "caps-lock";
        break;
    case TQt::Key_NumLock:
        keystr += "num-lock";
        break;
    case TQt::Key_ScrollLock:
        keystr += "scroll-lock";
        break;
    case TQt::Key_unknown:
        keystr += unicodeKeyToSymStr ( pressed_unichar );
        break;
    default:
        if( keyval >= TQt::Key_F1 && keyval <= TQt::Key_F35 )
        {
            keystr += "F" + TQString::number( keyval - TQt::Key_F1 + 1 );
            break;
        }
        else if( keyval < 256 )
        {
            TQChar ch = TQChar( keyval );

            if( mod & TQt::ShiftButton )
                ch = ch.upper();
            else
                ch = ch.lower();

            keystr += ch;
        }
    }

    m_keystr = keystr;

}

//----------------------------------------------------------------------------------------
CustomTable::CustomTable( struct uim_custom *c, TQWidget *parent )
    : TQFrame( parent ),
      UimCustomItemIface( c )
{
    TQPushButton *editButton = new TQPushButton (this);
    editButton->setText( _("Edit...") );
    TQObject::connect( editButton, TQ_SIGNAL(clicked()),
            this, TQ_SLOT(slotEditButtonClicked()) );

    TQHBoxLayout *layout = new TQHBoxLayout (this);
    layout->setMargin( 0 );
    layout->setSpacing( 3 );
    layout->addStretch();
    layout->addWidget( editButton );

    update();
}

void CustomTable::update()
{
    if( !m_custom || m_custom->type != UCustom_Table )
        return;
    /* sync with Label */
    parentWidget()->setEnabled( m_custom->is_active );
}

void CustomTable::setDefault()
{
    char ***custom_table = m_custom->value->as_table;
    for ( int row = 0; custom_table[row]; row++ ) {
        for ( int column = 0; custom_table[row][column]; column++ ) {
            free( custom_table[row][column] );
        }
        free( custom_table[row] );
    }
    char ***default_table = m_custom->default_value->as_table;
    int row;
    for ( row = 0; default_table[row]; row++ )
        ;
    custom_table = (char ***)malloc( sizeof(char **) * ( row + 1 ) );
    custom_table[row] = 0;

    m_custom->value->as_table = custom_table;

    for ( int row = 0; default_table[row]; row++ ) {
        int column;
        // the number of column may differ from row to row
        for ( column = 0; default_table[row][column]; column++ )
            ;
        custom_table[row] = (char **)malloc( sizeof(char *) * ( column + 1 ) );
        custom_table[row][column] = 0;
        for ( int column = 0; default_table[row][column]; column++ )
            custom_table[row][column] = strdup( default_table[row][column] );
    }

    setCustom( m_custom );
    update();
}

void CustomTable::slotEditButtonClicked()
{
    TableEditForm* d = new TableEditForm( this );
    d->setCaption( _FU8( m_custom->label ) );
    d->setTable( m_custom->value->as_table );
    int column = 0;
    for ( struct uim_custom_choice **item
            = m_custom->range->as_table_header.valid_items;
            *item; item++ ) {
        d->setTableHeaderItem( _FU8( ( *item )->label ), column );
        column++;
    }
    if ( d->exec() == TQDialog::Accepted ) {
        m_custom->value->as_table = d->table();
        setCustom( m_custom );
        update();
    }
    delete d;
}

TableEditForm::TableEditForm( TQWidget *parent )
    : TQDialog( parent )
{
    m_table = new TQTable( this );
    m_table->setFocusStyle( TQTable::SpreadSheet );
    m_table->setSelectionMode( TQTable::SingleRow );
    m_table->setRowMovingEnabled( true );
    TQHeader *verticalHeader = m_table->verticalHeader();
    verticalHeader->hide();
    TQHeader *horizontalHeader = m_table->horizontalHeader();
    horizontalHeader->setStretchEnabled( true );
    TQObject::connect( m_table, TQ_SIGNAL( currentChanged(int,int) ),
            this, TQ_SLOT( slotCurrentChanged(int,int) ) );

    TQPushButton *addButton = new TQPushButton( this );
    addButton->setText( _("Add") );
    TQObject::connect( addButton, TQ_SIGNAL( clicked() ),
            this, TQ_SLOT( slotAddClicked() ) );

    m_removeButton = new TQPushButton( this );
    m_removeButton->setEnabled( false );
    m_removeButton->setText( _("Remove") );
    TQObject::connect( m_removeButton, TQ_SIGNAL( clicked() ),
            this, TQ_SLOT( slotRemoveClicked() ) );

    m_upButton = new TQPushButton( this );
    m_upButton->setEnabled( false );
    m_upButton->setText( _("Up") );
    TQObject::connect( m_upButton, TQ_SIGNAL( clicked() ),
            this, TQ_SLOT( slotUpClicked() ) );

    m_downButton = new TQPushButton( this );
    m_downButton->setEnabled( false );
    m_downButton->setText( _("Down") );
    TQObject::connect( m_downButton, TQ_SIGNAL( clicked() ),
            this, TQ_SLOT( slotDownClicked() ) );

    TQPushButton *okButton = new TQPushButton( this );
    okButton->setText( _("OK") );
    TQObject::connect( okButton, TQ_SIGNAL( clicked() ), this, TQ_SLOT( accept() ) );

    TQPushButton *cancelButton = new TQPushButton( this );
    cancelButton->setText( _("Cancel") );
    TQObject::connect( cancelButton, TQ_SIGNAL( clicked() ), this, TQ_SLOT( reject() ) );

    TQHBoxLayout *layout = new TQHBoxLayout( this );
    layout->addWidget( m_table );

    TQVBoxLayout *buttonLayout = new TQVBoxLayout( layout );
    buttonLayout->addWidget( addButton );
    buttonLayout->addWidget( m_removeButton );
    buttonLayout->addWidget( m_upButton );
    buttonLayout->addWidget( m_downButton );
    buttonLayout->addStretch();
    buttonLayout->addWidget( okButton );
    buttonLayout->addWidget( cancelButton );

    horizontalHeader->adjustSize();
}

void TableEditForm::setTable( char ***custom_table )
{
    if ( !custom_table )
        return;
    // the number of column may differ from row to row
    int max_column = -1;
    int row;
    for ( row = 0; custom_table[row]; row++ ) {
        for ( int column = 0; custom_table[row][column]; column++ ) {
            if ( max_column < column )
                max_column = column;
        }
    }
    m_table->setNumRows( row );
    m_table->setNumCols( max_column + 1 );

    for ( int row = 0; custom_table[row]; row++ ) {
        bool expanded = false;
        for ( int column = 0; column < max_column + 1; column++ ) {
            if ( !custom_table[row][column] )
                expanded = true;
            TQTableItem *item = new TQTableItem( m_table, expanded ? TQTableItem::Never : TQTableItem::OnTyping,
		expanded ?  "" : _FU8( custom_table[row][column] ) );
            m_table->setItem( row, column, item );
        }
    }
    m_customTable = custom_table;
}

char ***TableEditForm::table() const
{
    char ***custom_table = m_customTable;
    for ( int row = 0; custom_table[row]; row++ ) {
        for ( int column = 0; custom_table[row][column]; column++ ) {
            free( custom_table[row][column] );
        }
        free( custom_table[row] );
    }

    int rowCount = m_table->numRows();
    custom_table = (char ***)malloc( sizeof(char **) * ( rowCount + 1 ) );
    custom_table[rowCount] = 0;

    int columnCount = m_table->numCols();

    for ( int row = 0; row < rowCount; row++ ) {
        int columnCountForRow = columnCount;
        // the number of column may differ from row to row
        for ( int column = 0; column < columnCount; column++ ) {
            if ( m_table->item( row, column )->editType() == TQTableItem::Never ) {
                columnCountForRow = column;
                break;
            }
        }
        custom_table[row]
            = (char **)malloc( sizeof(char *) * ( columnCountForRow + 1 ) );
        custom_table[row][columnCountForRow] = 0;
        for ( int column = 0; column < columnCountForRow; column++ )
            custom_table[row][column] = strdup(
                m_table->item( row, column )->text().utf8().data() );
    }

    return custom_table;
}

void TableEditForm::setTableHeaderItem( const TQString &item, int column )
{
    if ( column < m_table->numCols() ) {
        m_table->horizontalHeader()->setLabel( column, item );
        if ( column < m_table->numCols() - 1 ) {
            m_table->horizontalHeader()->setStretchEnabled( false );
        }
    }
}

void TableEditForm::slotCurrentChanged( int row, int col )
{
    bool itemSelected = ( row >= 0 && row < m_table->numRows() );
    m_removeButton->setEnabled( itemSelected );
    m_upButton->setEnabled( itemSelected && row != 0 );
    m_downButton->setEnabled( itemSelected && row < m_table->numRows() - 1 );
}

void TableEditForm::slotAddClicked()
{
    const int row = m_table->currentRow();
    const int col = m_table->currentColumn();
    m_table->insertRows( row );
    for ( int i = 0; i < m_table->numCols(); i++ )
        m_table->setItem( row, i, new TQTableItem( m_table, TQTableItem::OnTyping, "" ) );
    m_table->ensureCellVisible( row, col );
    m_table->setCurrentCell( row, col );
}

void TableEditForm::slotRemoveClicked()
{
    int row = m_table->currentRow();
    if ( row < 0 )
        return;
    const int col = m_table->currentColumn();
    m_table->removeRow( row );
    m_table->setCurrentCell( row, col );
}

void TableEditForm::slotUpClicked()
{
    const int row = m_table->currentRow();
    if ( row < 1 || row > m_table->numRows() - 1 )
        return;
    const int newRow = row - 1;
    const int col = m_table->currentColumn();
    m_table->selectRow( newRow );
    m_table->swapRows ( row, newRow, true );

    m_table->ensureCellVisible( newRow, col );
    m_table->setCurrentCell( newRow, col );

    m_table->updateHeaderStates();
    m_table->updateContents();
}

void TableEditForm::slotDownClicked()
{
    const int row = m_table->currentRow();
    if ( row < 0 || row > m_table->numRows() - 2 )
        return;
    const int newRow = row + 1;
    const int col = m_table->currentColumn();
    m_table->selectRow( newRow );
    m_table->swapRows ( row, newRow, true );

    m_table->ensureCellVisible( newRow, col );
    m_table->setCurrentCell( newRow, col );

    m_table->updateHeaderStates();
    m_table->updateContents();
}

static TQString unicodeKeyToSymStr ( TQChar c )
{
    TQString str = TQString::null;

    switch ( c.unicode() ) {
    case 0x00A5: str = "yen"; break;
    case 0x3002: str = "kana-fullstop"; break;
    case 0x300C: str = "kana-opening-bracket"; break;
    case 0x300D: str = "kana-closing-bracket"; break;
    case 0x3001: str = "kana-comma"; break;
    case 0x30FB: str = "kana-conjunctive"; break;
    case 0x30F2: str = "kana-WO"; break;
    case 0x30A1: str = "kana-a"; break;
    case 0x30A3: str = "kana-i"; break;
    case 0x30A5: str = "kana-u"; break;
    case 0x30A7: str = "kana-e"; break;
    case 0x30A9: str = "kana-o"; break;
    case 0x30E3: str = "kana-ya"; break;
    case 0x30E5: str = "kana-yu"; break;
    case 0x30E7: str = "kana-yo"; break;
    case 0x30C3: str = "kana-tsu"; break;
    case 0x30FC: str = "kana-prolonged-sound"; break;
    case 0x30A2: str = "kana-A"; break;
    case 0x30A4: str = "kana-I"; break;
    case 0x30A6: str = "kana-U"; break;
    case 0x30A8: str = "kana-E"; break;
    case 0x30AA: str = "kana-O"; break;
    case 0x30AB: str = "kana-KA"; break;
    case 0x30AD: str = "kana-KI"; break;
    case 0x30AF: str = "kana-KU"; break;
    case 0x30B1: str = "kana-KE"; break;
    case 0x30B3: str = "kana-KO"; break;
    case 0x30B5: str = "kana-SA"; break;
    case 0x30B7: str = "kana-SHI"; break;
    case 0x30B9: str = "kana-SU"; break;
    case 0x30BB: str = "kana-SE"; break;
    case 0x30BD: str = "kana-SO"; break;
    case 0x30BF: str = "kana-TA"; break;
    case 0x30C1: str = "kana-CHI"; break;
    case 0x30C4: str = "kana-TSU"; break;
    case 0x30C6: str = "kana-TE"; break;
    case 0x30C8: str = "kana-TO"; break;
    case 0x30CA: str = "kana-NA"; break;
    case 0x30CB: str = "kana-NI"; break;
    case 0x30CC: str = "kana-NU"; break;
    case 0x30CD: str = "kana-NE"; break;
    case 0x30CE: str = "kana-NO"; break;
    case 0x30CF: str = "kana-HA"; break;
    case 0x30D2: str = "kana-HI"; break;
    case 0x30D5: str = "kana-FU"; break;
    case 0x30D8: str = "kana-HE"; break;
    case 0x30DB: str = "kana-HO"; break;
    case 0x30DE: str = "kana-MA"; break;
    case 0x30DF: str = "kana-MI"; break;
    case 0x30E0: str = "kana-MU"; break;
    case 0x30E1: str = "kana-ME"; break;
    case 0x30E2: str = "kana-MO"; break;
    case 0x30E4: str = "kana-YA"; break;
    case 0x30E6: str = "kana-YU"; break;
    case 0x30E8: str = "kana-YO"; break;
    case 0x30E9: str = "kana-RA"; break;
    case 0x30EA: str = "kana-RI"; break;
    case 0x30EB: str = "kana-RU"; break;
    case 0x30EC: str = "kana-RE"; break;
    case 0x30ED: str = "kana-RO"; break;
    case 0x30EF: str = "kana-WA"; break;
    case 0x30F3: str = "kana-N"; break;
    case 0x309B: str = "kana-voiced-sound"; break;
    case 0x309C: str = "kana-semivoiced-sound"; break;
    default:
        break;
    }

    return str;
}

#include "customwidgets.moc"
