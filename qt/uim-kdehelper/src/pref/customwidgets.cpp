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
#include "customwidgets.h"

#define _FU8(String) QString::fromUtf8(String)

CustomCheckBox::CustomCheckBox( struct uim_custom *c, QWidget *parent, const char *name )
    : QCheckBox( parent, name ),
      UimCustomItemIface( c )
{
    QObject::connect( this, SIGNAL(toggled(bool)),
                      this, SLOT(slotCustomToggled(bool)) );
}

void CustomCheckBox::slotCustomToggled( bool check )
{
    Q_ASSERT( m_custom->type == UCustom_Bool );

    m_custom->value->as_bool = check;
    setCustom( m_custom );
}

CustomSpinBox::CustomSpinBox( struct uim_custom *c, QWidget *parent, const char *name)
    : QSpinBox( parent, name ),
      UimCustomItemIface( c )
{
    QObject::connect( this, SIGNAL(valueChanged(int)),
                      this, SLOT(slotCustomValueChanged(int)) );
}

void CustomSpinBox::slotCustomValueChanged( int value )
{
    Q_ASSERT( m_custom->type == UCustom_Int );

    m_custom->value->as_int = value;
    setCustom( m_custom );
}

CustomLineEdit::CustomLineEdit( struct uim_custom *c, QWidget *parent, const char *name)
    : QLineEdit( parent, name ),
      UimCustomItemIface( c )
{
    QObject::connect( this, SIGNAL(textChanged(const QString&)),
                      this, SLOT(slotCustomTextChanged(const QString&)) );
}

void CustomLineEdit::slotCustomTextChanged( const QString &text )
{
    Q_ASSERT( m_custom->type == UCustom_Str );

    free( m_custom->value->as_str );
    m_custom->value->as_str = strdup( (const char*)text.utf8() );

    setCustom( m_custom );
}

CustomPathnameEdit::CustomPathnameEdit( struct uim_custom *c, QWidget *parent, const char *name)
    : QHBox( parent, name ),
      UimCustomItemIface( c )
{
    setSpacing( 3 );
    m_lineEdit = new QLineEdit( this );
    m_lineEdit->setText( m_custom->value->as_pathname );
    QObject::connect( m_lineEdit, SIGNAL(textChanged(const QString &)),
                      this, SLOT(slotCustomTextChanged(const QString &)) );
    m_fileButton = new QToolButton( this );
    m_fileButton->setText( "File" );
    QObject::connect( m_fileButton, SIGNAL(clicked()),
                      this, SLOT(slotPathnameButtonClicked()) );
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
}

void CustomPathnameEdit::slotCustomTextChanged( const QString & text )
{
    Q_ASSERT( m_custom->type == UCustom_Pathname );

    free( m_custom->value->as_pathname );
    m_custom->value->as_pathname = strdup( (const char*)text.utf8() );

    setCustom( m_custom );
}

CustomChoiceCombo::CustomChoiceCombo( struct uim_custom *c, QWidget *parent, const char *name)
    : QComboBox( parent, name ),
      UimCustomItemIface( c )
{
    QObject::connect( this, SIGNAL(highlighted(int)),
                      this, SLOT(slotHighlighted(int)) );
}

void CustomChoiceCombo::slotHighlighted( int index )
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

CustomOrderedListEdit::CustomOrderedListEdit( struct uim_custom *c, QWidget *parent, const char *name )
    : QHBox( parent, name ),
      UimCustomItemIface( c )
{
    setSpacing( 3 );

    m_lineEdit = new QLineEdit( this );
    m_editButton = new QToolButton( this );
    m_editButton->setText( "Edit" );

    initPtrList();

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
                str.append(",");
            str.append( _FU8(item->label) );
        }
    }
    m_lineEdit->setText( str );

    QObject::connect( m_editButton, SIGNAL(clicked()),
                      this, SLOT(slotEditButtonClicked()) );
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

    /*
     * Adding items
     * The item whose symbol doesn't match any valid_item's symbol is inactive.
     */
    for( struct uim_custom_choice *valid_item = m_validItemList.first();
         valid_item;
         valid_item = m_validItemList.next() )
    {
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

        d->addCheckItem( isActive, _FU8(valid_item->label) );
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
            qDebug("label = %s", (const char*)activeItemLabelList[i] );
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

                    qDebug("active sym2 = %s", (const char*)activeItemLabelList[i] );
                    qDebug("active sym1 = %s", (const char*)_FU8(item->label) );
                    qDebug("active sym  = %s", (const char*)_FU8(activeItem->symbol) );

                    break;
                }
            }
        }

        /* free old olist */
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
        initPtrList();
    }
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
