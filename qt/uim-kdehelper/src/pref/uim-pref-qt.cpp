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
#include "uim-pref-qt.h"
#include "customwidgets.h"

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


UimPrefDialog::UimPrefDialog( QWidget *parent, const char *name )
    : QDialog( parent, name ),
      m_isValueChanged( false )
{
    uim_init();
    uim_custom_init();

    setupWidgets();
}

UimPrefDialog::~UimPrefDialog()
{
    uim_quit();
}

void UimPrefDialog::setupWidgets()
{
    createMainWidgets();
    createGroupWidgets();
}

void UimPrefDialog::createMainWidgets()
{
    QVBoxLayout *mainVLayout = new QVBoxLayout( this );

    QSplitter *mainSplitter = new QSplitter( this );

    m_groupListView = new QListView( mainSplitter );
    m_groupListView->addColumn( "Group" );
    m_groupListView->setSelectionMode( QListView::Single );
    m_groupListView->setSorting( -1 );
    QObject::connect( m_groupListView, SIGNAL(selectionChanged( QListViewItem * )),
                      this, SLOT(slotSelectionChanged( QListViewItem * )) );

    QWidget *leftSideWidget = new QWidget( mainSplitter );
    QVBoxLayout *leftVLayout = new QVBoxLayout( leftSideWidget );
    QWidget *buttonHWidget = new QWidget( leftSideWidget );
    m_groupWidgetStack = new QWidgetStack( leftSideWidget );
    QHBoxLayout *buttonHLayout = new QHBoxLayout( buttonHWidget );
    buttonHLayout->insertStretch( 0 );
    buttonHLayout->setMargin( 10 );
    buttonHLayout->setSpacing( 6 );
    QPushButton *applyButton  = new QPushButton( "Apply" , buttonHWidget );
    QObject::connect( applyButton, SIGNAL(clicked()),
                      this, SLOT(slotApply()) );
    QPushButton *okButton     = new QPushButton( "OK"    , buttonHWidget );
    QObject::connect( okButton, SIGNAL(clicked()),
                      this, SLOT(slotOK()) );
    QPushButton *cancelButton = new QPushButton( "Cancel", buttonHWidget );
    QObject::connect( cancelButton, SIGNAL(clicked()),
                      this, SLOT(slotCancel()) );
    buttonHLayout->addWidget( applyButton );
    buttonHLayout->addWidget( okButton );
    buttonHLayout->addWidget( cancelButton );
    leftVLayout->setMargin( 10 );
    leftVLayout->setSpacing( 6 );
    leftVLayout->addWidget( m_groupWidgetStack );
    leftVLayout->insertStretch( 1 );
    leftVLayout->addWidget( buttonHWidget );

    mainVLayout->addWidget( mainSplitter );
}

void UimPrefDialog::createGroupWidgets()
{
    char **primary_groups = uim_custom_primary_groups();
    char **grp = NULL;
    for( grp = primary_groups; *grp; grp++ )
    {
        struct uim_custom_group *group = uim_custom_group_get( *grp );
        
        new QListViewItem( m_groupListView, *grp );

        QWidget *w = createGroupWidget( *grp );
        m_groupWidgetsDict.insert( *grp, w );
        m_groupWidgetStack->addWidget( w );
        
        uim_custom_group_free( group );
    }
}

QWidget* UimPrefDialog::createGroupWidget( const char *group_name )
{
    QVBox *vbox = new QVBox( m_groupWidgetStack );

    struct uim_custom_group *group = uim_custom_group_get( group_name );
    if( group == NULL )
        return NULL;

    QLabel *groupLabel = new QLabel( group_name, vbox );
    groupLabel->setAlignment( Qt::AlignHCenter );
    QFont font;
    font.setWeight( QFont::Bold );
    font.setPixelSize( fontInfo().pixelSize() + 12 );
    groupLabel->setFont( font );

    /* add various widgets to the vbox */
    char **custom_syms = uim_custom_collect_by_group( group_name );
    if( custom_syms )
    {
        for( char **custom_sym = custom_syms; *custom_sym; custom_sym++ )
        {
            addCustom( vbox, *custom_sym );
        }

        uim_custom_symbol_list_free( custom_syms );
    }

    uim_custom_group_free( group );

    /* buttom up all widgets */
    vbox->setStretchFactor( new QWidget( vbox ), 1 );

    return vbox;
}

void UimPrefDialog::addCustom( QVBox *vbox, const char *custom_sym )
{
    struct uim_custom *custom = uim_custom_get( custom_sym );
    if( custom )
    {
        switch( custom->type )
        {
        case UCustom_Bool:
            addCustomTypeBool( vbox, custom );
            break;
        case UCustom_Int:
            addCustomTypeInteger( vbox, custom );
            break;
        case UCustom_Str:
            addCustomTypeString( vbox, custom );
            break;
        case UCustom_Pathname:
            addCustomTypePathname( vbox, custom );
            break;
        case UCustom_Choice:
            addCustomTypeChoice( vbox, custom );
            break;
        case UCustom_Key:
            addCustomTypeKey( vbox, custom );
            break;
        default:
            qFatal( "Invalid custom type: %d\n", custom->type );
            uim_custom_free( custom );
            break;
        }
    } else {
        qFatal( "Failed to get uim_custom object for %s.", custom_sym );
    }
}

void UimPrefDialog::addCustomTypeBool( QVBox *vbox, struct uim_custom *custom )
{
    CustomCheckBox *checkBox = new CustomCheckBox( custom, vbox );
    checkBox->setText( custom->label );
    checkBox->setChecked( custom->value->as_bool );
    QObject::connect( checkBox, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );
}

void UimPrefDialog::addCustomTypeInteger( QVBox *vbox, struct uim_custom *custom )
{
    QHBox *hbox = new QHBox( vbox );
    hbox->setSpacing( 6 );
    QLabel *label = new QLabel( custom->label, hbox );
    CustomSpinBox *spinBox = new CustomSpinBox( custom, hbox );
    spinBox->setValue( custom->value->as_int );
    spinBox->setMinValue( custom->range->as_int.min );
    spinBox->setMaxValue( custom->range->as_int.max );
    label->setBuddy( spinBox );

    QObject::connect( spinBox, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );
}

void UimPrefDialog::addCustomTypeString( QVBox *vbox, struct uim_custom *custom )
{
    QHBox *hbox = new QHBox( vbox );
    hbox->setSpacing( 6 );
    QLabel *label = new QLabel( custom->label, hbox );
    CustomLineEdit *lineEdit = new CustomLineEdit( custom, hbox );
    lineEdit->setText( custom->value->as_str );
    label->setBuddy( lineEdit );

    QObject::connect( lineEdit, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );
}

void UimPrefDialog::addCustomTypePathname( QVBox *vbox, struct uim_custom *custom )
{
    QHBox *hbox = new QHBox( vbox );
    hbox->setSpacing( 6 );
    QLabel *label = new QLabel( custom->label, hbox );
    CustomPathnameEdit *pathnameEdit = new CustomPathnameEdit( custom, hbox );
    pathnameEdit->setText( custom->value->as_pathname );
    label->setBuddy( pathnameEdit );

    QObject::connect( pathnameEdit, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );
}

void UimPrefDialog::addCustomTypeChoice( QVBox *vbox, struct uim_custom *custom )
{
    QHBox *hbox = new QHBox( vbox );
    hbox->setSpacing( 6 );
    QLabel *label = new QLabel( custom->label, hbox );

    CustomChoiceCombo *choiceCombo = new CustomChoiceCombo( custom, hbox );
    char *default_symbol = NULL;
    int default_index = -1;
    int index = 0;
    struct uim_custom_choice **item = custom->range->as_choice.valid_items;
    while( *item )
    {
        int count = choiceCombo->count();
        choiceCombo->insertItem( (*item)->label, count - 1 ); // insert item at last

        if( QString::compare( default_symbol, (*item)->symbol ) == 0 )
            default_index = index;

        index++;
        item++;
    }
    choiceCombo->setCurrentItem( default_index );

    label->setBuddy( choiceCombo );

    QObject::connect( choiceCombo, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );
}

void UimPrefDialog::addCustomTypeKey( QVBox *vbox, struct uim_custom *custom )
{
    // FIXME: not implemented yet
    ;
}

void UimPrefDialog::slotSelectionChanged( QListViewItem * item )
{
    /* confirm if save the change */
    if( m_isValueChanged )    
        confirmChange();
    
    /* switch group widget */
    QString grpname = item->text( 0 );
    m_groupWidgetStack->raiseWidget( m_groupWidgetsDict[grpname] );
}

void UimPrefDialog::slotCustomValueChanged()
{
    m_isValueChanged = true;    
}

void UimPrefDialog::confirmChange()
{
    QConfirmDialog *cDialog = new QConfirmDialog( "The value was changed.\nSave?",
                                                  this );
    if( cDialog->exec() == QDialog::Accepted )
    {
        slotApply();
    }
}

int main( int argc, char **argv )
{
    QApplication a( argc, argv );

    UimPrefDialog *dlg = new UimPrefDialog();
    a.setMainWidget( dlg );
    dlg->show();

    return a.exec();
}

void UimPrefDialog::slotApply()
{
    if( !m_isValueChanged )
        return;

    qDebug("start saving....");

    uim_custom_save();
    uim_custom_broadcast();

    m_isValueChanged = false;
}

void UimPrefDialog::slotOK()
{
    if( m_isValueChanged )
    {
        slotApply();
    }
    accept();
}

void UimPrefDialog::slotCancel()
{
    confirmChange();
    reject();
}

QConfirmDialog::QConfirmDialog( const QString &msg, QWidget *parent, const char *name )
    : QDialog( parent, name )
{
    QVBoxLayout *vLayout = new QVBoxLayout( this );
    vLayout->setSpacing( 6 );
    vLayout->setMargin( 10 );
    QLabel *msgLabel = new QLabel( msg, this );
    QHBox *buttonHBox = new QHBox( this );
    QPushButton *okButton = new QPushButton( "OK", buttonHBox );
    QPushButton *cancelButton = new QPushButton( "Cancel", buttonHBox );
    vLayout->addWidget( msgLabel );
    vLayout->addWidget( buttonHBox );

    QObject::connect( okButton, SIGNAL(clicked()),
                      this, SLOT(accept()) );
    QObject::connect( cancelButton, SIGNAL(clicked()),
                      this, SLOT(reject()) );
}
