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
#include "kseparator.h"

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
#include <qlayout.h>
#include <qobjectlist.h>

/*
 * FIXME! : 2004-01-14 Kazuki Ohta <mover@hct.zaq.ne.jp>
 * After uim-kdehelper is merged to uim, please include these files
 * instead of including <libintl.h>
 *
 * #include "uim/config.h"
 * #include "uim/gettext.h"
 */
#include <libintl.h>
#include <unistd.h>

#define _FU8(String) QString::fromUtf8(String)

UimPrefDialog::UimPrefDialog( QWidget *parent, const char *name )
    : QDialog( parent, name ),
      m_isValueChanged( false )
{
    uim_init();
    if (uim_custom_enable()) {
      setupWidgets();
    } else {
      qDebug("uim_custom_enable() failed.");
      uim_quit();
      exit(-1);
    }
}

UimPrefDialog::~UimPrefDialog()
{
    uim_quit();
}

/*
 * Building up GUI
 */
void UimPrefDialog::setupWidgets()
{
    createMainWidgets();
    createGroupWidgets();
}

void UimPrefDialog::createMainWidgets()
{
    QVBoxLayout *mainVLayout = new QVBoxLayout( this );
    mainVLayout->setMargin( 6 );

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
    buttonHLayout->setSpacing( 6 );
    QPushButton *defaultButton = new QPushButton( "Defaults", buttonHWidget );
    QObject::connect( defaultButton, SIGNAL(clicked()),
                      this, SLOT(slotSetDefault()) );
    QPushButton *okButton = new QPushButton( "OK", buttonHWidget );
    QObject::connect( okButton, SIGNAL(clicked()),
                      this, SLOT(slotOK()) );
    m_applyButton = new QPushButton( "Apply", buttonHWidget );
    m_applyButton->setEnabled( false );
    QObject::connect( m_applyButton, SIGNAL(clicked()),
                      this, SLOT(slotApply()) );
    QPushButton *cancelButton = new QPushButton( "Cancel", buttonHWidget );
    QObject::connect( cancelButton, SIGNAL(clicked()),
                      this, SLOT(slotCancel()) );
    buttonHLayout->addWidget( defaultButton );
    buttonHLayout->addStretch();
    buttonHLayout->addWidget( okButton );
    buttonHLayout->addWidget( m_applyButton );
    buttonHLayout->addWidget( cancelButton );
    leftVLayout->setSpacing( 6 );
    leftVLayout->addWidget( m_groupWidgetStack );
    leftVLayout->addWidget( new KSeparator( leftSideWidget ) );
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
        if( group == NULL )
            continue;

        /* insert item in uim's order */
        QListViewItem *item = NULL;
        QListViewItem *lastItem = m_groupListView->lastItem();
        if( lastItem )
            item = new QListViewItem( m_groupListView, lastItem, _FU8(group->label) );
        else
            item = new QListViewItem( m_groupListView, _FU8(group->label) );

        GroupPageWidget *w = new GroupPageWidget( m_groupWidgetStack, *grp );
        QObject::connect( w, SIGNAL(customValueChanged()),
                          this, SLOT(slotCustomValueChanged()) );

        m_groupWidgetsDict.insert( _FU8(group->label), w );
        m_groupWidgetStack->addWidget( w );

        uim_custom_group_free( group );
    }
    uim_custom_symbol_list_free( primary_groups );
}

/*
 * GUI event handling
 */
void UimPrefDialog::slotSelectionChanged( QListViewItem * item )
{
    /* confirm if save the change */
    if( m_isValueChanged )    
        confirmChange();
    
    /* switch group widget */
    QString grpname = item->text( 0 );
    m_groupWidgetStack->raiseWidget( m_groupWidgetsDict[grpname] );

    m_applyButton->setEnabled( false );
}

void UimPrefDialog::slotCustomValueChanged()
{
    m_isValueChanged = true;
    m_applyButton->setEnabled( true );
}

void UimPrefDialog::confirmChange()
{
    QConfirmDialog *cDialog = new QConfirmDialog( "The value was changed.\nSave?",
                                                  this );
    if( cDialog->exec() == QDialog::Accepted )
    {
        slotApply();
    }
    else
    {
        m_isValueChanged = false;
    }
}

void UimPrefDialog::slotSetDefault()
{
    QWidget *w = m_groupWidgetStack->visibleWidget();
    if( w )
    {
        ((GroupPageWidget*)w)->setDefault();
    }
}

void UimPrefDialog::slotApply()
{
    if( !m_isValueChanged )
        return;

    qDebug("start saving....");

    uim_custom_save();
    uim_custom_broadcast();

    m_isValueChanged = false;
    m_applyButton->setEnabled( false );
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
    /*
    if( m_isValueChanged )
        confirmChange();
    */

    reject();
}

//-------------------------------------------------------------------------------------
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

//-----------------------------------------------------------------------------------

GroupPageWidget::GroupPageWidget( QWidget *parent, const char *group_name )
    : QWidget( parent )
{
    m_customIfaceList.clear();
    m_customIfaceList.setAutoDelete( false );
    
    setupWidgets( group_name );
}

void GroupPageWidget::setupWidgets( const char *group_name )
{
    QVBoxLayout *vLayout = new QVBoxLayout( this );
    vLayout->setSpacing( 3 );
    
    struct uim_custom_group *group = uim_custom_group_get( group_name );
    if( group == NULL )
        return;

    QLabel *groupLabel = new QLabel( _FU8(group->label), this );
    groupLabel->setAlignment( Qt::AlignLeft );
    vLayout->addWidget( groupLabel );
    
    KSeparator *separator = new KSeparator( this );
    vLayout->addWidget( separator );

    /* subgroup data */
    SubgroupData *sd = new SubgroupData( this, group_name );

    /* add various widgets to the vbox */
    char **custom_syms = uim_custom_collect_by_group( group_name );
    if( custom_syms )
    {
        for( char **custom_sym = custom_syms; *custom_sym; custom_sym++ )
        {
            QVGroupBox *vbox = sd->searchGroupVBoxByCustomSym( *custom_sym );
            UimCustomItemIface *iface = addCustom( vbox, *custom_sym );
            m_customIfaceList.append( iface );
        }

        uim_custom_symbol_list_free( custom_syms );
    }

    /* 2004-02-02 Kazuki Ohta <mover@hct.zaq.ne.jp>
     *
     * This is very adhoc hack!!
     * if "main" subgroup's gvbox dosn't have child, hides it!
     */
    QVGroupBox *mainSubgroupGroupVBox = sd->getMainSubgroupGroupVBox();
    if( mainSubgroupGroupVBox && !mainSubgroupGroupVBox->children()->isEmpty() )
    {
        mainSubgroupGroupVBox->hide();
    }

    /* free */
    delete sd;
    uim_custom_group_free( group );

    /* bottom up */
    vLayout->addStretch();    
}

/*
 * Building up GUI in accordance with Custom Type.
 */
UimCustomItemIface *GroupPageWidget::addCustom( QVGroupBox *vbox, const char *custom_sym )
{
    UimCustomItemIface *w = NULL;
    struct uim_custom *custom = uim_custom_get( custom_sym );
    if( custom )
    {
        switch( custom->type )
        {
        case UCustom_Bool:
            w = addCustomTypeBool( vbox, custom );
            break;
        case UCustom_Int:
            w = addCustomTypeInteger( vbox, custom );
            break;
        case UCustom_Str:
            w = addCustomTypeString( vbox, custom );
            break;
        case UCustom_Pathname:
            w = addCustomTypePathname( vbox, custom );
            break;
        case UCustom_Choice:
            w = addCustomTypeChoice( vbox, custom );
            break;
        case UCustom_OrderedList:
            w = addCustomTypeOrderedList( vbox, custom );
            break;
        case UCustom_Key:
            w = addCustomTypeKey( vbox, custom );
            break;
        default:
            w = NULL;
            qWarning( "Invalid custom type: %d\n", custom->type );
            uim_custom_free( custom );
            break;
        }
    } else {
        qWarning( "Failed to get uim_custom object for %s.", custom_sym );
    }

    /* custom is freed by UimCustomItemIface's destructor */

    return w;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeBool( QVGroupBox *vbox, struct uim_custom *custom )
{
    CustomCheckBox *checkBox = new CustomCheckBox( custom, vbox );
    QObject::connect( checkBox, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    return checkBox;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeInteger( QVGroupBox *vbox, struct uim_custom *custom )
{
    QHBox *hbox = new QHBox( vbox );
    hbox->setSpacing( 6 );
    QLabel *label = new QLabel( _FU8(custom->label), hbox );
    hbox->setStretchFactor( new QWidget( hbox ), 1 );
    CustomSpinBox *spinBox = new CustomSpinBox( custom, hbox );
    label->setBuddy( spinBox );

    QObject::connect( spinBox, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    return spinBox;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeString( QVGroupBox *vbox, struct uim_custom *custom )
{
    QHBox *hbox = new QHBox( vbox );
    hbox->setSpacing( 6 );
    QLabel *label = new QLabel( _FU8(custom->label) + ":", hbox );
    CustomLineEdit *lineEdit = new CustomLineEdit( custom, hbox );
    label->setBuddy( lineEdit );

    QObject::connect( lineEdit, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    return lineEdit;
}

UimCustomItemIface *GroupPageWidget::addCustomTypePathname( QVGroupBox *vbox, struct uim_custom *custom )
{
    QHBox *hbox = new QHBox( vbox );
    hbox->setSpacing( 6 );
    QLabel *label = new QLabel( _FU8(custom->label), hbox );
    CustomPathnameEdit *pathnameEdit = new CustomPathnameEdit( custom, hbox );
    label->setBuddy( pathnameEdit );

    QObject::connect( pathnameEdit, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    return pathnameEdit;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeChoice( QVGroupBox *vbox, struct uim_custom *custom )
{
    QHBox *hbox = new QHBox( vbox );
    hbox->setSpacing( 6 );
    QLabel *label = new QLabel( _FU8(custom->label), hbox );

    CustomChoiceCombo *choiceCombo = new CustomChoiceCombo( custom, hbox );
    label->setBuddy( choiceCombo );

    QObject::connect( choiceCombo, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    return choiceCombo;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeOrderedList( QVGroupBox *vbox, struct uim_custom *custom )
{
    QHBox *hbox = new QHBox( vbox );
    hbox->setSpacing( 6 );
    QLabel *label = new QLabel( _FU8(custom->label), hbox );
    CustomOrderedListEdit *olistEditBox = new CustomOrderedListEdit( custom, hbox );
    label->setBuddy( olistEditBox );

    QObject::connect( olistEditBox, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    return olistEditBox;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeKey( QVGroupBox *vbox, struct uim_custom *custom )
{
    // FIXME: not implemented yet
    QHBox *hbox = new QHBox( vbox );
    hbox->setSpacing( 6 );
    QLabel *label = new QLabel( _FU8(custom->label), hbox );
    CustomKeyEdit *keyEditBox = new CustomKeyEdit( custom, hbox );
    label->setBuddy( keyEditBox );

    QObject::connect( keyEditBox, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    return keyEditBox;
}

void GroupPageWidget::setDefault()
{
    for( UimCustomItemIface *iface = m_customIfaceList.first();
         iface;
         iface = m_customIfaceList.next() )
    {
        iface->setDefault();
    }
}

//-----------------------------------------------------------------------------------
SubgroupData::SubgroupData( QWidget*parentWidget, const char *parent_group_name )
{
    // QVGroupBox for other subgroups
    char **sub_groups = uim_custom_group_subgroups( parent_group_name );
    char **sgrp;
    for( sgrp = sub_groups; *sgrp; sgrp++ )
    {
        struct uim_custom_group *sgroup_custom =  uim_custom_group_get( *sgrp );
        /*
         * 2004-01-31 Kazuki Ohta <mover@hct.zaq.ne.jp>
         *
         * The subgroup "main" doesn't contain any contents.
         * So, we need to create default QVGroupBox for it.
         */
        if( QString::compare( *sgrp, "main" ) == 0 )
        {
            // QVGroupBox for "main" subgroup
            m_defaultGVBox = new QVGroupBox( "main", parentWidget );
            parentWidget->layout()->add( m_defaultGVBox );
            uim_custom_group_free( sgroup_custom );
            continue;
        }

        QVGroupBox *gvbox = new QVGroupBox( _FU8(sgroup_custom->label), parentWidget );
        parentWidget->layout()->add( gvbox );
        
        char **custom_syms = uim_custom_collect_by_group( *sgrp );
        if( custom_syms )
        {
            for( char **custom_sym = custom_syms; *custom_sym; custom_sym++ )
            {
                gvboxMap[QString(*custom_sym)] = gvbox;
            }
        
            uim_custom_symbol_list_free( custom_syms );
        }

        uim_custom_group_free( sgroup_custom );
    }
    uim_custom_symbol_list_free( sub_groups );
}

SubgroupData::~SubgroupData()
{
    gvboxMap.clear();    
}

QVGroupBox * SubgroupData::searchGroupVBoxByCustomSym( const char *custom_sym ) const
{
    QVGroupBox *b = gvboxMap[QString(custom_sym)];
    if( b == NULL )
        return m_defaultGVBox;        

    return b;
}

//--------------------------------------------------------------------------------------
int main( int argc, char **argv )
{
    /*
     * FIXME! : 2004-01-14 Kazuki Ohta <mover@hct.zaq.ne.jp>
     * After uim-kdehelper is merged to uim, please use PACKAGE
     * instead of "uim"
     */
    // ensure code encoding is UTF-8
    bind_textdomain_codeset( "uim", "UTF-8" );
    
    QApplication a( argc, argv );

    UimPrefDialog *dlg = new UimPrefDialog();
    a.setMainWidget( dlg );
    dlg->show();

    return a.exec();
}
