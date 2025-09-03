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
#include <config.h>

#include "qt.h"
#include "customwidgets.h"

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
#include <tqlayout.h>
#include <tqobjectlist.h>
#include <tqfile.h>
#include <tqmessagebox.h>
#include <tqsettings.h>

#include "uim/counted-init.h"
#include "qtgettext.h"

#include <cstdlib>
#include <clocale>

#define DEFAULT_WINDOW_WIDTH 800
#define DEFAULT_WINDOW_HEIGHT 600

#define _FU8(String) TQString::fromUtf8(String)

UimPrefDialog::UimPrefDialog( TQWidget *parent, const char *name )
    : TQDialog( parent, name ),
      m_isValueChanged( false )
{
    uim_counted_init();
    if (uim_custom_enable()) {
        checkDotUimFile();
        setupWidgets();
    } else {
#if defined(ENABLE_DEBUG)
        tqDebug("uim_custom_enable() failed.");
#endif
        uim_counted_quit();
        TQApplication::exit( -1 );
    }

    setCaption( "uim-pref-tqt" );
}

UimPrefDialog::~UimPrefDialog()
{
    uim_counted_quit();
}

void UimPrefDialog::checkDotUimFile()
{
    /* Check Config */
    TQSettings settings;
    bool isShowOnStartup = settings.readBoolEntry( "/uim/qt/warnDotUim", true );
    if( !isShowOnStartup )
        return;

    /* Check File Existance */
    TQString homeDir = getenv( "HOME" );
    if( homeDir.isEmpty() )
        return;

    TQString dotUim = homeDir + "/.uim";
    if( TQFile::exists( dotUim ) )
    {
        TQString msg = _("The user customize file \"~/.uim\" is found.\n"
                        "This file will override all conflicted settings set by\n"
                        "this tool (stored in ~/.uim.d/customs/*.scm).\n"
                        "Please check the file if you find your settings aren't applied.");

        QConfirmDialog *d = new QConfirmDialog( msg,
                                                "/uim/qt/warnDotUim",
                                                this );
        d->setCaption( _("~/.uim exists!") );
        d->exec();
        delete d;
    }
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
    TQVBoxLayout *mainVLayout = new TQVBoxLayout( this );
    mainVLayout->setMargin( 6 );

    TQSplitter *mainSplitter = new TQSplitter( this );

    /* ListView */
    m_groupListView = new TQListView( mainSplitter );
    m_groupListView->addColumn( _("Group") );
    m_groupListView->setSelectionMode( TQListView::Single );
    m_groupListView->setSorting( -1 );
    TQObject::connect( m_groupListView, TQ_SIGNAL(selectionChanged( TQListViewItem * )),
                      this, TQ_SLOT(slotSelectionChanged( TQListViewItem * )) );

    /* Contents Frame */
    TQScrollView *rightSideWidget = new TQScrollView( mainSplitter );
    rightSideWidget->setResizePolicy(TQScrollView::AutoOneFit);
    m_groupWidgetStack = new TQWidgetStack( rightSideWidget->viewport() );
    rightSideWidget->addChild(m_groupWidgetStack);

    /* Buttons */
    TQWidget *buttonHWidget = new TQWidget( this );
    buttonHWidget->setSizePolicy( TQSizePolicy::Expanding, TQSizePolicy::Fixed);
    TQHBoxLayout *buttonHLayout = new TQHBoxLayout( buttonHWidget );
    buttonHLayout->setMargin( 6 );
    buttonHLayout->setSpacing( 6 );
    TQPushButton *defaultButton = new TQPushButton( _("Defaults"), buttonHWidget );
    TQObject::connect( defaultButton, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(slotSetDefault()) );
    TQPushButton *okButton = new TQPushButton( _("OK"), buttonHWidget );
    TQObject::connect( okButton, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(slotOK()) );
    m_applyButton = new TQPushButton( _("Apply"), buttonHWidget );
    m_applyButton->setEnabled( false );
    TQObject::connect( m_applyButton, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(slotApply()) );
    TQPushButton *cancelButton = new TQPushButton( _("Cancel"), buttonHWidget );
    TQObject::connect( cancelButton, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(slotCancel()) );
    buttonHLayout->addWidget( defaultButton );
    buttonHLayout->addStretch();
    buttonHLayout->addWidget( okButton );
    buttonHLayout->addWidget( m_applyButton );
    buttonHLayout->addWidget( cancelButton );

    TQFrame *separator = new TQFrame( this );
    separator->setFrameShape( TQFrame::HLine );
    separator->setFrameShadow( TQFrame::Sunken );

    mainVLayout->addWidget( mainSplitter );
    mainVLayout->addWidget( separator );
    mainVLayout->addWidget( buttonHWidget );
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
        TQListViewItem *item = NULL;
        TQListViewItem *lastItem = m_groupListView->lastItem();
        if( lastItem )
            item = new TQListViewItem( m_groupListView, lastItem, _FU8(group->label) );
        else
            item = new TQListViewItem( m_groupListView, _FU8(group->label) );

        GroupPageWidget *w = new GroupPageWidget( m_groupWidgetStack, *grp );
        if ( grp == primary_groups )
            w->setupWidgets();

        TQObject::connect( w, TQ_SIGNAL(customValueChanged()),
                          this, TQ_SLOT(slotCustomValueChanged()) );

        m_groupWidgetsDict.insert( _FU8(group->label), w );
        m_groupWidgetStack->addWidget( w );

        uim_custom_group_free( group );
    }
    uim_custom_symbol_list_free( primary_groups );
}

/*
 * GUI event handling
 */
void UimPrefDialog::slotSelectionChanged( TQListViewItem * item )
{
    /*
     * Don't confirm on each change in slot selection according to
     * [Anthy-dev 1795].
     * Dec 09 2005 ekato
     */
#if 0
    /* confirm if save the change */
    if( m_isValueChanged )
        confirmChange();
#endif

    /* switch group widget */
    TQString grpname = item->text( 0 );
    ((GroupPageWidget *)m_groupWidgetsDict[grpname])->setupWidgets();
    m_groupWidgetStack->raiseWidget( m_groupWidgetsDict[grpname] );

#if 0
    m_applyButton->setEnabled( false );
#endif
}

void UimPrefDialog::slotCustomValueChanged()
{
    m_isValueChanged = true;
    m_applyButton->setEnabled( true );
}

void UimPrefDialog::confirmChange()
{
    int result = TQMessageBox::question( this,
                                        _("Save Confirm"),
                                        _("The value was changed.\nSave?"),
                                        _("OK"),
                                        _("Cancel") );
    switch(result)
    {
    case 0:
        slotApply();
        break;
    case 1:
        m_isValueChanged = false;
        break;
    }
}

void UimPrefDialog::confirmQuit()
{
    int result = TQMessageBox::question( this,
                                        _("Quit Confirm"),
                                        _("Some value(s) have been changed.\n"
                                          "Do you really quit this program?"),
                                        _("Yes"),
                                        _("No"),
                                        TQString::null, 1, -1);
    switch(result)
    {
    case 0:
        reject();
        break;
    case 1:
        break;
    default:
        break;
    }
}

void UimPrefDialog::slotSetDefault()
{
    TQWidget *w = m_groupWidgetStack->visibleWidget();
    if( w )
    {
        ((GroupPageWidget*)w)->setDefault();
    }
}

void UimPrefDialog::slotApply()
{
    if( !m_isValueChanged )
        return;

#if defined(ENABLE_DEBUG)
    tqDebug("start saving....");
#endif

    uim_custom_save();
    uim_custom_broadcast_reload_request();

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
     * Enable confirmation since each change in slot selection is not
     * checked strictly according to [Anthy-dev 1795].
     * Dec 09 2005 ekato
     */
    if( m_isValueChanged )
        confirmQuit();
    else
        reject();
}

//-------------------------------------------------------------------------------------
QConfirmDialog::QConfirmDialog( const TQString &msg, const TQString &confname, TQWidget *parent, const char *name )
    : TQDialog( parent, name ),
      m_confname( confname )
{
    TQSettings settings;
    bool isShowOnStartup = settings.readBoolEntry( m_confname, true );
    if( isShowOnStartup )
        setupWidgets( msg );
}

void QConfirmDialog::setupWidgets( const TQString&msg )
{
    TQVBoxLayout *vLayout = new TQVBoxLayout( this );
    vLayout->setSpacing( 6 );
    vLayout->setMargin( 6 );
    TQLabel *msgLabel = new TQLabel( msg, this );
    TQFrame *sep = new TQFrame( this );
    sep->setFrameShape( TQFrame::HLine );
    sep->setFrameShadow( TQFrame::Sunken );
    vLayout->addWidget( msgLabel );
    vLayout->addWidget( sep );

    TQHBoxLayout *buttonHLayout = new TQHBoxLayout( vLayout );
    TQCheckBox *checkBox = new TQCheckBox( _("Show this dialog on startup"), this );
    TQSettings settings;
    bool isWarnDotUim = settings.readBoolEntry( m_confname, true );
    checkBox->setChecked( isWarnDotUim );
    TQPushButton *ok = new TQPushButton( _("OK"), this );
    ok->setDefault(true);
    buttonHLayout->addWidget( checkBox );
    buttonHLayout->addStretch();
    buttonHLayout->addWidget( ok );

    TQObject::connect( ok, TQ_SIGNAL(clicked()),
                      this, TQ_SLOT(accept()) );
    TQObject::connect( checkBox, TQ_SIGNAL(toggled(bool)),
                      this, TQ_SLOT(showOnStart(bool)) );
}

void QConfirmDialog::showOnStart( bool isShowOnStart )
{
    TQSettings settings;
    settings.writeEntry( m_confname, isShowOnStart );

#if defined(ENABLE_DEBUG)
    tqDebug("show on start = %d", isShowOnStart );
#endif
}


//-----------------------------------------------------------------------------------

GroupPageWidget::GroupPageWidget( TQWidget *parent, const char *group_name )
    : TQWidget( parent )
{
    m_customIfaceList.clear();
    m_customIfaceList.setAutoDelete( false );

    m_group_sym = group_name;
    m_widget_created = false;
}

void GroupPageWidget::setupWidgets()
{
    if ( m_widget_created )
        return;

    TQVBoxLayout *vLayout = new TQVBoxLayout( this );
    vLayout->setMargin( 6 );
    vLayout->setSpacing( 3 );

    struct uim_custom_group *group = uim_custom_group_get( m_group_sym.utf8() );
    if( group == NULL )
        return;

    TQLabel *groupLabel = new TQLabel( _FU8(group->label), this );
    groupLabel->setAlignment( TQt::AlignLeft );
    vLayout->addWidget( groupLabel );

    TQFrame *separator = new TQFrame( this );
    separator->setFrameShape( TQFrame::HLine );
    separator->setFrameShadow( TQFrame::Sunken );
    vLayout->addWidget( separator );

    /* default TQVGroupBox */
    TQVGroupBox *defaultGroupVBox = new TQVGroupBox( this );
    vLayout->addWidget( defaultGroupVBox );
    defaultGroupVBox->hide();

#if 0
    SubgroupData *sd = new SubgroupData( this, group_name );

    /* add various widgets to the vbox */
    char **custom_syms = uim_custom_collect_by_group( group_name );
    if( custom_syms )
    {
        for( char **custom_sym = custom_syms; *custom_sym; custom_sym++ )
        {
            TQVGroupBox *vbox = sd->searchGroupVBoxByCustomSym( *custom_sym );
            if( vbox == NULL )
            {
                vbox = defaultGroupVBox;
                vbox->show();
            }

            UimCustomItemIface *iface = addCustom( vbox, *custom_sym );
            if( iface )
                m_customIfaceList.append( iface );
        }

        uim_custom_symbol_list_free( custom_syms );
    }
#else
    char **sub_groups = uim_custom_group_subgroups( m_group_sym.utf8() );
    char **sgrp;
    for( sgrp = sub_groups; *sgrp; sgrp++ )
    {
        struct uim_custom_group *sgroup_custom =  uim_custom_group_get( *sgrp );
        TQVGroupBox *vbox;
        if( TQString::compare( *sgrp, "main" ) == 0 )
        {
            vbox = defaultGroupVBox;
	    vbox->show();
        }
        else
        {
            vbox = new TQVGroupBox( _FU8(sgroup_custom->label), this );
            layout()->add( vbox );
        }

	/* XXX quick hack to use AND expression of groups */
	TQString groups( m_group_sym );
	groups += " '";
	groups += *sgrp;
        char **custom_syms = uim_custom_collect_by_group( groups.utf8() );
        if( !custom_syms )
            continue;

        for( char **custom_sym = custom_syms; *custom_sym; custom_sym++ )
        {
            UimCustomItemIface *iface = addCustom( vbox, *custom_sym );
            if( iface )
                m_customIfaceList.append( iface );
        }
        uim_custom_symbol_list_free( custom_syms );


        uim_custom_group_free( sgroup_custom );
    }
    uim_custom_symbol_list_free( sub_groups );
#endif

    /* free */
    //delete sd;
    uim_custom_group_free( group );

    /* bottom up */
    vLayout->addStretch();

    m_widget_created = true;
}

/*
 * Building up GUI in accordance with Custom Type.
 */
UimCustomItemIface *GroupPageWidget::addCustom( TQVGroupBox *vbox, const char *custom_sym )
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
        case UCustom_Table:
            w = addCustomTypeTable( vbox, custom );
            break;
        default:
            w = NULL;
            tqWarning( "Invalid custom type: %d\n", custom->type );
            uim_custom_free( custom );
            break;
        }
    } else {
        tqWarning( "Failed to get uim_custom object for %s.", custom_sym );
    }

    /* custom is freed by UimCustomItemIface's destructor */

    return w;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeBool( TQVGroupBox *vbox, struct uim_custom *custom )
{
    CustomCheckBox *checkBox = new CustomCheckBox( custom, vbox );
    TQObject::connect( checkBox, TQ_SIGNAL(customValueChanged()),
                      this, TQ_SLOT(slotCustomValueChanged()) );

    return checkBox;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeInteger( TQVGroupBox *vbox, struct uim_custom *custom )
{
    TQHBox *hbox = new TQHBox( vbox );
    hbox->setSpacing( 6 );
    TQLabel *label = new TQLabel( _FU8(custom->label), hbox );
    hbox->setStretchFactor( new TQWidget( hbox ), 1 );
    CustomSpinBox *spinBox = new CustomSpinBox( custom, hbox );
    label->setBuddy( spinBox );

    TQObject::connect( spinBox, TQ_SIGNAL(customValueChanged()),
                      this, TQ_SLOT(slotCustomValueChanged()) );

    return spinBox;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeString( TQVGroupBox *vbox, struct uim_custom *custom )
{
    TQHBox *hbox = new TQHBox( vbox );
    hbox->setSpacing( 6 );
    TQLabel *label = new TQLabel( _FU8(custom->label) + ":", hbox );
    CustomLineEdit *lineEdit = new CustomLineEdit( custom, hbox );
    label->setBuddy( lineEdit );

    TQObject::connect( lineEdit, TQ_SIGNAL(customValueChanged()),
                      this, TQ_SLOT(slotCustomValueChanged()) );

    return lineEdit;
}

UimCustomItemIface *GroupPageWidget::addCustomTypePathname( TQVGroupBox *vbox, struct uim_custom *custom )
{
    TQHBox *hbox = new TQHBox( vbox );
    hbox->setSpacing( 6 );
    TQLabel *label = new TQLabel( _FU8(custom->label), hbox );
    CustomPathnameEdit *pathnameEdit = new CustomPathnameEdit( custom, hbox );
    label->setBuddy( pathnameEdit );

    TQObject::connect( pathnameEdit, TQ_SIGNAL(customValueChanged()),
                      this, TQ_SLOT(slotCustomValueChanged()) );

    return pathnameEdit;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeChoice( TQVGroupBox *vbox, struct uim_custom *custom )
{
    TQHBox *hbox = new TQHBox( vbox );
    hbox->setSpacing( 6 );
    TQLabel *label = new TQLabel( _FU8(custom->label), hbox );

    CustomChoiceCombo *choiceCombo = new CustomChoiceCombo( custom, hbox );
    label->setBuddy( choiceCombo );

    TQObject::connect( choiceCombo, TQ_SIGNAL(customValueChanged()),
                      this, TQ_SLOT(slotCustomValueChanged()) );

    return choiceCombo;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeOrderedList( TQVGroupBox *vbox, struct uim_custom *custom )
{
    TQHBox *hbox = new TQHBox( vbox );
    hbox->setSpacing( 6 );
    TQLabel *label = new TQLabel( _FU8(custom->label), hbox );
    CustomOrderedListEdit *olistEditBox = new CustomOrderedListEdit( custom, hbox );
    label->setBuddy( olistEditBox );

    TQObject::connect( olistEditBox, TQ_SIGNAL(customValueChanged()),
                      this, TQ_SLOT(slotCustomValueChanged()) );

    return olistEditBox;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeKey( TQVGroupBox *vbox, struct uim_custom *custom )
{
    TQHBox *hbox = new TQHBox( vbox );
    hbox->setSpacing( 6 );
    TQLabel *label = new TQLabel( _FU8(custom->label), hbox );
    CustomKeyEdit *keyEditBox = new CustomKeyEdit( custom, hbox );
    label->setBuddy( keyEditBox );
    TQObject::connect( keyEditBox, TQ_SIGNAL(customValueChanged()),
                      this, TQ_SLOT(slotCustomValueChanged()) );

    return keyEditBox;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeTable( TQVGroupBox *vbox, struct uim_custom *custom )
{
    TQFrame *hbox = new TQFrame( vbox );
    TQLabel *label = new TQLabel( _FU8(custom->label), hbox );
    CustomTable *tableBox = new CustomTable( custom, hbox );
    label->setBuddy( tableBox );
    TQObject::connect( tableBox, TQ_SIGNAL(customValueChanged()),
                      this, TQ_SLOT(slotCustomValueChanged()) );

    TQHBoxLayout *layout = new TQHBoxLayout( hbox );
    layout->setMargin( 0 );
    layout->setSpacing( 6 );
    layout->addWidget( label );
    layout->addWidget( tableBox );

    return tableBox;
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

//--------------------------------------------------------------------------------------
int main( int argc, char **argv )
{
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    bind_textdomain_codeset(PACKAGE, "UTF-8"); // ensure code encoding is UTF8-

    TQApplication a( argc, argv );

    UimPrefDialog *dlg = new UimPrefDialog();
    dlg->resize( DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT );
    dlg->setIcon( TQPixmap::fromMimeSource( UIM_PIXMAPSDIR "/uim-icon.png" ) );
    a.setMainWidget( dlg );
    dlg->show();

    return a.exec();
}

#include "qt.moc"
