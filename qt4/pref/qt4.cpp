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

#include "qt4.h"
#include "customwidgets.h"

#include <QtCore/QFile>
#include <QtCore/QSettings>
#if QT_VERSION < 0x050000
# include <QtGui/QApplication>
# include <QtGui/QCheckBox>
# include <QtGui/QGroupBox>
# include <QtGui/QHBoxLayout>
# include <QtGui/QLabel>
# include <QtGui/QMessageBox>
# include <QtGui/QPushButton>
# include <QtGui/QScrollArea>
# include <QtGui/QSplitter>
# include <QtGui/QStackedWidget>
# include <QtGui/QTreeWidget>
# include <QtGui/QTreeWidgetItem>
# include <QtGui/QVBoxLayout>
#else
# include <QtWidgets/QApplication>
# include <QtWidgets/QCheckBox>
# include <QtWidgets/QGroupBox>
# include <QtWidgets/QHBoxLayout>
# include <QtWidgets/QLabel>
# include <QtWidgets/QMessageBox>
# include <QtWidgets/QPushButton>
# include <QtWidgets/QScrollArea>
# include <QtWidgets/QSplitter>
# include <QtWidgets/QStackedWidget>
# include <QtWidgets/QTreeWidget>
# include <QtWidgets/QTreeWidgetItem>
# include <QtWidgets/QVBoxLayout>
#endif

#include "uim/counted-init.h"
#include "qtgettext.h"

#include <cstdlib>
#include <clocale>

static const int DEFAULT_WINDOW_WIDTH = 800;
static const int DEFAULT_WINDOW_HEIGHT = 600;

inline static QString _FU8( const char string[] )
{
    return QString::fromUtf8( string );
}

UimPrefDialog::UimPrefDialog( QWidget *parent )
    : QDialog( parent ),
      m_isValueChanged( false )
{
    uim_counted_init();
    if (uim_custom_enable()) {
        checkDotUimFile();        
        setupWidgets();
    } else {
#if defined(ENABLE_DEBUG)
        qDebug("uim_custom_enable() failed.");
#endif
        uim_counted_quit();
        QApplication::exit( -1 );
    }

    setWindowTitle( "uim-pref-qt4" );
}

UimPrefDialog::~UimPrefDialog()
{
    uim_counted_quit();
}

void UimPrefDialog::checkDotUimFile()
{
    /* Check Config */
    QSettings settings;
    bool isShowOnStartup
        = settings.value( "/uim/qt/warnDotUim", true ).toBool();
    if( !isShowOnStartup )
        return;

    /* Check File Existence */
    QString homeDir = qgetenv( "HOME" );
    if( homeDir.isEmpty() )
        return;

    QString dotUim = homeDir + "/.uim";
    if( QFile::exists( dotUim ) )
    {
        QString msg = _("The user customize file \"~/.uim\" is found.\n"
                        "This file will override all conflicted settings set by\n"
                        "this tool (stored in ~/.uim.d/customs/*.scm).\n"
                        "Please check the file if you find your settings aren't applied.");

        QConfirmDialog *d = new QConfirmDialog( msg,
                                                "/uim/qt/warnDotUim",
                                                this );
        d->setWindowTitle( _("~/.uim exists!") );
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
    QVBoxLayout *mainVLayout = new QVBoxLayout( this );
    mainVLayout->setMargin( 6 );

    QSplitter *mainSplitter = new QSplitter( this );

    /* ListView */
    m_groupListView = new QTreeWidget( mainSplitter );
    m_groupListView->setRootIsDecorated( false );
    m_groupListView->setHeaderLabel( _("Group") );
    m_groupListView->setSelectionMode( QAbstractItemView::SingleSelection );
    connect( m_groupListView, SIGNAL(itemSelectionChanged()),
                      this, SLOT(slotItemSelectionChanged()) );

    /* Contents Frame */
    m_rightSideWidget = new QScrollArea( mainSplitter );
    m_rightSideWidget->setWidgetResizable( true );
    m_groupWidgetStack = new QStackedWidget;

    /* Buttons */
    QWidget *buttonHWidget = new QWidget( this );
    buttonHWidget->setSizePolicy( QSizePolicy::Expanding, QSizePolicy::Fixed);
    QHBoxLayout *buttonHLayout = new QHBoxLayout( buttonHWidget );
    buttonHLayout->setMargin( 6 );
    buttonHLayout->setSpacing( 6 );
    QPushButton *defaultButton = new QPushButton( _("Defaults"), buttonHWidget );
    connect( defaultButton, SIGNAL(clicked()),
                      this, SLOT(slotSetDefault()) );
    QPushButton *okButton = new QPushButton( _("OK"), buttonHWidget );
    connect( okButton, SIGNAL(clicked()),
                      this, SLOT(slotOK()) );
    m_applyButton = new QPushButton( _("Apply"), buttonHWidget );
    m_applyButton->setEnabled( false );
    connect( m_applyButton, SIGNAL(clicked()),
                      this, SLOT(slotApply()) );
    QPushButton *cancelButton = new QPushButton( _("Cancel"), buttonHWidget );
    connect( cancelButton, SIGNAL(clicked()),
                      this, SLOT(slotCancel()) );
    buttonHLayout->addWidget( defaultButton );
    buttonHLayout->addStretch();
    buttonHLayout->addWidget( okButton );
    buttonHLayout->addWidget( m_applyButton );
    buttonHLayout->addWidget( cancelButton );

    QFrame *separator = new QFrame( this );
    separator->setFrameShape( QFrame::HLine );
    separator->setFrameShadow( QFrame::Sunken );
    mainVLayout->setMargin( 0 );
    mainVLayout->addWidget( mainSplitter );
    mainVLayout->addWidget( separator );
    mainVLayout->addWidget( buttonHWidget );

    mainSplitter->setStretchFactor( 1, 1 );
}

void UimPrefDialog::createGroupWidgets()
{
    char **primary_groups = uim_custom_primary_groups();
    char **grp = 0;
    for( grp = primary_groups; *grp; grp++ )
    {
        struct uim_custom_group *group = uim_custom_group_get( *grp );
        if( group == 0 )
            continue;

        /* insert item in uim's order */
        m_groupListView->addTopLevelItem(
            new QTreeWidgetItem( QStringList() << _FU8(group->label) ) );

        GroupPageWidget *w = new GroupPageWidget( m_groupWidgetStack, *grp );
        if ( grp == primary_groups )
            w->setupWidgets();

        connect( w, SIGNAL(customValueChanged()),
                          this, SLOT(slotCustomValueChanged()) );

        m_groupWidgetsDict.insert( _FU8(group->label), w );
        m_groupWidgetStack->addWidget( w );

        uim_custom_group_free( group );
    }
    uim_custom_symbol_list_free( primary_groups );

    m_rightSideWidget->setWidget( m_groupWidgetStack );
}

/*
 * GUI event handling
 */
void UimPrefDialog::slotItemSelectionChanged()
{
    QList<QTreeWidgetItem *>items = m_groupListView->selectedItems();
    if ( items.isEmpty() )
        return;
    QTreeWidgetItem *item = items[ 0 ];

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
    QString grpname = item->text( 0 );
    static_cast<GroupPageWidget *>(m_groupWidgetsDict[grpname])->setupWidgets();
    m_groupWidgetStack->setCurrentWidget( m_groupWidgetsDict[grpname] );

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
    int result = QMessageBox::question( this,
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
    int result = QMessageBox::question( this,
                                        _("Quit Confirm"),
                                        _("Some value(s) have been changed.\n"
                                          "Do you really quit this program?"),
                                        _("Yes"),
                                        _("No"),
                                        "", 1, -1);
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
    QWidget *w = m_groupWidgetStack->currentWidget();
    if( w )
    {
        (static_cast<GroupPageWidget*>(w))->setDefault();
    }
}

void UimPrefDialog::slotApply()
{
    if( !m_isValueChanged )
        return;

#if defined(ENABLE_DEBUG)
    qDebug("start saving....");
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
QConfirmDialog::QConfirmDialog( const QString &msg, const QString &confname, QWidget *parent )
    : QDialog( parent ),
      m_confname( confname )
{
    QSettings settings;
    bool isShowOnStartup = settings.value( m_confname, true ).toBool();
    if( isShowOnStartup )
        setupWidgets( msg );
}

void QConfirmDialog::setupWidgets( const QString&msg )
{
    QVBoxLayout *vLayout = new QVBoxLayout( this );
    vLayout->setSpacing( 6 );
    vLayout->setMargin( 6 );
    QLabel *msgLabel = new QLabel( msg, this );
    QFrame *sep = new QFrame( this );
    sep->setFrameShape( QFrame::HLine );
    sep->setFrameShadow( QFrame::Sunken );
    vLayout->addWidget( msgLabel );
    vLayout->addWidget( sep );

    QHBoxLayout *buttonHLayout = new QHBoxLayout;
    vLayout->addLayout( buttonHLayout );
    QCheckBox *checkBox = new QCheckBox( _("Show this dialog on startup"), this );
    QSettings settings;
    bool isWarnDotUim = settings.value( m_confname, true ).toBool();
    checkBox->setChecked( isWarnDotUim );
    QPushButton *ok = new QPushButton( _("OK"), this );
    ok->setDefault(true);
    buttonHLayout->addWidget( checkBox );
    buttonHLayout->addStretch();    
    buttonHLayout->addWidget( ok );

    connect( ok, SIGNAL(clicked()),
                      this, SLOT(accept()) );
    connect( checkBox, SIGNAL(toggled(bool)),
                      this, SLOT(showOnStart(bool)) );
}

void QConfirmDialog::showOnStart( bool isShowOnStart )
{
    QSettings settings;
    settings.setValue( m_confname, isShowOnStart );

#if defined(ENABLE_DEBUG)
    qDebug("show on start = %d", isShowOnStart );
#endif
}


//-----------------------------------------------------------------------------------

GroupPageWidget::GroupPageWidget( QWidget *parent, const char *group_name )
    : QWidget( parent )
{
    m_customIfaceList.clear();
    
    m_group_sym = group_name;
    m_widget_created = false;
}

void GroupPageWidget::setupWidgets()
{
    if ( m_widget_created )
        return;

    QVBoxLayout *vLayout = new QVBoxLayout( this );
    vLayout->setMargin( 6 );
    vLayout->setSpacing( 3 );
    
    struct uim_custom_group *group
        = uim_custom_group_get( m_group_sym.toLatin1().constData() );
    if( group == 0 )
        return;

    QLabel *groupLabel = new QLabel( _FU8(group->label), this );
    groupLabel->setAlignment( Qt::AlignLeft );
    vLayout->addWidget( groupLabel );
    
    QFrame *separator = new QFrame( this );
    separator->setFrameShape( QFrame::HLine );
    separator->setFrameShadow( QFrame::Sunken );
    vLayout->addWidget( separator );

    /* default QVGroupBox */
    QGroupBox *defaultGroupVBox = new QGroupBox( this );
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
            QGroupBox *vbox = sd->searchGroupVBoxByCustomSym( *custom_sym );
            if( vbox == 0 )
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
    char **sub_groups
        = uim_custom_group_subgroups( m_group_sym.toLatin1().constData() );
    char **sgrp;
    for( sgrp = sub_groups; *sgrp; sgrp++ )
    {
        struct uim_custom_group *sgroup_custom =  uim_custom_group_get( *sgrp );
        QGroupBox *vbox;
        if( QString::compare( *sgrp, "main" ) == 0 )
        {
            vbox = defaultGroupVBox;
            vbox->show();
        }
        else
        {
            vbox = new QGroupBox( _FU8(sgroup_custom->label), this );
            vLayout->addWidget( vbox );
        }
        QVBoxLayout *layout = new QVBoxLayout;
        vbox->setLayout( layout );

        /* XXX quick hack to use AND expression of groups */
        QString groups( m_group_sym );
        groups += " '";
        groups += *sgrp;
        char **custom_syms
            = uim_custom_collect_by_group( groups.toLatin1().constData() );
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

    setLayout( vLayout );

    m_widget_created = true;
}

/*
 * Building up GUI in accordance with Custom Type.
 */
UimCustomItemIface *GroupPageWidget::addCustom( QGroupBox *vbox, const char *custom_sym )
{
    UimCustomItemIface *w = 0;
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
            w = 0;
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

UimCustomItemIface *GroupPageWidget::addCustomTypeBool( QGroupBox *vbox, struct uim_custom *custom )
{
    CustomCheckBox *checkBox = new CustomCheckBox( custom );
    connect( checkBox, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );
    vbox->layout()->addWidget( checkBox );

    return checkBox;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeInteger( QGroupBox *vbox, struct uim_custom *custom )
{
    QFrame *hbox = new QFrame;
    QLabel *label = new QLabel( _FU8(custom->label), hbox );
    CustomSpinBox *spinBox = new CustomSpinBox( custom, hbox );
    label->setBuddy( spinBox );
    connect( spinBox, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    QHBoxLayout *layout = new QHBoxLayout;
    layout->setMargin( 0 );
    layout->setSpacing( 6 );
    layout->addWidget( label );
    layout->addStretch();
    layout->addWidget( spinBox );

    hbox->setLayout( layout );
    vbox->layout()->addWidget( hbox );

    return spinBox;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeString( QGroupBox *vbox, struct uim_custom *custom )
{
    QFrame *hbox = new QFrame;
    QLabel *label = new QLabel( _FU8(custom->label) + ':', hbox );
    CustomLineEdit *lineEdit = new CustomLineEdit( custom, hbox );
    label->setBuddy( lineEdit );
    connect( lineEdit, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    QHBoxLayout *layout = new QHBoxLayout;
    layout->setMargin( 0 );
    layout->setSpacing( 6 );
    layout->addWidget( label );
    layout->addWidget( lineEdit );

    hbox->setLayout( layout );
    vbox->layout()->addWidget( hbox );

    return lineEdit;
}

UimCustomItemIface *GroupPageWidget::addCustomTypePathname( QGroupBox *vbox, struct uim_custom *custom )
{
    QFrame *hbox = new QFrame;
    QLabel *label = new QLabel( _FU8(custom->label), hbox );
    CustomPathnameEdit *pathnameEdit = new CustomPathnameEdit( custom, hbox );
    label->setBuddy( pathnameEdit );
    connect( pathnameEdit, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    QHBoxLayout *layout = new QHBoxLayout;
    layout->setMargin( 0 );
    layout->setSpacing( 6 );
    layout->addWidget( label );
    layout->addWidget( pathnameEdit );

    hbox->setLayout( layout );
    vbox->layout()->addWidget( hbox );

    return pathnameEdit;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeChoice( QGroupBox *vbox, struct uim_custom *custom )
{
    QFrame *hbox = new QFrame;
    QLabel *label = new QLabel( _FU8(custom->label), hbox );

    CustomChoiceCombo *choiceCombo = new CustomChoiceCombo( custom, hbox );
    label->setBuddy( choiceCombo );
    connect( choiceCombo, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    QHBoxLayout *layout = new QHBoxLayout;
    layout->setMargin( 0 );
    layout->setSpacing( 6 );
    layout->addWidget( label );
    layout->addWidget( choiceCombo );

    hbox->setLayout( layout );
    vbox->layout()->addWidget( hbox );

    return choiceCombo;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeOrderedList( QGroupBox *vbox, struct uim_custom *custom )
{
    QFrame *hbox = new QFrame;
    QLabel *label = new QLabel( _FU8(custom->label), hbox );
    CustomOrderedListEdit *olistEditBox = new CustomOrderedListEdit( custom, hbox );
    label->setBuddy( olistEditBox );
    connect( olistEditBox, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    QHBoxLayout *layout = new QHBoxLayout;
    layout->setMargin( 0 );
    layout->setSpacing( 6 );
    layout->addWidget( label );
    layout->addWidget( olistEditBox );

    hbox->setLayout( layout );
    vbox->layout()->addWidget( hbox );

    return olistEditBox;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeKey( QGroupBox *vbox, struct uim_custom *custom )
{
    QFrame *hbox = new QFrame;
    QLabel *label = new QLabel( _FU8(custom->label), hbox );
    CustomKeyEdit *keyEditBox = new CustomKeyEdit( custom, hbox );
    label->setBuddy( keyEditBox );
    connect( keyEditBox, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    QHBoxLayout *layout = new QHBoxLayout;
    layout->setMargin( 0 );
    layout->setSpacing( 6 );
    layout->addWidget( label );
    layout->addWidget( keyEditBox );

    hbox->setLayout( layout );
    vbox->layout()->addWidget( hbox );

    return keyEditBox;
}

UimCustomItemIface *GroupPageWidget::addCustomTypeTable( QGroupBox *vbox, struct uim_custom *custom )
{
    QFrame *hbox = new QFrame;
    QLabel *label = new QLabel( _FU8(custom->label), hbox );
    CustomTable *tableBox = new CustomTable( custom, hbox );
    label->setBuddy( tableBox );
    connect( tableBox, SIGNAL(customValueChanged()),
                      this, SLOT(slotCustomValueChanged()) );

    QHBoxLayout *layout = new QHBoxLayout;
    layout->setMargin( 0 );
    layout->setSpacing( 6 );
    layout->addWidget( label );
    layout->addWidget( tableBox );

    hbox->setLayout( layout );
    vbox->layout()->addWidget( hbox );

    return tableBox;
}


void GroupPageWidget::setDefault()
{
    foreach ( UimCustomItemIface *iface, m_customIfaceList )
        iface->setDefault();
}

//--------------------------------------------------------------------------------------
int main( int argc, char **argv )
{
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    bind_textdomain_codeset(PACKAGE, "UTF-8"); // ensure code encoding is UTF8-
    
    QApplication a( argc, argv );

    QCoreApplication::setOrganizationName( "uim" );
    QCoreApplication::setApplicationName( "uim" );

    UimPrefDialog *dlg = new UimPrefDialog();
    dlg->resize( DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT );
    dlg->setWindowIcon( QIcon( UIM_PIXMAPSDIR "/uim-icon.png" ) );
    dlg->show();

    return a.exec();
}
