#include "uim-pref-qt.h"

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

UimPrefDialog::UimPrefDialog( QWidget *parent, const char *name )
    : QDialog( parent, name )
{
    uim_init();
    uim_custom_init();
    char **g = uim_custom_primary_groups();
//    setupWidgets();
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

    QWidget *leftSideWidget = new QWidget( mainSplitter );
    QVBoxLayout *leftVLayout = new QVBoxLayout( leftSideWidget );
    QWidget *buttonHWidget = new QWidget( leftSideWidget );
    m_groupWidgetStack = new QWidgetStack( leftSideWidget );
    QHBoxLayout *buttonHLayout = new QHBoxLayout( buttonHWidget );
    buttonHLayout->insertStretch( 0 );
    buttonHLayout->setMargin( 10 );
    buttonHLayout->setSpacing( 6 );
    buttonHLayout->addWidget( new QPushButton("Apply", buttonHWidget) );
    buttonHLayout->addWidget( new QPushButton("OK", buttonHWidget) );
    buttonHLayout->addWidget( new QPushButton("Cancel", buttonHWidget ) );
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
        ;

        /*
        struct uim_custom_group *group = uim_custom_group_get( *grp );
        m_groupWidgetStack->addWidget( createGroupWidget( *grp ) );        
        uim_custom_group_free( group );
        */
    }
}

QWidget* UimPrefDialog::createGroupWidget( const char *group_name )
{
    QVBox *vbox = new QVBox( m_groupWidgetStack );
    new QPushButton( "FEFEFE", vbox );
    return vbox;
    
    struct uim_custom_group *group;
    char **custom_syms, ** custom_sym;

    group = uim_custom_group_get( group_name );
    if( group == NULL )
        return NULL;

    custom_syms = uim_custom_collect_by_group( group_name );
    if( custom_syms )
    {
        for( custom_sym = custom_syms; *custom_sym; custom_sym++ )
        {
//            addCustom( *custom_sym );
            ;
        }

        uim_custom_symbol_list_free( custom_syms );        
    }
    
    uim_custom_group_free( group );
}

void UimPrefDialog::accept()
{
    done( QDialog::Accepted );
}
void UimPrefDialog::reject()
{
    done( QDialog::Rejected );
}

int main( int argc, char **argv )
{
    QApplication a( argc, argv );

    UimPrefDialog *dlg = new UimPrefDialog();
    a.setMainWidget( dlg );
    dlg->show();

    return a.exec();
}
void UimPrefDialog::addCustom( QVBox *vbox, const char *custom_sym )
{
    ;
}
