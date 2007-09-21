#include "plugin.h"

#include <qapplication.h>
#include <qinputcontextplugin.h>
#include <qinputcontext.h>
#include <qstringlist.h>

#include <uim/uim.h>

#include "quiminfomanager.h"
#include "quiminputcontext_with_slave.h"

#define UIM_QT_LIST_SUBIM_AS_QTIM 0

QUimInfoManager *UimInputContextPlugin::infoManager = NULL;


UimInputContextPlugin::UimInputContextPlugin()
{
    uimReady = false;
    uimInit();
}

UimInputContextPlugin::~UimInputContextPlugin()
{
    uimQuit();
}

QStringList UimInputContextPlugin::keys() const
{
    return createImList();
}

QInputContext *UimInputContextPlugin::create( const QString & key )
{
    QString imname = QString::null;

#if UIM_QT_LIST_SUBIM_AS_QTIM
    if ( key.startsWith( "uim-" ) )
	imname = key.mid( 4 );
    else
#endif
    if ( key == "uim" )
        imname = uim_get_default_im_name( setlocale( LC_ALL, NULL ) );

    QStringList langs = createLanguageList( key );
    QUimInputContext *uic = new QUimInputContext( imname.toUtf8(),
						  langs[ 0 ].toUtf8() );

    return uic;
}

QStringList UimInputContextPlugin::languages( const QString & key )
{
    return createLanguageList( key );
}

QString UimInputContextPlugin::displayName( const QString & key )
{
    return QString( key ) + " (" + languages( key ) [ 0 ] + ")";
}

QString UimInputContextPlugin::description( const QString & key )
{
    return displayName( key ) + ": an input method provided via the uim input method framework";
}

QUimInfoManager *
UimInputContextPlugin::getQUimInfoManager()
{
    return infoManager;
}

void UimInputContextPlugin::uimInit()
{
    if ( !uim_init() ) {
        if (!infoManager)
            infoManager = new QUimInfoManager();

        uimReady = true;
    }
}

void UimInputContextPlugin::uimQuit()
{
    if ( uimReady )
    {
        uim_quit();
        delete infoManager;
        uimReady = false;
    }
}



QStringList UimInputContextPlugin::createImList() const
{
    QStringList lst;

    // default
    lst.append( "uim" );
    qDebug( "name = uim" );

#if UIM_QT_LIST_SUBIM_AS_QTIM
    uim_context tmp_uc = uim_create_context( NULL, "UTF-8",
                         NULL, NULL, uim_iconv, NULL );
    int nr = uim_get_nr_im( tmp_uc );
    if ( uimReady )
    {
        for ( int i = 0; i < nr; i++ )
        {
            const char *name = uim_get_im_name( tmp_uc, i );
            QString qs( name );
            qs = "uim-" + qs;
            lst << qs;

            qDebug( "name = %s", ( const char* ) qs.toUtf8() );
        }
    }
    uim_release_context( tmp_uc );
#endif

    return lst;
}

QStringList UimInputContextPlugin::createLanguageList( const QString &key ) const
{
    if ( key == "uim" )
	return QStringList() << "ja" << "ko" << "zh" << "*";

#if UIM_QT_LIST_SUBIM_AS_QTIM
    uim_context tmp_uc = uim_create_context( NULL, "UTF-8",
                         NULL, NULL, uim_iconv, NULL );
    int nr = uim_get_nr_im( tmp_uc );
    if ( uimReady )
    {
        for ( int i = 0; i < nr; i++ )
        {
            const char *name = uim_get_im_name( tmp_uc, i );
            const char *lang = uim_get_im_language( tmp_uc, i );

            if ( key == QString( "uim-" ) + name )
            {
                // ":" separated languages for future extension
                QStringList langs = QString( lang ).split( ":" );
                return langs;
            }
        }
    }
    uim_release_context( tmp_uc );
#endif

    return QStringList( "" );
}

Q_EXPORT_PLUGIN2( uiminputcontextplugin, UimInputContextPlugin )
