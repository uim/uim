#include <qinputcontextplugin.h>
#include <qstringlist.h>

class UimInputContextPlugin : public QInputContextPlugin
{
    Q_OBJECT
public:
    UimInputContextPlugin();
    ~UimInputContextPlugin();

    QStringList keys() const;
    QInputContext *create( const QString &key );
    QStringList languages( const QString &key );
    QString displayName( const QString &key );
    QString description( const QString &key );

protected:
    void uimInit();
    void uimQuit();

    QStringList createImList() const;
    QStringList createLanguageList( const QString &key ) const;

    bool uimReady;
};

