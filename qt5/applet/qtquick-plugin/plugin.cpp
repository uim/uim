#include "plugin.h"

#include <QtQml/QtQml>

#include "uim-status.h"

void UimPlugin::registerTypes(const char* uri) {
    // Register our 'MyQuickItem' in qml engine
    qmlRegisterType<UimStatus>(uri, 1, 0, "MyQuickItem");
}
