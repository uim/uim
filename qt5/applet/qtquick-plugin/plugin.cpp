#include "plugin.h"

#include <QtQml/QtQml>

#include "uim-status.h"

void MyPlugin::registerTypes(const char* uri) {
    // Register our 'MyQuickItem' in qml engine
    qmlRegisterType<MyQuickItem>(uri, 1, 0, "MyQuickItem");
}
