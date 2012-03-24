TEMPLATE = app
CONFIG -= moc
DEPENDPATH += .
INCLUDEPATH += .
CONFIG += qt warn_on debug

greaterThan(QT_MAJOR_VERSION, 4) {
    QT += widgets
}

QMAKE_STRIP =

# Input
SOURCES += main.cpp
