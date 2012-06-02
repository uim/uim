exists(../common.pro) {
    include(../common.pro)
}

TEMPLATE = app
CONFIG -= moc
DEPENDPATH += .
INCLUDEPATH += .
CONFIG += qt warn_on debug

QMAKE_STRIP =

# Input
SOURCES += main.cpp
