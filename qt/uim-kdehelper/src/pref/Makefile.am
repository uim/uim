INCLUDES = $(all_includes)
METASOURCES = AUTO

noinst_HEADERS = uim-pref-qt.h customwidgets.h kseparator.h

bin_PROGRAMS = uim-pref-qt
uim_pref_qt_SOURCES = uim-pref-qt.cpp customwidgets.cpp kseparator.cpp \
                      olisteditformbase.ui keyeditformbase.ui keygrabformbase.ui
uim_pref_qt_LDFLAGS = $(all_libraries) $(KDE_RPATH) -luim -luim-custom
uim_pref_qt_LDADD = $(LIB_QT) $(LIB_KUTILS)
