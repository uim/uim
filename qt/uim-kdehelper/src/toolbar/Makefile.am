INCLUDES = $(all_includes)

bin_PROGRAMS = uim-toolbar-qt

noinst_HEADERS = draghandler.h
METASOURCES = AUTO

# toolbar
uim_toolbar_qt_SOURCES = uim-toolbar-qt.cpp draghandler.cpp
uim_toolbar_qt_LDADD = $(all_libraries) $(LIB_KDECORE) ../common/libcommon.la
uim_toolbar_qt_LDFLAGS = $(all_libraries) $(KDE_RPATH)
