INCLUDES = $(all_includes)

noinst_HEADERS = uimhelpersystray.h
METASOURCES = AUTO

bin_PROGRAMS = uim-toolbar-qt-systray

# systray
uim_toolbar_qt_systray_SOURCES = uimhelpersystray.cpp
uim_toolbar_qt_systray_LDADD = $(LIB_KDEUI) ../common/libcommon.la
uim_toolbar_qt_systray_LDFLAGS = $(all_libraries) $(KDE_RPATH)

