INCLUDES = $(all_includes)

bin_PROGRAMS = uim-im-switcher-qt

noinst_HEADERS = im-switcher-qt.h
METASOURCES = AUTO

# switcher
uim_im_switcher_qt_SOURCES = im-switcher-qt.cpp
uim_im_switcher_qt_LDADD = $(LIB_QT) $(LIB_UIM) $(LIB_KDECORE)
uim_im_switcher_qt_LDFLAGS = $(all_libraries) $(KDE_RPATH)
