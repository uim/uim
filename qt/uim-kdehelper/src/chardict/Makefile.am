INCLUDES = $(all_includes)

bin_PROGRAMS = uim-chardict-qt

noinst_HEADERS = uim-chardict-qt.h
METASOURCES = AUTO

# chardict
uim_chardict_qt_SOURCES = uim-chardict-qt.cpp
uim_chardict_qt_CPPFLAGS = -DBUSHUDICT=\"$(kde_datadir)/uim-kdehelper/dict/bushu.t\"
uim_chardict_qt_LDADD = $(LIB_QT) $(LIB_UIM) $(LIB_KDECORE)
uim_chardict_qt_LDFLAGS = $(all_libraries) $(KDE_RPATH)
