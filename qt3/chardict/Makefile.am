EXTRA_DIST = bushu.t

if QT
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)/qt3			\
	-I$(top_srcdir)

HEADER_FILES = \
	bushuviewwidget.h \
	unicodeviewwidget.h \
	chargridview.h \
	kseparator.h \
	qt.h
MOC_FILES = \
	bushuviewwidget.moc \
	unicodeviewwidget.moc \
	chargridview.moc \
	kseparator.moc \
	qt.moc

noinst_HEADERS = $(HEADER_FILES)

bin_PROGRAMS = uim-chardict-qt

uim_chardict_qt_SOURCES = \
	qt.cpp qt.h \
	bushuviewwidget.cpp bushuviewwidget.h \
	chargridview.cpp chargridview.h \
	kseparator.cpp kseparator.h \
	unicodeviewwidget.cpp unicodeviewwidget.h
uim_chardict_qt_CXXFLAGS = @UIM_QT_CXXFLAGS@ -DBUSHUDICT=\"$(datadir)/uim/helperdata/bushu.t\"

uim_chardict_qt_LDFLAGS  = @UIM_QT_LDFLAGS@
uim_chardict_qt_LDADD    = $(top_builddir)/uim/libuim.la @LTLIBINTL@
qt.cpp: qt.moc

bushuviewwidget.cpp: bushuviewwidget.moc
unicodeviewwidget.cpp: unicodeviewwidget.moc
chargridview.cpp: chargridview.moc
kseparator.cpp: kseparator.moc
helperdatadir = $(datadir)/uim/helperdata
helperdata_DATA = bushu.t

CLEANFILES = \
	$(MOC_FILES)

.h.moc: 
	$(MOC) $< -o $*.moc
.ui.h: 
	$(UIC) $< -o $*.h
endif

