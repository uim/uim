SUBDIRS = po
EXTRA_DIST = bushu.t

if QT
INCLUDES += -I$(top_srcdir) -I$(top_builddir)

HEADER_FILES = \
	chardict-bushuviewwidget.h \
	chardict-unicodeviewwidget.h \
	chardict-chargridview.h \
	chardict-kseparator.h \
	chardict-qt.h
MOC_FILES = \
	chardict-bushuviewwidget.moc \
	chardict-unicodeviewwidget.moc \
	chardict-chargridview.moc \
	chardict-kseparator.moc \
	chardict-qt.moc

noinst_HEADERS = $(HEADER_FILES)

bin_PROGRAMS = uim-chardict-qt

uim_chardict_qt_SOURCES = \
	chardict-qt.cpp chardict-qt.h \
	chardict-bushuviewwidget.cpp chardict-bushuviewwidget.h \
	chardict-chargridview.cpp chardict-chargridview.h \
	chardict-kseparator.cpp chardict-kseparator.h \
	chardict-unicodeviewwidget.cpp chardict-unicodeviewwidget.h \
	qtgettext.h
uim_chardict_qt_CXXFLAGS = @UIM_QT_CXXFLAGS@ -DBUSHUDICT=\"$(datadir)/uim/helperdata/bushu.t\"

uim_chardict_qt_LDFLAGS  = @UIM_QT_LDFLAGS@
uim_chardict_qt_LDADD    = @LTLIBINTL@
chardict-qt.cpp: chardict-qt.moc

chardict-bushuviewwidget.cpp: chardict-bushuviewwidget.moc
chardict-unicodeviewwidget.cpp: chardict-unicodeviewwidget.moc
chardict-chargridview.cpp: chardict-chargridview.moc
chardict-kseparator.cpp: chardict-kseparator.moc
helperdatadir = $(datadir)/uim/helperdata
helperdata_DATA = bushu.t

CLEANFILES = \
	$(MOC_FILES)

.h.moc: 
	$(MOC) $< -o $*.moc
.ui.h: 
	$(UIC) $< -o $*.h
endif

