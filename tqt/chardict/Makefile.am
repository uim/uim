if TQT
AM_CPPFLAGS = 				\
	-I$(top_builddir)/uim		\
	-I$(top_srcdir)/replace		\
	-I$(top_srcdir)/tqt		\
	-I$(top_srcdir)

HEADER_FILES = \
	bushuviewwidget.h \
	unicodeviewwidget.h \
	chargridview.h \
	qt.h
MOC_FILES = \
	bushuviewwidget.moc \
	unicodeviewwidget.moc \
	chargridview.moc \
	qt.moc

noinst_HEADERS = $(HEADER_FILES)

bin_PROGRAMS = uim-chardict-tqt

uim_chardict_tqt_SOURCES = \
	qt.cpp qt.h \
	bushuviewwidget.cpp bushuviewwidget.h \
	chargridview.cpp chargridview.h \
	unicodeviewwidget.cpp unicodeviewwidget.h
uim_chardict_tqt_CXXFLAGS = @UIM_TQT_CXXFLAGS@ -DBUSHUDICT=\"$(datadir)/uim/helperdata/bushu.t\"

uim_chardict_tqt_LDFLAGS  = @UIM_TQT_LDFLAGS@ @X11_LIBS@
uim_chardict_tqt_LDADD    = $(top_builddir)/uim/libuim.la @LTLIBINTL@

qt.cpp: qt.moc

bushuviewwidget.cpp: bushuviewwidget.moc
unicodeviewwidget.cpp: unicodeviewwidget.moc
chargridview.cpp: chargridview.moc

CLEANFILES = \
	$(MOC_FILES)

.h.moc:
	$(TQMOC) $< -o $*.moc
.ui.h:
	$(TQUIC) $< -o $*.h
endif

