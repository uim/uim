EXTRA_DIST = keyeditformbase.ui  olisteditformbase.ui

if QT
AM_CPPFLAGS += -I$(top_srcdir) -I$(top_builddir) -I..

if PREF
HEADER_FILES = \
	customwidgets.h \
	kseparator.h \
	qt.h

MOC_FILES = \
	customwidgets.moc \
	kseparator.moc \
	qt.moc

UI_TMP_FILES = \
	olisteditformbase.cpp \
	olisteditformbase.h \
	olisteditformbase.moc \
	keyeditformbase.cpp \
	keyeditformbase.h \
	keyeditformbase.moc

noinst_HEADERS = $(HEADER_FILES)

bin_PROGRAMS = \
	uim-pref-qt

nodist_uim_pref_qt_SOURCES = \
	olisteditformbase.cpp \
	olisteditformbase.h \
	keyeditformbase.cpp \
	keyeditformbase.h
uim_pref_qt_SOURCES = \
	qt.cpp customwidgets.cpp kseparator.cpp \
	../qtgettext.h qt.h \
	customwidgets.h kseparator.h

uim_pref_qt_CXXFLAGS = $(UIM_QT_CXXFLAGS)
uim_pref_qt_LDFLAGS  = $(UIM_QT_LDFLAGS)
uim_pref_qt_LDADD    = $(top_builddir)/uim/libuim-counted-init.la \
		       $(top_builddir)/uim/libuim-custom.la \
		       $(top_builddir)/uim/libuim-scm.la \
		       $(top_builddir)/uim/libuim.la

customwidgets.h:olisteditformbase.h keyeditformbase.h

qt.cpp: qt.moc olisteditformbase.h keyeditformbase.h
customwidgets.cpp: customwidgets.moc
kseparator.cpp: kseparator.moc

UIC_IMPL_FLAGS = -pch "qtgettext.h" -tr mygettext

olisteditformbase.cpp: olisteditformbase.h olisteditformbase.moc
	$(UIC) $(UIC_IMPL_FLAGS) $(top_srcdir)/qt3/pref/olisteditformbase.ui -i olisteditformbase.h > olisteditformbase.cpp.tmp
	echo '#include "olisteditformbase.moc"' >> olisteditformbase.cpp.tmp
	mv olisteditformbase.cpp.tmp olisteditformbase.cpp
olisteditformbase.h:
	$(UIC) $(top_srcdir)/qt3/pref/olisteditformbase.ui -o olisteditformbase.h

keyeditformbase.cpp: keyeditformbase.h keyeditformbase.moc
	$(UIC) $(UIC_IMPL_FLAGS) $(top_srcdir)/qt3/pref/keyeditformbase.ui -i keyeditformbase.h > keyeditformbase.cpp.tmp
	echo '#include "keyeditformbase.moc"' >> keyeditformbase.cpp.tmp
	mv keyeditformbase.cpp.tmp keyeditformbase.cpp
keyeditformbase.h:
	$(UIC) $(top_srcdir)/qt3/pref/keyeditformbase.ui -o keyeditformbase.h
endif

CLEANFILES = \
	$(MOC_FILES) \
	$(UI_TMP_FILES)

.h.moc: 
	$(MOC) $< -o $*.moc
.ui.h: 
	$(UIC) $< -o $*.h
endif

