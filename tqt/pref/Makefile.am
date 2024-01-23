EXTRA_DIST = keyeditformbase.ui  olisteditformbase.ui

if TQT
AM_CPPFLAGS =				\
	-I$(top_builddir)/uim		\
	-I$(top_srcdir)/replace		\
	-I$(top_srcdir)/tqt		\
	-I$(top_srcdir)

if PREF
HEADER_FILES = \
	customwidgets.h \
	qt.h

MOC_FILES = \
	customwidgets.moc \
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
	uim-pref-tqt

nodist_uim_pref_tqt_SOURCES = \
	olisteditformbase.cpp \
	olisteditformbase.h \
	keyeditformbase.cpp \
	keyeditformbase.h
uim_pref_tqt_SOURCES = \
	qt.cpp customwidgets.cpp \
	../qtgettext.h qt.h \
	customwidgets.h

uim_pref_tqt_CXXFLAGS = $(UIM_TQT_CXXFLAGS)
uim_pref_tqt_LDFLAGS  = $(UIM_TQT_LDFLAGS)
uim_pref_tqt_LDADD    = $(top_builddir)/uim/libuim-counted-init.la \
		        $(top_builddir)/uim/libuim-custom.la \
		        $(top_builddir)/uim/libuim-scm.la \
		        $(top_builddir)/uim/libuim.la

customwidgets.h:olisteditformbase.h keyeditformbase.h

qt.cpp: qt.moc olisteditformbase.h keyeditformbase.h
customwidgets.cpp: customwidgets.moc

TQUIC_IMPL_FLAGS= -pch "qtgettext.h" -tr mygettext

olisteditformbase.cpp: olisteditformbase.h olisteditformbase.moc
	$(TQUIC) $(TQUIC_IMPL_FLAGS) $(top_srcdir)/tqt/pref/olisteditformbase.ui -i olisteditformbase.h > olisteditformbase.cpp.tmp
	echo '#include "olisteditformbase.moc"' >> olisteditformbase.cpp.tmp
	mv olisteditformbase.cpp.tmp olisteditformbase.cpp
olisteditformbase.h:
	$(TQUIC) $(top_srcdir)/tqt/pref/olisteditformbase.ui -o olisteditformbase.h

keyeditformbase.cpp: keyeditformbase.h keyeditformbase.moc
	$(TQUIC) $(TQUIC_IMPL_FLAGS) $(top_srcdir)/tqt/pref/keyeditformbase.ui -i keyeditformbase.h > keyeditformbase.cpp.tmp
	echo '#include "keyeditformbase.moc"' >> keyeditformbase.cpp.tmp
	mv keyeditformbase.cpp.tmp keyeditformbase.cpp
keyeditformbase.h:
	$(TQUIC) $(top_srcdir)/tqt/pref/keyeditformbase.ui -o keyeditformbase.h
endif

CLEANFILES = \
	$(MOC_FILES) \
	$(UI_TMP_FILES)

.h.moc:
	$(TQMOC) $< -o $@
.ui.h:
	$(TQUIC) $< -o $@
endif

