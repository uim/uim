if QT
INCLUDES += -I$(top_srcdir) -I$(top_builddir) -I..
QT_CXXFLAGS = $(UIM_QT_CXXFLAGS)
QT_LDFLAGS  = $(UIM_QT_LDFLAGS)

HEADER_FILES = qt.h

MOC_FILES = qt.moc

noinst_HEADERS = $(HEADER_FILES)

bin_PROGRAMS = uim-im-switcher-qt

# Switcher
uim_im_switcher_qt_SOURCES  = qt.cpp qt.h ../qtgettext.h
uim_im_switcher_qt_CXXFLAGS = $(QT_CXXFLAGS)
uim_im_switcher_qt_LDFLAGS  = $(QT_LDFLAGS)
uim_im_switcher_qt_LDADD    = $(top_builddir)/uim/libuim-scm.la \
			      $(top_builddir)/uim/libuim.la \
			      $(top_builddir)/uim/libuim-custom.la
qt.cpp: qt.moc

CLEANFILES = \
	$(MOC_FILES) \
	$(UI_TMP_FILES)

.h.moc: 
	$(MOC) $< -o $*.moc
.ui.h: 
	$(UIC) $< -o $*.h
endif
