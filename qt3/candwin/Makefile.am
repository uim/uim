if QT
INCLUDES += -I$(top_srcdir) -I$(top_builddir) -I..

HEADER_FILES = qt.h
MOC_FILES = qt.moc

libexec_PROGRAMS = uim-candwin-qt

# Candidate Window
uim_candwin_qt_SOURCES  = qt.cpp qt.h ../qtgettext.h
uim_candwin_qt_CXXFLAGS = $(UIM_QT_CXXFLAGS)
uim_candwin_qt_LDFLAGS  = $(UIM_QT_LDFLAGS)
uim_candwin_qt_LDADD = $(top_builddir)/uim/libuim-scm.la \
		       $(top_builddir)/uim/libuim.la
qt.cpp: qt.moc

CLEANFILES = \
	$(MOC_FILES) \
	$(UI_TMP_FILES)

.h.moc: 
	$(MOC) $< -o $*.moc
.ui.h: 
	$(UIC) $< -o $*.h

endif
