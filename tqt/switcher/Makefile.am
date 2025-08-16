if TQT
AM_CPPFLAGS =				\
	-I$(top_builddir)		\
	-I$(top_srcdir)/replace		\
	-I$(top_srcdir)/tqt		\
	-I$(top_srcdir)

HEADER_FILES = qt.h

MOC_FILES = qt.moc

noinst_HEADERS = $(HEADER_FILES)

bin_PROGRAMS = uim-im-switcher-tqt

# Switcher
uim_im_switcher_tqt_SOURCES  = qt.cpp qt.h ../qtgettext.h
uim_im_switcher_tqt_CXXFLAGS = $(UIM_TQT_CXXFLAGS)
uim_im_switcher_tqt_LDFLAGS  = $(UIM_TQT_LDFLAGS)
uim_im_switcher_tqt_LDADD    = $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim.la \
			       $(top_builddir)/uim/libuim-custom.la
qt.cpp: qt.moc

CLEANFILES = \
	$(MOC_FILES) \
	$(UI_TMP_FILES)

.h.moc:
	$(TQMOC) $< -o $*.moc
.ui.h:
	$(TQUIC) $< -o $*.h
endif
