EXTRA_DIST = meson.build

if GTK4
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

libexec_PROGRAMS =				\
	uim-candwin-gtk4			\
	uim-candwin-tbl-gtk4			\
	uim-candwin-horizontal-gtk4

sources =					\
	gtk-gtk4.c				\
	cand-win-x11-util.c			\
	cand-win-x11-util.h			\
	caret-state-indicator-gtk4.c		\
	caret-state-indicator-gtk4.h		\
	uim-cand-win-gtk4.c			\
	uim-cand-win-gtk4.h

uim_candwin_gtk4_SOURCES  =			\
	$(sources)				\
	uim-cand-win-vertical-gtk4.c		\
	uim-cand-win-vertical-gtk4.h
uim_candwin_gtk4_LDADD    = @GTK4_LIBS@ @X11_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la
uim_candwin_gtk4_CPPFLAGS = $(AM_CPPFLAGS)
uim_candwin_gtk4_CFLAGS   = @GTK4_CFLAGS@ @X11_CFLAGS@

uim_candwin_tbl_gtk4_SOURCES  =		\
	$(sources)				\
	uim-cand-win-tbl-gtk4.c			\
	uim-cand-win-tbl-gtk4.h
uim_candwin_tbl_gtk4_LDADD    = $(uim_candwin_gtk4_LDADD)
uim_candwin_tbl_gtk4_CPPFLAGS =		\
	$(uim_candwin_gtk4_CPPFLAGS)		\
	-DUIM_CANDWIN_STYLE_TABLE
uim_candwin_tbl_gtk4_CFLAGS   = $(uim_candwin_gtk4_CFLAGS)

uim_candwin_horizontal_gtk4_SOURCES  =		\
	$(sources)				\
	uim-cand-win-horizontal-gtk4.c		\
	uim-cand-win-horizontal-gtk4.h
uim_candwin_horizontal_gtk4_LDADD    = $(uim_candwin_gtk4_LDADD)
uim_candwin_horizontal_gtk4_CPPFLAGS =		\
	$(uim_candwin_gtk4_CPPFLAGS)		\
	-DUIM_CANDWIN_STYLE_HORIZONTAL
uim_candwin_horizontal_gtk4_CFLAGS   = $(uim_candwin_gtk4_CFLAGS)
endif
