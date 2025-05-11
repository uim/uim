if GTK3
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

libexec_PROGRAMS =				\
	uim-candwin-gtk3			\
	uim-candwin-tbl-gtk3			\
	uim-candwin-horizontal-gtk3

sources =					\
	gtk.c					\
	../immodule/caret-state-indicator.c	\
	../immodule/caret-state-indicator.h	\
	../immodule/uim-cand-win-gtk.c		\
	../immodule/uim-cand-win-gtk.h
uim_candwin_gtk3_SOURCES  =			\
	$(sources)				\
	../immodule/uim-cand-win-vertical-gtk.c	\
	../immodule/uim-cand-win-vertical-gtk.h
uim_candwin_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la
uim_candwin_gtk3_CPPFLAGS = $(AM_CPPFLAGS)
uim_candwin_gtk3_CFLAGS   = @GTK3_CFLAGS@

uim_candwin_tbl_gtk3_SOURCES  =			\
	$(sources)				\
	../immodule/uim-cand-win-tbl-gtk.c	\
	../immodule/uim-cand-win-tbl-gtk.h
uim_candwin_tbl_gtk3_LDADD    = $(uim_candwin_gtk3_LDADD)
uim_candwin_tbl_gtk3_CPPFLAGS =			\
	$(uim_candwin_gtk3_CPPFLAGS)		\
	-DUIM_CANDWIN_STYLE_TABLE
uim_candwin_tbl_gtk3_CFLAGS   = $(uim_candwin_gtk3_CFLAGS)

uim_candwin_horizontal_gtk3_SOURCES  =			\
	$(sources)					\
	../immodule/uim-cand-win-horizontal-gtk.c	\
	../immodule/uim-cand-win-horizontal-gtk.h
uim_candwin_horizontal_gtk3_LDADD    = $(uim_candwin_gtk3_LDADD)
uim_candwin_horizontal_gtk3_CPPFLAGS =		\
	$(uim_candwin_gtk3_CPPFLAGS)		\
	-DUIM_CANDWIN_STYLE_HORIZONTAL
uim_candwin_horizontal_gtk3_CFLAGS   = $(uim_candwin_gtk3_CFLAGS)
endif
