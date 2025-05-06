if GTK3
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

libexec_PROGRAMS = uim-candwin-gtk3 uim-candwin-tbl-gtk3 uim-candwin-horizontal-gtk3
uim_candwin_gtk3_SOURCES  = gtk.c \
			   ../immodule/caret-state-indicator.c \
			   ../immodule/caret-state-indicator.h
uim_candwin_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la
uim_candwin_gtk3_CPPFLAGS = $(AM_CPPFLAGS)
uim_candwin_gtk3_CFLAGS   = @GTK3_CFLAGS@

uim_candwin_tbl_gtk3_SOURCES  = tbl-gtk.c \
			   ../immodule/caret-state-indicator.c \
			   ../immodule/caret-state-indicator.h
uim_candwin_tbl_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la
uim_candwin_tbl_gtk3_CPPFLAGS = $(AM_CPPFLAGS)
uim_candwin_tbl_gtk3_CFLAGS   = @GTK3_CFLAGS@

uim_candwin_horizontal_gtk3_SOURCES  = horizontal-gtk.c \
			   ../immodule/caret-state-indicator.c \
			   ../immodule/caret-state-indicator.h
uim_candwin_horizontal_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la
uim_candwin_horizontal_gtk3_CPPFLAGS = $(AM_CPPFLAGS)
uim_candwin_horizontal_gtk3_CFLAGS   = @GTK3_CFLAGS@
endif
