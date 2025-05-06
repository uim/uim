if GTK2
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

libexec_PROGRAMS = uim-candwin-gtk uim-candwin-tbl-gtk uim-candwin-horizontal-gtk

uim_candwin_gtk_SOURCES  = gtk.c \
			   ../immodule/caret-state-indicator.c \
			   ../immodule/caret-state-indicator.h
uim_candwin_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la
uim_candwin_gtk_CPPFLAGS = $(AM_CPPFLAGS)
uim_candwin_gtk_CFLAGS   = @GTK2_CFLAGS@

uim_candwin_tbl_gtk_SOURCES  = tbl-gtk.c \
			   ../immodule/caret-state-indicator.c \
			   ../immodule/caret-state-indicator.h
uim_candwin_tbl_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la
uim_candwin_tbl_gtk_CPPFLAGS = $(AM_CPPFLAGS)
uim_candwin_tbl_gtk_CFLAGS   = @GTK2_CFLAGS@

uim_candwin_horizontal_gtk_SOURCES  = horizontal-gtk.c \
			   ../immodule/caret-state-indicator.c \
			   ../immodule/caret-state-indicator.h
uim_candwin_horizontal_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la -lm
uim_candwin_horizontal_gtk_CPPFLAGS = $(AM_CPPFLAGS)
uim_candwin_horizontal_gtk_CFLAGS   = @GTK2_CFLAGS@
endif
