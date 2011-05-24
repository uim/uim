if GTK2
libexec_PROGRAMS = uim-candwin-gtk uim-candwin-tbl-gtk

uim_candwin_gtk_SOURCES  = gtk.c \
			   ../immodule/caret-state-indicator.c \
			   ../immodule/caret-state-indicator.h
uim_candwin_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la
uim_candwin_gtk_CPPFLAGS = -I$(top_srcdir)
uim_candwin_gtk_CFLAGS   = @GTK2_CFLAGS@

uim_candwin_tbl_gtk_SOURCES  = tbl-gtk.c \
			   ../immodule/caret-state-indicator.c \
			   ../immodule/caret-state-indicator.h
uim_candwin_tbl_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la
uim_candwin_tbl_gtk_CPPFLAGS = -I$(top_srcdir)
uim_candwin_tbl_gtk_CFLAGS   = @GTK2_CFLAGS@
endif
