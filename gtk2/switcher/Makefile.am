if GTK2
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

bin_PROGRAMS = uim-im-switcher-gtk

uim_im_switcher_gtk_SOURCES  = gtk.c
uim_im_switcher_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim-custom.la \
			       $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim.la
uim_im_switcher_gtk_CPPFLAGS = $(AM_CPPFLAGS)
uim_im_switcher_gtk_CFLAGS   = @GTK2_CFLAGS@
endif
