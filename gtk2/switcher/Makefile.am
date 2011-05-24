if GTK2
bin_PROGRAMS = uim-im-switcher-gtk

uim_im_switcher_gtk_SOURCES  = gtk.c
uim_im_switcher_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim-custom.la \
			       $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim.la
uim_im_switcher_gtk_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_im_switcher_gtk_CFLAGS   = @GTK2_CFLAGS@
endif
