if GTK3
bin_PROGRAMS = uim-im-switcher-gtk3
uim_im_switcher_gtk3_SOURCES  = ../../gtk2/switcher/gtk.c
uim_im_switcher_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim-custom.la \
			       $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim.la
uim_im_switcher_gtk3_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_im_switcher_gtk3_CFLAGS   = @GTK3_CFLAGS@

endif
