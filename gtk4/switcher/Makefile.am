EXTRA_DIST = meson.build

if GTK4
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

bin_PROGRAMS = uim-im-switcher-gtk4

uim_im_switcher_gtk4_SOURCES  = uim-im-switcher-gtk4.c
uim_im_switcher_gtk4_LDADD    = @GTK4_LIBS@ \
			       $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim.la \
			       $(top_builddir)/uim/libuim-custom.la
uim_im_switcher_gtk4_CPPFLAGS = $(AM_CPPFLAGS)
uim_im_switcher_gtk4_CFLAGS   = @GTK4_CFLAGS@
endif
