EXTRA_DIST = meson.build

AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

if GTK3
bin_PROGRAMS = uim-toolbar-gtk3 uim-toolbar-gtk3-systray
uim_toolbar_gtk3_SOURCES  = standalone-gtk.c common-gtk.c
uim_toolbar_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la \
			   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk3_CPPFLAGS = $(AM_CPPFLAGS)
uim_toolbar_gtk3_CFLAGS   = @GTK3_CFLAGS@

uim_toolbar_gtk3_systray_SOURCES  = systray-gtk.c \
				   common-gtk.c \
				   eggtrayicon.c \
				   eggtrayicon.h
uim_toolbar_gtk3_systray_LDADD    = @GTK3_LIBS@ \
				   @X11_LIBS@ \
				   $(top_builddir)/uim/libuim-scm.la \
				   $(top_builddir)/uim/libuim.la \
				   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk3_systray_CPPFLAGS = $(AM_CPPFLAGS)
uim_toolbar_gtk3_systray_CFLAGS   = @GTK3_CFLAGS@
endif
