EXTRA_DIST = meson.build

if GTK4
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

bin_PROGRAMS = uim-toolbar-gtk4

uim_toolbar_gtk4_SOURCES  = standalone-gtk4.c \
			   uim-toolbar-widget.c uim-toolbar-widget.h
uim_toolbar_gtk4_LDADD    = @GTK4_LIBS@ @GTK4_LAYER_SHELL_LIBS@ @X11_LIBS@ \
			   $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la \
			   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk4_CPPFLAGS = $(AM_CPPFLAGS)
uim_toolbar_gtk4_CFLAGS   = @GTK4_CFLAGS@ @GTK4_LAYER_SHELL_CFLAGS@ @X11_CFLAGS@
endif
