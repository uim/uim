uim_gtk4_im_module_path = $(libdir)/gtk-4.0

moduledir = $(uim_gtk4_im_module_path)/@GTK4_BINARY_VERSION@/immodules

if GTK4
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

libim_uim_la = libim-uim.la
libim_uim_la_CPPFLAGS = $(AM_CPPFLAGS)
libim_uim_la_CFLAGS = @GTK4_CFLAGS@
module_LTLIBRARIES = $(libim_uim_la)

libim_uim_la_SOURCES = $(LIBIM_UIM_SOURCES)
libim_uim_la_LDFLAGS = -module -avoid-version
libim_uim_la_LIBADD = $(top_builddir)/uim/libuim-scm.la \
		      $(top_builddir)/uim/libuim.la \
		      $(top_builddir)/uim/libuim-counted-init.la
libim_uim_la_LIBADD += @GTK4_LIBS@
libim_uim_la_LIBADD += -lm

LIBIM_UIM_SOURCES = \
	gtk-im-uim.c \
	uim-im-context.c uim-im-context.h
endif
