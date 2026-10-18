EXTRA_DIST = meson.build

if GTK4
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

bin_PROGRAMS = uim-input-pad-ja-gtk4
uim_input_pad_ja_gtk4_SOURCES  = ja-gtk4.c
uim_input_pad_ja_gtk4_LDADD    = @GTK4_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim.la
uim_input_pad_ja_gtk4_CPPFLAGS = $(AM_CPPFLAGS)
uim_input_pad_ja_gtk4_CFLAGS   = @GTK4_CFLAGS@
endif
