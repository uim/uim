if GTK3
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

bin_PROGRAMS = uim-input-pad-ja-gtk3
uim_input_pad_ja_gtk3_SOURCES  = ../../gtk2/pad/ja.c
uim_input_pad_ja_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			    $(top_builddir)/uim/libuim.la
uim_input_pad_ja_gtk3_CPPFLAGS = $(AM_CPPFLAGS)
uim_input_pad_ja_gtk3_CFLAGS   = @GTK3_CFLAGS@

endif
