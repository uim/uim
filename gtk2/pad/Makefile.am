if GTK2
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

bin_PROGRAMS = uim-input-pad-ja

uim_input_pad_ja_SOURCES  = ja.c
uim_input_pad_ja_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			    $(top_builddir)/uim/libuim.la
uim_input_pad_ja_CPPFLAGS = $(AM_CPPFLAGS)
uim_input_pad_ja_CFLAGS   = @GTK2_CFLAGS@
endif
