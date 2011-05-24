if GTK2
bin_PROGRAMS = uim-input-pad-ja

uim_input_pad_ja_SOURCES  = ja.c
uim_input_pad_ja_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			    $(top_builddir)/uim/libuim.la
uim_input_pad_ja_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_input_pad_ja_CFLAGS   = @GTK2_CFLAGS@
endif
