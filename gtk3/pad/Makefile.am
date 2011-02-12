if GTK3
bin_PROGRAMS = uim-input-pad-ja-gtk3
uim_input_pad_ja_gtk3_SOURCES  = ../../helper/input-pad-ja.c
uim_input_pad_ja_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			    $(top_builddir)/uim/libuim.la
uim_input_pad_ja_gtk3_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_input_pad_ja_gtk3_CFLAGS   = @GTK3_CFLAGS@

endif
