if GTK3
if PREF

helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

bin_PROGRAMS = uim-pref-gtk3
uim_pref_gtk3_SOURCES  = ../../helper/pref-gtk.c \
			../../helper/pref-gtk-custom-widgets.c \
			../../helper/pref-gtk-custom-widgets.h \
			../../helper/pref-gtk-keytab.h \
			../../gtk2/immodule/key-util-gtk.c \
			../../gtk2/immodule/key-util-gtk.h
uim_pref_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-custom.la \
			$(top_builddir)/uim/libuim-scm.la \
			$(top_builddir)/uim/libuim.la
if LIBUIM_X_UTIL
uim_pref_gtk3_LDADD += $(top_builddir)/uim/libuim-x-util.la
endif
uim_pref_gtk3_CPPFLAGS = $(helper_defs) -I$(top_srcdir) -I$(top_builddir) \
			-I$(top_srcdir)/helper
uim_pref_gtk3_CFLAGS   = @GTK3_CFLAGS@
endif
endif
