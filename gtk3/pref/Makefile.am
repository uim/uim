if GTK3
if PREF
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

bin_PROGRAMS = uim-pref-gtk3
uim_pref_gtk3_SOURCES  = gtk.c \
			gtk-custom-widgets.c \
			gtk-custom-widgets.h \
			gtk-keytab.h \
			../immodule/key-util-gtk.c \
			../immodule/key-util-gtk.h
uim_pref_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-custom.la \
			$(top_builddir)/uim/libuim-scm.la \
			$(top_builddir)/uim/libuim.la
if LIBUIM_X_UTIL
uim_pref_gtk3_LDADD += $(top_builddir)/uim/libuim-x-util.la
endif
uim_pref_gtk3_CPPFLAGS = $(helper_defs) $(AM_CPPFLAGS)
uim_pref_gtk3_CFLAGS   = @GTK3_CFLAGS@
endif
endif
