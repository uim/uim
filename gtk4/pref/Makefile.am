EXTRA_DIST = meson.build

if GTK4
if PREF
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

bin_PROGRAMS = uim-pref-gtk4
uim_pref_gtk4_SOURCES  = gtk-gtk4.c \
			gtk-custom-widgets-gtk4.c \
			gtk-custom-widgets-gtk4.h \
			gtk-keytab.h \
			key-util-gtk4.c \
			key-util-gtk4.h
uim_pref_gtk4_LDADD    = @GTK4_LIBS@ $(top_builddir)/uim/libuim-custom.la \
			$(top_builddir)/uim/libuim-scm.la \
			$(top_builddir)/uim/libuim.la
if LIBUIM_X_UTIL
uim_pref_gtk4_LDADD += $(top_builddir)/uim/libuim-x-util.la
endif
uim_pref_gtk4_CPPFLAGS = $(helper_defs) $(AM_CPPFLAGS)
uim_pref_gtk4_CFLAGS   = @GTK4_CFLAGS@
endif
endif
