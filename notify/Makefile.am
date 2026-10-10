AUTOMAKE_OPTIONS = foreign

AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/uim			\
	-I$(top_srcdir)/replace

uimnotify_plugin_LTLIBRARIES =
uimnotify_plugindir = $(pkglibdir)/notify

if LIBNOTIFY
# XXX
CFLAGS+= -Wno-error
uimnotify_plugin_LTLIBRARIES += libuimnotify-libnotify.la
libuimnotify_libnotify_la_SOURCES = uim-libnotify.c
libuimnotify_libnotify_la_CFLAGS = @LIBNOTIFY_CFLAGS@
libuimnotify_libnotify_la_LDFLAGS  = -avoid-version -module
libuimnotify_libnotify_la_LIBADD = @LIBNOTIFY_LIBS@
endif
