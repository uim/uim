AUTOMAKE_OPTIONS = foreign

uimnotify_plugin_LTLIBRARIES =
uimnotify_plugindir = $(pkglibdir)/notify

if LIBNOTIFY
uimnotify_plugin_LTLIBRARIES += libuimnotify-libnotify.la
libuimnotify_libnotify_la_SOURCES = uim-libnotify.c uim-libnotify.h
libuimnotify_libnotify_la_CFLAGS = @LIBNOTIFY_CFLAGS@
libuimnotify_libnotify_la_LDFLAGS  = -avoid-version -module
libuimnotify_libnotify_la_LIBADD = \
	$(top_builddir)/uim/libuim.la \
	@LIBNOTIFY_LIBS@
endif

if KNOTIFY
uimnotify_plugin_LTLIBRARIES += libuimnotify-knotify.la
libuimnotify_knotify_la_SOURCES = uim-knotify.cc uim-knotify.h
libuimnotify_knotify_la_CXXFLAGS = -I@KNOTIFY_INCLUDE_DIR@ $(QT_CXXFLAGS)
libuimnotify_knotify_la_LDFLAGS  = -avoid-version -module -L@KNOTIFY_LIB_DIR@ $(QT_LDFLAGS)
libuimnotify_knotify_la_LIBADD = \
	$(top_builddir)/uim/libuim.la \
	-lkdeui -lkdecore
endif
