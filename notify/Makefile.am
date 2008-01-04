AUTOMAKE_OPTIONS = foreign

uimnotify_plugin_LTLIBRARIES =
uimnotify_plugindir = $(pkglibdir)/notify

if LIBNOTIFY
uimnotify_plugin_LTLIBRARIES += libuimnotify-libnotify.la
libuimnotify_libnotify_la_SOURCES = uim-libnotify.c
libuimnotify_libnotify_la_CFLAGS = @LIBNOTIFY_CFLAGS@
libuimnotify_libnotify_la_LDFLAGS  = -avoid-version -module
libuimnotify_libnotify_la_LIBADD = @LIBNOTIFY_LIBS@
endif

if KNOTIFY3
uimnotify_plugin_LTLIBRARIES += libuimnotify-knotify3.la
libuimnotify_knotify3_la_SOURCES = uim-knotify3.cc
libuimnotify_knotify3_la_CXXFLAGS = -I@KNOTIFY3_INCLUDE_DIR@ $(QT_CXXFLAGS)
libuimnotify_knotify3_la_LDFLAGS  = -avoid-version -module -L@KNOTIFY3_LIB_DIR@ $(QT_LDFLAGS)
libuimnotify_knotify3_la_LIBADD = -lkdeui -lkdecore
endif
