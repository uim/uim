AUTOMAKE_OPTIONS = foreign

uimnotify_plugin_LTLIBRARIES =
uimnotify_plugindir = $(pkglibdir)/notify

if LIBNOTIFY
# XXX
CFLAGS+= -Wno-error
uimnotify_plugin_LTLIBRARIES += libuimnotify-libnotify.la
libuimnotify_libnotify_la_SOURCES = uim-libnotify.c
libuimnotify_libnotify_la_CFLAGS = @LIBNOTIFY_CFLAGS@
libuimnotify_libnotify_la_LDFLAGS  = -avoid-version -module
libuimnotify_libnotify_la_LIBADD = @LIBNOTIFY_LIBS@ $(top_builddir)/replace/libreplace.la
endif

if KNOTIFY3
uimnotify_plugin_LTLIBRARIES += libuimnotify-knotify3.la
libuimnotify_knotify3_la_SOURCES = uim-knotify3.cc
libuimnotify_knotify3_la_CXXFLAGS = -I@KNOTIFY3_INCLUDE_DIR@ $(QT_CXXFLAGS)
libuimnotify_knotify3_la_LDFLAGS  = -avoid-version -module -L@KNOTIFY3_LIB_DIR@ $(QT_LDFLAGS)
libuimnotify_knotify3_la_LIBADD = -lkdeui -lkdecore $(top_builddir)/replace/libreplace.la
endif

if GROWL
uimnotify_plugin_LTLIBRARIES += libuimnotify-growl.la
libuimnotify_growl_la_SOURCES = uim-growl.m
# FIXME: configuration for Growl framework path
libuimnotify_growl_la_OBJCFLAGS = -x objective-c -F../../../Sources/Growl
libuimnotify_growl_la_LDFLAGS  = -avoid-version -module -framework Cocoa -F../../../Sources/Growl -framework Growl
libuimnotify_growl_la_LIBADD = $(top_builddir)/replace/libreplace.la
endif

if KNOTIFY4
all cleaninstall install uninstall:
	( cd build; $(MAKE) $(AM_MAKEFLAGS) $@ )

distclean-local:
	-rm -rf build/*
endif 

FORCE:

EXTRA_DIST = uim-knotify4.cpp CMakeLists.txt
