SUBDIRS =					\
	qtquick-plugin

EXTRA_DIST =					\
	CMakeLists.txt				\
	plasmoid

if QT5
all clean install uninstall:
	$(MAKE) $(AM_MAKEFLAGS) -C ../applet.cmake $@
endif
