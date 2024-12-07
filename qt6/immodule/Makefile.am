.PHONY: mocclean FORCE

# Makefile.qmake is only exist when --enable-qt6-immodule
if QT6_IMMODULE
all clean mocclean install uninstall:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake INSTALL_ROOT=$(DESTDIR) $@
endif

# *.pro is required to run Makefile.qmake. So distclean-am is deferred.
distclean:
if QT6_IMMODULE
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake $@
	$(MAKE) $(AM_MAKEFLAGS) distclean-am
endif
	-rm -f Makefile quimplatforminputcontextplugin.pro

FORCE:

EXTRA_DIST =					\
	quimplatforminputcontextplugin.pro.in
