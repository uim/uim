.PHONY: mocclean FORCE

# Makefile.qmake is only exist when --enable-qt5-immodule
if QT5_IMMODULE
all clean mocclean install uninstall:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake INSTALL_ROOT=$(DESTDIR) $@
endif

# *.pro is required to run Makefile.qmake. So distclean-am is deferred.
distclean:
if QT5_IMMODULE
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake $@
	$(MAKE) $(AM_MAKEFLAGS) distclean-am
endif
	-rm -f Makefile quimplatforminputcontextplugin.pro

FORCE:

EXTRA_DIST =					\
	quimplatforminputcontext.cpp		\
	quimplatforminputcontext.h		\
	quimplatforminputcontextplugin.pro.in	\
	uim.json
