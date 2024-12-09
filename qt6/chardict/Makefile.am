.PHONY: mocclean FORCE

# Makefile.qmake is only exist when --enable-qt6
if QT6

all clean mocclean install uninstall:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake INSTALL_ROOT=$(DESTDIR) $@
endif

# *.pro is required to run Makefile.qmake. So distclean-am is deferred.
distclean:
if QT6
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake $@
	$(MAKE) $(AM_MAKEFLAGS) distclean-am
endif
	-rm -f Makefile uim-chardict-qt6.pro

EXTRA_DIST =  uim-chardict-qt6.pro.in
