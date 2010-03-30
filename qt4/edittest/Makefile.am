.PHONY: mocclean FORCE

# Makefile.qmake is only exist when --enable-qt4-immodule
if QT4_IMMODULE
all clean mocclean install uninstall:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake $@
endif

# *.pro is required to run Makefile.qmake. So distclean-am is deferred.
distclean:
if QT4_IMMODULE
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake $@
	$(MAKE) $(AM_MAKEFLAGS) distclean-am
endif
	-rm -f Makefile

FORCE:

EXTRA_DIST = README edittest.pro main.cpp
