.PHONY: mocclean FORCE

# Makefile.qmake is only exist when --enable-qt4
if QT4
all clean mocclean install uninstall:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake INSTALL_ROOT=$(DESTDIR) $@

# *.pro is required to run Makefile.qmake. So distclean-am is deferred.
distclean:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake $@
	$(MAKE) $(AM_MAKEFLAGS) distclean-am
	-rm -f Makefile uim-candwin-qt4.pro
else
distclean:
	-rm -f Makefile uim-candwin-qt4.pro
endif

FORCE:

EXTRA_DIST =  uim-candwin-qt4.pro.in \
	     qt4.h \
	     qt4.cpp
