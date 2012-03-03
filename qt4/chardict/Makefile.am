.PHONY: mocclean FORCE

# Makefile.qmake is only exist when --enable-qt4
if QT4

all clean mocclean install uninstall:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake INSTALL_ROOT=$(DESTDIR) $@

helperdatadir = $(datadir)/uim/helperdata
helperdata_DATA = bushu.t
endif

# *.pro is required to run Makefile.qmake. So distclean-am is deferred.
distclean:
if QT4
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake $@
	$(MAKE) $(AM_MAKEFLAGS) distclean-am
endif
	$(MAKE) $@ -C po
	-rm -f Makefile uim-chardict-qt4.pro

FORCE:

EXTRA_DIST = uim-chardict-qt4.pro.in \
	     bushuviewwidget.h \
	     chargridview.h \
	     qt4.h \
	     unicodeviewwidget.h \
	     bushuviewwidget.cpp \
	     chargridview.cpp \
	     qt4.cpp \
	     unicodeviewwidget.cpp \
	     bushu.t
