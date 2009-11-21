.PHONY: mocclean FORCE

# Makefile.qmake is only exist when --enable-qt4
if QT4
SUBDIRS = po

all clean mocclean install uninstall:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake INSTALL_ROOT=$(DESTDIR) $@

helperdatadir = $(datadir)/uim/helperdata
helperdata_DATA = bushu.t

# *.pro is required to run Makefile.qmake. So distclean-am is deferred.
distclean:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake $@
	$(MAKE) $(AM_MAKEFLAGS) distclean-am
	-rm -f Makefile uim-chardict-qt4.pro
else
distclean:
	-rm -f Makefile uim-chardict-qt4.pro
endif

FORCE:

EXTRA_DIST =  uim-chardict-qt4.pro.in \
	     chargridview.h \
	     kseparator.h \
	     qt4.h \
	     qtgettext.h \
	     unicodeviewwidget.h
	     bushuviewwidget.cpp \
	     chargridview.cpp \
	     kseparator.cpp \
	     qt4.cpp \
	     unicodeviewwidget.cpp \
		 bushu.t
