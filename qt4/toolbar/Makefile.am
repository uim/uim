.PHONY: mocclean FORCE

# Makefile.qmake is only exist when --enable-qt4
if QT4
all clean mocclean install uninstall:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake INSTALL_ROOT=$(DESTDIR) $@
if APPLET_KDE4
	( cd build; $(MAKE) $(AM_MAKEFLAGS) $@ )
endif

# *.pro is required to run Makefile.qmake. So distclean-am is deferred.
distclean:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake $@
	$(MAKE) $(AM_MAKEFLAGS) distclean-am
	-rm -f Makefile uim-toolbar-qt4.pro
	-rm -rf build/*
else
distclean:
	-rm -f Makefile uim-toolbar-qt4.pro
	-rm -rf build/*
endif

FORCE:

EXTRA_DIST =  uim-toolbar-qt4.pro.in \
	     common-quimhelpertoolbar.cpp \
	     common-quimhelpertoolbar.h \
	     common-uimstateindicator.cpp \
	     common-uimstateindicator.h \
	     standalone-qt4.cpp \
	     standalone-qt4.h \
	     standalone-qt4.cpp \
	     CMakeLists.txt \
	     plasma-applet-uim.desktop.in \
	     applet-kde4.cpp \
	     applet-kde4.h
