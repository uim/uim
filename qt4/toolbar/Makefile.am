.PHONY: mocclean FORCE

# Makefile.qmake is only exist when --enable-qt4
if QT4
all clean mocclean install uninstall:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake INSTALL_ROOT=$(DESTDIR) $@
if KDE4_APPLET
	( cd build; $(MAKE) $(AM_MAKEFLAGS) $@ )
endif
endif

# *.pro is required to run Makefile.qmake. So distclean-am is deferred.
distclean:
if QT4
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake $@
	$(MAKE) $(AM_MAKEFLAGS) distclean-am
endif
	-rm -f Makefile uim-toolbar-qt4.pro plasma-applet-uim.desktop
	-rm -rf build/*

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
