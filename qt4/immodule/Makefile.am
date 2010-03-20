.PHONY: mocclean FORCE

# Makefile.qmake is only exist when --enable-qt4-immodule
if QT4_IMMODULE
all clean mocclean install uninstall:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake INSTALL_ROOT=$(DESTDIR) $@

# *.pro is required to run Makefile.qmake. So distclean-am is deferred.
distclean:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake $@
	$(MAKE) $(AM_MAKEFLAGS) distclean-am
	-rm -f Makefile quiminputcontextplugin.pro
else
distclean:
	-rm -f Makefile quiminputcontextplugin.pro
endif

FORCE:

EXTRA_DIST = README.en README.ja INDENT \
	     quiminputcontextplugin.pro.in \
	     quiminputcontext_with_slave.h \
	     quiminputcontext_with_slave.cpp

EXTRA_DIST += candidatewindow.h \
              caretstateindicator.h \
              debug.h \
              plugin.h \
              qhelpermanager.h \
              qtextutil.h \
              quiminfomanager.h \
              quiminputcontext.h \
              quiminputcontext_compose.h \
              subwindow.h

EXTRA_DIST += candidatewindow.cpp \
              caretstateindicator.cpp \
              plugin.cpp \
              qhelpermanager.cpp \
              qtextutil.cpp \
              quiminfomanager.cpp \
              quiminputcontext.cpp \
              quiminputcontext_compose.cpp \
              subwindow.cpp
