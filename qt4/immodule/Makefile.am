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

EXTRA_DIST += debug.h \
              qhelpermanager.h \
              quiminputcontext.h \
              quiminputcontext_compose.h \
              plugin.h \
              candidatewindow.h \
              subwindow.h \
              quiminfomanager.h \
              qtextutil.h

EXTRA_DIST += plugin.cpp \
              qhelpermanager.cpp \
              quiminputcontext.cpp \
              quiminputcontext_compose.cpp \
              candidatewindow.cpp \
              subwindow.cpp \
              quiminfomanager.cpp \
              qtextutil.cpp
