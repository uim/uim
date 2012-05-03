.PHONY: mocclean FORCE

# Makefile.qmake is only exist when --enable-qt4
if QT4
all clean mocclean install uninstall:
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake INSTALL_ROOT=$(DESTDIR) $@
endif

# *.pro is required to run Makefile.qmake. So distclean-am is deferred.
distclean:
if QT4
	$(MAKE) $(AM_MAKEFLAGS) -f Makefile.qmake $@
	$(MAKE) $(AM_MAKEFLAGS) distclean-am
endif
	-rm -f Makefile uim-candwin-qt4.pro

FORCE:

EXTRA_DIST =  uim-candwin-qt4.pro.in \
	      abstractcandidatewindow.h \
	      abstractcandidatewindow.cpp \
	      candidatetablewindow.h \
	      candidatetablewindow.cpp \
	      candidatewindow.h \
	      candidatewindow.cpp \
	      subwindow.h \
	      subwindow.cpp \
	      qt4.cpp
