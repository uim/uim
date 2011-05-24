if QT
INCLUDES += -I$(top_srcdir) -I$(top_builddir)
QT_CXXFLAGS = $(UIM_QT_CXXFLAGS)
QT_LDFLAGS  = $(UIM_QT_LDFLAGS)

# Immodule
if QT_IMMODULE
QT_CXXFLAGS += -DQT_IMMODULE

extra_LTLIBRARIES = libquiminputcontextplugin.la
extradir = $(QT_PLUGINSDIR)/inputmethods

noinst_HEADERS = \
	candidatewindow.h qhelpermanager.h \
	quiminputcontext.h quiminputcontext_with_slave.h \
	quiminputcontext_compose.h \
	qtextutil.h quiminfomanager.h \
	subwindow.h
MOC_FILES = \
	candidatewindow.moc qhelpermanager.moc \
	quiminputcontext.moc quiminputcontext_with_slave.moc \
	qtextutil.moc \
	subwindow.moc

libquiminputcontextplugin_la_SOURCES = \
	candidatewindow.cpp  qhelpermanager.cpp \
	plugin.cpp plugin.h \
	candidatewindow.h    qhelpermanager.h \
	quiminputcontext.cpp quiminputcontext_with_slave.cpp \
	quiminputcontext.h   quiminputcontext_with_slave.h \
	quiminputcontext_compose.cpp \
	quiminputcontext_compose.h \
	qtextutil.cpp qtextutil.h \
	quiminfomanager.cpp quiminfomanager.h \
	subwindow.cpp subwindow.h
libquiminputcontextplugin_la_CXXFLAGS = $(QT_CXXFLAGS) @X_CFLAGS@
libquiminputcontextplugin_la_LDFLAGS  = $(QT_LDFLAGS) -avoid-version
libquiminputcontextplugin_la_LIBADD   = $(top_builddir)/uim/libuim-counted-init.la \
					$(top_builddir)/uim/libuim-scm.la \
					$(top_builddir)/uim/libuim.la
if LIBUIM_X_UTIL
libquiminputcontextplugin_la_LIBADD  += $(top_builddir)/uim/libuim-x-util.la
endif

candidatewindow.cpp: candidatewindow.moc
qhelpermanager.cpp: qhelpermanager.moc
quiminputcontext.cpp: quiminputcontext.moc
quiminputcontext_with_slave.cpp: quiminputcontext_with_slave.moc
qtextutil.cpp: qtextutil.moc
subwindow.cpp: subwindow.moc
endif

CLEANFILES = \
	$(MOC_FILES) \
	$(UI_TMP_FILES)

.h.moc: 
	$(MOC) $< -o $*.moc
.ui.h: 
	$(UIC) $< -o $*.h
endif
