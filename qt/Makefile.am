SUBDIRS = chardict test
EXTRA_DIST = pref-keyeditformbase.ui  pref-olisteditformbase.ui \
	     uimapplet.desktop.in

if QT
INCLUDES += -I$(top_srcdir) -I$(top_builddir)
QT_CXXFLAGS = $(UIM_QT_CXXFLAGS)
QT_LDFLAGS  = $(UIM_QT_LDFLAGS)

if QT_IMMODULE
QT_CXXFLAGS += -DQT_IMMODULE
endif

HEADER_FILES = \
	toolbar-common-quimhelpertoolbar.h \
	toolbar-common-uimstateindicator.h \
	toolbar-standalone-qt.h \
	candwin-qt.h \
	switcher-qt.h
if PREF
HEADER_FILES += \
	pref-customwidgets.h \
	pref-kseparator.h \
	pref-qt.h
endif
if APPLET_KDE
HEADER_FILES += \
	toolbar-applet-kde.h
endif

MOC_FILES = \
	toolbar-common-quimhelpertoolbar.moc \
	toolbar-common-uimstateindicator.moc \
	toolbar-standalone-qt.moc \
	candwin-qt.moc \
	switcher-qt.moc
if PREF
MOC_FILES += \
	pref-customwidgets.moc \
	pref-kseparator.moc \
	pref-qt.moc
endif
if APPLET_KDE
MOC_FILES += \
	toolbar-applet-kde.moc
endif

if PREF
UI_TMP_FILES = \
	pref-olisteditformbase.cpp \
	pref-olisteditformbase.h \
	pref-olisteditformbase.moc \
	pref-keyeditformbase.cpp \
	pref-keyeditformbase.h \
	pref-keyeditformbase.moc
endif

noinst_HEADERS = $(HEADER_FILES)

bin_PROGRAMS = \
	uim-toolbar-qt \
	uim-im-switcher-qt
if PREF
bin_PROGRAMS += \
	uim-pref-qt
endif

libexec_PROGRAMS = uim-candwin-qt

if APPLET_KDE
applet_LTLIBRARIES = uim_panelapplet.la
appletdir = @KDE_LIB_DIR@/kde3

lnkdir = @KDE_DATA_DIR@/kicker/applets
lnk_DATA = uimapplet.desktop
endif

# Immodule
if QT_IMMODULE
extra_LTLIBRARIES = libquiminputcontextplugin.la
extradir = $(QT_PLUGINSDIR)/inputmethods

noinst_HEADERS += \
	immodule-candidatewindow.h immodule-qhelpermanager.h \
	immodule-quiminputcontext.h immodule-quiminputcontext_with_slave.h \
	immodule-quiminputcontext_compose.h \
	immodule-qtextutil.h immodule-quiminfomanager.h \
	immodule-subwindow.h
MOC_FILES += \
	immodule-candidatewindow.moc immodule-qhelpermanager.moc \
	immodule-quiminputcontext.moc immodule-quiminputcontext_with_slave.moc \
	immodule-qtextutil.moc \
	immodule-subwindow.moc

libquiminputcontextplugin_la_SOURCES = \
	immodule-candidatewindow.cpp  immodule-qhelpermanager.cpp \
	immodule-plugin.cpp immodule-plugin.h \
	immodule-candidatewindow.h    immodule-qhelpermanager.h \
	immodule-quiminputcontext.cpp immodule-quiminputcontext_with_slave.cpp \
	immodule-quiminputcontext.h   immodule-quiminputcontext_with_slave.h \
	immodule-quiminputcontext_compose.cpp \
	immodule-quiminputcontext_compose.h \
	immodule-qtextutil.cpp immodule-qtextutil.h \
	immodule-quiminfomanager.cpp immodule-quiminfomanager.h \
	immodule-subwindow.cpp immodule-subwindow.h
libquiminputcontextplugin_la_CXXFLAGS = $(QT_CXXFLAGS) @X_CFLAGS@
libquiminputcontextplugin_la_LDFLAGS  = $(QT_LDFLAGS) -avoid-version
libquiminputcontextplugin_la_LIBADD   = $(top_builddir)/uim/libuim-counted-init.la \
					$(top_builddir)/uim/libuim-scm.la \
					$(top_builddir)/uim/libuim.la
if LIBUIM_X_UTIL
libquiminputcontextplugin_la_LIBADD  += $(top_builddir)/uim/libuim-x-util.la
endif

immodule-candidatewindow.cpp: immodule-candidatewindow.moc
immodule-qhelpermanager.cpp: immodule-qhelpermanager.moc
immodule-quiminputcontext.cpp: immodule-quiminputcontext.moc
immodule-quiminputcontext_with_slave.cpp: immodule-quiminputcontext_with_slave.moc
immodule-qtextutil.cpp: immodule-qtextutil.moc
immodule-subwindow.cpp: immodule-subwindow.moc
endif

# Toolbar Common
noinst_LTLIBRARIES = libtoolbarcommon.la
libtoolbarcommon_la_SOURCES  = toolbar-common-quimhelpertoolbar.cpp toolbar-common-uimstateindicator.cpp qtgettext.h \
			       toolbar-common-quimhelpertoolbar.h   toolbar-common-uimstateindicator.h
libtoolbarcommon_la_CXXFLAGS = $(QT_CXXFLAGS)
libtoolbarcommon_la_LDFLAGS  = $(QT_LDFLAGS) -avoid-version
libtoolbarcommon_la_LIBADD   = $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim.la
toolbar-common-quimhelpertoolbar.cpp: toolbar-common-quimhelpertoolbar.moc
toolbar-common-uimstateindicator.cpp: toolbar-common-uimstateindicator.moc

# Toolbar Standalone
uim_toolbar_qt_SOURCES = toolbar-standalone-qt.cpp toolbar-standalone-qt.h qtgettext.h \
			 toolbar-common-quimhelpertoolbar.h toolbar-common-uimstateindicator.h
uim_toolbar_qt_CXXFLAGS = $(QT_CXXFLAGS)
uim_toolbar_qt_LDFLAGS  = $(QT_LDFLAGS)
uim_toolbar_qt_LDADD    = $(top_builddir)/uim/libuim-scm.la \
			  $(top_builddir)/uim/libuim.la ./libtoolbarcommon.la
toolbar-standalone-qt.cpp: toolbar-standalone-qt.moc

# Toolbar applet
if APPLET_KDE
uim_panelapplet_la_SOURCES = toolbar-applet-kde.cpp toolbar-applet-kde.h qtgettext.h \
			 toolbar-common-quimhelpertoolbar.h toolbar-common-uimstateindicator.h
uim_panelapplet_la_CXXFLAGS = -I@KDE_INCLUDE_DIR@ $(QT_CXXFLAGS)
uim_panelapplet_la_LDFLAGS  = -avoid-version -module -no-undefined -Wl,--no-undefined -Wl,--allow-shlib-undefined -L@KDE_LIB_DIR@ $(QT_LDFLAGS)
uim_panelapplet_la_LIBADD   = $(top_builddir)/uim/libuim-scm.la \
			      $(top_builddir)/uim/libuim.la \
			      libtoolbarcommon.la  -lkdeui -lkdecore 
toolbar-applet-kde.cpp: toolbar-applet-kde.moc
endif

# Candidate Window
uim_candwin_qt_SOURCES  = candwin-qt.cpp candwin-qt.h qtgettext.h
uim_candwin_qt_CXXFLAGS = $(QT_CXXFLAGS)
uim_candwin_qt_LDFLAGS  = $(QT_LDFLAGS)
uim_candwin_qt_LDADD = $(top_builddir)/uim/libuim-scm.la \
		       $(top_builddir)/uim/libuim.la
candwin-qt.cpp: candwin-qt.moc

# Switcher
uim_im_switcher_qt_SOURCES  = switcher-qt.cpp switcher-qt.h qtgettext.h
uim_im_switcher_qt_CXXFLAGS = $(QT_CXXFLAGS)
uim_im_switcher_qt_LDFLAGS  = $(QT_LDFLAGS)
uim_im_switcher_qt_LDADD    = $(top_builddir)/uim/libuim-scm.la \
			      $(top_builddir)/uim/libuim.la \
			      $(top_builddir)/uim/libuim-custom.la
switcher-qt.cpp: switcher-qt.moc


# Pref
if PREF
nodist_uim_pref_qt_SOURCES = \
	pref-olisteditformbase.cpp \
	pref-olisteditformbase.h \
	pref-keyeditformbase.cpp \
	pref-keyeditformbase.h
uim_pref_qt_SOURCES = \
	pref-qt.cpp pref-customwidgets.cpp pref-kseparator.cpp \
	qtgettext.h pref-qt.h \
	pref-customwidgets.h pref-kseparator.h

uim_pref_qt_CXXFLAGS = $(QT_CXXFLAGS)
uim_pref_qt_LDFLAGS  = $(QT_LDFLAGS)
uim_pref_qt_LDADD    = $(top_builddir)/uim/libuim-counted-init.la \
		       $(top_builddir)/uim/libuim-custom.la \
		       $(top_builddir)/uim/libuim-scm.la \
		       $(top_builddir)/uim/libuim.la

pref-customwidgets.h:pref-olisteditformbase.h pref-keyeditformbase.h

pref-qt.cpp: pref-qt.moc pref-olisteditformbase.h pref-keyeditformbase.h
pref-customwidgets.cpp: pref-customwidgets.moc
pref-kseparator.cpp: pref-kseparator.moc

pref-olisteditformbase.cpp: pref-olisteditformbase.h pref-olisteditformbase.moc
	$(UIC) $(top_srcdir)/qt/pref-olisteditformbase.ui -i pref-olisteditformbase.h > pref-olisteditformbase.cpp.tmp
	echo '#include "pref-olisteditformbase.moc"' >> pref-olisteditformbase.cpp.tmp
	mv pref-olisteditformbase.cpp.tmp pref-olisteditformbase.cpp
pref-olisteditformbase.h:
	$(UIC) $(top_srcdir)/qt/pref-olisteditformbase.ui -o pref-olisteditformbase.h

pref-keyeditformbase.cpp: pref-keyeditformbase.h pref-keyeditformbase.moc
	$(UIC) $(top_srcdir)/qt/pref-keyeditformbase.ui -i pref-keyeditformbase.h > pref-keyeditformbase.cpp.tmp
	echo '#include "pref-keyeditformbase.moc"' >> pref-keyeditformbase.cpp.tmp
	mv pref-keyeditformbase.cpp.tmp pref-keyeditformbase.cpp
pref-keyeditformbase.h:
	$(UIC) $(top_srcdir)/qt/pref-keyeditformbase.ui -o pref-keyeditformbase.h
endif

CLEANFILES = \
	$(MOC_FILES) \
	$(UI_TMP_FILES)

.h.moc: 
	$(MOC) $< -o $*.moc
.ui.h: 
	$(UIC) $< -o $*.h
endif

