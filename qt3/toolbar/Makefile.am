EXTRA_DIST = uimapplet.desktop.in

if QT
INCLUDES += -I$(top_srcdir) -I$(top_builddir) -I..
QT_CXXFLAGS = $(UIM_QT_CXXFLAGS)
QT_LDFLAGS  = $(UIM_QT_LDFLAGS)

HEADER_FILES = \
	common-quimhelpertoolbar.h \
	common-uimstateindicator.h \
	standalone-qt.h
if APPLET_KDE
HEADER_FILES += \
	applet-kde.h
endif

MOC_FILES = \
	common-quimhelpertoolbar.moc \
	common-uimstateindicator.moc \
	standalone-qt.moc
if APPLET_KDE
MOC_FILES += \
	applet-kde.moc
endif

noinst_HEADERS = $(HEADER_FILES)

bin_PROGRAMS = uim-toolbar-qt

if APPLET_KDE
applet_LTLIBRARIES = uim_panelapplet.la
appletdir = @KDE_LIB_DIR@/kde3

lnkdir = @KDE_DATA_DIR@/kicker/applets
lnk_DATA = uimapplet.desktop
endif

# Toolbar Common
noinst_LTLIBRARIES = libtoolbarcommon.la
libtoolbarcommon_la_SOURCES  = common-quimhelpertoolbar.cpp common-uimstateindicator.cpp ../qtgettext.h \
			       common-quimhelpertoolbar.h   common-uimstateindicator.h
libtoolbarcommon_la_CXXFLAGS = $(QT_CXXFLAGS)
libtoolbarcommon_la_LDFLAGS  = $(QT_LDFLAGS) -avoid-version
libtoolbarcommon_la_LIBADD   = $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim.la
common-quimhelpertoolbar.cpp: common-quimhelpertoolbar.moc
common-uimstateindicator.cpp: common-uimstateindicator.moc

# Toolbar Standalone
uim_toolbar_qt_SOURCES = standalone-qt.cpp standalone-qt.h ../qtgettext.h \
			 common-quimhelpertoolbar.h common-uimstateindicator.h
uim_toolbar_qt_CXXFLAGS = $(QT_CXXFLAGS)
uim_toolbar_qt_LDFLAGS  = $(QT_LDFLAGS)
uim_toolbar_qt_LDADD    = $(top_builddir)/uim/libuim-scm.la \
			  $(top_builddir)/uim/libuim.la ./libtoolbarcommon.la
standalone-qt.cpp: standalone-qt.moc

# Toolbar applet
if APPLET_KDE
uim_panelapplet_la_SOURCES = applet-kde.cpp applet-kde.h ../qtgettext.h \
			 common-quimhelpertoolbar.h common-uimstateindicator.h
uim_panelapplet_la_CXXFLAGS = -I@KDE_INCLUDE_DIR@ $(QT_CXXFLAGS)
uim_panelapplet_la_LDFLAGS  = -avoid-version -module -no-undefined -Wl,--no-undefined -Wl,--allow-shlib-undefined -L@KDE_LIB_DIR@ $(QT_LDFLAGS)
uim_panelapplet_la_LIBADD   = $(top_builddir)/uim/libuim-scm.la \
			      $(top_builddir)/uim/libuim.la \
			      libtoolbarcommon.la  -lkdeui -lkdecore 
applet-kde.cpp: applet-kde.moc
endif

CLEANFILES = \
	$(MOC_FILES) \
	$(UI_TMP_FILES)

.h.moc: 
	$(MOC) $< -o $*.moc
.ui.h: 
	$(UIC) $< -o $*.h
endif

