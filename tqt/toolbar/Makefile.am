EXTRA_DIST = uimapplet.desktop.in

if TQT
AM_CPPFLAGS =				\
	-I$(top_builddir)/uim		\
	-I$(top_srcdir)/replace		\
	-I$(top_srcdir)/tqt		\
	-I$(top_srcdir)
TQT_CXXFLAGS = $(UIM_TQT_CXXFLAGS)
TQT_LDFLAGS  = $(UIM_TQT_LDFLAGS)

HEADER_FILES = \
	common-quimhelpertoolbar.h \
	common-uimstateindicator.h \
	standalone-qt.h
if TDE_APPLET
HEADER_FILES += \
	applet-tde.h
endif

MOC_FILES = \
	common-quimhelpertoolbar.moc \
	common-uimstateindicator.moc \
	standalone-qt.moc
if TDE_APPLET
MOC_FILES += \
	applet-tde.moc
endif

noinst_HEADERS = $(HEADER_FILES)

bin_PROGRAMS = uim-toolbar-tqt

if TDE_APPLET
applet_LTLIBRARIES = uim_panelapplet.la
appletdir = @TDE_LIB_DIR@/trinity

lnkdir = @TDE_DATA_DIR@/kicker/applets
lnk_DATA = uimapplet.desktop
endif

# Toolbar Common
noinst_LTLIBRARIES = libtoolbarcommon.la
libtoolbarcommon_la_SOURCES  = common-quimhelpertoolbar.cpp common-uimstateindicator.cpp ../qtgettext.h \
			       common-quimhelpertoolbar.h   common-uimstateindicator.h
libtoolbarcommon_la_CXXFLAGS = $(TQT_CXXFLAGS)
libtoolbarcommon_la_LDFLAGS  = $(TQT_LDFLAGS) -avoid-version
libtoolbarcommon_la_LIBADD   = $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim.la
common-quimhelpertoolbar.cpp: common-quimhelpertoolbar.moc
common-uimstateindicator.cpp: common-uimstateindicator.moc

# Toolbar Standalone
uim_toolbar_tqt_SOURCES = standalone-qt.cpp standalone-qt.h ../qtgettext.h \
			 common-quimhelpertoolbar.h common-uimstateindicator.h
uim_toolbar_tqt_CXXFLAGS = $(TQT_CXXFLAGS)
uim_toolbar_tqt_LDFLAGS  = $(TQT_LDFLAGS)
uim_toolbar_tqt_LDADD    = $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la ./libtoolbarcommon.la
standalone-qt.cpp: standalone-qt.moc

# Toolbar applet
if TDE_APPLET
uim_panelapplet_la_SOURCES = applet-tde.cpp applet-tde.h ../qtgettext.h \
			 common-quimhelpertoolbar.h common-uimstateindicator.h
uim_panelapplet_la_CXXFLAGS = -I@TDE_INCLUDE_DIR@ $(TQT_CXXFLAGS)
uim_panelapplet_la_LDFLAGS  = -avoid-version -module -no-undefined -Wl,--no-undefined -Wl,--allow-shlib-undefined -L@TDE_LIB_DIR@ $(TQT_LDFLAGS)
uim_panelapplet_la_LIBADD   = $(top_builddir)/uim/libuim-scm.la \
			      $(top_builddir)/uim/libuim.la \
			      libtoolbarcommon.la  -ltdeui -ltdecore
applet-tde.cpp: applet-tde.moc
endif

CLEANFILES = \
	$(MOC_FILES) \
	$(UI_TMP_FILES)

.h.moc:
	$(TQMOC) $< -o $*.moc
.ui.h:
	$(TQUIC) $< -o $*.h
endif

