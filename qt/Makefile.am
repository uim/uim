EXTRA_DIST = pref-keyeditformbase.ui  pref-keygrabformbase.ui  pref-olisteditformbase.ui

if QT

INCLUDES    = -I$(top_srcdir) -I$(top_builddir)

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
	switcher-qt.h \
	pref-customwidgets.h \
	pref-kseparator.h \
	pref-qt.h \
	chardict-bushuviewwidget.h \
	chardict-unicodeviewwidget.h \
	chardict-chargridview.h \
	chardict-kseparator.h \
	chardict-qt.h
MOC_FILES = \
	toolbar-common-quimhelpertoolbar.moc \
	toolbar-common-uimstateindicator.moc \
	toolbar-standalone-qt.moc \
	candwin-qt.moc \
	switcher-qt.moc \
	pref-customwidgets.moc \
	pref-kseparator.moc \
	pref-qt.moc \
	chardict-bushuviewwidget.moc \
	chardict-unicodeviewwidget.moc \
	chardict-chargridview.moc \
	chardict-kseparator.moc \
	chardict-qt.moc
UI_TMP_FILES = \
	pref-olisteditformbase.cpp \
	pref-olisteditformbase.h \
	pref-olisteditformbase.moc \
	pref-keyeditformbase.cpp \
	pref-keyeditformbase.h \
	pref-keyeditformbase.moc \
	pref-keygrabformbase.cpp \
	pref-keygrabformbase.h \
	pref-keygrabformbase.moc

noinst_HEADERS = $(HEADER_FILES)

bin_PROGRAMS = \
	uim-toolbar-qt \
	uim-candwin-qt \
	uim-im-switcher-qt \
	uim-pref-qt \
	uim-chardict-qt

# Immodule
if QT_IMMODULE
extra_LTLIBRARIES = libquiminputcontextplugin.la
extradir = $(QT_PLUGINSDIR)/inputmethods

noinst_HEADERS += \
	immodule-candidatewindow.h immodule-qhelpermanager.h \
	immodule-quiminputcontext.h immodule-quiminputcontext_with_slave.h \
	immodule-subwindow.h
MOC_FILES += \
	immodule-candidatewindow.moc immodule-qhelpermanager.moc \
	immodule-quiminputcontext.moc immodule-quiminputcontext_with_slave.moc \
	immodule-subwindow.moc

libquiminputcontextplugin_la_SOURCES = \
	immodule-plugin.cpp immodule-candidatewindow.cpp immodule-qhelpermanager.cpp \
	immodule-quiminputcontext.cpp immodule-quiminputcontext_with_slave.cpp \
	immodule-subwindow.cpp
libquiminputcontextplugin_la_CXXFLAGS = $(QT_CXXFLAGS)
libquiminputcontextplugin_la_LDFLAGS  = $(QT_LDFLAGS) -avoid-version
libquiminputcontextplugin_la_LIBADD   = $(top_builddir)/uim/libuim.la

immodule-candidatewindow.cpp: immodule-candidatewindow.moc
immodule-candidatewindow.moc: immodule-candidatewindow.h
	$(MOC) immodule-candidatewindow.h -o immodule-candidatewindow.moc
immodule-qhelpermanager.cpp: immodule-qhelpermanager.moc
immodule-qhelpermanager.moc: immodule-qhelpermanager.h
	$(MOC) immodule-qhelpermanager.h -o immodule-qhelpermanager.moc
immodule-quiminputcontext.cpp: immodule-quiminputcontext.moc
immodule-quiminputcontext.moc: immodule-quiminputcontext.h
	$(MOC) immodule-quiminputcontext.h -o immodule-quiminputcontext.moc
immodule-quiminputcontext_with_slave.cpp: immodule-quiminputcontext_with_slave.moc
immodule-quiminputcontext_with_slave.moc: immodule-quiminputcontext_with_slave.h
	$(MOC) immodule-quiminputcontext_with_slave.h -o immodule-quiminputcontext_with_slave.moc
immodule-subwindow.cpp: immodule-subwindow.moc
immodule-subwindow.moc: immodule-subwindow.h
	$(MOC) immodule-subwindow.h -o immodule-subwindow.moc
endif

# Toolbar Common
noinst_LTLIBRARIES = libtoolbarcommon.la
libtoolbarcommon_la_SOURCES  = toolbar-common-quimhelpertoolbar.cpp toolbar-common-uimstateindicator.cpp
libtoolbarcommon_la_CXXFLAGS = $(QT_CXXFLAGS)
libtoolbarcommon_la_LDFLAGS  = $(QT_LDFLAGS) -avoid-version
libtoolbarcommon_la_LIBADD   = $(top_builddir)/uim/libuim.la
toolbar-common-quimhelpertoolbar.cpp: toolbar-common-quimhelpertoolbar.moc
toolbar-common-uimstateindicator.cpp: toolbar-common-uimstateindicator.moc

# Toolbar Standalone
uim_toolbar_qt_SOURCES = toolbar-standalone-qt.cpp
uim_toolbar_qt_CXXFLAGS = $(QT_CXXFLAGS)
uim_toolbar_qt_LDFLAGS  = $(QT_LDFLAGS)
uim_toolbar_qt_LDADD    = $(top_builddir)/uim/libuim.la ./libtoolbarcommon.la
toolbar-standalone-qt.cpp: toolbar-standalone-qt.moc

# Candidate Window
uim_candwin_qt_SOURCES  = candwin-qt.cpp qtgettext.h
uim_candwin_qt_CXXFLAGS = $(QT_CXXFLAGS)
uim_candwin_qt_LDFLAGS  = $(QT_LDFLAGS)
uim_candwin_qt_LDADD = $(top_builddir)/uim/libuim.la
candwin-qt.cpp: candwin-qt.moc

# Switcher
uim_im_switcher_qt_SOURCES  = switcher-qt.cpp
uim_im_switcher_qt_CXXFLAGS = $(QT_CXXFLAGS)
uim_im_switcher_qt_LDFLAGS  = $(QT_LDFLAGS)
uim_im_switcher_qt_LDADD    = $(top_builddir)/uim/libuim.la
switcher-qt.cpp: switcher-qt.moc

# Pref
uim_pref_qt_SOURCES = \
	pref-olisteditformbase.cpp pref-keyeditformbase.cpp pref-keygrabformbase.cpp \
	pref-qt.cpp pref-customwidgets.cpp pref-kseparator.cpp 

uim_pref_qt_CXXFLAGS = $(QT_CXXFLAGS)
uim_pref_qt_LDFLAGS  = $(QT_LDFLAGS)
uim_pref_qt_LDADD    = $(top_builddir)/uim/libuim.la $(top_builddir)/uim/libuim-custom.la

pref-customwidgets.h:pref-olisteditformbase.h pref-keyeditformbase.h pref-keygrabformbase.h
pref-qt.h:pref-olisteditformbase.h pref-keyeditformbase.h pref-keygrabformbase.h

pref-qt.cpp: pref-qt.moc
pref-customwidgets.cpp: pref-customwidgets.moc
pref-kseparator.cpp: pref-kseparator.moc

pref-olisteditformbase.cpp:pref-olisteditformbase.h
	$(UIC) $(top_srcdir)/qt/pref-olisteditformbase.ui -i pref-olisteditformbase.h > pref-olisteditformbase.cpp.tmp
	$(MOC) pref-olisteditformbase.h -o pref-olisteditformbase.moc
	echo '#include "pref-olisteditformbase.moc"' >> pref-olisteditformbase.cpp.tmp
	mv pref-olisteditformbase.cpp.tmp pref-olisteditformbase.cpp
pref-olisteditformbase.h:
	$(UIC) $(top_srcdir)/qt/pref-olisteditformbase.ui -o pref-olisteditformbase.h

pref-keyeditformbase.cpp: pref-keyeditformbase.h
	$(UIC) $(top_srcdir)/qt/pref-keyeditformbase.ui -i pref-keyeditformbase.h > pref-keyeditformbase.cpp.tmp
	$(MOC) pref-keyeditformbase.h -o pref-keyeditformbase.moc
	echo '#include "pref-keyeditformbase.moc"' >> pref-keyeditformbase.cpp.tmp
	mv pref-keyeditformbase.cpp.tmp pref-keyeditformbase.cpp
pref-keyeditformbase.h:
	$(UIC) $(top_srcdir)/qt/pref-keyeditformbase.ui -o pref-keyeditformbase.h

pref-keygrabformbase.cpp: pref-keygrabformbase.h
	$(UIC) $(top_srcdir)/qt/pref-keygrabformbase.ui -i pref-keygrabformbase.h > pref-keygrabformbase.cpp.tmp
	$(MOC) pref-keygrabformbase.h -o pref-keygrabformbase.moc
	echo '#include "pref-keygrabformbase.moc"' >> pref-keygrabformbase.cpp.tmp
	mv pref-keygrabformbase.cpp.tmp pref-keygrabformbase.cpp
pref-keygrabformbase.h:
	$(UIC) $(top_srcdir)/qt/pref-keygrabformbase.ui -o pref-keygrabformbase.h

uim_chardict_qt_SOURCES = \
	chardict-qt.cpp \
	chardict-bushuviewwidget.cpp \
	chardict-chargridview.cpp \
	chardict-kseparator.cpp \
	chardict-unicodeviewwidget.cpp
uim_chardict_qt_CXXFLAGS = $(QT_CXXFLAGS) -DBUSHUDICT=\"$(datadir)/uim/helperdata/bushu.t\"
uim_chardict_qt_LDFLAGS  = $(QT_LDFLAGS)
uim_chardict_qt_LDADD    = $(top_builddir)/uim/libuim.la
chardict-qt.cpp: chardict-qt.moc

chardict-bushuviewwidget.cpp: chardict-bushuviewwidget.moc
chardict-bushuviewwidget.moc: chardict-bushuviewwidget.h
	$(MOC) $(top_srcdir)/qt/chardict-bushuviewwidget.h -o chardict-bushuviewwidget.moc
chardict-unicodeviewwidget.cpp: chardict-unicodeviewwidget.moc
chardict-unicodeviewwidget.moc: chardict-unicodeviewwidget.h
	$(MOC) $(top_srcdir)/qt/chardict-unicodeviewwidget.h -o chardict-unicodeviewwidget.moc
chardict-chargridview.cpp: chardict-chargridview.moc
chardict-chargridview.moc: chardict-chargridview.h
	$(MOC) $(top_srcdir)/qt/chardict-chargridview.h -o chardict-chargridview.moc
chardict-kseparator.cpp: chardict-kseparator.moc
chardict-kseparator.moc: chardict-kseparator.h
	$(MOC) $(top_srcdir)/qt/chardict-kseparator.h -o chardict-kseparator.moc
EXTRA_DIST += $(helperdata_DATA)
helperdatadir = $(datadir)/uim/helperdata
helperdata_DATA = bushu.t

CLEANFILES = \
	$(MOC_FILES) \
	$(UI_TMP_FILES)

.h.moc: 
	$(MOC) $< -o $*.moc
.ui.h: 
	$(UIC) $< -o $*.h
endif

