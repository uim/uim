CXXFLAGS        += $(UIM_QT_CFLAGS)
CFLAGS          += $(UIM_QT_CFLAGS)

HEADER_FILES = \
	toolbar-common-quimhelpertoolbar.h \
	toolbar-common-uimstateindicator.h \
	toolbar-standalone-qt.h \
	candwin-qt.h \
	switcher-qt.h \
	pref-customwidgets.h \
	pref-kseparator.h \
	pref-qt.h
MOC_FILES = \
	toolbar-common-quimhelpertoolbar.moc \
	toolbar-common-uimstateindicator.moc \
	toolbar-standalone-qt.moc \
	candwin-qt.moc \
	switcher-qt.moc \
	pref-customwidgets.moc \
	pref-kseparator.moc \
	pref-qt.moc
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
CLEANFILES = \
	$(MOC_FILES) \
	$(UI_TMP_FILES)

bin_PROGRAMS = \
	uim-toolbar-qt \
	uim-candwin-qt \
	uim-im-switcher-qt \
	uim-pref-qt


# Toolbar Common
noinst_LTLIBRARIES = libtoolbarcommon.la
libtoolbarcommon_la_SOURCES = toolbar-common-quimhelpertoolbar.cpp toolbar-common-uimstateindicator.cpp
libtoolbarcommon_la_LIBADD = -luim
toolbar-common-quimhelpertoolbar.cpp: toolbar-common-quimhelpertoolbar.moc
toolbar-common-quimhelpertoolbar.moc: toolbar-common-quimhelpertoolbar.h
	$(MOC) toolbar-common-quimhelpertoolbar.h -o toolbar-common-quimhelpertoolbar.moc
toolbar-common-uimstateindicator.cpp: toolbar-common-uimstateindicator.moc
toolbar-common-uimstateindicator.moc: toolbar-common-uimstateindicator.h
	$(MOC) toolbar-common-uimstateindicator.h -o toolbar-common-uimstateindicator.moc

# Toolbar Standalone
uim_toolbar_qt_SOURCES = toolbar-standalone-qt.cpp
uim_toolbar_qt_LDADD   = -lqt-mt -luim ./libtoolbarcommon.la
toolbar-standalone-qt.cpp: toolbar-standalone-qt.moc
toolbar-standalone-qt.moc: toolbar-standalone-qt.h
	$(MOC) toolbar-standalone-qt.h -o toolbar-standalone-qt.moc

# Candidate Window
uim_candwin_qt_SOURCES = candwin-qt.cpp
uim_candwin_qt_LDADD = -lqt-mt -luim
candwin-qt.cpp: candwin-qt.moc
candwin-qt.moc: candwin-qt.h
	$(MOC) candwin-qt.h -o candwin-qt.moc

# Switcher
uim_im_switcher_qt_SOURCES = switcher-qt.cpp
uim_im_switcher_qt_LDADD = -lqt-mt -luim
switcher-qt.cpp: switcher-qt.moc
switcher-qt.moc: switcher-qt.h
	$(MOC) switcher-qt.h -o switcher-qt.moc

# Pref
uim_pref_qt_SOURCES = \
	pref-olisteditformbase.cpp pref-keyeditformbase.cpp pref-keygrabformbase.cpp \
	pref-qt.cpp pref-customwidgets.cpp pref-kseparator.cpp

uim_pref_qt_LDADD = -lqt-mt -luim -luim-custom

pref-customwidgets.h:pref-olisteditformbase.h pref-keyeditformbase.h pref-keygrabformbase.h
pref-qt.h:pref-olisteditformbase.h pref-keyeditformbase.h pref-keygrabformbase.h

pref-qt.cpp: pref-qt.moc
pref-qt.moc: pref-qt.h
	$(MOC) pref-qt.h -o pref-qt.moc
pref-customwidgets.cpp: pref-customwidgets.moc
pref-customwidgets.moc: pref-customwidgets.h
	$(MOC) pref-customwidgets.h -o pref-customwidgets.moc
pref-kseparator.cpp: pref-kseparator.moc
pref-kseparator.moc: pref-kseparator.h
	$(MOC) pref-kseparator.h -o pref-kseparator.moc

pref-olisteditformbase.cpp: pref-olisteditformbase.h
	$(UIC) pref-olisteditformbase.ui -o pref-olisteditformbase.h
	$(UIC) pref-olisteditformbase.ui -i pref-olisteditformbase.h > pref-olisteditformbase.cpp.tmp
	$(MOC) pref-olisteditformbase.h -o pref-olisteditformbase.moc
	echo '#include "pref-olisteditformbase.moc"' >> pref-olisteditformbase.cpp.tmp
	mv pref-olisteditformbase.cpp.tmp pref-olisteditformbase.cpp

pref-keyeditformbase.cpp: pref-keyeditformbase.h
	$(UIC) pref-keyeditformbase.ui -o pref-keyeditformbase.h
	$(UIC) pref-keyeditformbase.ui -i pref-keyeditformbase.h > pref-keyeditformbase.cpp.tmp
	$(MOC) pref-keyeditformbase.h -o pref-keyeditformbase.moc
	echo '#include "pref-keyeditformbase.moc"' >> pref-keyeditformbase.cpp.tmp
	mv pref-keyeditformbase.cpp.tmp pref-keyeditformbase.cpp

pref-keygrabformbase.cpp: pref-keygrabformbase.h
	$(UIC) pref-keygrabformbase.ui -o pref-keygrabformbase.h
	$(UIC) pref-keygrabformbase.ui -i pref-keygrabformbase.h > pref-keygrabformbase.cpp.tmp
	$(MOC) pref-keygrabformbase.h -o pref-keygrabformbase.moc
	echo '#include "pref-keygrabformbase.moc"' >> pref-keygrabformbase.cpp.tmp
	mv pref-keygrabformbase.cpp.tmp pref-keygrabformbase.cpp



