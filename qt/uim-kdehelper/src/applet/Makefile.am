INCLUDES = $(all_includes)

noinst_HEADERS = uimhelperapplet.h
METASOURCES = AUTO

# applet
kde_module_LTLIBRARIES = libuimhelperapplet.la
libuimhelperapplet_la_SOURCES = uimhelperapplet.cpp
libuimhelperapplet_la_LIBADD  = $(LIB_KSYCOCA) $(LIB_KDEUI) ../common/libcommon.la 
libuimhelperapplet_la_LDFLAGS = -module $(KDE_PLUGIN) $(all_libraries)

lnkdir = $(kde_datadir)/kicker/applets
lnk_DATA = uimhelperapplet.desktop

EXTRA_DIST = $(lnk_DATA)
