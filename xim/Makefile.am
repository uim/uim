EXTRA_DIST = uim-xim.1

if XIM

AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

bin_PROGRAMS = uim-xim
uim_xim_LDFLAGS =  @X_LIBS@
uim_xim_LDADD =  -lXext -lX11 $(top_builddir)/uim/libuim-scm.la \
		 $(top_builddir)/uim/libuim.la \
		 $(top_builddir)/uim/libuim-x-util.la
uim_xim_CPPFLAGS = $(AM_CPPFLAGS)
uim_xim_CFLAGS = @X_CFLAGS@ -Wall
uim_xim_CXXFLAGS = @X_CFLAGS@ -Wall
man1_MANS = uim-xim.1

if DEFAULT_TOOLKIT_GTK
uim_xim_CPPFLAGS += -DUSE_GTK_CANDWIN
endif
if DEFAULT_TOOLKIT_GTK3
uim_xim_CPPFLAGS += -DUSE_GTK3_CANDWIN
endif
if DEFAULT_TOOLKIT_QT
uim_xim_CPPFLAGS += -DUSE_QT_CANDWIN
endif
if DEFAULT_TOOLKIT_TQT
uim_xim_CPPFLAGS += -DUSE_TQT_CANDWIN
endif
if DEFAULT_TOOLKIT_QT4
uim_xim_CPPFLAGS += -DUSE_QT4_CANDWIN
endif

if WITH_XFT
uim_xim_CFLAGS += @XFT_CFLAGS@
uim_xim_CXXFLAGS += @XFT_CFLAGS@
uim_xim_LDADD += @XFT_LIBS@
endif

uim_xim_SOURCES = \
	main.cpp convdisp.cpp \
        connection.cpp ximic.cpp \
	ximtrans.cpp ximim.cpp \
        ximserver.cpp ximpacket.cpp \
        locale.cpp connection.h \
	convdisp.h ximserver.h \
	xim.h xdispatch.h ximpn.h \
	canddisp.cpp canddisp.h \
	util.cpp util.h \
	helper.cpp helper.h \
	compose.cpp compose.h
endif
