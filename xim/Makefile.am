if XIM

bin_PROGRAMS = uim-xim
uim_xim_LDFLAGS =  @X_LIBS@
uim_xim_LDADD =  -lXext -lX11 $(top_builddir)/uim/libuim.la @LIBICONV@
uim_xim_CPPFLAGS = -I$(top_srcdir)
uim_xim_CFLAGS = @X_CFLAGS@ -Wall
uim_xim_CXXFLAGS = @X_CFLAGS@ -Wall

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
