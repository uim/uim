if SUMIKA

AM_CPPFLAGS = -I$(top_srcdir)/sumika/include

AM_CFLAGS =\
	 -Wall -Wimplicit -Wunused -Wmissing-prototypes \
	 -g\
	 $(GLIB_CFLAGS) \
	 $(uim_cflags)


EXTRA_DIST = 
noinst_LTLIBRARIES = libsumika_common.la

libsumika_common_la_SOURCES = \
	anthy.c \
	skk.c \
	cannadic.c \
	word.c \
	$(uim_source)

libsumika_common_la_LDFLAGS = 

libsumika_common_la_LIBADD = \
	$(GLIB_LIBS) \
	$(uim_libs)

uim_source = uimconfig.c

uim_cflags = -I$(top_srcdir) -I$(top_builddir)

#uim_libs   = $(UIM_LIBS)

endif
