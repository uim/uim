if SUMIKA

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/sumika/include \
	 -DG_DISABLE_DEPRECATED -DGDK_PIXBUF_DISABLE_DEPRECATED \
	 -DGDK_DISABLE_DEPRECATED -DGTK_DISABLE_DEPRECATED

AM_CFLAGS = \
	 -pedantic\
	 -Wall -Wimplicit -Wunused -Wmissing-prototypes \
	 -g \
	$(GTK2_CFLAGS) \
	$(uim_cflags)

noinst_PROGRAMS = sumika

sumika_SOURCES = \
	main.c\
	topwin.c topwin.h\
	addwordpane.c addwordpane.h\
	charset.c charset.h\
	anthycontainer.c anthycontainer.h\
	skkcontainer.c skkcontainer.h\
	anthyaddwordpane.c anthyaddwordpane.h\
	anthyimportwordpane.c anthyimportwordpane.h\
	anthyviewwordpane.c anthyviewwordpane.h\
	posdialog.c posdialog.h\
	$(uim_source) \
	wordlist_view.c wordlist_view.h

sumika_LDFLAGS = 

sumika_LDADD =  \
	$(GTK2_LIBS) \
	$(uim_libs) \
	../common/libsumika_common.la

uim_source = uimcontainer.c uimcontainer.h
uim_cflags = $(UIM_CFLAGS)
uim_libs   = $(top_builddir)/uim/libuim.la

endif
