if APPLET_GNOME3

helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

libexec_PROGRAMS =  uim-toolbar-applet-gnome3

uim_toolbar_applet_gnome3_LDADD = @GTK2_LIBS@ @APPLET_GNOME3_LIBS@ \
			   $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la \
			   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_applet_gnome3_CPPFLAGS = \
			   $(helper_defs) -I$(top_srcdir) -I$(top_builddir)
									 
uim_toolbar_applet_gnome3_CFLAGS = @GTK2_CFLAGS@ @APPLET_GNOME3_CFLAGS@

uim_toolbar_applet_gnome3_SOURCES = toolbar-applet-gnome3.c \
			       ../../helper/toolbar-common-gtk.c

endif

if GTK3
bin_PROGRAMS = uim-toolbar-gtk3
uim_toolbar_gtk3_SOURCES  = ../../helper/toolbar-standalone-gtk.c \
			       ../../helper/toolbar-common-gtk.c
uim_toolbar_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la \
			   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk3_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_gtk3_CFLAGS   = @GTK3_CFLAGS@
endif
