EXTRA_DIST = UimApplet.panel-applet.in.in \
	     uim-applet-menu.xml

if GNOME3_APPLET
helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

uim_toolbar_applet_gnome3_libdir = $(pkglibdir)
uim_toolbar_applet_gnome3_lib_LTLIBRARIES = libuim-toolbar-applet-gnome3.la

xmluidir = $(pkgdatadir)/ui
xmlui_DATA = uim-applet-menu.xml

libuim_toolbar_applet_gnome3_la_LIBADD = @GTK3_LIBS@ @GNOME3_APPLET_LIBS@ \
			   $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la \
			   $(top_builddir)/uim/libuim-custom.la
libuim_toolbar_applet_gnome3_la_CPPFLAGS = \
			   -DUIM_UIDATADIR="\"${xmluidir}\"" \
			   $(helper_defs) -I$(top_srcdir) -I$(top_builddir)

libuim_toolbar_applet_gnome3_la_CFLAGS = @GTK3_CFLAGS@ @GNOME3_APPLET_CFLAGS@

libuim_toolbar_applet_gnome3_la_SOURCES = applet-gnome3.c \
			       ../../gtk2/toolbar/common-gtk.c

APPLET_LOCATION = $(pkglibdir)/libuim-toolbar-applet-gnome3.so

appletdir = $(LIBPANEL_APPLET_DIR)
applet_DATA = UimApplet.panel-applet
applet_in_files = $(applet_DATA:=.in)
applet_in_in_files = $(applet_in_files:=.in)

$(applet_in_files): $(applet_in_in_files) Makefile
	$(SED) s,@APPLET_LOCATION@,$(APPLET_LOCATION),g <$< >$@.tmp
	$(SED) s,@UIM_PIXMAPSDIR@,$(uim_pixmapsdir),g <$@.tmp >$@

po_files = $(wildcard $(top_srcdir)/po/*.po)
$(applet_DATA): $(applet_in_files) $(INTLTOOL_MERGE) $(po_files)
	LC_ALL=C $(INTLTOOL_MERGE) -d -u -c $(top_builddir)/po/.intltool-merge-cache $(top_srcdir)/po $< $@ 

DISTCLEANFILES = UimApplet.panel-applet.in UimApplet.panel-applet \
		 UimApplet.panel-applet.in.tmp
endif

if GTK3
bin_PROGRAMS = uim-toolbar-gtk3 uim-toolbar-gtk3-systray
uim_toolbar_gtk3_SOURCES  = ../../gtk2/toolbar/standalone-gtk.c \
			       ../../gtk2/toolbar/common-gtk.c
uim_toolbar_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la \
			   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk3_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_gtk3_CFLAGS   = @GTK3_CFLAGS@

uim_toolbar_gtk3_systray_SOURCES  = ../../gtk2/toolbar/systray-gtk.c \
				   ../../gtk2/toolbar/common-gtk.c \
				   ../../gtk2/toolbar/eggtrayicon.c \
				   ../../gtk2/toolbar/eggtrayicon.h
uim_toolbar_gtk3_systray_LDADD    = @GTK3_LIBS@ \
				   @X11_LIBS@ \
				   $(top_builddir)/uim/libuim-scm.la \
				   $(top_builddir)/uim/libuim.la \
				   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk3_systray_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_gtk3_systray_CFLAGS   = @GTK3_CFLAGS@
endif
