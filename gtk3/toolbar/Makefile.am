EXTRA_DIST = UimApplet.panel-applet.in.in \
	     org.gnome.panel.applet.UimAppletFactory.service.in

if APPLET_GNOME3
helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

libexec_PROGRAMS = uim-toolbar-applet-gnome3

uim_toolbar_applet_gnome3_LDADD = @GTK3_LIBS@ @APPLET_GNOME3_LIBS@ \
			   $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la \
			   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_applet_gnome3_CPPFLAGS = \
			   $(helper_defs) -I$(top_srcdir) -I$(top_builddir)
									 
uim_toolbar_applet_gnome3_CFLAGS = @GTK3_CFLAGS@ @APPLET_GNOME3_CFLAGS@

uim_toolbar_applet_gnome3_SOURCES = applet-gnome3.c \
			       ../../helper/toolbar-common-gtk.c

appletdir = $(datadir)/gnome-panel/4.0/applets
applet_DATA = UimApplet.panel-applet
applet_in_files = $(applet_DATA:=.in)
applet_in_in_files = $(applet_in_files:=.in)

$(applet_in_files): $(applet_in_in_files) Makefile
	sed s,@LIBEXECDIR@,$(libexecdir),g <$< >$@.tmp
	sed s,@UIM_PIXMAPSDIR@,$(uim_pixmapsdir),g <$@.tmp >$@

po_files = $(wildcard $(top_srcdir)/po/*.po)
$(applet_DATA): $(applet_in_files) $(INTLTOOL_MERGE) $(po_files)
	LC_ALL=C $(INTLTOOL_MERGE) -d -u -c $(top_builddir)/po/.intltool-merge-cache $(top_srcdir)/po $< $@ 

servicedir = $(datadir)/dbus-1/services
service_DATA = org.gnome.panel.applet.UimAppletFactory.service
service_in_files = $(service_DATA:=.in)

$(service_DATA): $(service_in_files) Makefile
	sed s,@LIBEXECDIR@,$(libexecdir),g <$< >$@

DISTCLEANFILES = UimApplet.panel-applet.in UimApplet.panel-applet \
		 UimApplet.panel-applet.in.tmp \
		 org.gnome.panel.applet.UimAppletFactory.service
endif

if GTK3
bin_PROGRAMS = uim-toolbar-gtk3 uim-toolbar-gtk3-systray
uim_toolbar_gtk3_SOURCES  = ../../helper/toolbar-standalone-gtk.c \
			       ../../helper/toolbar-common-gtk.c
uim_toolbar_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la \
			   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk3_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_gtk3_CFLAGS   = @GTK3_CFLAGS@

uim_toolbar_gtk3_systray_SOURCES  = ../../helper/toolbar-systray-gtk.c \
				   ../../helper/toolbar-common-gtk.c \
				   ../../helper/eggtrayicon.c ../../helper/eggtrayicon.h
uim_toolbar_gtk3_systray_LDADD    = @GTK3_LIBS@ \
				   @X11_LIBS@ \
				   $(top_builddir)/uim/libuim-scm.la \
				   $(top_builddir)/uim/libuim.la \
				   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk3_systray_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_gtk3_systray_CFLAGS   = @GTK3_CFLAGS@
endif
