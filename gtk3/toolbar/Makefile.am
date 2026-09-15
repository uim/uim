EXTRA_DIST = UimApplet.panel-applet.desktop.in.in \
	     uim-applet-menu.xml

AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

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
			   $(helper_defs) $(AM_CPPFLAGS)
libuim_toolbar_applet_gnome3_la_LDFLAGS = \
			   -module -avoid-version

libuim_toolbar_applet_gnome3_la_CFLAGS = @GTK3_CFLAGS@ @GNOME3_APPLET_CFLAGS@

libuim_toolbar_applet_gnome3_la_SOURCES = applet-gnome3.c common-gtk.c

APPLET_LOCATION = $(pkglibdir)/libuim-toolbar-applet-gnome3.so

appletdir = $(LIBPANEL_APPLET_DIR)
applet_DATA = UimApplet.panel-applet
applet_in_files = $(applet_DATA:=.in)
# Named *.desktop.in.in so that xgettext parses it as a key file.
applet_in_in_files = $(applet_DATA:=.desktop.in.in)

$(applet_in_files): $(applet_in_in_files) Makefile
	$(SED) s,@APPLET_LOCATION@,$(APPLET_LOCATION),g <$< >$@.tmp
	$(SED) s,@UIM_PIXMAPSDIR@,$(uim_pixmapsdir),g <$@.tmp >$@

if USE_NLS
  po_files = $(wildcard $(top_srcdir)/po/*.po)
  $(applet_DATA): $(applet_in_files) $(po_files)
	$(MSGFMT) --desktop --keyword=Name --keyword=Description \
	  --template=$< -d $(top_srcdir)/po -o $@
else
  $(applet_DATA): $(applet_in_files) ; cp $< $@
endif

DISTCLEANFILES = UimApplet.panel-applet.in UimApplet.panel-applet \
		 UimApplet.panel-applet.in.tmp
endif

if GTK3
bin_PROGRAMS = uim-toolbar-gtk3 uim-toolbar-gtk3-systray
uim_toolbar_gtk3_SOURCES  = standalone-gtk.c common-gtk.c
uim_toolbar_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la \
			   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk3_CPPFLAGS = $(AM_CPPFLAGS)
uim_toolbar_gtk3_CFLAGS   = @GTK3_CFLAGS@

uim_toolbar_gtk3_systray_SOURCES  = systray-gtk.c \
				   common-gtk.c \
				   eggtrayicon.c \
				   eggtrayicon.h
uim_toolbar_gtk3_systray_LDADD    = @GTK3_LIBS@ \
				   @X11_LIBS@ \
				   $(top_builddir)/uim/libuim-scm.la \
				   $(top_builddir)/uim/libuim.la \
				   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk3_systray_CPPFLAGS = $(AM_CPPFLAGS)
uim_toolbar_gtk3_systray_CFLAGS   = @GTK3_CFLAGS@
endif
