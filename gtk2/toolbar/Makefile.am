EXTRA_DIST = GNOME_UimApplet.server.in.in

helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

if GNOME_APPLET
libexec_PROGRAMS =  uim-toolbar-applet

uim_toolbar_applet_LDADD = @GTK2_LIBS@ @GNOME_APPLET_LIBS@ \
			   $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la \
			   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_applet_CPPFLAGS = $(helper_defs) $(AM_CPPFLAGS)
uim_toolbar_applet_CFLAGS = @GTK2_CFLAGS@ @GNOME_APPLET_CFLAGS@
if GCC
# to cancel "-std=gnu89 -pedantic -Werror" and accept the string
# uim_menu_xml longer than ISO C 89 maximum length (509)
CFLAGS += -Wno-error
# recent version of gcc has the option to suppress the warning
#uim_toolbar_applet_CFLAGS += -Wno-overlength-strings
endif

uim_toolbar_applet_SOURCES = applet-gnome.c common-gtk.c

serverdir = $(libdir)/bonobo/servers
server_DATA = GNOME_UimApplet.server
server_in_files = $(server_DATA:=.in)
server_in_in_files = $(server_in_files:=.in)

if USE_NLS
  @INTLTOOL_SERVER_RULE@
else
  %.server: %.server.in ; cp $< $@
endif

$(server_in_files): $(server_in_in_files) Makefile
	$(SED) s,@LIBEXECDIR@,$(libexecdir),g <$< >$@.tmp
	$(SED) s,@UIM_PIXMAPSDIR@,$(uim_pixmapsdir),g <$@.tmp >$@

DISTCLEANFILES = GNOME_UimApplet.server.in GNOME_UimApplet.server \
		 GNOME_UimApplet.server.in.tmp
endif

if GTK2
bin_PROGRAMS = uim-toolbar-gtk uim-toolbar-gtk-systray
endif

uim_toolbar_gtk_SOURCES  = standalone-gtk.c common-gtk.c
uim_toolbar_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la \
			   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk_CPPFLAGS = $(AM_CPPFLAGS)
uim_toolbar_gtk_CFLAGS   = @GTK2_CFLAGS@

uim_toolbar_gtk_systray_SOURCES  = systray-gtk.c common-gtk.c \
				   eggtrayicon.c eggtrayicon.h
uim_toolbar_gtk_systray_LDADD    = @GTK2_LIBS@ \
				   @X11_LIBS@ \
				   $(top_builddir)/uim/libuim-scm.la \
				   $(top_builddir)/uim/libuim.la \
				   $(top_builddir)/uim/libuim-custom.la
uim_toolbar_gtk_systray_CPPFLAGS = $(AM_CPPFLAGS)
uim_toolbar_gtk_systray_CFLAGS   = @GTK2_CFLAGS@
