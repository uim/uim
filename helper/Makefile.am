EXTRA_DIST = GNOME_UimApplet.server.in.in $(uim_dict_ui_DATA)

helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

if APPLET
libexec_PROGRAMS =  uim-toolbar-applet

uim_toolbar_applet_LDADD = @GTK2_LIBS@ @APPLET_LIBS@ \
			   $(top_builddir)/uim/libuim.la
uim_toolbar_applet_CPPFLAGS = $(helper_defs) -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_applet_CFLAGS = @GTK2_CFLAGS@ @APPLET_CFLAGS@

uim_toolbar_applet_SOURCES = toolbar-applet-gnome.c toolbar-common-gtk.c

serverdir = $(libdir)/bonobo/servers
server_DATA = GNOME_UimApplet.server
server_in_files = $(server_DATA:=.in)
server_in_in_files = $(server_in_files:=.in)

@INTLTOOL_SERVER_RULE@

$(server_in_files): $(server_in_in_files) Makefile
	sed s,@LIBEXECDIR@,$(libexecdir),g <$< >$@.tmp
	sed s,@UIM_PIXMAPSDIR@,$(uim_pixmapsdir),g <$@.tmp >$@

DISTCLEANFILES = GNOME_UimApplet.server.in GNOME_UimApplet.server \
		 GNOME_UimApplet.server.in.tmp
endif


if GTK2
bin_PROGRAMS = uim-toolbar-gtk uim-toolbar-gtk-systray uim-im-switcher-gtk \
	       uim-input-pad-ja
if APPLET
libexec_PROGRAMS += uim-candwin-gtk
else
libexec_PROGRAMS = uim-candwin-gtk
endif

if DICT
bin_PROGRAMS += uim-dict-gtk
endif

if PREF
if GTK2_4
bin_PROGRAMS += uim-pref-gtk
endif
endif

uim_candwin_gtk_SOURCES  = candwin-gtk.c \
			   ../gtk/caret-state-indicator.c \
			   ../gtk/caret-state-indicator.h
uim_candwin_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim.la
uim_candwin_gtk_CPPFLAGS = -I$(top_srcdir)
uim_candwin_gtk_CFLAGS   = @GTK2_CFLAGS@  -Wall

uim_toolbar_gtk_SOURCES  = toolbar-standalone-gtk.c toolbar-common-gtk.c
uim_toolbar_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim.la
uim_toolbar_gtk_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_gtk_CFLAGS   = @GTK2_CFLAGS@ -Wall

uim_toolbar_gtk_systray_SOURCES  = toolbar-systray-gtk.c toolbar-common-gtk.c \
				   eggtrayicon.c eggtrayicon.h
uim_toolbar_gtk_systray_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim.la
uim_toolbar_gtk_systray_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_gtk_systray_CFLAGS   = @GTK2_CFLAGS@  -Wall

uim_im_switcher_gtk_SOURCES  = im-switcher-gtk.c
uim_im_switcher_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim.la
uim_im_switcher_gtk_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_im_switcher_gtk_CFLAGS   = @GTK2_CFLAGS@ -Wall

uim_input_pad_ja_SOURCES  = input-pad-ja.c
uim_input_pad_ja_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim.la
uim_input_pad_ja_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_input_pad_ja_CFLAGS   = @GTK2_CFLAGS@ -Wall

if PREF
if GTK2_4
uim_pref_gtk_SOURCES  = pref-gtk.c \
			pref-gtk-custom-widgets.c pref-gtk-custom-widgets.h \
			../gtk/key-util-gtk.c ../gtk/key-util-gtk.h
uim_pref_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-custom.la \
			$(top_builddir)/uim/libuim.la
uim_pref_gtk_CPPFLAGS = $(helper_defs) -I$(top_srcdir) -I$(top_builddir)
uim_pref_gtk_CFLAGS   = @GTK2_CFLAGS@ -Wall
endif
endif

if DICT
if ANTHY
uim_dict_gtk_SOURCES  = dict-main-gtk.c dict-anthy.c dict-anthy.h dict-canna.c dict-canna.h dict-word-win-gtk.c dict-word-win-gtk.h dict-word-list-win-gtk.c dict-word-list-win-gtk.h dict-util.c dict-util.h dict-word-list-view-gtk.c dict-word-list-view-gtk.h dict-word.c dict-word.h dict-dict.c dict-dict.h dict-cclass-dialog.c dict-cclass-dialog.h
uim_dict_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim.la @ANTHY_LIBS@
uim_dict_gtk_CPPFLAGS = $(helper_defs) -I$(top_srcdir) -I$(top_builddir)
uim_dict_gtk_CFLAGS   = @GTK2_CFLAGS@
uim_dict_uidir = $(datadir)/uim/helperdata
uim_dict_ui_DATA = uim-dict-ui.xml
endif
endif

endif
