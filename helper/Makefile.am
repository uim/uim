EXTRA_DIST = GNOME_UimApplet.server.in.in uim-dict-ui.xml.in

helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

if APPLET_GNOME
libexec_PROGRAMS =  uim-toolbar-applet

uim_toolbar_applet_LDADD = @GTK2_LIBS@ @APPLET_GNOME_LIBS@ \
			   $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la
uim_toolbar_applet_CPPFLAGS = $(helper_defs) -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_applet_CFLAGS = @GTK2_CFLAGS@ @APPLET_GNOME_CFLAGS@
if GCC
# to cancel "-std=gnu89 -pedantic -Werror" and accept the string
# uim_menu_xml longer than ISO C 89 maximum length (509)
CFLAGS += -Wno-error
# recent version of gcc has the option to suppress the warning
#uim_toolbar_applet_CFLAGS += -Wno-overlength-strings
endif

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
if APPLET_GNOME
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
uim_candwin_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la
uim_candwin_gtk_CPPFLAGS = -I$(top_srcdir)
uim_candwin_gtk_CFLAGS   = @GTK2_CFLAGS@  -Wall

uim_toolbar_gtk_SOURCES  = toolbar-standalone-gtk.c toolbar-common-gtk.c
uim_toolbar_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			   $(top_builddir)/uim/libuim.la
uim_toolbar_gtk_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_gtk_CFLAGS   = @GTK2_CFLAGS@ -Wall

uim_toolbar_gtk_systray_SOURCES  = toolbar-systray-gtk.c toolbar-common-gtk.c \
				   eggtrayicon.c eggtrayicon.h
uim_toolbar_gtk_systray_LDADD    = @GTK2_LIBS@ \
				   @X11_LIBS@ \
				   $(top_builddir)/uim/libuim-scm.la \
				   $(top_builddir)/uim/libuim.la
uim_toolbar_gtk_systray_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_toolbar_gtk_systray_CFLAGS   = @GTK2_CFLAGS@  -Wall

uim_im_switcher_gtk_SOURCES  = im-switcher-gtk.c
uim_im_switcher_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			       $(top_builddir)/uim/libuim.la	
uim_im_switcher_gtk_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_im_switcher_gtk_CFLAGS   = @GTK2_CFLAGS@ -Wall

uim_input_pad_ja_SOURCES  = input-pad-ja.c
uim_input_pad_ja_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			    $(top_builddir)/uim/libuim.la
uim_input_pad_ja_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)
uim_input_pad_ja_CFLAGS   = @GTK2_CFLAGS@ -Wall

if PREF
if GTK2_4
uim_pref_gtk_SOURCES  = pref-gtk.c \
			pref-gtk-custom-widgets.c pref-gtk-custom-widgets.h \
			pref-gtk-keytab.h \
			../gtk/key-util-gtk.c ../gtk/key-util-gtk.h
uim_pref_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-custom.la \
			$(top_builddir)/uim/libuim-scm.la \
			$(top_builddir)/uim/libuim.la
if LIBUIM_X_UTIL
uim_pref_gtk_LDADD += $(top_builddir)/uim/libuim-x-util.la
endif
uim_pref_gtk_CPPFLAGS = $(helper_defs) -I$(top_srcdir) -I$(top_builddir)
uim_pref_gtk_CFLAGS   = @GTK2_CFLAGS@ -Wall
endif
endif

if DICT
uim_dict_gtk_SOURCES  = dict-main-gtk.c dict-canna-cclass.c dict-canna-cclass.h dict-word-win-gtk.c dict-word-win-gtk.h dict-word-list-win-gtk.c dict-word-list-win-gtk.h dict-util.c dict-util.h dict-word-list-view-gtk.c dict-word-list-view-gtk.h dict-word.c dict-word.h dict-dict.c dict-dict.h dict-cclass-dialog.c dict-cclass-dialog.h
uim_dict_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			$(top_builddir)/uim/libuim.la
uim_dict_gtk_CPPFLAGS = $(helper_defs) -I$(top_srcdir) -I$(top_builddir)
uim_dict_gtk_CFLAGS   = @GTK2_CFLAGS@
uim_dict_uidir = $(datadir)/uim/helperdata
uim_dict_ui_DATA = uim-dict-ui.xml
if ANTHY
uim_dict_gtk_SOURCES  += dict-anthy.c dict-anthy.h
uim_dict_gtk_LDADD    += @ANTHY_LIBS@
uim_dict_gtk_CPPFLAGS += -DUSE_ANTHY
else
if ANTHY_UTF8
uim_dict_gtk_SOURCES  += dict-anthy.c dict-anthy.h
uim_dict_gtk_LDADD    += @ANTHY_UTF8_LIBS@
uim_dict_gtk_CPPFLAGS += -DUSE_ANTHY @ANTHY_UTF8_CFLAGS@
endif
endif
if CANNA
uim_dict_gtk_SOURCES  += dict-canna.c dict-canna.h
uim_dict_gtk_LDADD    += -lcanna
uim_dict_gtk_CPPFLAGS += -DUSE_CANNA
endif
endif

endif
