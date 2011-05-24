EXTRA_DIST = uim-dict-ui.xml.in

helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

if GTK2
if DICT
bin_PROGRAMS = uim-dict-gtk

uim_dict_gtk_SOURCES  = main-gtk.c canna-cclass.c canna-cclass.h word-win-gtk.c word-win-gtk.h word-list-win-gtk.c word-list-win-gtk.h util.c util.h word-list-view-gtk.c word-list-view-gtk.h word.c word.h dict.c dict.h cclass-dialog.c cclass-dialog.h
uim_dict_gtk_LDADD    = @GTK2_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			$(top_builddir)/uim/libuim.la
uim_dict_gtk_CPPFLAGS = $(helper_defs) -I$(top_srcdir) -I$(top_builddir)
uim_dict_gtk_CFLAGS   = @GTK2_CFLAGS@
uim_dict_uidir = $(datadir)/uim/helperdata
uim_dict_ui_DATA = uim-dict-ui.xml
if ANTHY
uim_dict_gtk_SOURCES  += anthy.c anthy.h
uim_dict_gtk_LDADD    += @ANTHY_LIBS@
uim_dict_gtk_CPPFLAGS += -DUSE_ANTHY
else
if ANTHY_UTF8
uim_dict_gtk_SOURCES  += anthy.c anthy.h
uim_dict_gtk_LDADD    += @ANTHY_UTF8_LIBS@
uim_dict_gtk_CPPFLAGS += -DUSE_ANTHY @ANTHY_UTF8_CFLAGS@
endif
endif
if DICT_CANNA
uim_dict_gtk_SOURCES  += canna.c canna.h
uim_dict_gtk_LDADD    += -lcanna
uim_dict_gtk_CPPFLAGS += -DUSE_CANNA
endif
endif
endif
