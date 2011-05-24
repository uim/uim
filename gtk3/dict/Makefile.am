if GTK3
if DICT

helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

bin_PROGRAMS = uim-dict-gtk3
uim_dict_gtk3_SOURCES  = ../../gtk2/dict/main-gtk.c \
			../../gtk2/dict/canna-cclass.c \
			../../gtk2/dict/canna-cclass.h \
			../../gtk2/dict/word-win-gtk.c \
			../../gtk2/dict/word-win-gtk.h \
			../../gtk2/dict/word-list-win-gtk.c \
			../../gtk2/dict/word-list-win-gtk.h \
			../../gtk2/dict/util.c ../../gtk2/dict/util.h \
			../../gtk2/dict/word-list-view-gtk.c \
			../../gtk2/dict/word-list-view-gtk.h \
			../../gtk2/dict/word.c ../../gtk2/dict/word.h \
			../../gtk2/dict/dict.c ../../gtk2/dict/dict.h \
			../../gtk2/dict/cclass-dialog.c \
			../../gtk2/dict/cclass-dialog.h
						 
uim_dict_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			$(top_builddir)/uim/libuim.la
uim_dict_gtk3_CPPFLAGS = $(helper_defs) -I$(top_srcdir) -I$(top_builddir)
uim_dict_gtk3_CFLAGS   = @GTK3_CFLAGS@
uim_dict_uidir = $(datadir)/uim/helperdata
uim_dict_ui_DATA = ../../gtk2/dict/uim-dict-ui.xml
if ANTHY
uim_dict_gtk3_SOURCES  += ../../gtk2/dict/anthy.c ../../gtk2/dict/anthy.h
uim_dict_gtk3_LDADD    += @ANTHY_LIBS@
uim_dict_gtk3_CPPFLAGS += -DUSE_ANTHY
else
if ANTHY_UTF8
uim_dict_gtk3_SOURCES  += ../../gtk2/dict/anthy.c ../../gtk2/dict/anthy.h
uim_dict_gtk3_LDADD    += @ANTHY_UTF8_LIBS@
uim_dict_gtk3_CPPFLAGS += -DUSE_ANTHY @ANTHY_UTF8_CFLAGS@
endif
endif
if DICT_CANNA
uim_dict_gtk3_SOURCES  += ../../gtk2/dict/canna.c ../../gtk2/dict/canna.h
uim_dict_gtk3_LDADD    += -lcanna
uim_dict_gtk3_CPPFLAGS += -DUSE_CANNA
endif

endif

endif
