if GTK3
if DICT

helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

bin_PROGRAMS = uim-dict-gtk3
uim_dict_gtk3_SOURCES  = ../../helper/dict-main-gtk.c \
			../../helper/dict-canna-cclass.c ../../helper/dict-canna-cclass.h \
			../../helper/dict-word-win-gtk.c ../../helper/dict-word-win-gtk.h \
			../../helper/dict-word-list-win-gtk.c \
			../../helper/dict-word-list-win-gtk.h \
			../../helper/dict-util.c ../../helper/dict-util.h \
			../../helper/dict-word-list-view-gtk.c \
			../../helper/dict-word-list-view-gtk.h \
			../../helper/dict-word.c ../../helper/dict-word.h \
			../../helper/dict-dict.c ../../helper/dict-dict.h \
			../../helper/dict-cclass-dialog.c dict-cclass-dialog.h
						 
uim_dict_gtk3_LDADD    = @GTK3_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			$(top_builddir)/uim/libuim.la
uim_dict_gtk3_CPPFLAGS = $(helper_defs) -I$(top_srcdir) -I$(top_builddir)
uim_dict_gtk3_CFLAGS   = @GTK3_CFLAGS@
uim_dict_uidir = $(datadir)/uim/helperdata
uim_dict_ui_DATA = ../../helper/uim-dict-ui.xml
if ANTHY
uim_dict_gtk3_SOURCES  += ../../helper/dict-anthy.c ../../helper/dict-anthy.h
uim_dict_gtk3_LDADD    += @ANTHY_LIBS@
uim_dict_gtk3_CPPFLAGS += -DUSE_ANTHY
else
if ANTHY_UTF8
uim_dict_gtk3_SOURCES  += ../../helper/dict-anthy.c ../../helper/dict-anthy.h
uim_dict_gtk3_LDADD    += @ANTHY_UTF8_LIBS@
uim_dict_gtk3_CPPFLAGS += -DUSE_ANTHY @ANTHY_UTF8_CFLAGS@
endif
endif
if DICT_CANNA
uim_dict_gtk3_SOURCES  += ../../helper/dict-canna.c ../../helper/dict-canna.h
uim_dict_gtk3_LDADD    += -lcanna
uim_dict_gtk3_CPPFLAGS += -DUSE_CANNA
endif

endif

endif
