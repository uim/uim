EXTRA_DIST = meson.build


if GTK4
if DICT
AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

helper_defs = -DUIM_DATADIR=\""$(datadir)/@PACKAGE@"\"

bin_PROGRAMS = uim-dict-gtk4
uim_dict_gtk4_SOURCES  = main-gtk4.c \
			canna-cclass.c \
			canna-cclass.h \
			word-win-gtk4.c \
			word-win-gtk4.h \
			word-list-win-gtk4.c \
			word-list-win-gtk4.h \
			util.c util.h \
			word-list-view-gtk4.c \
			word-list-view-gtk4.h \
			word.c word.h \
			dict.c dict.h \
			cclass-dialog-gtk4.c \
			cclass-dialog-gtk4.h

uim_dict_gtk4_LDADD    = @GTK4_LIBS@ $(top_builddir)/uim/libuim-scm.la \
			$(top_builddir)/uim/libuim.la
uim_dict_gtk4_CPPFLAGS = $(helper_defs) $(AM_CPPFLAGS)
uim_dict_gtk4_CFLAGS   = @GTK4_CFLAGS@
if ANTHY
uim_dict_gtk4_SOURCES  += anthy.c anthy.h
uim_dict_gtk4_LDADD    += @ANTHY_LIBS@
uim_dict_gtk4_CPPFLAGS += -DUSE_ANTHY
else
if ANTHY_UTF8
uim_dict_gtk4_SOURCES  += anthy.c anthy.h
uim_dict_gtk4_LDADD    += @ANTHY_UTF8_LIBS@
uim_dict_gtk4_CPPFLAGS += -DUSE_ANTHY @ANTHY_UTF8_CFLAGS@
endif
endif
if DICT_CANNA
uim_dict_gtk4_SOURCES  += canna.c canna.h
uim_dict_gtk4_LDADD    += -lcanna
uim_dict_gtk4_CPPFLAGS += -DUSE_CANNA
endif

endif

endif
