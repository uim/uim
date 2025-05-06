uim_libs   = $(top_builddir)/uim/libuim-custom.la \
	     $(top_builddir)/uim/libuim-scm.la \
	     $(top_builddir)/uim/libuim.la

AM_CPPFLAGS =					\
	-I$(top_builddir)/uim			\
	-I$(top_srcdir)/replace			\
	-I$(top_srcdir)

noinst_PROGRAMS = uim-custom-variable uim-custom-dump uim-custom-update

uim_custom_variable_LIBS =
uim_custom_variable_LDADD = $(uim_libs)
uim_custom_variable_CPPFLAGS = $(AM_CPPFLAGS)
uim_custom_variable_CFLAGS =
uim_custom_variable_SOURCES = uim-custom-variable.c

uim_custom_dump_LIBS =
uim_custom_dump_LDADD = $(uim_libs)
uim_custom_dump_CPPFLAGS = $(AM_CPPFLAGS)
uim_custom_dump_CFLAGS =
uim_custom_dump_SOURCES = uim-custom-dump.c

uim_custom_update_LIBS =
uim_custom_update_LDADD = $(uim_libs)
uim_custom_update_CPPFLAGS = $(AM_CPPFLAGS)
uim_custom_update_CFLAGS =
uim_custom_update_SOURCES = uim-custom-update.c
