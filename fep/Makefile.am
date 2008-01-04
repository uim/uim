
EXTRA_DIST = README.ja README.key

if UIM_FEP

bin_PROGRAMS = uim-fep uim-fep-tick

uim_fep_SOURCES = uim-fep.c uim-fep.h udsock.c udsock.h str.c str.h callbacks.c callbacks.h draw.c draw.h escseq.c escseq.h key.c key.h read.c read.h helper.c helper.h
uim_fep_CPPFLAGS = -I$(top_srcdir)
uim_fep_CFLAGS =
uim_fep_LDADD = $(top_builddir)/uim/libuim-scm.la \
		$(top_builddir)/uim/libuim.la @FEP_LIBADD@

uim_fep_tick_SOURCES = uim-fep-tick.c uim-fep.h udsock.c udsock.h
uim_fep_tick_CPPFLAGS= -I$(top_srcdir)
uim_fep_tick_LDADD = $(top_builddir)/uim/libuim.la

endif
