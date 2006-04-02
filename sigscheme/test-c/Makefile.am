objdir = @objdir@

AM_CPPFLAGS = -I$(top_srcdir)/src
AM_LDFLAGS  = -rpath $(libdir)

EXTRA_DIST = test_template.c test-storage-compact.c
noinst_LTLIBRARIES =
TEST_SRCS =

TEST_SRCS                     += test_strcasecmp.c
noinst_LTLIBRARIES            += libtest_strcasecmp.la
libtest_strcasecmp_la_SOURCES = test_strcasecmp.c cutter-sscm.h
libtest_strcasecmp_la_LIBADD  = $(top_builddir)/src/libsscm.la

TEST_SRCS                  += test_global.c
noinst_LTLIBRARIES         += libtest_global.la
libtest_global_la_SOURCES  = test_global.c cutter-sscm.h
libtest_global_la_LIBADD   = $(top_builddir)/src/libsscm.la

TEST_SRCS                  += test_format.c
noinst_LTLIBRARIES         += libtest_format.la
libtest_format_la_SOURCES  = test_format.c cutter-sscm.h
libtest_format_la_LIBADD   = $(top_builddir)/src/libsscm.la

.PHONY: test update-suites

test:
	cutter $(objdir)

update-suites:
	for src in $(TEST_SRCS); do cutter-update-suite $$src; done
